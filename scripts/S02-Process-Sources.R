library(here)
library(tidyverse)
##
sdir <- here("sources")
ddir <- here("data")

###############################################################################@
## Source information ----
###############################################################################@

sfi <- read_tsv(
   file.path(sdir, "ARCHIVES/ARCHIVES.txt")
)
HPO_Source_files <- sfi %>%
   filter(inUse) %>%
   mutate(current=current %>% as.Date() %>% as.character())

###############################################################################@
## Data from hp.obo ----
###############################################################################@

obo <- readLines(file.path(sdir, "hp.obo"))

## _+ Current update ----
oboDate <- grep("^data[-]version[:]", obo, value=TRUE) %>%
   str_remove("^.*[/]") %>%
   as.Date() %>%
   as.POSIXct() %>%
   as.Date() %>%
   as.character()
HPO_Source_files[which(HPO_Source_files$file=="hp.obo"), "current"] <- oboDate
HPO_Source_files <- HPO_Source_files %>%
   select(url, current)

## _+ Basic information ----
# starts <- which(obo=="[Term]")
starts <- grep("^[[][[:alpha:]]+[]]", obo)
ends <- c(starts[-1]-1, length(obo))
hpDef <- do.call(bind_rows, apply(
   data.frame(starts, ends),
   1,
   function(x){
      type <- obo[x[1]]
      termDesc <- obo[(x[1]+1):(x[2]-1)]
      ##
      fn <- "^id: "
      id <- sub(fn, "", grep(fn, termDesc, value=T))
      if(length(id)==0) id <- NA
      ##
      fn <- "^name: "
      name <- sub(fn, "", grep(fn, termDesc, value=T))
      if(length(name)==0) name <- NA
      ##
      fn <- "^synonym: "
      syn <- sub(fn, "", grep(fn, termDesc, value=T))
      if(length(syn)==0) syn <- NA
      ##
      fn <- "^def: "
      def <- sub(fn, "", grep(fn, termDesc, value=T))
      if(length(def)==0) def <- NA
      ##
      fn <- "^is_a: "
      parent <- sub(fn, "", grep(fn, termDesc, value=T))
      fn <- " [!].*$"
      parent <- sub(fn, "", parent)
      if(length(parent)==0) parent <- NA
      parent <- paste(unique(c(id, parent)), collapse=", ")
      ##
      fn <- "^alt_id: "
      altId <- sub(fn, "", grep(fn, termDesc, value=T))
      altId <- paste(unique(c(id, altId)), collapse=", ")
      ##
      fn <- "^is_obsolete: "
      obsolete <- sub(fn, "", grep(fn, termDesc, value=T))
      if(length(obsolete)==0){
         obsolete <- FALSE
      }else{
         obsolete <- TRUE
      }
      ##
      fn <- "^replaced_by: "
      replacedBy <- sub(fn, "", grep(fn, termDesc, value=T))
      replacedBy <- paste(replacedBy, collapse=", ")
      ##
      return(tibble(
         type=type,
         id=id, name=name, syn = syn, def=def,
         parent=parent,
         altId=altId,
         obsolete=obsolete,
         replacedBy=replacedBy
      ))
   }
))
hpDef <- hpDef %>% filter(type=="[Term]")
altId <- hpDef %>%
   filter(!obsolete) %>% 
   select(id, altId) %>%
   unique()
parentId <- hpDef %>%
   select(id, parent) %>%
   unique()
hpDef <- hpDef %>%
   select(-parent, -altId) %>%
   unique()

## _+ Main HP table ----
HPO_HP <- hpDef %>%
   filter(!obsolete) %>% 
   select(id, name, def) %>%
   unique() %>%
   rename("description"="def") %>%
   mutate(
      id=id %>% str_remove("^HP[:]"),
      description=description %>%
         str_replace_all("\t|\n|\r|\\\\", " ") %>%
         str_remove('" [^"]*$') %>%
         str_remove("^\"")
   ) %>%
   filter(!duplicated(id))

## _+ Obsolete HP ----
HPO_Obsolete_HP <- hpDef %>%
   filter(obsolete) %>% 
   select(id, name, def, replacedBy) %>%
   unique() %>%
   rename("description"="def") %>%
   mutate(
      id=id %>% str_remove("^HP[:]"),
      description=description %>%
         str_replace_all("\t|\n|\r|\\\\", " ") %>%
         str_remove('" [^"]*$') %>%
         str_remove("^\"")
   ) %>%
   filter(!duplicated(id))
HPO_Replaced_HP <- HPO_Obsolete_HP$replacedBy %>% 
   strsplit(", ") %>% 
   setNames(HPO_Obsolete_HP$id) %>% 
   stack() %>% 
   as_tibble() %>% 
   mutate_all(as.character) %>% 
   rename(current="values", obsolete="ind") %>% 
   mutate(current=str_remove(current, "HP:"))
HPO_Obsolete_HP <- HPO_Obsolete_HP %>% 
   select(-replacedBy) %>% 
   distinct()


## _+ Synonyms ----
HPO_Synonyms <- hpDef %>%
   select(id, syn) %>%
   unique() %>%
   mutate(
      id=id %>% str_remove("^HP[:]"),
      type = syn %>% str_remove('^.*" ') %>% str_remove(' .*$'),
      syn = syn %>%
         str_replace_all("\t|\n|\r|\\\\", " ") %>%
         str_remove('" [^"]*$') %>%
         str_remove('^"')
   ) %>%
   filter(!is.na(syn) & id %in% HPO_HP$id) %>%
   rename("synonym"="syn")

## _+ Parents ----
parentList <- strsplit(parentId$parent, ", ")
names(parentList) <- parentId$id
HPO_Parents <- stack(parentList) %>%
   as_tibble() %>%
   rename("parent"="values", "id"="ind") %>%
   mutate(
      parent = parent %>% as.character() %>% str_remove("^HP[:]"),
      id = id %>% as.character() %>% str_remove("^HP[:]")
   ) %>%
   filter(
      !is.na(parent) & parent!="NA" & !is.na(id) & id!="NA" & id!=parent
   ) %>%
   select(id, parent)

## _+ Alternative ID ----
altIdList <- strsplit(altId$altId, ", ")
names(altIdList) <- altId$id
HPO_Alternative_ID <- stack(altIdList) %>%
   as_tibble() %>%
   rename("alt"="values", "id"="ind") %>%
   mutate(
      alt = alt %>% as.character() %>% str_remove("^HP[:]"),
      id = id %>% as.character() %>% str_remove("^HP[:]")
   ) %>%
   filter(
      !is.na(alt) & alt!="NA" & !is.na(id) & id!="NA" & id!=alt
   ) %>%
   select(id, alt)

## _+ Ancestors/Descendants ----
getAncestors <- function(id){
   direct <- termParents[[id]]
   parents <- direct
   level <- 0
   dLev <- c()
   for(d in direct){
      dPar <- getAncestors(d)
      dLev <- c(dLev, dPar$level)
      parents <- c(parents, dPar$parents)
   }
   if(length(dLev)>0){
      level <- max(dLev)+1
   }
   return(list(parents=unique(parents), level=level))
}

termParents <- split(HPO_Parents$parent, HPO_Parents$id)
termAncestors <- lapply(
   unique(HPO_HP$id),
   getAncestors
)
names(termAncestors) <- unique(HPO_HP$id)

HPO_HP <- HPO_HP %>%
   mutate(
      level=unlist(lapply(termAncestors, function(x) x$level))[HPO_HP$id]
   )

termAncestors <- lapply(termAncestors, function(x) x$parents)
termAncestors <- termAncestors[HPO_HP$id]
HPO_Descendants <- stack(termAncestors) %>%
   as_tibble() %>%
   mutate_all(as.character) %>%
   bind_rows(
      tibble(values=names(termAncestors), ind=names(termAncestors))
   ) %>%
   rename(
      "id"="values", "descendant"="ind"
   )

###############################################################################@
## Data from phenotype.hpoa ----
###############################################################################@

hpd <- read_tsv(
   file.path(sdir, "phenotype.hpoa"),
   comment = "#",
   col_types=paste(rep("c", 12), collapse="")
) %>%
   mutate(
      hp=str_remove(hpo_id, ".*[:]"),
      hpp=str_remove(hpo_id, "[:].*"),
      db=str_remove(database_id, "[:].*"),
      id=str_remove(database_id, ".*[:]")
   ) %>%
   rename(freq_from_hpo=frequency)

## _+ diseaseHP ----
hpfreq <- hpd %>% 
   select(freq_from_hpo) %>%
   mutate(id=str_remove(freq_from_hpo, "HP:")) %>% 
   distinct() %>% 
   left_join(
      HPO_HP %>% 
      filter(
         id %in% str_remove(
            unique(grep("HP:", hpd$freq_from_hpo, value=T)),
            "HP:"
         )
      ) %>% 
      select(id, name),
      by=c("id"="id")
   ) %>% 
   mutate(freq_num= unlist(lapply(freq_from_hpo, function(x){
         toRet <- try(as.numeric(eval(parse(text=x))), silent=TRUE)
         if(inherits(toRet, "try-error")){
            if(length(grep("%", x))>0){
               toRet <- as.numeric(str_remove(x, "%"))/100
            }else{
               toRet <- NA
            }
         }
         return(toRet)
   }))) %>% 
   mutate(name=ifelse(
      !is.na(name), name,
      case_when(
         freq_num == 0 ~ "Excluded",
         freq_num < 0.05 ~ "Very rare",
         freq_num < 0.30 ~ "Occasional",
         freq_num < 0.80 ~ "Frequent",
         freq_num < 1.00 ~ "Very frequent",
         freq_num == 1.00 ~ "Obligate"
      )
   )) %>% 
   mutate(
      freq_order=as.integer(c(
         "Excluded"=1,
         "Very rare"=2,
         "Occasional"=3,
         "Frequent"=4,
         "Very frequent"=5,
         "Obligate"=6
      )[name])
   ) %>% 
   select(
      freq_from_hpo,
      freq_num,
      freq_category=name,
      freq_order
   )
HPO_Disease_HP <- hpd %>% 
   select(db, id, hp, freq_from_hpo) %>% 
   distinct() %>% 
   left_join(hpfreq, by="freq_from_hpo")
   
## _+ diseaseSynonyms ----
HPO_Disease_synonyms <- hpd %>%
   select(db, id, synonym=disease_name) %>%
   distinct() %>%
   mutate(preferred=!duplicated(paste(db, id))) %>% 
   unique()

## _+ Diseases
HPO_Diseases <- HPO_Disease_synonyms %>%
   filter(preferred) %>%
   select(db, id, synonym) %>%
   rename("label"="synonym") %>%
   unique()

###############################################################################@
## Writing tables ----
###############################################################################@

rm_tr_spaces <- function(x){
   stringr::str_remove(x, stringr::regex("^ *")) %>%
      stringr::str_remove(stringr::regex(" *$"))
}

message("Writing tables...")
message(Sys.time())
file.rename(ddir, paste0(ddir, "_BCK_", Sys.Date()))
dir.create(ddir)
toSave <- grep("^HPO[_]", ls(), value=T)
for(f in toSave){
   message(paste("   Writing", f))
   tv <- get(f)
   for(cn in colnames(tv)){
      if(class(pull(tv, !!cn))=="character"){
         tv[, cn] <- rm_tr_spaces(pull(tv, !!cn))
      }
   }
   tv <- distinct(tv)
   write_tsv(
      tv,
      file=file.path(ddir, paste(sub("^HPO[_]", "", f), ".txt", sep="")),
      quote="all", na="<NA>"
   )
}
message(Sys.time())
message("... Done\n")

###############################################################################@
## Data model ----
###############################################################################@
# library(TKCat)
# dmf <- here("model/HPO.json")
# dm <- read_json_data_model(dmf)
# write_json_data_model(dm, dmf)
# plot(dm) %>%
#    visNetwork::visOptions(width="2000px", height="1000px") %>%
#    htmlwidgets::saveWidget(file=stringr::str_replace(dmf, "[.]json$", ".html"))
# 
