library(here)
library(tidyverse)
##
sdir <- here("sources")
ddir <- here("data")

source(here("scripts/writeLastUpdate.R"))

###############################################################################@
## Source information ----
###############################################################################@

sfi <- read_tsv(
   file.path(sdir, "ARCHIVES/ARCHIVES.txt")
)
HPO_sourceFiles <- sfi %>%
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
HPO_sourceFiles[which(HPO_sourceFiles$file=="hp.obo"), "current"] <- oboDate
HPO_sourceFiles <- HPO_sourceFiles %>%
   select(url, current)

## _+ Basic information ----
starts <- which(obo=="[Term]")
ends <- c(starts[-1]-1, length(obo))
hpDef <- do.call(bind_rows, apply(
   data.frame(starts, ends),
   1,
   function(x){
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
      return(tibble(
         id=id, name=name, syn = syn, def=def,
         parent=parent,
         altId=altId
      ))
   }
))
altId <- hpDef %>%
   select(id, altId) %>%
   unique()
parentId <- hpDef %>%
   select(id, parent) %>%
   unique()
hpDef <- hpDef %>%
   select(-parent, -altId) %>%
   unique()

## _+ Main HP table ----
HPO_hp <- hpDef %>%
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

## _+ Synonyms ----
HPO_synonyms <- hpDef %>%
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
   filter(!is.na(syn)) %>%
   rename("synonym"="syn")

## _+ Parents ----
parentList <- strsplit(parentId$parent, ", ")
names(parentList) <- parentId$id
HPO_parents <- stack(parentList) %>%
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
HPO_altId <- stack(altIdList) %>%
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

termParents <- split(HPO_parents$parent, HPO_parents$id)
termAncestors <- lapply(
   unique(HPO_hp$id),
   getAncestors
)
names(termAncestors) <- unique(HPO_hp$id)

HPO_hp <- HPO_hp %>%
   mutate(
      level=unlist(lapply(termAncestors, function(x) x$level))[HPO_hp$id]
   )

termAncestors <- lapply(termAncestors, function(x) x$parents)
termAncestors <- termAncestors[HPO_hp$id]
HPO_descendants <- stack(termAncestors) %>%
   as_tibble() %>%
   mutate_all(as.character) %>%
   bind_rows(
      tibble(values=names(termAncestors), ind=names(termAncestors))
   ) %>%
   rename(
      "id"="values", "descendant"="ind"
   )

###############################################################################@
## Data from phenotype_annotation.tab ----
###############################################################################@

hpdl <- readLines(file.path(sdir, "phenotype_annotation.tab"))
hpdl <- c(
   hpdl[1][str_detect(hpdl[1], regex("^#"), negate=TRUE)],
   hpdl[-1]
)
hpd <- read_tsv(
   I(hpdl),
   col_types=paste(rep("c", 15), collapse=""),
   col_names=FALSE,
)

## _+ diseaseHP ----
HPO_diseaseHP <- hpd %>%
   select(X1, X2, X5) %>%
   rename("db"="X1", "id"="X2", "hp"="X5") %>%
   mutate(
      # id=id %>% as.character(),
      hp=hp %>% str_remove("^HP[:]")
   )
   
## _+ diseaseSynonyms ----
diseases <- unique(hpd[,1:3])

HPO_diseaseSynonyms <- hpd %>%
   select(X1, X2, X3) %>%
   apply(
      1, function(x){
         names(x) <- c()
         syns <- unlist(strsplit(x[3], split=";;"))
         syns <- sub(
            paste0('^ *[%#]?', x[2], ' +'),
            "",
            syns
         )
         pref <- c(TRUE, rep(FALSE, length(syns)-1))
         toRet <- tibble(
            db=x[1],
            id=x[2] %>% str_remove_all(" "),
            synonym=syns,
            preferred=pref
         )
         return(toRet)
      }
   ) %>%
   bind_rows() %>%
   unique()

## _+ Diseases
HPO_diseases <- HPO_diseaseSynonyms %>%
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
      file=file.path(ddir, paste(f, ".txt", sep="")),
      quote="all", na="<NA>"
   )
}
message(Sys.time())
message("... Done\n")

writeLastUpdate()
