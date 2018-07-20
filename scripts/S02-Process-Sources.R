setwd("~/Shared/Data-Science/Data-Source-Model-Repository/HPO/scripts/")

##
sdir <- "../sources"
ddir <- "../data"

###############################################################################@
## Source information ----
###############################################################################@

sfi <- read.table(
   file.path(sdir, "ARCHIVES/ARCHIVES.txt"),
   sep="\t",
   header=T,
   stringsAsFactors=FALSE
)
HPO_sourceFiles <- sfi[which(sfi$inUse),]

###############################################################################@
## Data from hp.obo ----
###############################################################################@

## * Current update ----
obo <- readLines(file.path(sdir, "hp.obo"))
oboDate <- format(as.POSIXct(as.Date(
   sub("^.*[/]", "", grep("^data[-]version[:]", obo, value=TRUE))
)))
HPO_sourceFiles[which(HPO_sourceFiles$file=="hp.obo"), "current"] <- oboDate
HPO_sourceFiles <- HPO_sourceFiles[,c("url", "current")]

## * Basic information ----
starts <- which(obo=="[Term]")
ends <- c(starts[-1]-1, length(obo))
hpDef <- do.call(rbind, apply(
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
      fn <- "^def: "
      def <- sub(fn, "", grep(fn, termDesc, value=T))
      if(length(def)==0) def <- NA
      ##
      fn <- "^is_a: "
      parent <- sub(fn, "", grep(fn, termDesc, value=T))
      fn <- " [!].*$"
      parent <- sub(fn, "", parent)
      if(length(parent)==0) parent <- NA
      ##
      fn <- "^alt_id: "
      altId <- sub(fn, "", grep(fn, termDesc, value=T))
      altId <- paste(unique(c(id, altId)), collapse=", ")
      ##
      return(data.frame(
         id=id, name=name, def=def,
         parent=parent,
         altId=altId,
         stringsAsFactors=F)
      )
   }
))
altId <- unique(hpDef[, c("id", "altId")])
hpDef <- hpDef[, setdiff(colnames(hpDef), "altId")]

HPO_hp <- hpDef
colnames(HPO_hp) <- c("id", "name", "description", "parent")
HPO_hp$id <- sub("^HP[:]", "", HPO_hp$id)
HPO_hp$parent <- sub("^HP[:]", "", HPO_hp$parent)

## * Alternative ID ----
altIdList <- strsplit(altId$altId, ", ")
names(altIdList) <- altId$id
altId <- stack(altIdList)
colnames(altId) <- c("alt", "id")
altId$id <- as.character(altId$id)
altId$alt <- as.character(altId$alt)
HPO_altId <- altId
HPO_altId$alt <- sub("^HP[:]", "", HPO_altId$alt)
HPO_altId$id <- sub("^HP[:]", "", HPO_altId$id)

###############################################################################@
## Data from phenotype_annotation.tab ----
###############################################################################@

hpd <- read.table(
   file=file.path(sdir, "phenotype_annotation.tab"),
   header=FALSE,
   quote="", comment="",
   sep="\t",
   stringsAsFactors=F
)
HPO_diseaseHP <- unique(hpd[,c(1, 2, 5)])
colnames(HPO_diseaseHP) <- c("db", "id", "hp")
HPO_diseaseHP$hp <- sub("^HP[:]", "", HPO_diseaseHP$hp)

diseases <- unique(hpd[,1:3])
HPO_diseaseSynonyms <- do.call(rbind, apply(
   diseases, 1,
   function(x){
      names(x) <- c()
      syns <- unlist(strsplit(x[3], split=";;"))
      syns <- sub(
         paste0('^ *[%#]?', x[2], ' +'),
         "",
         syns
      )
      pref <- c(TRUE, rep(FALSE, length(syns)-1))
      toRet <- data.frame(
         db=x[1],
         id=x[2],
         synonym=syns,
         preferred=pref,
         stringsAsFactors=FALSE
      )
      return(toRet)
   }
))
HPO_diseases <- HPO_diseaseSynonyms[
   which(HPO_diseaseSynonyms$preferred),
   c("db", "id", "synonym")
]
colnames(HPO_diseases) <- c("db", "id", "label")

###############################################################################@
## Writing tables ----
###############################################################################@

message("Writing tables...")
message(Sys.time())
toSave <- grep("^HPO[_]", ls(), value=T)
for(f in toSave){
   message(paste("   Writing", f))
   ## Ensure unicity
   toWrite <- get(f)
   write.table(
      get(f),
      file=file.path(ddir, paste(f, ".txt", sep="")),
      sep="\t",
      row.names=FALSE, col.names=TRUE,
      quote=TRUE,
	  qmethod="double"
   )
}
message(Sys.time())
message("... Done\n")
