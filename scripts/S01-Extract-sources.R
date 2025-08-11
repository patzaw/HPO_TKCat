library(here)

options(timeout = 3000)

source(here("scripts/downloadSourceFiles.R"))

srcDir <- here("sources")

urls <- c(
   "hp.obo" = "http://purl.obolibrary.org/obo/hp.obo",
   "phenotype.hpoa" = "http://purl.obolibrary.org/obo/hp/hpoa/phenotype.hpoa"
)

downloadSourceFiles(urls, srcDir)
