###############################################################################@
## Requirements ----
source("~/opt/KMT.R")
library(here)

tbkmh <- get_R_helpers(.tbkm)

rdir <- here("scripts")
expDir <- here("TBKM")
dir.create(expDir)

fmb <- read_fileMDB(here(), check=FALSE)
fmb <- add_km_spec(fmb, .tbkm)

###############################################################################@
## Helpers ----
# fmb <- add_helpers(
#    fmb,
#    code=file.path(rdir, "TBKM/helpers.R"),
#    name="R-Helpers", language="R",
#    kmr=.tbkm
# )

###############################################################################@
## Collibra ----
fmb <- tbkmh$add_collibra_metadata(
   fmb,
   `Alias`="HPO, Human Phenotype Ontology",
   `Domain`="Scientific Information",
   `Drug development stage`=c(
      "Target Identification",
      "Target Validation",
      "Hit finding",
      "Biomarker Development",
      "Post-marketing",
      "Clinical research and development",
      "Pre-clinical",
      "Lead generation (Hit to Lead)",
      "Lead optimization to candidate selection",
      "Regulatory approval"
   ),
   `Primary Use Case`="What are subforms of a disease and relate it to genetics, pharmacology and drugs associated to it? Whata are specific phenotypes associated with (rare) diseases of interest?",
   `Restrictions`="No Restrictions",
   `Restrictions summary`="None",
   `License type`="Open access with standard license agreement e.g. Creative Commons",
   `License`="Public access",
   `Data Protection Category`="Non-personal data",
   `Source of data`="Medical ontology of diseases",
   `Nature of data`="Disease, Phenotype and Condition",
   `Refresh Frequency`=as.character(NA),
   `Community`="Early Solutions External Data Catalog"
)

###############################################################################@
## Save Collibra metadata ----
toExport <- tbkmh$get_collibra_mdb(fmb)
f <- file.path(expDir, db_info(toExport)$name)
if(dir.exists(f)){
   file.rename(
      f,
      file.path(expDir, paste0(db_info(toExport)$name, "-BCK-", Sys.Date()))
   ) %>% invisible()
}
as_fileMDB(toExport, path=expDir, htmlModel = FALSE)

###############################################################################@
## Save TBKM spepc ----
toExport <- get_km_spec(fmb, .tbkm)
f <- file.path(expDir, db_info(toExport)$name)
if(dir.exists(f)){
   file.rename(
      f,
      file.path(expDir, paste0(db_info(toExport)$name, "-BCK-", Sys.Date()))
   ) %>% invisible()
}
as_fileMDB(toExport, path=expDir, htmlModel = FALSE)

