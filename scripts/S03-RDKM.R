###############################################################################@
## Requirements ----
source("~/opt/KMT.R")
library(here)

# rdkmh <- get_R_helpers(rdkm)
# 
# rdir <- here("scripts")
# expDir <- here("RDKM")
# dir.create(expDir)
# 
# fmb <- read_fileMDB(here(), check=FALSE)
# fmb <- add_km_spec(fmb, rdkm)
# 
# ###############################################################################@
# ## Helpers ----
# # fmb <- add_helpers(
# #    fmb,
# #    code=file.path(rdir, "RDKM/helpers.R"),
# #    name="R-Helpers", language="R",
# #    kmr=rdkm
# # )
# 
# ###############################################################################@
# ## Save RDKM spepc ----
# toExport <- get_km_spec(fmb, rdkm)
# f <- file.path(expDir, db_info(toExport)$name)
# if(dir.exists(f)){
#    file.rename(
#       f,
#       file.path(expDir, paste0(db_info(toExport)$name, "-BCK-", Sys.Date()))
#    ) %>% invisible()
# }
# as_fileMDB(toExport, path=expDir, htmlModel = FALSE)
# 
