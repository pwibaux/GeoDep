## Copies the latest GeoDep datasets from Dropbox into 1_Data/ as .rds files,
## which preparation.qmd then reads. Set DROPBOX_DIR in .Renviron.
library(haven)

## .Renviron is only read when R starts: load it here if R was started elsewhere
## (run this script with the project folder as working directory)
if (!nzchar(Sys.getenv("DROPBOX_DIR")) && file.exists(".Renviron")) readRenviron(".Renviron")
if (!nzchar(Sys.getenv("DROPBOX_DIR")))
  stop("DROPBOX_DIR is not set. Run setwd() to the project folder, or restart R from it.")

update_dataset <- function(stata_file, rds_file) {
  src <- file.path(Sys.getenv("DROPBOX_DIR"), "CEPII/Dependence/GeoDep_2024/Data/old", stata_file)
  if (!file.exists(src)) stop("Dataset not found. Check DROPBOX_DIR in .Renviron: ", src)
  dat <- read_dta(src)
  dat <- zap_labels(dat)   # or as_factor(dat) to keep Stata value labels as factors
  saveRDS(dat, file.path("1_Data", rds_file))
  message("Data updated from ", src, " (modified ", file.mtime(src), ")")
}

#Imports
update_dataset("geodep_M_hs22.dta", "geodep_M_hs22.rds")

#Exports
update_dataset("geodep_X_hs22.dta", "geodep_X_hs22.rds")
