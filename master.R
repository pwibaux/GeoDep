## =============================================================================
## GEODEP - MASTER
## Runs the whole pipeline in order. Set each step to TRUE or FALSE below.
##   1. update_data.R  : copies the GeoDep datasets from Dropbox into 1_Data/
##   2. preparation.R  : builds 1_Data/geodep_shiny_inputs.rds
##   3. app.R          : runs the app locally (stop it to move on to step 4)
##   4. deploy.R       : publishes the app on Posit Connect Cloud
## Run with the project folder as the working directory.
## =============================================================================

RUN_UPDATE      <- TRUE
RUN_PREPARATION <- TRUE
RUN_APP         <- TRUE
RUN_DEPLOY      <- FALSE   # publishes online: switch on only when the app is checked

if (!file.exists("geodep_shiny.Rproj"))
  stop("Run master.R with the project folder as the working directory.")

## Each script runs in its own environment, so objects do not carry over
run_step <- function(file) {
  message("\n---- Running ", file, " ----")
  source(file, local = new.env(), encoding = "UTF-8")
}

## -----------------------------------------------------------------------
## 1. Update the data from Dropbox
## -----------------------------------------------------------------------

if (RUN_UPDATE) run_step("update_data.R")

## -----------------------------------------------------------------------
## 2. Prepare the app inputs
## -----------------------------------------------------------------------

if (RUN_PREPARATION) run_step("preparation.R")

## -----------------------------------------------------------------------
## 3. Run the app locally
## -----------------------------------------------------------------------

if (RUN_APP) {
  message("\n---- Running app.R (stop the app to continue) ----")
  shiny::runApp(".", launch.browser = TRUE)
}

## -----------------------------------------------------------------------
## 4. Deploy
## -----------------------------------------------------------------------

if (RUN_DEPLOY) {
  ## A .RData in the folder would be uploaded with the app
  if (file.exists(".RData")) stop("Delete .RData from the project folder before deploying.")
  run_step("deploy.R")
}
