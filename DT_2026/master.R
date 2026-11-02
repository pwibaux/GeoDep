## =============================================================================
## GEODEP WORKING PAPER - MASTER
## Runs the R scripts that are not called by the qmd, then renders the paper.
## Set each step to TRUE or FALSE below.
##   1. independent R scripts         : none for now
##   2. trade_dependencies_geodep.qmd : renders the paper (PDF and HTML)
## Run with DT_2026 as the working directory.
## =============================================================================

RUN_RENDER <- TRUE

if (!file.exists("trade_dependencies_geodep.qmd"))
  stop("Run master.R with DT_2026 as the working directory.")

## Each script runs in its own environment, so objects do not carry over
run_step <- function(file) {
  message("\n---- Running ", file, " ----")
  source(file, local = new.env(), encoding = "UTF-8")
}

## -----------------------------------------------------------------------
## 1. Independent R scripts (add them here, e.g. run_step("R/script.R"))
## -----------------------------------------------------------------------

## -----------------------------------------------------------------------
## 2. Render the paper
## -----------------------------------------------------------------------

## Quarto: from the PATH, otherwise from its usual install folders
quarto <- Sys.which("quarto")
if (quarto == "") {
  quarto <- c("C:/Program Files/Quarto/bin/quarto.exe",
              file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Quarto/bin/quarto.exe"),
              "/Applications/quarto/bin/quarto")
  quarto <- quarto[file.exists(quarto)][1]
}
if (is.na(quarto)) stop("Quarto not found: add its folder to the PATH.")

if (RUN_RENDER) system2(quarto, c("render", "trade_dependencies_geodep.qmd"))
