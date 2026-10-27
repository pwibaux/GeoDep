## =============================================================================
## GEODEP - MAP CAPTURES
## Saves the world maps (no country selected) as PNG files in a Dropbox folder,
## for use in documents. The maps are drawn by the app's own make_map_plot(),
## so they match the "Download map (PNG)" button exactly.
## Run with the project folder as the working directory, after preparation.R.
## =============================================================================

## Dropbox folder for the PNG files, relative to DROPBOX_DIR (set in .Renviron)
OUT_SUBDIR <- "CEPII/Dependence/GeoDep_2024/Output/explorer"   # to be checked

## Maps to save: direction ("import" / "export") and metric ("count" / "value")
MAPS <- data.frame(
  direction = c("import", "export"),
  metric    = c("count",  "count")
)

## FALSE: no title, subtitle or source on the image (Quarto adds the caption)
KEEP_TITLES <- FALSE

## -----------------------------------------------------------------------

if (!nzchar(Sys.getenv("DROPBOX_DIR")) && file.exists(".Renviron")) readRenviron(".Renviron")
if (!nzchar(Sys.getenv("DROPBOX_DIR")))
  stop("DROPBOX_DIR is not set. Run setwd() to the project folder, or restart R from it.")

out_dir <- file.path(Sys.getenv("DROPBOX_DIR"), OUT_SUBDIR)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

## testServer() runs the app's server code without a browser: the inputs are set
## by hand and make_map_plot() is called as the download button would
shiny::testServer(shiny::shinyAppDir("."), {
  for (i in seq_len(nrow(MAPS))) {
    session$setInputs(dep_direction = MAPS$direction[i],
                      map_metric    = MAPS$metric[i],
                      sector_filter = "all")
    p <- make_map_plot()
    if (!KEEP_TITLES) p <- p + labs(title = NULL, subtitle = NULL, caption = NULL)
    file <- file.path(out_dir, paste0("GeoDep_map_", MAPS$direction[i], "_",
                                      MAPS$metric[i], "_2024.png"))
    ggsave(filename = file, plot = p, device = "png",
           width = 10, height = if (KEEP_TITLES) 6.6 else 5.6, dpi = 300, bg = "white")
    message("Saved ", file)
  }
})
