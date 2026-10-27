# Après la compilation : copie le PDF dans la Dropbox (Dependence/02_working_paper).
# Le chemin du dossier Dependence est dans .Renviron (DEPENDENCE_DIR), non versionné.
if (file.exists(".Renviron")) readRenviron(".Renviron")
dir <- file.path(Sys.getenv("DEPENDENCE_DIR"), "02_working_paper")
pdf <- strsplit(Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES"), "\n")[[1]]
pdf <- pdf[endsWith(pdf, ".pdf")]

if (Sys.getenv("DEPENDENCE_DIR") == "") {
  message("DEPENDENCE_DIR n'est pas défini dans .Renviron : PDF non copié dans la Dropbox.")
} else if (!dir.exists(dir)) {
  message("Dossier introuvable, PDF non copié : ", dir)
} else if (length(pdf) > 0) {
  file.copy(pdf, dir, overwrite = TRUE)
  message("PDF copié dans ", dir)
}
