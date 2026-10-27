# Après la compilation : copie le PDF dans la Dropbox (Dependence/02_working_paper).
# Dossier Dependence selon l'utilisateur, comme en tête des do-files.
dependence <- c(lefebvre      = "C:/Users/lefebvre.LENOVOM90117/CIREM Dropbox/kévin LEFEBVRE/Dependence",
                Kevin         = "C:/Users/Kevin/CIREM Dropbox/kévin LEFEBVRE/Dependence",
                wibaux        = "d:/Dropbox/CEPII/Dependence",
                paulinewibaux = "/Users/paulinewibaux/Dropbox/CEPII/Dependence")

user <- Sys.info()[["user"]]
dir  <- file.path(dependence[user], "02_working_paper")
pdf  <- strsplit(Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES"), "\n")[[1]]
pdf  <- pdf[endsWith(pdf, ".pdf")]

if (!user %in% names(dependence)) {
  message("Utilisateur ", user, " inconnu dans R/copy_pdf.R : PDF non copié dans la Dropbox.")
} else if (!dir.exists(dir)) {
  message("Dossier introuvable, PDF non copié : ", dir)
} else if (length(pdf) > 0) {
  file.copy(pdf, dir, overwrite = TRUE)
  message("PDF copié dans ", dir)
}
