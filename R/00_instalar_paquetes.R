# =============================================================================
# 00_instalar_paquetes.R
# Instala los paquetes que faltan. Solo hace falta ejecutarlo una vez:
#   Rscript R/00_instalar_paquetes.R
# =============================================================================

paquetes <- c(
  # Análisis
  "readr", "dplyr", "ggplot2", "fitdistrplus", "actuar", "corrplot",
  "GGally", "scatterplot3d", "survival", "survminer",
  # Para que VS Code entienda R (autocompletado, ayuda, errores)
  "languageserver"
)

# Instalamos en la librería personal del usuario. La librería del sistema
# suele no tener permisos de escritura (sobre todo en macOS).
lib_usuario <- Sys.getenv("R_LIBS_USER")
if (lib_usuario == "") {
  # Si R no define la variable, usamos la ruta habitual de la librería personal
  lib_usuario <- file.path(Sys.getenv("HOME"), "R", "library")
  message("R_LIBS_USER no está definida. Se instalará en: ", lib_usuario)
}
dir.create(lib_usuario, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_usuario, .libPaths()))

faltan <- paquetes[!paquetes %in% rownames(installed.packages())]

if (length(faltan) > 0) {
  cat("Instalando:", paste(faltan, collapse = ", "), "\n")
  install.packages(faltan, lib = lib_usuario, repos = "https://cloud.r-project.org")
} else {
  cat("Todos los paquetes ya están instalados.\n")
}
