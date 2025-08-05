# ===================================================
# Dependencias del proyecto
# ===================================================
paquetes <- c(
  "here",
  "frontier",
  "tidyverse",
  "patchwork",
  "openxlsx",
  "readxl",
  "rstatix",
  "writexl",
  "ggplot2",
  "randomForest",
  "caret",
  "RColorBrewer",
  "rnaturalearthdata",
  "rnaturalearth",
  "chilemapas",
  "purrr",
  "Metrics"
)

cat("=== Instalando paquetes requeridos ===\n")
for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) {
    cat(paste0("Instalando ", p, "...\n"))
    install.packages(p, dependencies = TRUE)
  } else {
    cat(paste0(p, " ya está instalado.\n"))
  }
}
cat("=== Todos los paquetes han sido instalados ===\n")
