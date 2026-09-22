# ============================================================================
# Tema y colores comunes para las figuras del libro
# Todos los scripts de figuras hacen source() de este archivo.
# ============================================================================
suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

# Carpeta de salida de las figuras (la fija quien ejecuta el script)
dir_figuras <- Sys.getenv("DIR_FIGURAS", "figuras")
dir.create(dir_figuras, showWarnings = FALSE, recursive = TRUE)

# Paleta categórica (en este orden, nunca cíclica; máximo 8 series)
paleta_libro <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                  "#e87ba4", "#008300", "#4a3aa7", "#e34948")
color_principal <- "#2a78d6"   # una sola serie
color_resalte   <- "#eb6834"   # para destacar un elemento
color_gris      <- "#b5b3ad"   # el resto, cuando se resalta uno

tema_libro <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", color = "#0b0b0b"),
      plot.subtitle    = element_text(color = "#52514e"),
      plot.caption     = element_text(color = "#52514e", size = rel(0.8)),
      axis.title       = element_text(color = "#52514e"),
      axis.text        = element_text(color = "#52514e"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e6e5e1", linewidth = 0.3),
      legend.position  = "top",
      legend.title     = element_text(color = "#52514e")
    )
}

# Guarda una figura en PDF con el tamaño estándar del libro
guardar_figura <- function(grafico, nombre, ancho = 6.5, alto = 3.8) {
  ggsave(file.path(dir_figuras, paste0(nombre, ".pdf")), grafico,
         width = ancho, height = alto, device = cairo_pdf)
}
