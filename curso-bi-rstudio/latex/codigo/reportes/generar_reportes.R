# ========================================================================
# generar_reportes.R
# Genera el reporte ejecutivo de ventas (02-reporte-ejecutivo.Rmd) para
# CADA tienda, guarda los archivos en resultados/reportes/ y deja un
# registro (log) de lo que se generó y de lo que falló.
#
# Uso (con el directorio de trabajo en la carpeta del curso, la que
# contiene datasets/ y reportes/):
#   - Desde RStudio:     source("reportes/generar_reportes.R")
#   - Desde la terminal: Rscript reportes/generar_reportes.R 2023-12
#                        Rscript reportes/generar_reportes.R 2023-12 pdf
# ========================================================================

suppressPackageStartupMessages({
  library(rmarkdown)
  library(readr)
  library(dplyr)
})

# ---- 1. Configuración --------------------------------------------------
# Argumentos opcionales de la terminal: mes (AAAA-MM) y formato
argumentos  <- commandArgs(trailingOnly = TRUE)
mes_reporte <- if (length(argumentos) >= 1) argumentos[1] else "2023-12"
formato     <- if (length(argumentos) >= 2) argumentos[2] else "html"
# En producción usarías el mes anterior a hoy, por ejemplo:
# mes_reporte <- format(lubridate::floor_date(Sys.Date(), "month") - 1,
#                       "%Y-%m")

# Formatos disponibles: nombre corto -> formato de R Markdown y extensión
formatos <- list(
  html = c("html_document", "html"),
  pdf  = c("pdf_document", "pdf"),
  word = c("word_document", "docx")
)
if (!formato %in% names(formatos)) {
  stop("Formato no válido: ", formato, ". Usa html, pdf o word.")
}

# Carpeta del curso: la actual o la superior (si ejecutas desde reportes/)
raiz <- if (dir.exists("datasets")) "." else
        if (dir.exists("../datasets")) ".."
if (is.null(raiz)) {
  stop("No encuentro datasets/. Ejecuta desde la carpeta del curso.")
}
raiz <- normalizePath(raiz)

plantilla      <- file.path(raiz, "reportes", "02-reporte-ejecutivo.Rmd")
carpeta_salida <- file.path(raiz, "resultados", "reportes")
dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)
archivo_log    <- file.path(carpeta_salida, "registro_reportes.csv")
fecha_hoy      <- format(Sys.Date(), "%Y-%m-%d")

# ---- 2. Lista de tiendas -----------------------------------------------
tiendas <- read_csv(file.path(raiz, "datasets", "tiendas.csv"),
                    show_col_types = FALSE)

# Convierte "Sucursal Plaza A" en "sucursal-plaza-a" (sin acentos ni
# espacios) para usarlo en el nombre del archivo
limpiar_nombre <- function(x) {
  x <- chartr("áéíóúüñÁÉÍÓÚÜÑ", "aeiouunAEIOUUN", x)
  x <- gsub("[^A-Za-z0-9]+", "-", x)
  tolower(gsub("^-|-$", "", x))
}

# ---- 3. Bucle: un reporte por tienda -----------------------------------
cat("Generando", nrow(tiendas), "reportes del mes", mes_reporte,
    "en formato", formato, "\n")
registro <- vector("list", nrow(tiendas))

for (i in seq_len(nrow(tiendas))) {
  id     <- tiendas$tienda_id[i]
  nombre <- tiendas$nombre_tienda[i]
  archivo <- sprintf("%s_T%02d_%s_%s.%s", mes_reporte, id,
                     limpiar_nombre(nombre), fecha_hoy,
                     formatos[[formato]][2])
  inicio <- Sys.time()

  # tryCatch: si UNA tienda falla, se registra el error y seguimos
  resultado <- tryCatch({
    render(
      input         = plantilla,
      output_format = formatos[[formato]][1],
      output_file   = archivo,
      output_dir    = carpeta_salida,
      params        = list(tienda = id, mes = mes_reporte),
      knit_root_dir = raiz,       # el código del .Rmd corre en la raíz
      envir         = new.env(),  # entorno limpio para cada reporte
      quiet         = TRUE
    )
    "OK"
  }, error = function(e) {
    paste("ERROR:", conditionMessage(e))
  })

  segundos <- round(as.numeric(difftime(Sys.time(), inicio,
                                        units = "secs")), 1)
  registro[[i]] <- tibble(
    fecha_hora = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    tienda_id  = id,
    tienda     = nombre,
    mes        = mes_reporte,
    archivo    = archivo,
    estado     = if (resultado == "OK") "OK" else "ERROR",
    detalle    = resultado,
    segundos   = segundos
  )
  cat(sprintf("  [%2d/%d] %-18s %-5s %s\n", i, nrow(tiendas), nombre,
              registro[[i]]$estado, archivo))
}

# ---- 4. Registro (log) -------------------------------------------------
registro <- bind_rows(registro)
# append = TRUE agrega al historial; los encabezados solo la primera vez
write_csv(registro, archivo_log, append = file.exists(archivo_log))

n_ok <- sum(registro$estado == "OK")
cat("\nListo:", n_ok, "reportes generados,", nrow(registro) - n_ok,
    "con error. Carpeta:", carpeta_salida, "\n")
cat("Registro guardado en:", archivo_log, "\n")
