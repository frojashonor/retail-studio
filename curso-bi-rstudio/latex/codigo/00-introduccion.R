# ==========================================================================
# Cómo usar este libro
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/00-introduccion.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Bloque 1 --------------------------------------------------------
# Ventas diarias de una sucursal durante una semana (en pesos)
ventas_semana <- c(1200, 950, 1830, 1100, 2400, 3100, 2800)
sum(ventas_semana)    # venta total de la semana
mean(ventas_semana)   # venta promedio por día

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # Instala un paquete (se hace UNA sola vez en cada computadora)
# install.packages("tidyverse")

# ---- Bloque 2 --------------------------------------------------------
# Números de día de un mes de 30 días
dias_mes <- 1:30
dias_mes

# ---- Bloque 3 --------------------------------------------------------
library(tibble)   # paquete de tablas modernas (parte del tidyverse)

# Una tabla pequeña: ventas de tres tiendas
ventas_tienda <- tibble(
  tienda = c("Centro", "Norte", "Sur"),
  ventas = c(152300, 98750, 120400)
)
ventas_tienda

# ---- Bloque 4 --------------------------------------------------------
# Ventas que llegaron como texto desde un sistema
ventas_texto <- c("1500", "2300", "N/D", "980")
ventas_numero <- as.numeric(ventas_texto)   # convertir a número
ventas_numero
sum(ventas_numero)                 # la suma con un NA da NA
sum(ventas_numero, na.rm = TRUE)   # na.rm = TRUE ignora los NA

# ---- Bloque 5 --------------------------------------------------------
# Número de tickets (compras) registrados cada día de la semana
tickets_semana <- c(40, 31, 55, 38, 70, 88, 81)

# Ticket promedio = venta total / número total de tickets
ticket_promedio <- sum(ventas_semana) / sum(tickets_semana)
round(ticket_promedio, 2)   # redondeamos a centavos

# ---- Solo referencia (no se ejecuta automáticamente) ----
# paquetes <- c(
#   # --- Importar, manipular, visualizar y explorar (Módulos 1 a 4) ---
#   "tidyverse",      # dplyr, ggplot2, tidyr, readr, tibble, stringr...
#   "readxl",         # leer archivos de Excel (.xlsx y .xls)
#   "writexl",        # escribir archivos de Excel
#   "lubridate",      # trabajar con fechas: meses, semanas, días
#   "scales",         # formato de ejes: $, %, separador de miles
#   "skimr",          # resumen rápido de una tabla completa
#   "corrplot",       # gráficas de matrices de correlación
#   # --- Bases de datos y SQL (Módulo 5) ---
#   "DBI",            # interfaz común para cualquier base de datos
#   "RSQLite",        # base de datos SQLite en un archivo local
#   "dbplyr",         # traducir verbos de dplyr a SQL
#   # --- Dashboards interactivos (Módulo 6) ---
#   "shiny",          # aplicaciones web con R
#   "shinydashboard", # plantilla de dashboard para Shiny
#   "DT",             # tablas interactivas: buscar, ordenar, filtrar
#   "plotly",         # gráficas interactivas
#   # --- Machine learning (Módulo 7) ---
#   "cluster",        # segmentación (clustering) y silueta
#   "factoextra",     # visualizar clusters y componentes principales
#   "forecast",       # series de tiempo y pronósticos
#   "rpart",          # árboles de decisión
#   "randomForest",   # bosques aleatorios
#   "caret",          # entrenar y evaluar modelos
#   # --- Reportes automáticos (Módulo 8) ---
#   "rmarkdown",      # reportes en HTML, Word y PDF
#   "knitr"           # motor que ejecuta el código de los reportes
# )
#
# # Instala solo los paquetes que todavía no tienes
# faltantes <- setdiff(paquetes, rownames(installed.packages()))
# install.packages(faltantes)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# install.packages(c(
#   "arules",      # reglas de asociación (análisis de canasta de compras)
#   "rpart.plot",  # dibujos más legibles de árboles de decisión
#   "janitor",     # limpiar nombres de columnas y tablas de frecuencia
#   "openxlsx"     # Excel con formato: colores, anchos, estilos
# ))

# ---- Solo referencia (no se ejecuta automáticamente) ----
# getwd()   # muestra el directorio de trabajo actual

# ---- Bloque 6 --------------------------------------------------------
source("datasets/generar_datasets.R")   # ejecuta el script completo

# ---- Bloque 7 --------------------------------------------------------
Sys.getlocale("LC_TIME")   # idioma y región que R usa para las fechas

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # Windows
# Sys.setlocale("LC_TIME", "Spanish")
# # macOS y Linux
# Sys.setlocale("LC_TIME", "es_MX.UTF-8")
#
# source("datasets/generar_datasets.R")   # vuelve a generar los datos

# ---- Bloque 8 --------------------------------------------------------
library(tidyverse)   # carga dplyr, ggplot2, readr y compañía

# ---- 1. Versión de R ----
cat("Versión de R:", as.character(getRversion()), "\n\n")

# ---- 2. Paquetes del curso: ¿se pueden cargar? ----
paquetes_curso <- c("tidyverse", "readxl", "writexl", "lubridate",
                    "scales", "DBI", "RSQLite", "dbplyr", "shiny",
                    "shinydashboard", "DT", "plotly", "cluster",
                    "factoextra", "forecast", "rpart", "randomForest",
                    "caret", "rmarkdown", "knitr", "skimr", "corrplot")
problemas <- 0                         # contador de problemas

cat("Paquetes:\n")
for (p in paquetes_curso) {            # revisa los paquetes uno por uno
  if (requireNamespace(p, quietly = TRUE)) {        # ¿carga bien?
    version <- as.character(packageVersion(p))
    cat(sprintf("  OK     %-15s %s\n", p, version))
  } else {
    problemas <- problemas + 1
    cat(sprintf("  FALTA  %-15s instálalo (paso 4)\n", p))
  }
}

# ---- 3. Archivos de datos ----
cat("\nArchivos en datasets/:\n")
tablas <- c("ventas_retail", "clientes", "productos", "empleados",
            "transacciones", "tiendas")
for (t in tablas) {
  archivo_csv <- file.path("datasets", paste0(t, ".csv"))
  if (file.exists(archivo_csv)) {
    datos <- read_csv(archivo_csv, show_col_types = FALSE)
    cat(sprintf("  OK     %-18s %5d filas %3d columnas\n",
                basename(archivo_csv), nrow(datos), ncol(datos)))
  } else {
    problemas <- problemas + 1
    cat(sprintf("  FALTA  %s (ejecuta el paso 6)\n", archivo_csv))
  }
}
hojas <- readxl::excel_sheets("datasets/datos_empresa.xlsx")
cat("  OK     datos_empresa.xlsx con hojas:\n        ",
    paste(hojas, collapse = ", "), "\n")

# ---- 4. Veredicto ----
if (problemas == 0) {
  cat("\n¡Todo listo! Tu entorno está preparado para el curso.\n")
} else {
  cat("\nHay", problemas, "problema(s): revisa las líneas FALTA.\n")
}

# ---- Bloque 9 --------------------------------------------------------
# Leer la tabla de ventas
ventas <- read_csv("datasets/ventas_retail.csv", show_col_types = FALSE)

ventas %>%                                  # toma las ventas, y luego
  group_by(trimestre) %>%                   # agrúpalas por trimestre,
  summarise(ventas_totales = sum(total),    # suma el total vendido,
            numero_ventas = n(),            # cuenta las ventas
            ticket_promedio = mean(total),  # y promedia su importe
            .groups = "drop")

# ---- Bloque 10 --------------------------------------------------------
# Leemos las tablas que vamos a revisar
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)
clientes  <- read_csv("datasets/clientes.csv", show_col_types = FALSE)
empleados <- read_csv("datasets/empleados.csv", show_col_types = FALSE)

# Hallazgo 1: ¿cuántos productos tienen margen negativo?
sum(productos$margen_pct < 0)

# Hallazgo 2: ¿cuántas ventas son de productos inactivos?
ventas %>%
  left_join(productos, by = "producto_id") %>%  # agrega datos del producto
  filter(activo == FALSE) %>%                   # solo productos inactivos
  nrow()                                        # cuenta las filas

# Hallazgo 3: ¿cuántas ventas ocurrieron ANTES del registro del cliente?
ventas %>%
  left_join(clientes, by = "cliente_id") %>%    # agrega datos del cliente
  filter(fecha < fecha_registro) %>%            # compra antes del alta
  nrow()

# ---- Bloque 11 --------------------------------------------------------
# Hallazgo 4: el nombre y el correo no corresponden a la misma persona
clientes %>%
  select(cliente_id, nombre, email) %>%
  head(3)

# Hallazgo 5: ¿de qué departamento son los "vendedores" 1 a 25?
empleados %>%
  filter(empleado_id <= 25) %>%
  count(departamento, sort = TRUE)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# install.packages("tinytex")
# tinytex::install_tinytex()                  # solo la primera vez
# setwd("latex")                              # carpeta del libro
# tinytex::latexmk("curso-bi-rstudio.tex")    # compila con pdfLaTeX

# ---- Solo referencia (no se ejecuta automáticamente) ----
# ?mean                      # ayuda de una función (o F1 sobre su nombre)
# help("read_csv")           # lo mismo, escrito de otra forma
# ??"regression"             # busca un tema en todas las ayudas instaladas
# example(mean)              # ejecuta los ejemplos de la ayuda
# args(round)                # muestra solo los argumentos de una función
# vignette("dplyr")          # guía larga (viñeta) de un paquete
