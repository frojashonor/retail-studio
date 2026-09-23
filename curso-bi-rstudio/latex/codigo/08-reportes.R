# ==========================================================================
# Reportes automáticos con R Markdown
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/08-reportes.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Bloque 1 --------------------------------------------------------
library(tidyverse)   # dplyr, readr, ggplot2, tidyr, purrr...
library(knitr)       # knit() y kable()
library(scales)      # dollar(), percent(), comma()

# Un mini documento R Markdown guardado en un vector de texto
rmd_mini <- c(
  "Este año vendimos `r format(1234567, big.mark = ',')` pesos.",
  "",
  "```{r suma}",
  "sum(1:10)",
  "```"
)
# knitr ejecuta el código y devuelve Markdown puro (lo que ve pandoc)
cat(knit(text = rmd_mini, quiet = TRUE))

# ---- Solo referencia (no se ejecuta automáticamente) ----
# install.packages(c("rmarkdown", "knitr", "writexl", "readxl"))

# ---- Bloque 2 --------------------------------------------------------
ventas <- read_csv("datasets/ventas_retail.csv", show_col_types = FALSE)

nrow(ventas)                                   # número de ventas
format(round(sum(ventas$total)), big.mark = ",")  # total con comas

# ---- Bloque 3 --------------------------------------------------------
doc_opciones <- c(
  "```{r a, echo = FALSE}",
  "sum(1:10)",
  "```",
  "",
  "```{r b, include = FALSE}",
  "total <- sum(1:10)",
  "```",
  "",
  "El total es `r total`."
)
cat(knit(text = doc_opciones, quiet = TRUE))

# ---- Bloque 4 --------------------------------------------------------
doc_asis <- c(
  "```{r lista, echo = FALSE, results = 'asis'}",
  "tiendas_top <- c('Sucursal Outlet', 'Sucursal Sur')",
  "cat(paste0('- **', tiendas_top, '**'), sep = '\\n')",
  "```"
)
cat(knit(text = doc_asis, quiet = TRUE))

# ---- Bloque 5 --------------------------------------------------------
# Datos que usaremos en el resto del módulo
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
tiendas   <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)

# Resumen de diciembre por tienda
ventas_dic <- transacciones %>%
  filter(mes == "2023-12") %>%
  group_by(tienda_id) %>%
  summarise(transacciones = n(),
            ventas = sum(total_transaccion),
            ticket = mean(total_transaccion), .groups = "drop") %>%
  left_join(tiendas %>% select(tienda_id, nombre_tienda, ciudad),
            by = "tienda_id") %>%
  select(nombre_tienda, ciudad, transacciones, ventas, ticket) %>%
  arrange(desc(ventas))

ventas_dic

# ---- Bloque 6 --------------------------------------------------------
kable(ventas_dic,
      format = "pipe",                        # Markdown (para verla aquí)
      col.names = c("Tienda", "Ciudad", "Transacciones",
                    "Ventas ($)", "Ticket ($)"),  # encabezados legibles
      digits = 0,                             # sin decimales
      format.args = list(big.mark = ","),     # separador de miles
      align = c("l", "l", "r", "r", "r"),     # texto izq., números der.
      caption = "Ventas de diciembre de 2023 por tienda")

# ---- Bloque 7 --------------------------------------------------------
tabla_dic <- ventas_dic %>%
  mutate(participacion = ventas / sum(ventas),       # % del total
         ventas        = dollar(ventas, accuracy = 1),
         ticket        = dollar(ticket, accuracy = 0.01),
         participacion = percent(participacion, accuracy = 0.1)) %>%
  select(-ciudad) %>%
  slice_head(n = 5)                                  # top 5

kable(tabla_dic, format = "pipe", align = c("l", "r", "r", "r", "r"),
      col.names = c("Tienda", "Transacciones", "Ventas", "Ticket",
                    "% del total"))

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # kableExtra: añade estilo a kable() (HTML y PDF)
# install.packages("kableExtra")
# library(kableExtra)
# kable(tabla_dic) %>%
#   kable_styling(bootstrap_options = c("striped", "hover")) %>%
#   row_spec(1, bold = TRUE, background = "#fde6dc")   # resalta la 1a fila
#
# # gt: "gramática de tablas" de Posit, muy flexible
# install.packages("gt")
# library(gt)
# ventas_dic %>%
#   gt() %>%
#   fmt_currency(columns = c(ventas, ticket)) %>%
#   tab_header(title = "Ventas de diciembre de 2023")

# ---- Bloque 8 --------------------------------------------------------
x <- 96393.537
dollar(x)                       # pesos, con separador de miles
dollar(x, accuracy = 1)         # sin centavos
dollar(x, scale = 1e-3, suffix = " mil", accuracy = 0.1)
comma(1234567)                  # separador de miles
percent(0.19731, accuracy = 0.1)
format(x, big.mark = ",", nsmall = 2)          # con base R
format(as.Date("2023-12-15"), "%d de %B de %Y")  # fecha en texto

# ---- Bloque 9 --------------------------------------------------------
frase_variacion <- function(actual, anterior, periodo = "el mes anterior") {
  # Caso 1: no hay con qué comparar
  if (is.na(anterior) || anterior == 0) {
    return(paste0("Las ventas fueron de ", dollar(actual, accuracy = 1),
                  "; no hay datos para comparar con ", periodo, "."))
  }
  var <- actual / anterior - 1                   # variación relativa
  # Caso 2, 3 y 4: la palabra depende del signo
  verbo <- if (abs(var) < 0.005) {
    "se mantuvieron sin cambio"
  } else if (var > 0) {
    paste("crecieron", percent(var, accuracy = 0.1))
  } else {
    paste("cayeron", percent(abs(var), accuracy = 0.1))
  }
  paste0("Las ventas ", verbo, " en comparación con ", periodo, " (de ",
         dollar(anterior, accuracy = 1), " a ",
         dollar(actual, accuracy = 1), ").")
}

# Probamos los cuatro casos
frase_variacion(12000, 10000)
frase_variacion(8000, 10000)
frase_variacion(10020, 10000)
frase_variacion(5000, NA)

# ---- Bloque 10 --------------------------------------------------------
ventas_mes <- transacciones %>%
  group_by(mes) %>%
  summarise(ventas = sum(total_transaccion), .groups = "drop")

actual   <- ventas_mes$ventas[ventas_mes$mes == "2023-12"]
anterior <- ventas_mes$ventas[ventas_mes$mes == "2023-11"]
frase_variacion(actual, anterior, periodo = "noviembre")

# ---- Bloque 11 --------------------------------------------------------
comparacion <- transacciones %>%
  filter(mes %in% c("2023-11", "2023-12")) %>%
  group_by(tienda_id, mes) %>%
  summarise(ventas = sum(total_transaccion), .groups = "drop") %>%
  pivot_wider(names_from = mes, values_from = ventas) %>%
  rename(nov = `2023-11`, dic = `2023-12`) %>%
  left_join(tiendas %>% select(tienda_id, nombre_tienda),
            by = "tienda_id") %>%
  mutate(frase = map2_chr(dic, nov, frase_variacion,
                          periodo = "noviembre"))

# Imprimimos las frases de las 3 primeras tiendas
cat(paste0(comparacion$nombre_tienda[1:3], ": ",
           comparacion$frase[1:3]), sep = "\n")

# ---- Bloque 12 --------------------------------------------------------
alertas <- comparacion %>%
  mutate(variacion = dic / nov - 1,
         estado = case_when(
           variacion >  0.10 ~ "Crecimiento sólido",
           variacion >= 0    ~ "Estable",
           variacion > -0.10 ~ "Ligera caída",
           TRUE              ~ "Alerta"
         )) %>%
  arrange(variacion) %>%
  select(nombre_tienda, variacion, estado)

alertas %>%
  mutate(variacion = percent(variacion, accuracy = 0.1)) %>%
  kable(format = "pipe", col.names = c("Tienda", "Variación", "Estado"))

# ---- Bloque 13 --------------------------------------------------------
en_alerta <- alertas %>% filter(estado == "Alerta")

if (nrow(en_alerta) == 0) {
  cat("- Ninguna tienda cayó más de 10 % este mes.\n")
} else {
  cat(paste0("- **", en_alerta$nombre_tienda, "**: cayó ",
             percent(abs(en_alerta$variacion), accuracy = 0.1),
             " contra noviembre."), sep = "\n")
}

# ---- Solo referencia (no se ejecuta automáticamente) ----
# install.packages("tinytex")
# tinytex::install_tinytex()   # descarga e instala LaTeX (una sola vez)
# tinytex::is_tinytex()        # TRUE si quedó instalado

# ---- Solo referencia (no se ejecuta automáticamente) ----
# library(rmarkdown)
# render("reportes/02-reporte-ejecutivo.Rmd",
#        output_format = "pdf_document")   # solo PDF
# render("reportes/02-reporte-ejecutivo.Rmd",
#        output_format = "all")            # HTML, PDF y Word

# ---- Bloque 14 --------------------------------------------------------
# En la consola simulamos los parámetros que recibiría el .Rmd
params <- list(tienda = 3, mes = "2023-12")

id_tienda  <- as.integer(params$tienda)
mes_actual <- params$mes

# Validaciones: mejor un error claro que un reporte vacío
info_tienda <- tiendas %>% filter(tienda_id == id_tienda)
if (nrow(info_tienda) == 0) stop("La tienda ", id_tienda, " no existe.")
if (!mes_actual %in% transacciones$mes) {
  stop("No hay datos del mes ", mes_actual)
}

# Mes anterior: el día 1 del mes menos un día cae en el mes previo
mes_previo <- format(as.Date(paste0(mes_actual, "-01")) - 1, "%Y-%m")

info_tienda$nombre_tienda
mes_previo

# ---- Solo referencia (no se ejecuta automáticamente) ----
# library(rmarkdown)
# # Reporte de la Sucursal Sur (tienda 3), diciembre de 2023, en PDF
# render("reportes/02-reporte-ejecutivo.Rmd",
#        output_format = "pdf_document",
#        output_file   = "reporte_T03_2023-12.pdf",
#        output_dir    = "resultados/reportes",
#        params        = list(tienda = 3, mes = "2023-12"),
#        knit_root_dir = getwd())

# ---- Bloque 15 --------------------------------------------------------
# "Sucursal Plaza A" -> "sucursal-plaza-a" (sin acentos ni espacios)
limpiar_nombre <- function(x) {
  x <- chartr("áéíóúüñÁÉÍÓÚÜÑ", "aeiouunAEIOUUN", x)  # quita acentos
  x <- gsub("[^A-Za-z0-9]+", "-", x)     # lo que no sea letra -> guion
  tolower(gsub("^-|-$", "", x))          # sin guiones al inicio/final
}
limpiar_nombre(c("Sucursal Plaza A", "Tienda Querétaro Ñuñoa"))

# En el script real la fecha es Sys.Date(); aquí la fijamos para que
# el ejemplo dé siempre el mismo resultado
fecha_ejecucion <- as.Date("2024-01-05")
mes_reporte <- "2023-12"

archivos <- sprintf("%s_T%02d_%s_%s.html", mes_reporte,
                    tiendas$tienda_id, limpiar_nombre(tiendas$nombre_tienda),
                    format(fecha_ejecucion, "%Y-%m-%d"))
head(archivos, 4)

# ---- Bloque 16 --------------------------------------------------------
trabajos <- expand_grid(tienda_id = tiendas$tienda_id,
                        mes = c("2023-11", "2023-12")) %>%
  left_join(tiendas %>% select(tienda_id, nombre_tienda),
            by = "tienda_id") %>%
  mutate(archivo = sprintf("%s_T%02d_%s.html", mes, tienda_id,
                           limpiar_nombre(nombre_tienda)))

nrow(trabajos)
head(trabajos, 3)

# La lista de parámetros de la primera fila, lista para render()
parametros <- list(tienda = trabajos$tienda_id[1],
                   mes = trabajos$mes[1])
str(parametros)

# ---- Bloque 17 --------------------------------------------------------
# Simula render(): falla igual que el .Rmd cuando algo no cuadra
generar_uno <- function(tienda, mes) {
  if (!tienda %in% tiendas$tienda_id) {
    stop("La tienda ", tienda, " no existe.")
  }
  if (!mes %in% transacciones$mes) stop("No hay datos del mes ", mes)
  sprintf("reporte_T%02d_%s.html", tienda, mes)
}

pendientes <- tibble(tienda = c(1, 2, 15, 3),
                     mes = c("2023-12", "2023-12", "2023-12", "2024-01"))
registro <- vector("list", nrow(pendientes))

for (fila in seq_len(nrow(pendientes))) {
  resultado <- tryCatch({
    archivo <- generar_uno(pendientes$tienda[fila], pendientes$mes[fila])
    paste("OK:", archivo)
  }, error = function(e) {
    paste("ERROR:", conditionMessage(e))   # el mensaje del error
  })
  registro[[fila]] <- tibble(tienda = pendientes$tienda[fila],
                             mes = pendientes$mes[fila],
                             estado = if (startsWith(resultado, "OK"))
                               "OK" else "ERROR",
                             detalle = resultado)
}
registro <- bind_rows(registro)
registro

# ---- Solo referencia (no se ejecuta automáticamente) ----
# suppressPackageStartupMessages({
#   library(rmarkdown)
#   library(readr)
#   library(dplyr)
# })
#
# # ---- 1. Configuración -----------------------------------------------
# # Argumentos opcionales de la terminal: mes (AAAA-MM) y formato
# argumentos  <- commandArgs(trailingOnly = TRUE)
# mes_reporte <- if (length(argumentos) >= 1) argumentos[1] else "2023-12"
# formato     <- if (length(argumentos) >= 2) argumentos[2] else "html"
# # En producción usarías el mes anterior a hoy, por ejemplo:
# # mes_reporte <- format(lubridate::floor_date(Sys.Date(), "month") - 1,
# #                       "%Y-%m")
#
# # Formatos disponibles: nombre corto -> formato de R Markdown y extensión
# formatos <- list(
#   html = c("html_document", "html"),
#   pdf  = c("pdf_document", "pdf"),
#   word = c("word_document", "docx")
# )
# if (!formato %in% names(formatos)) {
#   stop("Formato no válido: ", formato, ". Usa html, pdf o word.")
# }
#
# # Carpeta del curso: la actual o la superior (si ejecutas desde reportes/)
# raiz <- if (dir.exists("datasets")) "." else
#         if (dir.exists("../datasets")) ".."
# if (is.null(raiz)) {
#   stop("No encuentro datasets/. Ejecuta desde la carpeta del curso.")
# }
# raiz <- normalizePath(raiz)
#
# plantilla      <- file.path(raiz, "reportes", "02-reporte-ejecutivo.Rmd")
# carpeta_salida <- file.path(raiz, "resultados", "reportes")
# dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)
# archivo_log    <- file.path(carpeta_salida, "registro_reportes.csv")
# fecha_hoy      <- format(Sys.Date(), "%Y-%m-%d")

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # ---- 2. Lista de tiendas --------------------------------------------
# tiendas <- read_csv(file.path(raiz, "datasets", "tiendas.csv"),
#                     show_col_types = FALSE)
# # (aquí va la función limpiar_nombre() que ya viste)
#
# # ---- 3. Bucle: un reporte por tienda --------------------------------
# cat("Generando", nrow(tiendas), "reportes del mes", mes_reporte,
#     "en formato", formato, "\n")
# registro <- vector("list", nrow(tiendas))
#
# for (i in seq_len(nrow(tiendas))) {
#   id     <- tiendas$tienda_id[i]
#   nombre <- tiendas$nombre_tienda[i]
#   archivo <- sprintf("%s_T%02d_%s_%s.%s", mes_reporte, id,
#                      limpiar_nombre(nombre), fecha_hoy,
#                      formatos[[formato]][2])
#   inicio <- Sys.time()
#
#   # tryCatch: si UNA tienda falla, se registra el error y seguimos
#   resultado <- tryCatch({
#     render(
#       input         = plantilla,
#       output_format = formatos[[formato]][1],
#       output_file   = archivo,
#       output_dir    = carpeta_salida,
#       params        = list(tienda = id, mes = mes_reporte),
#       knit_root_dir = raiz,       # el código del .Rmd corre en la raíz
#       envir         = new.env(),  # entorno limpio para cada reporte
#       quiet         = TRUE
#     )
#     "OK"
#   }, error = function(e) {
#     paste("ERROR:", conditionMessage(e))
#   })
#
#   segundos <- round(as.numeric(difftime(Sys.time(), inicio,
#                                         units = "secs")), 1)
#   registro[[i]] <- tibble(
#     fecha_hora = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
#     tienda_id  = id,
#     tienda     = nombre,
#     mes        = mes_reporte,
#     archivo    = archivo,
#     estado     = if (resultado == "OK") "OK" else "ERROR",
#     detalle    = resultado,
#     segundos   = segundos
#   )
#   cat(sprintf("  [%2d/%d] %-18s %-5s %s\n", i, nrow(tiendas), nombre,
#               registro[[i]]$estado, archivo))
# }
#
# # ---- 4. Registro (log) ----------------------------------------------
# registro <- bind_rows(registro)
# # append = TRUE agrega al historial; los encabezados solo la primera vez
# write_csv(registro, archivo_log, append = file.exists(archivo_log))
#
# n_ok <- sum(registro$estado == "OK")
# cat("\nListo:", n_ok, "reportes generados,", nrow(registro) - n_ok,
#     "con error. Carpeta:", carpeta_salida, "\n")
# cat("Registro guardado en:", archivo_log, "\n")

# ---- Solo referencia (no se ejecuta automáticamente) ----
# install.packages("taskscheduleR")
# library(taskscheduleR)
# taskscheduler_create(
#   taskname  = "reportes_mensuales",
#   rscript   = "C:/Users/andrea/curso-bi-rstudio/reportes/generar_reportes.R",
#   schedule  = "MONTHLY",
#   days      = 1,
#   starttime = "07:00"
# )
# taskscheduler_ls()                        # lista las tareas programadas
# taskscheduler_delete("reportes_mensuales")  # para borrarla

# ---- Solo referencia (no se ejecuta automáticamente) ----
# install.packages("blastula")
# library(blastula)
#
# # UNA sola vez, de forma interactiva: guarda las credenciales cifradas
# # en el llavero del sistema (te pide la contraseña, no queda en el código)
# create_smtp_creds_key(id = "correo_bi", user = "reportes@techretail.mx",
#                       host = "smtp.office365.com", port = 587,
#                       use_ssl = TRUE)
#
# # En generar_reportes.R, después de generar cada archivo:
# correo <- compose_email(
#   body = md(paste0(
#     "Hola, adjunto el reporte ejecutivo de **", nombre, "** del mes ",
#     mes_reporte, ".\n\nÁrea de Business Intelligence"))
# ) %>%
#   add_attachment(file = file.path(carpeta_salida, archivo))
#
# smtp_send(correo,
#           to = "gerente.centro@techretail.mx",
#           from = "reportes@techretail.mx",
#           subject = paste("Reporte de ventas", mes_reporte, "-", nombre),
#           credentials = creds_key(id = "correo_bi"))

# ---- Bloque 18 --------------------------------------------------------
library(writexl)   # escribir .xlsx
library(readxl)    # leer .xlsx (para verificar)

# Hoja "Detalle": transacciones con nombres legibles
detalle <- transacciones %>%
  left_join(tiendas %>% select(tienda_id, nombre_tienda),
            by = "tienda_id") %>%
  left_join(productos %>% select(producto_id, nombre_producto, categoria),
            by = "producto_id") %>%
  select(transaccion_id, fecha, mes, nombre_tienda, nombre_producto,
         categoria, cantidad, precio_venta, total_transaccion,
         metodo_pago) %>%
  arrange(fecha, transaccion_id)

# Hoja "Resumen": un KPI por fila
resumen <- tibble(
  indicador = c("Ventas totales", "Transacciones", "Unidades vendidas",
                "Ticket promedio"),
  valor = c(round(sum(detalle$total_transaccion), 2), nrow(detalle),
            sum(detalle$cantidad),
            round(mean(detalle$total_transaccion), 2))
)

# Hoja "Mensual": ventas por mes y variación contra el mes anterior
mensual <- detalle %>%
  group_by(mes) %>%
  summarise(ventas = round(sum(total_transaccion), 2),
            transacciones = n(), .groups = "drop") %>%
  mutate(variacion_pct = round((ventas / lag(ventas) - 1) * 100, 1))

# Hoja "Por tienda"
por_tienda <- detalle %>%
  group_by(nombre_tienda) %>%
  summarise(ventas = round(sum(total_transaccion), 2),
            transacciones = n(),
            ticket_promedio = round(mean(total_transaccion), 2),
            .groups = "drop") %>%
  arrange(desc(ventas))

resumen

# ---- Bloque 19 --------------------------------------------------------
dir.create("resultados", showWarnings = FALSE)

write_xlsx(
  list(Resumen      = resumen,
       Mensual      = mensual,
       `Por tienda` = por_tienda,
       Detalle      = detalle),
  path = "resultados/reporte_ventas.xlsx"
)

# Verificación: ¿qué hojas tiene y cuántas filas hay en cada una?
hojas <- excel_sheets("resultados/reporte_ventas.xlsx")
hojas
sapply(hojas, function(h) {
  nrow(read_excel("resultados/reporte_ventas.xlsx", sheet = h))
})

# ---- Bloque 20 --------------------------------------------------------
read_excel("resultados/reporte_ventas.xlsx", sheet = "Mensual") %>%
  head(4)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# install.packages("openxlsx")
# library(openxlsx)
# libro <- createWorkbook()
# addWorksheet(libro, "Por tienda")
# writeData(libro, "Por tienda", por_tienda,
#           headerStyle = createStyle(textDecoration = "bold",
#                                     fgFill = "#173F69",
#                                     fontColour = "white"))
# addStyle(libro, "Por tienda", createStyle(numFmt = "$#,##0.00"),
#          rows = 2:11, cols = c(2, 4), gridExpand = TRUE)
# setColWidths(libro, "Por tienda", cols = 1:4, widths = "auto")
# freezePane(libro, "Por tienda", firstRow = TRUE)   # fija el encabezado
# saveWorkbook(libro, "resultados/reporte_formato.xlsx", overwrite = TRUE)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# install.packages("quarto")
# quarto::quarto_render("reportes/reporte-quarto.qmd",
#                       output_format = "html",
#                       execute_params = list(tienda = 3))

# ---- Solo referencia (no se ejecuta automáticamente) ----
# install.packages("renv")
# renv::init()       # crea una biblioteca de paquetes propia del proyecto
# renv::snapshot()   # guarda las versiones actuales en renv.lock
# renv::restore()    # (en otra PC) instala exactamente esas versiones

# ---- Bloque 21 --------------------------------------------------------
calcular_kpis <- function(datos) {
  tibble(
    ventas        = sum(datos$total_transaccion),
    transacciones = nrow(datos),
    unidades      = sum(datos$cantidad),
    ticket        = if (nrow(datos) > 0) ventas / transacciones
                    else NA_real_
  )
}

tienda_1 <- transacciones %>% filter(tienda_id == 1)
kpi     <- calcular_kpis(tienda_1 %>% filter(mes == "2023-12"))
kpi_ant <- calcular_kpis(tienda_1 %>% filter(mes == "2023-11"))
bind_rows(diciembre = kpi, noviembre = kpi_ant, .id = "periodo")

# Ranking de las tiendas en diciembre
ranking <- ventas_dic %>% mutate(posicion = row_number())
ranking %>% filter(nombre_tienda == "Sucursal Centro") %>%
  select(nombre_tienda, ventas, posicion)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# rmarkdown::render("reportes/02-reporte-ejecutivo.Rmd",
#                   output_format = "pdf_document",
#                   output_file   = "reporte.pdf",
#                   output_dir    = "resultados",
#                   params        = list(tienda = 1, mes = "2023-12"),
#                   knit_root_dir = getwd())
