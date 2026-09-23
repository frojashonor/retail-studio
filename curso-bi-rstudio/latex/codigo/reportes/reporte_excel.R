# ========================================================================
# reporte_excel.R
# Genera un reporte de ventas en Excel con varias hojas (writexl):
#   1. Resumen      -> KPIs generales del año
#   2. Mensual      -> ventas por mes con variación contra el mes anterior
#   3. Por tienda   -> ventas, transacciones y ticket de cada tienda
#   4. Top productos-> los 15 productos con más ventas
#   5. Detalle      -> todas las transacciones con nombre de tienda/producto
#
# Uso (con el directorio de trabajo en la carpeta del curso):
#   source("reportes/reporte_excel.R")
#   Rscript reportes/reporte_excel.R
# ========================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(writexl)
  library(readxl)
})

# ---- 1. Datos -----------------------------------------------------------
raiz <- if (dir.exists("datasets")) "." else
        if (dir.exists("../datasets")) ".."
if (is.null(raiz)) {
  stop("No encuentro datasets/. Ejecuta desde la carpeta del curso.")
}
transacciones <- read_csv(file.path(raiz, "datasets", "transacciones.csv"),
                          show_col_types = FALSE)
tiendas   <- read_csv(file.path(raiz, "datasets", "tiendas.csv"),
                      show_col_types = FALSE)
productos <- read_csv(file.path(raiz, "datasets", "productos.csv"),
                      show_col_types = FALSE)

detalle <- transacciones %>%
  left_join(tiendas %>% select(tienda_id, nombre_tienda, ciudad),
            by = "tienda_id") %>%
  left_join(productos %>% select(producto_id, nombre_producto, categoria),
            by = "producto_id") %>%
  select(transaccion_id, fecha, mes, nombre_tienda, ciudad,
         nombre_producto, categoria, cantidad, precio_venta,
         total_transaccion, metodo_pago) %>%
  arrange(fecha, transaccion_id)

# ---- 2. Hojas -----------------------------------------------------------
resumen <- tibble(
  indicador = c("Ventas totales", "Transacciones", "Unidades vendidas",
                "Ticket promedio", "Clientes distintos",
                "Fecha de generación"),
  valor = c(round(sum(detalle$total_transaccion), 2), nrow(detalle),
            sum(detalle$cantidad),
            round(mean(detalle$total_transaccion), 2),
            n_distinct(transacciones$cliente_id),
            NA)
) %>%
  mutate(valor = as.character(valor),
         valor = if_else(indicador == "Fecha de generación",
                         format(Sys.Date(), "%Y-%m-%d"), valor))

mensual <- detalle %>%
  group_by(mes) %>%
  summarise(ventas = round(sum(total_transaccion), 2),
            transacciones = n(), .groups = "drop") %>%
  mutate(variacion_pct = round((ventas / lag(ventas) - 1) * 100, 1))

por_tienda <- detalle %>%
  group_by(nombre_tienda, ciudad) %>%
  summarise(ventas = round(sum(total_transaccion), 2),
            transacciones = n(),
            ticket_promedio = round(mean(total_transaccion), 2),
            .groups = "drop") %>%
  mutate(participacion_pct = round(ventas / sum(ventas) * 100, 1)) %>%
  arrange(desc(ventas))

top_productos <- detalle %>%
  group_by(nombre_producto, categoria) %>%
  summarise(unidades = sum(cantidad),
            ventas = round(sum(total_transaccion), 2), .groups = "drop") %>%
  arrange(desc(ventas)) %>%
  slice_head(n = 15)

# ---- 3. Guardar ---------------------------------------------------------
dir.create(file.path(raiz, "resultados"), showWarnings = FALSE)
archivo <- file.path(raiz, "resultados", "reporte_ventas.xlsx")
write_xlsx(
  list(Resumen = resumen, Mensual = mensual, `Por tienda` = por_tienda,
       `Top productos` = top_productos, Detalle = detalle),
  path = archivo
)

# ---- 4. Verificar: releer el archivo ------------------------------------
hojas <- excel_sheets(archivo)
filas <- sapply(hojas, function(h) nrow(read_excel(archivo, sheet = h)))
cat("Archivo creado:", archivo, "\n")
print(data.frame(hoja = hojas, filas = filas, row.names = NULL))
