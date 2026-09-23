# ============================================================================
# Figuras del Módulo 6 (Dashboards interactivos con Shiny)
# Se ejecuta desde una carpeta que contenga datasets/ (ver guía del libro).
# Las gráficas son las mismas funciones que usan las apps del módulo.
# ============================================================================
source(file.path(Sys.getenv("DIR_LIBRO"), "herramientas/figuras/tema_libro.R"))
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
})

# ---- App 02: grafica_mensual() (mismo código que en app.R) ------------------
buscar_datasets <- function(desde = ".", max_niveles = 6) {
  carpeta <- desde
  for (i in 0:max_niveles) {
    candidato <- file.path(carpeta, "datasets", "transacciones.csv")
    if (file.exists(candidato)) return(file.path(carpeta, "datasets"))
    carpeta <- file.path(carpeta, "..")
  }
  stop("No encontré datasets/")
}
carpeta_datos <- buscar_datasets()
tiendas   <- read_csv(file.path(carpeta_datos, "tiendas.csv"),
                      show_col_types = FALSE)
productos <- read_csv(file.path(carpeta_datos, "productos.csv"),
                      show_col_types = FALSE)
ventas <- read_csv(file.path(carpeta_datos, "transacciones.csv"),
                   show_col_types = FALSE) %>%
  left_join(select(tiendas, tienda_id, nombre_tienda), by = "tienda_id") %>%
  left_join(select(productos, producto_id, nombre_producto, categoria),
            by = "producto_id") %>%
  mutate(mes = floor_date(fecha, "month"))

resumir_por_mes <- function(datos, metrica = "ventas") {
  datos %>%
    group_by(mes) %>%
    summarise(ventas        = sum(total_transaccion),
              unidades      = sum(cantidad),
              transacciones = n(),
              .groups = "drop") %>%
    mutate(valor = .data[[metrica]])
}
grafica_mensual <- function(resumen, metrica = "ventas") {
  etiquetas <- c(ventas = "Ventas", unidades = "Unidades vendidas",
                 transacciones = "Transacciones")
  formato <- if (metrica == "ventas") label_dollar() else label_comma()
  ggplot(resumen, aes(x = mes, y = valor)) +
    geom_col(fill = "#2a78d6", width = 22) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(labels = formato,
                       expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = etiquetas[[metrica]]) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
}
p_mensual <- resumir_por_mes(ventas, "ventas") %>%
  grafica_mensual("ventas")
guardar_figura(p_mensual, "m06-explorador-mensual")

# ---- App 03: funciones de R/funciones_dashboard.R ---------------------------
source(file.path(Sys.getenv("DIR_LIBRO"),
                 "codigo/apps/03-dashboard-ejecutivo/R/funciones_dashboard.R"))
datos <- cargar_datos_dashboard()
guardar_figura(grafica_evolucion(datos), "m06-evolucion")
guardar_figura(grafica_top_productos(datos), "m06-top-productos")
guardar_figura(grafica_tiendas(datos), "m06-tiendas")
guardar_figura(grafica_segmentos(datos), "m06-segmentos")
cat("Figuras del Módulo 6 generadas\n")
