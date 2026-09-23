# ============================================================================
# Figuras del Módulo 5 - Bases de datos, SQL y ETL desde R
# Reproduce la gráfica del caso práctico: la base se construye en memoria
# con el mismo cálculo que la vista vw_kpi_mensual del capítulo.
# ============================================================================
source(file.path(Sys.getenv("DIR_LIBRO"), "herramientas/figuras/tema_libro.R"))
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(DBI); library(RSQLite)
})

con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(con, "transacciones",
             read_csv("datasets/transacciones.csv", show_col_types = FALSE) %>%
               mutate(fecha = format(fecha, "%Y-%m-%d"),
                      hora = as.character(hora)))
nombres_mes <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
                 "julio", "agosto", "septiembre", "octubre", "noviembre",
                 "diciembre")
kpi_mensual <- dbGetQuery(con, "
  SELECT CAST(strftime('%m', fecha) AS INTEGER) AS mes,
         ROUND(SUM(cantidad * precio_venta), 2) AS ventas
  FROM transacciones GROUP BY mes ORDER BY mes") %>%
  mutate(nombre_mes = nombres_mes[mes])
dbDisconnect(con)

grafica_mensual <- kpi_mensual %>%
  mutate(nombre_mes = factor(nombre_mes, levels = nombre_mes)) %>%
  ggplot(aes(x = nombre_mes, y = ventas, group = 1)) +
  geom_line(color = color_principal, linewidth = 0.8) +
  geom_point(color = color_principal, size = 2) +
  scale_y_continuous(labels = scales::dollar,
                     limits = c(0, NA)) +
  labs(title = "Ventas mensuales 2023",
       subtitle = "Fuente: vista vw_kpi_mensual del data warehouse",
       x = NULL, y = "Ventas") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
guardar_figura(grafica_mensual, "m05-ventas-mensuales")
