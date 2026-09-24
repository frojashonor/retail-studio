# ==========================================================================
# Referencia rápida
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/B-referencia-rapida.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Bloque 1 --------------------------------------------------------
# Paquetes y datos que usan los ejemplos de este apéndice
library(tidyverse)   # dplyr, ggplot2, tidyr, readr, stringr, lubridate
library(scales)      # formatos de negocio: dólar, coma, porcentaje

ventas    <- read_csv("datasets/ventas_retail.csv", show_col_types = FALSE)
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)
tiendas   <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)

# ---- Bloque 2 --------------------------------------------------------
precios <- c(120, 85, NA, 240, 99)       # vector con un dato faltante
mean(precios)                            # el NA "contagia" el resultado
mean(precios, na.rm = TRUE)              # ignora los NA
sum(is.na(precios))                      # ¿cuántos faltan?
precios[precios > 100 & !is.na(precios)] # filtrar sin arrastrar NA

# ---- Bloque 3 --------------------------------------------------------
# Clasifica un monto de venta; umbral tiene un valor por omisión
clasificar_venta <- function(monto, umbral = 1000) {
  if (is.na(monto)) return("sin dato")    # protegerse de NA
  if (monto >= umbral) "alta" else "normal"
}
clasificar_venta(1500)                     # usa umbral = 1000
clasificar_venta(300, umbral = 200)        # cambia el umbral
sapply(c(1500, 300, NA), clasificar_venta) # aplicar a cada elemento
for (trimestre in c("Q1", "Q2")) {         # bucle sencillo
  cat("Procesando", trimestre, "\n")
}

# ---- Bloque 4 --------------------------------------------------------
ventas %>%
  filter(descuento_pct > 0) %>%                 # 1. filas relevantes
  left_join(tiendas, by = "tienda_id") %>%      # 2. agregar la ciudad
  group_by(ciudad) %>%                          # 3. agrupar
  summarise(ventas = sum(total),                # 4. resumir
            tickets = n(),
            ticket_prom = mean(total),
            .groups = "drop") %>%
  arrange(desc(ventas)) %>%                     # 5. ordenar
  mutate(participacion = percent(ventas / sum(ventas),
                                 accuracy = 0.1)) # 6. formato

# ---- Bloque 5 --------------------------------------------------------
fechas <- ymd(c("2023-01-15", "2023-07-04"))
month(fechas, label = TRUE, abbr = FALSE)   # nombre del mes
wday(fechas, label = TRUE)                  # día de la semana
floor_date(fechas, "month")                 # primer día del mes
fechas %m+% months(1)                       # un mes después
str_to_title("sucursal centro norte")
str_detect(c("Tarjeta Crédito", "Efectivo"), "Tarjeta")

# ---- Bloque 6 --------------------------------------------------------
color_principal <- "#2a78d6"                   # color corporativo
p <- ventas %>%
  mutate(mes = floor_date(fecha, "month")) %>%
  group_by(mes) %>%
  summarise(ventas = sum(total), .groups = "drop") %>%
  ggplot(aes(x = mes, y = ventas)) +
  geom_line(color = color_principal, linewidth = 0.8) +
  geom_point(color = color_principal) +
  scale_y_continuous(labels = label_dollar(), limits = c(0, NA)) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(title = "Ventas mensuales 2023", x = NULL, y = "Ventas",
       caption = "Fuente: ventas_retail.csv") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
dir.create("resultados", showWarnings = FALSE)
ggsave("resultados/ventas_mensuales.png", p,
       width = 8, height = 4.5, dpi = 300)     # PNG listo para PowerPoint

# ---- Bloque 7 --------------------------------------------------------
dollar(1250.5)                          # moneda
comma(12500)                            # miles
percent(0.1253, accuracy = 0.1)         # porcentaje con 1 decimal
label_number(scale_cut = cut_short_scale(), prefix = "$")(c(12500, 2500000))

# ---- Bloque 8 --------------------------------------------------------
library(DBI)
con <- dbConnect(RSQLite::SQLite(), ":memory:")   # base temporal
dbWriteTable(con, "ventas", select(ventas, tienda_id, descuento_pct, total))
dbGetQuery(con, "
  SELECT tienda_id, COUNT(*) AS n, ROUND(SUM(total), 2) AS ventas
  FROM ventas
  WHERE descuento_pct > 0
  GROUP BY tienda_id
  HAVING COUNT(*) >= 20
  ORDER BY ventas DESC")
ventas %>%                                          # lo mismo en dplyr
  filter(descuento_pct > 0) %>%
  group_by(tienda_id) %>%
  summarise(n = n(), ventas = round(sum(total), 2), .groups = "drop") %>%
  filter(n >= 20) %>%                               # el HAVING
  arrange(desc(ventas))
dbDisconnect(con)

# ---- Bloque 9 --------------------------------------------------------
# ¿Gastan más los clientes en fin de semana?
ventas <- ventas %>%
  mutate(fin_semana = dia_semana %in% c("sábado", "domingo"))
prueba <- t.test(total ~ fin_semana, data = ventas)
prueba$estimate      # promedio de cada grupo (FALSE = entre semana)
prueba$p.value       # p-valor

# ---- Bloque 10 --------------------------------------------------------
kpi_mes <- ventas %>%
  mutate(mes = floor_date(fecha, "month")) %>%
  group_by(mes) %>%
  summarise(ventas = sum(total), tickets = n(), .groups = "drop") %>%
  mutate(ticket_prom = ventas / tickets,          # ticket promedio
         crec_mom = ventas / lag(ventas) - 1)     # vs. mes anterior
kpi_mes %>%
  mutate(crec_mom = percent(crec_mom, accuracy = 0.1)) %>%
  head(4)

# ---- Bloque 11 --------------------------------------------------------
ventas %>%
  left_join(select(productos, producto_id, costo), by = "producto_id") %>%
  summarise(ingresos = sum(total),
            costo_ventas = sum(cantidad * costo),
            margen_bruto_pct = (ingresos - costo_ventas) / ingresos)

# ---- Bloque 12 --------------------------------------------------------
# --- Regresión / pronóstico ---
real       <- c(120, 95, 130, 150, 110)
pronostico <- c(115, 100, 128, 140, 118)
error <- real - pronostico
tibble(MAE  = mean(abs(error)),
       RMSE = sqrt(mean(error^2)),
       MAPE = mean(abs(error / real)))
# --- Clasificación (1 = el cliente se fue) ---
obs  <- factor(c(1, 0, 1, 1, 0, 0, 1, 0, 0, 0), levels = c(1, 0))
pred <- factor(c(1, 0, 0, 1, 0, 1, 1, 0, 0, 0), levels = c(1, 0))
mc <- table(Predicho = pred, Real = obs)      # matriz de confusión
mc
vp <- mc["1", "1"]; fp <- mc["1", "0"]
fn <- mc["0", "1"]; vn <- mc["0", "0"]
exactitud <- (vp + vn) / sum(mc)
precision <- vp / (vp + fp)
recall    <- vp / (vp + fn)
f1        <- 2 * precision * recall / (precision + recall)
round(c(exactitud = exactitud, precision = precision,
        recall = recall, f1 = f1), 3)
# --- AUC a partir de probabilidades ---
prob <- c(0.81, 0.30, 0.45, 0.66, 0.12, 0.58, 0.90, 0.25, 0.40, 0.08)
curva <- pROC::roc(response = obs, predictor = prob,
                   levels = c("0", "1"), direction = "<")
pROC::auc(curva)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# library(shiny)
# library(tidyverse)
# ventas <- read_csv("datasets/ventas_retail.csv", show_col_types = FALSE)
#
# ui <- fluidPage(
#   titlePanel("Ventas por tienda"),
#   sidebarLayout(
#     sidebarPanel(selectInput("tienda", "Tienda", sort(unique(ventas$tienda_id)))),
#     mainPanel(textOutput("kpi"), plotOutput("grafico"))
#   )
# )
#
# server <- function(input, output, session) {
#   datos <- reactive(filter(ventas, tienda_id == input$tienda))
#   output$kpi <- renderText(paste("Ventas:", scales::dollar(sum(datos()$total))))
#   output$grafico <- renderPlot(
#     ggplot(datos(), aes(fecha, total)) + geom_line(linewidth = 0.8)
#   )
# }
#
# shinyApp(ui, server)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# for (t in 1:10) {
#   rmarkdown::render("reporte.Rmd", params = list(tienda = t),
#                     output_file = paste0("reporte_tienda_", t, ".html"))
# }
