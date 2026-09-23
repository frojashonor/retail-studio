# ============================================================================
# Figuras del Módulo 4: Análisis exploratorio y estadística para negocios
# Cada gráfica repite EXACTAMENTE el código del bloque rcode del capítulo.
# Ejecutar desde una carpeta que contenga datasets/ :
#   DIR_LIBRO=.../latex DIR_FIGURAS=.../latex/figuras Rscript figuras-04.R
# ============================================================================
source(file.path(Sys.getenv("DIR_LIBRO"), "herramientas/figuras/tema_libro.R"))
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(corrplot)
})

transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
clientes  <- read_csv("datasets/clientes.csv", show_col_types = FALSE)
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)
tiendas   <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)

ticket <- transacciones$total_transaccion
# Coeficiente de asimetría (versión sencilla)
asimetria <- function(x) {
  mean((x - mean(x))^3) / sd(x)^3
}
media_ticket   <- mean(ticket)
mediana_ticket <- median(ticket)

# ---------------------------------------------------------------------------
# 1. Histograma + densidad del ticket
# ---------------------------------------------------------------------------
p_hist <- ggplot(transacciones, aes(x = total_transaccion)) +
  # barras: cuántos tickets caen en cada intervalo de 150 pesos
  geom_histogram(binwidth = 150, boundary = 0,
                 fill = color_principal, color = "white", alpha = 0.85) +
  # densidad reescalada (x 150) para que quede en la escala de las barras
  geom_density(aes(y = after_stat(count) * 150),
               color = "#0b0b0b", linewidth = 0.8) +
  # media (línea continua) y mediana (línea punteada)
  geom_vline(xintercept = media_ticket, color = color_resalte,
             linewidth = 0.8) +
  geom_vline(xintercept = mediana_ticket, color = color_resalte,
             linewidth = 0.8, linetype = "dashed") +
  annotate("text", x = media_ticket + 50, y = 140, hjust = 0,
           label = paste("Media:", dollar(media_ticket)),
           color = color_resalte, size = 3.5) +
  annotate("text", x = mediana_ticket - 50, y = 140, hjust = 1,
           label = paste("Mediana:", dollar(mediana_ticket)),
           color = color_resalte, size = 3.5) +
  scale_x_continuous(labels = dollar) +
  labs(title = "Distribución del ticket por transacción",
       subtitle = "Muchos tickets pequeños y una cola larga a la derecha",
       x = "Total de la transacción", y = "Número de transacciones") +
  tema_libro()
guardar_figura(p_hist, "m04-histograma-ticket")

# ---------------------------------------------------------------------------
# 2. Boxplot del gasto por cliente con atípicos
# ---------------------------------------------------------------------------
marcar_atipicos <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  rango_iq <- q3 - q1
  x < q1 - 1.5 * rango_iq | x > q3 + 1.5 * rango_iq
}

gasto_clientes <- transacciones %>%
  group_by(cliente_id) %>%
  summarise(compras = n(),
            gasto   = sum(total_transaccion),
            .groups = "drop") %>%
  mutate(atipico = marcar_atipicos(gasto),
         z       = (gasto - mean(gasto)) / sd(gasto))

set.seed(123)   # geom_jitter() mueve los puntos al azar
p_caja <- ggplot(gasto_clientes, aes(x = gasto, y = "")) +
  geom_boxplot(width = 0.5, outlier.shape = NA, fill = "grey96") +
  geom_jitter(aes(color = atipico), height = 0.15,
              size = 1.8, alpha = 0.8) +
  scale_color_manual(values = c(`FALSE` = color_gris,
                                `TRUE` = color_resalte),
                     labels = c("Dentro de las cercas",
                                "Atípico (regla 1.5 × IQR)"),
                     name = NULL) +
  scale_x_continuous(labels = dollar) +
  labs(title = "Gasto anual por cliente en 2023",
       subtitle = "8 clientes superan el límite superior de la caja",
       x = "Gasto total del cliente", y = NULL) +
  tema_libro()
guardar_figura(p_caja, "m04-boxplot-clientes", alto = 3.2)

# ---------------------------------------------------------------------------
# 3. KPIs mensuales con promedio móvil
# ---------------------------------------------------------------------------
meta_mensual <- 80000   # meta de ventas por mes

kpi_mensual <- transacciones %>%
  group_by(mes) %>%
  summarise(ventas = sum(total_transaccion), .groups = "drop") %>%
  arrange(mes) %>%                                  # orden cronológico
  mutate(
    crec_mom   = ventas / lag(ventas) - 1,          # vs mes anterior
    acumulado  = cumsum(ventas),                    # acumulado del año
    prom_movil = (ventas + lag(ventas) + lag(ventas, 2)) / 3,
    indice_100 = ventas / first(ventas) * 100,      # enero = 100
    vs_meta    = ventas / meta_mensual - 1          # sobre/bajo la meta
  )


# Formato largo: una fila por mes y por serie (Módulo 2, pivot_longer)
kpi_largo <- kpi_mensual %>%
  mutate(fecha_mes = as.Date(paste0(mes, "-01"))) %>%
  select(fecha_mes,
         `Ventas del mes` = ventas,
         `Promedio móvil 3 meses` = prom_movil) %>%
  pivot_longer(-fecha_mes, names_to = "serie", values_to = "valor")

p_kpi <- ggplot(kpi_largo, aes(x = fecha_mes, y = valor, color = serie)) +
  geom_hline(yintercept = meta_mensual, linetype = "dashed",
             color = "#52514e", linewidth = 0.5) +
  annotate("text", x = as.Date("2023-09-20"), y = meta_mensual + 2500,
           label = "Meta: $80,000", hjust = 0, size = 3.3,
           color = "#52514e") +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  geom_point(size = 1.8, na.rm = TRUE) +
  scale_color_manual(
    values = c("Ventas del mes" = color_principal,
               "Promedio móvil 3 meses" = color_resalte),
    breaks = c("Ventas del mes", "Promedio móvil 3 meses"),
    name = NULL) +
  scale_y_continuous(labels = dollar) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Ventas mensuales 2023 contra la meta",
       subtitle = "El promedio móvil suaviza los brincos de cada mes",
       x = NULL, y = "Ventas") +
  tema_libro()
guardar_figura(p_kpi, "m04-kpi-evolucion")

# ---------------------------------------------------------------------------
# 4. Curva de Pareto
# ---------------------------------------------------------------------------
pareto <- transacciones %>%
  group_by(producto_id) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop") %>%
  arrange(desc(ingresos)) %>%                        # de mayor a menor
  mutate(
    pct_ingresos  = ingresos / sum(ingresos),        # peso de cada uno
    pct_acumulado = cumsum(pct_ingresos),            # % acumulado
    pct_productos = row_number() / n(),              # % del catálogo
    clase = case_when(
      pct_acumulado <= 0.80 ~ "A",
      pct_acumulado <= 0.95 ~ "B",
      TRUE                  ~ "C"
    )
  ) %>%
  left_join(productos %>%
              select(producto_id, categoria, estado_stock, activo),
            by = "producto_id")

p_pareto <- ggplot(pareto, aes(x = pct_productos, y = pct_acumulado)) +
  # diagonal: lo que pasaría si todos los productos vendieran lo mismo
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = color_gris) +
  geom_hline(yintercept = 0.80, color = "#52514e", linewidth = 0.4) +
  geom_line(color = "#52514e", linewidth = 0.6) +
  geom_point(aes(color = clase), size = 2) +
  # punto de referencia de la regla 80/20
  annotate("point", x = 0.2, y = 0.8, shape = 4, size = 4, stroke = 1.2) +
  annotate("text", x = 0.22, y = 0.86, hjust = 0, size = 3.3,
           label = "Regla 80/20 (referencia)") +
  scale_color_manual(values = paleta_libro[1:3], name = "Clase") +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(title = "Curva de Pareto de ingresos por producto",
       subtitle = "Ingresos repartidos: no hay pocos productos estrella",
       x = "% de productos (de mayor a menor ingreso)",
       y = "% acumulado de ingresos") +
  tema_libro()
guardar_figura(p_pareto, "m04-pareto")

# ---------------------------------------------------------------------------
# 5. Matriz de correlación (corrplot usa gráficos base)
# ---------------------------------------------------------------------------
perfil_clientes <- transacciones %>%
  group_by(cliente_id) %>%
  summarise(compras   = n(),
            unidades  = sum(cantidad),
            gasto     = sum(total_transaccion),
            ticket    = mean(total_transaccion),
            .groups = "drop") %>%
  left_join(clientes, by = "cliente_id") %>%
  mutate(antiguedad = as.numeric(as.Date("2023-12-31") - fecha_registro))

matriz_cor <- perfil_clientes %>%
  select(compras, unidades, gasto, ticket, edad, antiguedad) %>%
  cor()

cairo_pdf(file.path(dir_figuras, "m04-corrplot.pdf"), width = 6, height = 5)
corrplot(matriz_cor,
         method = "color",          # celdas coloreadas
         type = "upper",            # solo el triángulo superior
         addCoef.col = "black",     # escribe el coeficiente
         number.cex = 0.8,          # tamaño de los números
         tl.col = "black",          # color de los nombres
         tl.srt = 45,               # nombres inclinados
         col = colorRampPalette(c("#e34948", "white", "#2a78d6"))(200),
         diag = FALSE)              # sin la diagonal de unos
invisible(dev.off())

# ---------------------------------------------------------------------------
# 6. Ventas semanales vs normal
# ---------------------------------------------------------------------------
library(lubridate)
ventas_semanales <- transacciones %>%
  # semanas completas de lunes a domingo (del 2 de enero al 31 de dic.)
  filter(fecha >= as.Date("2023-01-02")) %>%
  mutate(semana = floor_date(fecha, unit = "week", week_start = 1)) %>%
  group_by(semana) %>%
  summarise(ventas = sum(total_transaccion), .groups = "drop")

media_sem <- mean(ventas_semanales$ventas)
sd_sem    <- sd(ventas_semanales$ventas)

p_normal <- ggplot(ventas_semanales, aes(x = ventas)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 2000,
                 fill = color_principal, color = "white", alpha = 0.85) +
  # curva normal con la media y desviación de los datos
  stat_function(fun = dnorm, args = list(mean = media_sem, sd = sd_sem),
                color = color_resalte, linewidth = 0.9) +
  geom_vline(xintercept = 25000, linetype = "dashed", color = "#52514e") +
  annotate("text", x = 25300, y = 9e-05, hjust = 0, size = 3.3,
           label = "¿Más de $25,000?", color = "#52514e") +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = NULL) +
  labs(title = "Ventas semanales de 2023 y curva normal",
       subtitle = "52 semanas; la curva naranja es Normal(18,445; 4,591)",
       x = "Ventas de la semana", y = "Densidad") +
  tema_libro()
guardar_figura(p_normal, "m04-normal-semanal")

# ---------------------------------------------------------------------------
# 7. Bootstrap de la mediana
# ---------------------------------------------------------------------------
set.seed(123)   # para que el resultado sea reproducible
medianas_boot <- replicate(5000, {
  remuestra <- sample(ticket, size = length(ticket), replace = TRUE)
  median(remuestra)
})

ic_boot <- quantile(medianas_boot, probs = c(0.025, 0.975))

p_boot <- ggplot(tibble(mediana = medianas_boot), aes(x = mediana)) +
  geom_histogram(binwidth = 5, fill = color_principal, color = "white") +
  geom_vline(xintercept = ic_boot, color = color_resalte,
             linewidth = 0.8, linetype = "dashed") +
  geom_vline(xintercept = mediana_ticket, color = "#0b0b0b",
             linewidth = 0.8) +
  scale_x_continuous(labels = dollar) +
  labs(title = "Distribución bootstrap de la mediana del ticket",
       subtitle = "5,000 remuestras; líneas naranjas = IC del 95%",
       x = "Mediana del ticket en cada remuestra",
       y = "Frecuencia") +
  tema_libro()
guardar_figura(p_boot, "m04-bootstrap")

# ---------------------------------------------------------------------------
# 8. Ticket por categoría con IC
# ---------------------------------------------------------------------------
tx_productos <- transacciones %>%
  left_join(productos, by = "producto_id")

ticket_categoria <- tx_productos %>%
  group_by(categoria) %>%
  summarise(n     = n(),
            media = mean(total_transaccion),
            ee    = sd(total_transaccion) / sqrt(n()),
            .groups = "drop") %>%
  arrange(desc(media))

p_anova <- ggplot(ticket_categoria,
                  aes(x = media, y = reorder(categoria, media))) +
  geom_vline(xintercept = mean(ticket), linetype = "dashed",
             color = color_gris) +
  geom_errorbarh(aes(xmin = media - 2 * ee, xmax = media + 2 * ee),
                 height = 0.25, color = color_principal,
                 linewidth = 0.8) +
  geom_point(size = 2.5, color = color_principal) +
  scale_x_continuous(labels = dollar) +
  labs(title = "Ticket promedio por categoría con IC del 95%",
       subtitle = "Intervalos muy encimados; gris = promedio general",
       x = "Ticket promedio", y = NULL) +
  tema_libro()
guardar_figura(p_anova, "m04-anova-categorias")

# ---------------------------------------------------------------------------
# 9. Regresión simple
# ---------------------------------------------------------------------------
set.seed(123)
sucursales <- tibble(
  publicidad = round(runif(60, min = 5, max = 50), 1),   # miles de $
  tamano_m2  = round(runif(60, min = 300, max = 800))    # m2
) %>%
  mutate(ventas = round(120 + 3.5 * publicidad + 0.25 * tamano_m2 +
                          rnorm(60, sd = 30), 1))        # miles de $

p_regresion <- ggplot(sucursales, aes(x = publicidad, y = ventas)) +
  geom_point(color = color_principal, size = 2, alpha = 0.8) +
  # recta de regresión con su banda de confianza del 95%
  geom_smooth(method = "lm", formula = y ~ x, color = color_resalte,
              fill = color_resalte, alpha = 0.15, linewidth = 0.9) +
  annotate("text", x = 6, y = 480, hjust = 0, size = 3.5,
           label = "ventas = 261.5 + 3.39 × publicidad\nR² = 0.53") +
  scale_x_continuous(labels = label_dollar(suffix = "k")) +
  scale_y_continuous(labels = label_dollar(suffix = "k")) +
  labs(title = "Publicidad vs ventas por sucursal (datos simulados)",
       subtitle = "Cada punto es una sucursal; banda = IC 95% de la recta",
       x = "Inversión en publicidad (miles)",
       y = "Ventas mensuales (miles)") +
  tema_libro()
guardar_figura(p_regresion, "m04-regresion")

# ---------------------------------------------------------------------------
# 10. Residuos
# ---------------------------------------------------------------------------
modelo_multiple <- lm(ventas ~ publicidad + tamano_m2, data = sucursales)

diagnostico <- tibble(
  ajustado = fitted(modelo_multiple),     # ventas que predice el modelo
  residuo  = residuals(modelo_multiple)   # real - predicho
)

p_residuos <- ggplot(diagnostico, aes(x = ajustado, y = residuo)) +
  geom_hline(yintercept = 0, color = color_resalte, linewidth = 0.8) +
  geom_point(color = color_principal, size = 2, alpha = 0.8) +
  labs(title = "Residuos vs valores ajustados (modelo múltiple)",
       subtitle = "Nube sin patrón alrededor de cero: supuestos razonables",
       x = "Ventas ajustadas (miles)", y = "Residuo (miles)") +
  tema_libro()
guardar_figura(p_residuos, "m04-residuos")
