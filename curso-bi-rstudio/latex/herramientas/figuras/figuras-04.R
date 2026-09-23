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
