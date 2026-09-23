# ==========================================================================
# Análisis exploratorio y estadística para negocios
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/04-analisis-exploratorio.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Solo referencia (no se ejecuta automáticamente) ----
# install.packages(c("skimr", "corrplot"))

# ---- Bloque 1 --------------------------------------------------------
# Paquetes del módulo
library(tidyverse)   # dplyr, ggplot2, readr, tidyr... (Módulos 2 y 3)
library(skimr)       # resúmenes rápidos de un data frame
library(corrplot)    # matrices de correlación
library(scales)      # formatos de negocio: dollar(), percent(), comma()

# Datos del curso
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
clientes  <- read_csv("datasets/clientes.csv", show_col_types = FALSE)
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)
tiendas   <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)

# ---- Bloque 2 --------------------------------------------------------
# Colores corporativos
color_principal <- "#2a78d6"   # una sola serie
color_resalte   <- "#eb6834"   # para destacar un elemento
color_gris      <- "#b5b3ad"   # el resto, cuando se resalta uno
paleta_libro <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                  "#e87ba4", "#008300", "#4a3aa7", "#e34948")

# Tema corporativo (misma forma que en el Módulo 3)
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

# ---- Bloque 3 --------------------------------------------------------
dim(transacciones)        # filas y columnas
glimpse(transacciones)    # tipo y primeros valores de cada columna

# ---- Bloque 4 --------------------------------------------------------
transacciones %>%
  select(cantidad, precio_venta, total_transaccion) %>%
  summary()

# ---- Bloque 5 --------------------------------------------------------
skim_without_charts(transacciones)

# ---- Bloque 6 --------------------------------------------------------
ticket <- transacciones$total_transaccion   # 1,000 tickets de venta
length(ticket)

# ---- Bloque 7 --------------------------------------------------------
media_ticket   <- mean(ticket)     # promedio
mediana_ticket <- median(ticket)   # valor central
media_ticket
mediana_ticket
sum(ticket) / length(ticket)       # la media "a mano": mismo resultado

# ---- Bloque 8 --------------------------------------------------------
# Moda: el valor que más veces aparece en un vector
moda <- function(x) {
  frecuencias <- table(x)                      # cuenta cada valor
  names(frecuencias)[which.max(frecuencias)]   # el más frecuente
}

moda(transacciones$metodo_pago)   # método de pago más usado
moda(transacciones$dia_semana)    # día con más transacciones
moda(transacciones$cantidad)      # unidades más comunes por ticket

# ---- Bloque 9 --------------------------------------------------------
table(transacciones$cantidad)

# ---- Bloque 10 --------------------------------------------------------
range(ticket)                  # mínimo y máximo
diff(range(ticket))            # rango = máximo - mínimo
var(ticket)                    # varianza (pesos al cuadrado)
sd(ticket)                     # desviación estándar (pesos)
IQR(ticket)                    # rango intercuartílico
sd(ticket) / mean(ticket)      # coeficiente de variación

# La varianza "a mano", para que veas que no hay magia:
sum((ticket - mean(ticket))^2) / (length(ticket) - 1)

# ---- Bloque 11 --------------------------------------------------------
quantile(ticket, probs = c(0.10, 0.25, 0.50, 0.75, 0.90, 0.95))

# ---- Bloque 12 --------------------------------------------------------
umbral_cupon <- quantile(ticket, probs = 0.80)   # percentil 80
umbral_cupon
sum(ticket > umbral_cupon)   # tickets que superan el umbral

# ---- Bloque 13 --------------------------------------------------------
compras_b2b <- tibble(
  cliente = paste("Cliente", 1:10),
  compra_mensual = c(22000, 25000, 21000, 28000, 30000,
                     24000, 26000, 23000, 27000, 900000)
)

compras_b2b %>%
  summarise(media   = mean(compra_mensual),
            mediana = median(compra_mensual))

# ---- Bloque 14 --------------------------------------------------------
salarios <- c(rep(15000, 9), 250000)   # 9 empleados y un director
mean(salarios)
median(salarios)

# ---- Bloque 15 --------------------------------------------------------
resumen_pago <- transacciones %>%
  group_by(metodo_pago) %>%
  summarise(
    n        = n(),                                 # cuántos tickets
    ventas   = round(sum(total_transaccion)),       # total vendido
    media    = mean(total_transaccion),             # ticket promedio
    mediana  = median(total_transaccion),           # ticket típico
    desv_est = sd(total_transaccion),               # dispersión
    cv       = sd(total_transaccion) / mean(total_transaccion),
    .groups = "drop"
  ) %>%
  mutate(pct_ventas = ventas / sum(ventas)) %>%     # % de las ventas
  arrange(desc(ventas))

resumen_pago

# ---- Bloque 16 --------------------------------------------------------
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

p_hist

# ---- Bloque 17 --------------------------------------------------------
# Coeficiente de asimetría (versión sencilla)
asimetria <- function(x) {
  mean((x - mean(x))^3) / sd(x)^3
}

asimetria(ticket)                        # ticket de compra
asimetria(transacciones$precio_venta)    # precio unitario
asimetria(compras_b2b$compra_mensual)    # distribuidor con cliente gigante

# ---- Bloque 18 --------------------------------------------------------
q1  <- quantile(ticket, 0.25)
q3  <- quantile(ticket, 0.75)
iqr <- q3 - q1
limite_inferior <- unname(q1 - 1.5 * iqr)   # unname() quita el "25%"
limite_superior <- unname(q3 + 1.5 * iqr)
c(limite_inferior, limite_superior)

# ¿Cuántos tickets quedan fuera de las cercas?
sum(ticket < limite_inferior | ticket > limite_superior)

# ---- Bloque 19 --------------------------------------------------------
z_ticket <- (ticket - mean(ticket)) / sd(ticket)
range(z_ticket)              # el z más bajo y el más alto
sum(abs(z_ticket) > 3)       # tickets con |z| > 3

# ---- Bloque 20 --------------------------------------------------------
# Devuelve TRUE para los valores fuera de las cercas de Tukey
marcar_atipicos <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  rango_iq <- q3 - q1
  x < q1 - 1.5 * rango_iq | x > q3 + 1.5 * rango_iq
}

# ---- Bloque 21 --------------------------------------------------------
# Copia de los tickets + 3 registros con problemas (simulados)
tickets_revision <- transacciones %>%
  select(transaccion_id, total_transaccion) %>%
  bind_rows(tibble(transaccion_id    = c(1001, 1002, 1003),
                   total_transaccion = c(29923.50, -450, 0)))

tickets_revision %>%
  mutate(
    atipico_iqr = marcar_atipicos(total_transaccion),
    z = (total_transaccion - mean(total_transaccion)) /
          sd(total_transaccion),
    atipico_z = abs(z) > 3
  ) %>%
  filter(atipico_iqr | atipico_z | total_transaccion <= 0)

# ---- Bloque 22 --------------------------------------------------------
sd(transacciones$total_transaccion)       # sin los registros erróneos
sd(tickets_revision$total_transaccion)    # con los registros erróneos

# ---- Bloque 23 --------------------------------------------------------
gasto_clientes <- transacciones %>%
  group_by(cliente_id) %>%
  summarise(compras = n(),
            gasto   = sum(total_transaccion),
            .groups = "drop") %>%
  mutate(atipico = marcar_atipicos(gasto),
         z       = (gasto - mean(gasto)) / sd(gasto))

gasto_clientes %>%
  filter(atipico) %>%
  arrange(desc(gasto))

# ---- Bloque 24 --------------------------------------------------------
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

p_caja

# ---- Bloque 25 --------------------------------------------------------
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

kpi_mensual

# ---- Bloque 26 --------------------------------------------------------
kpi_mensual %>%
  transmute(
    mes,
    ventas     = dollar(ventas, accuracy = 1),
    crec_mom   = percent(crec_mom, accuracy = 0.1),
    acumulado  = dollar(acumulado, accuracy = 1),
    vs_meta    = percent(vs_meta, accuracy = 0.1)
  )

# ---- Bloque 27 --------------------------------------------------------
meses_arriba <- sum(kpi_mensual$vs_meta >= 0)   # meses que cumplieron
meses_arriba
sum(kpi_mensual$ventas) - meta_mensual * 12     # diferencia anual

# ---- Bloque 28 --------------------------------------------------------
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

p_kpi

# ---- Bloque 29 --------------------------------------------------------
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

pareto %>%
  select(producto_id, ingresos, pct_ingresos, pct_acumulado, clase) %>%
  head(10)

# ---- Bloque 30 --------------------------------------------------------
pareto %>%
  group_by(clase) %>%
  summarise(productos     = n(),
            pct_catalogo  = n() / nrow(pareto),
            ingresos      = sum(ingresos),
            pct_ingresos  = sum(pct_ingresos),
            .groups = "drop")

# ---- Bloque 31 --------------------------------------------------------
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
       subtitle = "Ingresos muy repartidos: no hay pocos productos estrella",
       x = "% de productos (de mayor a menor ingreso)",
       y = "% acumulado de ingresos") +
  tema_libro()

p_pareto

# ---- Bloque 32 --------------------------------------------------------
pareto %>% count(clase, estado_stock)

# Productos A con stock bajo: prioridad de reabastecimiento
pareto %>%
  filter(clase == "A", estado_stock != "Stock OK") %>%
  select(producto_id, categoria, ingresos, estado_stock)

# ---- Bloque 33 --------------------------------------------------------
perfil_clientes <- transacciones %>%
  group_by(cliente_id) %>%
  summarise(compras   = n(),
            unidades  = sum(cantidad),
            gasto     = sum(total_transaccion),
            ticket    = mean(total_transaccion),
            .groups = "drop") %>%
  left_join(clientes, by = "cliente_id") %>%
  mutate(antiguedad = as.numeric(as.Date("2023-12-31") - fecha_registro))

# Pearson (el método por defecto) y Spearman
cor(perfil_clientes$compras, perfil_clientes$gasto)
cor(perfil_clientes$compras, perfil_clientes$gasto, method = "spearman")
cor(perfil_clientes$edad, perfil_clientes$gasto)

# ---- Bloque 34 --------------------------------------------------------
visitantes <- c(100, 120, 150, 170, 200, 220, 250, 270, 300, 320)
ventas_dia <- c(10, 12, 14, 17, 19, 22, 24, 27, 29, 300)  # la última: atípica

cor(visitantes, ventas_dia)                        # Pearson
cor(visitantes, ventas_dia, method = "spearman")   # Spearman

# ---- Bloque 35 --------------------------------------------------------
matriz_cor <- perfil_clientes %>%
  select(compras, unidades, gasto, ticket, edad, antiguedad) %>%
  cor()

round(matriz_cor, 2)

# ---- Bloque 36 --------------------------------------------------------
corrplot(matriz_cor,
         method = "color",          # celdas coloreadas
         type = "upper",            # solo el triángulo superior
         addCoef.col = "black",     # escribe el coeficiente
         number.cex = 0.8,          # tamaño de los números
         tl.col = "black",          # color de los nombres
         tl.srt = 45,               # nombres inclinados
         col = colorRampPalette(c("#e34948", "white", "#2a78d6"))(200),
         diag = FALSE)              # sin la diagonal de unos

# ---- Bloque 37 --------------------------------------------------------
cor.test(perfil_clientes$edad, perfil_clientes$gasto)

# ---- Bloque 38 --------------------------------------------------------
set.seed(123)
dias_verano <- tibble(
  temperatura = runif(90, min = 18, max = 38),           # grados
  helados     = 20 + 3 * temperatura + rnorm(90, sd = 8),
  ventiladores = 5 + 1.5 * temperatura + rnorm(90, sd = 5)
)

# Ventas de helados vs ventiladores: correlación alta...
cor(dias_verano$helados, dias_verano$ventiladores)

# ---- Bloque 39 --------------------------------------------------------
ventas_semanales <- transacciones %>%
  # semanas completas de lunes a domingo (del 2 de enero al 31 de dic.)
  filter(fecha >= as.Date("2023-01-02")) %>%
  mutate(semana = floor_date(fecha, unit = "week", week_start = 1)) %>%
  group_by(semana) %>%
  summarise(ventas = sum(total_transaccion), .groups = "drop")

media_sem <- mean(ventas_semanales$ventas)
sd_sem    <- sd(ventas_semanales$ventas)
nrow(ventas_semanales)
c(media = media_sem, desv_est = sd_sem)
asimetria(ventas_semanales$ventas)

# ---- Bloque 40 --------------------------------------------------------
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

p_normal

# ---- Bloque 41 --------------------------------------------------------
# 1. P(ventas > 25,000) = 1 - P(ventas <= 25,000)
1 - pnorm(25000, mean = media_sem, sd = sd_sem)

# Comprobación con los datos reales: proporción de semanas > 25,000
mean(ventas_semanales$ventas > 25000)

# 2. Venta semanal que solo se supera el 10% de las veces
qnorm(0.90, mean = media_sem, sd = sd_sem)

# 3. P(ventas < 12,000)
pnorm(12000, mean = media_sem, sd = sd_sem)

# ---- Bloque 42 --------------------------------------------------------
n_correos  <- 200     # intentos
p_conv     <- 0.05    # probabilidad de conversión

n_correos * p_conv                            # conversiones esperadas
dbinom(10, size = n_correos, prob = p_conv)   # P(exactamente 10)
pbinom(5, size = n_correos, prob = p_conv)    # P(5 o menos)
1 - pbinom(14, size = n_correos, prob = p_conv)   # P(15 o más)

# ---- Bloque 43 --------------------------------------------------------
# ¿Cuántos correos hay que enviar para tener 90% de probabilidad
# de conseguir al menos 15 compras?
envios <- seq(200, 600, by = 10)
prob_15_o_mas <- 1 - pbinom(14, size = envios, prob = p_conv)
min(envios[prob_15_o_mas >= 0.90])

# ---- Bloque 44 --------------------------------------------------------
tx_por_dia <- tibble(fecha = seq(as.Date("2023-01-01"),
                                 as.Date("2023-12-31"), by = "day")) %>%
  left_join(count(transacciones, fecha), by = "fecha") %>%
  mutate(n = replace_na(n, 0))              # días sin ventas = 0

lambda <- mean(tx_por_dia$n)
c(media = lambda, varianza = var(tx_por_dia$n))

# Frecuencia observada vs la que predice una Poisson(lambda)
tibble(transacciones_dia = 0:6) %>%
  mutate(
    dias_observados = map_int(transacciones_dia,
                              ~ sum(tx_por_dia$n == .x)),
    dias_poisson = round(365 * dpois(transacciones_dia, lambda), 1)
  )

# ---- Bloque 45 --------------------------------------------------------
lambda_hora <- 12
1 - ppois(16, lambda = lambda_hora)   # P(más de 16): 2 cajeros
1 - ppois(24, lambda = lambda_hora)   # P(más de 24): 3 cajeros
dpois(0, lambda = lambda_hora)        # P(ningún cliente en la hora)

# ---- Bloque 46 --------------------------------------------------------
n_tickets <- length(ticket)
error_estandar <- sd(ticket) / sqrt(n_tickets)
error_estandar

# ---- Bloque 47 --------------------------------------------------------
t_critico <- qt(0.975, df = n_tickets - 1)   # valor t para 95%
t_critico
media_ticket - t_critico * error_estandar    # límite inferior
media_ticket + t_critico * error_estandar    # límite superior

# Lo mismo en una línea:
t.test(ticket)$conf.int

# ---- Bloque 48 --------------------------------------------------------
set.seed(123)   # para que el resultado sea reproducible
medianas_boot <- replicate(5000, {
  remuestra <- sample(ticket, size = length(ticket), replace = TRUE)
  median(remuestra)
})

length(medianas_boot)                           # 5,000 medianas
quantile(medianas_boot, probs = c(0.025, 0.975))   # IC 95% (percentiles)

# ---- Bloque 49 --------------------------------------------------------
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

p_boot

# ---- Bloque 50 --------------------------------------------------------
tx_clientes <- transacciones %>%
  left_join(clientes, by = "cliente_id")

tx_clientes %>%
  group_by(es_premium) %>%
  summarise(tickets = n(),
            media   = mean(total_transaccion),
            mediana = median(total_transaccion),
            desv    = sd(total_transaccion),
            .groups = "drop")

# ---- Bloque 51 --------------------------------------------------------
prueba_premium <- t.test(total_transaccion ~ es_premium,
                         data = tx_clientes)
prueba_premium

# ---- Bloque 52 --------------------------------------------------------
prueba_premium$p.value
prueba_premium$conf.int

# ---- Bloque 53 --------------------------------------------------------
set.seed(123)
campana <- tibble(
  version  = rep(c("A", "B"), each = 2000),
  # 1 = compró, 0 = no compró; tasas reales: A 5%, B 6.5%
  convirtio = c(rbinom(2000, size = 1, prob = 0.050),
                rbinom(2000, size = 1, prob = 0.065))
)

resumen_ab <- campana %>%
  group_by(version) %>%
  summarise(enviados    = n(),
            conversiones = sum(convirtio),
            tasa        = mean(convirtio),
            .groups = "drop")
resumen_ab

# ---- Bloque 54 --------------------------------------------------------
prueba_ab <- prop.test(x = resumen_ab$conversiones,
                       n = resumen_ab$enviados)
prueba_ab

# ---- Bloque 55 --------------------------------------------------------
power.prop.test(p1 = 0.05, p2 = 0.065, power = 0.80)$n

# ---- Bloque 56 --------------------------------------------------------
set.seed(123)
campana_4000 <- tibble(
  version   = rep(c("A", "B"), each = 4000),
  convirtio = c(rbinom(4000, size = 1, prob = 0.050),
                rbinom(4000, size = 1, prob = 0.065))
)

resumen_4000 <- campana_4000 %>%
  group_by(version) %>%
  summarise(enviados = n(), conversiones = sum(convirtio),
            .groups = "drop")

prop.test(x = resumen_4000$conversiones, n = resumen_4000$enviados)

# ---- Bloque 57 --------------------------------------------------------
tx_productos <- transacciones %>%
  left_join(productos, by = "producto_id")

ticket_categoria <- tx_productos %>%
  group_by(categoria) %>%
  summarise(n     = n(),
            media = mean(total_transaccion),
            ee    = sd(total_transaccion) / sqrt(n()),
            .groups = "drop") %>%
  arrange(desc(media))
ticket_categoria

# ---- Bloque 58 --------------------------------------------------------
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
       subtitle = "Los intervalos se enciman mucho; línea gris = promedio general",
       x = "Ticket promedio", y = NULL) +
  tema_libro()

p_anova

# ---- Bloque 59 --------------------------------------------------------
modelo_anova <- aov(total_transaccion ~ categoria, data = tx_productos)
summary(modelo_anova)

# ---- Bloque 60 --------------------------------------------------------
modelo_segmento <- aov(total_transaccion ~ segmento, data = tx_clientes)
summary(modelo_segmento)

# ---- Bloque 61 --------------------------------------------------------
TukeyHSD(modelo_segmento)

# ---- Bloque 62 --------------------------------------------------------
tabla_pago <- table(transacciones$metodo_pago, transacciones$tienda_id)
tabla_pago

# ---- Bloque 63 --------------------------------------------------------
prueba_chi <- chisq.test(tabla_pago)
prueba_chi
round(prueba_chi$expected[, 1:5], 1)   # conteos esperados (5 tiendas)

# ---- Bloque 64 --------------------------------------------------------
set.seed(123)
sucursales <- tibble(
  publicidad = round(runif(60, min = 5, max = 50), 1),   # miles de $
  tamano_m2  = round(runif(60, min = 300, max = 800))    # m2
) %>%
  mutate(ventas = round(120 + 3.5 * publicidad + 0.25 * tamano_m2 +
                          rnorm(60, sd = 30), 1))        # miles de $

head(sucursales, 4)
cor(sucursales$publicidad, sucursales$ventas)

# ---- Bloque 65 --------------------------------------------------------
modelo_simple <- lm(ventas ~ publicidad, data = sucursales)
summary(modelo_simple)

# ---- Bloque 66 --------------------------------------------------------
coef(modelo_simple)
confint(modelo_simple)

# ---- Bloque 67 --------------------------------------------------------
p_regresion <- ggplot(sucursales, aes(x = publicidad, y = ventas)) +
  geom_point(color = color_principal, size = 2, alpha = 0.8) +
  # recta de regresión con su banda de confianza del 95%
  geom_smooth(method = "lm", formula = y ~ x, color = color_resalte,
              fill = color_resalte, alpha = 0.15, linewidth = 0.9) +
  annotate("text", x = 6, y = 480, hjust = 0, size = 3.5,
           label = "ventas = 261.5 + 3.39 × publicidad\nR² = 0.53") +
  scale_x_continuous(labels = label_dollar(suffix = "k")) +
  scale_y_continuous(labels = label_dollar(suffix = "k")) +
  labs(title = "Publicidad mensual vs ventas por sucursal (datos simulados)",
       subtitle = "Cada punto es una sucursal; banda = IC 95% de la recta",
       x = "Inversión en publicidad (miles)",
       y = "Ventas mensuales (miles)") +
  tema_libro()

p_regresion

# ---- Bloque 68 --------------------------------------------------------
nuevas <- tibble(publicidad = c(30, 40))

# Intervalo de confianza: para las ventas PROMEDIO de muchas sucursales
predict(modelo_simple, newdata = nuevas, interval = "confidence")

# Intervalo de predicción: para UNA sucursal concreta
predict(modelo_simple, newdata = nuevas, interval = "prediction")

# ---- Bloque 69 --------------------------------------------------------
modelo_multiple <- lm(ventas ~ publicidad + tamano_m2, data = sucursales)
summary(modelo_multiple)

# ---- Bloque 70 --------------------------------------------------------
lm(ventiladores ~ helados, data = dias_verano) %>% coef()
lm(ventiladores ~ helados + temperatura, data = dias_verano) %>%
  summary() %>%
  coef() %>%
  round(3)

# ---- Bloque 71 --------------------------------------------------------
diagnostico <- tibble(
  ajustado = fitted(modelo_multiple),     # ventas que predice el modelo
  residuo  = residuals(modelo_multiple)   # real - predicho
)

p_residuos <- ggplot(diagnostico, aes(x = ajustado, y = residuo)) +
  geom_hline(yintercept = 0, color = color_resalte, linewidth = 0.8) +
  geom_point(color = color_principal, size = 2, alpha = 0.8) +
  labs(title = "Residuos vs valores ajustados (modelo múltiple)",
       subtitle = "Nube sin patrón alrededor de cero: los supuestos se ven razonables",
       x = "Ventas ajustadas (miles)", y = "Residuo (miles)") +
  tema_libro()

p_residuos

# ---- Bloque 72 --------------------------------------------------------
base <- transacciones %>%
  left_join(clientes %>%
              select(cliente_id, es_premium, segmento, fecha_registro),
            by = "cliente_id") %>%
  left_join(productos %>%
              select(producto_id, categoria, precio_catalogo, costo),
            by = "producto_id") %>%
  left_join(tiendas %>%
              select(tienda_id, nombre_tienda, tipo, tamano_m2, empleados),
            by = "tienda_id") %>%
  mutate(costo_total = cantidad * costo,                 # costo de lo vendido
         utilidad    = total_transaccion - costo_total)  # utilidad bruta

dim(base)
sum(is.na(base$categoria)) + sum(is.na(base$nombre_tienda))  # sin pareja

# ---- Bloque 73 --------------------------------------------------------
base %>%
  summarise(
    antes_registro  = sum(fecha < fecha_registro),   # compras "imposibles"
    clientes_afect  = n_distinct(cliente_id[fecha < fecha_registro]),
    bajo_costo      = sum(precio_venta < costo),     # vendidas con pérdida
    pct_bajo_costo  = mean(precio_venta < costo)
  )

# Productos cuyo costo supera su precio de catálogo
productos %>% filter(costo > precio_catalogo) %>% nrow()

# ---- Bloque 74 --------------------------------------------------------
base %>%
  summarise(
    ventas          = sum(total_transaccion),
    utilidad        = sum(utilidad),
    margen          = sum(utilidad) / sum(total_transaccion),
    clientes        = n_distinct(cliente_id),
    ticket_promedio = mean(total_transaccion),
    ticket_mediano  = median(total_transaccion)
  )

# ---- Bloque 75 --------------------------------------------------------
productividad <- base %>%
  group_by(nombre_tienda, tipo, tamano_m2, empleados) %>%
  summarise(ventas = sum(total_transaccion),
            margen = sum(utilidad) / sum(total_transaccion),
            .groups = "drop") %>%
  mutate(ventas_m2       = ventas / tamano_m2,
         ventas_empleado = ventas / empleados) %>%
  arrange(desc(ventas_m2))

productividad %>%
  select(nombre_tienda, tipo, ventas, margen, ventas_m2, ventas_empleado)

# ---- Bloque 76 --------------------------------------------------------
# ¿El tamaño se relaciona con las ventas totales?
cor(productividad$tamano_m2, productividad$ventas, method = "spearman")

# ---- Bloque 77 --------------------------------------------------------
pruebas <- tibble(
  pregunta = c("Ticket: premium vs no premium",
               "Ticket: entre categorías",
               "Ticket: entre tiendas",
               "Método de pago vs tienda"),
  p_valor = c(
    t.test(total_transaccion ~ es_premium, data = base)$p.value,
    summary(aov(total_transaccion ~ categoria, data = base))[[1]]$`Pr(>F)`[1],
    summary(aov(total_transaccion ~ nombre_tienda, data = base))[[1]]$`Pr(>F)`[1],
    chisq.test(table(base$metodo_pago, base$tienda_id))$p.value
  )
) %>%
  mutate(significativo = p_valor < 0.05)

pruebas

# ---- Bloque 78 --------------------------------------------------------
set.seed(123)
p_valores_ruido <- replicate(100, {
  grupo_al_azar <- sample(c("A", "B"), size = length(ticket),
                          replace = TRUE)          # variable sin sentido
  t.test(ticket ~ grupo_al_azar)$p.value
})
sum(p_valores_ruido < 0.05)     # "descubrimientos" falsos
which(p_valores_ruido < 0.05)   # ¿cuáles variables?
round(min(p_valores_ruido), 4)  # el p-valor más pequeño

# ---- Bloque 79 --------------------------------------------------------
canales <- tibble(
  canal   = c("Tienda física", "Tienda en línea"),
  tickets = c(9500, 500),
  ticket_promedio = c(400, 1500)
)

mean(canales$ticket_promedio)          # promedio de promedios: MAL
weighted.mean(canales$ticket_promedio,
              w = canales$tickets)     # promedio ponderado: BIEN

# ---- Bloque 80 --------------------------------------------------------
ventas_con_faltantes <- c(1200, 950, NA, 1100, 870)
mean(ventas_con_faltantes)                 # NA
mean(ventas_con_faltantes, na.rm = TRUE)   # ignora el NA
sum(is.na(ventas_con_faltantes))           # ¿cuántos faltan?

# ---- Bloque 81 --------------------------------------------------------
tx_clientes %>%
  group_by(ciudad) %>%
  summarise(tickets = n(),
            clientes_distintos = n_distinct(cliente_id),
            media = mean(total_transaccion),
            ee    = sd(total_transaccion) / sqrt(n()),
            .groups = "drop") %>%
  arrange(desc(media)) %>%
  head(4)

# ---- Bloque 82 --------------------------------------------------------
cierres <- tibble(
  vendedor = rep(c("Ana", "Beto"), each = 2),
  tipo_cliente = rep(c("Nuevo", "Recurrente"), times = 2),
  atendidos = c(80, 20, 20, 80),
  ventas_cerradas = c(24, 18, 4, 64)
)

# Tasa de cierre por tipo de cliente: Ana gana en AMBOS
cierres %>%
  mutate(tasa = ventas_cerradas / atendidos) %>%
  select(vendedor, tipo_cliente, tasa) %>%
  pivot_wider(names_from = vendedor, values_from = tasa)

# Tasa de cierre total: ¡Beto gana!
cierres %>%
  group_by(vendedor) %>%
  summarise(tasa_total = sum(ventas_cerradas) / sum(atendidos),
            .groups = "drop")

# ---- Solo referencia (no se ejecuta automáticamente) ----
# t.test(total_transaccion ~ metodo_pago, data = transacciones)
# # Error in t.test.formula(...) : grouping factor must have exactly 2 levels
# # Solución: t.test() compara 2 grupos; con 3 o más usa aov().
#
# cor(transacciones$metodo_pago, transacciones$total_transaccion)
# # Error in cor(...) : 'x' must be numeric
# # Solución: cor() solo acepta columnas numéricas; para una categórica
# # y una numérica compara grupos (t.test, aov).
#
# chisq.test(matrix(c(3, 5, 2, 8), nrow = 2))
# # Warning message: Chi-squared approximation may be incorrect
# # Solución: hay conteos esperados menores a 5; usa fisher.test().
