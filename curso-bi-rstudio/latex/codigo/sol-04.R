# ==========================================================================
# sol-04.tex
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/sol-04.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Bloque 1 --------------------------------------------------------
library(tidyverse)
options(cli.unicode = FALSE)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)

# ---- Bloque 2 --------------------------------------------------------
precio <- transacciones$precio_venta

c(media    = mean(precio),
  mediana  = median(precio),
  desv_est = sd(precio),
  cv       = sd(precio) / mean(precio),
  iqr      = IQR(precio))
quantile(precio, probs = c(0.10, 0.90))

# ---- Bloque 3 --------------------------------------------------------
library(tidyverse)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)

moda <- function(x) {
  frecuencias <- table(x)
  names(frecuencias)[which.max(frecuencias)]
}

# ---- Bloque 4 --------------------------------------------------------
moda(transacciones$dia_semana)

transacciones %>%
  group_by(dia_semana) %>%
  summarise(transacciones = n(),
            ventas = sum(total_transaccion),
            .groups = "drop") %>%
  mutate(pct_transacciones = transacciones / sum(transacciones)) %>%
  arrange(desc(transacciones))

# ---- Bloque 5 --------------------------------------------------------
library(tidyverse)
empleados <- read_csv("datasets/empleados.csv", show_col_types = FALSE)

# ---- Bloque 6 --------------------------------------------------------
empleados %>%
  group_by(departamento) %>%
  summarise(empleados  = n(),
            media      = mean(salario_mensual),
            mediana    = median(salario_mensual),
            diferencia = media - mediana,
            .groups = "drop") %>%
  arrange(desc(abs(diferencia)))

# ---- Bloque 7 --------------------------------------------------------
library(tidyverse)
ventas <- read_csv("datasets/ventas_retail.csv", show_col_types = FALSE)

# ---- Bloque 8 --------------------------------------------------------
q1 <- quantile(ventas$total, 0.25)
q3 <- quantile(ventas$total, 0.75)
limite_superior <- unname(q3 + 1.5 * (q3 - q1))
limite_superior

ventas_marcadas <- ventas %>%
  mutate(atipico_iqr = total < q1 - 1.5 * (q3 - q1) |
                       total > limite_superior,
         z = (total - mean(total)) / sd(total),
         atipico_z = abs(z) > 3)

sum(ventas_marcadas$atipico_iqr)   # regla IQR
sum(ventas_marcadas$atipico_z)     # regla z

ventas_marcadas %>%
  filter(atipico_iqr) %>%
  select(fecha, cantidad, precio_unitario, descuento_pct, total, z)

# Asimetría
mean((ventas$total - mean(ventas$total))^3) / sd(ventas$total)^3

# ---- Bloque 9 --------------------------------------------------------
library(tidyverse)
library(scales)
ventas <- read_csv("datasets/ventas_retail.csv", show_col_types = FALSE)

# ---- Bloque 10 --------------------------------------------------------
meta_trimestral <- 130000

kpi_trimestral <- ventas %>%
  group_by(trimestre) %>%
  summarise(ventas = sum(total), .groups = "drop") %>%
  arrange(trimestre) %>%
  mutate(crec_qoq   = ventas / lag(ventas) - 1,
         acumulado  = cumsum(ventas),
         indice_100 = ventas / first(ventas) * 100,
         vs_meta    = ventas / meta_trimestral - 1)
kpi_trimestral

sum(kpi_trimestral$vs_meta >= 0)            # trimestres en meta
sum(kpi_trimestral$ventas) / (4 * meta_trimestral) - 1   # vs meta anual

# ---- Bloque 11 --------------------------------------------------------
library(tidyverse)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
clientes <- read_csv("datasets/clientes.csv", show_col_types = FALSE)

# ---- Bloque 12 --------------------------------------------------------
frecuencia <- transacciones %>%
  count(cliente_id, name = "compras") %>%          # compras por cliente
  left_join(clientes, by = "cliente_id")

frecuencia %>%
  group_by(es_premium) %>%
  summarise(clientes = n(), compras_prom = mean(compras),
            .groups = "drop")

t.test(compras ~ es_premium, data = frecuencia)

# ---- Bloque 13 --------------------------------------------------------
library(tidyverse)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)

# ---- Bloque 14 --------------------------------------------------------
abc_clientes <- transacciones %>%
  group_by(cliente_id) %>%
  summarise(gasto = sum(total_transaccion), .groups = "drop") %>%
  arrange(desc(gasto)) %>%
  mutate(pct_acumulado = cumsum(gasto) / sum(gasto),
         pct_clientes  = row_number() / n(),
         clase = case_when(pct_acumulado <= 0.80 ~ "A",
                           pct_acumulado <= 0.95 ~ "B",
                           TRUE                  ~ "C"))

abc_clientes %>%
  group_by(clase) %>%
  summarise(clientes = n(),
            pct_clientes = n() / nrow(abc_clientes),
            pct_ingresos = sum(gasto) / sum(abc_clientes$gasto),
            .groups = "drop")

# Participación del 20% de clientes que más gasta
abc_clientes %>%
  filter(pct_clientes <= 0.20) %>%
  summarise(clientes = n(), pct_ingresos = max(pct_acumulado))

# ---- Bloque 15 --------------------------------------------------------
# 1. Normal: ventas semanales
pnorm(20000, mean = 18445, sd = 4591) -
  pnorm(15000, mean = 18445, sd = 4591)       # P(15,000 < X < 20,000)
qnorm(0.95, mean = 18445, sd = 4591)          # se supera 5% de las veces

# 2. Binomial: devoluciones de 150 ventas con p = 0.03
1 - pbinom(8, size = 150, prob = 0.03)        # P(más de 8)
dbinom(0, size = 150, prob = 0.03)            # P(ninguna)

# 3. Poisson: llamadas en 10 minutos con lambda = 4
1 - ppois(7, lambda = 4)                      # P(8 o más)

# ---- Bloque 16 --------------------------------------------------------
compras <- c(A = 180, B = 225)
visitas <- c(A = 3000, B = 3000)
compras / visitas                    # tasas de conversión
prop.test(x = compras, n = visitas)

# ---- Bloque 17 --------------------------------------------------------
library(tidyverse)
empleados <- read_csv("datasets/empleados.csv", show_col_types = FALSE)

# ---- Bloque 18 --------------------------------------------------------
modelo_salarios <- aov(salario_mensual ~ departamento, data = empleados)
summary(modelo_salarios)

# ---- Bloque 19 --------------------------------------------------------
library(tidyverse)
set.seed(123)
semanas <- tibble(
  precio = runif(52, min = 80, max = 120)
) %>%
  mutate(unidades = 500 - 3 * precio + rnorm(52, sd = 15))

# ---- Bloque 20 --------------------------------------------------------
modelo_precio <- lm(unidades ~ precio, data = semanas)
coef(modelo_precio)
confint(modelo_precio)
summary(modelo_precio)$r.squared

# ---- Bloque 21 --------------------------------------------------------
predict(modelo_precio, newdata = tibble(precio = 100),
        interval = "prediction")
predict(modelo_precio, newdata = tibble(precio = 200))
range(semanas$precio)       # rango de precios observado

# ---- Bloque 22 --------------------------------------------------------
diagnostico <- tibble(ajustado = fitted(modelo_precio),
                      residuo  = residuals(modelo_precio))
summary(diagnostico$residuo)
# Correlación entre residuos y ajustados (debe ser ~0)
round(cor(diagnostico$ajustado, diagnostico$residuo), 4)

ggplot(diagnostico, aes(x = ajustado, y = residuo)) +
  geom_hline(yintercept = 0, color = "#eb6834") +
  geom_point(color = "#2a78d6") +
  labs(x = "Unidades ajustadas", y = "Residuo") +
  theme_minimal()
