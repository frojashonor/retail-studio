# ==========================================================================
# sol-07.tex
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/sol-07.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Bloque 1 --------------------------------------------------------
library(tidyverse)
ventas <- read_csv("datasets/ventas_retail.csv", show_col_types = FALSE)

modelo <- lm(total ~ descuento_pct + dia_semana, data = ventas)
round(summary(modelo)$r.squared, 4)
round(summary(modelo)$adj.r.squared, 4)

# ---- Bloque 2 --------------------------------------------------------
real       <- c(120, 95, 150, 80, 110, 130)
pronostico <- c(110, 100, 140, 90, 115, 120)
error <- real - pronostico

mae  <- mean(abs(error))
rmse <- sqrt(mean(error^2))
mape <- mean(abs(error) / real) * 100
r2   <- 1 - sum(error^2) / sum((real - mean(real))^2)

round(c(MAE = mae, RMSE = rmse, MAPE = mape, R2 = r2), 3)

# ---- Bloque 3 --------------------------------------------------------
matriz <- matrix(c(890, 60, 10, 40), nrow = 2,
                 dimnames = list(Prediccion = c("Legítima", "Fraude"),
                                 Real = c("Legítima", "Fraude")))
matriz

vp <- 40; fn <- 10; fp <- 60; vn <- 890
precision    <- vp / (vp + fp)
sensibilidad <- vp / (vp + fn)
round(c(exactitud     = (vp + vn) / (vp + vn + fp + fn),
        precision     = precision,
        sensibilidad  = sensibilidad,
        especificidad = vn / (vn + fp),
        F1 = 2 * precision * sensibilidad / (precision + sensibilidad)), 3)

costo_errores <- fn * 5000 + fp * 100
costo_errores

# ---- Bloque 4 --------------------------------------------------------
library(tidyverse)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
fecha_corte <- as.Date("2024-01-01")

niveles <- transacciones %>%
  group_by(cliente_id) %>%
  summarise(recencia   = as.numeric(fecha_corte - max(fecha)),
            frecuencia = n(),
            monto      = sum(total_transaccion), .groups = "drop") %>%
  mutate(r = ntile(desc(recencia), 5),
         f = ntile(frecuencia, 5),
         m = ntile(monto, 5),
         puntaje = r + f + m,
         nivel = case_when(puntaje >= 13 ~ "Oro",
                           puntaje >= 9  ~ "Plata",
                           TRUE          ~ "Bronce"),
         nivel = factor(nivel, levels = c("Oro", "Plata", "Bronce")))

niveles %>%
  group_by(nivel) %>%
  summarise(clientes = n(),
            monto_prom = round(mean(monto)),
            ingresos = sum(monto), .groups = "drop") %>%
  mutate(pct_ingresos = round(100 * ingresos / sum(ingresos), 1)) %>%
  select(-ingresos)

# ---- Bloque 5 --------------------------------------------------------
library(tidyverse)
library(cluster)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)

perfil_cliente <- transacciones %>%
  group_by(cliente_id) %>%
  summarise(monto_total     = sum(total_transaccion),
            ticket_promedio = mean(total_transaccion),
            n_tiendas       = n_distinct(tienda_id),
            n_productos     = n_distinct(producto_id), .groups = "drop")

datos_esc <- scale(perfil_cliente %>% select(-cliente_id))

siluetas <- map_dbl(2:6, function(k) {
  set.seed(123)
  km <- kmeans(datos_esc, centers = k, nstart = 25)
  mean(silhouette(km$cluster, dist(datos_esc))[, "sil_width"])
})
tibble(k = 2:6, silueta = round(siluetas, 3))
k_elegido <- (2:6)[which.max(siluetas)]
k_elegido

# ---- Bloque 6 --------------------------------------------------------
set.seed(123)
km <- kmeans(datos_esc, centers = k_elegido, nstart = 25)

perfil <- perfil_cliente %>%
  mutate(cluster = km$cluster) %>%
  group_by(cluster) %>%
  summarise(clientes = n(),
            monto_total = round(mean(monto_total)),
            ticket = round(mean(ticket_promedio)),
            tiendas = round(mean(n_tiendas), 1),
            productos = round(mean(n_productos), 1), .groups = "drop")

# Nombres según el perfil (reglas de negocio, nunca por número de grupo)
extremos <- range(perfil$monto_total)
perfil %>%
  mutate(segmento = case_when(
    monto_total == extremos[2] ~ "Alto valor",
    monto_total == extremos[1] ~ "Valor mínimo",
    ticket == max(ticket[!monto_total %in% extremos]) ~
      "Ticket alto ocasional",
    TRUE ~ "Valor medio")) %>%
  arrange(desc(monto_total)) %>%
  select(segmento, everything(), -cluster)

# ---- Bloque 7 --------------------------------------------------------
library(tidyverse)
library(caret)

set.seed(123)
n_clientes <- 2000
clientes_churn <- tibble(
  cliente_id       = sprintf("C%04d", 1:n_clientes),
  antiguedad_meses = sample(1:72, n_clientes, replace = TRUE),
  plan             = sample(c("Básico", "Estándar", "Premium"),
                            n_clientes, replace = TRUE,
                            prob = c(0.5, 0.3, 0.2)),
  uso_mensual      = rpois(n_clientes, 8),
  quejas_6m        = rpois(n_clientes, 0.8),
  pagos_tardios    = rpois(n_clientes, 0.4),
  genero           = sample(c("F", "M"), n_clientes, replace = TRUE),
  edad             = sample(18:70, n_clientes, replace = TRUE)
) %>%
  mutate(
    efecto_plan = case_when(plan == "Básico"   ~ 0,
                            plan == "Estándar" ~ -0.6,
                            TRUE               ~ -1.3),
    riesgo = 2.0 - 0.05 * antiguedad_meses +
      1.0 * (antiguedad_meses <= 6) + 1.0 * quejas_6m -
      0.35 * uso_mensual + 0.8 * pagos_tardios + efecto_plan,
    churn = if_else(runif(n_clientes) < plogis(riesgo), "Si", "No"),
    churn = factor(churn, levels = c("No", "Si")),
    plan  = factor(plan, levels = c("Básico", "Estándar", "Premium")),
    cliente_nuevo = as.integer(antiguedad_meses <= 6)   # variable nueva
  ) %>%
  select(-efecto_plan, -riesgo)

set.seed(123)
filas_ent <- createDataPartition(clientes_churn$churn, p = 0.7,
                                 list = FALSE)
ent    <- clientes_churn[filas_ent, ]
prueba <- clientes_churn[-filas_ent, ]

calcular_auc <- function(prob, real, positivo = "Si") {
  p_pos <- prob[real == positivo]
  p_neg <- prob[real != positivo]
  mean(outer(p_pos, p_neg, ">") + 0.5 * outer(p_pos, p_neg, "=="))
}

f_original <- churn ~ antiguedad_meses + plan + uso_mensual + quejas_6m +
  pagos_tardios + genero + edad
f_nuevo    <- update(f_original, . ~ . + cliente_nuevo)
f_justo    <- update(f_nuevo, . ~ . - genero - edad)

modelos <- list(Original = f_original, `Con cliente_nuevo` = f_nuevo,
                `Sin genero ni edad` = f_justo) %>%
  map(~ glm(.x, data = ent, family = binomial))

map_dbl(modelos, ~ calcular_auc(predict(.x, prueba, type = "response"),
                                prueba$churn)) %>%
  round(3)

round(exp(coef(modelos[["Con cliente_nuevo"]])["cliente_nuevo"]), 2)

# ---- Bloque 8 --------------------------------------------------------
library(tidyverse)
library(caret)

set.seed(123)
n_clientes <- 2000
clientes_churn <- tibble(
  cliente_id       = sprintf("C%04d", 1:n_clientes),
  antiguedad_meses = sample(1:72, n_clientes, replace = TRUE),
  plan             = sample(c("Básico", "Estándar", "Premium"),
                            n_clientes, replace = TRUE,
                            prob = c(0.5, 0.3, 0.2)),
  uso_mensual      = rpois(n_clientes, 8),
  quejas_6m        = rpois(n_clientes, 0.8),
  pagos_tardios    = rpois(n_clientes, 0.4),
  genero           = sample(c("F", "M"), n_clientes, replace = TRUE),
  edad             = sample(18:70, n_clientes, replace = TRUE)
) %>%
  mutate(
    efecto_plan = case_when(plan == "Básico"   ~ 0,
                            plan == "Estándar" ~ -0.6,
                            TRUE               ~ -1.3),
    riesgo = 2.0 - 0.05 * antiguedad_meses +
      1.0 * (antiguedad_meses <= 6) + 1.0 * quejas_6m -
      0.35 * uso_mensual + 0.8 * pagos_tardios + efecto_plan,
    churn = if_else(runif(n_clientes) < plogis(riesgo), "Si", "No"),
    churn = factor(churn, levels = c("No", "Si")),
    plan  = factor(plan, levels = c("Básico", "Estándar", "Premium"))
  ) %>%
  select(-efecto_plan, -riesgo)

set.seed(123)
filas_ent <- createDataPartition(clientes_churn$churn, p = 0.7,
                                 list = FALSE)
ent    <- clientes_churn[filas_ent, ]
prueba <- clientes_churn[-filas_ent, ]

modelo_log <- glm(churn ~ antiguedad_meses + plan + uso_mensual +
                    quejas_6m + pagos_tardios + genero + edad,
                  data = ent, family = binomial)
prob <- predict(modelo_log, prueba, type = "response")
real <- prueba$churn == "Si"

costo_descuento <- 800
costo_vp <- costo_descuento + 0.5 * 3000
costo_fp <- costo_descuento
costo_fn <- 3000

costos <- map_dfr(seq(0.05, 0.95, by = 0.05), function(u) {
  pred <- prob >= u
  tibble(umbral = u, llamadas = sum(pred),
         costo_total = sum(pred & real) * costo_vp +
           sum(pred & !real) * costo_fp + sum(!pred & real) * costo_fn)
})
costos %>% arrange(costo_total) %>% head(3)

# ---- Bloque 9 --------------------------------------------------------
library(tidyverse)
library(forecast)

set.seed(123)
meses <- seq(as.Date("2021-01-01"), as.Date("2025-12-01"), by = "month")
n_meses <- length(meses)
factor_mes <- c(0.88, 0.85, 0.95, 0.97, 1.04, 0.96,
                0.97, 1.00, 0.93, 0.98, 1.15, 1.32)
factor_mes <- factor_mes / mean(factor_mes)
unidades <- round((1500 + 15 * (1:n_meses)) * factor_mes[month(meses)] *
                    (1 + rnorm(n_meses, 0, 0.03)))
serie <- ts(unidades, start = c(2021, 1), frequency = 12)

# Suma por trimestre: frecuencia 4
serie_trim <- aggregate(serie, nfrequency = 4, FUN = sum)
serie_trim

ent    <- window(serie_trim, end = c(2024, 4))
prueba <- window(serie_trim, start = c(2025, 1))

pronosticos <- list(
  `Ingenuo estacional` = snaive(ent, h = 4),
  ETS   = forecast(ets(ent), h = 4),
  ARIMA = forecast(auto.arima(ent), h = 4)
)
comparacion <- map_dfr(pronosticos, function(p) {
  acc <- accuracy(p, prueba)["Test set", ]
  tibble(MAE = acc["MAE"], RMSE = acc["RMSE"], MAPE = acc["MAPE"])
}, .id = "modelo") %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  arrange(MAPE)
comparacion

# ---- Bloque 10 --------------------------------------------------------
# Reentrenamos el ganador (menor MAPE) con toda la serie trimestral
ganador <- comparacion$modelo[1]
modelo_final <- switch(ganador,
                       ETS   = ets(serie_trim),
                       ARIMA = auto.arima(serie_trim),
                       snaive(serie_trim, h = 4))
ganador
forecast(modelo_final, h = 4)

# ---- Bloque 11 --------------------------------------------------------
library(tidyverse)

set.seed(123)
n_tickets <- 3000
tickets_ancho <- tibble(
  ticket_id = 1:n_tickets,
  Laptop    = rbinom(n_tickets, 1, 0.08),
  Monitor   = rbinom(n_tickets, 1, 0.07),
  Impresora = rbinom(n_tickets, 1, 0.05),
  Audifonos = rbinom(n_tickets, 1, 0.15),
  USB       = rbinom(n_tickets, 1, 0.18),
  Papel     = rbinom(n_tickets, 1, 0.20),
  Teclado   = rbinom(n_tickets, 1, 0.08)
) %>%
  mutate(
    Mouse = if_else(Laptop == 1, rbinom(n(), 1, 0.60),
                    rbinom(n(), 1, 0.08)),
    Funda = if_else(Laptop == 1, rbinom(n(), 1, 0.40),
                    rbinom(n(), 1, 0.02)),
    `Cable HDMI` = if_else(Monitor == 1, rbinom(n(), 1, 0.50),
                           rbinom(n(), 1, 0.04)),
    Tinta = if_else(Impresora == 1, rbinom(n(), 1, 0.70),
                    rbinom(n(), 1, 0.06))
  )
tickets <- tickets_ancho %>%
  pivot_longer(-ticket_id, names_to = "producto", values_to = "compro") %>%
  filter(compro == 1) %>%
  select(-compro)

# Una fila por ticket con TRUE/FALSE para los productos de interés
por_ticket <- tickets %>%
  group_by(ticket_id) %>%
  summarise(laptop = "Laptop" %in% producto,
            mouse  = "Mouse" %in% producto,
            funda  = "Funda" %in% producto, .groups = "drop")
n_total <- nrow(por_ticket)

soporte_funda <- mean(por_ticket$funda)
tres <- por_ticket %>%
  reframe(regla = c("Laptop+Mouse -> Funda", "Laptop -> Funda"),
            tickets_x  = c(sum(laptop & mouse), sum(laptop)),
            tickets_xy = c(sum(laptop & mouse & funda),
                           sum(laptop & funda)))
tres %>%
  mutate(soporte   = round(tickets_xy / n_total, 3),
         confianza = round(tickets_xy / tickets_x, 3),
         lift      = round(tickets_xy / tickets_x / soporte_funda, 2))

# ---- Bloque 12 --------------------------------------------------------
library(tidyverse)
library(caret)
library(randomForest)

simular_clientes <- function(n, prefijo = "C") {
  tibble(
    cliente_id       = sprintf("%s%05d", prefijo, 1:n),
    antiguedad_meses = sample(1:72, n, replace = TRUE),
    plan             = sample(c("Básico", "Estándar", "Premium"), n,
                              replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    uso_mensual      = rpois(n, 8),
    quejas_6m        = rpois(n, 0.8),
    pagos_tardios    = rpois(n, 0.4),
    genero           = sample(c("F", "M"), n, replace = TRUE),
    edad             = sample(18:70, n, replace = TRUE)
  ) %>%
    mutate(
      efecto_plan = case_when(plan == "Básico"   ~ 0,
                              plan == "Estándar" ~ -0.6,
                              TRUE               ~ -1.3),
      riesgo = 2.0 - 0.05 * antiguedad_meses +
        1.0 * (antiguedad_meses <= 6) + 1.0 * quejas_6m -
        0.35 * uso_mensual + 0.8 * pagos_tardios + efecto_plan,
      churn = if_else(runif(n) < plogis(riesgo), "Si", "No"),
      churn = factor(churn, levels = c("Si", "No")),  # "Si" primero: caret
      plan  = factor(plan, levels = c("Básico", "Estándar", "Premium")),
      cuota_mensual = case_when(plan == "Básico"   ~ 199,
                                plan == "Estándar" ~ 349,
                                TRUE               ~ 599)
    ) %>%
    select(-efecto_plan, -riesgo)
}

set.seed(2024)
historico <- simular_clientes(3000)
set.seed(2024)
filas_ent <- createDataPartition(historico$churn, p = 0.7, list = FALSE)
ent    <- historico[filas_ent, ]
prueba <- historico[-filas_ent, ]

# Sin género ni edad (sección de ética)
formula_churn <- churn ~ antiguedad_meses + plan + uso_mensual +
  quejas_6m + pagos_tardios
control <- trainControl(method = "cv", number = 5, classProbs = TRUE,
                        summaryFunction = twoClassSummary)
set.seed(2024)
m_log <- train(formula_churn, data = ent, method = "glm",
               family = binomial, trControl = control, metric = "ROC")
set.seed(2024)
m_rf <- train(formula_churn, data = ent, method = "rf", ntree = 200,
              trControl = control, metric = "ROC",
              tuneGrid = data.frame(mtry = 2:3))

calcular_auc <- function(prob, real, positivo = "Si") {
  p_pos <- prob[real == positivo]
  p_neg <- prob[real != positivo]
  mean(outer(p_pos, p_neg, ">") + 0.5 * outer(p_pos, p_neg, "=="))
}
auc_prueba <- c(
  Logistica = calcular_auc(predict(m_log, prueba, type = "prob")[, "Si"],
                           prueba$churn),
  RandomForest = calcular_auc(predict(m_rf, prueba, type = "prob")[, "Si"],
                              prueba$churn))
round(auc_prueba, 3)

mejor <- list(Logistica = m_log, RandomForest = m_rf)[[
  names(which.max(auc_prueba))]]
dir.create("resultados", showWarnings = FALSE)
saveRDS(mejor, "resultados/modelo_churn_reto.rds")

# ---- Bloque 13 --------------------------------------------------------
puntuar <- function(nuevos, modelo, top = 100) {
  nuevos %>%
    mutate(prob_churn = predict(modelo, newdata = nuevos,
                                type = "prob")[, "Si"],
           perdida_esperada = prob_churn * cuota_mensual * 12) %>%
    arrange(desc(perdida_esperada)) %>%
    slice_head(n = top) %>%
    mutate(prioridad = row_number()) %>%
    select(prioridad, cliente_id, plan, quejas_6m, prob_churn,
           perdida_esperada)
}

set.seed(99)
lote_nuevo <- simular_clientes(1000, prefijo = "N") %>% select(-churn)

modelo <- readRDS("resultados/modelo_churn_reto.rds")
top100 <- puntuar(lote_nuevo, modelo)
write_csv(top100, "resultados/top100_churn.csv")

top100 %>%
  mutate(across(c(prob_churn, perdida_esperada), ~ round(.x, 2))) %>%
  head(5)
nrow(read_csv("resultados/top100_churn.csv", show_col_types = FALSE))
