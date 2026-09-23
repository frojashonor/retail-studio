# ============================================================================
# Figuras del Módulo 7 (machine learning)
# Repite, en el mismo orden, el código de los bloques del capítulo
# capitulos/07-machine-learning.tex y guarda cada gráfica con guardar_figura().
# Ejecutar desde una carpeta que contenga datasets/.
# ============================================================================
source(file.path(Sys.getenv("DIR_LIBRO"), "herramientas/figuras/tema_libro.R"))
options(width = 70, cli.unicode = FALSE)
directorio_original <- getwd()
carpeta_temporal <- tempfile("fig07_")
dir.create(carpeta_temporal)
file.copy("datasets", carpeta_temporal, recursive = TRUE)
setwd(carpeta_temporal)   # para no dejar resultados/ en la carpeta de trabajo
pdf(NULL)                 # evita crear Rplots.pdf

# ---- Código del capítulo -----------------------------------------------------
suppressPackageStartupMessages({
library(tidyverse)     # dplyr, ggplot2, readr, tidyr, purrr...
library(cluster)       # silueta y otras medidas de clustering
library(factoextra)    # gráficas de clustering (devuelven ggplot)
library(rpart)         # árboles de decisión
library(randomForest)  # bosques aleatorios
library(caret)         # entrenar y comparar modelos con un mismo formato
library(forecast)      # series de tiempo: ETS, ARIMA, pronósticos

# Comillas simples normales en las salidas de R (en lugar de tipográficas)
options(useFancyQuotes = FALSE)

# Colores corporativos del libro (Módulo 3)
color_principal <- "#2a78d6"
color_resalte   <- "#eb6834"
color_gris      <- "#b5b3ad"
paleta_libro <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                  "#e87ba4", "#008300", "#4a3aa7", "#e34948")
})

# Transacciones del curso (1,000 compras de 2023)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)

# ¿El monto depende de la tienda, el método de pago o el día?
modelo_sin_senal <- lm(
  total_transaccion ~ factor(tienda_id) + metodo_pago + dia_semana,
  data = transacciones
)

# R²: proporción de la variación del monto que explica el modelo (0 a 1)
summary(modelo_sin_senal)$r.squared
summary(modelo_sin_senal)$adj.r.squared   # R² ajustado

set.seed(123)   # fija el azar: cualquiera que corra esto obtiene lo mismo
sucursales <- tibble(
  sucursal   = 1:60,
  publicidad = round(runif(60, min = 5, max = 50), 1),   # miles de $
  ventas     = round(200 + 6 * publicidad + rnorm(60, 0, 25), 1)
)

# Elegimos al azar 45 filas (75 %) para entrenar
set.seed(123)
filas_ent <- sample(nrow(sucursales), size = 45)

suc_entrenamiento <- sucursales[filas_ent, ]    # 45 sucursales
suc_prueba        <- sucursales[-filas_ent, ]   # las 15 restantes

nrow(suc_entrenamiento)
nrow(suc_prueba)

# Función auxiliar: error típico de una predicción
rmse <- function(real, predicho) sqrt(mean((real - predicho)^2))

pocas_sucursales <- sucursales %>% slice(1:14)    # entrenamiento chico
resto_sucursales <- sucursales %>% slice(15:60)   # 46 casos "nuevos"

errores_por_grado <- map_dfr(1:10, function(g) {
  modelo <- lm(ventas ~ poly(publicidad, g), data = pocas_sucursales)
  tibble(
    grado = g,
    error_entrenamiento = rmse(pocas_sucursales$ventas, fitted(modelo)),
    error_prueba = rmse(resto_sucursales$ventas,
                        predict(modelo, resto_sucursales))
  )
})

errores_por_grado %>% mutate(across(-grado, ~ round(.x, 1)))

malla <- tibble(publicidad = seq(min(pocas_sucursales$publicidad),
                                 max(pocas_sucursales$publicidad),
                                 length.out = 300))
recta   <- lm(ventas ~ publicidad, data = pocas_sucursales)
memoria <- lm(ventas ~ poly(publicidad, 10), data = pocas_sucursales)

curvas <- bind_rows(
  malla %>% mutate(modelo = "Grado 1: generaliza",
                   ventas = predict(recta, malla)),
  malla %>% mutate(modelo = "Grado 10: memoriza",
                   ventas = predict(memoria, malla))
)

grafico_sobreajuste <- ggplot() +
  geom_point(data = resto_sucursales, aes(publicidad, ventas),
             color = color_gris, alpha = 0.8) +
  geom_point(data = pocas_sucursales, aes(publicidad, ventas),
             color = "black", size = 2.2) +
  geom_line(data = curvas, aes(publicidad, ventas, color = modelo),
            linewidth = 0.8) +
  scale_color_manual(values = c(color_principal, color_resalte)) +
  coord_cartesian(ylim = c(150, 600)) +
  labs(title = "Un modelo demasiado flexible memoriza el ruido",
       subtitle = paste("Negro: 14 sucursales de entrenamiento;",
                        "gris: sucursales nuevas"),
       x = "Publicidad (miles de $)", y = "Ventas (miles de $)",
       color = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), legend.position = "top")
grafico_sobreajuste

set.seed(123)
# Asigna cada sucursal a uno de 5 pliegues, al azar y en partes iguales
pliegue <- sample(rep(1:5, length.out = nrow(sucursales)))
table(pliegue)

error_cv <- function(grado) {
  errores <- map_dbl(1:5, function(k) {
    entrena <- sucursales[pliegue != k, ]   # 4 pliegues para aprender
    valida  <- sucursales[pliegue == k, ]   # 1 pliegue para evaluar
    modelo  <- lm(ventas ~ poly(publicidad, grado), data = entrena)
    rmse(valida$ventas, predict(modelo, valida))
  })
  mean(errores)   # promedio de los 5 errores
}

tibble(grado = c(1, 3, 6, 10)) %>%
  mutate(rmse_cv = map_dbl(grado, error_cv))

clientes <- read_csv("datasets/clientes.csv", show_col_types = FALSE)

fecha_corte <- as.Date("2024-01-01")   # fecha fija del análisis

rfm <- transacciones %>%
  group_by(cliente_id) %>%
  summarise(
    ultima_compra = max(fecha),
    recencia   = as.numeric(fecha_corte - ultima_compra),  # días
    frecuencia = n(),                                      # compras
    monto      = sum(total_transaccion),                   # pesos
    .groups = "drop"
  )

rfm

# Clientes registrados que no aparecen en las transacciones
clientes %>%
  anti_join(rfm, by = "cliente_id") %>%
  select(cliente_id, nombre, fecha_registro)

rfm %>%
  select(recencia, frecuencia, monto) %>%
  summary()

rfm_escalado <- rfm %>%
  select(recencia, frecuencia, monto) %>%
  scale()                     # (x - media) / desviación estándar

round(colMeans(rfm_escalado), 2)          # medias: todas 0
round(apply(rfm_escalado, 2, sd), 2)      # desviaciones: todas 1
head(round(rfm_escalado, 2), 3)

set.seed(123)
codo <- tibble(k = 1:10) %>%
  mutate(wss = map_dbl(k, function(k) {
    kmeans(rfm_escalado, centers = k, nstart = 25)$tot.withinss
  }))

codo %>%
  mutate(mejora_pct = round(100 * (lag(wss) - wss) / lag(wss), 1),
         wss = round(wss, 1))

grafico_codo <- ggplot(codo, aes(x = k, y = wss)) +
  geom_line(color = color_principal, linewidth = 0.8) +
  geom_point(color = color_principal, size = 2) +
  geom_point(data = filter(codo, k == 4), color = color_resalte,
             size = 4) +
  annotate("text", x = 4.3, y = codo$wss[4] + 60, hjust = 0,
           label = "k = 4: las mejoras\nempiezan a ser pequeñas",
           color = color_resalte, size = 3.5) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Método del codo",
       subtitle = "Dispersión dentro de los grupos según k",
       x = "Número de grupos (k)",
       y = "Suma de cuadrados dentro (WSS)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
grafico_codo

set.seed(123)
grafico_silueta <- fviz_nbclust(rfm_escalado, kmeans,
                                method = "silhouette", k.max = 10,
                                nstart = 25, linecolor = color_principal) +
  labs(title = "Silueta promedio según el número de grupos",
       subtitle = "Más alto = grupos más separados",
       x = "Número de grupos (k)", y = "Silueta promedio") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
grafico_silueta

# Los valores calculados vienen dentro del objeto ggplot
grafico_silueta$data %>%
  mutate(y = round(y, 3)) %>%
  filter(clusters %in% 2:6)

set.seed(123)
modelo_kmeans <- kmeans(rfm_escalado, centers = 4, nstart = 25)

modelo_kmeans$size                     # clientes por grupo
round(modelo_kmeans$centers, 2)        # centros (en escala estandarizada)

# Proporción de la variación total explicada por los grupos
round(modelo_kmeans$betweenss / modelo_kmeans$totss, 3)

silueta <- silhouette(modelo_kmeans$cluster, dist(rfm_escalado))

# Silueta promedio por grupo y cuántos clientes quedaron "mal ubicados"
round(summary(silueta)$clus.avg.widths, 3)
sum(silueta[, "sil_width"] < 0)

rfm_seg <- rfm %>%
  mutate(cluster = modelo_kmeans$cluster)

perfil <- rfm_seg %>%
  group_by(cluster) %>%
  summarise(
    clientes        = n(),
    recencia_prom   = mean(recencia),
    frecuencia_prom = mean(frecuencia),
    monto_prom      = mean(monto),
    ingresos        = sum(monto),
    .groups = "drop"
  ) %>%
  mutate(pct_clientes = 100 * clientes / sum(clientes),
         pct_ingresos = 100 * ingresos / sum(ingresos))

perfil %>%
  select(cluster, clientes, recencia_prom, frecuencia_prom, monto_prom,
         pct_ingresos) %>%
  mutate(across(where(is.double), ~ round(.x, 1)))

nombres_segmentos <- perfil %>%
  mutate(es_dormido = recencia_prom == max(recencia_prom)) %>%
  arrange(es_dormido, desc(monto_prom)) %>%   # dormidos al final
  mutate(segmento = c("Campeones", "Leales", "Ocasionales",
                      "Dormidos")) %>%
  select(cluster, segmento)

orden_segmentos <- c("Campeones", "Leales", "Ocasionales", "Dormidos")

rfm_seg <- rfm_seg %>%
  left_join(nombres_segmentos, by = "cluster") %>%
  mutate(segmento = factor(segmento, levels = orden_segmentos))

perfil_segmentos <- perfil %>%
  left_join(nombres_segmentos, by = "cluster") %>%
  mutate(segmento = factor(segmento, levels = orden_segmentos)) %>%
  arrange(segmento) %>%
  transmute(segmento, clientes,
            recencia_dias = round(recencia_prom),
            compras       = round(frecuencia_prom, 1),
            monto_prom    = round(monto_prom),
            pct_ingresos  = round(pct_ingresos, 1))
perfil_segmentos

grafico_clusters <- fviz_cluster(modelo_kmeans, data = rfm_escalado,
                                 geom = "point", ellipse.type = "convex",
                                 palette = paleta_libro[1:4],
                                 ggtheme = theme_minimal()) +
  labs(title = "Grupos de k-means sobre el RFM escalado",
       subtitle = "Proyección en los dos componentes principales",
       color = "Grupo", fill = "Grupo", shape = "Grupo")
grafico_clusters

grafico_segmentos <- ggplot(rfm_seg,
                            aes(recencia, monto, color = segmento)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_manual(values = paleta_libro[1:4]) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Segmentos de clientes según recencia y monto",
       subtitle = "Clientes del curso, compras de 2023",
       x = "Días desde la última compra (recencia)",
       y = "Monto total comprado", color = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), legend.position = "top")
grafico_segmentos

rfm_seg %>%
  # de clientes.csv solo tomamos lo que necesitamos (ya trae una
  # columna "segmento" por edad que chocaría con la nuestra)
  left_join(select(clientes, cliente_id, edad, es_premium),
            by = "cliente_id") %>%
  group_by(segmento) %>%
  summarise(clientes    = n(),
            edad_prom   = round(mean(edad), 1),
            pct_premium = round(100 * mean(es_premium), 1),
            .groups = "drop")

asignar_segmento <- function(nuevos, modelo, datos_escalados, nombres) {
  medias   <- attr(datos_escalados, "scaled:center")
  desv_est <- attr(datos_escalados, "scaled:scale")
  # Escalamos con los parámetros del entrenamiento (no con los nuevos)
  nuevos_esc <- scale(nuevos[, names(medias)], center = medias,
                      scale = desv_est)
  # Para cada cliente: el centro con menor distancia al cuadrado
  grupo <- apply(nuevos_esc, 1, function(fila) {
    which.min(colSums((t(modelo$centers) - fila)^2))
  })
  nuevos %>%
    mutate(cluster = grupo) %>%
    left_join(nombres, by = "cluster")
}

nuevos_clientes <- tibble(
  cliente    = c("Ana", "Beto", "Carla"),
  recencia   = c(10, 250, 45),
  frecuencia = c(11, 2, 4),
  monto      = c(9500, 1800, 3500)
)

asignar_segmento(nuevos_clientes, modelo_kmeans, rfm_escalado,
                 nombres_segmentos)

rfm_reglas <- rfm %>%
  mutate(
    r_score = ntile(desc(recencia), 5),   # 5 = compró más recientemente
    f_score = ntile(frecuencia, 5),       # 5 = compra más seguido
    m_score = ntile(monto, 5),            # 5 = gasta más
    codigo_rfm = paste0(r_score, f_score, m_score),
    segmento_reglas = case_when(
      r_score >= 4 & f_score >= 4 & m_score >= 4 ~ "Campeones",
      r_score >= 3 & f_score >= 3                ~ "Leales",
      r_score <= 2 & f_score >= 3                ~ "En riesgo",
      r_score <= 2                               ~ "Dormidos",
      TRUE                                       ~ "Ocasionales"
    )
  )

rfm_reglas %>%
  select(cliente_id, recencia, frecuencia, monto, codigo_rfm,
         segmento_reglas) %>%
  head(5)

rfm_reglas %>% count(segmento_reglas, sort = TRUE)

table(Reglas = rfm_reglas$segmento_reglas, kmeans = rfm_seg$segmento)

set.seed(123)
n_tiendas <- 400
tiendas_sim <- tibble(
  tienda     = sprintf("T%03d", 1:n_tiendas),
  zona       = sample(c("Centro", "Norte", "Sur"), n_tiendas,
                      replace = TRUE),
  tamano_m2  = round(runif(n_tiendas, 150, 1200)),
  publicidad = round(runif(n_tiendas, 10, 120), 1),  # miles de $/mes
  promocion  = rbinom(n_tiendas, 1, 0.35),           # 1 = participa
  trafico    = round(3000 + 5 * tamano_m2 +          # personas/mes
                       rnorm(n_tiendas, 0, 1500))
) %>%
  mutate(
    efecto_zona = case_when(zona == "Norte" ~ 40,
                            zona == "Sur"   ~ -30,
                            TRUE            ~ 0),
    ventas = round(120 + 3.2 * publicidad + 0.25 * tamano_m2 +
                     0.04 * trafico + 60 * promocion + efecto_zona +
                     rnorm(n_tiendas, 0, 50), 1)       # miles de $/mes
  ) %>%
  select(-efecto_zona)   # el modelo no debe ver la "respuesta secreta"

head(tiendas_sim, 4)

set.seed(123)
filas_ent <- sample(nrow(tiendas_sim), size = 0.8 * nrow(tiendas_sim))
tiendas_ent    <- tiendas_sim[filas_ent, ]    # 320 tiendas
tiendas_prueba <- tiendas_sim[-filas_ent, ]   # 80 tiendas
c(entrenamiento = nrow(tiendas_ent), prueba = nrow(tiendas_prueba))

modelo_lm <- lm(ventas ~ publicidad + tamano_m2 + trafico + promocion +
                  zona, data = tiendas_ent)
summary(modelo_lm)

metricas_regresion <- function(real, predicho) {
  error <- real - predicho
  tibble(
    MAE  = mean(abs(error)),
    RMSE = sqrt(mean(error^2)),
    MAPE = mean(abs(error) / real) * 100,
    R2   = 1 - sum(error^2) / sum((real - mean(real))^2)
  )
}

tiendas_prueba <- tiendas_prueba %>%
  mutate(pred_lm = predict(modelo_lm, newdata = tiendas_prueba))

metricas_regresion(tiendas_prueba$ventas, tiendas_prueba$pred_lm) %>%
  mutate(across(everything(), ~ round(.x, 3)))

prediccion_ingenua <- mean(tiendas_ent$ventas)   # siempre el promedio
metricas_regresion(tiendas_prueba$ventas,
                   rep(prediccion_ingenua, nrow(tiendas_prueba))) %>%
  mutate(across(everything(), ~ round(.x, 3)))

grafico_real_pred <- ggplot(tiendas_prueba,
                            aes(x = pred_lm, y = ventas)) +
  geom_abline(slope = 1, intercept = 0, color = color_gris,
              linewidth = 0.8, linetype = "dashed") +
  geom_point(color = color_principal, size = 2, alpha = 0.8) +
  scale_x_continuous(labels = scales::dollar_format(suffix = "k")) +
  scale_y_continuous(labels = scales::dollar_format(suffix = "k")) +
  labs(title = "Ventas reales contra ventas predichas",
       subtitle = "80 tiendas de prueba; diagonal = predicción perfecta",
       x = "Ventas predichas (miles)", y = "Ventas reales (miles)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
grafico_real_pred

candidatas <- tibble(
  tienda     = c("Local Plaza Norte", "Local Centro Histórico",
                 "Local Sur Express"),
  zona       = c("Norte", "Centro", "Sur"),
  tamano_m2  = c(900, 400, 250),
  publicidad = c(80, 50, 30),
  promocion  = c(1, 0, 1),
  trafico    = c(9000, 6500, 4000)
)

predict(modelo_lm, newdata = candidatas, interval = "prediction") %>%
  as_tibble() %>%
  mutate(tienda = candidatas$tienda, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x)))

set.seed(123)
modelo_rf_ventas <- randomForest(
  ventas ~ publicidad + tamano_m2 + trafico + promocion + zona,
  data = tiendas_ent %>% mutate(zona = factor(zona)),
  ntree = 300,          # número de árboles
  importance = TRUE     # calcular importancia de variables
)
modelo_rf_ventas

tiendas_prueba <- tiendas_prueba %>%
  mutate(pred_rf = predict(modelo_rf_ventas,
                           newdata = tiendas_prueba %>%
                             mutate(zona = factor(zona))))

bind_rows(
  metricas_regresion(tiendas_prueba$ventas, tiendas_prueba$pred_lm) %>%
    mutate(modelo = "Regresión lineal", .before = 1),
  metricas_regresion(tiendas_prueba$ventas, tiendas_prueba$pred_rf) %>%
    mutate(modelo = "Random forest", .before = 1)
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

set.seed(123)
n_clientes <- 2000
clientes_churn <- tibble(
  cliente_id       = sprintf("C%04d", 1:n_clientes),
  antiguedad_meses = sample(1:72, n_clientes, replace = TRUE),
  plan             = sample(c("Básico", "Estándar", "Premium"),
                            n_clientes, replace = TRUE,
                            prob = c(0.5, 0.3, 0.2)),
  uso_mensual      = rpois(n_clientes, 8),    # visitas/usos al mes
  quejas_6m        = rpois(n_clientes, 0.8),  # quejas últimos 6 meses
  pagos_tardios    = rpois(n_clientes, 0.4),  # pagos atrasados en el año
  genero           = sample(c("F", "M"), n_clientes, replace = TRUE),
  edad             = sample(18:70, n_clientes, replace = TRUE)
) %>%
  mutate(
    # "Verdad" oculta: puntaje de riesgo (el modelo no la verá)
    efecto_plan = case_when(plan == "Básico"   ~ 0,
                            plan == "Estándar" ~ -0.6,
                            TRUE               ~ -1.3),
    riesgo = 2.0 - 0.05 * antiguedad_meses +
      1.0 * (antiguedad_meses <= 6) + 1.0 * quejas_6m -
      0.35 * uso_mensual + 0.8 * pagos_tardios + efecto_plan,
    prob_real = plogis(riesgo),               # convierte a probabilidad
    churn = if_else(runif(n_clientes) < prob_real, "Si", "No"),
    churn = factor(churn, levels = c("No", "Si")),
    plan  = factor(plan, levels = c("Básico", "Estándar", "Premium")),
    cuota_mensual = case_when(plan == "Básico"   ~ 199,
                              plan == "Estándar" ~ 349,
                              TRUE               ~ 599)
  ) %>%
  select(-efecto_plan, -riesgo, -prob_real)

glimpse(clientes_churn)
clientes_churn %>% count(churn) %>% mutate(pct = round(100 * n / sum(n), 1))

clientes_churn %>%
  mutate(quejas = if_else(quejas_6m >= 3, "3 o más",
                          as.character(quejas_6m))) %>%
  group_by(quejas) %>%
  summarise(clientes = n(),
            tasa_churn = round(100 * mean(churn == "Si"), 1),
            .groups = "drop")

set.seed(123)
filas_ent <- createDataPartition(clientes_churn$churn, p = 0.7,
                                 list = FALSE)
churn_ent    <- clientes_churn[filas_ent, ]
churn_prueba <- clientes_churn[-filas_ent, ]

# Misma proporción de "Si" en ambos conjuntos
round(prop.table(table(churn_ent$churn)), 3)
round(prop.table(table(churn_prueba$churn)), 3)

formula_churn <- churn ~ antiguedad_meses + plan + uso_mensual +
  quejas_6m + pagos_tardios + genero + edad

modelo_log <- glm(formula_churn, data = churn_ent, family = binomial)
summary(modelo_log)

tibble(variable   = names(coef(modelo_log)),
       odds_ratio = round(exp(coef(modelo_log)), 3)) %>%
  mutate(efecto = case_when(
    odds_ratio > 1 ~ paste0("+", round(100 * (odds_ratio - 1)), "% riesgo"),
    TRUE           ~ paste0(round(100 * (odds_ratio - 1)), "% riesgo")
  )) %>%
  filter(variable != "(Intercept)")

churn_prueba <- churn_prueba %>%
  mutate(prob_log = predict(modelo_log, newdata = churn_prueba,
                            type = "response"),   # probabilidad de "Si"
         pred_log = factor(if_else(prob_log >= 0.5, "Si", "No"),
                           levels = c("No", "Si")))

churn_prueba %>%
  select(cliente_id, antiguedad_meses, quejas_6m, prob_log, pred_log,
         churn) %>%
  mutate(prob_log = round(prob_log, 3)) %>%
  head(5)

# Matriz de confusión: filas = predicción, columnas = realidad
matriz <- table(Prediccion = churn_prueba$pred_log,
                Real = churn_prueba$churn)
matriz

metricas_clasificacion <- function(prediccion, real, positivo = "Si") {
  vp <- sum(prediccion == positivo & real == positivo)
  vn <- sum(prediccion != positivo & real != positivo)
  fp <- sum(prediccion == positivo & real != positivo)
  fn <- sum(prediccion != positivo & real == positivo)
  precision    <- vp / (vp + fp)
  sensibilidad <- vp / (vp + fn)
  tibble(
    exactitud    = (vp + vn) / (vp + vn + fp + fn),
    precision    = precision,
    sensibilidad = sensibilidad,
    especificidad = vn / (vn + fp),
    F1           = 2 * precision * sensibilidad /
                   (precision + sensibilidad)
  )
}

metricas_clasificacion(churn_prueba$pred_log, churn_prueba$churn) %>%
  mutate(across(everything(), ~ round(.x, 3)))

curva_roc <- function(prob, real, positivo = "Si") {
  map_dfr(seq(0, 1, by = 0.01), function(u) {
    pred <- prob >= u
    tibble(umbral = u,
           sensibilidad = sum(pred & real == positivo) /
                          sum(real == positivo),
           tasa_fp = sum(pred & real != positivo) / sum(real != positivo))
  })
}

# AUC como probabilidad de ordenar bien un par (positivo, negativo)
calcular_auc <- function(prob, real, positivo = "Si") {
  p_pos <- prob[real == positivo]
  p_neg <- prob[real != positivo]
  mean(outer(p_pos, p_neg, ">") + 0.5 * outer(p_pos, p_neg, "=="))
}

roc_log <- curva_roc(churn_prueba$prob_log, churn_prueba$churn)
auc_log <- calcular_auc(churn_prueba$prob_log, churn_prueba$churn)
round(auc_log, 3)
# round() evita problemas de precisión al comparar decimales
roc_log %>% filter(round(umbral, 2) %in% c(0.1, 0.2, 0.3, 0.5, 0.7))

grafico_roc <- ggplot(roc_log, aes(x = tasa_fp, y = sensibilidad)) +
  geom_abline(slope = 1, intercept = 0, color = color_gris,
              linetype = "dashed", linewidth = 0.8) +
  geom_path(color = color_principal, linewidth = 0.8) +
  geom_point(data = filter(roc_log, umbral %in% c(0.2, 0.5)),
             color = color_resalte, size = 3) +
  geom_text(data = filter(roc_log, umbral %in% c(0.2, 0.5)),
            aes(label = paste("umbral", umbral)), color = color_resalte,
            hjust = -0.2, vjust = 1.2, size = 3.5) +
  annotate("text", x = 0.75, y = 0.25,
           label = paste("AUC =", round(auc_log, 3)), size = 4.5) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  coord_equal() +
  labs(title = "Curva ROC de la regresión logística",
       subtitle = "Clientes de prueba; la diagonal equivale a adivinar",
       x = "Tasa de falsos positivos (1 - especificidad)",
       y = "Sensibilidad") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
grafico_roc

costo_vp <- 300 + 0.5 * 3000   # descuento + clientes que igual se van
costo_fp <- 300                # descuento desperdiciado
costo_fn <- 3000               # cliente perdido sin intentar retenerlo

costos_umbral <- map_dfr(seq(0.05, 0.95, by = 0.05), function(u) {
  pred <- churn_prueba$prob_log >= u
  real <- churn_prueba$churn == "Si"
  tibble(umbral = u,
         llamadas = sum(pred),
         vp = sum(pred & real), fp = sum(pred & !real),
         fn = sum(!pred & real),
         costo_total = vp * costo_vp + fp * costo_fp + fn * costo_fn)
})

costos_umbral %>% arrange(costo_total) %>% head(3)
costos_umbral %>% filter(umbral %in% c(0.5, 0.95))

grafico_costos <- ggplot(costos_umbral, aes(umbral, costo_total)) +
  geom_line(color = color_principal, linewidth = 0.8) +
  geom_point(color = color_principal, size = 1.5) +
  geom_point(data = slice_min(costos_umbral, costo_total, n = 1),
             color = color_resalte, size = 4) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Costo total de la campaña según el umbral",
       subtitle = "Clientes de prueba; en naranja, el umbral óptimo",
       x = "Umbral de probabilidad para llamar al cliente",
       y = "Costo total") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
grafico_costos

set.seed(123)
modelo_arbol <- rpart(formula_churn, data = churn_ent, method = "class",
                      control = rpart.control(cp = 0.02))
modelo_arbol

round(modelo_arbol$variable.importance, 1)

churn_prueba <- churn_prueba %>%
  mutate(prob_arbol = predict(modelo_arbol, newdata = churn_prueba,
                              type = "prob")[, "Si"])
round(calcular_auc(churn_prueba$prob_arbol, churn_prueba$churn), 3)

set.seed(123)
modelo_rf <- randomForest(formula_churn, data = churn_ent,
                          ntree = 300, importance = TRUE)
modelo_rf

importancia_rf <- importance(modelo_rf) %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  as_tibble() %>%
  select(variable, MeanDecreaseAccuracy) %>%
  arrange(desc(MeanDecreaseAccuracy))
importancia_rf %>% mutate(MeanDecreaseAccuracy =
                            round(MeanDecreaseAccuracy, 1))

grafico_importancia <- ggplot(importancia_rf,
       aes(x = MeanDecreaseAccuracy,
           y = reorder(variable, MeanDecreaseAccuracy))) +
  geom_col(fill = color_principal, width = 0.7) +
  labs(title = "¿Qué variables usa el bosque para predecir el abandono?",
       subtitle = "Pérdida de exactitud al desordenar cada variable",
       x = "Importancia (disminución media de exactitud)", y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
grafico_importancia

# caret toma como clase positiva el PRIMER nivel del factor
churn_ent_caret <- churn_ent %>%
  mutate(churn = factor(churn, levels = c("Si", "No")))

control_cv <- trainControl(
  method = "cv", number = 5,           # validación cruzada de 5 pliegues
  classProbs = TRUE,                   # calcular probabilidades
  summaryFunction = twoClassSummary    # reportar ROC, Sens y Spec
)

set.seed(123)
cv_log <- train(formula_churn, data = churn_ent_caret, method = "glm",
                family = binomial, trControl = control_cv, metric = "ROC")
set.seed(123)
cv_arbol <- train(formula_churn, data = churn_ent_caret,
                  method = "rpart", trControl = control_cv,
                  metric = "ROC", tuneLength = 5)   # prueba 5 valores de cp
set.seed(123)
cv_rf <- train(formula_churn, data = churn_ent_caret, method = "rf",
               trControl = control_cv, metric = "ROC", ntree = 200,
               tuneGrid = data.frame(mtry = 2:4))   # variables por corte

cv_rf

comparacion <- resamples(list(Logistica = cv_log, Arbol = cv_arbol,
                              RandomForest = cv_rf))

summary(comparacion)$statistics$ROC[, c("Min.", "Mean", "Max.")] %>%
  round(3)

churn_prueba <- churn_prueba %>%
  mutate(prob_rf = predict(modelo_rf, newdata = churn_prueba,
                           type = "prob")[, "Si"])

tibble(
  modelo = c("Regresión logística", "Árbol de decisión",
             "Random forest"),
  auc_prueba = c(calcular_auc(churn_prueba$prob_log, churn_prueba$churn),
                 calcular_auc(churn_prueba$prob_arbol, churn_prueba$churn),
                 calcular_auc(churn_prueba$prob_rf, churn_prueba$churn))
) %>%
  mutate(auc_prueba = round(auc_prueba, 3)) %>%
  arrange(desc(auc_prueba))

lista_retencion <- churn_prueba %>%
  mutate(valor_anual     = cuota_mensual * 12,
         perdida_esperada = prob_log * valor_anual) %>%
  arrange(desc(perdida_esperada)) %>%
  mutate(prioridad = row_number()) %>%
  select(prioridad, cliente_id, plan, quejas_6m, prob_log,
         perdida_esperada)

lista_retencion %>%
  mutate(prob_log = round(prob_log, 2),
         perdida_esperada = round(perdida_esperada)) %>%
  head(8)

set.seed(123)
meses <- seq(as.Date("2021-01-01"), as.Date("2025-12-01"), by = "month")
n_meses <- length(meses)   # 60 meses

# Factor estacional de cada mes (1 = mes normal)
factor_mes <- c(0.88, 0.85, 0.95, 0.97, 1.04, 0.96,   # ene-jun
                0.97, 1.00, 0.93, 0.98, 1.15, 1.32)   # jul-dic
factor_mes <- factor_mes / mean(factor_mes)            # promedio = 1

ventas_mensuales <- tibble(
  mes = meses,
  tendencia = 1500 + 15 * (1:n_meses),
  unidades = round(tendencia * factor_mes[month(meses)] *
                     (1 + rnorm(n_meses, 0, 0.03)))    # ruido de 3 %
) %>%
  select(-tendencia)

# Objeto ts: la estructura de series de tiempo de R
serie <- ts(ventas_mensuales$unidades, start = c(2021, 1),
            frequency = 12)   # 12 = datos mensuales
serie

grafico_serie <- autoplot(serie, color = color_principal,
                          linewidth = 0.8) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Unidades vendidas por mes, electrónica",
       subtitle = "2021-2025 (datos simulados)",
       x = NULL, y = "Unidades") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
grafico_serie

descomposicion <- decompose(serie, type = "multiplicative")

# Índices estacionales: cuánto se aleja cada mes de un mes "normal"
nombres_mes <- c("ene", "feb", "mar", "abr", "may", "jun",
                 "jul", "ago", "sep", "oct", "nov", "dic")
tibble(mes = nombres_mes, indice = round(descomposicion$figure, 3)) %>%
  arrange(desc(indice)) %>%
  head(4)

grafico_descomp <- autoplot(descomposicion) +
  labs(title = "Descomposición multiplicativa de la serie",
       x = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
grafico_descomp

serie_ent    <- window(serie, end = c(2024, 12))   # 2021-2024: 48 meses
serie_prueba <- window(serie, start = c(2025, 1))  # 2025: 12 meses
c(length(serie_ent), length(serie_prueba))

h <- length(serie_prueba)   # horizonte: 12 meses

pron_naive  <- naive(serie_ent, h = h)
pron_snaive <- snaive(serie_ent, h = h)
modelo_ets  <- ets(serie_ent)
pron_ets    <- forecast(modelo_ets, h = h)
modelo_arima <- auto.arima(serie_ent)
pron_arima  <- forecast(modelo_arima, h = h)

modelo_ets     # ¿qué combinación eligió ets()?
modelo_arima

accuracy(pron_ets, serie_prueba)   # salida completa de un modelo

comparacion_ts <- list(Ingenuo = pron_naive,
                       `Ingenuo estacional` = pron_snaive,
                       ETS = pron_ets, ARIMA = pron_arima) %>%
  map_dfr(function(p) {
    acc <- accuracy(p, serie_prueba)["Test set", ]
    tibble(MAE = acc["MAE"], RMSE = acc["RMSE"], MAPE = acc["MAPE"])
  }, .id = "modelo") %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  arrange(MAPE)
comparacion_ts

grafico_comparacion <- autoplot(window(serie, start = c(2023, 1)),
                                color = "black", linewidth = 0.6) +
  autolayer(pron_snaive, series = "Ingenuo estacional", PI = FALSE) +
  autolayer(pron_ets, series = "ETS", PI = FALSE) +
  autolayer(pron_arima, series = "ARIMA", PI = FALSE) +
  scale_color_manual(values = paleta_libro[c(2, 1, 3)]) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Pronósticos de 2025 contra la realidad",
       subtitle = "Negro: ventas reales; entrenamiento hasta dic-2024",
       x = NULL, y = "Unidades", color = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), legend.position = "top")
grafico_comparacion

modelo_final <- ets(serie)
pronostico_2026 <- forecast(modelo_final, h = 12, level = c(80, 95))
pronostico_2026

grafico_pronostico <- autoplot(pronostico_2026, color = color_principal,
                               fcol = color_resalte, flwd = 0.8) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Pronóstico de unidades para 2026",
       subtitle = "Modelo ETS; bandas de 80 % y 95 %",
       x = NULL, y = "Unidades") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
grafico_pronostico

tabla_direccion <- tibble(
  mes         = format(seq(as.Date("2026-01-01"), by = "month",
                           length.out = 12), "%b %Y"),
  pesimista   = round(pronostico_2026$lower[, "80%"]),
  esperado    = round(pronostico_2026$mean),
  optimista   = round(pronostico_2026$upper[, "80%"])
)
tabla_direccion %>% slice(c(1, 6, 11, 12))
sum(tabla_direccion$esperado)   # unidades esperadas en todo 2026

nivel_servicio <- 0.95
z <- qnorm(nivel_servicio)                 # 1.645
error_tipico <- comparacion_ts %>%
  filter(modelo == "ETS") %>% pull(RMSE)   # error típico en 2025
tiempo_entrega <- 2                        # meses de anticipación

stock_seguridad <- z * error_tipico * sqrt(tiempo_entrega)

tabla_direccion %>%
  slice(11:12) %>%
  mutate(stock_seguridad = round(stock_seguridad),
         pedido_sugerido = esperado + stock_seguridad) %>%
  select(mes, esperado, stock_seguridad, pedido_sugerido)

canastas_curso <- transacciones %>%
  group_by(cliente_id, fecha) %>%
  summarise(productos = n_distinct(producto_id), .groups = "drop")

canastas_curso %>% count(productos, name = "canastas")

set.seed(123)
n_tickets <- 3000
tickets_ancho <- tibble(
  ticket_id = 1:n_tickets,
  Laptop    = rbinom(n_tickets, 1, 0.08),   # 1 = el ticket lo incluye
  Monitor   = rbinom(n_tickets, 1, 0.07),
  Impresora = rbinom(n_tickets, 1, 0.05),
  Audifonos = rbinom(n_tickets, 1, 0.15),
  USB       = rbinom(n_tickets, 1, 0.18),
  Papel     = rbinom(n_tickets, 1, 0.20),
  Teclado   = rbinom(n_tickets, 1, 0.08)
) %>%
  mutate(
    # Asociaciones "reales" que el análisis debe descubrir
    Mouse = if_else(Laptop == 1, rbinom(n(), 1, 0.60),
                    rbinom(n(), 1, 0.08)),
    Funda = if_else(Laptop == 1, rbinom(n(), 1, 0.40),
                    rbinom(n(), 1, 0.02)),
    `Cable HDMI` = if_else(Monitor == 1, rbinom(n(), 1, 0.50),
                           rbinom(n(), 1, 0.04)),
    Tinta = if_else(Impresora == 1, rbinom(n(), 1, 0.70),
                    rbinom(n(), 1, 0.06))
  )

# Formato largo: una fila por ticket y producto (como en un sistema real)
tickets <- tickets_ancho %>%
  pivot_longer(-ticket_id, names_to = "producto", values_to = "compro") %>%
  filter(compro == 1) %>%
  select(-compro)

head(tickets, 5)
n_total <- n_distinct(tickets$ticket_id)   # tickets con al menos 1 producto
n_total

soporte_producto <- tickets %>%
  count(producto, name = "tickets_x") %>%
  mutate(soporte_x = tickets_x / n_total) %>%
  arrange(desc(soporte_x))
soporte_producto %>% mutate(soporte_x = round(soporte_x, 3))

reglas <- tickets %>%
  inner_join(tickets, by = "ticket_id", suffix = c("_x", "_y"),
             relationship = "many-to-many") %>%
  filter(producto_x != producto_y) %>%          # sin pares consigo mismo
  count(producto_x, producto_y, name = "tickets_xy") %>%
  left_join(soporte_producto, by = c("producto_x" = "producto")) %>%
  left_join(soporte_producto %>%
              select(producto_y = producto, soporte_y = soporte_x),
            by = "producto_y") %>%
  mutate(soporte   = tickets_xy / n_total,
         confianza = tickets_xy / tickets_x,
         lift      = confianza / soporte_y)

mejores_reglas <- reglas %>%
  filter(soporte >= 0.02) %>%                   # soporte mínimo: 2 %
  arrange(desc(lift)) %>%
  transmute(regla = paste(producto_x, "->", producto_y),
            tickets_xy, soporte = round(soporte, 3),
            confianza = round(confianza, 3), lift = round(lift, 2))
mejores_reglas %>% head(10)

grafico_lift <- mejores_reglas %>%
  filter(lift > 1.5) %>%
  ggplot(aes(x = lift, y = reorder(regla, lift))) +
  geom_col(fill = color_principal, width = 0.7) +
  geom_vline(xintercept = 1, color = color_gris, linetype = "dashed") +
  labs(title = "Reglas de asociación con mayor lift",
       subtitle = "Soporte mínimo 2 %; la línea punteada es lift = 1",
       x = "Lift (veces más probable que al azar)", y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
grafico_lift

mejores_reglas %>% arrange(lift) %>% head(3)

dir.create("resultados", showWarnings = FALSE)

saveRDS(modelo_log, "resultados/modelo_churn.rds")

# ... días después, en otro script o en otra computadora:
modelo_cargado <- readRDS("resultados/modelo_churn.rds")

# Comprobamos que predice exactamente lo mismo
all.equal(predict(modelo_cargado, churn_prueba, type = "response"),
          predict(modelo_log, churn_prueba, type = "response"))

predecir_churn <- function(nuevos_datos, modelo, umbral = 0.15) {
  # 1. Validar que vengan todas las columnas que el modelo necesita
  necesarias <- c("antiguedad_meses", "plan", "uso_mensual", "quejas_6m",
                  "pagos_tardios", "genero", "edad")
  faltan <- setdiff(necesarias, names(nuevos_datos))
  if (length(faltan) > 0) {
    stop("Faltan columnas: ", paste(faltan, collapse = ", "))
  }
  # 2. Mismos niveles de factor que en el entrenamiento
  nuevos_datos <- nuevos_datos %>%
    mutate(plan = factor(plan, levels = c("Básico", "Estándar",
                                          "Premium")))
  # 3. Probabilidad y acción sugerida
  nuevos_datos %>%
    mutate(
      prob_churn = predict(modelo, newdata = nuevos_datos,
                           type = "response"),
      nivel_riesgo = cut(prob_churn, breaks = c(0, umbral, 0.5, 1),
                         labels = c("Bajo", "Medio", "Alto"),
                         include.lowest = TRUE),
      accion = if_else(prob_churn >= umbral, "Llamar", "Sin acción")
    )
}

set.seed(123)
lote_semana <- tibble(
  cliente_id       = sprintf("N%04d", 1:500),
  antiguedad_meses = sample(1:72, 500, replace = TRUE),
  plan             = sample(c("Básico", "Estándar", "Premium"), 500,
                            replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  uso_mensual      = rpois(500, 8),
  quejas_6m        = rpois(500, 0.8),
  pagos_tardios    = rpois(500, 0.4),
  genero           = sample(c("F", "M"), 500, replace = TRUE),
  edad             = sample(18:70, 500, replace = TRUE)
)

puntuados <- predecir_churn(lote_semana, modelo_cargado)
puntuados %>% count(nivel_riesgo, accion)

# Exportar la lista para el equipo de retención
puntuados %>%
  filter(accion == "Llamar") %>%
  arrange(desc(prob_churn)) %>%
  mutate(prob_churn = round(prob_churn, 3)) %>%
  select(cliente_id, plan, quejas_6m, prob_churn, nivel_riesgo) %>%
  write_csv("resultados/clientes_a_llamar.csv")

file.exists("resultados/clientes_a_llamar.csv")

comparar_distribucion <- function(entrenamiento, nuevo, variables) {
  map_dfr(variables, function(v) {
    tibble(variable = v,
           media_entrenamiento = mean(entrenamiento[[v]]),
           media_nuevo = mean(nuevo[[v]]))
  }) %>%
    mutate(cambio_pct = round(100 * (media_nuevo - media_entrenamiento) /
                                media_entrenamiento, 1),
           alerta = if_else(abs(cambio_pct) > 20, "REVISAR", "ok"))
}

comparar_distribucion(churn_ent, lote_semana,
                      c("antiguedad_meses", "uso_mensual", "quejas_6m",
                        "pagos_tardios")) %>%
  mutate(across(starts_with("media"), ~ round(.x, 2)))

puntuados %>%
  group_by(genero) %>%
  summarise(clientes = n(),
            prob_media = round(mean(prob_churn), 3),
            pct_llamar = round(100 * mean(accion == "Llamar"), 1),
            .groups = "drop")

set.seed(123)
churn_fuga <- clientes_churn %>%
  mutate(llamo_cancelaciones = if_else(
    churn == "Si", rbinom(n(), 1, 0.85),   # casi todos los que se van
    rbinom(n(), 1, 0.03)))                 # muy pocos de los que se quedan

fuga_ent    <- churn_fuga[filas_ent, ]    # misma división que antes
fuga_prueba <- churn_fuga[-filas_ent, ]

modelo_fuga <- glm(update(formula_churn, . ~ . + llamo_cancelaciones),
                   data = fuga_ent, family = binomial)
prob_fuga <- predict(modelo_fuga, fuga_prueba, type = "response")
round(calcular_auc(prob_fuga, fuga_prueba$churn), 3)

prob_rf_ent <- predict(modelo_rf, newdata = churn_ent,
                       type = "prob")[, "Si"]
c(auc_entrenamiento = calcular_auc(prob_rf_ent, churn_ent$churn),
  auc_prueba = calcular_auc(churn_prueba$prob_rf, churn_prueba$churn)) %>%
  round(3)

set.seed(123)
fraude <- tibble(
  es_fraude = factor(if_else(runif(2000) < 0.05, "Si", "No"),
                     levels = c("No", "Si"))
)
prediccion_perezosa <- factor(rep("No", 2000), levels = c("No", "Si"))

metricas_clasificacion(prediccion_perezosa, fraude$es_fraude) %>%
  mutate(across(everything(), ~ round(.x, 3)))

set.seed(123)
kmeans_sin_escalar <- kmeans(rfm %>% select(recencia, frecuencia, monto),
                             centers = 4, nstart = 25)

rfm %>%
  mutate(grupo = kmeans_sin_escalar$cluster) %>%
  group_by(grupo) %>%
  summarise(clientes = n(), recencia = round(mean(recencia)),
            frecuencia = round(mean(frecuencia), 1),
            monto = round(mean(monto)), .groups = "drop") %>%
  arrange(monto)

# ---- Guardar figuras -------------------------------------------------------
guardar_figura(grafico_sobreajuste, "m07-sobreajuste")
guardar_figura(grafico_codo, "m07-codo")
guardar_figura(grafico_silueta, "m07-silueta")
guardar_figura(grafico_clusters, "m07-clusters")
guardar_figura(grafico_segmentos, "m07-segmentos")
guardar_figura(grafico_real_pred, "m07-real-vs-predicho")
guardar_figura(grafico_roc, "m07-roc", alto = 4.2)
guardar_figura(grafico_costos, "m07-costo-umbral")
guardar_figura(grafico_importancia, "m07-importancia")
guardar_figura(grafico_serie, "m07-serie")
guardar_figura(grafico_descomp, "m07-descomposicion", alto = 5)
guardar_figura(grafico_comparacion, "m07-comparacion-pronosticos")
guardar_figura(grafico_pronostico, "m07-pronostico")
guardar_figura(grafico_lift, "m07-lift")
setwd(directorio_original)
cat("Figuras del Módulo 7 generadas en", dir_figuras, "\n")
