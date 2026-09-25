# ============================================================================
# MÓDULO 7 - LECCIÓN 2: CLUSTERING - SEGMENTACIÓN DE CLIENTES
# ============================================================================
# Objetivo: Segmentar clientes automáticamente usando K-means
# ============================================================================

# CASO DE NEGOCIO:
# Tienes 200 clientes y quieres agruparlos en segmentos para:
# - Personalizar campañas de marketing
# - Ofrecer productos específicos a cada grupo
# - Optimizar recursos de atención al cliente

# ============================================================================
# 1. INSTALACIÓN Y CARGA DE PAQUETES
# ============================================================================

# install.packages("cluster")
# install.packages("factoextra")
# install.packages("ggplot2")

library(cluster)      # Para clustering
library(factoextra)   # Para visualizar clusters
library(ggplot2)      # Para gráficos
library(dplyr)        # Para manipulación de datos

# ============================================================================
# 2. GENERAR DATOS DE CLIENTES
# ============================================================================

set.seed(123)  # Para reproducibilidad

# Crear dataset de clientes con diferentes características
clientes <- data.frame(
  cliente_id = 1:200,

  # Comportamiento de compra
  frecuencia_compra = c(
    rnorm(50, mean = 2, sd = 1),    # Clientes poco frecuentes
    rnorm(75, mean = 8, sd = 2),    # Clientes regulares
    rnorm(75, mean = 20, sd = 3)    # Clientes muy frecuentes
  ),

  # Monto promedio de compra
  ticket_promedio = c(
    rnorm(50, mean = 500, sd = 100),   # Compras pequeñas
    rnorm(75, mean = 1500, sd = 300),  # Compras medianas
    rnorm(75, mean = 3500, sd = 500)   # Compras grandes
  ),

  # Antigüedad en meses
  antiguedad_meses = c(
    runif(50, 1, 12),      # Nuevos
    runif(75, 12, 36),     # Establecidos
    runif(75, 36, 60)      # Leales
  ),

  # Satisfacción (1-10)
  satisfaccion = c(
    rnorm(50, mean = 6, sd = 1.5),
    rnorm(75, mean = 7.5, sd = 1),
    rnorm(75, mean = 9, sd = 0.8)
  )
)

# Asegurar valores positivos
clientes$frecuencia_compra <- pmax(1, round(clientes$frecuencia_compra))
clientes$ticket_promedio <- pmax(100, round(clientes$ticket_promedio, 2))
clientes$antiguedad_meses <- round(clientes$antiguedad_meses, 0)
clientes$satisfaccion <- pmin(10, pmax(1, round(clientes$satisfaccion, 1)))

# Calcular valor total del cliente (CLV simplificado)
clientes <- clientes %>%
  mutate(valor_total = frecuencia_compra * ticket_promedio)

# Ver primeros registros
head(clientes, 10)

# Resumen estadístico
summary(clientes)

# ============================================================================
# 3. ANÁLISIS EXPLORATORIO
# ============================================================================

# Distribución de frecuencia de compra
ggplot(clientes, aes(x = frecuencia_compra)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribución de Frecuencia de Compra",
       x = "Número de Compras", y = "Cantidad de Clientes") +
  theme_minimal()

# Relación entre frecuencia y ticket promedio
ggplot(clientes, aes(x = frecuencia_compra, y = ticket_promedio)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  labs(title = "Frecuencia vs Ticket Promedio",
       x = "Frecuencia de Compra", y = "Ticket Promedio ($)") +
  theme_minimal()

# Correlación entre variables
cor(clientes[, c("frecuencia_compra", "ticket_promedio",
                 "antiguedad_meses", "satisfaccion")])

# ============================================================================
# 4. PREPARACIÓN DE DATOS PARA CLUSTERING
# ============================================================================

# Seleccionar solo las variables numéricas relevantes
datos_cluster <- clientes %>%
  select(frecuencia_compra, ticket_promedio, antiguedad_meses, satisfaccion)

# Ver primeros registros
head(datos_cluster)

# IMPORTANTE: ESCALAR LOS DATOS
# Las variables están en diferentes escalas:
# - frecuencia_compra: 1-30
# - ticket_promedio: 100-5000
# - antiguedad_meses: 1-60
# - satisfaccion: 1-10

# Sin escalar, ticket_promedio dominaría el clustering
# Escalamos para que todas las variables tengan media=0 y sd=1

datos_escalados <- scale(datos_cluster)

# Ver cómo cambiaron los datos
head(datos_escalados)
summary(datos_escalados)

# ============================================================================
# 5. DETERMINAR EL NÚMERO ÓPTIMO DE CLUSTERS
# ============================================================================

# Método 1: Elbow Method (Método del Codo)
# Probamos diferentes números de clusters y vemos dónde se "quiebra" la curva

set.seed(123)

# Calcular WSS (Within-cluster Sum of Squares) para k = 1 a 10
wss <- sapply(1:10, function(k) {
  kmeans(datos_escalados, centers = k, nstart = 25)$tot.withinss
})

# Graficar
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Número de Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Método del Codo")

# El "codo" está alrededor de k=3 o k=4

# Método 2: Silhouette Method
# Mide qué tan bien está cada punto en su cluster

fviz_nbclust(datos_escalados, kmeans, method = "silhouette") +
  labs(title = "Número Óptimo de Clusters - Método Silhouette")

# Método 3: Gap Statistic
set.seed(123)
gap_stat <- clusGap(datos_escalados, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat) +
  labs(title = "Número Óptimo de Clusters - Gap Statistic")

# Conclusión: Los 3 métodos sugieren 3 o 4 clusters
# Vamos a usar k=3 para este ejemplo

# ============================================================================
# 6. APLICAR K-MEANS CLUSTERING
# ============================================================================

set.seed(123)

# Crear el modelo de clustering con k=3
kmeans_resultado <- kmeans(
  datos_escalados,
  centers = 3,      # Número de clusters
  nstart = 25,      # Número de inicializaciones aleatorias
  iter.max = 100    # Máximo de iteraciones
)

# Ver resultados
print(kmeans_resultado)

# Tamaño de cada cluster
kmeans_resultado$size

# Centros de los clusters (en escala estandarizada)
kmeans_resultado$centers

# ============================================================================
# 7. AGREGAR CLUSTERS AL DATASET ORIGINAL
# ============================================================================

# Agregar la asignación de cluster a cada cliente
clientes$cluster <- as.factor(kmeans_resultado$cluster)

# Ver distribución
table(clientes$cluster)

# Ver primeros clientes con su cluster
head(clientes, 10)

# ============================================================================
# 8. VISUALIZAR LOS CLUSTERS
# ============================================================================

# Visualización 2D usando PCA (Principal Component Analysis)
fviz_cluster(kmeans_resultado, data = datos_escalados,
             palette = c("red", "blue", "green"),
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_minimal(),
             main = "Segmentación de Clientes - K-means Clustering")

# Gráfico de dispersión: Frecuencia vs Ticket Promedio
ggplot(clientes, aes(x = frecuencia_compra, y = ticket_promedio, color = cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("red", "blue", "green")) +
  labs(title = "Segmentación de Clientes",
       subtitle = "Frecuencia de Compra vs Ticket Promedio",
       x = "Frecuencia de Compra",
       y = "Ticket Promedio ($)",
       color = "Cluster") +
  theme_minimal()

# ============================================================================
# 9. PERFILAR CADA CLUSTER (ANÁLISIS DE NEGOCIO)
# ============================================================================

# Calcular promedios por cluster
perfil_clusters <- clientes %>%
  group_by(cluster) %>%
  summarise(
    num_clientes = n(),
    frecuencia_promedio = round(mean(frecuencia_compra), 1),
    ticket_promedio = round(mean(ticket_promedio), 2),
    antiguedad_promedio = round(mean(antiguedad_meses), 1),
    satisfaccion_promedio = round(mean(satisfaccion), 1),
    valor_total_promedio = round(mean(valor_total), 2)
  ) %>%
  arrange(desc(valor_total_promedio))

print("PERFIL DE CADA CLUSTER:")
print(perfil_clusters)

# Visualizar perfiles
library(tidyr)
perfil_largo <- perfil_clusters %>%
  select(-num_clientes) %>%
  pivot_longer(cols = -cluster, names_to = "metrica", values_to = "valor")

ggplot(perfil_largo, aes(x = cluster, y = valor, fill = cluster)) +
  geom_col() +
  facet_wrap(~metrica, scales = "free_y") +
  scale_fill_manual(values = c("red", "blue", "green")) +
  labs(title = "Perfil de Clusters por Métrica") +
  theme_minimal() +
  theme(legend.position = "none")

# ============================================================================
# 10. INTERPRETAR Y NOMBRAR LOS CLUSTERS
# ============================================================================

# Basándonos en los perfiles, podemos nombrar los clusters:

# OJO: k-means numera los clusters al azar (1, 2, 3 pueden cambiar de una
# ejecución a otra). Por eso NO asignamos nombres por número de cluster:
# ordenamos los clusters por su valor total promedio y nombramos según ese
# orden (el de menor valor = Básicos, el de mayor valor = Premium).
nombres_segmento <- perfil_clusters %>%
  arrange(valor_total_promedio) %>%
  mutate(segmento = c("Clientes Básicos", "Clientes Estándar",
                      "Clientes Premium")) %>%
  select(cluster, segmento)

clientes <- clientes %>%
  left_join(nombres_segmento, by = "cluster")

# Ver distribución de segmentos
table(clientes$segmento)

# ============================================================================
# 11. RECOMENDACIONES DE NEGOCIO POR SEGMENTO
# ============================================================================

# Crear resumen con recomendaciones
cat("\n")
cat("===============================================\n")
cat("SEGMENTACIÓN DE CLIENTES - INSIGHTS\n")
cat("===============================================\n\n")

for (i in 1:nrow(perfil_clusters)) {
  cluster_num <- perfil_clusters$cluster[i]
  segmento_clientes <- clientes %>% filter(cluster == cluster_num)
  nombre_segmento <- unique(segmento_clientes$segmento)

  cat("CLUSTER", cluster_num, ":", nombre_segmento, "\n")
  cat("- Clientes:", perfil_clusters$num_clientes[i], "\n")
  cat("- Frecuencia promedio:", perfil_clusters$frecuencia_promedio[i], "compras\n")
  cat("- Ticket promedio: $", perfil_clusters$ticket_promedio[i], "\n")
  cat("- Antigüedad promedio:", perfil_clusters$antiguedad_promedio[i], "meses\n")
  cat("- Satisfacción promedio:", perfil_clusters$satisfaccion_promedio[i], "/10\n")
  cat("- Valor total promedio: $", perfil_clusters$valor_total_promedio[i], "\n")
  cat("\n")

  # Recomendaciones personalizadas
  if (perfil_clusters$valor_total_promedio[i] == max(perfil_clusters$valor_total_promedio)) {
    cat("ESTRATEGIA: Retención y VIP\n")
    cat("  - Programa de lealtad premium\n")
    cat("  - Atención personalizada\n")
    cat("  - Acceso anticipado a nuevos productos\n")
  } else if (perfil_clusters$valor_total_promedio[i] == min(perfil_clusters$valor_total_promedio)) {
    cat("ESTRATEGIA: Activación y Crecimiento\n")
    cat("  - Campañas de reactivación\n")
    cat("  - Descuentos especiales\n")
    cat("  - Onboarding mejorado\n")
  } else {
    cat("ESTRATEGIA: Up-selling y Cross-selling\n")
    cat("  - Promociones por volumen\n")
    cat("  - Productos complementarios\n")
    cat("  - Programa de puntos\n")
  }
  cat("\n-----------------------------------------------\n\n")
}

# ============================================================================
# 12. VALIDAR LA CALIDAD DEL CLUSTERING
# ============================================================================

# Calcular el Silhouette Score
# Valores cercanos a 1 = buen clustering
# Valores cercanos a 0 = clusters superpuestos
# Valores negativos = mal asignados

silhouette_score <- silhouette(kmeans_resultado$cluster, dist(datos_escalados))
fviz_silhouette(silhouette_score) +
  labs(title = "Análisis de Silhouette - Calidad del Clustering")

# Promedio de Silhouette
mean(silhouette_score[, 3])

# ============================================================================
# 13. EXPORTAR RESULTADOS
# ============================================================================

# Guardar clientes con su segmento asignado
write.csv(clientes, "clientes_segmentados.csv", row.names = FALSE)

# Guardar perfil de clusters
write.csv(perfil_clusters, "perfil_clusters.csv", row.names = FALSE)

cat("✓ Resultados exportados:\n")
cat("  - clientes_segmentados.csv\n")
cat("  - perfil_clusters.csv\n")

# ============================================================================
# EJERCICIO PRÁCTICO 1
# ============================================================================

# Ejecuta el clustering con k=4 en lugar de k=3
# Compara los resultados:
# - ¿Los clusters tienen sentido de negocio?
# - ¿Cuál valor de k es mejor para este caso?

# TU CÓDIGO AQUÍ:




# ============================================================================
# EJERCICIO PRÁCTICO 2
# ============================================================================

# Agrega una nueva variable al análisis: "tasa_devolucion"
# Simula tasas de devolución entre 0% y 30%
# Re-ejecuta el clustering y analiza cómo cambian los segmentos

# TU CÓDIGO AQUÍ:




# ============================================================================
# EJERCICIO PRÁCTICO 3: APLICACIÓN REAL
# ============================================================================

# Crea una función que clasifique nuevos clientes
# Dado un nuevo cliente, asignarlo al cluster más cercano

clasificar_nuevo_cliente <- function(nuevo_cliente, modelo_kmeans, datos_escalados) {
  # Escalar el nuevo cliente usando los mismos parámetros
  centro_original <- attr(datos_escalados, "scaled:center")
  escala_original <- attr(datos_escalados, "scaled:scale")

  nuevo_escalado <- scale(nuevo_cliente,
                          center = centro_original,
                          scale = escala_original)

  # Encontrar el cluster más cercano
  centros <- modelo_kmeans$centers
  distancias <- apply(centros, 1, function(centro) {
    sqrt(sum((nuevo_escalado - centro)^2))
  })

  cluster_asignado <- which.min(distancias)
  return(cluster_asignado)
}

# Probar con un cliente nuevo
cliente_nuevo <- data.frame(
  frecuencia_compra = 15,
  ticket_promedio = 2000,
  antiguedad_meses = 24,
  satisfaccion = 8.5
)

cluster_nuevo <- clasificar_nuevo_cliente(cliente_nuevo, kmeans_resultado, datos_escalados)
cat("El nuevo cliente pertenece al Cluster:", cluster_nuevo, "\n")

# ============================================================================
# ¡EXCELENTE TRABAJO!
# ============================================================================
# Ahora dominas:
# ✓ Preparar datos para clustering
# ✓ Escalar variables
# ✓ Determinar el número óptimo de clusters
# ✓ Aplicar K-means
# ✓ Visualizar y validar resultados
# ✓ Perfilar clusters para insights de negocio
# ✓ Crear estrategias específicas por segmento
# ✓ Clasificar nuevos clientes
#
# Continúa con: 03_regresion_lineal.R
# ============================================================================
