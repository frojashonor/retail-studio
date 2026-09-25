# Módulo 7: Machine Learning para Business Intelligence 🤖

## Objetivo
Aplicar técnicas de Machine Learning para resolver problemas reales de negocio: segmentación, predicción y forecasting.

## Contenido

### 📘 En el libro

El contenido completo de este módulo es el **Módulo 7** del
[libro del curso](../latex/curso-bi-rstudio.pdf):
- Todo su código, probado de principio a fin: [`latex/codigo/07-machine-learning.R`](../latex/codigo/07-machine-learning.R)
- Las soluciones de sus ejercicios: [`latex/codigo/sol-07.R`](../latex/codigo/sol-07.R)

### 📝 Scripts cortos de práctica en esta carpeta

Scripts de la primera versión del curso, con secciones "TU CÓDIGO AQUÍ" para
practicar. Ejecútalos con el directorio de trabajo en esta carpeta.

| Script | Tema |
|---|---|
| [`02_clustering_clientes.R`](02_clustering_clientes.R) | Segmentación de clientes con k-means, de principio a fin |

## ⏱️ Tiempo estimado
10-12 horas de estudio y práctica

## 🎯 Al finalizar este módulo podrás:
- Segmentar clientes automáticamente
- Predecir ventas futuras
- Clasificar transacciones y detectar patrones
- Hacer forecasting de demanda
- Encontrar asociaciones entre productos
- Evaluar modelos de ML
- Implementar soluciones de ML en producción

## 📦 Paquetes necesarios
```r
install.packages("caret")        # Framework de ML
install.packages("cluster")      # Clustering
install.packages("factoextra")   # Visualización de clusters
install.packages("randomForest") # Random Forest
install.packages("rpart")        # Árboles de decisión
install.packages("rpart.plot")   # Visualizar árboles
install.packages("forecast")     # Series de tiempo
install.packages("arules")       # Market basket analysis
```

## 💡 Conceptos clave

### Aprendizaje Supervisado
- **Regresión**: Predecir valores numéricos (ventas, precios)
- **Clasificación**: Predecir categorías (churn, segmento)

### Aprendizaje No Supervisado
- **Clustering**: Agrupar clientes similares
- **Reglas de asociación**: Productos que se compran juntos

### Series de Tiempo
- **Forecasting**: Predecir valores futuros
- **Estacionalidad**: Patrones que se repiten
- **Tendencias**: Dirección a largo plazo

## 🎯 Aplicaciones en BI

### Segmentación de Clientes
- Identificar grupos de clientes con comportamiento similar
- Personalizar estrategias de marketing
- Optimizar recursos

### Predicción de Ventas
- Forecasting de demanda
- Planificación de inventario
- Presupuestos y metas

### Detección de Patrones
- Productos que se compran juntos
- Comportamiento de compra
- Cross-selling y up-selling

### Clasificación de Riesgo
- Clientes con probabilidad de cancelar (churn)
- Detección de fraude
- Scoring de crédito

## 📊 Flujo de trabajo de ML

1. **Definir el problema** - ¿Qué queremos predecir/clasificar?
2. **Recolectar datos** - Obtener datos históricos
3. **Explorar datos** - EDA (Exploratory Data Analysis)
4. **Preparar datos** - Limpieza y transformación
5. **Dividir datos** - Train/Test split
6. **Entrenar modelo** - Ajustar el algoritmo
7. **Evaluar modelo** - Métricas de performance
8. **Optimizar** - Mejorar el modelo
9. **Implementar** - Usar en producción

## 🚀 Comienza por aquí
Lee el Módulo 7 del libro y ejecuta [`latex/codigo/07-machine-learning.R`](../latex/codigo/07-machine-learning.R) por partes en RStudio. Después practica con `02_clustering_clientes.R`.
