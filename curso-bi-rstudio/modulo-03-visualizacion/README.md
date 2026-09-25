# Módulo 3: Visualización de Datos con ggplot2 📊

## Objetivo
Crear visualizaciones profesionales y efectivas para reportes de Business Intelligence.

## Contenido

### 📘 En el libro

El contenido completo de este módulo es el **Módulo 3** del
[libro del curso](../latex/curso-bi-rstudio.pdf):
- Todo su código, probado de principio a fin: [`latex/codigo/03-visualizacion.R`](../latex/codigo/03-visualizacion.R)
- Las soluciones de sus ejercicios: [`latex/codigo/sol-03.R`](../latex/codigo/sol-03.R)

### 📝 Scripts cortos de práctica en esta carpeta

Scripts de la primera versión del curso, con secciones "TU CÓDIGO AQUÍ" para
practicar. Ejecútalos con el directorio de trabajo en esta carpeta.

| Script | Tema |
|---|---|
| [`01_introduccion_ggplot2.R`](01_introduccion_ggplot2.R) | Barras, líneas, dispersión, histogramas, boxplots, temas y exportar |

## ⏱️ Tiempo estimado
8-10 horas de estudio y práctica

## 🎯 Al finalizar este módulo podrás:
- Crear gráficos profesionales para reportes ejecutivos
- Visualizar tendencias y patrones en datos
- Personalizar gráficos con marca corporativa
- Combinar múltiples gráficos
- Exportar visualizaciones en alta calidad
- Elegir el tipo correcto de gráfico para cada análisis

## 📦 Paquetes necesarios
```r
install.packages("ggplot2")
install.packages("scales")      # Formateo de ejes
install.packages("RColorBrewer") # Paletas de colores
install.packages("plotly")       # Gráficos interactivos
```

## 💡 Conceptos clave
- **Grammar of Graphics**: Sistema coherente para crear gráficos
- **Aesthetics (aes)**: Mapeo de datos a propiedades visuales
- **Geoms**: Objetos geométricos (puntos, líneas, barras)
- **Scales**: Control de ejes, colores y tamaños
- **Themes**: Estilos y apariencia

## 📚 Tipos de gráficos que aprenderás

### Para Comparaciones
- Gráficos de barras
- Gráficos de columnas
- Gráficos de barras apiladas

### Para Tendencias
- Gráficos de líneas
- Gráficos de área
- Series de tiempo

### Para Distribuciones
- Histogramas
- Boxplots (diagramas de caja)
- Gráficos de densidad
- Violin plots

### Para Relaciones
- Scatter plots (dispersión)
- Gráficos de burbujas
- Mapas de calor (heatmaps)

### Para Composiciones
- Gráficos de torta (pie charts)
- Treemaps
- Gráficos apilados

## 🎨 Principios de visualización efectiva
1. **Claridad** - El mensaje debe ser obvio
2. **Precisión** - Representar datos honestamente
3. **Eficiencia** - Máxima información con mínimo ruido
4. **Estética** - Profesional y atractivo

## 🚀 Comienza por aquí
Lee el Módulo 3 del libro y ejecuta [`latex/codigo/03-visualizacion.R`](../latex/codigo/03-visualizacion.R) por partes en RStudio. Después practica con `01_introduccion_ggplot2.R`.
