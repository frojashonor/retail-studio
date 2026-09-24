# 📊 Curso Práctico de Business Intelligence con RStudio
### De Principiante a Avanzado

¡Bienvenido al curso completo de Business Intelligence y Business Advanced con RStudio! Este curso está diseñado para llevarte desde cero hasta un nivel avanzado en el análisis de datos empresariales.

---

## 🎯 Objetivos del Curso

Al finalizar este curso serás capaz de:
- Manipular y transformar grandes volúmenes de datos
- Crear visualizaciones profesionales para reportes ejecutivos
- Conectarte a bases de datos y realizar consultas SQL desde R
- Construir dashboards interactivos
- Aplicar técnicas de Machine Learning para predicciones empresariales
- Automatizar reportes y análisis

---

## 📘 Versión en libro (PDF)

Todo el curso está también en un libro de 798 páginas, con código comentado,
salidas reales de R, ejercicios y sus soluciones:
[`latex/curso-bi-rstudio.pdf`](latex/curso-bi-rstudio.pdf).
El código fuente LaTeX y cómo compilarlo están en [`latex/`](latex/README.md).

---

## 📚 Estructura del Curso

### **NIVEL PRINCIPIANTE** 🌱

#### **Módulo 1: Fundamentos de R y RStudio**
- Instalación y configuración del entorno
- Tipos de datos y estructuras
- Operaciones básicas y funciones
- Importación y exportación de datos
- 📁 Carpeta: `modulo-01-fundamentos/`

#### **Módulo 2: Manipulación de Datos (dplyr & tidyr)**
- Filtrar, seleccionar y ordenar datos
- Crear nuevas columnas y transformaciones
- Agrupar y resumir información
- Unir datasets (joins)
- Pivotar tablas
- 📁 Carpeta: `modulo-02-manipulacion-datos/`

#### **Módulo 3: Visualización de Datos (ggplot2)**
- Gráficos de barras, líneas y dispersión
- Histogramas y boxplots
- Mapas de calor y gráficos de correlación
- Personalización y temas profesionales
- 📁 Carpeta: `modulo-03-visualizacion/`

---

### **NIVEL INTERMEDIO** 🚀

#### **Módulo 4: Análisis Exploratorio y Estadística**
- Medidas de tendencia central y dispersión
- Distribuciones de probabilidad
- Pruebas de hipótesis
- Correlaciones y regresiones lineales
- Detección de outliers
- 📁 Carpeta: `modulo-04-analisis-exploratorio/`

#### **Módulo 5: Conexión a Bases de Datos y SQL**
- Conectar a MySQL, PostgreSQL, SQL Server
- Ejecutar consultas SQL desde R
- Optimización de queries
- ETL (Extract, Transform, Load)
- 📁 Carpeta: `modulo-05-bases-datos/`

#### **Módulo 6: Dashboards Interactivos (Shiny)**
- Estructura básica de una app Shiny
- Inputs y outputs reactivos
- Dashboards con múltiples pestañas
- Publicación de aplicaciones
- 📁 Carpeta: `modulo-06-dashboards-shiny/`

---

### **NIVEL AVANZADO** 🎓

#### **Módulo 7: Machine Learning para Business Intelligence**
- Clustering (segmentación de clientes)
- Clasificación (predicción de categorías)
- Regresión (forecasting de ventas)
- Series de tiempo
- Análisis de canasta de compras
- 📁 Carpeta: `modulo-07-machine-learning/`

#### **Módulo 8: Reportes Automáticos (RMarkdown)**
- Crear reportes en PDF, HTML y Word
- Integrar código, gráficos y texto
- Parametrizar reportes
- Automatización con scripts
- 📁 Carpeta: `modulo-08-reportes/`

---

## 📊 Datasets Incluidos

En la carpeta `datasets/` encontrarás:
- **ventas_retail.csv** - Datos de ventas de comercio minorista
- **clientes.csv** - Base de clientes con información demográfica
- **productos.csv** - Catálogo de productos
- **empleados.csv** - Información de recursos humanos
- **transacciones.csv** - Transacciones detalladas

---

## 🎯 Proyecto Final Integrador

Aplicarás todo lo aprendido en un caso real de Business Intelligence:
**"Dashboard Ejecutivo de Retail Analytics"**

Incluye:
- Análisis de ventas y tendencias
- Segmentación de clientes
- Predicción de demanda
- Dashboard interactivo
- Reporte ejecutivo automatizado

📁 Carpeta: `proyecto-final/`

---

## 🚀 Cómo Usar Este Curso

1. **Sigue el orden de los módulos** - Están diseñados progresivamente
2. **Ejecuta todos los ejemplos** - La práctica es fundamental
3. **Haz los ejercicios** - Cada módulo incluye ejercicios prácticos
4. **Experimenta** - Modifica los códigos y observa qué pasa
5. **Consulta la documentación** - Cada script está ampliamente comentado

---

## 📋 Requisitos Previos

- **RStudio** instalado (versión 2023.x o superior recomendada)
- **R** versión 4.0 o superior
- Conocimientos básicos de computación
- Ganas de aprender 🚀

---

## 📦 Paquetes Necesarios

Ejecuta este código al inicio para instalar todos los paquetes:

```r
# Lista de paquetes necesarios
paquetes <- c(
  "tidyverse",      # Colección de paquetes para ciencia de datos
  "readxl",         # Leer archivos Excel
  "writexl",        # Escribir archivos Excel
  "lubridate",      # Manejo de fechas
  "scales",         # Formateo de escalas
  "plotly",         # Gráficos interactivos
  "DT",             # Tablas interactivas
  "shiny",          # Aplicaciones web
  "shinydashboard", # Dashboards
  "DBI",            # Interfaz de bases de datos
  "RSQLite",        # SQLite
  "odbc",           # Conexión ODBC
  "caret",          # Machine Learning
  "randomForest",   # Random Forest
  "cluster",        # Clustering
  "factoextra",     # Visualización de clustering
  "forecast",       # Series de tiempo
  "rmarkdown",      # Reportes
  "knitr"           # Generación de reportes
)

# Instalar paquetes que no estén instalados
install.packages(setdiff(paquetes, rownames(installed.packages())))
```

---

## 💡 Consejos para el Éxito

1. **Practica diariamente** - Aunque sea 30 minutos
2. **Toma notas** - Anota lo que aprendes
3. **Comparte** - Enseñar a otros refuerza tu aprendizaje
4. **Busca ayuda** - La comunidad de R es muy activa
5. **Sé paciente** - El dominio viene con la práctica

---

## 📖 Recursos Adicionales

- [R for Data Science](https://r4ds.had.co.nz/) - Libro gratuito online
- [RStudio Cheatsheets](https://www.rstudio.com/resources/cheatsheets/) - Guías rápidas
- [Stack Overflow](https://stackoverflow.com/questions/tagged/r) - Comunidad de ayuda
- [R-bloggers](https://www.r-bloggers.com/) - Blog con tutoriales

---

## 🎉 ¡Comencemos!

Dirígete al **Módulo 1** para comenzar tu viaje en Business Intelligence con RStudio.

**¡Éxito en tu aprendizaje! 🚀📊**
