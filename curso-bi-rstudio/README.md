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

El material principal es el **libro**. Cada módulo tiene su capítulo, un script
con todo su código (probado de principio a fin) y un script con las soluciones
de sus ejercicios.

| Nivel | Módulo | Tema | Código del módulo | Soluciones |
|---|---|---|---|---|
| — | Intro | Cómo usar el curso, instalación, diccionario de datos | [`00-introduccion.R`](latex/codigo/00-introduccion.R) | — |
| 🌱 Principiante | 1 | Fundamentos de R y RStudio | [`01-fundamentos.R`](latex/codigo/01-fundamentos.R) | [`sol-01.R`](latex/codigo/sol-01.R) |
| 🌱 Principiante | 2 | Manipulación de datos con dplyr y tidyr | [`02-manipulacion-datos.R`](latex/codigo/02-manipulacion-datos.R) | [`sol-02.R`](latex/codigo/sol-02.R) |
| 🌱 Principiante | 3 | Visualización con ggplot2 | [`03-visualizacion.R`](latex/codigo/03-visualizacion.R) | [`sol-03.R`](latex/codigo/sol-03.R) |
| 🚀 Intermedio | 4 | Análisis exploratorio y estadística | [`04-analisis-exploratorio.R`](latex/codigo/04-analisis-exploratorio.R) | [`sol-04.R`](latex/codigo/sol-04.R) |
| 🚀 Intermedio | 5 | Bases de datos, SQL y ETL | [`05-bases-datos.R`](latex/codigo/05-bases-datos.R) | [`sol-05.R`](latex/codigo/sol-05.R) |
| 🚀 Intermedio | 6 | Dashboards con Shiny | [`06-dashboards-shiny.R`](latex/codigo/06-dashboards-shiny.R) + [`apps/`](latex/codigo/apps/) | [`sol-06.R`](latex/codigo/sol-06.R) |
| 🎓 Avanzado | 7 | Machine learning para BI | [`07-machine-learning.R`](latex/codigo/07-machine-learning.R) | [`sol-07.R`](latex/codigo/sol-07.R) |
| 🎓 Avanzado | 8 | Reportes automáticos con R Markdown | [`08-reportes.R`](latex/codigo/08-reportes.R) + [`reportes/`](latex/codigo/reportes/) | [`sol-08.R`](latex/codigo/sol-08.R) |
| 🎯 Proyecto | 9 | Proyecto final: TechRetail México | [`09-proyecto-final.R`](latex/codigo/09-proyecto-final.R) | [`sol-09.R`](latex/codigo/sol-09.R) |
| 📎 Apéndice | B | Referencia rápida, errores frecuentes y glosario | [`B-referencia-rapida.R`](latex/codigo/B-referencia-rapida.R) | — |

### Scripts cortos de práctica

Las carpetas `modulo-01-fundamentos/`, `modulo-02-manipulacion-datos/`,
`modulo-03-visualizacion/`, `modulo-07-machine-learning/` y `proyecto-final/`
tienen scripts más cortos, de la primera versión del curso, con ejercicios
"TU CÓDIGO AQUÍ" para practicar. Son un complemento: el contenido completo
de cada módulo está en el libro y en `latex/codigo/`.

---

## 📊 Datasets del curso

Los datos son simulados (con semilla fija, así que obtendrás los mismos
números que el libro). Genéralos una sola vez con el directorio de trabajo en
esta carpeta (`curso-bi-rstudio/`):

```r
source("datasets/generar_datasets.R")
```

Se crean en `datasets/`:
- **ventas_retail.csv** — una venta por día de 2023 (365 filas)
- **transacciones.csv** — 1,000 transacciones con método de pago, hora, tienda y vendedor
- **clientes.csv** — 200 clientes con datos demográficos
- **productos.csv** — catálogo de 50 productos con precio, costo e inventario
- **empleados.csv** — 100 empleados
- **tiendas.csv** — 10 sucursales
- **datos_empresa.xlsx** — muestra de todo lo anterior en un Excel con varias hojas

---

## 🎯 Proyecto Final Integrador

El Módulo 9 del libro aplica todo lo aprendido al caso **TechRetail México**:
ETL y auditoría de calidad de datos, KPIs, segmentación de clientes,
pronóstico, dashboard, reporte ejecutivo y presentación de resultados, con 8
retos de extensión resueltos.

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
  "dbplyr",         # dplyr sobre bases de datos
  "caret",          # Machine Learning
  "randomForest",   # Random Forest
  "cluster",        # Clustering
  "factoextra",     # Visualización de clustering
  "rpart",          # Árboles de decisión
  "forecast",       # Series de tiempo
  "rmarkdown",      # Reportes
  "knitr",          # Generación de reportes
  "skimr",          # Resúmenes rápidos
  "corrplot"        # Matrices de correlación
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

- [R for Data Science, 2.ª ed.](https://r4ds.hadley.nz/) - Libro gratuito online
- [RStudio Cheatsheets](https://www.rstudio.com/resources/cheatsheets/) - Guías rápidas
- [Stack Overflow](https://stackoverflow.com/questions/tagged/r) - Comunidad de ayuda
- [R-bloggers](https://www.r-bloggers.com/) - Blog con tutoriales

---

## 🎉 ¡Comencemos!

Abre el [libro](latex/curso-bi-rstudio.pdf), sigue la introducción para preparar tu entorno y continúa con el **Módulo 1**.

**¡Éxito en tu aprendizaje! 🚀📊**
