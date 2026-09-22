# Libro LaTeX: Business Intelligence con R y RStudio

Versión en libro (PDF) del curso completo, de principiante a avanzado.

- **PDF listo para leer:** [`curso-bi-rstudio.pdf`](curso-bi-rstudio.pdf)
- **Archivo principal:** `curso-bi-rstudio.tex`

## Contenido

| Parte | Módulo | Tema |
|---|---|---|
| — | Introducción | Cómo usar el libro, instalación, diccionario de datos |
| Principiante | 1 | Fundamentos de R y RStudio |
| Principiante | 2 | Manipulación de datos con dplyr y tidyr |
| Principiante | 3 | Visualización de datos con ggplot2 |
| Intermedio | 4 | Análisis exploratorio y estadística para negocios |
| Intermedio | 5 | Bases de datos, SQL y ETL desde R |
| Intermedio | 6 | Dashboards interactivos con Shiny |
| Avanzado | 7 | Machine learning para BI |
| Avanzado | 8 | Reportes automáticos con R Markdown |
| Proyecto | 9 | Proyecto final: TechRetail México |
| Apéndice | A | Soluciones de todos los ejercicios |
| Apéndice | B | Referencia rápida (chuleta, errores frecuentes, glosario) |

Cada módulo incluye código comentado, la salida real de R, ejemplos de negocio
explicados, cajas "Hazlo tú" y ejercicios graduados con su solución.

## Cómo compilar

### En Overleaf (sin instalar nada)
1. Comprime la carpeta `latex/` en un `.zip`.
2. En Overleaf: **New Project → Upload Project** y sube el `.zip`.
3. Verifica que el compilador sea **pdfLaTeX** (Menu → Compiler) y que el
   documento principal sea `curso-bi-rstudio.tex`.
4. Pulsa **Recompile**.

### En tu computadora
Necesitas una distribución de LaTeX (TeX Live, MiKTeX o MacTeX):

```bash
cd curso-bi-rstudio/latex
latexmk -pdf curso-bi-rstudio.tex
```

## Estructura

```
latex/
├── curso-bi-rstudio.tex      archivo principal
├── curso-bi-rstudio.pdf      el libro compilado
├── preambulo.tex             paquetes, colores, cajas y estilos de código
├── portada.tex
├── capitulos/                un archivo por módulo
├── apendices/                soluciones y referencia rápida
├── figuras/                  gráficas generadas con R
├── codigo/                   todo el código del libro listo para ejecutar
│   ├── 01-fundamentos.R ...  un script por módulo (y sol-NN.R con soluciones)
│   ├── apps/                 apps Shiny completas del Módulo 6
│   └── reportes/             reportes R Markdown del Módulo 8
└── herramientas/             scripts para extraer y verificar el código
```

## Ejecutar el código del libro

1. Abre RStudio en la carpeta `curso-bi-rstudio/` (idealmente como Proyecto).
2. Genera los datos una sola vez:
   ```r
   source("datasets/generar_datasets.R")
   ```
3. Abre el script del módulo en `latex/codigo/` y ejecútalo por partes.

## Cómo se verificó el libro

Todo el código de los bloques ejecutables se extrajo y se corrió de principio a
fin con R 4.3 (`herramientas/extraer_codigo.py`), y las salidas impresas en el
libro se compararon automáticamente con las salidas reales
(`herramientas/verificar_salidas.py`). Las figuras se generan con los scripts de
`herramientas/figuras/` a partir del mismo código que aparece en el texto.

Los datos son simulados con semilla fija, así que obtendrás los mismos números
que el libro. Los nombres de días y meses dependen del idioma de tu sistema
(el libro usa español).
