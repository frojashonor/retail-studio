# Progreso del libro LaTeX

**Estado: terminado.** El libro compila completo en 798 páginas
(`curso-bi-rstudio.pdf`) y todo su código pasó la verificación final.

## Contenido

| Parte | Archivo | Bloques de código | Ejercicios |
|---|---|---|---|
| Introducción | `capitulos/00-introduccion.tex` | 13 | — |
| Módulo 1. Fundamentos de R y RStudio | `capitulos/01-fundamentos.tex` | 98 | 12 |
| Módulo 2. dplyr y tidyr | `capitulos/02-manipulacion-datos.tex` | 96 | 12 |
| Módulo 3. Visualización con ggplot2 | `capitulos/03-visualizacion.tex` | 49 | 10 |
| Módulo 4. Análisis exploratorio y estadística | `capitulos/04-analisis-exploratorio.tex` | 82 | 10 |
| Módulo 5. Bases de datos, SQL y ETL | `capitulos/05-bases-datos.tex` | 71 | 10 |
| Módulo 6. Dashboards con Shiny | `capitulos/06-dashboards-shiny.tex` | 42 | 8 |
| Módulo 7. Machine learning para BI | `capitulos/07-machine-learning.tex` | 77 | 10 |
| Módulo 8. Reportes con R Markdown | `capitulos/08-reportes.tex` | 21 | 8 |
| Módulo 9. Proyecto final | `capitulos/09-proyecto-final.tex` | 40 | 8 |
| Apéndice A. Soluciones | `apendices/soluciones/sol-01.tex` … `sol-09.tex` | 110 | 88 soluciones |
| Apéndice B. Referencia rápida | `apendices/B-referencia-rapida.tex` | 12 | — |

Además: 3 apps Shiny en `codigo/apps/`, 4 archivos de reportes en
`codigo/reportes/` y un script `.R` por módulo y por archivo de soluciones en
`codigo/`.

## Verificación final

Se hizo en una carpeta limpia, generando los datos desde cero con
`datasets/generar_datasets.R`, igual que lo hará el lector.

- **Ejecución:** los 20 archivos (10 capítulos, 9 de soluciones y el
  apéndice B) se ejecutan de principio a fin sin errores con
  `herramientas/extraer_codigo.py --ejecutar`. Se ejecutaron dos veces
  seguidas en la misma carpeta, con el mismo resultado.
- **Salidas:** `herramientas/verificar_salidas.py` comparó 594 salidas
  impresas en el libro con la salida real de R; todas coinciden.
- **Apps y reportes:** las 3 apps Shiny arrancan y pasan pruebas con
  `testServer()`. El reporte ejecutivo se generó en HTML, PDF y Word, y
  también para las 10 tiendas en un solo paso.
- **LaTeX:** compila sin errores, sin referencias rotas, sin etiquetas
  duplicadas y sin caracteres faltantes. Solo quedan 4 avisos de cajas anchas
  en la portada, que no afectan cómo se ve.

## Notas

- Los módulos salieron más largos de lo planeado (entre 51 y 85 páginas los
  centrales). Si se quiere un libro más corto, lo más fácil de recortar son
  las salidas largas de R y algunos listados completos de las apps Shiny.
- Algunos resultados con los datos simulados son "no significativos" o no
  muestran patrón (por ejemplo, los clientes premium no gastan más). El libro
  los presenta como resultados válidos y los aprovecha para enseñar.
- Los nombres de días y meses dependen del idioma del sistema del lector; el
  libro usa español.
