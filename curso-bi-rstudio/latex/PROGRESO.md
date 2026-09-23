# Progreso del libro LaTeX

Estado al 23 de septiembre de 2026. El libro **todavía no compila completo**:
faltan capítulos y el apéndice de soluciones.

## Qué pasó

Los 11 redactores que escribían los capítulos en paralelo se detuvieron a la
mitad porque se alcanzó el límite de uso de la sesión. Lo que alcanzaron a
escribir está guardado en esta rama. Nada de lo que aparece abajo como
"parcial" ha pasado todavía la verificación final (ejecutar todo el código y
comparar las salidas impresas con las reales).

## Estado por parte

| Parte | Archivo | Estado | Hasta dónde llega |
|---|---|---|---|
| Estructura, estilos, portada | `preambulo.tex`, `portada.tex`, `curso-bi-rstudio.tex` | ✅ Terminado y probado | — |
| Herramientas de verificación | `herramientas/*.py`, `*.sh` | ✅ Terminado y probado | — |
| Introducción | `capitulos/00-introduccion.tex` | 🟡 Escrito, sin verificación final | Completo (13 secciones, 11 bloques de código) |
| Módulo 1. Fundamentos | `capitulos/01-fundamentos.tex` | 🟡 Parcial | Secciones 1–8 (hasta listas y matrices). Faltan: control de flujo, funciones propias, paquetes, importar/exportar, ayuda, caso integral, errores comunes, resumen y ejercicios |
| Módulo 2. dplyr y tidyr | `capitulos/02-manipulacion-datos.tex` | 🟡 Parcial | Hasta funciones de ventana. Faltan: lubridate, stringr, joins, tidyr, limpieza, caso integral, errores, resumen y ejercicios |
| Módulo 3. ggplot2 | — | ❌ Sin capítulo | Solo existe un borrador del script de figuras (`herramientas/figuras/figuras-03.R`) |
| Módulo 4. Estadística | `capitulos/04-analisis-exploratorio.tex` | 🟡 Parcial | Hasta Pareto/ABC (2 figuras hechas). Faltan: correlación, distribuciones, intervalos, pruebas de hipótesis, regresión, caso integral, resumen y ejercicios |
| Módulo 5. Bases de datos | — | ❌ Sin empezar | — |
| Módulo 6. Shiny | — | ❌ Sin capítulo | Las 3 apps ya existen en `codigo/apps/` pero aún no se verificaron |
| Módulo 7. Machine learning | — | ❌ Sin empezar | — |
| Módulo 8. Reportes | — | ❌ Sin capítulo | Reportes `.Rmd` y script de automatización en `codigo/reportes/`, aún sin verificar |
| Módulo 9. Proyecto final | `capitulos/09-proyecto-final.tex` | 🟡 Parcial | Fases 1 y 2. Faltan: fases 3–6, script maestro, checklist y retos |
| Apéndice A. Soluciones | `apendices/soluciones/` | ❌ Vacío | Ningún módulo alcanzó a escribir ejercicios ni soluciones |
| Apéndice B. Referencia rápida | — | ❌ Sin empezar | — |

## Pasos siguientes

1. Terminar los módulos parciales (1, 2, 4, 9) a partir de donde se quedaron.
2. Escribir los módulos 3, 5, 6, 7 y 8, y el apéndice B.
3. Escribir los ejercicios de cada módulo y sus soluciones.
4. Verificar cada capítulo con `herramientas/extraer_codigo.py --ejecutar` y
   `herramientas/verificar_salidas.py`.
5. Compilar el libro completo, revisarlo visualmente y generar
   `curso-bi-rstudio.pdf`.
