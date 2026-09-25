# Proyecto Final: Dashboard Ejecutivo de Retail Analytics 🎯

## Descripción del Proyecto

Este es el proyecto integrador que combina **TODOS** los conceptos aprendidos en el curso. Crearás un sistema completo de Business Intelligence para una empresa de retail.

## Objetivos del Proyecto

Desarrollar un dashboard ejecutivo completo que incluya:

1. **ETL (Extract, Transform, Load)**
   - Importar datos de múltiples fuentes
   - Limpiar y transformar datos
   - Crear un data warehouse integrado

2. **Análisis Exploratorio**
   - Estadísticas descriptivas
   - Identificación de tendencias
   - Detección de anomalías

3. **Visualizaciones Ejecutivas**
   - KPIs principales
   - Gráficos de tendencias
   - Análisis comparativos

4. **Machine Learning**
   - Segmentación de clientes
   - Predicción de ventas
   - Análisis de canasta de compras

5. **Reportes Automatizados**
   - Reportes en RMarkdown
   - Dashboard interactivo con Shiny
   - Automatización de análisis

## Estructura sugerida para tu proyecto

Esta es la organización que te recomendamos para **tu** versión del proyecto
(el Módulo 9 del libro la explica y muestra el script maestro):

```
proyecto-final/
├── README.md                    (este archivo)
├── datos/                       (datos fuente)
│   ├── raw/                     (datos originales)
│   └── processed/               (datos procesados)
├── scripts/
│   ├── 01_etl.R                (carga y limpieza)
│   ├── 02_analisis.R           (análisis exploratorio)
│   ├── 03_visualizacion.R      (gráficos)
│   ├── 04_machine_learning.R   (modelos ML)
│   └── 05_master.R             (ejecutar todo)
├── dashboard/
│   ├── app.R                   (Shiny dashboard)
│   └── ui_components.R         (componentes UI)
├── reportes/
│   ├── reporte_ejecutivo.Rmd   (reporte principal)
│   └── reporte_detallado.Rmd   (análisis completo)
└── resultados/
    ├── graficos/               (visualizaciones)
    ├── modelos/                (modelos ML guardados)
    └── exports/                (datos exportados)
```

## Escenario de Negocio

### Empresa: **TechRetail México**

**Descripción**: Cadena de tiendas de electrónica con 10 sucursales en diferentes ciudades de México.

**Desafío**: La dirección necesita un sistema de BI para:
- Entender el comportamiento de compra de los clientes
- Optimizar el inventario
- Aumentar las ventas
- Reducir costos operativos

**Datos Disponibles**:
- Transacciones de ventas (1 año)
- Catálogo de productos
- Base de datos de clientes
- Información de empleados
- Datos de tiendas/sucursales

## KPIs Principales a Analizar

### Ventas
- **Ingresos Totales**: Suma de todas las ventas
- **Crecimiento MoM**: Crecimiento mes sobre mes
- **Ticket Promedio**: Venta promedio por transacción
- **Unidades Vendidas**: Total de productos vendidos

### Clientes
- **Clientes Activos**: Clientes con compras en el período
- **Tasa de Retención**: % de clientes que regresan
- **CLV (Customer Lifetime Value)**: Valor de vida del cliente
- **NPS (Net Promoter Score)**: Satisfacción del cliente

### Productos
- **Productos Top**: Los más vendidos
- **Margen Promedio**: Rentabilidad por producto
- **Rotación de Inventario**: Velocidad de venta
- **Productos Sin Movimiento**: Stock muerto

### Operaciones
- **Ventas por Sucursal**: Rendimiento por tienda
- **Productividad por Empleado**: Ventas por vendedor
- **Días de Inventario**: Días de stock disponible

## Fases del Proyecto

### Fase 1: Preparación de Datos (ETL)
**Duración estimada: 2-3 horas**

- [ ] Cargar todos los datasets
- [ ] Explorar estructura y calidad de datos
- [ ] Limpiar datos (duplicados, NAs, outliers)
- [ ] Transformar y crear variables derivadas
- [ ] Unir tablas (joins)
- [ ] Crear dataset maestro integrado

**Archivos**: `01_etl.R`

### Fase 2: Análisis Exploratorio
**Duración estimada: 2-3 horas**

- [ ] Análisis estadístico descriptivo
- [ ] Identificar tendencias temporales
- [ ] Análisis por segmentos
- [ ] Correlaciones entre variables
- [ ] Detección de anomalías
- [ ] Responder preguntas clave de negocio

**Archivos**: `02_analisis.R`

### Fase 3: Visualizaciones
**Duración estimada: 3-4 horas**

- [ ] Crear KPI cards
- [ ] Gráficos de tendencias
- [ ] Comparativos por categoría/región
- [ ] Mapas de calor
- [ ] Dashboards estáticos
- [ ] Aplicar marca corporativa

**Archivos**: `03_visualizacion.R`

### Fase 4: Machine Learning
**Duración estimada: 3-4 horas**

- [ ] Segmentación de clientes (K-means)
- [ ] Predicción de ventas (Regresión)
- [ ] Forecasting de demanda (Series de tiempo)
- [ ] Market Basket Analysis
- [ ] Evaluación de modelos
- [ ] Interpretación de resultados

**Archivos**: `04_machine_learning.R`

### Fase 5: Dashboard Interactivo
**Duración estimada: 4-5 horas**

- [ ] Diseñar interfaz con Shiny
- [ ] Implementar filtros interactivos
- [ ] Crear pestañas por tema
- [ ] Integrar gráficos reactivos
- [ ] Agregar tablas de datos
- [ ] Optimizar rendimiento

**Archivos**: `dashboard/app.R`

### Fase 6: Reportes Automatizados
**Duración estimada: 2-3 horas**

- [ ] Crear reporte ejecutivo en RMarkdown
- [ ] Parametrizar reportes
- [ ] Exportar a PDF/HTML/Word
- [ ] Automatizar generación
- [ ] Documentar hallazgos

**Archivos**: `reportes/reporte_ejecutivo.Rmd`

## Preguntas de Negocio a Responder

### Ventas
1. ¿Cuál es la tendencia de ventas en el último año?
2. ¿Qué meses tienen mejor rendimiento?
3. ¿Qué categorías de productos generan más ingresos?
4. ¿Cuál es el ticket promedio y cómo ha evolucionado?
5. ¿Qué sucursales tienen mejor desempeño?

### Clientes
6. ¿Cuántos clientes activos tenemos?
7. ¿Cuál es el perfil del cliente típico?
8. ¿Podemos segmentar clientes automáticamente?
9. ¿Qué clientes son más valiosos (CLV)?
10. ¿Cuál es la tasa de retención?

### Productos
11. ¿Cuáles son los productos estrella?
12. ¿Qué productos tienen mejor margen?
13. ¿Hay productos con baja rotación?
14. ¿Qué productos se compran juntos?
15. ¿Podemos predecir la demanda futura?

### Operaciones
16. ¿Qué vendedores tienen mejor desempeño?
17. ¿Qué días/horas hay más ventas?
18. ¿Cuál es el método de pago preferido?
19. ¿Hay estacionalidad en las ventas?
20. ¿Qué sucursales necesitan más atención?

## Entregables

Al finalizar el proyecto deberás tener:

1. **Código R Completo**
   - Scripts organizados y comentados
   - Código reproducible
   - Buenas prácticas aplicadas

2. **Dashboard Interactivo**
   - Shiny app funcional
   - Interfaz intuitiva
   - Filtros interactivos

3. **Reporte Ejecutivo**
   - PDF profesional
   - Insights clave
   - Recomendaciones de negocio

4. **Presentación**
   - Resumen de hallazgos
   - Visualizaciones impactantes
   - Plan de acción

5. **Documentación**
   - README completo
   - Guía de uso
   - Diccionario de datos

## Criterios de Evaluación

### Técnico (50%)
- [x] Código limpio y bien organizado
- [x] Uso correcto de funciones y paquetes
- [x] Manejo adecuado de datos
- [x] Implementación correcta de ML
- [x] Dashboard funcional

### Analítico (30%)
- [x] Análisis profundo y relevante
- [x] Insights valiosos para el negocio
- [x] Interpretación correcta de resultados
- [x] Recomendaciones accionables

### Presentación (20%)
- [x] Visualizaciones claras y efectivas
- [x] Reporte bien estructurado
- [x] Documentación completa
- [x] Profesionalismo

## Recursos Adicionales

### Datos
Los datasets están en la carpeta `../datasets/`

### Ejemplos
Revisa los módulos anteriores del curso para referencia

### Ayuda
- Consulta la documentación de R: `?funcion`
- Stack Overflow: https://stackoverflow.com/questions/tagged/r
- RStudio Cheatsheets: https://www.rstudio.com/resources/cheatsheets/

## Comenzar el Proyecto

1. **Lee el Módulo 9 del libro** ([`latex/curso-bi-rstudio.pdf`](../latex/curso-bi-rstudio.pdf)):
   resuelve el caso completo paso a paso y trae 8 retos de extensión con sus
   soluciones. Su código completo está en
   [`latex/codigo/09-proyecto-final.R`](../latex/codigo/09-proyecto-final.R).

2. **Genera los datasets** (una sola vez, desde la carpeta del curso):
   ```r
   source("datasets/generar_datasets.R")
   ```

3. **Ejecuta el ETL de ejemplo** de esta carpeta, con el directorio de trabajo
   en `proyecto-final/`:
   ```r
   source("01_proyecto_etl.R")
   ```

4. **Lanza el dashboard ejecutivo** (Módulo 6), desde la carpeta del curso:
   ```r
   shiny::runApp("latex/codigo/apps/03-dashboard-ejecutivo")
   ```

5. **Genera el reporte ejecutivo** (Módulo 8) con
   [`latex/codigo/reportes/generar_reportes.R`](../latex/codigo/reportes/generar_reportes.R).

## ¡Buena Suerte! 🚀

Este proyecto es tu oportunidad de demostrar todo lo aprendido. Tómate tu tiempo, experimenta, y crea algo de lo que estés orgulloso.

Recuerda: **El objetivo no es solo completar el proyecto, sino entender profundamente cómo resolver problemas reales de negocio con datos.**

---

**Tiempo total estimado**: 18-25 horas
**Nivel de dificultad**: Avanzado
**Aplicabilidad real**: 100%
