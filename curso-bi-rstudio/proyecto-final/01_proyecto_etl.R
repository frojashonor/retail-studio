# ============================================================================
# PROYECTO FINAL - FASE 1: ETL (EXTRACT, TRANSFORM, LOAD)
# ============================================================================
# Empresa: TechRetail México
# Objetivo: Preparar datos para análisis de Business Intelligence
# ============================================================================

# ============================================================================
# 1. CONFIGURACIÓN INICIAL
# ============================================================================

# Limpiar entorno
rm(list = ls())

# Cargar librerías necesarias
library(tidyverse)    # Suite completa de data science
library(lubridate)    # Manejo de fechas
library(scales)       # Formateo de números

# Fecha de corte fija: los datos cubren 2023, así que medimos antigüedad y
# recencia al 1 de enero de 2024. Usar Sys.Date() haría que los resultados
# cambiaran cada día que ejecutes el script.
fecha_corte <- as.Date("2024-01-01")

# Configurar directorio de trabajo
# setwd("tu_ruta/curso-bi-rstudio/proyecto-final")

# Crear carpetas si no existen
if (!dir.exists("datos/raw")) dir.create("datos/raw", recursive = TRUE)
if (!dir.exists("datos/processed")) dir.create("datos/processed", recursive = TRUE)
if (!dir.exists("resultados")) dir.create("resultados", recursive = TRUE)

# ============================================================================
# 2. EXTRACT - CARGAR DATOS
# ============================================================================

cat("==== FASE 1: CARGA DE DATOS ====\n")

# Cargar todos los datasets
ventas <- read_csv("../datasets/ventas_retail.csv")
clientes <- read_csv("../datasets/clientes.csv")
productos <- read_csv("../datasets/productos.csv")
empleados <- read_csv("../datasets/empleados.csv")
tiendas <- read_csv("../datasets/tiendas.csv")
transacciones <- read_csv("../datasets/transacciones.csv")

cat("✓ Datos cargados exitosamente\n\n")

# ============================================================================
# 3. EXPLORACIÓN INICIAL
# ============================================================================

cat("==== EXPLORACIÓN INICIAL ====\n")

# Función para resumen rápido
explorar_datos <- function(df, nombre) {
  cat("\n", nombre, "\n")
  cat("  Dimensiones:", nrow(df), "filas x", ncol(df), "columnas\n")
  cat("  Columnas:", paste(names(df), collapse = ", "), "\n")
  cat("  NAs totales:", sum(is.na(df)), "\n")
}

# Explorar cada dataset
explorar_datos(ventas, "VENTAS")
explorar_datos(clientes, "CLIENTES")
explorar_datos(productos, "PRODUCTOS")
explorar_datos(empleados, "EMPLEADOS")
explorar_datos(tiendas, "TIENDAS")
explorar_datos(transacciones, "TRANSACCIONES")

# ============================================================================
# 4. TRANSFORM - LIMPIEZA Y TRANSFORMACIÓN
# ============================================================================

cat("\n==== FASE 2: LIMPIEZA Y TRANSFORMACIÓN ====\n")

# ----------------------------------------------------------------------------
# 4.1 VENTAS
# ----------------------------------------------------------------------------

ventas_clean <- ventas %>%
  # Convertir fecha a formato Date
  mutate(
    fecha = as.Date(fecha),
    mes = floor_date(fecha, "month"),
    trimestre = quarter(fecha),
    año = year(fecha),
    mes_nombre = format(fecha, "%B"),
    dia_semana = wday(fecha, label = TRUE, abbr = FALSE)
  ) %>%
  # Validar que no haya ventas negativas
  filter(cantidad > 0, precio_unitario > 0, total > 0) %>%
  # Eliminar duplicados si existen
  distinct()

cat("✓ Ventas limpiadas:", nrow(ventas_clean), "registros\n")

# ----------------------------------------------------------------------------
# 4.2 CLIENTES
# ----------------------------------------------------------------------------

clientes_clean <- clientes %>%
  # Convertir fecha de registro
  mutate(
    fecha_registro = as.Date(fecha_registro),
    antiguedad_dias = as.numeric(fecha_corte - fecha_registro),
    antiguedad_años = round(antiguedad_dias / 365, 1)
  ) %>%
  # Estandarizar ciudades (mayúsculas)
  mutate(ciudad = toupper(ciudad)) %>%
  # Eliminar registros sin email
  filter(!is.na(email)) %>%
  distinct(cliente_id, .keep_all = TRUE)

cat("✓ Clientes limpiados:", nrow(clientes_clean), "registros\n")

# ----------------------------------------------------------------------------
# 4.3 PRODUCTOS
# ----------------------------------------------------------------------------

productos_clean <- productos %>%
  # Calcular métricas adicionales
  mutate(
    # Margen ya está calculado, pero verificar
    margen_pct = ifelse(is.na(margen_pct),
                       ((precio_catalogo - costo) / precio_catalogo) * 100,
                       margen_pct),
    # Categorizar margen
    categoria_margen = case_when(
      margen_pct < 20 ~ "Bajo",
      margen_pct < 40 ~ "Medio",
      TRUE ~ "Alto"
    ),
    # Estado de stock más detallado
    dias_stock = ifelse(stock_actual > 0, stock_actual / 1, NA)  # Simplificado
  ) %>%
  # Solo productos activos
  filter(activo == TRUE) %>%
  distinct(producto_id, .keep_all = TRUE)

cat("✓ Productos limpiados:", nrow(productos_clean), "registros\n")

# ----------------------------------------------------------------------------
# 4.4 EMPLEADOS
# ----------------------------------------------------------------------------

empleados_clean <- empleados %>%
  # Calcular métricas
  mutate(
    fecha_ingreso = as.Date(fecha_ingreso),
    # Categorizar antigüedad
    categoria_antiguedad = case_when(
      antiguedad_anos < 2 ~ "Nuevo",
      antiguedad_anos < 5 ~ "Intermedio",
      TRUE ~ "Veterano"
    ),
    # Categorizar salario
    categoria_salario = case_when(
      salario_mensual < 15000 ~ "Junior",
      salario_mensual < 30000 ~ "Mid-Level",
      TRUE ~ "Senior"
    )
  ) %>%
  distinct(empleado_id, .keep_all = TRUE)

cat("✓ Empleados limpiados:", nrow(empleados_clean), "registros\n")

# ----------------------------------------------------------------------------
# 4.5 TIENDAS
# ----------------------------------------------------------------------------

tiendas_clean <- tiendas %>%
  mutate(
    fecha_apertura = as.Date(fecha_apertura),
    años_operacion = as.numeric(difftime(fecha_corte, fecha_apertura, units = "days")) / 365,
    años_operacion = round(años_operacion, 1),
    # Productividad aproximada (ventas por m2)
    categoria_tamaño = case_when(
      tamano_m2 < 400 ~ "Pequeña",
      tamano_m2 < 600 ~ "Mediana",
      TRUE ~ "Grande"
    )
  ) %>%
  distinct(tienda_id, .keep_all = TRUE)

cat("✓ Tiendas limpiadas:", nrow(tiendas_clean), "registros\n")

# ============================================================================
# 5. CREAR DATASET MAESTRO (DATA WAREHOUSE)
# ============================================================================

cat("\n==== FASE 3: CREACIÓN DE DATA WAREHOUSE ====\n")

# Unir ventas con todas las dimensiones
data_warehouse <- ventas_clean %>%
  # Unir con productos
  left_join(productos_clean %>%
             select(producto_id, nombre_producto, categoria, marca,
                   precio_catalogo, costo, margen_pct, categoria_margen),
           by = "producto_id") %>%
  # Unir con clientes
  left_join(clientes_clean %>%
             select(cliente_id, nombre, edad, genero, ciudad,
                   es_premium, segmento, antiguedad_años),
           by = "cliente_id") %>%
  # Unir con tiendas
  left_join(tiendas_clean %>%
             select(tienda_id, nombre_tienda, ciudad_tienda = ciudad,
                   zona, tipo, tamano_m2),
           by = "tienda_id") %>%
  # Unir con vendedores (empleados)
  left_join(empleados_clean %>%
             select(empleado_id, nombre_vendedor = nombre,
                   departamento, salario_mensual),
           by = c("vendedor_id" = "empleado_id")) %>%
  # Calcular métricas derivadas
  mutate(
    # Costo total de la venta
    costo_total = cantidad * costo,
    # Ganancia bruta
    ganancia_bruta = total - costo_total,
    # Margen real de la venta
    margen_real = (ganancia_bruta / total) * 100,
    # Descuento en monto absoluto
    descuento_absoluto = subtotal - total,
    # Categoría de venta
    categoria_venta = case_when(
      total < 500 ~ "Baja",
      total < 2000 ~ "Media",
      total < 5000 ~ "Alta",
      TRUE ~ "Premium"
    )
  )

cat("✓ Data Warehouse creado:", nrow(data_warehouse), "registros\n")
cat("  Columnas:", ncol(data_warehouse), "\n")

# ============================================================================
# 6. VALIDACIÓN DE CALIDAD DE DATOS
# ============================================================================

cat("\n==== FASE 4: VALIDACIÓN DE CALIDAD ====\n")

# Verificar NAs por columna
nas_por_columna <- data_warehouse %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "columna", values_to = "nas") %>%
  filter(nas > 0) %>%
  arrange(desc(nas))

if (nrow(nas_por_columna) > 0) {
  cat("⚠ Columnas con valores faltantes:\n")
  print(nas_por_columna)
} else {
  cat("✓ No hay valores faltantes\n")
}

# Verificar valores negativos en campos críticos
validaciones <- list(
  cantidad_negativa = sum(data_warehouse$cantidad < 0, na.rm = TRUE),
  precio_negativo = sum(data_warehouse$precio_unitario < 0, na.rm = TRUE),
  total_negativo = sum(data_warehouse$total < 0, na.rm = TRUE),
  margen_invalido = sum(data_warehouse$margen_real < -100 | data_warehouse$margen_real > 100, na.rm = TRUE)
)

cat("\nValidaciones:\n")
for (nombre in names(validaciones)) {
  if (validaciones[[nombre]] > 0) {
    cat("  ⚠", nombre, ":", validaciones[[nombre]], "registros\n")
  } else {
    cat("  ✓", nombre, ": OK\n")
  }
}

# ============================================================================
# 7. CREAR TABLAS AGREGADAS PARA ANÁLISIS
# ============================================================================

cat("\n==== FASE 5: CREACIÓN DE TABLAS AGREGADAS ====\n")

# Ventas por mes
ventas_por_mes <- data_warehouse %>%
  group_by(año, mes, mes_nombre) %>%
  summarise(
    ingresos_totales = sum(total, na.rm = TRUE),
    unidades_vendidas = sum(cantidad, na.rm = TRUE),
    num_transacciones = n(),
    ticket_promedio = mean(total, na.rm = TRUE),
    clientes_unicos = n_distinct(cliente_id),
    productos_unicos = n_distinct(producto_id),
    .groups = "drop"
  ) %>%
  arrange(año, mes)

# Ventas por categoría
ventas_por_categoria <- data_warehouse %>%
  group_by(categoria) %>%
  summarise(
    ingresos_totales = sum(total, na.rm = TRUE),
    ganancia_bruta = sum(ganancia_bruta, na.rm = TRUE),
    unidades_vendidas = sum(cantidad, na.rm = TRUE),
    num_transacciones = n(),
    margen_promedio = mean(margen_real, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(ingresos_totales))

# Ventas por tienda
ventas_por_tienda <- data_warehouse %>%
  group_by(tienda_id, nombre_tienda, ciudad_tienda, zona, tipo) %>%
  summarise(
    ingresos_totales = sum(total, na.rm = TRUE),
    unidades_vendidas = sum(cantidad, na.rm = TRUE),
    num_transacciones = n(),
    ticket_promedio = mean(total, na.rm = TRUE),
    clientes_unicos = n_distinct(cliente_id),
    .groups = "drop"
  ) %>%
  arrange(desc(ingresos_totales))

# Performance de vendedores
ventas_por_vendedor <- data_warehouse %>%
  filter(!is.na(nombre_vendedor)) %>%
  group_by(vendedor_id, nombre_vendedor, departamento) %>%
  summarise(
    ingresos_totales = sum(total, na.rm = TRUE),
    unidades_vendidas = sum(cantidad, na.rm = TRUE),
    num_transacciones = n(),
    ticket_promedio = mean(total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(ingresos_totales))

# Análisis de clientes
analisis_clientes <- data_warehouse %>%
  group_by(cliente_id, nombre, ciudad, segmento, es_premium) %>%
  summarise(
    total_gastado = sum(total, na.rm = TRUE),
    num_compras = n(),
    ticket_promedio = mean(total, na.rm = TRUE),
    productos_diferentes = n_distinct(producto_id),
    ultima_compra = max(fecha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    dias_desde_ultima_compra = as.numeric(fecha_corte - ultima_compra),
    categoria_cliente = case_when(
      total_gastado > 50000 ~ "VIP",
      total_gastado > 20000 ~ "Oro",
      total_gastado > 5000 ~ "Plata",
      TRUE ~ "Bronce"
    )
  ) %>%
  arrange(desc(total_gastado))

cat("✓ Tablas agregadas creadas:\n")
cat("  - ventas_por_mes:", nrow(ventas_por_mes), "filas\n")
cat("  - ventas_por_categoria:", nrow(ventas_por_categoria), "filas\n")
cat("  - ventas_por_tienda:", nrow(ventas_por_tienda), "filas\n")
cat("  - ventas_por_vendedor:", nrow(ventas_por_vendedor), "filas\n")
cat("  - analisis_clientes:", nrow(analisis_clientes), "filas\n")

# ============================================================================
# 8. EXPORTAR DATOS PROCESADOS
# ============================================================================

cat("\n==== FASE 6: EXPORTACIÓN ====\n")

# Guardar dataset maestro
write_csv(data_warehouse, "datos/processed/data_warehouse.csv")
saveRDS(data_warehouse, "datos/processed/data_warehouse.rds")

# Guardar tablas agregadas
write_csv(ventas_por_mes, "datos/processed/ventas_por_mes.csv")
write_csv(ventas_por_categoria, "datos/processed/ventas_por_categoria.csv")
write_csv(ventas_por_tienda, "datos/processed/ventas_por_tienda.csv")
write_csv(ventas_por_vendedor, "datos/processed/ventas_por_vendedor.csv")
write_csv(analisis_clientes, "datos/processed/analisis_clientes.csv")

# Guardar dimensiones limpias
write_csv(clientes_clean, "datos/processed/dim_clientes.csv")
write_csv(productos_clean, "datos/processed/dim_productos.csv")
write_csv(empleados_clean, "datos/processed/dim_empleados.csv")
write_csv(tiendas_clean, "datos/processed/dim_tiendas.csv")

cat("✓ Datos exportados a 'datos/processed/'\n")

# ============================================================================
# 9. RESUMEN EJECUTIVO
# ============================================================================

cat("\n")
cat("==================================================\n")
cat("           RESUMEN EJECUTIVO - ETL\n")
cat("==================================================\n\n")

cat("PERIODO ANALIZADO:\n")
cat("  Desde:", min(data_warehouse$fecha), "\n")
cat("  Hasta:", max(data_warehouse$fecha), "\n\n")

cat("VOLUMEN DE DATOS:\n")
cat("  Total de transacciones:", nrow(data_warehouse) %>% comma(), "\n")
cat("  Clientes únicos:", n_distinct(data_warehouse$cliente_id) %>% comma(), "\n")
cat("  Productos únicos:", n_distinct(data_warehouse$producto_id), "\n")
cat("  Tiendas:", n_distinct(data_warehouse$tienda_id), "\n\n")

cat("MÉTRICAS GLOBALES:\n")
cat("  Ingresos totales: $", sum(data_warehouse$total, na.rm = TRUE) %>% comma(), "\n")
cat("  Unidades vendidas:", sum(data_warehouse$cantidad, na.rm = TRUE) %>% comma(), "\n")
cat("  Ticket promedio: $", mean(data_warehouse$total, na.rm = TRUE) %>% round(2) %>% comma(), "\n")
cat("  Margen promedio:", mean(data_warehouse$margen_real, na.rm = TRUE) %>% round(2), "%\n\n")

cat("TOP 3 CATEGORÍAS POR INGRESOS:\n")
top_categorias <- ventas_por_categoria %>% slice(1:3)
for (i in 1:nrow(top_categorias)) {
  cat("  ", i, ".", top_categorias$categoria[i], "- $",
     comma(top_categorias$ingresos_totales[i]), "\n")
}

cat("\nTOP 3 TIENDAS POR INGRESOS:\n")
top_tiendas <- ventas_por_tienda %>% slice(1:3)
for (i in 1:nrow(top_tiendas)) {
  cat("  ", i, ".", top_tiendas$nombre_tienda[i], "- $",
     comma(top_tiendas$ingresos_totales[i]), "\n")
}

cat("\n==================================================\n")
cat("✓ PROCESO ETL COMPLETADO EXITOSAMENTE\n")
cat("==================================================\n")

# ============================================================================
# SIGUIENTE PASO: ANÁLISIS EXPLORATORIO
# ============================================================================

cat("\nPróximo paso: Ejecutar 02_analisis.R para análisis exploratorio\n")
