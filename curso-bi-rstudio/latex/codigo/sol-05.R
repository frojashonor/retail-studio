# ==========================================================================
# sol-05.tex
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/sol-05.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Bloque 1 --------------------------------------------------------
library(tidyverse)
library(DBI)
library(RSQLite)
library(dbplyr)

# ---- Bloque 2 --------------------------------------------------------
con <- dbConnect(RSQLite::SQLite(), ":memory:")

productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)
dbWriteTable(con, "productos", productos)

dbListTables(con)
dbListFields(con, "productos")

dbGetQuery(con, "
  SELECT categoria,
         COUNT(*)                      AS productos,
         ROUND(AVG(precio_catalogo), 2) AS precio_promedio
  FROM productos
  GROUP BY categoria
  ORDER BY productos DESC, categoria
")

dbDisconnect(con)

# ---- Bloque 3 --------------------------------------------------------
con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(con, "productos",
             read_csv("datasets/productos.csv", show_col_types = FALSE))

alerta <- dbGetQuery(con, "
  SELECT nombre_producto, categoria, proveedor, stock_actual,
         stock_minimo, stock_minimo - stock_actual AS faltante
  FROM productos
  WHERE activo = 1                       -- TRUE se guardó como 1
    AND stock_actual < stock_minimo
  ORDER BY faltante DESC
")
alerta

# ¿Qué proveedor aparece más?
dbGetQuery(con, "
  SELECT proveedor, COUNT(*) AS productos_en_alerta
  FROM productos
  WHERE activo = 1 AND stock_actual < stock_minimo
  GROUP BY proveedor
  ORDER BY productos_en_alerta DESC
")
dbDisconnect(con)

# ---- Bloque 4 --------------------------------------------------------
con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(con, "empleados",
             read_csv("datasets/empleados.csv", show_col_types = FALSE) %>%
               mutate(fecha_ingreso = format(fecha_ingreso, "%Y-%m-%d")))

dbGetQuery(con, "
  SELECT departamento,
         COUNT(*)                       AS empleados,
         ROUND(AVG(salario_mensual), 2) AS salario_promedio,
         MAX(salario_mensual)           AS salario_maximo
  FROM empleados
  GROUP BY departamento
  HAVING COUNT(*) >= 15
  ORDER BY salario_promedio DESC
")
dbDisconnect(con)

# ---- Bloque 5 --------------------------------------------------------
con <- dbConnect(RSQLite::SQLite(), ":memory:")
leer_para_bd <- function(archivo) {
  read_csv(file.path("datasets", archivo), show_col_types = FALSE) %>%
    mutate(across(where(is.Date), ~ format(.x, "%Y-%m-%d")),
           across(where(hms::is_hms), as.character))
}
for (tabla in c("transacciones", "tiendas", "productos")) {
  dbWriteTable(con, tabla, leer_para_bd(paste0(tabla, ".csv")))
}

# (a) Ventas por ciudad de la tienda
dbGetQuery(con, "
  SELECT ti.ciudad,
         COUNT(*)                           AS tickets,
         ROUND(SUM(t.total_transaccion), 0) AS ventas,
         ROUND(AVG(t.total_transaccion), 2) AS ticket_promedio
  FROM transacciones AS t
  INNER JOIN tiendas AS ti ON t.tienda_id = ti.tienda_id
  GROUP BY ti.ciudad
  ORDER BY ventas DESC
")

# (b) Productos sin ventas en diciembre
dbGetQuery(con, "
  SELECT p.producto_id, p.nombre_producto, p.categoria
  FROM productos AS p
  LEFT JOIN transacciones AS t
         ON p.producto_id = t.producto_id
        AND t.fecha BETWEEN '2023-12-01' AND '2023-12-31'
  WHERE t.transaccion_id IS NULL
  ORDER BY p.producto_id
")
dbDisconnect(con)

# ---- Bloque 6 --------------------------------------------------------
con <- dbConnect(RSQLite::SQLite(), ":memory:")
ventas_r <- read_csv("datasets/ventas_retail.csv", show_col_types = FALSE)

dbWriteTable(con, "ventas_mal", ventas_r)
dbWriteTable(con, "ventas",
             ventas_r %>% mutate(fecha = format(fecha, "%Y-%m-%d")))

dbGetQuery(con, "SELECT fecha, typeof(fecha) AS tipo,
                        strftime('%m', fecha) AS mes
                 FROM ventas_mal LIMIT 2")
dbGetQuery(con, "SELECT fecha, typeof(fecha) AS tipo,
                        strftime('%m', fecha) AS mes
                 FROM ventas LIMIT 2")

dbGetQuery(con, "
  SELECT (CAST(strftime('%m', fecha) AS INTEGER) + 2) / 3 AS trimestre,
         ROUND(SUM(total), 2)         AS ventas,
         ROUND(AVG(descuento_pct), 2) AS descuento_prom,
         SUM(descuento_pct > 0)       AS dias_con_descuento
  FROM ventas
  GROUP BY trimestre
  ORDER BY trimestre
")
dbDisconnect(con)

# ---- Bloque 7 --------------------------------------------------------
con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(con, "transacciones",
             read_csv("datasets/transacciones.csv",
                      show_col_types = FALSE) %>%
               mutate(fecha = format(fecha, "%Y-%m-%d"),
                      hora = as.character(hora)))

ventas_cliente <- function(con, cliente, desde, hasta) {
  dbGetQuery(con, "
    SELECT COUNT(*)                         AS compras,
           ROUND(SUM(total_transaccion), 2) AS gasto
    FROM transacciones
    WHERE cliente_id = :cliente
      AND fecha BETWEEN :desde AND :hasta",
    params = list(cliente = cliente,
                  desde   = format(as.Date(desde)),
                  hasta   = format(as.Date(hasta))))
}

ventas_cliente(con, 5, "2023-01-01", "2023-06-30")
ventas_cliente(con, 5, "2023-07-01", "2023-12-31")

# Intento de inyección: se busca un cliente_id igual al texto
ventas_cliente(con, "5 OR 1=1", "2023-01-01", "2023-12-31")

# Lo que habría pasado con paste0() (¡no lo hagas!)
dbGetQuery(con, paste0("SELECT COUNT(*) AS compras FROM transacciones ",
                       "WHERE cliente_id = ", "5 OR 1=1"))
dbDisconnect(con)

# ---- Bloque 8 --------------------------------------------------------
library(dbplyr)
con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(con, "transacciones",
             read_csv("datasets/transacciones.csv",
                      show_col_types = FALSE) %>%
               mutate(fecha = format(fecha, "%Y-%m-%d"),
                      hora = as.character(hora)))

consulta <- tbl(con, "transacciones") %>%
  group_by(metodo_pago, trimestre) %>%
  summarise(tickets = n(),
            ventas  = sum(total_transaccion, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(metodo_pago, trimestre)

show_query(consulta)
resultado_dbplyr <- collect(consulta)
head(resultado_dbplyr, 4)

resultado_sql <- dbGetQuery(con, "
  SELECT metodo_pago, trimestre, COUNT(*) AS tickets,
         SUM(total_transaccion) AS ventas
  FROM transacciones
  GROUP BY metodo_pago, trimestre
  ORDER BY metodo_pago, trimestre")

all.equal(as.data.frame(resultado_dbplyr), resultado_sql)
dbDisconnect(con)

# ---- Bloque 9 --------------------------------------------------------
con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(con, "clientes",
             read_csv("datasets/clientes.csv", show_col_types = FALSE) %>%
               mutate(fecha_registro = format(fecha_registro)))
dbWriteTable(con, "transacciones",
             read_csv("datasets/transacciones.csv",
                      show_col_types = FALSE) %>%
               mutate(fecha = format(fecha), hora = as.character(hora)))

# (a) Los dos mejores clientes de cada ciudad
dbGetQuery(con, "
  WITH gasto AS (
    SELECT c.ciudad, c.nombre, SUM(t.total_transaccion) AS gasto
    FROM transacciones AS t
    JOIN clientes AS c ON t.cliente_id = c.cliente_id
    GROUP BY c.cliente_id
  ),
  ranking AS (
    SELECT ciudad, nombre, ROUND(gasto, 0) AS gasto,
           RANK() OVER (PARTITION BY ciudad ORDER BY gasto DESC) AS lugar
    FROM gasto
  )
  SELECT * FROM ranking
  WHERE lugar <= 2 AND ciudad IN ('Monterrey', 'Guadalajara', 'CDMX')
  ORDER BY ciudad, lugar
")

# (b) Curva de Pareto de productos
pareto <- dbGetQuery(con, "
  WITH ventas_producto AS (
    SELECT producto_id, SUM(total_transaccion) AS ventas
    FROM transacciones
    GROUP BY producto_id
  )
  SELECT producto_id, ROUND(ventas, 0) AS ventas,
         ROW_NUMBER() OVER (ORDER BY ventas DESC) AS posicion,
         ROUND(100.0 * SUM(ventas) OVER (ORDER BY ventas DESC) /
               SUM(ventas) OVER (), 1) AS pct_acumulado
  FROM ventas_producto
  ORDER BY ventas DESC
")
head(pareto, 3)
pareto %>% filter(pct_acumulado >= 80) %>% head(1)
dbDisconnect(con)

# ---- Bloque 10 --------------------------------------------------------
con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbExecute(con, "PRAGMA foreign_keys = ON")
dbExecute(con, "CREATE TABLE dim_producto (
  producto_id INTEGER PRIMARY KEY, nombre_producto TEXT, categoria TEXT)")
dbExecute(con, "CREATE TABLE dim_tienda (
  tienda_id INTEGER PRIMARY KEY, nombre_tienda TEXT, tipo TEXT)")
dbExecute(con, "CREATE TABLE fact_ventas_diarias (
  fecha TEXT PRIMARY KEY,
  producto_id INTEGER NOT NULL REFERENCES dim_producto(producto_id),
  tienda_id   INTEGER NOT NULL REFERENCES dim_tienda(tienda_id),
  cantidad    INTEGER CHECK (cantidad > 0),
  total REAL)")

dbAppendTable(con, "dim_producto",
  read_csv("datasets/productos.csv", show_col_types = FALSE) %>%
    transmute(producto_id = as.integer(producto_id), nombre_producto,
              categoria))
dbAppendTable(con, "dim_tienda",
  read_csv("datasets/tiendas.csv", show_col_types = FALSE) %>%
    transmute(tienda_id = as.integer(tienda_id), nombre_tienda, tipo))
dbAppendTable(con, "fact_ventas_diarias",
  read_csv("datasets/ventas_retail.csv", show_col_types = FALSE) %>%
    transmute(fecha = format(fecha, "%Y-%m-%d"),
              producto_id = as.integer(producto_id),
              tienda_id = as.integer(tienda_id),
              cantidad = as.integer(cantidad), total))

dbExecute(con, "CREATE VIEW vw_trimestre_tipo AS
  SELECT (CAST(strftime('%m', f.fecha) AS INTEGER) + 2) / 3 AS trimestre,
         t.tipo, COUNT(*) AS dias, ROUND(SUM(f.total), 0) AS ventas
  FROM fact_ventas_diarias AS f
  JOIN dim_tienda AS t ON f.tienda_id = t.tienda_id
  GROUP BY trimestre, t.tipo")
dbGetQuery(con, "SELECT * FROM vw_trimestre_tipo
                 WHERE trimestre = 1 ORDER BY ventas DESC")

consulta <- "SELECT SUM(total) FROM fact_ventas_diarias WHERE tienda_id = 3"
dbGetQuery(con, paste("EXPLAIN QUERY PLAN", consulta))$detail
dbExecute(con, "CREATE INDEX idx_tienda ON fact_ventas_diarias(tienda_id)")
dbGetQuery(con, paste("EXPLAIN QUERY PLAN", consulta))$detail
dbDisconnect(con)

# ---- Bloque 11 --------------------------------------------------------
con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbExecute(con, "CREATE TABLE fact_ventas_diarias (
  fecha TEXT PRIMARY KEY, producto_id INTEGER, cliente_id INTEGER,
  tienda_id INTEGER, cantidad INTEGER CHECK (cantidad > 0),
  precio_unitario REAL, descuento_pct REAL, total REAL)")
dbExecute(con, "CREATE TABLE log_etl (
  id INTEGER PRIMARY KEY AUTOINCREMENT, fecha_hora TEXT, lote TEXT,
  leidas INTEGER, nuevas INTEGER, estado TEXT, mensaje TEXT)")

extraer_ventas <- function(ruta) read_csv(ruta, show_col_types = FALSE)

transformar_ventas <- function(datos) {
  datos %>%
    distinct(fecha, .keep_all = TRUE) %>%
    transmute(fecha = format(as.Date(fecha), "%Y-%m-%d"),
              producto_id = as.integer(producto_id),
              cliente_id = as.integer(cliente_id),
              tienda_id = as.integer(tienda_id),
              cantidad = as.integer(cantidad), precio_unitario,
              descuento_pct, total)
}

cargar_ventas <- function(con, datos) {
  dbWriteTable(con, "stg", datos, temporary = TRUE, overwrite = TRUE)
  # Solo las fechas que aún no existen (sin OR IGNORE: ver explicación)
  n <- dbExecute(con, "INSERT INTO fact_ventas_diarias
                       SELECT * FROM stg
                       WHERE fecha NOT IN (SELECT fecha
                                           FROM fact_ventas_diarias)")
  dbExecute(con, "DROP TABLE stg")
  n
}

ejecutar_etl <- function(con, ruta) {
  leidas <- NA_integer_
  en_transaccion <- FALSE
  registrar <- function(nuevas, estado, mensaje = NA_character_) {
    dbAppendTable(con, "log_etl", tibble(
      fecha_hora = format(Sys.time()), lote = basename(ruta),
      leidas = leidas, nuevas = nuevas, estado = estado,
      mensaje = mensaje))
  }
  tryCatch({
    datos <- extraer_ventas(ruta)
    leidas <- nrow(datos)
    limpios <- transformar_ventas(datos)
    dbBegin(con); en_transaccion <- TRUE
    nuevas <- cargar_ventas(con, limpios)
    registrar(nuevas, "OK")
    dbCommit(con)
  }, error = function(e) {
    if (en_transaccion) dbRollback(con)
    registrar(0L, "ERROR", conditionMessage(e))
  })
  invisible(NULL)
}

# Simular los lotes en una carpeta temporal
carpeta <- file.path(tempdir(), "lotes")
dir.create(carpeta, showWarnings = FALSE)
ventas <- read_csv("datasets/ventas_retail.csv", show_col_types = FALSE)
lote <- function(desde, hasta, nombre) {
  ruta <- file.path(carpeta, nombre)
  ventas %>% filter(fecha >= as.Date(desde), fecha <= as.Date(hasta)) %>%
    write_csv(ruta)
  ruta
}
l1 <- lote("2023-01-01", "2023-06-30", "lote_ene_jun.csv")
l2 <- lote("2023-04-01", "2023-09-30", "lote_abr_sep.csv")
l3 <- lote("2023-10-01", "2023-12-31", "lote_oct_dic.csv")
l_malo <- file.path(carpeta, "lote_malo.csv")
tibble(fecha = as.Date("2024-01-01"), producto_id = 1, cliente_id = 1,
       tienda_id = 1, cantidad = -2, precio_unitario = 100,
       descuento_pct = 0, total = -200) %>% write_csv(l_malo)

for (ruta in c(l1, l2, l3, l3, l_malo)) ejecutar_etl(con, ruta)

dbGetQuery(con, "SELECT id, lote, leidas, nuevas, estado FROM log_etl")
dbGetQuery(con, "SELECT mensaje FROM log_etl WHERE estado = 'ERROR'")

# Validación final contra el CSV
dbGetQuery(con, "SELECT COUNT(*) AS filas, ROUND(SUM(total), 2) AS total
                 FROM fact_ventas_diarias")
round(sum(ventas$total), 2)
dbDisconnect(con)
