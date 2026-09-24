# ==========================================================================
# Bases de datos, SQL y ETL desde R
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/05-bases-datos.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Solo referencia (no se ejecuta automáticamente) ----
# install.packages(c("DBI", "RSQLite", "dbplyr"))

# ---- Bloque 1 --------------------------------------------------------
library(tidyverse)
library(DBI)        # funciones comunes para cualquier base de datos
library(RSQLite)    # driver (controlador) de SQLite
library(dbplyr)     # traduce verbos de dplyr a SQL
library(lubridate)  # fechas

# ---- Bloque 2 --------------------------------------------------------
# Carpeta donde vivirá la base de datos
dir.create("datos", showWarnings = FALSE)
ruta_bd <- "datos/bi_retail.sqlite"

# Si la base ya existe (de una ejecución anterior), la borramos para
# que el script sea reproducible y siempre empiece desde cero.
# unlink() no marca error si el archivo no existe.
unlink(ruta_bd)
file.exists(ruta_bd)

# ---- Bloque 3 --------------------------------------------------------
# Abrir (y crear) la base de datos en el archivo
con <- dbConnect(RSQLite::SQLite(), ruta_bd)

# ¿Qué tablas tiene? Ninguna todavía
dbListTables(con)
file.exists(ruta_bd)

# ---- Bloque 4 --------------------------------------------------------
# Lee un CSV del curso y deja fechas y horas como texto ISO
leer_para_bd <- function(archivo) {
  read_csv(file.path("datasets", archivo), show_col_types = FALSE) %>%
    mutate(
      across(where(is.Date), ~ format(.x, "%Y-%m-%d")),  # fechas
      across(where(hms::is_hms), as.character)          # horas
    )
}

# Nombre de la tabla en la base = nombre del archivo sin ".csv"
archivos <- c("ventas_retail", "clientes", "productos",
              "empleados", "transacciones", "tiendas")

for (tabla in archivos) {
  datos <- leer_para_bd(paste0(tabla, ".csv"))
  # overwrite = TRUE: si la tabla existe, la reemplaza
  dbWriteTable(con, tabla, datos, overwrite = TRUE)
  cat(sprintf("%-14s %5d filas cargadas\n", tabla, nrow(datos)))
}

dbListTables(con)

# ---- Bloque 5 --------------------------------------------------------
# Columnas de la tabla de tiendas
dbListFields(con, "tiendas")

# Tipo de dato de cada columna (instrucción propia de SQLite)
dbGetQuery(con, "PRAGMA table_info(tiendas)") %>%
  select(name, type)

# ---- Bloque 6 --------------------------------------------------------
# Una consulta: ¿cuántas transacciones hay?
dbGetQuery(con, "SELECT COUNT(*) AS num_transacciones
                 FROM transacciones")

# Una modificación: crear una tabla de metas mensuales y llenarla
dbExecute(con, "CREATE TABLE metas (mes TEXT, meta REAL)")
dbExecute(con, "INSERT INTO metas VALUES ('2023-01', 80000),
                                         ('2023-02', 75000)")
dbGetQuery(con, "SELECT * FROM metas")

# Borrar la tabla de prueba
dbExecute(con, "DROP TABLE metas")

# ---- Bloque 7 --------------------------------------------------------
# Función que abre su propia conexión y SIEMPRE la cierra
contar_filas <- function(ruta, tabla) {
  con_local <- dbConnect(RSQLite::SQLite(), ruta)
  on.exit(dbDisconnect(con_local))   # se ejecuta al salir, pase lo que pase
  sql <- paste("SELECT COUNT(*) AS n FROM", tabla)
  dbGetQuery(con_local, sql)$n
}

contar_filas(ruta_bd, "clientes")
contar_filas(ruta_bd, "productos")

# ---- Bloque 8 --------------------------------------------------------
dbGetQuery(con, "
  SELECT nombre_tienda, ciudad, zona   -- columnas que quiero
  FROM tiendas                         -- tabla de origen
")

# ---- Bloque 9 --------------------------------------------------------
# IN (...) equivale a varias condiciones unidas con OR
dbGetQuery(con, "
  SELECT COUNT(*) AS clientes
  FROM clientes
  WHERE es_premium = 1
    AND ciudad IN ('Monterrey', 'Guadalajara')
")

dbGetQuery(con, "
  SELECT nombre, ciudad, edad
  FROM clientes
  WHERE es_premium = 1
    AND ciudad IN ('Monterrey', 'Guadalajara')
  ORDER BY ciudad, nombre
  LIMIT 3
")

# ---- Bloque 10 --------------------------------------------------------
# BETWEEN incluye ambos extremos; las fechas son texto ISO
dbGetQuery(con, "
  SELECT transaccion_id, fecha, producto_id, total_transaccion
  FROM transacciones
  WHERE fecha BETWEEN '2023-12-22' AND '2023-12-24'
  ORDER BY fecha
")

dbGetQuery(con, "
  SELECT COUNT(*) AS tickets_grandes
  FROM transacciones
  WHERE total_transaccion > 2800
")

# ---- Bloque 11 --------------------------------------------------------
# Clientes cuyo apellido es García y cuyo nombre empieza con "Laura"
dbGetQuery(con, "
  SELECT
    SUM(nombre LIKE '%García') AS apellido_garcia,
    SUM(nombre LIKE 'Laura%')  AS nombre_laura
  FROM clientes
")

# ---- Bloque 12 --------------------------------------------------------
dbGetQuery(con, "
  SELECT transaccion_id, fecha, cantidad, precio_venta,
         total_transaccion
  FROM transacciones
  ORDER BY total_transaccion DESC   -- DESC = de mayor a menor
  LIMIT 5
")

# ---- Bloque 13 --------------------------------------------------------
dbGetQuery(con, "
  SELECT COUNT(*)                         AS tickets,
         COUNT(DISTINCT cliente_id)       AS clientes,
         ROUND(SUM(total_transaccion), 2) AS ventas,
         ROUND(AVG(total_transaccion), 2) AS ticket_promedio,
         MAX(total_transaccion)           AS ticket_maximo
  FROM transacciones
")

# ---- Bloque 14 --------------------------------------------------------
dbGetQuery(con, "
  SELECT metodo_pago,
         COUNT(*)                         AS tickets,
         ROUND(SUM(total_transaccion), 0) AS ventas,
         ROUND(AVG(total_transaccion), 2) AS ticket_promedio
  FROM transacciones
  GROUP BY metodo_pago
  ORDER BY ventas DESC
")

# ---- Bloque 15 --------------------------------------------------------
dbGetQuery(con, "
  SELECT cliente_id,
         COUNT(*)                         AS compras,
         ROUND(SUM(total_transaccion), 2) AS gasto_total
  FROM transacciones
  GROUP BY cliente_id
  HAVING COUNT(*) >= 10      -- condición sobre el grupo
  ORDER BY compras DESC, gasto_total DESC
")

# ---- Bloque 16 --------------------------------------------------------
dbGetQuery(con, "
  SELECT CASE
           WHEN total_transaccion < 500  THEN '1. Menos de 500'
           WHEN total_transaccion < 1500 THEN '2. De 500 a 1,500'
           ELSE '3. 1,500 o más'
         END AS rango_ticket,
         COUNT(*) AS tickets,
         ROUND(SUM(total_transaccion), 0) AS ventas
  FROM transacciones
  GROUP BY rango_ticket
  ORDER BY rango_ticket
")

# ---- Bloque 17 --------------------------------------------------------
# Base temporal para experimentar (no toca bi_retail.sqlite)
con_prueba <- dbConnect(RSQLite::SQLite(), ":memory:")

tiendas_r <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)
class(tiendas_r$fecha_apertura)       # en R es una fecha

dbWriteTable(con_prueba, "tiendas", tiendas_r)
dbGetQuery(con_prueba, "
  SELECT nombre_tienda, fecha_apertura,
         typeof(fecha_apertura)             AS tipo,
         strftime('%Y', fecha_apertura)     AS anio
  FROM tiendas
  LIMIT 3
")

# ---- Bloque 18 --------------------------------------------------------
tiendas_ok <- tiendas_r %>%
  mutate(fecha_apertura = format(fecha_apertura, "%Y-%m-%d"))

dbWriteTable(con_prueba, "tiendas", tiendas_ok, overwrite = TRUE)
dbGetQuery(con_prueba, "
  SELECT nombre_tienda, fecha_apertura,
         typeof(fecha_apertura)             AS tipo,
         strftime('%Y', fecha_apertura)     AS anio
  FROM tiendas
  LIMIT 3
")

# De regreso en R, la columna es texto: la convertimos con as.Date()
dbReadTable(con_prueba, "tiendas") %>%
  mutate(fecha_apertura = as.Date(fecha_apertura)) %>%
  pull(fecha_apertura) %>%
  class()

dbDisconnect(con_prueba)

# ---- Bloque 19 --------------------------------------------------------
ventas_mes_sql <- dbGetQuery(con, "
  SELECT strftime('%Y-%m', fecha)         AS mes,
         COUNT(*)                         AS tickets,
         ROUND(SUM(total_transaccion), 2) AS ventas
  FROM transacciones
  GROUP BY mes
  ORDER BY mes
")
ventas_mes_sql

# ---- Bloque 20 --------------------------------------------------------
dbGetQuery(con, "
  SELECT p.categoria,
         COUNT(*)                           AS tickets,
         SUM(t.cantidad)                    AS unidades,
         ROUND(SUM(t.total_transaccion), 0) AS ventas
  FROM transacciones AS t
  INNER JOIN productos AS p
          ON t.producto_id = p.producto_id  -- llave de unión
  GROUP BY p.categoria
  ORDER BY ventas DESC
")

# ---- Bloque 21 --------------------------------------------------------
dbGetQuery(con, "
  SELECT c.cliente_id, c.nombre, c.ciudad, c.fecha_registro
  FROM clientes AS c
  LEFT JOIN transacciones AS t
         ON c.cliente_id = t.cliente_id
  WHERE t.transaccion_id IS NULL   -- sin pareja en transacciones
")

# ---- Bloque 22 --------------------------------------------------------
dbGetQuery(con, "
  SELECT nombre_producto, categoria, precio_catalogo, margen_pct
  FROM productos
  WHERE precio_catalogo > (SELECT AVG(precio_catalogo)
                           FROM productos)
  ORDER BY precio_catalogo DESC
  LIMIT 5
")

# ---- Bloque 23 --------------------------------------------------------
# Paso 1 (subconsulta): gasto total de cada cliente
# Paso 2: estadísticas sobre esos totales
dbGetQuery(con, "
  SELECT COUNT(*)                   AS clientes,
         ROUND(AVG(gasto), 2)       AS gasto_promedio,
         ROUND(MAX(gasto), 2)       AS gasto_maximo,
         SUM(gasto > 8000)          AS clientes_mas_de_8000
  FROM (SELECT cliente_id, SUM(total_transaccion) AS gasto
        FROM transacciones
        GROUP BY cliente_id) AS gasto_cliente
")

# ---- Bloque 24 --------------------------------------------------------
dbGetQuery(con, "
  WITH ventas_tienda AS (          -- paso 1: ventas por tienda
    SELECT tienda_id, SUM(total_transaccion) AS ventas
    FROM transacciones
    GROUP BY tienda_id
  ),
  total AS (                       -- paso 2: total de la empresa
    SELECT SUM(ventas) AS ventas_total FROM ventas_tienda
  )
  SELECT ti.nombre_tienda,         -- paso 3: combinar y calcular %
         ROUND(v.ventas, 0) AS ventas,
         ROUND(100.0 * v.ventas / total.ventas_total, 1) AS pct
  FROM ventas_tienda AS v
  INNER JOIN tiendas AS ti ON v.tienda_id = ti.tienda_id
  CROSS JOIN total
  ORDER BY v.ventas DESC
")

# ---- Bloque 25 --------------------------------------------------------
dbGetQuery(con, "
  WITH ventas_producto AS (
    SELECT p.categoria, p.nombre_producto,
           SUM(t.total_transaccion) AS ventas
    FROM transacciones AS t
    INNER JOIN productos AS p ON t.producto_id = p.producto_id
    GROUP BY p.categoria, p.nombre_producto
  ),
  ranking AS (
    SELECT categoria, nombre_producto, ROUND(ventas, 0) AS ventas,
           ROW_NUMBER() OVER (PARTITION BY categoria
                              ORDER BY ventas DESC) AS posicion
    FROM ventas_producto
  )
  SELECT * FROM ranking
  WHERE posicion = 1
  ORDER BY ventas DESC
")

# ---- Bloque 26 --------------------------------------------------------
dbGetQuery(con, "
  WITH mensual AS (
    SELECT strftime('%Y-%m', fecha) AS mes,
           SUM(total_transaccion)   AS ventas
    FROM transacciones
    GROUP BY mes
  )
  SELECT mes,
         ROUND(ventas, 0)                           AS ventas,
         ROUND(SUM(ventas) OVER (ORDER BY mes), 0)  AS acumulado,
         ROUND(LAG(ventas) OVER (ORDER BY mes), 0)  AS mes_anterior,
         ROUND(100.0 * (ventas - LAG(ventas) OVER (ORDER BY mes)) /
               LAG(ventas) OVER (ORDER BY mes), 1)  AS crec_pct
  FROM mensual
  ORDER BY mes
")

# ---- Bloque 27 --------------------------------------------------------
dbGetQuery(con, "
  SELECT tienda_id,
         COUNT(*)                                   AS tickets,
         RANK()       OVER (ORDER BY COUNT(*) DESC) AS rank,
         DENSE_RANK() OVER (ORDER BY COUNT(*) DESC) AS dense_rank,
         ROW_NUMBER() OVER (ORDER BY COUNT(*) DESC) AS row_number
  FROM transacciones
  GROUP BY tienda_id
  ORDER BY tickets DESC
")

# ---- Bloque 28 --------------------------------------------------------
# Lo que escribió el usuario en la caja de texto
ciudad_usuario <- "Mérida' OR '1'='1"

# MAL: pegar el texto del usuario dentro del SQL
sql_peligroso <- paste0(
  "SELECT COUNT(*) AS clientes FROM clientes WHERE ciudad = '",
  ciudad_usuario, "'"
)
cat(sql_peligroso, "\n")
dbGetQuery(con, sql_peligroso)

# ---- Bloque 29 --------------------------------------------------------
sql_seguro <- "SELECT COUNT(*) AS clientes FROM clientes WHERE ciudad = ?"

# Con el texto malicioso: busca literalmente esa "ciudad" y no la halla
dbGetQuery(con, sql_seguro, params = list(ciudad_usuario))

# Con un valor legítimo funciona normalmente
dbGetQuery(con, sql_seguro, params = list("Mérida"))

# ---- Bloque 30 --------------------------------------------------------
reporte_pago <- function(con, metodo, desde, hasta) {
  dbGetQuery(con, "
    SELECT metodo_pago,
           COUNT(*)                         AS tickets,
           ROUND(SUM(total_transaccion), 2) AS ventas
    FROM transacciones
    WHERE metodo_pago = :metodo
      AND fecha BETWEEN :desde AND :hasta
    GROUP BY metodo_pago",
    params = list(metodo = metodo,
                  desde  = format(as.Date(desde)),   # fecha como texto
                  hasta  = format(as.Date(hasta))))
}

reporte_pago(con, "Transferencia", "2023-01-01", "2023-03-31")
reporte_pago(con, "Efectivo", "2023-12-01", "2023-12-31")

# ---- Bloque 31 --------------------------------------------------------
library(glue)

ciudades <- c("Monterrey", "Guadalajara", "Tijuana")
edad_min <- 40
sql_glue <- glue_sql("
  SELECT ciudad, COUNT(*) AS clientes
  FROM clientes
  WHERE ciudad IN ({ciudades*}) AND edad >= {edad_min}
  GROUP BY ciudad", .con = con)
sql_glue
dbGetQuery(con, sql_glue)

# El texto malicioso queda "desactivado": sus comillas se duplican
glue_sql("SELECT * FROM clientes WHERE ciudad = {ciudad_usuario}",
         .con = con)
sqlInterpolate(con, "SELECT * FROM clientes WHERE ciudad = ?ciudad",
               ciudad = ciudad_usuario)

# ---- Bloque 32 --------------------------------------------------------
transacciones_bd <- tbl(con, "transacciones")
productos_bd     <- tbl(con, "productos")

transacciones_bd %>%
  select(transaccion_id, fecha, producto_id, total_transaccion) %>%
  head(3)

# ---- Bloque 33 --------------------------------------------------------
consulta_pago <- transacciones_bd %>%
  group_by(metodo_pago) %>%
  summarise(tickets = n(),
            ventas  = round(sum(total_transaccion, na.rm = TRUE), 0)) %>%
  arrange(desc(ventas))

# ¿Qué SQL generó dbplyr?
show_query(consulta_pago)

# ---- Bloque 34 --------------------------------------------------------
resultado_pago <- collect(consulta_pago)
resultado_pago
class(resultado_pago)

# ---- Bloque 35 --------------------------------------------------------
ventas_categoria_dbplyr <- transacciones_bd %>%
  inner_join(productos_bd, by = "producto_id") %>%
  group_by(categoria) %>%
  summarise(tickets = n(),
            ventas  = round(sum(total_transaccion, na.rm = TRUE), 0)) %>%
  arrange(desc(ventas)) %>%
  collect()

head(ventas_categoria_dbplyr, 4)

# ---- Bloque 36 --------------------------------------------------------
ddl_estrella <- c(
  dim_fecha = "
    CREATE TABLE dim_fecha (
      fecha_id       INTEGER PRIMARY KEY,  -- AAAAMMDD, ej. 20230113
      fecha          TEXT NOT NULL UNIQUE, -- texto ISO 'AAAA-MM-DD'
      anio           INTEGER, trimestre INTEGER, mes INTEGER,
      nombre_mes     TEXT, dia INTEGER, dia_semana TEXT,
      num_dia_semana INTEGER,              -- 1 = lunes ... 7 = domingo
      es_fin_semana  INTEGER               -- 1 = sábado o domingo
    )",
  dim_producto = "
    CREATE TABLE dim_producto (
      producto_id INTEGER PRIMARY KEY, nombre_producto TEXT NOT NULL,
      categoria TEXT, subcategoria TEXT, marca TEXT,
      precio_catalogo REAL, costo REAL, proveedor TEXT, activo INTEGER
    )",
  dim_cliente = "
    CREATE TABLE dim_cliente (
      cliente_id INTEGER PRIMARY KEY, nombre TEXT NOT NULL,
      edad INTEGER, genero TEXT, ciudad TEXT, segmento TEXT,
      es_premium INTEGER, fecha_registro TEXT
    )",
  dim_tienda = "
    CREATE TABLE dim_tienda (
      tienda_id INTEGER PRIMARY KEY, nombre_tienda TEXT NOT NULL,
      ciudad TEXT, zona TEXT, tipo TEXT, tamano_m2 REAL,
      fecha_apertura TEXT
    )",
  dim_vendedor = "
    CREATE TABLE dim_vendedor (
      vendedor_id INTEGER PRIMARY KEY, nombre TEXT NOT NULL,
      departamento TEXT, puesto TEXT, sucursal TEXT, fecha_ingreso TEXT
    )",
  fact_ventas = "
    CREATE TABLE fact_ventas (
      venta_id     INTEGER PRIMARY KEY,
      fecha_id     INTEGER NOT NULL REFERENCES dim_fecha(fecha_id),
      cliente_id   INTEGER NOT NULL REFERENCES dim_cliente(cliente_id),
      producto_id  INTEGER NOT NULL REFERENCES dim_producto(producto_id),
      tienda_id    INTEGER NOT NULL REFERENCES dim_tienda(tienda_id),
      vendedor_id  INTEGER NOT NULL REFERENCES dim_vendedor(vendedor_id),
      hora TEXT, metodo_pago TEXT,
      cantidad     INTEGER NOT NULL CHECK (cantidad > 0),
      precio_venta REAL    NOT NULL CHECK (precio_venta > 0),
      total REAL NOT NULL, costo_total REAL, margen REAL
    )",
  log_etl = "
    CREATE TABLE log_etl (
      ejecucion_id  INTEGER PRIMARY KEY AUTOINCREMENT,
      fecha_hora    TEXT, archivo TEXT,
      filas_leidas  INTEGER, filas_validas INTEGER,
      filas_nuevas  INTEGER, estado TEXT, mensaje TEXT
    )"
)

crear_esquema <- function(con) {
  # SQLite trae apagada la revisión de llaves foráneas: la encendemos
  dbExecute(con, "PRAGMA foreign_keys = ON")
  for (sql in ddl_estrella) dbExecute(con, sql)
  invisible(names(ddl_estrella))
}

crear_esquema(con)
dbListTables(con)

# ---- Bloque 37 --------------------------------------------------------
construir_dim_fecha <- function(desde, hasta) {
  tibble(fecha = seq(as.Date(desde), as.Date(hasta), by = "day")) %>%
    mutate(
      fecha_id   = as.integer(format(fecha, "%Y%m%d")),
      anio       = year(fecha),
      trimestre  = quarter(fecha),
      mes        = month(fecha),
      nombre_mes = as.character(month(fecha, label = TRUE, abbr = FALSE)),
      dia        = day(fecha),
      dia_semana = as.character(wday(fecha, label = TRUE, abbr = FALSE,
                                     week_start = 1)),
      num_dia_semana = wday(fecha, week_start = 1),   # 1 = lunes
      es_fin_semana  = as.integer(num_dia_semana >= 6),
      fecha      = format(fecha, "%Y-%m-%d")          # texto ISO
    ) %>%
    relocate(fecha_id)
}

dim_fecha <- construir_dim_fecha("2023-01-01", "2023-12-31")
nrow(dim_fecha)
dim_fecha %>% filter(fecha >= "2023-12-29") %>% select(-dia)

# ---- Bloque 38 --------------------------------------------------------
preparar_dimensiones <- function(carpeta = "datasets") {
  leer <- function(archivo) {
    read_csv(file.path(carpeta, archivo), show_col_types = FALSE)
  }
  empleados     <- leer("empleados.csv")
  transacciones <- leer("transacciones.csv")

  list(
    dim_fecha = construir_dim_fecha("2023-01-01", "2023-12-31"),

    dim_producto = leer("productos.csv") %>%
      transmute(producto_id = as.integer(producto_id), nombre_producto,
                categoria, subcategoria, marca, precio_catalogo, costo,
                proveedor, activo = as.integer(activo)),

    dim_cliente = leer("clientes.csv") %>%       # sin email ni teléfono
      transmute(cliente_id = as.integer(cliente_id), nombre,
                edad = as.integer(edad), genero, ciudad,
                segmento, es_premium = as.integer(es_premium),
                fecha_registro = format(fecha_registro, "%Y-%m-%d")),

    dim_tienda = leer("tiendas.csv") %>%
      transmute(tienda_id = as.integer(tienda_id), nombre_tienda, ciudad,
                zona, tipo, tamano_m2,
                fecha_apertura = format(fecha_apertura, "%Y-%m-%d")),

    # Los vendedores son los empleados que aparecen como vendedor_id
    dim_vendedor = empleados %>%
      semi_join(transacciones, by = c("empleado_id" = "vendedor_id")) %>%
      transmute(vendedor_id = as.integer(empleado_id), nombre,
                departamento, puesto, sucursal,
                fecha_ingreso = format(fecha_ingreso, "%Y-%m-%d"))
  )
}

dimensiones <- preparar_dimensiones()
map_int(dimensiones, nrow)

# ---- Bloque 39 --------------------------------------------------------
cargar_dimensiones <- function(con, dimensiones) {
  dbBegin(con)                          # todo o nada (ver más adelante)
  for (tabla in names(dimensiones)) {
    dbAppendTable(con, tabla, dimensiones[[tabla]])
  }
  dbCommit(con)
  # Devolver cuántas filas quedaron en cada tabla
  map_int(names(dimensiones), function(tabla) {
    dbGetQuery(con, paste("SELECT COUNT(*) AS n FROM", tabla))$n
  }) %>% set_names(names(dimensiones))
}

cargar_dimensiones(con, dimensiones)

# ---- Bloque 40 --------------------------------------------------------
dbGetQuery(con, "
  SELECT departamento, COUNT(*) AS vendedores
  FROM dim_vendedor
  GROUP BY departamento
  ORDER BY vendedores DESC
")

# ---- Bloque 41 --------------------------------------------------------
con_pedidos <- dbConnect(RSQLite::SQLite(), ":memory:")
dbExecute(con_pedidos, "PRAGMA foreign_keys = ON")
dbExecute(con_pedidos, "CREATE TABLE productos (id INTEGER PRIMARY KEY)")
dbExecute(con_pedidos, "INSERT INTO productos VALUES (1), (2)")
dbExecute(con_pedidos, "CREATE TABLE pedidos (pedido_id INTEGER
                          PRIMARY KEY, cliente TEXT)")
dbExecute(con_pedidos, "CREATE TABLE renglones (pedido_id INTEGER,
            producto_id INTEGER REFERENCES productos(id))")

guardar_pedido <- function(con, id, cliente, productos) {
  dbAppendTable(con, "pedidos", tibble(pedido_id = id,
                                       cliente = cliente))
  dbAppendTable(con, "renglones", tibble(pedido_id = id,
                                         producto_id = productos))
}

# 1) SIN transacción: el producto 99 no existe y los renglones fallan
try(guardar_pedido(con_pedidos, 1, "Ana", c(1, 99)), silent = TRUE)
dbGetQuery(con_pedidos, "SELECT * FROM pedidos")   # ¡encabezado huérfano!

# ---- Bloque 42 --------------------------------------------------------
# 2) CON transacción: si algo falla, se deshace todo
dbBegin(con_pedidos)
tryCatch({
  guardar_pedido(con_pedidos, 2, "Luis", c(1, 99))
  dbCommit(con_pedidos)
}, error = function(e) {
  dbRollback(con_pedidos)
  cat("Se canceló el pedido:", conditionMessage(e), "\n")
})
dbGetQuery(con_pedidos, "SELECT * FROM pedidos")   # el 2 no quedó
dbDisconnect(con_pedidos)

# ---- Bloque 43 --------------------------------------------------------
extraer_transacciones <- function(ruta) {
  read_csv(ruta, show_col_types = FALSE,
           col_types = cols(hora = col_character()))
}

# Otra fuente frecuente: una hoja de Excel
extraer_excel <- function(ruta, hoja) {
  readxl::read_excel(ruta, sheet = hoja)
}

# ---- Bloque 44 --------------------------------------------------------
# Carpeta de "archivos recibidos": se vacía para que el ejemplo dé el
# mismo resultado aunque ejecutes el capítulo varias veces
unlink("datos/entrada", recursive = TRUE)
dir.create("datos/entrada", recursive = TRUE, showWarnings = FALSE)
trans_origen <- extraer_transacciones("datasets/transacciones.csv")

trans_origen %>% filter(fecha < as.Date("2023-07-01")) %>%
  write_csv("datos/entrada/ventas_2023_s1.csv")
trans_origen %>% filter(fecha >= as.Date("2023-05-01")) %>%
  write_csv("datos/entrada/ventas_2023_s2.csv")

list.files("datos/entrada")

# ---- Bloque 45 --------------------------------------------------------
transformar_transacciones <- function(datos, costos) {
  datos %>%
    # 1. Calidad: quitar filas sin llave, imposibles o repetidas
    filter(!is.na(transaccion_id), cantidad > 0, precio_venta > 0) %>%
    distinct(transaccion_id, .keep_all = TRUE) %>%
    # 2. Enriquecer con el costo unitario del producto
    left_join(costos, by = "producto_id") %>%
    # 3. Tipos, llaves y métricas con los nombres del data warehouse
    transmute(
      venta_id    = as.integer(transaccion_id),
      fecha_id    = as.integer(format(as.Date(fecha), "%Y%m%d")),
      cliente_id  = as.integer(cliente_id),
      producto_id = as.integer(producto_id),
      tienda_id   = as.integer(tienda_id),
      vendedor_id = as.integer(vendedor_id),
      hora, metodo_pago,
      cantidad    = as.integer(cantidad),
      precio_venta,
      total       = round(cantidad * precio_venta, 2),
      costo_total = round(cantidad * costo, 2),
      margen      = round(total - costo_total, 2)
    )
}

# Prueba rápida con las primeras filas del archivo
costos <- dbGetQuery(con, "SELECT producto_id, costo FROM dim_producto")
trans_origen %>% head(3) %>% transformar_transacciones(costos) %>%
  select(venta_id, fecha_id, cantidad, total, costo_total, margen)

# ---- Bloque 46 --------------------------------------------------------
# Opción 1: evitar duplicados en R con anti_join()
cargar_hechos <- function(con, datos) {
  existentes <- dbGetQuery(con, "SELECT venta_id FROM fact_ventas")
  nuevos <- anti_join(datos, existentes, by = "venta_id")
  if (nrow(nuevos) > 0) dbAppendTable(con, "fact_ventas", nuevos)
  nrow(nuevos)                  # devuelve cuántas filas se cargaron
}

# Opción 2: dejar que la base ignore las llaves repetidas
cargar_hechos_sql <- function(con, datos) {
  dbWriteTable(con, "stg_ventas", datos, temporary = TRUE,
               overwrite = TRUE)          # tabla temporal de paso
  n <- dbExecute(con, "INSERT OR IGNORE INTO fact_ventas
                       SELECT * FROM stg_ventas")
  dbExecute(con, "DROP TABLE stg_ventas")
  n
}

# ---- Bloque 47 --------------------------------------------------------
registrar_log <- function(con, archivo, leidas, validas, nuevas,
                          estado, mensaje = NA_character_) {
  dbAppendTable(con, "log_etl", tibble(
    fecha_hora    = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    archivo       = basename(archivo),
    filas_leidas  = leidas,  filas_validas = validas,
    filas_nuevas  = nuevas,  estado = estado,  mensaje = mensaje
  ))
}

ejecutar_etl <- function(con, ruta, cargar = cargar_hechos) {
  leidas <- validas <- NA_integer_
  en_transaccion <- FALSE
  estado <- tryCatch({
    datos   <- extraer_transacciones(ruta)                  # E
    leidas  <- nrow(datos)
    costos  <- dbGetQuery(con, "SELECT producto_id, costo
                                FROM dim_producto")
    limpios <- transformar_transacciones(datos, costos)     # T
    validas <- nrow(limpios)
    dbBegin(con); en_transaccion <- TRUE
    nuevas  <- cargar(con, limpios)                         # L
    registrar_log(con, ruta, leidas, validas, nuevas, "OK")
    dbCommit(con)
    sprintf("OK: %d leídas, %d nuevas", leidas, nuevas)
  }, error = function(e) {
    if (en_transaccion) dbRollback(con)     # deshacer la carga a medias
    registrar_log(con, ruta, leidas, validas, 0L, "ERROR",
                  conditionMessage(e))
    paste("ERROR:", conditionMessage(e))
  })
  cat(basename(ruta), "->", estado, "\n")
  invisible(estado)
}

# ---- Bloque 48 --------------------------------------------------------
ejecutar_etl(con, "datos/entrada/ventas_2023_s1.csv")
ejecutar_etl(con, "datos/entrada/ventas_2023_s2.csv")

# ¿Y si por error se vuelve a ejecutar el mismo archivo?
ejecutar_etl(con, "datos/entrada/ventas_2023_s2.csv")

# Lo mismo con la opción 2 (INSERT OR IGNORE)
ejecutar_etl(con, "datos/entrada/ventas_2023_s2.csv",
             cargar = cargar_hechos_sql)

dbGetQuery(con, "SELECT COUNT(*) AS filas FROM fact_ventas")

# ---- Bloque 49 --------------------------------------------------------
lote_malo <- trans_origen %>%
  slice(1:3) %>%
  mutate(transaccion_id = 1001:1003, fecha = as.Date("2023-12-31"),
         producto_id = c(5, 999, 7))
write_csv(lote_malo, "datos/entrada/ventas_2023_12_31.csv")

ejecutar_etl(con, "datos/entrada/ventas_2023_12_31.csv")
dbGetQuery(con, "SELECT COUNT(*) AS filas FROM fact_ventas")

# ---- Bloque 50 --------------------------------------------------------
dbGetQuery(con, "
  SELECT ejecucion_id AS id, archivo, filas_leidas AS leidas,
         filas_validas AS validas, filas_nuevas AS nuevas, estado
  FROM log_etl
")
dbGetQuery(con, "SELECT mensaje FROM log_etl WHERE estado = 'ERROR'")

# ---- Bloque 51 --------------------------------------------------------
tiendas_excel <- extraer_excel("datasets/datos_empresa.xlsx", "Tiendas")
class(tiendas_excel$fecha_apertura)

tiendas_excel_t <- tiendas_excel %>%
  transmute(tienda_id = as.integer(tienda_id), nombre_tienda,
            fecha_apertura = format(as.Date(fecha_apertura), "%Y-%m-%d"))

# Filas del Excel que no coinciden exactamente con la dimensión
tiendas_excel_t %>%
  anti_join(dbReadTable(con, "dim_tienda"),
            by = c("tienda_id", "nombre_tienda", "fecha_apertura"))

# ---- Bloque 52 --------------------------------------------------------
vistas <- c(
  # Vista "ancha": cada venta con todos sus atributos descriptivos
  "CREATE VIEW vw_ventas_detalle AS
   SELECT f.*, d.fecha, d.anio, d.mes, d.nombre_mes, d.dia_semana,
          d.es_fin_semana, p.nombre_producto, p.categoria, p.marca,
          c.nombre AS cliente, c.ciudad AS ciudad_cliente, c.segmento,
          c.es_premium, t.nombre_tienda, t.ciudad AS ciudad_tienda,
          t.zona, t.tipo AS tipo_tienda, v.nombre AS vendedor
   FROM fact_ventas AS f
   JOIN dim_fecha    AS d ON f.fecha_id    = d.fecha_id
   JOIN dim_producto AS p ON f.producto_id = p.producto_id
   JOIN dim_cliente  AS c ON f.cliente_id  = c.cliente_id
   JOIN dim_tienda   AS t ON f.tienda_id   = t.tienda_id
   JOIN dim_vendedor AS v ON f.vendedor_id = v.vendedor_id",
  # KPIs mensuales
  "CREATE VIEW vw_kpi_mensual AS
   SELECT d.anio, d.mes, d.nombre_mes,
          COUNT(*)                     AS tickets,
          COUNT(DISTINCT f.cliente_id) AS clientes,
          ROUND(SUM(f.total), 2)       AS ventas,
          ROUND(SUM(f.margen), 2)      AS margen,
          ROUND(AVG(f.total), 2)       AS ticket_promedio
   FROM fact_ventas AS f
   JOIN dim_fecha AS d ON f.fecha_id = d.fecha_id
   GROUP BY d.anio, d.mes, d.nombre_mes"
)
for (sql in vistas) dbExecute(con, sql)

dbGetQuery(con, "SELECT * FROM vw_kpi_mensual") %>% head(3)

# ---- Bloque 53 --------------------------------------------------------
consulta_cliente <- "SELECT SUM(total) FROM fact_ventas
                     WHERE cliente_id = 5"

# Antes del índice
dbGetQuery(con, paste("EXPLAIN QUERY PLAN", consulta_cliente))$detail

dbExecute(con, "CREATE INDEX idx_fact_cliente
                ON fact_ventas(cliente_id)")

# Después del índice
dbGetQuery(con, paste("EXPLAIN QUERY PLAN", consulta_cliente))$detail

# ---- Bloque 54 --------------------------------------------------------
indices <- c(
  "CREATE INDEX IF NOT EXISTS idx_fact_cliente ON fact_ventas(cliente_id)",
  "CREATE INDEX IF NOT EXISTS idx_fact_fecha ON fact_ventas(fecha_id)",
  "CREATE INDEX IF NOT EXISTS idx_fact_producto
     ON fact_ventas(producto_id)",
  "CREATE INDEX IF NOT EXISTS idx_fact_tienda ON fact_ventas(tienda_id)",
  "CREATE INDEX IF NOT EXISTS idx_fact_vendedor
     ON fact_ventas(vendedor_id)"
)

crear_vistas_indices <- function(con) {
  for (sql in c(indices, vistas)) dbExecute(con, sql)
  invisible(TRUE)
}
for (sql in indices) dbExecute(con, sql)

dbGetQuery(con, "SELECT type, name FROM sqlite_master
                 WHERE type IN ('index', 'view') ORDER BY type, name")

# ---- Bloque 55 --------------------------------------------------------
# a) Todo el detalle, todas las columnas
todo <- dbGetQuery(con, "SELECT * FROM fact_ventas")
# b) Solo las columnas necesarias
dos_columnas <- dbGetQuery(con, "SELECT fecha_id, total FROM fact_ventas")
# c) Ya agregado en la base
agregado <- dbGetQuery(con, "
  SELECT fecha_id / 100 AS anio_mes, SUM(total) AS ventas
  FROM fact_ventas GROUP BY anio_mes")

tibble(forma = c("a) todo", "b) dos columnas", "c) agregado"),
       filas = c(nrow(todo), nrow(dos_columnas), nrow(agregado)),
       kb    = round(c(object.size(todo), object.size(dos_columnas),
                       object.size(agregado)) / 1024, 1))

# ---- Bloque 56 --------------------------------------------------------
resultado <- dbSendQuery(con, "SELECT venta_id, metodo_pago, total
                               FROM fact_ventas")
bloque <- 0
total_acumulado <- 0
while (!dbHasCompleted(resultado)) {
  datos_bloque <- dbFetch(resultado, n = 300)     # 300 filas a la vez
  bloque <- bloque + 1
  total_acumulado <- total_acumulado + sum(datos_bloque$total)
  cat(sprintf("Bloque %d: %d filas, acumulado $%s\n", bloque,
              nrow(datos_bloque),
              format(round(total_acumulado), big.mark = ",")))
}
dbClearResult(resultado)   # liberar el resultado en la base

# ---- Solo referencia (no se ejecuta automáticamente) ----
# install.packages("odbc")
# odbc::odbcListDrivers()          # drivers ODBC instalados en tu equipo
# odbc::odbcListDataSources()      # conexiones (DSN) ya configuradas

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # SQL Server con autenticación de Windows (la más común en empresas)
# con_sql <- dbConnect(
#   odbc::odbc(),
#   Driver   = "ODBC Driver 18 for SQL Server",
#   Server   = "srv-datos.miempresa.local",
#   Database = "VentasDW",
#   Trusted_Connection = "yes",
#   TrustServerCertificate = "yes"
# )
#
# # SQL Server con usuario y contraseña (tomados de variables de entorno)
# con_sql <- dbConnect(
#   odbc::odbc(),
#   Driver   = "ODBC Driver 18 for SQL Server",
#   Server   = Sys.getenv("SQLSERVER_HOST"),
#   Database = "VentasDW",
#   UID      = Sys.getenv("SQLSERVER_USER"),
#   PWD      = Sys.getenv("SQLSERVER_PASS")
# )
#
# # Desde aquí, todo es igual: dbGetQuery(), tbl(), dbDisconnect()...
# # Ojo con el dialecto: SQL Server usa TOP en lugar de LIMIT
# dbGetQuery(con_sql, "SELECT TOP 10 * FROM dbo.fact_ventas")

# ---- Solo referencia (no se ejecuta automáticamente) ----
# install.packages(c("RPostgres", "RMariaDB"))
#
# # PostgreSQL
# con_pg <- dbConnect(
#   RPostgres::Postgres(),
#   host     = Sys.getenv("PG_HOST"),
#   port     = 5432,
#   dbname   = "analitica",
#   user     = Sys.getenv("PG_USER"),
#   password = Sys.getenv("PG_PASS")
# )
#
# # MySQL o MariaDB
# con_my <- dbConnect(
#   RMariaDB::MariaDB(),
#   host     = Sys.getenv("MYSQL_HOST"),
#   port     = 3306,
#   dbname   = "tienda_en_linea",
#   user     = Sys.getenv("MYSQL_USER"),
#   password = Sys.getenv("MYSQL_PASS")
# )
#
# dbListTables(con_pg)
# dbDisconnect(con_pg)
# dbDisconnect(con_my)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# usethis::edit_r_environ()          # abre ~/.Renviron para editarlo
# Sys.getenv("SQLSERVER_USER")       # "analista_bi"
# nchar(Sys.getenv("SQLSERVER_PASS")) > 0   # comprobar sin imprimirla

# ---- Bloque 57 --------------------------------------------------------
dbDisconnect(con)

reconstruir_dw <- function(ruta_bd, archivos_ventas) {
  unlink(ruta_bd)                                 # empezar desde cero
  con <- dbConnect(RSQLite::SQLite(), ruta_bd)
  crear_esquema(con)                              # tablas y llaves
  cargar_dimensiones(con, preparar_dimensiones()) # dimensiones
  for (archivo in archivos_ventas) {              # hechos (ETL)
    ejecutar_etl(con, archivo)
  }
  crear_vistas_indices(con)                       # vistas e índices
  con
}

con <- reconstruir_dw("datos/bi_retail.sqlite",
                      c("datos/entrada/ventas_2023_s1.csv",
                        "datos/entrada/ventas_2023_s2.csv"))
dbListTables(con)

# ---- Bloque 58 --------------------------------------------------------
kpi_mensual <- dbGetQuery(con, "
  SELECT mes, nombre_mes, tickets, ventas,
         ROUND(100.0 * margen / ventas, 1) AS margen_pct,
         ROUND(100.0 * (ventas - LAG(ventas) OVER (ORDER BY mes)) /
               LAG(ventas) OVER (ORDER BY mes), 1) AS crec_pct
  FROM vw_kpi_mensual
  ORDER BY anio, mes
")
kpi_mensual

# ---- Bloque 59 --------------------------------------------------------
kpi_dplyr <- read_csv("datasets/transacciones.csv",
                      show_col_types = FALSE) %>%
  group_by(mes = month(fecha)) %>%
  summarise(tickets = n(),
            ventas  = round(sum(cantidad * precio_venta), 2),
            .groups = "drop")

comparacion <- kpi_mensual %>%
  select(mes, tickets, ventas) %>%
  inner_join(kpi_dplyr, by = "mes", suffix = c("_sql", "_dplyr"))

all.equal(comparacion$ventas_sql, comparacion$ventas_dplyr)
all(comparacion$tickets_sql == comparacion$tickets_dplyr)

# ---- Bloque 60 --------------------------------------------------------
color_principal <- "#2a78d6"

grafica_mensual <- kpi_mensual %>%
  mutate(nombre_mes = factor(nombre_mes, levels = nombre_mes)) %>%
  ggplot(aes(x = nombre_mes, y = ventas, group = 1)) +
  geom_line(color = color_principal, linewidth = 0.8) +
  geom_point(color = color_principal, size = 2) +
  scale_y_continuous(labels = scales::dollar,
                     limits = c(0, NA)) +
  labs(title = "Ventas mensuales 2023",
       subtitle = "Fuente: vista vw_kpi_mensual del data warehouse",
       x = NULL, y = "Ventas") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
grafica_mensual

# ---- Bloque 61 --------------------------------------------------------
dbGetQuery(con, "
  SELECT p.nombre_producto, p.categoria,
         SUM(f.cantidad)                            AS unidades,
         ROUND(SUM(f.total), 0)                     AS ventas,
         ROUND(SUM(f.margen), 0)                    AS margen,
         ROUND(100.0 * SUM(f.margen) / SUM(f.total), 1) AS margen_pct
  FROM fact_ventas AS f
  JOIN dim_producto AS p ON f.producto_id = p.producto_id
  GROUP BY p.producto_id
  ORDER BY ventas DESC
  LIMIT 5
")

# ---- Bloque 62 --------------------------------------------------------
# ¿Hay productos que se venden con pérdida?
dbGetQuery(con, "
  SELECT p.nombre_producto, p.costo,
         ROUND(AVG(f.precio_venta), 2) AS precio_venta_prom,
         ROUND(SUM(f.margen), 0)       AS margen_total
  FROM fact_ventas AS f
  JOIN dim_producto AS p ON f.producto_id = p.producto_id
  GROUP BY p.producto_id
  HAVING SUM(f.margen) < 0
  ORDER BY margen_total
")

# ---- Bloque 63 --------------------------------------------------------
dbGetQuery(con, "
  SELECT c.nombre, c.ciudad, c.segmento,
         CASE c.es_premium WHEN 1 THEN 'Sí' ELSE 'No' END AS premium,
         COUNT(*)               AS compras,
         ROUND(SUM(f.total), 0) AS gasto,
         ROUND(SUM(f.margen), 0) AS margen
  FROM fact_ventas AS f
  JOIN dim_cliente AS c ON f.cliente_id = c.cliente_id
  GROUP BY c.cliente_id
  ORDER BY gasto DESC
  LIMIT 5
")

# ---- Bloque 64 --------------------------------------------------------
dbGetQuery(con, "
  SELECT t.nombre_tienda, t.tipo, t.tamano_m2,
         ROUND(SUM(f.total), 0)               AS ventas,
         ROUND(SUM(f.total) / t.tamano_m2, 1) AS ventas_m2,
         ROUND(AVG(f.total), 0)               AS ticket_prom
  FROM fact_ventas AS f
  JOIN dim_tienda AS t ON f.tienda_id = t.tienda_id
  GROUP BY t.tienda_id
  ORDER BY ventas_m2 DESC
")

# ---- Bloque 65 --------------------------------------------------------
dbGetQuery(con, "
  WITH ventas_vendedor_zona AS (
    SELECT t.zona, v.nombre AS vendedor, SUM(f.total) AS ventas
    FROM fact_ventas AS f
    JOIN dim_tienda   AS t ON f.tienda_id   = t.tienda_id
    JOIN dim_vendedor AS v ON f.vendedor_id = v.vendedor_id
    GROUP BY t.zona, v.vendedor_id
  ),
  ranking AS (
    SELECT zona, vendedor, ROUND(ventas, 0) AS ventas,
           RANK() OVER (PARTITION BY zona ORDER BY ventas DESC) AS lugar
    FROM ventas_vendedor_zona
  )
  SELECT * FROM ranking
  WHERE lugar <= 2
  ORDER BY zona, lugar
")

# ---- Bloque 66 --------------------------------------------------------
dbGetQuery(con, "
  SELECT CASE d.es_fin_semana WHEN 1 THEN 'Fin de semana'
                              ELSE 'Entre semana' END AS tipo_dia,
         COUNT(DISTINCT d.fecha_id)                    AS dias,
         COUNT(*)                                      AS tickets,
         ROUND(1.0 * COUNT(*) / COUNT(DISTINCT d.fecha_id), 2)
                                                  AS tickets_por_dia,
         ROUND(AVG(f.total), 2)                        AS ticket_prom
  FROM fact_ventas AS f
  JOIN dim_fecha AS d ON f.fecha_id = d.fecha_id
  GROUP BY tipo_dia
")

# ---- Bloque 67 --------------------------------------------------------
mostrar_error <- function(expr) {
  tryCatch(expr,
           error   = function(e) cat("Error:", conditionMessage(e), "\n"),
           warning = function(w) cat("Aviso:", conditionMessage(w), "\n"))
}

# ---- Bloque 68 --------------------------------------------------------
mostrar_error(dbGetQuery(con, "SELECT * FROM ventas_2024"))
mostrar_error(dbGetQuery(con, "SELECT totl FROM fact_ventas"))
mostrar_error(dbWriteTable(con, "dim_tienda", dimensiones$dim_tienda))
mostrar_error(dbAppendTable(con, "dim_tienda", dimensiones$dim_tienda))

# ---- Bloque 69 --------------------------------------------------------
dbGetQuery(con, "SELECT COUNT(*) AS n FROM dim_cliente
                 WHERE ciudad = 'CDMX'")     # correcto: 27
dbGetQuery(con, "SELECT COUNT(*) AS n FROM dim_cliente
                 WHERE ciudad = \"ciudad\"")  # ¡columna consigo misma!

# ---- Bloque 70 --------------------------------------------------------
con_temporal <- dbConnect(RSQLite::SQLite(), ":memory:")
dbDisconnect(con_temporal)
mostrar_error(dbGetQuery(con_temporal, "SELECT 1"))

# ---- Bloque 71 --------------------------------------------------------
dbDisconnect(con)
