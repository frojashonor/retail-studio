# ==========================================================================
# Manipulación de datos con dplyr y tidyr
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/02-manipulacion-datos.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # Se ejecuta UNA sola vez (tarda unos minutos)
# install.packages(c("tidyverse", "writexl"))

# ---- Bloque 1 --------------------------------------------------------
# Carga dplyr, tidyr, readr, stringr, lubridate, ggplot2, etc.
library(tidyverse)
# writexl no es parte del tidyverse: se carga aparte
library(writexl)

# ---- Bloque 2 --------------------------------------------------------
# Leer el archivo de ventas y guardarlo en el objeto "ventas"
ventas <- read_csv("datasets/ventas_retail.csv")

# ---- Bloque 3 --------------------------------------------------------
ventas

# ---- Bloque 4 --------------------------------------------------------
# Convertimos a data.frame clásico solo para comparar la impresión
as.data.frame(head(ventas, 3))

# ---- Bloque 5 --------------------------------------------------------
glimpse(ventas)

# ---- Bloque 6 --------------------------------------------------------
# Leer los identificadores como texto y la cantidad como entero
ventas_tipos <- read_csv(
  "datasets/ventas_retail.csv",
  col_types = cols(
    producto_id = col_character(),   # texto
    cliente_id  = col_character(),   # texto
    cantidad    = col_integer(),     # número entero
    .default    = col_guess()        # el resto: que readr adivine
  )
)
# Revisamos solo las columnas que nos interesan
glimpse(select(ventas_tipos, producto_id, cliente_id, cantidad))

# ---- Bloque 7 --------------------------------------------------------
# Crear la carpeta datos/ (si ya existe, no pasa nada)
dir.create("datos", showWarnings = FALSE)
# Escribir un CSV pequeño con dos valores "raros" en existencias
write_lines(c("sucursal,producto,existencias",
              "Centro,P01,120",
              "Norte,P01,N/D",
              "Sur,P02,85",
              "Este,P02,sin dato"),
            "datos/inventario.csv")
# Leerlo exigiendo que existencias sea numérica
inventario <- read_csv("datos/inventario.csv",
                       col_types = cols(existencias = col_double()))

# ---- Bloque 8 --------------------------------------------------------
problems(inventario) %>%
  select(row, col, expected, actual)

# ---- Bloque 9 --------------------------------------------------------
inventario <- read_csv("datos/inventario.csv",
                       na = c("", "NA", "N/D", "sin dato"),
                       show_col_types = FALSE)
inventario

# ---- Solo referencia (no se ejecuta automáticamente) ----
# reporte <- read_csv2("datos/reporte_excel.csv")      # ; y decimal ,
# clientes_erp <- read_csv("datos/clientes_erp.csv",
#                          locale = locale(encoding = "latin1"))

# ---- Bloque 10 --------------------------------------------------------
productos     <- read_csv("datasets/productos.csv",
                          show_col_types = FALSE)
clientes      <- read_csv("datasets/clientes.csv",
                          show_col_types = FALSE)
tiendas       <- read_csv("datasets/tiendas.csv",
                          show_col_types = FALSE)
empleados     <- read_csv("datasets/empleados.csv",
                          show_col_types = FALSE)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)

# Un pequeño inventario de lo que cargamos
tibble(
  tabla    = c("ventas", "productos", "clientes", "tiendas",
               "empleados", "transacciones"),
  filas    = c(nrow(ventas), nrow(productos), nrow(clientes),
               nrow(tiendas), nrow(empleados), nrow(transacciones)),
  columnas = c(ncol(ventas), ncol(productos), ncol(clientes),
               ncol(tiendas), ncol(empleados), ncol(transacciones))
)

# ---- Bloque 11 --------------------------------------------------------
arrange(
  select(
    filter(ventas, tienda_id == 1, mes == "2023-12"),
    fecha, producto_id, cantidad, total
  ),
  desc(total)
)

# ---- Bloque 12 --------------------------------------------------------
paso1 <- filter(ventas, tienda_id == 1, mes == "2023-12")
paso2 <- select(paso1, fecha, producto_id, cantidad, total)
paso3 <- arrange(paso2, desc(total))

# ---- Bloque 13 --------------------------------------------------------
ventas %>%                                   # toma las ventas,
  filter(tienda_id == 1, mes == "2023-12") %>% # y luego filtra,
  select(fecha, producto_id, cantidad, total) %>% # y luego elige,
  arrange(desc(total))                       # y luego ordena

# ---- Bloque 14 --------------------------------------------------------
# Mismo resultado que con %>%: contar las ventas de diciembre
ventas |>
  filter(mes == "2023-12") |>
  nrow()

# ---- Solo referencia (no se ejecuta automáticamente) ----
# head(arrange(filter(productos, categoria == "Ropa"), precio_catalogo), 3)

# ---- Bloque 15 --------------------------------------------------------
ventas %>%
  filter(total > 4500) %>%                       # condición numérica
  select(fecha, tienda_id, cantidad, precio_unitario, total)

# ---- Bloque 16 --------------------------------------------------------
ventas %>%
  filter(tienda_id == 9,            # tienda Premium
         trimestre == "Q1",         # y primer trimestre
         descuento_pct > 0) %>%     # y con descuento
  select(fecha, producto_id, descuento_pct, total)

# ---- Bloque 17 --------------------------------------------------------
# O: basta con que se cumpla una de las dos condiciones
ventas %>%
  filter(descuento_pct == 20 | cantidad == 10) %>%
  nrow()

# %in%: la tienda está en la lista (más corto que cuatro "|")
ventas_cdmx <- ventas %>%
  filter(tienda_id %in% c(1, 4, 6, 9))
nrow(ventas_cdmx)

# ---- Bloque 18 --------------------------------------------------------
# between() incluye ambos extremos: 1000 <= total <= 2000
ventas %>%
  filter(between(total, 1000, 2000)) %>%
  nrow()

# Con fechas: compara contra objetos Date, no contra texto
ventas %>%
  filter(fecha >= as.Date("2023-11-17"),
         fecha <= as.Date("2023-11-20")) %>%
  select(fecha, dia_semana, producto_id, cantidad, total)

# ---- Bloque 19 --------------------------------------------------------
productos %>%
  filter(!activo) %>%                 # los que NO están activos
  select(producto_id, nombre_producto, categoria, stock_actual)

# ---- Bloque 20 --------------------------------------------------------
metas <- tibble(
  vendedor = c("Ana", "Luis", "Rosa", "Jorge", "Elena"),
  meta     = c(50000, NA, 42000, NA, 61000)   # NA = aún no capturada
)
# filter() descarta en silencio las filas donde la condición es NA
metas %>% filter(meta > 45000)
# Para encontrar los faltantes usa is.na(), nunca meta == NA
metas %>% filter(is.na(meta))

# ---- Bloque 21 --------------------------------------------------------
transacciones %>%
  filter(str_detect(metodo_pago, "Tarjeta")) %>%  # contiene "Tarjeta"
  count(metodo_pago)                              # cuántas de cada una

# ---- Bloque 22 --------------------------------------------------------
lista_precios <- productos %>%
  select(nombre_producto, categoria, precio_catalogo)   # por nombre
head(lista_precios, 4)

# ---- Bloque 23 --------------------------------------------------------
# Rango: de fecha hasta cantidad (columnas consecutivas)
ventas %>% select(fecha:cantidad) %>% names()

# Quitar: todo menos las etiquetas de texto que no usaremos
ventas %>% select(-dia_semana, -mes, -trimestre) %>% names()

# ---- Bloque 24 --------------------------------------------------------
# Todas las llaves: terminan en "_id"
ventas %>% select(ends_with("_id")) %>% names()

# Columnas de inventario: empiezan con "stock"
productos %>% select(producto_id, starts_with("stock")) %>% head(3)

# Columnas que contienen "fecha" en cualquier parte del nombre
clientes %>% select(cliente_id, contains("fecha")) %>% head(3)

# Solo las columnas numéricas (útil antes de calcular resúmenes)
productos %>% select(where(is.numeric)) %>% names()

# ---- Bloque 25 --------------------------------------------------------
tiendas %>%
  rename(tienda = nombre_tienda, m2 = tamano_m2) %>%  # nuevo = viejo
  select(tienda_id, tienda, ciudad, m2) %>%
  head(3)

# Poner el total justo después de la fecha para leerlo más fácil
ventas %>%
  relocate(total, .after = fecha) %>%
  head(3)

# ---- Bloque 26 --------------------------------------------------------
ventas %>%
  mutate(
    iva            = total * 0.16,        # 16% sobre el total
    total_con_iva  = total + iva          # usa la columna recién creada
  ) %>%
  select(fecha, subtotal, descuento_monto, total, iva, total_con_iva) %>%
  head(4)

# ---- Bloque 27 --------------------------------------------------------
productos %>%
  mutate(
    margen_pesos = precio_catalogo - costo,
    margen_calc  = round(margen_pesos / precio_catalogo * 100, 1)
  ) %>%
  select(nombre_producto, precio_catalogo, costo,
         margen_pesos, margen_calc) %>%
  arrange(margen_calc) %>%            # del menor al mayor margen
  head(5)

# ---- Bloque 28 --------------------------------------------------------
ventas %>%
  mutate(tipo_dia = if_else(dia_semana %in% c("sábado", "domingo"),
                            "Fin de semana", "Entre semana")) %>%
  count(tipo_dia)

# ---- Bloque 29 --------------------------------------------------------
ventas_segmentadas <- ventas %>%
  mutate(segmento_ticket = case_when(
    total < 500   ~ "1. Chico",       # se evalúa primero
    total < 1500  ~ "2. Mediano",     # solo si no fue Chico
    total < 3000  ~ "3. Grande",
    .default      = "4. Premium"      # todo lo demás
  ))

ventas_segmentadas %>%
  group_by(segmento_ticket) %>%
  summarise(ventas = n(), ingresos = sum(total))

# ---- Bloque 30 --------------------------------------------------------
# Redondear a 0 decimales las tres columnas de dinero
ventas %>%
  mutate(across(c(subtotal, descuento_monto, total), round)) %>%
  select(fecha, subtotal, descuento_monto, total) %>%
  head(3)

# Con una función "al vuelo": ~ .x indica "cada columna"
productos %>%
  mutate(across(c(precio_catalogo, costo), ~ .x * 1.16,
                .names = "{.col}_con_iva")) %>%
  select(nombre_producto, ends_with("_con_iva")) %>%
  head(3)

# ---- Bloque 31 --------------------------------------------------------
# Solo las columnas que participaron en el cálculo, más la nueva
ventas %>%
  mutate(precio_neto = total / cantidad, .keep = "used") %>%
  head(3)

# Solo las nuevas (equivalente a transmute())
ventas %>%
  mutate(anio = str_sub(mes, 1, 4), ticket = round(total),
         .keep = "none") %>%
  head(3)

# ---- Bloque 32 --------------------------------------------------------
productos %>%
  arrange(desc(precio_catalogo)) %>%    # de mayor a menor
  select(nombre_producto, categoria, precio_catalogo) %>%
  head(5)

tiendas %>%
  arrange(ciudad, desc(tamano_m2)) %>%  # ciudad A-Z; luego m2 desc.
  select(ciudad, nombre_tienda, tamano_m2)

# ---- Bloque 33 --------------------------------------------------------
# Top 3 ventas del año por monto
ventas %>%
  slice_max(total, n = 3) %>%
  select(fecha, tienda_id, total)

# Los 3 productos más baratos
productos %>%
  slice_min(precio_catalogo, n = 3) %>%
  select(nombre_producto, precio_catalogo)

# Una muestra aleatoria de 3 ventas para auditar
set.seed(123)
ventas %>%
  slice_sample(n = 3) %>%
  select(fecha, producto_id, total)

# ---- Bloque 34 --------------------------------------------------------
# La venta más grande de cada trimestre
ventas %>%
  group_by(trimestre) %>%
  slice_max(total, n = 1) %>%
  ungroup() %>%
  select(trimestre, fecha, tienda_id, total)

# El producto más caro de cada categoría
productos %>%
  group_by(categoria) %>%
  slice_max(precio_catalogo, n = 1) %>%
  ungroup() %>%
  select(categoria, nombre_producto, precio_catalogo)

# ---- Bloque 35 --------------------------------------------------------
kpis_2023 <- ventas %>%
  summarise(
    ingresos        = sum(total),
    num_ventas      = n(),                    # cuenta filas
    unidades        = sum(cantidad),
    ticket_promedio = mean(total),
    ticket_mediano  = median(total),
    venta_minima    = min(total),
    venta_maxima    = max(total),
    clientes_unicos = n_distinct(cliente_id)  # clientes distintos
  )
glimpse(kpis_2023)

# ---- Bloque 36 --------------------------------------------------------
ventas %>%
  group_by(trimestre) %>%                 # un grupo por trimestre
  summarise(
    ingresos        = sum(total),
    num_ventas      = n(),
    ticket_promedio = mean(total)
  )

# ---- Bloque 37 --------------------------------------------------------
ventas_tienda <- ventas %>%
  group_by(tienda_id) %>%
  summarise(
    ingresos        = sum(total),
    num_ventas      = n(),
    unidades        = sum(cantidad),
    ticket_promedio = round(mean(total), 2)
  ) %>%
  arrange(desc(ingresos))                 # la mejor tienda primero
ventas_tienda

# ---- Bloque 38 --------------------------------------------------------
ventas_trim_tienda <- ventas %>%
  group_by(trimestre, tienda_id) %>%
  summarise(ingresos = sum(total))

# ---- Bloque 39 --------------------------------------------------------
ventas_trim_tienda <- ventas %>%
  group_by(trimestre, tienda_id) %>%
  summarise(ingresos = sum(total), .groups = "drop")  # sin grupos
ventas_trim_tienda

# ---- Bloque 40 --------------------------------------------------------
# ¿Cuántas transacciones por método de pago?
transacciones %>% count(metodo_pago, sort = TRUE)

# Con wt: suma el total en vez de contar filas
transacciones %>%
  count(metodo_pago, wt = total_transaccion, sort = TRUE,
        name = "ingresos")

# Dos columnas: combinaciones de tienda y método de pago
transacciones %>% count(tienda_id, metodo_pago) %>% head(5)

# ---- Bloque 41 --------------------------------------------------------
mezcla_pago <- transacciones %>%
  group_by(trimestre, metodo_pago) %>%
  summarise(ingresos = sum(total_transaccion),
            .groups = "drop_last") %>%   # queda agrupado por trimestre
  mutate(pct_trimestre = round(ingresos / sum(ingresos) * 100, 1)) %>%
  ungroup()                               # quitamos el último grupo
mezcla_pago %>% filter(trimestre %in% c("Q1", "Q4"))

# ---- Bloque 42 --------------------------------------------------------
ventas_vs_tienda <- ventas %>%
  group_by(tienda_id) %>%
  mutate(
    promedio_tienda = mean(total),              # promedio de SU tienda
    veces_promedio  = round(total / promedio_tienda, 2)
  ) %>%
  ungroup()                                     # ¡no lo olvides!

ventas_vs_tienda %>%
  select(fecha, tienda_id, total, promedio_tienda, veces_promedio) %>%
  slice_max(veces_promedio, n = 4)

# ---- Bloque 43 --------------------------------------------------------
metas %>%
  summarise(
    suma_sin_na_rm  = sum(meta),               # NA "contagia"
    suma_capturada  = sum(meta, na.rm = TRUE), # ignora los NA
    metas_faltantes = sum(is.na(meta))         # cuántos NA hay
  )

# ---- Bloque 44 --------------------------------------------------------
ventas_mensuales <- ventas %>%
  group_by(mes) %>%
  summarise(ingresos = sum(total), num_ventas = n(),
            .groups = "drop") %>%
  arrange(mes)                  # ¡las ventanas dependen del orden!

# ---- Bloque 45 --------------------------------------------------------
kpis_mensuales <- ventas_mensuales %>%
  mutate(
    mes_anterior = lag(ingresos),                    # fila previa
    mom_pct      = (ingresos / lag(ingresos) - 1) * 100,
    ytd          = cumsum(ingresos)                  # acumulado
  )

kpis_mensuales %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  select(mes, ingresos, mes_anterior, mom_pct, ytd)

# ---- Bloque 46 --------------------------------------------------------
kpis_mensuales <- kpis_mensuales %>%
  mutate(
    participacion = ingresos / sum(ingresos) * 100,
    prom_movil_3m = (ingresos + lag(ingresos) + lag(ingresos, 2)) / 3,
    vs_siguiente  = lead(ingresos) - ingresos     # ¿el mes que sigue?
  )

kpis_mensuales %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  select(mes, ingresos, participacion, prom_movil_3m, vs_siguiente)

# ---- Bloque 47 --------------------------------------------------------
tibble(vendedor = c("Ana", "Beto", "Caro", "Dani", "Eva"),
       ventas   = c(900, 700, 700, 500, 300)) %>%
  mutate(
    row_number = row_number(desc(ventas)),  # 1,2,3,4,5 (sin empates)
    min_rank   = min_rank(desc(ventas)),    # 1,2,2,4,5 (con hueco)
    dense_rank = dense_rank(desc(ventas))   # 1,2,2,3,4 (sin hueco)
  )

# ---- Bloque 48 --------------------------------------------------------
podio_trimestral <- ventas_trim_tienda %>%     # la calculamos antes
  group_by(trimestre) %>%
  mutate(lugar = min_rank(desc(ingresos))) %>% # ranking por trimestre
  filter(lugar <= 3) %>%
  arrange(trimestre, lugar) %>%
  ungroup()
podio_trimestral

# ---- Bloque 49 --------------------------------------------------------
abc_productos <- ventas %>%
  group_by(producto_id) %>%
  summarise(ingresos = sum(total), .groups = "drop") %>%
  arrange(desc(ingresos)) %>%                    # del mejor al peor
  mutate(
    pct_acumulado = cumsum(ingresos) / sum(ingresos) * 100,
    clase = case_when(
      pct_acumulado <= 80 ~ "A",
      pct_acumulado <= 95 ~ "B",
      .default            = "C"
    )
  )

abc_productos %>%
  group_by(clase) %>%
  summarise(productos = n(), ingresos = sum(ingresos),
            .groups = "drop") %>%
  mutate(pct_ingresos = round(ingresos / sum(ingresos) * 100, 1))

# ---- Bloque 50 --------------------------------------------------------
ymd("2023-03-15")            # año, mes, día
dmy("15/03/2023")            # día, mes, año (formato de México)
mdy("03-15-2023")            # mes, día, año (formato de EE. UU.)
dmy("15 de marzo de 2023")   # ¡entiende meses en español!
ymd(20230315)                # también números tipo AAAAMMDD

# ---- Bloque 51 --------------------------------------------------------
ventas_cal <- ventas %>%
  mutate(
    anio       = year(fecha),                           # 2023
    mes_num    = month(fecha),                          # 1 a 12
    mes_nombre = month(fecha, label = TRUE),            # ene, feb...
    trim       = quarter(fecha),                        # 1 a 4
    dia_sem    = wday(fecha, label = TRUE, abbr = FALSE), # lunes...
    semana     = week(fecha)                            # 1 a 53
  )
ventas_cal %>%
  select(fecha, anio, mes_num, mes_nombre, trim, dia_sem, semana) %>%
  head(4)

# ---- Bloque 52 --------------------------------------------------------
ventas_cal %>%
  group_by(dia_sem) %>%
  summarise(ventas = n(), ingresos = sum(total),
            ticket_promedio = mean(total), .groups = "drop")

# ---- Bloque 53 --------------------------------------------------------
ventas %>%
  mutate(mes_fecha = floor_date(fecha, unit = "month")) %>%
  group_by(mes_fecha) %>%
  summarise(ingresos = sum(total), .groups = "drop") %>%
  head(3)

# Ventas por semana (las semanas empiezan en lunes: week_start = 1)
ventas %>%
  mutate(semana_inicio = floor_date(fecha, "week", week_start = 1)) %>%
  count(semana_inicio) %>%
  head(3)

# ---- Bloque 54 --------------------------------------------------------
metas_mensuales <- tibble(anio = 2023, mes = 1:3,
                          meta = c(45000, 42000, 40000)) %>%
  mutate(fecha_mes = make_date(anio, mes, 1))    # día 1 de cada mes
metas_mensuales

# ---- Bloque 55 --------------------------------------------------------
fecha_corte <- as.Date("2024-01-01")     # fija: resultados reproducibles

clientes_antig <- clientes %>%
  mutate(
    dias_registro  = as.numeric(difftime(fecha_corte, fecha_registro,
                                         units = "days")),
    anios_registro = time_length(interval(fecha_registro, fecha_corte),
                                 "years"),
    tipo_cliente   = if_else(anios_registro < 1, "Nuevo", "Recurrente")
  )
clientes_antig %>%
  select(cliente_id, fecha_registro, dias_registro, anios_registro) %>%
  head(3)
clientes_antig %>% count(tipo_cliente)

# ---- Bloque 56 --------------------------------------------------------
recencia <- ventas %>%
  group_by(cliente_id) %>%
  summarise(
    primera_compra = min(fecha),
    ultima_compra  = max(fecha),
    compras        = n(),
    monto          = sum(total),
    .groups = "drop"
  ) %>%
  mutate(
    dias_sin_comprar = as.numeric(fecha_corte - ultima_compra),
    estado = case_when(
      dias_sin_comprar <= 90  ~ "Activo",
      dias_sin_comprar <= 180 ~ "En riesgo",
      .default                = "Inactivo"
    )
  )
recencia %>%
  arrange(desc(dias_sin_comprar)) %>%
  select(cliente_id, ultima_compra, compras, dias_sin_comprar, estado) %>%
  head(3)
recencia %>% count(estado)

# ---- Bloque 57 --------------------------------------------------------
ciudades <- c("  cdmx", "Guadalajara ", "MONTERREY", "san  luis   potosí")

str_to_upper(ciudades)    # TODO EN MAYÚSCULAS
str_to_lower(ciudades)    # todo en minúsculas
str_to_title(ciudades)    # Tipo Título
str_trim(ciudades)        # quita espacios al inicio y al final
str_squish(ciudades)      # además, reduce espacios internos repetidos

# ---- Bloque 58 --------------------------------------------------------
clientes %>%
  filter(str_detect(nombre, "García")) %>%          # contiene García
  mutate(
    tel_limpio = str_replace_all(telefono, "-", ""), # quita guiones
    lada       = str_sub(telefono, 1, 2)             # caracteres 1 a 2
  ) %>%
  select(cliente_id, nombre, telefono, tel_limpio, lada) %>%
  head(4)

# ---- Bloque 59 --------------------------------------------------------
productos %>%
  mutate(
    sku = str_c("SKU-", str_pad(producto_id, width = 4, pad = "0")),
    nombre_corto = str_replace(nombre_producto, "Producto", "Prod.")
  ) %>%
  select(producto_id, sku, nombre_producto, nombre_corto) %>%
  slice(c(1, 9, 10, 50))                  # filas 1, 9, 10 y 50

# ---- Bloque 60 --------------------------------------------------------
str_split("maria.lopez@email.com", "@")     # devuelve una lista

clientes %>%
  select(cliente_id, nombre, email) %>%
  separate_wider_delim(nombre, delim = " ",
                       names = c("nombre_pila", "apellido")) %>%
  separate_wider_delim(email, delim = "@",
                       names = c("usuario", "dominio")) %>%
  head(3)

# ---- Bloque 61 --------------------------------------------------------
ventas_mini <- tibble(venta = 1:3, producto_id = c(10, 20, 99),
                      unidades = c(2, 5, 1))
catalogo_mini <- tibble(producto_id = c(10, 20, 30),
                        nombre = c("Café", "Té", "Azúcar"))

left_join(ventas_mini, catalogo_mini, by = "producto_id")
inner_join(ventas_mini, catalogo_mini, by = "producto_id")
full_join(ventas_mini, catalogo_mini, by = "producto_id")

# ---- Bloque 62 --------------------------------------------------------
semi_join(ventas_mini, catalogo_mini, by = "producto_id") # con pareja
anti_join(ventas_mini, catalogo_mini, by = "producto_id") # sin pareja
anti_join(catalogo_mini, ventas_mini, by = "producto_id") # sin ventas

# ---- Bloque 63 --------------------------------------------------------
ventas_prod <- ventas %>%
  left_join(productos, by = "producto_id")   # agrega 12 columnas
dim(ventas_prod)                              # mismas 365 filas

ventas_prod %>%
  group_by(categoria) %>%
  summarise(ingresos = sum(total), unidades = sum(cantidad),
            productos_vendidos = n_distinct(producto_id),
            .groups = "drop") %>%
  arrange(desc(ingresos))

# ---- Bloque 64 --------------------------------------------------------
# Forma clásica: c("nombre en x" = "nombre en y")
v1 <- ventas %>%
  left_join(empleados, by = c("vendedor_id" = "empleado_id"))

# Forma moderna (dplyr 1.1+): join_by() con ==
v2 <- ventas %>%
  left_join(empleados, join_by(vendedor_id == empleado_id))

identical(v1, v2)       # ¿dan exactamente lo mismo?

# ---- Bloque 65 --------------------------------------------------------
v2 %>%
  group_by(departamento) %>%
  summarise(vendedores = n_distinct(vendedor_id),
            ingresos = sum(total), .groups = "drop") %>%
  arrange(desc(ingresos))

# ---- Bloque 66 --------------------------------------------------------
ventas_ciudad <- ventas %>%
  left_join(clientes, by = "cliente_id") %>%
  left_join(tiendas, by = "tienda_id",
            suffix = c("_cliente", "_tienda"))   # en vez de .x / .y

ventas_ciudad %>%
  select(fecha, ciudad_cliente, ciudad_tienda) %>%
  head(3)

ventas_ciudad %>%
  summarise(pct_misma_ciudad =
              mean(ciudad_cliente == ciudad_tienda) * 100)

# ---- Bloque 67 --------------------------------------------------------
precios_compras <- tibble(
  producto_id = c(31, 31, 15),
  vigencia    = c("2022", "2023", "2023"),
  precio_lista = c(340, 355, 290)
)
ventas_ene <- ventas %>%
  filter(fecha <= as.Date("2023-01-02")) %>%
  select(fecha, producto_id, total)
ventas_ene
ventas_ene %>% left_join(precios_compras, by = "producto_id")

# ---- Bloque 68 --------------------------------------------------------
precios_compras %>%
  count(producto_id) %>%
  filter(n > 1)             # llaves repetidas: debería salir vacío

# ---- Solo referencia (no se ejecuta automáticamente) ----
# ventas_ene %>%
#   left_join(precios_compras, by = "producto_id",
#             relationship = "many-to-one")

# ---- Bloque 69 --------------------------------------------------------
# 1. ¿Ventas con un producto que no está en el catálogo? (huérfanas)
ventas %>% anti_join(productos, by = "producto_id") %>% nrow()

# 2. ¿Productos del catálogo que no se vendieron en ventas?
productos %>% anti_join(ventas, by = "producto_id") %>% nrow()

# 3. ¿Clientes registrados que no compraron en 2023?
clientes_sin_compra <- clientes %>%
  anti_join(ventas, by = "cliente_id")
nrow(clientes_sin_compra)

# 4. De los que sí compraron (semi_join), ¿cuántos son premium?
clientes %>%
  semi_join(ventas, by = "cliente_id") %>%
  count(es_premium)

# ---- Bloque 70 --------------------------------------------------------
ventas %>%
  left_join(select(clientes, cliente_id, fecha_registro),
            by = "cliente_id") %>%
  filter(fecha < fecha_registro) %>%          # compra antes del registro
  select(fecha, cliente_id, fecha_registro, total) %>%
  arrange(fecha_registro - fecha) %>%
  head(3)

ventas %>%
  left_join(select(clientes, cliente_id, fecha_registro),
            by = "cliente_id") %>%
  summarise(ventas_antes_registro = sum(fecha < fecha_registro))

# ---- Bloque 71 --------------------------------------------------------
# Simulamos tres archivos mensuales
enero   <- ventas %>% filter(mes == "2023-01")
febrero <- ventas %>% filter(mes == "2023-02")
marzo   <- ventas %>% filter(mes == "2023-03") %>%
  select(-dia_semana)               # a marzo le falta una columna

primer_trimestre <- bind_rows(enero, febrero, marzo,
                              .id = "archivo")   # de qué tabla vino
primer_trimestre %>% count(archivo)
primer_trimestre %>% filter(is.na(dia_semana)) %>% nrow()

# ---- Bloque 72 --------------------------------------------------------
tabla_trim_cat <- ventas_prod %>%              # ventas + productos
  group_by(categoria, trimestre) %>%
  summarise(ingresos = sum(total) / 1000, .groups = "drop") %>%
  pivot_wider(
    names_from  = trimestre,    # de dónde salen los nombres de columna
    values_from = ingresos      # de dónde salen los valores
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))
tabla_trim_cat

# ---- Bloque 73 --------------------------------------------------------
ventas_prod %>%
  count(categoria, mes) %>%            # número de ventas
  pivot_wider(names_from = mes, values_from = n,
              values_fill = 0) %>%     # 0 donde no hubo ventas
  select(categoria, `2023-01`:`2023-06`)

# ---- Bloque 74 --------------------------------------------------------
presupuesto <- tibble(
  tienda_id = c(1, 2, 3),
  Ene = c(5000, 4500, 4800), Feb = c(4800, 4300, 4600),
  Mar = c(5200, 4700, 5000), Abr = c(5100, 4600, 4900),
  May = c(5300, 4900, 5100), Jun = c(5000, 4400, 4700)
)
presupuesto_largo <- presupuesto %>%
  pivot_longer(
    cols      = Ene:Jun,          # columnas a "derretir"
    names_to  = "mes_texto",      # los nombres van a esta columna
    values_to = "presupuesto"     # los valores van a esta otra
  )
presupuesto_largo

# ---- Bloque 75 --------------------------------------------------------
meses_abrev <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun")

# Ventas reales por tienda y mes
real_tienda_mes <- ventas %>%
  group_by(tienda_id, mes) %>%
  summarise(real = sum(total), .groups = "drop")

cumplimiento <- presupuesto_largo %>%
  mutate(mes = str_c("2023-", str_pad(match(mes_texto, meses_abrev),
                                      2, pad = "0"))) %>%
  left_join(real_tienda_mes,
            by = c("tienda_id", "mes")) %>%    # llave de 2 columnas
  mutate(cumplimiento_pct = round(real / presupuesto * 100))

cumplimiento %>%
  select(tienda_id, mes, presupuesto, real, cumplimiento_pct) %>%
  filter(tienda_id == 1)

# ---- Bloque 76 --------------------------------------------------------
codigos <- tibble(clave_tienda = c("CDMX-Centro-001", "GDL-Norte-002",
                                   "MTY-Sur-003"),
                  ventas = c(43899, 54846, 54066))
codigos_sep <- codigos %>%
  separate_wider_delim(clave_tienda, delim = "-",
                       names = c("ciudad", "zona", "numero"))
codigos_sep

codigos_sep %>%
  unite("etiqueta", ciudad, zona, sep = " / ")   # une en una columna

# ---- Bloque 77 --------------------------------------------------------
reporte_regional <- tibble(
  region      = c("Norte", NA, NA, "Centro", NA, "Sur"),
  tienda      = c("Monterrey", "Tijuana", "Guadalajara",
                  "CDMX", "Puebla", "Mérida"),
  ventas      = c(54066, 70155, 54846, 43899, 53611, NA),
  devoluciones = c(1200, NA, 800, NA, 650, 300)
)
reporte_regional %>%
  fill(region, .direction = "down")    # copia el valor hacia abajo

# ---- Bloque 78 --------------------------------------------------------
reporte_limpio <- reporte_regional %>%
  fill(region) %>%
  replace_na(list(devoluciones = 0)) %>%   # NA -> 0 solo aquí
  drop_na(ventas)                          # quita filas sin ventas
reporte_limpio

# ---- Bloque 79 --------------------------------------------------------
ventas_tienda_mes <- ventas %>%
  group_by(tienda_id, mes) %>%
  summarise(ingresos = sum(total), .groups = "drop")
nrow(ventas_tienda_mes)            # ¿120?

ventas_tienda_mes_completa <- ventas_tienda_mes %>%
  complete(tienda_id, mes,                 # todas las combinaciones
           fill = list(ingresos = 0))      # con 0 donde no hubo ventas
nrow(ventas_tienda_mes_completa)

ventas_tienda_mes_completa %>% filter(ingresos == 0)

# ---- Bloque 80 --------------------------------------------------------
ventas_sucias <- tibble(
  folio    = c(101, 102, 102, 103, 104, 105, 106, 107, 108, 109),
  fecha    = c("2023-03-01", "02/03/2023", "02/03/2023", "2023-03-03",
               "03/03/2023", "2023-03-04", "05/03/2023", "2023-03-06",
               NA, "2023-03-07"),
  ciudad   = c("CDMX", "cdmx", "cdmx", "CDMX ", "Cd. de México",
               " guadalajara", "Guadalajara", "GDL", "Monterrey",
               "monterrey"),
  categoria = c("Electrónica", "electronica", "electronica", "Ropa",
                "ROPA", "Hogar", "hogar ", "Electrónica", "Ropa", "Hogar"),
  monto    = c("$1,234.50", "980", "980", "$2,100.00", "450.5",
               "$3,050", "1,120.00", "$899.99", "560", "$1,780.25"),
  cantidad = c(2, 1, 1, 3, 1, 4, 2, -1, 1, 2)
)
ventas_sucias

# ---- Bloque 81 --------------------------------------------------------
ventas_sucias %>% count(folio) %>% filter(n > 1)   # ¿folios repetidos?
paso1 <- ventas_sucias %>% distinct()              # quita filas idénticas
nrow(ventas_sucias); nrow(paso1)

# ---- Bloque 82 --------------------------------------------------------
paso2 <- paso1 %>%
  mutate(
    ciudad = str_squish(str_to_upper(ciudad)),     # " cdmx" -> "CDMX"
    ciudad = case_when(                            # unificar sinónimos
      ciudad %in% c("CDMX", "CD. DE MÉXICO") ~ "CDMX",
      ciudad %in% c("GUADALAJARA", "GDL")   ~ "Guadalajara",
      ciudad == "MONTERREY"                 ~ "Monterrey",
      .default = ciudad                     # por si aparece otra
    ),
    categoria = str_to_title(str_squish(categoria)),
    categoria = recode(categoria, "Electronica" = "Electrónica")
  )
paso2 %>% count(ciudad)
paso2 %>% count(categoria)

# ---- Bloque 83 --------------------------------------------------------
paso3 <- paso2 %>%
  mutate(
    monto = parse_number(monto),        # "$1,234.50" -> 1234.5
    fecha = parse_date_time(fecha,      # prueba ambos formatos
                            orders = c("ymd", "dmy")),
    fecha = as_date(fecha)              # de fecha-hora a fecha
  )
paso3 %>% select(folio, fecha, monto)

# ---- Bloque 84 --------------------------------------------------------
# Revisar antes de borrar: ¿qué filas violan las reglas?
paso3 %>% filter(cantidad <= 0 | is.na(fecha))

ventas_limpias <- paso3 %>%
  filter(cantidad > 0,              # cantidades imposibles fuera
         !is.na(fecha)) %>%         # sin fecha no se puede asignar mes
  mutate(precio_unitario = monto / cantidad)

ventas_limpias %>%
  group_by(ciudad) %>%
  summarise(ventas = n(), ingresos = sum(monto), .groups = "drop")

# ---- Bloque 85 --------------------------------------------------------
maestra <- ventas %>%
  left_join(select(productos, producto_id, nombre_producto, categoria,
                   precio_catalogo, costo),
            by = "producto_id") %>%
  left_join(select(clientes, cliente_id, cliente = nombre,
                   ciudad_cliente = ciudad, es_premium),
            by = "cliente_id") %>%
  left_join(select(tiendas, tienda_id, nombre_tienda,
                   ciudad_tienda = ciudad, zona),
            by = "tienda_id") %>%
  left_join(select(empleados, empleado_id, vendedor = nombre),
            join_by(vendedor_id == empleado_id)) %>%
  mutate(
    mes_fecha   = floor_date(fecha, "month"),
    mes_nombre  = month(fecha, label = TRUE),
    costo_total = costo * cantidad,          # costo de lo vendido
    utilidad    = total - costo_total        # utilidad bruta
  )

# Control: los joins no deben agregar ni quitar filas
stopifnot(nrow(maestra) == nrow(ventas))
dim(maestra)

# ---- Bloque 86 --------------------------------------------------------
kpis <- maestra %>%
  summarise(
    ingresos          = sum(total),
    ventas            = n(),
    unidades          = sum(cantidad),
    ticket_promedio   = mean(total),
    clientes_unicos   = n_distinct(cliente_id),
    descuento_prom    = mean(descuento_pct),
    utilidad_bruta    = sum(utilidad),
    margen_bruto_pct  = utilidad_bruta / ingresos * 100
  ) %>%
  mutate(across(everything(), ~ round(.x, 2)))
glimpse(kpis)

# ---- Bloque 87 --------------------------------------------------------
por_mes <- maestra %>%
  group_by(mes_fecha, mes_nombre) %>%
  summarise(ingresos = sum(total), ventas = n(),
            clientes = n_distinct(cliente_id), .groups = "drop") %>%
  arrange(mes_fecha) %>%
  mutate(
    crecimiento_mom = round((ingresos / lag(ingresos) - 1) * 100, 1),
    acumulado_ytd   = cumsum(ingresos),
    ingresos        = round(ingresos, 2)
  )
por_mes %>% select(-mes_fecha)

# ---- Bloque 88 --------------------------------------------------------
por_categoria <- maestra %>%
  group_by(categoria) %>%
  summarise(ingresos = sum(total), unidades = sum(cantidad),
            utilidad = sum(utilidad), .groups = "drop") %>%
  mutate(
    participacion_pct = round(ingresos / sum(ingresos) * 100, 1),
    margen_pct        = round(utilidad / ingresos * 100, 1),
    ranking           = min_rank(desc(ingresos))
  ) %>%
  arrange(ranking)
por_categoria %>% select(ranking, categoria, ingresos,
                         participacion_pct, margen_pct)

# ---- Bloque 89 --------------------------------------------------------
top_clientes <- maestra %>%
  group_by(cliente_id, cliente, ciudad_cliente, es_premium) %>%
  summarise(compras = n(), ingresos = sum(total),
            ultima_compra = max(fecha), .groups = "drop") %>%
  slice_max(ingresos, n = 5)
top_clientes %>% select(cliente, es_premium, compras, ingresos,
                        ultima_compra)

ranking_tiendas <- maestra %>%
  group_by(nombre_tienda, ciudad_tienda) %>%
  summarise(ingresos = sum(total), ventas = n(),
            ticket_promedio = mean(total), .groups = "drop") %>%
  mutate(ranking = min_rank(desc(ingresos)),
         vs_promedio_pct = round((ingresos / mean(ingresos) - 1) * 100,
                                 1)) %>%
  arrange(ranking)
ranking_tiendas %>% select(ranking, nombre_tienda, ingresos, ventas,
                           vs_promedio_pct)

# ---- Bloque 90 --------------------------------------------------------
vendedores <- maestra %>%
  group_by(vendedor_id, vendedor) %>%
  summarise(ventas = n(), ingresos = sum(total),
            ticket_promedio = mean(total),
            tiendas_distintas = n_distinct(tienda_id), .groups = "drop") %>%
  mutate(ranking = min_rank(desc(ingresos))) %>%
  arrange(ranking)
bind_rows(head(vendedores, 3), tail(vendedores, 2)) %>%  # mejores/peores
  select(ranking, vendedor, ventas, ingresos, ticket_promedio)

# ---- Bloque 91 --------------------------------------------------------
dinamica <- maestra %>%
  group_by(mes_nombre, categoria) %>%
  summarise(ingresos = round(sum(total)), .groups = "drop") %>%
  pivot_wider(names_from = categoria, values_from = ingresos,
              values_fill = 0) %>%
  mutate(Total = rowSums(across(where(is.numeric))))  # total por mes
dinamica %>% select(mes_nombre, Belleza, Electrónica, Juguetes, Total)

# ---- Bloque 92 --------------------------------------------------------
calidad <- tibble(
  revision = c(
    "Productos con costo > precio de catálogo",
    "Ventas con precio unitario < costo",
    "Ventas con precio +/-10% vs. catálogo",
    "Ventas antes del registro del cliente",
    "Vendedores fuera del depto. de Ventas"
  ),
  casos = c(
    sum(productos$costo > productos$precio_catalogo),
    sum(maestra$precio_unitario < maestra$costo),
    sum(abs(maestra$precio_unitario / maestra$precio_catalogo - 1) > 0.10),
    maestra %>%
      left_join(select(clientes, cliente_id, fecha_registro),
                by = "cliente_id") %>%
      filter(fecha < fecha_registro) %>% nrow(),
    empleados %>%
      filter(empleado_id %in% ventas$vendedor_id,
             departamento != "Ventas") %>% nrow()
  ),
  de_un_total_de = c(nrow(productos), nrow(maestra), nrow(maestra),
                     nrow(maestra), n_distinct(ventas$vendedor_id))
)
calidad

# ---- Bloque 93 --------------------------------------------------------
dir.create("resultados", showWarnings = FALSE)   # carpeta de salida

hojas <- list(
  "KPIs"           = kpis,
  "Tendencia"      = por_mes,
  "Categorias"     = por_categoria,
  "Top clientes"   = top_clientes,
  "Tiendas"        = ranking_tiendas,
  "Vendedores"     = vendedores,
  "Mes x categoria" = dinamica %>%
                       mutate(mes_nombre = as.character(mes_nombre)),
  "Calidad datos"  = calidad
)
write_xlsx(hojas, "resultados/reporte_ventas_2023.xlsx")

# Comprobamos que el archivo existe y qué hojas tiene
file.exists("resultados/reporte_ventas_2023.xlsx")
readxl::excel_sheets("resultados/reporte_ventas_2023.xlsx")

# ---- Bloque 94 --------------------------------------------------------
ventas %>%
  left_join(productos, by = "producto_id") %>%      # LEFT JOIN
  filter(trimestre == "Q4") %>%                     # WHERE
  group_by(categoria) %>%                           # GROUP BY
  summarise(ingresos = sum(total), ventas = n(),    # SUM, COUNT
            .groups = "drop") %>%
  arrange(desc(ingresos)) %>%                       # ORDER BY DESC
  head(3)                                           # LIMIT 3

# ---- Solo referencia (no se ejecuta automáticamente) ----
# ventas %>%
#   filter(total > 4800)      # <- falta %>% al final de esta línea
#   arrange(desc(total))

# ---- Solo referencia (no se ejecuta automáticamente) ----
# ventas
#   %>% filter(total > 4800)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# ventas %>% filter(tienda_id = 3)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # Sesión nueva, sin library(tidyverse)
# ventas <- readr::read_csv("datasets/ventas_retail.csv",
#                           show_col_types = FALSE)
# filter(ventas, total > 4800)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# ventas %>% summarise(total = sum(Total))    # "Total" con mayúscula

# ---- Solo referencia (no se ejecuta automáticamente) ----
# reporte %>% select(Ventas Totales)

# ---- Bloque 95 --------------------------------------------------------
reporte <- tibble(`Ventas Totales` = c(100, 200),
                  `Sucursal ID` = c("A", "B"))
reporte %>%
  rename(ventas_totales = `Ventas Totales`,     # backticks
         sucursal_id    = `Sucursal ID`)

# ---- Bloque 96 --------------------------------------------------------
agrupada <- ventas %>% group_by(tienda_id)
agrupada %>% select(fecha, tienda_id, total) %>% head(2)

# Con grupos: slice_max() da la mejor venta DE CADA tienda (10 filas)
agrupada %>% slice_max(total, n = 1) %>% nrow()
# Sin grupos: la mejor venta del año (1 fila)
agrupada %>% ungroup() %>% slice_max(total, n = 1) %>% nrow()

# ---- Solo referencia (no se ejecuta automáticamente) ----
# ventas_ene %>% left_join(precios_compras, by = "producto_id")
# # ¿3 filas en lugar de 2? -> llave duplicada en la tabla derecha

# ---- Solo referencia (no se ejecuta automáticamente) ----
# a <- tibble(id = 1, monto = "100")    # monto como texto
# b <- tibble(id = 2, monto = 200)      # monto como número
# bind_rows(a, b)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# lista_proveedor <- tibble(
#   id        = c(1, 2, 2, 3, 4, 5, 6),
#   categoria = c("electronica", "Electrónica ", "Electrónica ",
#                 "HOGAR", "hogar", "Ropa", " ropa"),
#   precio    = c("$1,250.00", "$899.50", "$899.50", "450", "$0",
#                 "$320.75", "-15"),
#   fecha     = c("2023-05-01", "02/05/2023", "02/05/2023", "2023-05-03",
#                 "04/05/2023", "2023-05-05", "06/05/2023")
# )
