# ==========================================================================
# Proyecto final: TechRetail México
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/09-proyecto-final.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Bloque 1 --------------------------------------------------------
# ---- Paquetes (todos se explicaron en módulos anteriores) ----
library(tidyverse)   # dplyr, ggplot2, readr, tidyr, purrr, lubridate
library(scales)      # formatos de negocio: dollar(), percent(), comma()
library(DBI)         # interfaz a bases de datos (Módulo 5)
library(RSQLite)     # motor SQLite en un archivo local
library(cluster)     # silueta para evaluar k-means (Módulo 7)
library(forecast)    # modelos de pronóstico (Módulo 7)

# ---- Fecha de corte FIJA (nunca Sys.Date() en un proyecto) ----
fecha_corte <- as.Date("2024-01-01")

# ---- Carpetas de salida del proyecto ----
dir.create("resultados", showWarnings = FALSE)
dir.create("datos", showWarnings = FALSE)

# ---- Colores corporativos y tema (los mismos del Módulo 3) ----
color_principal <- "#2a78d6"   # una sola serie
color_resalte   <- "#eb6834"   # lo que queremos destacar
color_gris      <- "#b5b3ad"   # el resto
paleta_libro <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                  "#e87ba4", "#008300", "#4a3aa7", "#e34948")

tema_libro <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", color = "#0b0b0b"),
      plot.subtitle    = element_text(color = "#52514e"),
      plot.caption     = element_text(color = "#52514e", size = rel(0.8)),
      axis.title       = element_text(color = "#52514e"),
      axis.text        = element_text(color = "#52514e"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e6e5e1", linewidth = 0.3),
      legend.position  = "top",
      legend.title     = element_text(color = "#52514e")
    )
}

# Formato de pesos sin decimales para textos y tablas
pesos <- function(x) dollar(x, accuracy = 1)

# ---- Bloque 2 --------------------------------------------------------
tiendas <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)

# Antigüedad de cada sucursal calculada a la fecha de corte
tiendas %>%
  mutate(anios = floor(time_length(
           interval(fecha_apertura, fecha_corte), "years"))) %>%
  select(tienda = nombre_tienda, ciudad, zona, tipo,
         m2 = tamano_m2, empleados, anios) %>%
  arrange(desc(m2))

# ---- Bloque 3 --------------------------------------------------------
# Resumen de la red de sucursales
tiendas %>%
  summarise(sucursales = n(),
            ciudades   = n_distinct(ciudad),
            m2_totales = sum(tamano_m2),
            empleados  = sum(empleados),
            m2_por_empleado = round(sum(tamano_m2) / sum(empleados), 1))

tiendas %>% count(ciudad, sort = TRUE)

# ---- Bloque 4 --------------------------------------------------------
# Tabla de hechos candidata principal: una fila por transacción
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
# Segunda fuente de ventas (una venta registrada por día)
ventas_retail <- read_csv("datasets/ventas_retail.csv",
                          show_col_types = FALSE)
# Catálogos (futuras dimensiones)
clientes  <- read_csv("datasets/clientes.csv",  show_col_types = FALSE)
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)
empleados <- read_csv("datasets/empleados.csv", show_col_types = FALSE)
# tiendas ya se cargó en la sección anterior

glimpse(transacciones)

# ---- Bloque 5 --------------------------------------------------------
# Perfil rápido de cada fuente de ventas
perfil_fuente <- function(datos, nombre, col_total) {
  tibble(fuente   = nombre,
         filas    = nrow(datos),
         columnas = ncol(datos),
         desde    = min(datos$fecha),
         hasta    = max(datos$fecha),
         ventas   = sum(datos[[col_total]]))
}

bind_rows(
  perfil_fuente(transacciones, "transacciones", "total_transaccion"),
  perfil_fuente(ventas_retail, "ventas_retail", "total")
)

# ¿Qué información tiene cada fuente que la otra no?
setdiff(names(transacciones), names(ventas_retail))
setdiff(names(ventas_retail), names(transacciones))

# ¿Coinciden registros (misma fecha, cliente y producto)?
inner_join(transacciones, ventas_retail,
           by = c("fecha", "cliente_id", "producto_id")) %>%
  nrow()

# ---- Bloque 6 --------------------------------------------------------
# Auditoría básica de una tabla: tamaño, faltantes y duplicados
auditar <- function(datos, nombre, llave) {
  tibble(
    tabla      = nombre,
    filas      = nrow(datos),
    columnas   = ncol(datos),
    celdas_na  = sum(is.na(datos)),               # valores faltantes
    filas_dup  = sum(duplicated(datos)),          # filas idénticas
    llaves_dup = sum(duplicated(datos[[llave]]))  # llave repetida
  )
}

auditoria <- bind_rows(
  auditar(transacciones, "transacciones", "transaccion_id"),
  auditar(clientes,      "clientes",      "cliente_id"),
  auditar(productos,     "productos",     "producto_id"),
  auditar(tiendas,       "tiendas",       "tienda_id"),
  auditar(empleados,     "empleados",     "empleado_id")
)
auditoria

# ---- Bloque 7 --------------------------------------------------------
# Rangos de las columnas clave de la tabla de hechos
rango <- function(x) str_c(min(x), " a ", max(x))
transacciones %>%
  summarise(fecha    = rango(fecha),
            hora     = rango(hms::as_hms(hora)),
            cantidad = rango(cantidad),
            precio   = rango(precio_venta),
            total    = rango(total_transaccion)) %>%
  pivot_longer(everything(), names_to = "columna",
               values_to = "rango")

# ¿El total cuadra con cantidad x precio? (tolerancia de 1 centavo)
transacciones %>%
  summarise(totales_incorrectos =
              sum(abs(total_transaccion - cantidad * precio_venta) > 0.01))

# ---- Bloque 8 --------------------------------------------------------
# Llaves huérfanas: transacciones cuyo id no existe en el catálogo
tibble(
  llave = c("cliente_id", "producto_id", "tienda_id", "vendedor_id"),
  huerfanas = c(
    nrow(anti_join(transacciones, clientes,  by = "cliente_id")),
    nrow(anti_join(transacciones, productos, by = "producto_id")),
    nrow(anti_join(transacciones, tiendas,   by = "tienda_id")),
    nrow(anti_join(transacciones, empleados,
                   by = c("vendedor_id" = "empleado_id")))
  )
)

# En sentido contrario: elementos del catálogo sin ninguna venta
clientes_sin_compra <- anti_join(clientes, transacciones,
                                 by = "cliente_id")
clientes_sin_compra %>% select(cliente_id, nombre, fecha_registro)
nrow(anti_join(productos, transacciones, by = "producto_id"))

# ---- Bloque 9 --------------------------------------------------------
# Unimos a cada transacción los datos del producto y del cliente
revision <- transacciones %>%
  left_join(productos, by = "producto_id") %>%
  left_join(select(clientes, cliente_id, fecha_registro),
            by = "cliente_id")

vendedores_ids <- unique(transacciones$vendedor_id)

hallazgos <- tibble(
  regla = c(
    "Productos con margen de catálogo negativo",
    "Transacciones con precio menor al costo",
    "Transacciones a +/-20% del precio de catálogo",
    "Productos inactivos que sí se vendieron",
    "Transacciones de productos inactivos",
    "Transacciones anteriores al registro del cliente",
    "Clientes con compras antes de registrarse",
    "Vendedores que no son del depto. de Ventas"),
  casos = c(
    sum(productos$margen_pct < 0),
    sum(revision$precio_venta < revision$costo),
    sum(abs(revision$precio_venta / revision$precio_catalogo - 1)
        <= 0.20),
    n_distinct(revision$producto_id[!revision$activo]),
    sum(!revision$activo),
    sum(revision$fecha < revision$fecha_registro),
    n_distinct(revision$cliente_id[revision$fecha <
                                     revision$fecha_registro]),
    empleados %>%
      filter(empleado_id %in% vendedores_ids,
             departamento != "Ventas") %>% nrow())
)
hallazgos

# ---- Bloque 10 --------------------------------------------------------
# Detalle: productos cuyo costo supera al precio de catálogo
productos %>%
  filter(margen_pct < 0) %>%
  select(producto_id, categoria, precio_catalogo, costo, margen_pct,
         activo) %>%
  arrange(margen_pct)

# ¿Qué tan lejos está el precio de venta del de catálogo?
revision %>%
  mutate(razon_precio = precio_venta / precio_catalogo) %>%
  summarise(minimo  = min(razon_precio),
            mediana = median(razon_precio),
            maximo  = max(razon_precio))

# ¿A qué departamento pertenecen los vendedores según Recursos Humanos?
empleados %>%
  filter(empleado_id %in% vendedores_ids) %>%
  count(departamento, sort = TRUE)

# ---- Bloque 11 --------------------------------------------------------
# ---- Dimensión cliente (sin datos personales) ----
dim_cliente <- clientes %>%
  select(cliente_id, nombre, edad, genero, ciudad, segmento_edad = segmento,
         es_premium, fecha_registro) %>%
  mutate(con_compras = cliente_id %in% transacciones$cliente_id)

# ---- Dimensión producto (con banderas de calidad) ----
dim_producto <- productos %>%
  select(producto_id, nombre_producto, categoria, subcategoria, marca,
         proveedor, precio_catalogo, costo, margen_pct, activo,
         stock_actual, stock_minimo, estado_stock) %>%
  mutate(margen_negativo = margen_pct < 0)

# ---- Dimensión tienda (antigüedad a la fecha de corte) ----
dim_tienda <- tiendas %>%
  rename(num_empleados = empleados) %>%
  mutate(anios_operacion = floor(time_length(
           interval(fecha_apertura, fecha_corte), "years")))

# ---- Dimensión vendedor (solo los 25 que aparecen en ventas) ----
dim_vendedor <- empleados %>%
  filter(empleado_id %in% vendedores_ids) %>%
  select(vendedor_id = empleado_id, nombre_vendedor = nombre,
         departamento, puesto, salario_mensual, antiguedad_anos) %>%
  mutate(depto_ventas = departamento == "Ventas")

# ---- Dimensión fecha (calendario completo de 2023) ----
dias_es <- c("lunes", "martes", "miércoles", "jueves", "viernes",
             "sábado", "domingo")
dim_fecha <- tibble(fecha = seq(as.Date("2023-01-01"),
                                as.Date("2023-12-31"), by = "day")) %>%
  mutate(anio       = year(fecha),
         mes        = month(fecha),
         trimestre  = quarter(fecha),
         semana_iso = isoweek(fecha),
         dia_semana = factor(dias_es[wday(fecha, week_start = 1)],
                             levels = dias_es),
         fin_semana = wday(fecha, week_start = 1) >= 6)

map_int(list(dim_cliente, dim_producto, dim_tienda, dim_vendedor,
             dim_fecha), nrow)

# ---- Bloque 12 --------------------------------------------------------
hechos_ventas <- transacciones %>%
  select(transaccion_id, fecha, hora, cliente_id, producto_id,
         tienda_id, vendedor_id, metodo_pago, cantidad, precio_venta,
         total = total_transaccion) %>%
  # Costo unitario vigente y banderas que vienen de los catálogos
  left_join(select(productos, producto_id, costo_unitario = costo,
                   activo), by = "producto_id") %>%
  left_join(select(clientes, cliente_id, fecha_registro),
            by = "cliente_id") %>%
  mutate(
    hora_dia          = hour(hora),                 # 8, 9, ..., 20
    costo_total       = cantidad * costo_unitario,
    margen_estimado   = total - costo_total,
    bajo_costo        = precio_venta < costo_unitario,
    producto_inactivo = !activo,
    antes_registro    = fecha < fecha_registro
  ) %>%
  select(-activo, -fecha_registro)

# Control: un join mal hecho puede duplicar filas sin avisar
stopifnot(nrow(hechos_ventas) == nrow(transacciones))
stopifnot(sum(hechos_ventas$total) == sum(transacciones$total_transaccion))

hechos_ventas %>%
  summarise(filas = n(), ventas = sum(total),
            margen_estimado = sum(margen_estimado),
            bajo_costo = sum(bajo_costo),
            inactivos = sum(producto_inactivo),
            antes_registro = sum(antes_registro))

# ---- Bloque 13 --------------------------------------------------------
# ---- RDS: una lista con todas las tablas del modelo ----
modelo <- list(hechos_ventas = hechos_ventas,
               dim_cliente   = dim_cliente,
               dim_producto  = dim_producto,
               dim_tienda    = dim_tienda,
               dim_vendedor  = dim_vendedor,
               dim_fecha     = dim_fecha)
saveRDS(modelo, "resultados/modelo_techretail.rds")

# ---- SQLite: fechas y horas como texto para no perder el formato ----
con <- dbConnect(RSQLite::SQLite(), "datos/techretail.sqlite")
walk2(modelo, names(modelo), function(tabla, nombre) {
  tabla <- tabla %>%
    mutate(across(where(is.Date), as.character),
           across(where(hms::is_hms), as.character),
           across(where(is.factor), as.character))
  dbWriteTable(con, nombre, tabla, overwrite = TRUE)
})
dbListTables(con)

# Comprobación con SQL: ventas por zona directamente en la base
dbGetQuery(con, "
  SELECT t.zona, COUNT(*) AS transacciones,
         ROUND(SUM(h.total), 0) AS ventas
  FROM hechos_ventas h
  JOIN dim_tienda t ON h.tienda_id = t.tienda_id
  GROUP BY t.zona
  ORDER BY ventas DESC")
dbDisconnect(con)

# ---- Bloque 14 --------------------------------------------------------
ventas <- hechos_ventas %>%
  left_join(select(dim_producto, producto_id, nombre_producto,
                   categoria), by = "producto_id") %>%
  left_join(select(dim_tienda, tienda_id, nombre_tienda, zona, tipo,
                   tamano_m2), by = "tienda_id") %>%
  left_join(select(dim_cliente, cliente_id, es_premium),
            by = "cliente_id") %>%
  left_join(select(dim_fecha, fecha, mes, dia_semana), by = "fecha")

stopifnot(nrow(ventas) == nrow(hechos_ventas))   # conciliación

# ---- Bloque 15 --------------------------------------------------------
kpis <- ventas %>%
  summarise(ventas_totales  = sum(total),
            transacciones   = n(),
            ticket_promedio = mean(total),
            unidades        = sum(cantidad),
            clientes        = n_distinct(cliente_id),
            margen          = sum(margen_estimado))

tabla_kpis <- tibble(
  kpi = c("Ventas totales", "Transacciones", "Ticket promedio",
          "Unidades vendidas", "Unidades por transacción",
          "Clientes activos", "Base de clientes activa",
          "Venta anual por cliente activo", "Margen bruto estimado",
          "Margen estimado (%)"),
  valor = c(pesos(kpis$ventas_totales),
            comma(kpis$transacciones),
            dollar(kpis$ticket_promedio, accuracy = 0.01),
            comma(kpis$unidades),
            number(kpis$unidades / kpis$transacciones, accuracy = 0.01),
            comma(kpis$clientes),
            percent(kpis$clientes / nrow(dim_cliente), accuracy = 0.1),
            pesos(kpis$ventas_totales / kpis$clientes),
            pesos(kpis$margen),
            percent(kpis$margen / kpis$ventas_totales, accuracy = 0.1))
)
tabla_kpis

# ---- Bloque 16 --------------------------------------------------------
meses_es <- c("ene", "feb", "mar", "abr", "may", "jun",
              "jul", "ago", "sep", "oct", "nov", "dic")

ventas_mes <- ventas %>%
  group_by(mes) %>%
  summarise(ventas = sum(total), transacciones = n(),
            ticket = mean(total), .groups = "drop") %>%
  mutate(crecimiento = ventas / lag(ventas) - 1,          # vs mes previo
         nombre_mes  = factor(meses_es[mes], levels = meses_es))

ventas_mes %>%
  transmute(nombre_mes, ventas = round(ventas), transacciones,
            ticket = round(ticket), crecimiento = percent(crecimiento, 0.1))

# ---- Bloque 17 --------------------------------------------------------
promedio_mes <- mean(ventas_mes$ventas)
extremos <- ventas_mes %>%
  filter(ventas %in% range(ventas)) %>%
  mutate(ajuste = if_else(ventas == max(ventas), -1.1, 2))  # arriba/abajo

ggplot(ventas_mes, aes(x = nombre_mes, y = ventas, group = 1)) +
  geom_hline(yintercept = promedio_mes, linetype = "dashed",
             color = color_gris, linewidth = 0.6) +
  geom_line(color = color_principal, linewidth = 0.8) +
  geom_point(color = color_principal, size = 2) +
  geom_point(data = extremos, color = color_resalte, size = 3) +
  geom_text(data = extremos, aes(label = pesos(ventas), vjust = ajuste),
            color = color_resalte, size = 3.5) +
  scale_y_continuous(labels = dollar, limits = c(0, 110000)) +
  labs(title = "Ventas mensuales de TechRetail, 2023",
       subtitle = str_c("Mucha variación y sin tendencia; línea punteada = ",
                        "promedio mensual (", pesos(promedio_mes), ")"),
       x = NULL, y = "Ventas") +
  tema_libro()

# ---- Bloque 18 --------------------------------------------------------
ventas_categoria <- ventas %>%
  group_by(categoria) %>%
  summarise(ventas = sum(total), margen = sum(margen_estimado),
            .groups = "drop") %>%
  mutate(participacion = ventas / sum(ventas),
         pct_margen    = margen / ventas) %>%
  arrange(desc(ventas))

ventas_categoria %>%
  mutate(across(c(ventas, margen), round),
         across(c(participacion, pct_margen), ~ percent(.x, 0.1)))

# ---- Bloque 19 --------------------------------------------------------
ventas_categoria %>%
  mutate(grupo = if_else(pct_margen < 0.25, "Margen estimado < 25%",
                         "Margen estimado de 25% o más"),
         etiqueta = if_else(pct_margen < 0.25,
                            str_c("margen ", percent(pct_margen, 1)), "")) %>%
  ggplot(aes(x = ventas, y = fct_reorder(categoria, ventas),
             fill = grupo)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = etiqueta), hjust = -0.1, size = 3.3,
            color = color_resalte) +
  scale_fill_manual(values = c("Margen estimado < 25%" = color_resalte,
                               "Margen estimado de 25% o más" = color_gris)) +
  scale_x_continuous(labels = dollar, expand = expansion(c(0, 0.18))) +
  labs(title = "Ventas por categoría, 2023",
       subtitle = "Hogar y Ropa dejan el menor margen estimado",
       x = "Ventas", y = NULL, fill = NULL) +
  tema_libro()

# ---- Bloque 20 --------------------------------------------------------
ventas_tienda <- ventas %>%
  group_by(nombre_tienda, zona, tipo, tamano_m2) %>%
  summarise(ventas = sum(total), transacciones = n(),
            ticket = mean(total), .groups = "drop") %>%
  arrange(desc(ventas))

ventas_tienda %>% mutate(across(c(ventas, ticket), round))

# Por zona: total y promedio por sucursal (las zonas tienen
# distinto número de tiendas)
ventas_tienda %>%
  group_by(zona) %>%
  summarise(tiendas = n(),
            ventas_por_tienda = round(mean(ventas)),   # antes de sumar
            ventas_zona = round(sum(ventas)), .groups = "drop") %>%
  arrange(desc(ventas_por_tienda))

# ---- Bloque 21 --------------------------------------------------------
ventas_tienda %>%
  mutate(tienda  = str_c(nombre_tienda, " (", tamano_m2, " m²)"),
         formato = if_else(tamano_m2 < 400, "Menos de 400 m²",
                           "400 m² o más")) %>%
  ggplot(aes(x = ventas, y = fct_reorder(tienda, ventas),
             fill = formato)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("Menos de 400 m²" = color_resalte,
                               "400 m² o más" = color_gris)) +
  scale_x_continuous(labels = dollar, expand = expansion(c(0, 0.1))) +
  labs(title = "Ventas por sucursal, 2023",
       subtitle = "Las dos tiendas más chicas están entre las 4 que más venden",
       x = "Ventas", y = NULL, fill = "Tamaño") +
  tema_libro()

# ---- Bloque 22 --------------------------------------------------------
ventas %>%
  group_by(metodo_pago) %>%
  summarise(transacciones = n(), ventas = sum(total),
            ticket = mean(total), .groups = "drop") %>%
  mutate(pct_ventas = percent(ventas / sum(ventas), 0.1),
         across(c(ventas, ticket), round)) %>%
  arrange(desc(ventas))

# ¿El ticket difiere según el método de pago? (Módulo 4)
kruskal.test(total ~ metodo_pago, data = ventas)$p.value

# ---- Bloque 23 --------------------------------------------------------
# Ventas por día de la semana (dia_semana ya es un factor ordenado)
ventas %>%
  group_by(dia_semana) %>%
  summarise(transacciones = n(), ventas = round(sum(total)),
            .groups = "drop")

# Mapa de calor día x hora
calor <- ventas %>%
  count(dia_semana, hora_dia, wt = total, name = "ventas")

ggplot(calor, aes(x = factor(hora_dia), y = fct_rev(dia_semana),
                  fill = ventas)) +
  geom_tile(color = "white", linewidth = 0.6) +
  scale_fill_gradient(low = "#e3eefb", high = "#0f4c92",
                      labels = dollar) +
  labs(title = "Ventas por día de la semana y hora, 2023",
       subtitle = "Sin horas pico claras: las celdas oscuras están dispersas",
       x = "Hora del día", y = NULL, fill = "Ventas") +
  tema_libro() +
  theme(panel.grid.major = element_blank(),
        legend.key.width = unit(1.5, "cm"))

# ---- Bloque 24 --------------------------------------------------------
pareto <- ventas %>%
  group_by(producto_id, nombre_producto, categoria) %>%
  summarise(ventas = sum(total), .groups = "drop") %>%
  arrange(desc(ventas)) %>%
  mutate(rango         = row_number(),
         pct_productos = rango / n(),
         pct_acumulado = cumsum(ventas) / sum(ventas))

pareto %>%
  summarise(
    ventas_top20pct  = percent(max(pct_acumulado[pct_productos <= 0.2]),
                               0.1),
    productos_al_80  = min(rango[pct_acumulado >= 0.8]),
    mayor_vs_menor   = round(max(ventas) / min(ventas), 1))

# ---- Bloque 25 --------------------------------------------------------
ggplot(pareto, aes(x = pct_productos, y = pct_acumulado)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = color_gris, linewidth = 0.6) +
  geom_hline(yintercept = 0.8, linetype = "dotted", color = color_gris) +
  geom_vline(xintercept = 0.2, linetype = "dotted", color = color_gris) +
  geom_line(color = color_principal, linewidth = 0.8) +
  annotate("point", x = 0.2, y = 0.264, color = color_resalte, size = 3) +
  annotate("text", x = 0.24, y = 0.12, hjust = 0, size = 3.4,
           color = color_resalte,
           label = "El 20% de los productos genera 26% de las ventas") +
  annotate("text", x = 0.62, y = 0.52, hjust = 0, size = 3,
           color = "#52514e", label = "Reparto perfectamente igual") +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(title = "Curva de Pareto de productos, 2023",
       subtitle = "Las ventas están repartidas: no hay un grupo de productos estrella",
       x = "Productos (ordenados de mayor a menor venta)",
       y = "Ventas acumuladas") +
  tema_libro()

# ---- Bloque 26 --------------------------------------------------------
ventas_vendedor <- ventas %>%
  group_by(vendedor_id) %>%
  summarise(ventas = sum(total), transacciones = n(),
            .groups = "drop") %>%
  left_join(dim_vendedor, by = "vendedor_id") %>%
  arrange(desc(ventas))

ventas_vendedor %>%
  select(vendedor_id, departamento, puesto, ventas, transacciones,
         salario_mensual) %>%
  mutate(ventas = round(ventas)) %>%
  slice(c(1:3, 23:25))           # los 3 mejores y los 3 últimos

# ¿Venden más quienes ganan más? (correlación, Módulo 4)
prueba_salario <- cor.test(ventas_vendedor$ventas,
                           ventas_vendedor$salario_mensual)
c(r = round(unname(prueba_salario$estimate), 3),
  p_valor = round(prueba_salario$p.value, 3))

# ---- Bloque 27 --------------------------------------------------------
ventas_vendedor %>%
  mutate(grupo = if_else(depto_ventas, "Depto. de Ventas",
                         "Otro departamento")) %>%
  ggplot(aes(x = salario_mensual, y = ventas, color = grupo)) +
  geom_point(size = 2.8) +
  scale_color_manual(values = c("Depto. de Ventas" = color_resalte,
                                "Otro departamento" = color_gris)) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Ventas anuales contra salario mensual por vendedor",
       subtitle = "Sin relación (r = -0.14, p = 0.51); dato de RR. HH. por validar",
       x = "Salario mensual", y = "Ventas 2023", color = NULL) +
  tema_libro()

# ---- Bloque 28 --------------------------------------------------------
gasto_cliente <- ventas %>%
  group_by(cliente_id, es_premium) %>%
  summarise(gasto = sum(total), compras = n(), .groups = "drop") %>%
  mutate(grupo = if_else(es_premium, "Premium", "No premium"))

gasto_cliente %>%
  group_by(grupo) %>%
  summarise(clientes = n(), gasto_promedio = round(mean(gasto)),
            gasto_mediano = round(median(gasto)),
            compras_promedio = round(mean(compras), 2), .groups = "drop")

# Prueba t de Welch y, como respaldo, Wilcoxon (no paramétrica)
t.test(gasto ~ grupo, data = gasto_cliente)
wilcox.test(gasto ~ grupo, data = gasto_cliente)$p.value

# ---- Bloque 29 --------------------------------------------------------
ggplot(gasto_cliente, aes(x = grupo, y = gasto)) +
  geom_boxplot(fill = color_principal, alpha = 0.25, width = 0.5,
               color = color_principal, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.5,
              color = color_principal) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4,
               color = color_resalte) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Gasto anual por cliente: premium contra no premium",
       subtitle = "Distribuciones casi idénticas (rombo naranja = promedio)",
       x = NULL, y = "Gasto en 2023") +
  tema_libro()

# ---- Bloque 30 --------------------------------------------------------
rfm <- hechos_ventas %>%
  group_by(cliente_id) %>%
  summarise(recencia   = as.numeric(fecha_corte - max(fecha)),
            frecuencia = n(),
            monto      = sum(total), .groups = "drop")

summary(select(rfm, -cliente_id))

# ---- Bloque 31 --------------------------------------------------------
rfm_escalado <- scale(select(rfm, recencia, frecuencia, monto))

set.seed(123)
evaluacion_k <- map_dfr(2:7, function(k) {
  modelo_k <- kmeans(rfm_escalado, centers = k, nstart = 25)
  tibble(k = k,
         suma_cuadrados = round(modelo_k$tot.withinss, 1),
         silueta = round(mean(silhouette(modelo_k$cluster,
                                         dist(rfm_escalado))[, 3]), 3))
})
evaluacion_k

# ---- Bloque 32 --------------------------------------------------------
set.seed(123)
km_rfm <- kmeans(rfm_escalado, centers = 4, nstart = 25)

perfil <- rfm %>%
  mutate(cluster = km_rfm$cluster) %>%
  group_by(cluster) %>%
  summarise(clientes = n(), recencia = mean(recencia),
            frecuencia = mean(frecuencia), monto = mean(monto),
            .groups = "drop")

# Nombres de negocio a partir del perfil (no del número de cluster,
# que puede cambiar entre versiones de R)
perfil <- perfil %>%
  mutate(segmento = case_when(
    monto == max(monto)       ~ "Campeones",
    recencia == max(recencia) ~ "En riesgo",
    monto == max(monto[!monto == max(monto) &
                         !recencia == max(recencia)]) ~ "Leales",
    TRUE                      ~ "Ocasionales"))

rfm <- rfm %>%
  mutate(cluster = km_rfm$cluster) %>%
  left_join(select(perfil, cluster, segmento), by = "cluster")

perfil %>%
  mutate(pct_clientes = percent(clientes / sum(clientes), 1),
         pct_ventas   = percent(clientes * monto / sum(clientes * monto),
                                1),
         across(c(recencia, frecuencia), ~ round(.x, 1)),
         monto = round(monto)) %>%
  select(segmento, clientes, pct_clientes, recencia, frecuencia, monto,
         pct_ventas) %>%
  arrange(desc(monto))

# ---- Bloque 33 --------------------------------------------------------
ggplot(rfm, aes(x = recencia, y = monto, color = segmento)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_manual(values = c("Campeones" = paleta_libro[3],
                                "Leales" = paleta_libro[1],
                                "Ocasionales" = paleta_libro[4],
                                "En riesgo" = paleta_libro[2])) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Segmentos de clientes según RFM (k-means, k = 4)",
       subtitle = "A la derecha, clientes que llevan meses sin comprar",
       x = "Recencia (días desde la última compra)",
       y = "Gasto en 2023", color = NULL) +
  tema_libro()

# ---- Bloque 34 --------------------------------------------------------
ventas_mes_ts <- ts(ventas_mes$ventas, start = c(2023, 1),
                    frequency = 12)
# Intentar descomponer tendencia + estacionalidad con 12 meses
tryCatch(stl(ventas_mes_ts, s.window = "periodic"),
         error = function(e) conditionMessage(e))

# ---- Bloque 35 --------------------------------------------------------
# Semanas completas de lunes a domingo (el 1 de enero de 2023 fue
# domingo: lo dejamos fuera para no tener una semana de un solo día)
ventas_semana <- hechos_ventas %>%
  filter(fecha >= as.Date("2023-01-02")) %>%
  mutate(semana = floor_date(fecha, "week", week_start = 1)) %>%
  count(semana, wt = total, name = "ventas")

nrow(ventas_semana)
round(c(promedio = mean(ventas_semana$ventas),
        desv_est = sd(ventas_semana$ventas),
        cv = sd(ventas_semana$ventas) / mean(ventas_semana$ventas)), 2)

# ¿Hay autocorrelación? (si no, la serie se comporta como ruido)
Box.test(ventas_semana$ventas, lag = 10, type = "Ljung-Box")$p.value

# ---- Bloque 36 --------------------------------------------------------
serie <- ts(ventas_semana$ventas)
entrenamiento <- window(serie, end = 44)
prueba        <- window(serie, start = 45)

modelos <- list(
  promedio = meanf(entrenamiento, h = 8),   # el promedio histórico
  ingenuo  = naive(entrenamiento, h = 8),   # "igual que la última semana"
  ets      = forecast(ets(entrenamiento), h = 8)
)
map_dfr(modelos, ~ as_tibble(t(accuracy(.x, prueba)["Test set",
                                c("RMSE", "MAE", "MAPE")])),
        .id = "modelo") %>%
  mutate(across(-modelo, ~ round(.x, 1)))

# ---- Bloque 37 --------------------------------------------------------
# Pronóstico final con todas las semanas
modelo_final <- ets(serie)
pronostico <- forecast(modelo_final, h = 8, level = c(80, 95))
modelo_final$method

tabla_pronostico <- tibble(
  semana   = max(ventas_semana$semana) + weeks(1:8),
  estimado = as.numeric(pronostico$mean),
  inf_80 = as.numeric(pronostico$lower[, "80%"]),
  sup_80 = as.numeric(pronostico$upper[, "80%"]),
  inf_95 = as.numeric(pronostico$lower[, "95%"]),
  sup_95 = as.numeric(pronostico$upper[, "95%"]))

tabla_pronostico %>%
  mutate(across(-semana, round)) %>%
  slice(1:3)

# ---- Bloque 38 --------------------------------------------------------
ggplot() +
  geom_ribbon(data = tabla_pronostico,
              aes(x = semana, ymin = inf_95, ymax = sup_95),
              fill = color_resalte, alpha = 0.15) +
  geom_ribbon(data = tabla_pronostico,
              aes(x = semana, ymin = inf_80, ymax = sup_80),
              fill = color_resalte, alpha = 0.25) +
  geom_line(data = ventas_semana, aes(x = semana, y = ventas),
            color = color_principal, linewidth = 0.8) +
  geom_line(data = tabla_pronostico, aes(x = semana, y = estimado),
            color = color_resalte, linewidth = 0.8) +
  annotate("text", x = as.Date("2024-02-19"), y = 29000, size = 3.2,
           hjust = 1, vjust = 0, color = color_resalte,
           label = "Pronóstico e intervalos de 80% y 95%") +
  scale_y_continuous(labels = dollar, limits = c(0, NA)) +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") +
  labs(title = "Ventas semanales y pronóstico a 8 semanas",
       subtitle = "Sin tendencia ni patrón: el pronóstico es un nivel constante",
       x = NULL, y = "Ventas por semana") +
  tema_libro()

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # ---- Cambio 1: cargar el modelo en estrella de la Fase 1 ----
# cargar_datos_dashboard <- function(ruta = "resultados/modelo_techretail.rds") {
#   modelo <- readRDS(ruta)
#   modelo$hechos_ventas %>%
#     left_join(select(modelo$dim_producto, producto_id, nombre_producto,
#                      categoria), by = "producto_id") %>%
#     left_join(transmute(modelo$dim_tienda, tienda_id, zona, ciudad,
#                         tienda = sub("Sucursal ", "", nombre_tienda)),
#               by = "tienda_id") %>%
#     left_join(select(modelo$dim_cliente, cliente_id,
#                      nombre_cliente = nombre, segmento = segmento_edad,
#                      es_premium), by = "cliente_id") %>%
#     mutate(mes = floor_date(fecha, "month")) %>%
#     rename(total_transaccion = total, costo = costo_unitario)
# }
#
# # ---- Cambio 2: filtro de zona (en app.R, dentro de dashboardSidebar) ----
# selectizeInput("zonas", "Zonas:", choices = sort(unique(datos$zona)),
#                multiple = TRUE, options = list(placeholder = "Todas"))
#
# # ... y en filtrar_datos(), un argumento más:
# filtrar_datos <- function(datos, fechas, tiendas = NULL,
#                           categorias = NULL, zonas = NULL) {
#   datos <- filter(datos, fecha >= fechas[1], fecha <= fechas[2])
#   if (length(zonas) > 0)      datos <- filter(datos, zona %in% zonas)
#   if (length(tiendas) > 0)    datos <- filter(datos, tienda %in% tiendas)
#   if (length(categorias) > 0) datos <- filter(datos, categoria %in% categorias)
#   datos
# }
#
# # ---- Cambio 3: segmentos RFM guardados en la Fase 3 ----
# segmentos_rfm <- readRDS("resultados/segmentos_rfm.rds")
# output$graf_rfm <- renderPlot({
#   datos_validos() %>%
#     distinct(cliente_id) %>%
#     inner_join(segmentos_rfm, by = "cliente_id") %>%
#     count(segmento) %>%
#     ggplot(aes(x = n, y = reorder(segmento, n))) +
#     geom_col(fill = color_principal) +
#     labs(x = "Clientes", y = NULL) +
#     tema_dashboard()
# }, res = 96)

# ---- Bloque 39 --------------------------------------------------------
saveRDS(select(rfm, cliente_id, recencia, frecuencia, monto, segmento),
        "resultados/segmentos_rfm.rds")
saveRDS(tabla_pronostico, "resultados/pronostico_semanal.rds")
file.exists(c("resultados/segmentos_rfm.rds",
              "resultados/pronostico_semanal.rds"))

# ---- Bloque 40 --------------------------------------------------------
# Insumos del resumen: todos salen de objetos calculados antes
mejor_mes <- ventas_mes %>% slice_max(ventas, n = 1)
peor_mes  <- ventas_mes %>% slice_min(ventas, n = 1)
top_cat   <- ventas_categoria %>% slice_max(ventas, n = 1)
top_tienda <- ventas_tienda %>% slice_max(ventas, n = 1)
p_premium <- t.test(gasto ~ grupo, data = gasto_cliente)$p.value
en_riesgo <- perfil %>% filter(segmento == "En riesgo")
meses_largos <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
                  "julio", "agosto", "septiembre", "octubre",
                  "noviembre", "diciembre")

resumen_ejecutivo <- c(
  str_glue("SITUACIÓN. En 2023 TechRetail vendió {pesos(kpis$ventas_totales)} ",
           "en {comma(kpis$transacciones)} transacciones (ticket promedio ",
           "de {pesos(kpis$ticket_promedio)}) a {kpis$clientes} clientes, ",
           "con un margen bruto estimado de ",
           "{percent(kpis$margen / kpis$ventas_totales, 0.1)}."),
  str_glue("VENTAS. Sin tendencia anual: el mejor mes fue ",
           "{meses_largos[mejor_mes$mes]} ({pesos(mejor_mes$ventas)}) y el ",
           "peor {meses_largos[peor_mes$mes]} ({pesos(peor_mes$ventas)})."),
  str_glue("PRODUCTOS. {top_cat$categoria} lidera con ",
           "{percent(top_cat$participacion, 0.1)} de las ventas, pero con ",
           "margen de {percent(top_cat$pct_margen, 0.1)}; ",
           "{sum(productos$margen_pct < 0)} productos tienen costo mayor ",
           "al precio de catálogo."),
  str_glue("TIENDAS. La que más vende es {top_tienda$nombre_tienda} ",
           "({pesos(top_tienda$ventas)}) con solo {top_tienda$tamano_m2} m²."),
  str_glue("CLIENTES. Los clientes premium no gastan más que el resto ",
           "(p = {round(p_premium, 2)}); {en_riesgo$clientes} clientes ",
           "están en riesgo (en promedio {round(en_riesgo$recencia)} días ",
           "sin comprar).")
)
cat(strwrap(resumen_ejecutivo, width = 68, exdent = 2), sep = "\n")

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # ========================================================================
# # 00_ejecutar_todo.R - Reconstruye TODO el proyecto TechRetail
# # Ejecuta con el directorio de trabajo en la raíz del proyecto
# # (abre techretail-bi.Rproj y corre: source("R/00_ejecutar_todo.R"))
# # ========================================================================
# inicio <- Sys.time()
#
# # Un solo lugar para los parámetros del proyecto
# fecha_corte <- as.Date("2024-01-01")   # fija: resultados reproducibles
# set.seed(123)                          # k-means y cualquier muestreo
#
# fases <- c("R/funciones.R",            # funciones compartidas
#            "R/01_etl.R",               # -> resultados/modelo_techretail.rds
#            "R/02_kpis_eda.R",          # -> resultados/figuras/
#            "R/03_modelos.R")           # -> segmentos y pronóstico
#
# for (fase in fases) {
#   cat("\n==>", fase, "\n")
#   t0 <- Sys.time()
#   source(fase, encoding = "UTF-8", echo = FALSE)
#   cat("    listo en", round(difftime(Sys.time(), t0, units = "secs"), 1),
#       "segundos\n")
# }
#
# # Fase 5: el reporte se genera con los resultados recién calculados
# rmarkdown::render("reportes/reporte_ejecutivo.Rmd",
#                   output_dir = "resultados",
#                   knit_root_dir = getwd())
#
# # Registro de la sesión: versiones de R y paquetes usados
# writeLines(capture.output(sessionInfo()), "resultados/session_info.txt")
# cat("\nProyecto completo en",
#     round(difftime(Sys.time(), inicio, units = "mins"), 1), "minutos\n")
#
# # Fase 4: el tablero se abre aparte (queda esperando en el navegador)
# # shiny::runApp("dashboard")
