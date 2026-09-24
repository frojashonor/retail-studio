# ==========================================================================
# sol-02.tex
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/sol-02.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Bloque 1 --------------------------------------------------------
library(tidyverse)

# ---- Bloque 2 --------------------------------------------------------
ventas <- read_csv("datasets/ventas_retail.csv", show_col_types = FALSE)

tienda3_desc <- ventas %>%
  filter(tienda_id == 3, descuento_pct >= 10) %>%   # ambas condiciones
  select(fecha, producto_id, descuento_pct, total) %>%
  arrange(desc(total))
tienda3_desc

tienda3_desc %>%
  summarise(ventas = n(), total = sum(total))

# ---- Bloque 3 --------------------------------------------------------
library(tidyverse)
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)

resurtir <- productos %>%
  filter(activo, stock_actual < stock_minimo) %>%
  mutate(faltante = stock_minimo - stock_actual) %>%
  select(nombre_producto, proveedor, stock_actual, stock_minimo,
         faltante) %>%
  arrange(desc(faltante))
resurtir
resurtir %>% count(proveedor, wt = faltante, sort = TRUE)

# ---- Bloque 4 --------------------------------------------------------
library(tidyverse)
clientes <- read_csv("datasets/clientes.csv", show_col_types = FALSE)

clientes %>%
  mutate(rango_edad = case_when(
    edad < 30 ~ "18-29",
    edad < 45 ~ "30-44",
    edad < 60 ~ "45-59",
    .default  = "60+"
  )) %>%
  count(rango_edad) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

# ---- Bloque 5 --------------------------------------------------------
library(tidyverse)
clientes <- read_csv("datasets/clientes.csv", show_col_types = FALSE)

perfil_ciudad <- clientes %>%
  group_by(ciudad) %>%
  summarise(clientes    = n(),
            pct_premium = round(mean(es_premium) * 100, 1),
            edad_prom   = round(mean(edad), 1),
            .groups = "drop") %>%
  arrange(desc(clientes))
perfil_ciudad
perfil_ciudad %>% slice_max(pct_premium, n = 1)

# ---- Bloque 6 --------------------------------------------------------
library(tidyverse)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)

transacciones %>%
  mutate(dia = wday(fecha, label = TRUE, abbr = FALSE)) %>%
  group_by(dia) %>%
  summarise(transacciones = n(),
            ingresos = sum(total_transaccion),
            ticket_promedio = round(mean(total_transaccion), 2),
            .groups = "drop") %>%
  mutate(participacion_pct = round(ingresos / sum(ingresos) * 100, 1))

# ---- Bloque 7 --------------------------------------------------------
library(tidyverse)
empleados <- read_csv("datasets/empleados.csv", show_col_types = FALSE)
fecha_corte <- as.Date("2024-01-01")

empleados_antig <- empleados %>%
  mutate(
    antig_calc = time_length(interval(fecha_ingreso, fecha_corte),
                             "years"),
    diferencia = abs(antig_calc - antiguedad_anos),
    grupo = case_when(
      antig_calc < 3  ~ "1. Menos de 3 años",
      antig_calc <= 6 ~ "2. 3 a 6 años",
      .default        = "3. Más de 6 años"
    )
  )
max(empleados_antig$diferencia)          # diferencia máxima

empleados_antig %>%
  group_by(grupo) %>%
  summarise(empleados = n(),
            salario_prom = round(mean(salario_mensual), 2),
            .groups = "drop")

# ---- Bloque 8 --------------------------------------------------------
library(tidyverse)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)

margen_cat <- transacciones %>%
  left_join(select(productos, producto_id, categoria, costo),
            by = "producto_id") %>%
  mutate(utilidad = (precio_venta - costo) * cantidad) %>%
  group_by(categoria) %>%
  summarise(con_perdida = sum(utilidad < 0),   # antes de resumir
            ingresos = sum(total_transaccion),
            utilidad = sum(utilidad),
            .groups = "drop") %>%
  mutate(margen_pct = round(utilidad / ingresos * 100, 1)) %>%
  arrange(desc(utilidad))
margen_cat

# ---- Bloque 9 --------------------------------------------------------
library(tidyverse)
ventas <- read_csv("datasets/ventas_retail.csv", show_col_types = FALSE)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
clientes <- read_csv("datasets/clientes.csv", show_col_types = FALSE)

cli_ventas <- ventas %>% distinct(cliente_id)
cli_trans  <- transacciones %>% distinct(cliente_id)

# (a) En ambas tablas
cli_ventas %>% semi_join(cli_trans, by = "cliente_id") %>% nrow()
# (b) En transacciones pero no en ventas
cli_trans %>% anti_join(cli_ventas, by = "cliente_id") %>% nrow()
# (c) Registrados que no aparecen en ninguna
compradores <- bind_rows(cli_ventas, cli_trans) %>% distinct()
clientes %>% anti_join(compradores, by = "cliente_id") %>%
  select(cliente_id, nombre, fecha_registro)

# ---- Bloque 10 --------------------------------------------------------
library(tidyverse)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
tiendas <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)

pagos_tienda <- transacciones %>%
  left_join(select(tiendas, tienda_id, nombre_tienda), by = "tienda_id") %>%
  group_by(nombre_tienda, metodo_pago) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop_last") %>%
  mutate(pct = round(ingresos / sum(ingresos) * 100, 1)) %>%
  ungroup() %>%
  select(-ingresos) %>%
  pivot_wider(names_from = metodo_pago, values_from = pct,
              values_fill = 0) %>%
  arrange(desc(Efectivo))
pagos_tienda

# ---- Bloque 11 --------------------------------------------------------
library(tidyverse)
ventas <- read_csv("datasets/ventas_retail.csv", show_col_types = FALSE)

tienda_mes <- ventas %>%
  group_by(tienda_id, mes) %>%
  summarise(ingresos = sum(total), .groups = "drop") %>%
  complete(tienda_id, mes, fill = list(ingresos = 0)) %>%
  arrange(tienda_id, mes) %>%
  group_by(tienda_id) %>%
  mutate(cambio_pesos = ingresos - lag(ingresos),
         acumulado    = cumsum(ingresos)) %>%
  ungroup()
nrow(tienda_mes)

tienda_mes %>%
  slice_min(cambio_pesos, n = 3) %>%        # las 3 mayores caídas
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# ---- Bloque 12 --------------------------------------------------------
library(tidyverse)
lista_proveedor <- tibble(
  id        = c(1, 2, 2, 3, 4, 5, 6),
  categoria = c("electronica", "Electrónica ", "Electrónica ",
                "HOGAR", "hogar", "Ropa", " ropa"),
  precio    = c("$1,250.00", "$899.50", "$899.50", "450", "$0",
                "$320.75", "-15"),
  fecha     = c("2023-05-01", "02/05/2023", "02/05/2023", "2023-05-03",
                "04/05/2023", "2023-05-05", "06/05/2023")
)

lista_limpia <- lista_proveedor %>%
  distinct() %>%                                   # 1. duplicados
  mutate(
    categoria = str_to_title(str_squish(categoria)),   # 2. texto
    categoria = recode(categoria, "Electronica" = "Electrónica"),
    precio    = parse_number(precio),                  # 3. número
    fecha     = as_date(parse_date_time(fecha,         # 4. fecha
                                        orders = c("ymd", "dmy"))),
    sku       = str_c("SKU-", str_pad(id, 4, pad = "0"))
  ) %>%
  filter(precio > 0)                               # 5. regla de negocio
lista_limpia

lista_limpia %>%
  group_by(categoria) %>%
  summarise(productos = n(), precio_prom = mean(precio),
            .groups = "drop")

# ---- Bloque 13 --------------------------------------------------------
library(tidyverse)
library(writexl)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)
tiendas   <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)

maestra_t <- transacciones %>%
  left_join(select(productos, producto_id, nombre_producto, categoria),
            by = "producto_id") %>%
  left_join(select(tiendas, tienda_id, nombre_tienda), by = "tienda_id")
stopifnot(nrow(maestra_t) == nrow(transacciones))   # control

resumen <- maestra_t %>%
  group_by(nombre_tienda) %>%
  summarise(ingresos = sum(total_transaccion), transacciones = n(),
            ticket_promedio = round(mean(total_transaccion), 2),
            clientes_unicos = n_distinct(cliente_id), .groups = "drop") %>%
  mutate(ranking = min_rank(desc(ingresos))) %>%
  arrange(ranking)

trimestres <- maestra_t %>%
  group_by(nombre_tienda, trimestre) %>%
  summarise(ingresos = round(sum(total_transaccion)), .groups = "drop") %>%
  pivot_wider(names_from = trimestre, values_from = ingresos,
              values_fill = 0)

top_producto <- maestra_t %>%
  group_by(nombre_tienda, nombre_producto, categoria) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop") %>%
  group_by(nombre_tienda) %>%
  slice_max(ingresos, n = 1, with_ties = FALSE) %>%
  ungroup()

dir.create("resultados", showWarnings = FALSE)
write_xlsx(list("Resumen" = resumen, "Trimestres" = trimestres,
                "Top producto" = top_producto),
           "resultados/reporte_tiendas.xlsx")
readxl::excel_sheets("resultados/reporte_tiendas.xlsx")
resumen %>% select(ranking, nombre_tienda, ingresos, ticket_promedio)
top_producto %>% head(3)
