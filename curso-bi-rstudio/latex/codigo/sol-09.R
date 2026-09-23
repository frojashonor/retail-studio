# ==========================================================================
# sol-09.tex
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/sol-09.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Bloque 1 --------------------------------------------------------
library(tidyverse)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)

# (a) Porcentaje de clientes activos con 2 o más compras
compras_cliente <- transacciones %>% count(cliente_id, name = "compras")
compras_cliente %>%
  summarise(activos = n(),
            recompran = sum(compras >= 2),
            tasa_recompra = round(100 * mean(compras >= 2), 1))

# (b) Días entre compras consecutivas del mismo cliente
intervalos <- transacciones %>%
  arrange(cliente_id, fecha) %>%
  group_by(cliente_id) %>%
  mutate(dias_desde_anterior = as.numeric(fecha - lag(fecha))) %>%
  ungroup() %>%
  filter(!is.na(dias_desde_anterior))

quantile(intervalos$dias_desde_anterior, c(0.25, 0.5, 0.75))

# (c) Clientes con al menos una recompra en 30 días o menos
intervalos %>%
  group_by(cliente_id) %>%
  summarise(recompra_30 = any(dias_desde_anterior <= 30)) %>%
  summarise(clientes = sum(recompra_30))

# ---- Bloque 2 --------------------------------------------------------
library(tidyverse)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
tiendas <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)

productividad <- transacciones %>%
  group_by(tienda_id) %>%
  summarise(ventas = sum(total_transaccion), .groups = "drop") %>%
  left_join(tiendas, by = "tienda_id") %>%
  mutate(ventas_m2       = ventas / tamano_m2,
         ventas_empleado = ventas / empleados,
         rank_ventas     = min_rank(desc(ventas)),
         rank_m2         = min_rank(desc(ventas_m2)),
         rank_empleado   = min_rank(desc(ventas_empleado)),
         cambio          = rank_ventas - rank_m2) %>%
  arrange(rank_m2)

productividad %>%
  transmute(tienda = str_remove(nombre_tienda, "Sucursal "),
            m2 = tamano_m2, ventas = round(ventas),
            por_m2 = round(ventas_m2), por_empleado = round(ventas_empleado),
            rank_ventas, rank_m2, cambio)

# ---- Bloque 3 --------------------------------------------------------
color_resalte <- "#eb6834"; color_gris <- "#b5b3ad"
productividad %>%
  mutate(mejor = rank_m2 == 1) %>%
  ggplot(aes(x = ventas_m2, y = fct_reorder(nombre_tienda, ventas_m2),
             fill = mejor)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c(`TRUE` = color_resalte,
                               `FALSE` = color_gris)) +
  scale_x_continuous(labels = scales::dollar) +
  labs(title = "Ventas por metro cuadrado, 2023",
       x = "Ventas por m²", y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# ---- Bloque 4 --------------------------------------------------------
library(tidyverse)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)

alerta <- transacciones %>%
  group_by(producto_id) %>%
  summarise(unidades_2023 = sum(cantidad), .groups = "drop") %>%
  right_join(productos, by = "producto_id") %>%
  filter(activo) %>%
  mutate(unidades_2023   = replace_na(unidades_2023, 0),
         venta_diaria    = unidades_2023 / 365,
         dias_cobertura  = stock_actual / venta_diaria,
         bajo_minimo     = stock_actual < stock_minimo,
         alerta          = dias_cobertura < 30 | bajo_minimo) %>%
  filter(alerta) %>%
  arrange(dias_cobertura) %>%
  transmute(producto_id, stock = stock_actual, minimo = stock_minimo,
            diaria = round(venta_diaria, 2),
            cobertura = round(dias_cobertura), bajo_minimo)

alerta

# Contexto: distribución de la cobertura de todos los activos
productos %>%
  filter(activo) %>%
  left_join(count(transacciones, producto_id, wt = cantidad,
                  name = "unidades"), by = "producto_id") %>%
  mutate(dias_cobertura = stock_actual / (unidades / 365)) %>%
  pull(dias_cobertura) %>%
  quantile(c(0, 0.25, 0.5)) %>%
  round()

# ---- Bloque 5 --------------------------------------------------------
library(tidyverse)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)

compras <- transacciones %>%
  mutate(mes_compra = floor_date(fecha, "month")) %>%
  group_by(cliente_id) %>%
  mutate(cohorte = min(mes_compra)) %>%
  ungroup() %>%
  mutate(meses_desde = (year(mes_compra) - year(cohorte)) * 12 +
                        month(mes_compra) - month(cohorte))

# Tamaño de cada cohorte
compras %>% distinct(cliente_id, cohorte) %>% count(cohorte) %>%
  mutate(cohorte = format(cohorte, "%Y-%m"))

matriz <- compras %>%
  distinct(cliente_id, cohorte, meses_desde) %>%
  count(cohorte, meses_desde, name = "clientes") %>%
  group_by(cohorte) %>%
  mutate(pct = round(100 * clientes / clientes[meses_desde == 0])) %>%
  ungroup()

# Primer trimestre: % de la cohorte que compró 1, 2 y 3 meses después
matriz %>%
  filter(cohorte <= as.Date("2023-03-01"), meses_desde <= 3) %>%
  mutate(cohorte = format(cohorte, "%Y-%m")) %>%
  select(-clientes) %>%
  pivot_wider(names_from = meses_desde, values_from = pct,
              names_prefix = "mes_")

# ---- Bloque 6 --------------------------------------------------------
library(tidyverse)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)

# Regla (a): Tukey sobre el total de la transacción
q <- quantile(transacciones$total_transaccion, c(0.25, 0.75))
iqr <- unname(q[2] - q[1])
limites <- c(inferior = q[[1]] - 1.5 * iqr, superior = q[[2]] + 1.5 * iqr)
round(limites, 2)
sum(transacciones$total_transaccion < limites["inferior"] |
      transacciones$total_transaccion > limites["superior"])

# Regla (b): precio muy alejado de la mediana del mismo producto
revision <- transacciones %>%
  group_by(producto_id) %>%
  mutate(precio_mediano = median(precio_venta),
         razon = precio_venta / precio_mediano) %>%
  ungroup() %>%
  mutate(atipico_precio = razon > 3 | razon < 1 / 3)

sum(revision$atipico_precio)

# Lista para Auditoría: los casos más extremos primero
revision %>%
  filter(atipico_precio) %>%
  mutate(distancia = abs(log(razon))) %>%
  arrange(desc(distancia)) %>%
  transmute(id = transaccion_id, tienda = tienda_id,
            vendedor = vendedor_id, producto = producto_id,
            precio = precio_venta, mediana = precio_mediano,
            razon = round(razon, 2)) %>%
  head(6)

# ¿Se concentran en algún vendedor?
revision %>% filter(atipico_precio) %>%
  count(vendedor_id, sort = TRUE) %>% head(3)

# ---- Bloque 7 --------------------------------------------------------
library(tidyverse)
ventas_retail <- read_csv("datasets/ventas_retail.csv",
                          show_col_types = FALSE)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)

# 1. ¿Con descuento se venden más unidades?
ventas_retail %>%
  mutate(con_descuento = descuento_pct > 0) %>%
  group_by(con_descuento) %>%
  summarise(ventas = n(), cantidad_promedio = round(mean(cantidad), 2),
            .groups = "drop")
t.test(cantidad ~ descuento_pct > 0, data = ventas_retail)$p.value

# 2. Simulación de diciembre con 10% de descuento
ingreso_base <- transacciones %>%
  filter(month(fecha) == 12) %>%
  summarise(total = sum(total_transaccion)) %>%
  pull(total)

set.seed(123)
simulacion <- tibble(aumento_unidades = runif(1000, 0, 0.15)) %>%
  mutate(ingreso = ingreso_base * (1 + aumento_unidades) * 0.90,
         diferencia = ingreso - ingreso_base)

simulacion %>%
  summarise(ingreso_base = round(ingreso_base),
            ingreso_promedio = round(mean(ingreso)),
            pct_pierde = round(100 * mean(diferencia < 0), 1),
            peor_caso = round(min(diferencia)),
            mejor_caso = round(max(diferencia)))

# 3. Aumento de unidades necesario para no perder: (1 + x) * 0.9 = 1
round(100 * (1 / 0.90 - 1), 1)

# ---- Bloque 8 --------------------------------------------------------
library(tidyverse)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)

ranking <- transacciones %>%
  mutate(mes = month(fecha)) %>%
  count(vendedor_id, mes, wt = total_transaccion, name = "ventas") %>%
  complete(vendedor_id, mes = 1:12, fill = list(ventas = 0)) %>%
  group_by(mes) %>%
  mutate(lugar = min_rank(desc(ventas))) %>%          # ranking del mes
  group_by(vendedor_id) %>%
  arrange(mes, .by_group = TRUE) %>%
  mutate(acumulado = cumsum(ventas),                  # venta acumulada
         cambio_lugar = lag(lugar) - lugar) %>%       # + = subió
  ungroup()

# Vendedores con más meses en el top 5
ranking %>%
  group_by(vendedor_id) %>%
  summarise(meses_top5 = sum(lugar <= 5),
            ventas_anuales = round(max(acumulado)), .groups = "drop") %>%
  arrange(desc(meses_top5), desc(ventas_anuales)) %>%
  head(5)

# Mayor subida de un mes a otro
ranking %>%
  slice_max(cambio_lugar, n = 1) %>%
  select(vendedor_id, mes, lugar, cambio_lugar, ventas) %>%
  mutate(ventas = round(ventas))

# ---- Bloque 9 --------------------------------------------------------
library(tidyverse)
library(writexl)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
tiendas   <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)

dir.create("resultados/gerentes", recursive = TRUE, showWarnings = FALSE)

# KPIs de un conjunto de transacciones
kpis_de <- function(datos) {
  datos %>% summarise(ventas = round(sum(total_transaccion)),
                      transacciones = n(),
                      ticket = round(mean(total_transaccion), 2),
                      clientes = n_distinct(cliente_id))
}

# Promedio de la cadena por tienda (para comparar)
cadena <- transacciones %>%
  group_by(tienda_id) %>%
  group_modify(~ kpis_de(.x)) %>%
  ungroup() %>%
  summarise(across(-tienda_id, mean)) %>%
  mutate(ventas = round(ventas), ticket = round(ticket, 2),
         clientes = round(clientes, 1))

paquete_gerente <- function(id) {
  info  <- filter(tiendas, tienda_id == id)
  datos <- filter(transacciones, tienda_id == id)

  resumen <- bind_rows(
    kpis_de(datos) %>% mutate(nivel = info$nombre_tienda),
    cadena %>% mutate(nivel = "Promedio cadena")) %>%
    relocate(nivel)

  mensual <- datos %>%
    group_by(mes) %>%
    summarise(ventas = round(sum(total_transaccion)),
              transacciones = n(), .groups = "drop") %>%
    mutate(crecimiento_pct = round(100 * (ventas / lag(ventas) - 1), 1))

  top <- datos %>%
    group_by(producto_id) %>%
    summarise(unidades = sum(cantidad),
              ventas = round(sum(total_transaccion)), .groups = "drop") %>%
    left_join(select(productos, producto_id, nombre_producto, categoria),
              by = "producto_id") %>%
    slice_max(ventas, n = 10, with_ties = FALSE)

  archivo <- file.path("resultados/gerentes",
                       str_c("tienda_", sprintf("%02d", id), "_",
                             str_replace_all(info$nombre_tienda, " ", "_"),
                             ".xlsx"))
  write_xlsx(list(Resumen = resumen, Mensual = mensual,
                  `Top productos` = top), archivo)
}

walk(tiendas$tienda_id, paquete_gerente)

archivos <- list.files("resultados/gerentes", pattern = "xlsx$")
length(archivos)
head(archivos, 3)

# Comprobación: leer la hoja Resumen de la tienda 8
readxl::read_excel(file.path("resultados/gerentes", archivos[8]),
                   sheet = "Resumen")
