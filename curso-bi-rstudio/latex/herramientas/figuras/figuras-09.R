# ============================================================================
# Figuras del Módulo 9 - Proyecto final: TechRetail México
# Repite el código de las gráficas del capítulo y las guarda en PDF.
# Ejecútalo con el directorio de trabajo en la carpeta que contiene datasets/
# ============================================================================
source(file.path(Sys.getenv("DIR_LIBRO", "."),
                 "herramientas/figuras/tema_libro.R"))
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(forecast)
})

fecha_corte <- as.Date("2024-01-01")
pesos <- function(x) dollar(x, accuracy = 1)

# ---- Datos y modelo en estrella (mismo código que la Fase 1) ----
transacciones <- read_csv("datasets/transacciones.csv", show_col_types = FALSE)
clientes  <- read_csv("datasets/clientes.csv",  show_col_types = FALSE)
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)
empleados <- read_csv("datasets/empleados.csv", show_col_types = FALSE)
tiendas   <- read_csv("datasets/tiendas.csv",   show_col_types = FALSE)

vendedores_ids <- unique(transacciones$vendedor_id)
dim_vendedor <- empleados %>%
  filter(empleado_id %in% vendedores_ids) %>%
  select(vendedor_id = empleado_id, nombre_vendedor = nombre,
         departamento, puesto, salario_mensual, antiguedad_anos) %>%
  mutate(depto_ventas = departamento == "Ventas")

dias_es <- c("lunes", "martes", "miércoles", "jueves", "viernes",
             "sábado", "domingo")
dim_fecha <- tibble(fecha = seq(as.Date("2023-01-01"),
                                as.Date("2023-12-31"), by = "day")) %>%
  mutate(mes = month(fecha),
         dia_semana = factor(dias_es[wday(fecha, week_start = 1)],
                             levels = dias_es))

hechos_ventas <- transacciones %>%
  select(transaccion_id, fecha, hora, cliente_id, producto_id,
         tienda_id, vendedor_id, metodo_pago, cantidad, precio_venta,
         total = total_transaccion) %>%
  left_join(select(productos, producto_id, costo_unitario = costo),
            by = "producto_id") %>%
  mutate(hora_dia = hour(hora),
         margen_estimado = total - cantidad * costo_unitario)

ventas <- hechos_ventas %>%
  left_join(select(productos, producto_id, nombre_producto, categoria),
            by = "producto_id") %>%
  left_join(select(tiendas, tienda_id, nombre_tienda, zona, tipo,
                   tamano_m2), by = "tienda_id") %>%
  left_join(select(clientes, cliente_id, es_premium), by = "cliente_id") %>%
  left_join(dim_fecha, by = "fecha")

# ---- Tablas de la Fase 2 ----
meses_es <- c("ene", "feb", "mar", "abr", "may", "jun",
              "jul", "ago", "sep", "oct", "nov", "dic")
ventas_mes <- ventas %>%
  group_by(mes) %>%
  summarise(ventas = sum(total), .groups = "drop") %>%
  mutate(nombre_mes = factor(meses_es[mes], levels = meses_es))

ventas_categoria <- ventas %>%
  group_by(categoria) %>%
  summarise(ventas = sum(total), margen = sum(margen_estimado),
            .groups = "drop") %>%
  mutate(pct_margen = margen / ventas)

ventas_tienda <- ventas %>%
  group_by(nombre_tienda, zona, tipo, tamano_m2) %>%
  summarise(ventas = sum(total), .groups = "drop")

pareto <- ventas %>%
  group_by(producto_id) %>%
  summarise(ventas = sum(total), .groups = "drop") %>%
  arrange(desc(ventas)) %>%
  mutate(pct_productos = row_number() / n(),
         pct_acumulado = cumsum(ventas) / sum(ventas))

ventas_vendedor <- ventas %>%
  group_by(vendedor_id) %>%
  summarise(ventas = sum(total), .groups = "drop") %>%
  left_join(dim_vendedor, by = "vendedor_id")

gasto_cliente <- ventas %>%
  group_by(cliente_id, es_premium) %>%
  summarise(gasto = sum(total), .groups = "drop") %>%
  mutate(grupo = if_else(es_premium, "Premium", "No premium"))

# ---- Fase 3: RFM y pronóstico ----
rfm <- hechos_ventas %>%
  group_by(cliente_id) %>%
  summarise(recencia = as.numeric(fecha_corte - max(fecha)),
            frecuencia = n(), monto = sum(total), .groups = "drop")
rfm_escalado <- scale(select(rfm, recencia, frecuencia, monto))
set.seed(123)
invisible(map(2:7, ~ kmeans(rfm_escalado, centers = .x, nstart = 25)))
set.seed(123)
km_rfm <- kmeans(rfm_escalado, centers = 4, nstart = 25)
perfil <- rfm %>%
  mutate(cluster = km_rfm$cluster) %>%
  group_by(cluster) %>%
  summarise(recencia = mean(recencia), monto = mean(monto),
            .groups = "drop") %>%
  mutate(segmento = case_when(
    monto == max(monto)       ~ "Campeones",
    recencia == max(recencia) ~ "En riesgo",
    monto == max(monto[!monto == max(monto) &
                         !recencia == max(recencia)]) ~ "Leales",
    TRUE                      ~ "Ocasionales"))
rfm <- rfm %>%
  mutate(cluster = km_rfm$cluster) %>%
  left_join(select(perfil, cluster, segmento), by = "cluster")

ventas_semana <- hechos_ventas %>%
  filter(fecha >= as.Date("2023-01-02")) %>%
  mutate(semana = floor_date(fecha, "week", week_start = 1)) %>%
  count(semana, wt = total, name = "ventas")
pronostico <- forecast(ets(ts(ventas_semana$ventas)), h = 8,
                       level = c(80, 95))
tabla_pronostico <- tibble(
  semana   = max(ventas_semana$semana) + weeks(1:8),
  estimado = as.numeric(pronostico$mean),
  inf_80 = as.numeric(pronostico$lower[, "80%"]),
  sup_80 = as.numeric(pronostico$upper[, "80%"]),
  inf_95 = as.numeric(pronostico$lower[, "95%"]),
  sup_95 = as.numeric(pronostico$upper[, "95%"]))

# ============================================================================
# Figuras (código idéntico al del capítulo)
# ============================================================================

# ---- m09-ventas-mensuales ----
promedio_mes <- mean(ventas_mes$ventas)
extremos <- ventas_mes %>%
  filter(ventas %in% range(ventas)) %>%
  mutate(ajuste = if_else(ventas == max(ventas), -1.1, 2))  # arriba/abajo
p <- 
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
guardar_figura(p, "m09-ventas-mensuales")

# ---- m09-categorias ----
p <- ventas_categoria %>%
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
guardar_figura(p, "m09-categorias")

# ---- m09-tiendas ----
p <- ventas_tienda %>%
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
guardar_figura(p, "m09-tiendas")

# ---- m09-dia-hora ----
calor <- ventas %>%
  count(dia_semana, hora_dia, wt = total, name = "ventas")
p <- ggplot(calor, aes(x = factor(hora_dia), y = fct_rev(dia_semana),
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
guardar_figura(p, "m09-dia-hora")

# ---- m09-pareto ----
p <- ggplot(pareto, aes(x = pct_productos, y = pct_acumulado)) +
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
guardar_figura(p, "m09-pareto")

# ---- m09-vendedores ----
p <- ventas_vendedor %>%
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
guardar_figura(p, "m09-vendedores")

# ---- m09-premium ----
p <- ggplot(gasto_cliente, aes(x = grupo, y = gasto)) +
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
guardar_figura(p, "m09-premium")

# ---- m09-rfm ----
p <- ggplot(rfm, aes(x = recencia, y = monto, color = segmento)) +
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
guardar_figura(p, "m09-rfm")

# ---- m09-pronostico ----
p <- ggplot() +
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
guardar_figura(p, "m09-pronostico")

cat("Figuras del Módulo 9 generadas\n")
