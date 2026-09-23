# ============================================================================
# Figuras del Módulo 3: Visualización de datos con ggplot2
# ----------------------------------------------------------------------------
# Repite EXACTAMENTE el código de las gráficas del capítulo
# (capitulos/03-visualizacion.tex) y de las soluciones (sol-03.tex) y las
# guarda en PDF con guardar_figura().
# Ejecución (desde una carpeta que contenga datasets/):
#   DIR_LIBRO=.../latex DIR_FIGURAS=.../latex/figuras \
#   Rscript herramientas/figuras/figuras-03.R
# ============================================================================
source(file.path(Sys.getenv("DIR_LIBRO"), "herramientas/figuras/tema_libro.R"))
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

# Tamaño de los paneles que van lado a lado en el libro
panel_ancho <- 3.2
panel_alto  <- 2.7

# ---------------------------------------------------------------------------
# Datos (mismo código que el capítulo)
# ---------------------------------------------------------------------------
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)
tiendas   <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)

ventas_detalle <- transacciones %>%
  left_join(select(productos, producto_id, categoria),
            by = "producto_id") %>%
  left_join(select(tiendas, tienda_id, nombre_tienda),
            by = "tienda_id") %>%
  mutate(tienda = str_remove(nombre_tienda, "Sucursal "))

# ---------------------------------------------------------------------------
# 1. Anscombe
# ---------------------------------------------------------------------------
anscombe_largo <- anscombe %>%
  pivot_longer(cols = everything(),
               names_to = c(".value", "conjunto"),
               names_pattern = "(.)(.)") %>%
  mutate(conjunto = paste("Conjunto", conjunto))

g_anscombe <- ggplot(anscombe_largo, aes(x = x, y = y)) +
  geom_point(color = "#2a78d6", size = 2) +        # los 11 puntos
  geom_smooth(method = "lm", formula = y ~ x,      # recta de regresión
              se = FALSE, color = "#eb6834", linewidth = 0.8) +
  facet_wrap(~ conjunto, ncol = 4) +               # un panel por conjunto
  labs(title = "Mismas estadísticas, cuatro historias distintas",
       subtitle = "Media, varianza, correlación y recta casi idénticas",
       caption = "Fuente: datasets::anscombe (Anscombe, 1973)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
guardar_figura(g_anscombe, "m03-anscombe", alto = 3.2)

# ---------------------------------------------------------------------------
# 2. Primer gráfico capa por capa
# ---------------------------------------------------------------------------
ventas_mes <- ventas_detalle %>%
  mutate(mes = floor_date(fecha, unit = "month")) %>%
  group_by(mes) %>%
  summarise(ingresos = sum(total_transaccion),
            tickets  = n(),
            .groups  = "drop")

paso1 <- ggplot(data = ventas_mes, mapping = aes(x = mes, y = ingresos))
paso2 <- paso1 + geom_line(color = "#2a78d6", linewidth = 0.8)
paso3 <- paso2 +
  geom_point(color = "#2a78d6", size = 2) +
  scale_y_continuous(labels = dollar)
paso4 <- paso3 +
  labs(title = "Ingresos mensuales 2023", x = NULL, y = "Ingresos") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

guardar_figura(paso1, "m03-paso-1", panel_ancho, panel_alto)
guardar_figura(paso2, "m03-paso-2", panel_ancho, panel_alto)
guardar_figura(paso3, "m03-paso-3", panel_ancho, panel_alto)
guardar_figura(paso4, "m03-paso-4", panel_ancho, panel_alto)

# ---------------------------------------------------------------------------
# 4. Barras
# ---------------------------------------------------------------------------
ingresos_categoria <- ventas_detalle %>%
  group_by(categoria) %>%
  summarise(ingresos = sum(total_transaccion),
            tickets  = n(),
            .groups  = "drop") %>%
  mutate(participacion = ingresos / sum(ingresos)) %>%
  arrange(desc(ingresos))

g_categorias <- ggplot(ingresos_categoria,
                       aes(x = ingresos,
                           y = fct_reorder(categoria, ingresos))) +
  geom_col(fill = color_principal, width = 0.7) +
  scale_x_continuous(labels = dollar,
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Ingresos por categoría, 2023",
       x = "Ingresos (MXN)", y = NULL,
       caption = "Fuente: transacciones.csv y productos.csv") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
guardar_figura(g_categorias, "m03-barras-categoria")

ingresos_categoria <- ingresos_categoria %>%
  mutate(grupo = if_else(categoria == "Belleza", "Destacada", "Resto"))

g_resalte <- ggplot(ingresos_categoria,
                    aes(x = ingresos,
                        y = fct_reorder(categoria, ingresos),
                        fill = grupo)) +
  geom_col(width = 0.7) +
  geom_text(data = filter(ingresos_categoria, grupo == "Destacada"),
            aes(label = dollar(ingresos)),
            hjust = 1.1, color = "white", fontface = "bold",
            size = 3.5) +
  scale_fill_manual(values = c(Destacada = color_resalte,
                               Resto     = color_gris),
                    guide = "none") +
  scale_x_continuous(labels = dollar,
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Belleza aporta 19% de los ingresos del año",
       subtitle = "Ingresos por categoría, 2023",
       x = "Ingresos (MXN)", y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
guardar_figura(g_resalte, "m03-barras-resalte")

top5 <- ingresos_categoria %>%
  slice_max(ingresos, n = 5) %>%
  pull(categoria)

ingresos_semestre <- ventas_detalle %>%
  filter(categoria %in% top5) %>%
  mutate(semestre = if_else(month(fecha) <= 6, "Ene-Jun", "Jul-Dic")) %>%
  group_by(categoria, semestre) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop")

g_agrupadas <- ggplot(ingresos_semestre,
                      aes(x = fct_reorder(categoria, ingresos,
                                          .fun = sum, .desc = TRUE),
                          y = ingresos, fill = semestre)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = paleta_libro[1:2]) +
  scale_y_continuous(labels = dollar,
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Belleza cayó 41% en el segundo semestre",
       subtitle = "Ingresos de las 5 categorías principales, 2023",
       x = NULL, y = "Ingresos (MXN)", fill = "Semestre") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top")
guardar_figura(g_agrupadas, "m03-barras-agrupadas")

pagos_trimestre <- ventas_detalle %>%
  group_by(trimestre, metodo_pago) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop")

g_100 <- ggplot(pagos_trimestre,
                aes(x = trimestre, y = ingresos,
                    fill = fct_reorder(metodo_pago, ingresos,
                                       .fun = sum, .desc = TRUE))) +
  geom_col(position = position_fill(reverse = TRUE), width = 0.7) +
  scale_fill_manual(values = paleta_libro[1:4]) +
  scale_y_continuous(labels = percent) +
  labs(title = "La mezcla de pagos casi no cambia en el año",
       subtitle = "Participación de cada método de pago en los ingresos",
       x = NULL, y = "% de los ingresos", fill = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top")
guardar_figura(g_100, "m03-barras-100")

# ---------------------------------------------------------------------------
# 5. Líneas
# ---------------------------------------------------------------------------
meta_mensual <- 80000
mes_record <- ventas_mes %>% slice_max(ingresos, n = 1)

g_linea <- ggplot(ventas_mes, aes(x = mes, y = ingresos)) +
  geom_hline(yintercept = meta_mensual, linetype = "dashed",
             color = color_gris, linewidth = 0.6) +
  geom_line(color = color_principal, linewidth = 0.8) +
  geom_point(color = color_principal, size = 2) +
  annotate("text", x = as.Date("2023-09-15"), y = meta_mensual,
           label = "Meta mensual: $80,000", vjust = -0.6,
           size = 3.2, color = "#52514e") +
  annotate("point", x = mes_record$mes, y = mes_record$ingresos,
           color = color_resalte, size = 3.5) +
  annotate("text", x = mes_record$mes, y = mes_record$ingresos,
           label = paste("Récord del año:",
                         dollar(mes_record$ingresos, accuracy = 1)),
           hjust = 1, vjust = -1.2, size = 3.2, color = color_resalte) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(labels = dollar,
                     expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "7 de 12 meses superaron la meta",
       subtitle = "Ingresos mensuales 2023 contra la meta de $80,000",
       x = NULL, y = "Ingresos (MXN)",
       caption = "Fuente: transacciones.csv") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
guardar_figura(g_linea, "m03-linea-meta")

metas <- tibble(
  mes  = seq(as.Date("2023-01-01"), as.Date("2023-12-01"),
             by = "month"),
  meta = c(75, 70, 80, 75, 80, 85, 85, 80, 75, 80, 85, 95) * 1000
)

real_vs_meta <- ventas_mes %>%
  select(mes, Real = ingresos) %>%
  left_join(rename(metas, Meta = meta), by = "mes") %>%
  pivot_longer(cols = c(Real, Meta),
               names_to = "serie", values_to = "monto") %>%
  mutate(serie = fct_relevel(serie, "Real"))

g_real_meta <- ggplot(real_vs_meta,
                      aes(x = mes, y = monto,
                          color = serie, linetype = serie)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = filter(real_vs_meta, serie == "Real"),
             size = 2) +
  scale_color_manual(values = c(Real = color_principal,
                                Meta = color_gris)) +
  scale_linetype_manual(values = c(Real = "solid", Meta = "dashed")) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(labels = dollar) +
  labs(title = "Abril y octubre, los meses más lejos de su meta",
       subtitle = "Ingresos reales contra meta mensual, 2023",
       x = NULL, y = "Ingresos (MXN)", color = NULL,
       linetype = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top")
guardar_figura(g_real_meta, "m03-real-meta")

acumulado_tienda <- ventas_detalle %>%
  mutate(mes = floor_date(fecha, unit = "month")) %>%
  group_by(tienda, mes) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop") %>%
  arrange(tienda, mes) %>%
  group_by(tienda) %>%
  mutate(acumulado = cumsum(ingresos)) %>%
  ungroup() %>%
  mutate(grupo = if_else(tienda == "Outlet", "Outlet",
                         "Otras 9 tiendas"))

g_carrera <- ggplot(acumulado_tienda,
                    aes(x = mes, y = acumulado,
                        group = tienda, color = grupo)) +
  geom_line(data = filter(acumulado_tienda, grupo != "Outlet"),
            linewidth = 0.6) +
  geom_line(data = filter(acumulado_tienda, grupo == "Outlet"),
            linewidth = 1.1) +
  scale_color_manual(values = c("Outlet" = color_resalte,
                                "Otras 9 tiendas" = color_gris)) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(labels = dollar) +
  labs(title = "Outlet rebasó a todas las tiendas en el último trimestre",
       subtitle = "Ingresos acumulados por tienda, 2023",
       x = NULL, y = "Ingresos acumulados (MXN)", color = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top")
guardar_figura(g_carrera, "m03-lineas-resalte")

# ---------------------------------------------------------------------------
# 6. Dispersión
# ---------------------------------------------------------------------------
g_precio_cantidad <- ggplot(ventas_detalle,
                            aes(x = precio_venta, y = cantidad)) +
  geom_point(position = position_jitter(width = 0, height = 0.15,
                                        seed = 123),
             color = color_principal, alpha = 0.35) +
  geom_smooth(method = "lm", formula = y ~ x,
              color = color_resalte, linewidth = 0.8) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(breaks = 1:5) +
  labs(title = "El precio no cambia cuántas piezas lleva el cliente",
       subtitle = "Cada punto es un ticket de 2023 (1,000 en total)",
       x = "Precio unitario (MXN)", y = "Piezas por ticket") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
guardar_figura(g_precio_cantidad, "m03-dispersion-precio")

set.seed(123)
campanas <- tibble(
  canal     = rep(c("Correo", "Redes", "Radio"), each = 40),
  inversion = round(10^runif(120, min = 3, max = 6))
) %>%
  mutate(retorno = case_when(canal == "Correo" ~ 6,
                             canal == "Redes"  ~ 3,
                             canal == "Radio"  ~ 1.5),
         ventas = round(inversion * retorno *
                          exp(rnorm(n(), mean = 0, sd = 0.35))))

dinero_corto <- label_dollar(scale_cut = cut_short_scale())

g_campanas <- ggplot(campanas, aes(x = inversion, y = ventas,
                                   color = canal)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = paleta_libro[1:3]) +
  labs(x = "Inversión", y = "Ventas", color = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        plot.margin = margin(5, 15, 5, 5))

g_lineal <- g_campanas +
  scale_x_continuous(labels = dinero_corto,
                     breaks = c(0, 500000, 1000000)) +
  scale_y_continuous(labels = dinero_corto) +
  labs(title = "Escala lineal")

g_log <- g_campanas +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              linewidth = 0.8) +
  scale_x_log10(labels = dinero_corto) +
  scale_y_log10(labels = dinero_corto) +
  labs(title = "Escala logarítmica")
guardar_figura(g_lineal, "m03-escala-lineal", panel_ancho, 3)
guardar_figura(g_log, "m03-escala-log", panel_ancho, 3)

# ---------------------------------------------------------------------------
# 7. Distribuciones
# ---------------------------------------------------------------------------
g_hist <- ggplot(ventas_detalle, aes(x = total_transaccion)) +
  geom_histogram(binwidth = 200, boundary = 0,
                 fill = color_principal, color = "white") +
  scale_x_continuous(labels = dollar) +
  labs(title = "Histograma (conteos)", x = "Importe del ticket",
       y = "Número de tickets") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

g_densidad <- ggplot(ventas_detalle, aes(x = total_transaccion)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 200,
                 boundary = 0, fill = color_gris, color = "white") +
  geom_density(color = color_principal, linewidth = 0.8) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = label_number(accuracy = 0.0001)) +
  labs(title = "Histograma + densidad", x = "Importe del ticket",
       y = "Densidad") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
guardar_figura(g_hist, "m03-histograma", panel_ancho, panel_alto)
guardar_figura(g_densidad, "m03-densidad", panel_ancho, panel_alto)

g_caja <- ggplot(ventas_detalle,
                 aes(x = total_transaccion,
                     y = fct_reorder(metodo_pago, total_transaccion,
                                     .fun = median))) +
  geom_boxplot(fill = "#cde2fb", color = color_principal,
               width = 0.6, outlier.color = color_resalte) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3,
               color = "#0d366b") +
  scale_x_continuous(labels = dollar) +
  labs(title = "Transferencia: tickets más bajos y varios atípicos",
       subtitle = "Importe por ticket según método de pago (rombo = promedio)",
       x = "Importe del ticket (MXN)", y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
guardar_figura(g_caja, "m03-boxplot-pago")

# ---------------------------------------------------------------------------
# 8. Mapa de calor
# ---------------------------------------------------------------------------
calor <- ventas_detalle %>%
  mutate(mes = month(fecha, label = TRUE),
         dia = wday(fecha, label = TRUE, week_start = 1)) %>%
  group_by(mes, dia) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop")

g_calor <- ggplot(calor, aes(x = mes, y = fct_rev(dia), fill = ingresos)) +
  geom_tile(color = "white", linewidth = 0.6) +
  scale_fill_gradient(low = "#cde2fb", high = "#0d366b",
                      labels = dollar) +
  labs(title = "Los días fuertes cambian de un mes a otro",
       subtitle = "Ingresos por día de la semana y mes, 2023",
       x = NULL, y = NULL, fill = "Ingresos") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "right")
guardar_figura(g_calor, "m03-calor")

# ---------------------------------------------------------------------------
# 9. Facetas
# ---------------------------------------------------------------------------
categoria_mes <- ventas_detalle %>%
  mutate(mes = floor_date(fecha, unit = "month")) %>%
  group_by(categoria, mes) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop") %>%
  complete(categoria, mes, fill = list(ingresos = 0))

g_facetas <- ggplot(categoria_mes, aes(x = mes, y = ingresos)) +
  geom_line(color = color_principal, linewidth = 0.7) +
  facet_wrap(~ fct_reorder(categoria, ingresos, .fun = sum,
                           .desc = TRUE),
             ncol = 5) +
  scale_x_date(date_labels = "%b", date_breaks = "4 months") +
  scale_y_continuous(labels = label_number(scale = 1e-3)) +
  labs(title = "Ingresos mensuales por categoría (misma escala)",
       x = NULL, y = "Ingresos (miles de MXN)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
guardar_figura(g_facetas, "m03-facetas")

g_facetas_libre <- g_facetas +
  facet_wrap(~ fct_reorder(categoria, ingresos, .fun = sum,
                           .desc = TRUE),
             ncol = 5, scales = "free_y") +
  labs(title = "Ingresos mensuales por categoría (escala libre)")
guardar_figura(g_facetas_libre, "m03-facetas-libre")

# ---------------------------------------------------------------------------
# 10. Escalas: eje truncado vs. eje honesto
# ---------------------------------------------------------------------------
ingresos_tienda <- ventas_detalle %>%
  group_by(tienda) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop")

g_truncado <- ggplot(ingresos_tienda,
                     aes(x = ingresos,
                         y = fct_reorder(tienda, ingresos))) +
  geom_col(fill = color_principal, width = 0.7) +
  coord_cartesian(xlim = c(78000, 116000)) +
  scale_x_continuous(labels = label_dollar(scale = 1e-3,
                                           suffix = "K")) +
  labs(title = "MAL: eje truncado", x = "Ingresos", y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

g_honesto <- ggplot(ingresos_tienda,
                    aes(x = ingresos,
                        y = fct_reorder(tienda, ingresos))) +
  geom_col(fill = color_principal, width = 0.7) +
  scale_x_continuous(labels = label_dollar(scale = 1e-3,
                                           suffix = "K"),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "BIEN: eje desde cero", x = "Ingresos", y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(5, 15, 5, 5))
guardar_figura(g_truncado, "m03-eje-truncado", panel_ancho, panel_alto)
guardar_figura(g_honesto, "m03-eje-honesto", panel_ancho, panel_alto)

# ---------------------------------------------------------------------------
# 11. Tema corporativo
# ---------------------------------------------------------------------------
tema_empresa <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold", color = "#0b0b0b"),
      plot.subtitle = element_text(color = "#52514e"),
      plot.caption  = element_text(color = "#52514e", size = rel(0.8),
                                   hjust = 0),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      axis.title    = element_text(color = "#52514e"),
      axis.text     = element_text(color = "#52514e"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e6e5e1",
                                      linewidth = 0.3),
      legend.position = "top",
      legend.title    = element_text(color = "#52514e"),
      plot.margin     = margin(10, 15, 10, 10)
    )
}

ingresos_tienda <- ingresos_tienda %>%
  mutate(destacar = if_else(tienda == "Outlet", "Outlet", "Resto"))

g_tema <- ggplot(ingresos_tienda,
                 aes(x = ingresos, y = fct_reorder(tienda, ingresos),
                     fill = destacar)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c(Outlet = color_resalte,
                               Resto  = color_gris),
                    guide = "none") +
  scale_x_continuous(labels = dollar,
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Outlet, la tienda que más vendió en 2023",
       subtitle = "Ingresos por tienda, en pesos",
       x = NULL, y = NULL,
       caption = "Fuente: transacciones.csv | Área de BI") +
  tema_empresa() +
  theme(panel.grid.major.y = element_blank())
guardar_figura(g_tema, "m03-tema-empresa")

# ---------------------------------------------------------------------------
# 12. Antes y después
# ---------------------------------------------------------------------------
g_antes <- ggplot(ingresos_tienda, aes(x = tienda, y = ingresos,
                                       fill = tienda)) +
  geom_col() +
  labs(title = "Antes")

g_despues <- ggplot(ingresos_tienda,
                    aes(x = ingresos, y = fct_reorder(tienda, ingresos))) +
  geom_col(fill = color_principal, width = 0.7) +
  scale_x_continuous(labels = label_dollar(scale = 1e-3, suffix = "K"),
                     breaks = seq(0, 120000, by = 40000),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Después", x = "Ingresos", y = NULL) +
  tema_empresa() +
  theme(panel.grid.major.y = element_blank())
guardar_figura(g_antes, "m03-antes-multicolor", panel_ancho, panel_alto)
guardar_figura(g_despues, "m03-despues-multicolor", panel_ancho,
               panel_alto)

g_sin_pastel <- ggplot(ingresos_categoria,
                       aes(x = participacion,
                           y = fct_reorder(categoria, participacion))) +
  geom_col(fill = color_principal, width = 0.7) +
  geom_text(aes(label = percent(participacion, accuracy = 0.1)),
            hjust = -0.15, size = 3.2, color = "#52514e") +
  scale_x_continuous(labels = percent,
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Cinco categorías concentran el 69% de los ingresos",
       subtitle = "Participación de cada categoría en los ingresos 2023",
       x = NULL, y = NULL) +
  tema_empresa() +
  theme(panel.grid.major.y = element_blank())
guardar_figura(g_sin_pastel, "m03-despues-pastel")

indice <- ventas_mes %>%
  mutate(Ingresos = ingresos / first(ingresos) * 100,
         Tickets  = tickets / first(tickets) * 100) %>%
  select(mes, Ingresos, Tickets) %>%
  pivot_longer(-mes, names_to = "medida", values_to = "indice")

g_indice <- ggplot(indice, aes(x = mes, y = indice, color = medida)) +
  geom_hline(yintercept = 100, color = color_gris, linewidth = 0.5) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_color_manual(values = paleta_libro[1:2]) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(title = "En abril cayeron ambas; de mayo a julio subió el ticket",
       subtitle = "Índice base 100 = enero de 2023",
       x = NULL, y = "Índice (enero = 100)", color = NULL) +
  tema_empresa()
guardar_figura(g_indice, "m03-despues-doble-eje")

# ---------------------------------------------------------------------------
# 15. Caso práctico: tablero de ventas 2023
# ---------------------------------------------------------------------------
kpis <- ventas_detalle %>%
  summarise(ingresos = sum(total_transaccion),
            tickets  = n(),
            ticket_promedio = mean(total_transaccion),
            clientes = n_distinct(cliente_id))

tarjetas <- tibble(
  x      = 1:4,
  valor  = c(dollar(kpis$ingresos, accuracy = 1),
             comma(kpis$tickets),
             dollar(kpis$ticket_promedio, accuracy = 1),
             comma(kpis$clientes)),
  titulo = c("Ingresos 2023", "Tickets", "Ticket promedio",
             "Clientes distintos")
)

g_kpi <- ggplot(tarjetas, aes(x = x, y = 0)) +
  geom_tile(width = 0.92, height = 1, fill = "#f3f7fd",
            color = "#cde2fb") +
  geom_text(aes(label = valor), vjust = -0.1, size = 7,
            fontface = "bold", color = "#0d366b") +
  geom_text(aes(label = titulo), vjust = 2.2, size = 3.5,
            color = "#52514e") +
  theme_void()
guardar_figura(g_kpi, "m03-tablero-kpi", alto = 1.3)

t_mensual <- ggplot(ventas_mes, aes(x = mes, y = ingresos)) +
  geom_hline(yintercept = meta_mensual, linetype = "dashed",
             color = color_gris, linewidth = 0.6) +
  geom_line(color = color_principal, linewidth = 0.8) +
  geom_point(color = color_principal, size = 2) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(labels = label_dollar(scale = 1e-3, suffix = "K")) +
  labs(title = "1. Abril fue el peor mes; diciembre, el mejor",
       subtitle = "Ingresos mensuales (línea punteada = meta de $80K)",
       x = NULL, y = NULL) +
  tema_empresa()

t_categorias <- ggplot(ingresos_categoria,
                       aes(x = ingresos,
                           y = fct_reorder(categoria, ingresos),
                           fill = grupo)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c(Destacada = color_resalte,
                               Resto     = color_gris),
                    guide = "none") +
  scale_x_continuous(labels = label_dollar(scale = 1e-3, suffix = "K"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "2. Belleza lidera", x = NULL, y = NULL) +
  tema_empresa() +
  theme(panel.grid.major.y = element_blank())

t_tiendas <- g_tema +
  scale_x_continuous(labels = label_dollar(scale = 1e-3, suffix = "K"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "3. Outlet vende más", subtitle = NULL,
       caption = NULL)

ingresos_pago <- ventas_detalle %>%
  group_by(metodo_pago) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop") %>%
  mutate(participacion = ingresos / sum(ingresos))

t_pagos <- ggplot(ingresos_pago,
                  aes(x = participacion,
                      y = fct_reorder(metodo_pago, participacion))) +
  geom_col(fill = color_principal, width = 0.6) +
  geom_text(aes(label = percent(participacion, accuracy = 1)),
            hjust = -0.2, size = 3.2, color = "#52514e") +
  scale_x_continuous(labels = percent,
                     expand = expansion(mult = c(0, 0.2))) +
  labs(title = "4. Seis de cada diez pesos\nllegan con tarjeta",
       x = NULL, y = NULL) +
  tema_empresa() +
  theme(panel.grid.major.y = element_blank())

t_calor <- g_calor +
  labs(title = "5. Sin un día fijo fuerte", subtitle = NULL) +
  tema_empresa() +
  theme(panel.grid = element_blank(), legend.position = "none",
        axis.text.x = element_text(size = 7))

guardar_figura(t_mensual, "m03-tablero-mensual", alto = 3)
guardar_figura(t_categorias, "m03-tablero-categorias", panel_ancho,
               panel_alto)
guardar_figura(t_tiendas, "m03-tablero-tiendas", panel_ancho, panel_alto)
guardar_figura(t_pagos, "m03-tablero-pagos", panel_ancho, panel_alto)
guardar_figura(t_calor, "m03-tablero-calor", panel_ancho, panel_alto)

# ===========================================================================
# Figuras de las soluciones (apendices/soluciones/sol-03.tex)
# ===========================================================================
clientes  <- read_csv("datasets/clientes.csv", show_col_types = FALSE)

# m3-5: día de la semana con más ingresos
ingresos_dia <- transacciones %>%
  mutate(dia = wday(fecha, label = TRUE, week_start = 1)) %>%
  group_by(dia) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop") %>%
  mutate(destacar = if_else(ingresos == max(ingresos), "Máximo",
                            "Resto"))

s_dia <- ggplot(ingresos_dia, aes(x = dia, y = ingresos,
                                  fill = destacar)) +
  geom_col(width = 0.7) +
  geom_text(data = filter(ingresos_dia, destacar == "Máximo"),
            aes(label = dollar(ingresos, accuracy = 1)),
            vjust = -0.5, size = 3.5, color = color_resalte) +
  scale_fill_manual(values = c(Máximo = color_resalte,
                               Resto  = color_gris),
                    guide = "none") +
  scale_y_continuous(labels = dollar,
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "El viernes es el día que más vende",
       subtitle = "Ingresos por día de la semana, 2023",
       x = NULL, y = "Ingresos (MXN)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
guardar_figura(s_dia, "m03-sol-dia")

# m3-7: precio de catálogo contra costo
productos_margen <- productos %>%
  mutate(situacion = if_else(costo > precio_catalogo,
                             "Costo mayor al precio",
                             "Margen positivo"))

s_costo <- ggplot(productos_margen,
                  aes(x = costo, y = precio_catalogo,
                      color = situacion)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = color_gris) +
  geom_point(size = 2.5, alpha = 0.8) +
  annotate("text", x = 380, y = 390, label = "precio = costo",
           angle = 20, size = 3, color = "#52514e", vjust = -0.5) +
  scale_color_manual(values = c("Margen positivo" = color_principal,
                                "Costo mayor al precio" = color_resalte)) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = dollar) +
  labs(title = "10 de 50 productos cuestan más de lo que se venden",
       subtitle = "Cada punto es un producto del catálogo",
       x = "Costo", y = "Precio de catálogo", color = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top")
guardar_figura(s_costo, "m03-sol-costo")

# m3-8: las 3 tiendas líderes con leyenda, el resto en gris
acumulado <- transacciones %>%
  left_join(tiendas, by = "tienda_id") %>%
  mutate(tienda = str_remove(nombre_tienda, "Sucursal "),
         mes = floor_date(fecha, unit = "month")) %>%
  group_by(tienda, mes) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop") %>%
  group_by(tienda) %>%
  arrange(mes, .by_group = TRUE) %>%
  mutate(acumulado = cumsum(ingresos)) %>%
  ungroup()

top3 <- acumulado %>%
  filter(mes == max(mes)) %>%
  slice_max(acumulado, n = 3) %>%
  pull(tienda)

lideres <- acumulado %>%
  filter(tienda %in% top3) %>%
  mutate(tienda = factor(tienda, levels = top3))
resto <- acumulado %>% filter(!tienda %in% top3)

s_top3 <- ggplot(mapping = aes(x = mes, y = acumulado)) +
  geom_line(data = resto, aes(group = tienda),
            color = color_gris, linewidth = 0.5) +
  geom_line(data = lideres, aes(color = tienda), linewidth = 0.9) +
  scale_color_manual(values = paleta_libro[1:3]) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(labels = dollar) +
  labs(title = "Las tres tiendas líderes al cierre de 2023",
       subtitle = "Ingresos acumulados; en gris, las otras 7 tiendas",
       x = NULL, y = "Ingresos acumulados (MXN)", color = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top")
guardar_figura(s_top3, "m03-sol-top3")

# m3-10: clientes por ciudad
clientes_ciudad <- clientes %>%
  count(ciudad, name = "clientes") %>%
  mutate(destacar = if_else(clientes == max(clientes), "Mayor",
                            "Resto"))

s_ciudades <- ggplot(clientes_ciudad,
                     aes(x = clientes,
                         y = fct_reorder(ciudad, clientes),
                         fill = destacar)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = clientes), hjust = -0.3, size = 3.2,
            color = "#52514e") +
  scale_fill_manual(values = c(Mayor = color_resalte,
                               Resto = color_gris),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Puebla encabeza por poco: 28 clientes contra 27 de CDMX",
       subtitle = "Clientes registrados por ciudad",
       x = "Clientes", y = NULL,
       caption = "Fuente: clientes.csv") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
guardar_figura(s_ciudades, "m03-sol-ciudades")
