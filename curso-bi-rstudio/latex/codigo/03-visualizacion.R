# ==========================================================================
# Visualización de datos con ggplot2
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/03-visualizacion.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Bloque 1 --------------------------------------------------------
library(tidyverse)   # incluye ggplot2, dplyr, tidyr, forcats...
library(scales)      # formatos de ejes: dólares, comas, porcentajes

# ---- Bloque 2 --------------------------------------------------------
anscombe_largo <- anscombe %>%
  pivot_longer(cols = everything(),
               # ".value" = la 1a letra (x o y) será el nombre de columna
               names_to = c(".value", "conjunto"),
               # el patrón separa "x1" en "x" y "1"
               names_pattern = "(.)(.)") %>%
  mutate(conjunto = paste("Conjunto", conjunto))

# Las estadísticas de cada conjunto
anscombe_largo %>%
  group_by(conjunto) %>%
  summarise(media_x     = mean(x),
            media_y     = mean(y),
            desv_y      = sd(y),
            correlacion = cor(x, y),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# ---- Bloque 3 --------------------------------------------------------
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
g_anscombe

# ---- Bloque 4 --------------------------------------------------------
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)
tiendas   <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)

ventas_detalle <- transacciones %>%
  left_join(select(productos, producto_id, categoria),
            by = "producto_id") %>%
  left_join(select(tiendas, tienda_id, nombre_tienda),
            by = "tienda_id") %>%
  # "Sucursal Outlet" -> "Outlet": etiquetas más cortas para las gráficas
  mutate(tienda = str_remove(nombre_tienda, "Sucursal "))

ventas_detalle %>%
  select(fecha, categoria, tienda, metodo_pago, total_transaccion) %>%
  head(4)

# ---- Bloque 5 --------------------------------------------------------
ventas_mes <- ventas_detalle %>%
  mutate(mes = floor_date(fecha, unit = "month")) %>%
  group_by(mes) %>%
  summarise(ingresos = sum(total_transaccion),
            tickets  = n(),
            .groups  = "drop")
ventas_mes

# ---- Bloque 6 --------------------------------------------------------
# Paso 1: datos + estéticas. Solo el "lienzo" con los ejes
paso1 <- ggplot(data = ventas_mes, mapping = aes(x = mes, y = ingresos))
paso1

# ---- Bloque 7 --------------------------------------------------------
# Paso 2: + una geometría de línea (color y grosor fijos, fuera de aes)
paso2 <- paso1 + geom_line(color = "#2a78d6", linewidth = 0.8)
paso2

# ---- Bloque 8 --------------------------------------------------------
# Paso 3: + puntos en cada mes y + escala del eje y en dólares
paso3 <- paso2 +
  geom_point(color = "#2a78d6", size = 2) +
  scale_y_continuous(labels = dollar)
paso3

# ---- Bloque 9 --------------------------------------------------------
# Paso 4: + títulos y + un tema limpio
paso4 <- paso3 +
  labs(title = "Ingresos mensuales 2023", x = NULL, y = "Ingresos") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
paso4

# ---- Bloque 10 --------------------------------------------------------
color_principal <- "#2a78d6"   # azul: para una sola serie
color_resalte   <- "#eb6834"   # naranja: para destacar UN elemento
color_gris      <- "#b5b3ad"   # gris: para "el resto"
# Paleta para varias series (usar en este orden, máximo 8)
paleta_libro <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                  "#e87ba4", "#008300", "#4a3aa7", "#e34948")

# ---- Bloque 11 --------------------------------------------------------
# geom_bar() cuenta cuántas filas (tickets) hay de cada método de pago
g_conteo <- ggplot(ventas_detalle, aes(x = metodo_pago)) +
  geom_bar(fill = color_principal)

# Es lo mismo que contar primero y luego usar geom_col():
ventas_detalle %>% count(metodo_pago)

# ---- Bloque 12 --------------------------------------------------------
ingresos_categoria <- ventas_detalle %>%
  group_by(categoria) %>%
  summarise(ingresos = sum(total_transaccion),
            tickets  = n(),
            .groups  = "drop") %>%
  mutate(participacion = ingresos / sum(ingresos)) %>%   # % del total
  arrange(desc(ingresos))
ingresos_categoria

# ---- Bloque 13 --------------------------------------------------------
g_categorias <- ggplot(ingresos_categoria,
                       aes(x = ingresos,
                           # ordena las categorías según sus ingresos
                           y = fct_reorder(categoria, ingresos))) +
  # UN solo color: todas las barras miden lo mismo (ingresos)
  geom_col(fill = color_principal, width = 0.7) +
  scale_x_continuous(labels = dollar,
                     # las barras arrancan pegadas al eje (sin hueco)
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Ingresos por categoría, 2023",
       x = "Ingresos (MXN)", y = NULL,
       caption = "Fuente: transacciones.csv y productos.csv") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        # sin líneas horizontales: las barras ya marcan las filas
        panel.grid.major.y = element_blank())
g_categorias

# ---- Bloque 14 --------------------------------------------------------
ingresos_categoria <- ingresos_categoria %>%
  mutate(grupo = if_else(categoria == "Belleza", "Destacada", "Resto"))

g_resalte <- ggplot(ingresos_categoria,
                    aes(x = ingresos,
                        y = fct_reorder(categoria, ingresos),
                        fill = grupo)) +          # el color SÍ significa algo
  geom_col(width = 0.7) +
  # etiqueta solo en la barra destacada (no un número en cada barra)
  geom_text(data = filter(ingresos_categoria, grupo == "Destacada"),
            aes(label = dollar(ingresos)),
            hjust = 1.1, color = "white", fontface = "bold",
            size = 3.5) +
  scale_fill_manual(values = c(Destacada = color_resalte,
                               Resto     = color_gris),
                    guide = "none") +              # sin leyenda
  scale_x_continuous(labels = dollar,
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Belleza aporta 19% de los ingresos del año",
       subtitle = "Ingresos por categoría, 2023",
       x = "Ingresos (MXN)", y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
g_resalte

# ---- Bloque 15 --------------------------------------------------------
top5 <- ingresos_categoria %>%
  slice_max(ingresos, n = 5) %>%     # las 5 con más ingresos
  pull(categoria)                    # como vector de texto

ingresos_semestre <- ventas_detalle %>%
  filter(categoria %in% top5) %>%
  mutate(semestre = if_else(month(fecha) <= 6, "Ene-Jun", "Jul-Dic")) %>%
  group_by(categoria, semestre) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop")

ingresos_semestre %>%
  pivot_wider(names_from = semestre, values_from = ingresos) %>%
  mutate(cambio = `Jul-Dic` / `Ene-Jun` - 1)

# ---- Bloque 16 --------------------------------------------------------
g_agrupadas <- ggplot(ingresos_semestre,
                      aes(x = fct_reorder(categoria, ingresos,
                                          .fun = sum, .desc = TRUE),
                          y = ingresos, fill = semestre)) +
  # dodge = una barra al lado de la otra (no encimadas)
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = paleta_libro[1:2]) +   # 2 series, 2 colores
  scale_y_continuous(labels = dollar,
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Belleza cayó 41% en el segundo semestre",
       subtitle = "Ingresos de las 5 categorías principales, 2023",
       x = NULL, y = "Ingresos (MXN)", fill = "Semestre") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top")                    # leyenda arriba
g_agrupadas

# ---- Bloque 17 --------------------------------------------------------
pagos_trimestre <- ventas_detalle %>%
  group_by(trimestre, metodo_pago) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop")

g_100 <- ggplot(pagos_trimestre,
                aes(x = trimestre, y = ingresos,
                    # el método más grande primero (abajo y en la leyenda)
                    fill = fct_reorder(metodo_pago, ingresos,
                                       .fun = sum, .desc = TRUE))) +
  geom_col(position = position_fill(reverse = TRUE), width = 0.7) +
  scale_fill_manual(values = paleta_libro[1:4]) +
  scale_y_continuous(labels = percent) +            # 0.25 -> 25%
  labs(title = "La mezcla de pagos casi no cambia en el año",
       subtitle = "Participación de cada método de pago en los ingresos",
       x = NULL, y = "% de los ingresos", fill = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top")
g_100

# ---- Bloque 18 --------------------------------------------------------
meta_mensual <- 80000
mes_record <- ventas_mes %>% slice_max(ingresos, n = 1)   # mejor mes
sum(ventas_mes$ingresos >= meta_mensual)    # meses que alcanzaron meta

# ---- Bloque 19 --------------------------------------------------------
g_linea <- ggplot(ventas_mes, aes(x = mes, y = ingresos)) +
  # 1) la meta: línea horizontal punteada y gris (es referencia, no dato)
  geom_hline(yintercept = meta_mensual, linetype = "dashed",
             color = color_gris, linewidth = 0.6) +
  # 2) los datos reales
  geom_line(color = color_principal, linewidth = 0.8) +
  geom_point(color = color_principal, size = 2) +
  # 3) anotaciones: texto de la meta y punto + texto del récord
  annotate("text", x = as.Date("2023-09-15"), y = meta_mensual,
           label = "Meta mensual: $80,000", vjust = -0.6,
           size = 3.2, color = "#52514e") +
  annotate("point", x = mes_record$mes, y = mes_record$ingresos,
           color = color_resalte, size = 3.5) +
  annotate("text", x = mes_record$mes, y = mes_record$ingresos,
           label = paste("Récord del año:",
                         dollar(mes_record$ingresos, accuracy = 1)),
           hjust = 1, vjust = -1.2, size = 3.2, color = color_resalte) +
  # 4) eje x: una marca por mes, con el nombre corto del mes
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(labels = dollar,
                     expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "7 de 12 meses superaron la meta",
       subtitle = "Ingresos mensuales 2023 contra la meta de $80,000",
       x = NULL, y = "Ingresos (MXN)",
       caption = "Fuente: transacciones.csv") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
g_linea

# ---- Bloque 20 --------------------------------------------------------
metas <- tibble(
  mes  = seq(as.Date("2023-01-01"), as.Date("2023-12-01"),
             by = "month"),
  meta = c(75, 70, 80, 75, 80, 85, 85, 80, 75, 80, 85, 95) * 1000
)

real_vs_meta <- ventas_mes %>%
  select(mes, Real = ingresos) %>%
  left_join(rename(metas, Meta = meta), by = "mes") %>%
  # de ancho (Real, Meta) a largo (serie, monto)
  pivot_longer(cols = c(Real, Meta),
               names_to = "serie", values_to = "monto") %>%
  mutate(serie = fct_relevel(serie, "Real"))   # "Real" primero
head(real_vs_meta, 4)

# ---- Bloque 21 --------------------------------------------------------
g_real_meta <- ggplot(real_vs_meta,
                      aes(x = mes, y = monto,
                          color = serie, linetype = serie)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = filter(real_vs_meta, serie == "Real"),
             size = 2) +                        # puntos solo en lo real
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
g_real_meta

# ---- Bloque 22 --------------------------------------------------------
acumulado_tienda <- ventas_detalle %>%
  mutate(mes = floor_date(fecha, unit = "month")) %>%
  group_by(tienda, mes) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop") %>%
  arrange(tienda, mes) %>%
  group_by(tienda) %>%
  mutate(acumulado = cumsum(ingresos)) %>%     # suma corrida por tienda
  ungroup() %>%
  mutate(grupo = if_else(tienda == "Outlet", "Outlet",
                         "Otras 9 tiendas"))

g_carrera <- ggplot(acumulado_tienda,
                    aes(x = mes, y = acumulado,
                        group = tienda, color = grupo)) +
  # primero las grises (quedan al fondo), luego la destacada encima
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
g_carrera

# ---- Bloque 23 --------------------------------------------------------
g_precio_cantidad <- ggplot(ventas_detalle,
                            aes(x = precio_venta, y = cantidad)) +
  # jitter: mueve cada punto un poquito en vertical para que no se
  # encimen (cantidad solo vale 1, 2, 3, 4 o 5); alpha = transparencia
  geom_point(position = position_jitter(width = 0, height = 0.15,
                                        seed = 123),
             color = color_principal, alpha = 0.35) +
  # recta de tendencia por mínimos cuadrados con su banda de confianza
  geom_smooth(method = "lm", formula = y ~ x,
              color = color_resalte, linewidth = 0.8) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(breaks = 1:5) +
  labs(title = "El precio no cambia cuántas piezas lleva el cliente",
       subtitle = "Cada punto es un ticket de 2023 (1,000 en total)",
       x = "Precio unitario (MXN)", y = "Piezas por ticket") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
g_precio_cantidad

cor(ventas_detalle$precio_venta, ventas_detalle$cantidad)

# ---- Bloque 24 --------------------------------------------------------
set.seed(123)
campanas <- tibble(
  canal     = rep(c("Correo", "Redes", "Radio"), each = 40),
  # inversiones entre 10^3 y 10^6: $1,000 a $1,000,000
  inversion = round(10^runif(120, min = 3, max = 6))
) %>%
  mutate(retorno = case_when(canal == "Correo" ~ 6,
                             canal == "Redes"  ~ 3,
                             canal == "Radio"  ~ 1.5),
         ventas = round(inversion * retorno *
                          exp(rnorm(n(), mean = 0, sd = 0.35))))

# formato corto: 1500000 -> $1.5M
dinero_corto <- label_dollar(scale_cut = cut_short_scale())

g_campanas <- ggplot(campanas, aes(x = inversion, y = ventas,
                                   color = canal)) +   # 3 grupos
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
  scale_x_log10(labels = dinero_corto) +    # ejes logarítmicos
  scale_y_log10(labels = dinero_corto) +
  labs(title = "Escala logarítmica")
g_lineal
g_log

# ---- Bloque 25 --------------------------------------------------------
g_hist <- ggplot(ventas_detalle, aes(x = total_transaccion)) +
  # binwidth = ancho de cada barra ($200); boundary = 0 hace que los
  # cortes caigan en 0, 200, 400...
  geom_histogram(binwidth = 200, boundary = 0,
                 fill = color_principal, color = "white") +
  scale_x_continuous(labels = dollar) +
  labs(title = "Histograma (conteos)", x = "Importe del ticket",
       y = "Número de tickets") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

g_densidad <- ggplot(ventas_detalle, aes(x = total_transaccion)) +
  # after_stat(density): la altura pasa de conteo a densidad para que
  # el histograma y la curva compartan la misma escala
  geom_histogram(aes(y = after_stat(density)), binwidth = 200,
                 boundary = 0, fill = color_gris, color = "white") +
  geom_density(color = color_principal, linewidth = 0.8) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = label_number(accuracy = 0.0001)) +
  labs(title = "Histograma + densidad", x = "Importe del ticket",
       y = "Densidad") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
g_hist
g_densidad

# ---- Bloque 26 --------------------------------------------------------
ventas_detalle %>%
  group_by(metodo_pago) %>%
  summarise(q1      = quantile(total_transaccion, 0.25),
            mediana = median(total_transaccion),
            q3      = quantile(total_transaccion, 0.75),
            promedio = mean(total_transaccion),
            .groups = "drop") %>%
  arrange(desc(mediana))

# ---- Bloque 27 --------------------------------------------------------
g_caja <- ggplot(ventas_detalle,
                 aes(x = total_transaccion,
                     # ordena los métodos por su mediana
                     y = fct_reorder(metodo_pago, total_transaccion,
                                     .fun = median))) +
  geom_boxplot(fill = "#cde2fb", color = color_principal,
               width = 0.6, outlier.color = color_resalte) +
  # agrega el promedio como un rombo oscuro
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3,
               color = "#0d366b") +
  scale_x_continuous(labels = dollar) +
  labs(title = "Transferencia: tickets más bajos y varios atípicos",
       subtitle = "Importe por ticket según método de pago (rombo = promedio)",
       x = "Importe del ticket (MXN)", y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
g_caja

# ---- Bloque 28 --------------------------------------------------------
g_violin <- ggplot(ventas_detalle,
                   aes(x = total_transaccion, y = metodo_pago)) +
  geom_violin(fill = "#cde2fb", color = color_principal) +
  scale_x_continuous(labels = dollar) +
  theme_minimal()

# ---- Bloque 29 --------------------------------------------------------
calor <- ventas_detalle %>%
  mutate(mes = month(fecha, label = TRUE),                 # ene, feb...
         dia = wday(fecha, label = TRUE, week_start = 1)) %>% # lun...dom
  group_by(mes, dia) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop")

calor %>% slice_max(ingresos, n = 3)
calor %>% slice_min(ingresos, n = 1)

# ---- Bloque 30 --------------------------------------------------------
g_calor <- ggplot(calor, aes(x = mes, y = fct_rev(dia), fill = ingresos)) +
  # cada celda es un rectángulo; el borde blanco separa las celdas
  geom_tile(color = "white", linewidth = 0.6) +
  # gradiente SECUENCIAL de un solo tono: claro = poco, oscuro = mucho
  scale_fill_gradient(low = "#cde2fb", high = "#0d366b",
                      labels = dollar) +
  labs(title = "Los días fuertes cambian de un mes a otro",
       subtitle = "Ingresos por día de la semana y mes, 2023",
       x = NULL, y = NULL, fill = "Ingresos") +
  theme_minimal() +
  theme(panel.grid = element_blank(),     # sin cuadrícula
        legend.position = "right")
g_calor

# ---- Bloque 31 --------------------------------------------------------
categoria_mes <- ventas_detalle %>%
  mutate(mes = floor_date(fecha, unit = "month")) %>%
  group_by(categoria, mes) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop") %>%
  # si una categoría no vendió en un mes, ese mes no existe: lo creamos
  # con ingresos = 0 para que la línea no "salte" el mes
  complete(categoria, mes, fill = list(ingresos = 0))

g_facetas <- ggplot(categoria_mes, aes(x = mes, y = ingresos)) +
  geom_line(color = color_principal, linewidth = 0.7) +
  # un panel por categoría, ordenados de mayor a menor ingreso anual
  facet_wrap(~ fct_reorder(categoria, ingresos, .fun = sum,
                           .desc = TRUE),
             ncol = 5) +
  scale_x_date(date_labels = "%b", date_breaks = "4 months") +
  scale_y_continuous(labels = label_number(scale = 1e-3)) +
  labs(title = "Ingresos mensuales por categoría (misma escala)",
       x = NULL, y = "Ingresos (miles de MXN)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
g_facetas

# ---- Bloque 32 --------------------------------------------------------
g_facetas_libre <- g_facetas +
  facet_wrap(~ fct_reorder(categoria, ingresos, .fun = sum,
                           .desc = TRUE),
             ncol = 5, scales = "free_y") +   # eje y propio por panel
  labs(title = "Ingresos mensuales por categoría (escala libre)")
g_facetas_libre

# ---- Bloque 33 --------------------------------------------------------
montos <- c(500, 25000, 750000, 1200000, 3500000)
dollar(montos)                                    # moneda
comma(montos)                                     # separador de miles
percent(c(0.19, 0.0877, 0.366), accuracy = 0.1)   # porcentajes
label_dollar(scale = 1e-3, suffix = "K")(montos)  # en miles
label_number(scale_cut = cut_short_scale())(montos)   # K, M automáticos
label_dollar(scale_cut = cut_short_scale())(montos)

# ---- Bloque 34 --------------------------------------------------------
ingresos_tienda <- ventas_detalle %>%
  group_by(tienda) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop")

g_truncado <- ggplot(ingresos_tienda,
                     aes(x = ingresos,
                         y = fct_reorder(tienda, ingresos))) +
  geom_col(fill = color_principal, width = 0.7) +
  coord_cartesian(xlim = c(78000, 116000)) +   # el eje empieza en 78K
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
g_truncado
g_honesto

# ---- Bloque 35 --------------------------------------------------------
tema_empresa <- function(base_size = 11) {
  # (1) punto de partida: tema minimalista con el tamaño de letra base
  theme_minimal(base_size = base_size) +
    theme(
      # (2) jerarquía de textos
      plot.title    = element_text(face = "bold", color = "#0b0b0b"),
      plot.subtitle = element_text(color = "#52514e"),
      plot.caption  = element_text(color = "#52514e", size = rel(0.8),
                                   hjust = 0),     # fuente a la izquierda
      plot.title.position   = "plot",   # título alineado a la izquierda
      plot.caption.position = "plot",
      axis.title    = element_text(color = "#52514e"),
      axis.text     = element_text(color = "#52514e"),
      # (3) limpieza
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e6e5e1",
                                      linewidth = 0.3),
      legend.position = "top",
      legend.title    = element_text(color = "#52514e"),
      plot.margin     = margin(10, 15, 10, 10)  # arriba, der., abajo, izq.
    )
}

# ---- Bloque 36 --------------------------------------------------------
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
  tema_empresa() +                               # nuestro tema
  theme(panel.grid.major.y = element_blank())    # ajuste puntual
g_tema

# ---- Bloque 37 --------------------------------------------------------
# ANTES: lo que sale "por defecto" si pones fill = tienda
g_antes <- ggplot(ingresos_tienda, aes(x = tienda, y = ingresos,
                                       fill = tienda)) +
  geom_col() +
  labs(title = "Antes")

# DESPUÉS: un color, horizontal, ordenado, eje con formato
g_despues <- ggplot(ingresos_tienda,
                    aes(x = ingresos, y = fct_reorder(tienda, ingresos))) +
  geom_col(fill = color_principal, width = 0.7) +
  scale_x_continuous(labels = label_dollar(scale = 1e-3, suffix = "K"),
                     breaks = seq(0, 120000, by = 40000),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Después", x = "Ingresos", y = NULL) +
  tema_empresa() +
  theme(panel.grid.major.y = element_blank())
g_antes
g_despues

# ---- Bloque 38 --------------------------------------------------------
# ANTES (no lo hagas): pastel de 10 porciones
g_pastel <- ggplot(ingresos_categoria,
                   aes(x = "", y = ingresos, fill = categoria)) +
  geom_col() +
  coord_polar(theta = "y")

# ---- Bloque 39 --------------------------------------------------------
g_sin_pastel <- ggplot(ingresos_categoria,
                       aes(x = participacion,
                           y = fct_reorder(categoria, participacion))) +
  geom_col(fill = color_principal, width = 0.7) +
  # aquí sí etiquetamos cada barra: son pocas y el % es el mensaje
  geom_text(aes(label = percent(participacion, accuracy = 0.1)),
            hjust = -0.15, size = 3.2, color = "#52514e") +
  scale_x_continuous(labels = percent,
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Cinco categorías concentran el 69% de los ingresos",
       subtitle = "Participación de cada categoría en los ingresos 2023",
       x = NULL, y = NULL) +
  tema_empresa() +
  theme(panel.grid.major.y = element_blank())
g_sin_pastel

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # ANTES (no lo hagas): segundo eje con sec_axis()
# ggplot(ventas_mes, aes(x = mes)) +
#   geom_col(aes(y = ingresos)) +
#   geom_line(aes(y = tickets * 1000)) +              # escala inventada
#   scale_y_continuous(sec.axis = sec_axis(~ . / 1000, name = "Tickets"))

# ---- Bloque 40 --------------------------------------------------------
indice <- ventas_mes %>%
  mutate(Ingresos = ingresos / first(ingresos) * 100,   # enero = 100
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
g_indice

# ---- Bloque 41 --------------------------------------------------------
dir.create("resultados", showWarnings = FALSE)   # crea la carpeta

# PNG para PowerPoint o correo: 300 puntos por pulgada (alta calidad)
ggsave("resultados/ingresos_categoria.png", plot = g_resalte,
       width = 8, height = 4.5, units = "in", dpi = 300, bg = "white")

# PDF para imprimir o para LaTeX: vectorial, no se pixelea al ampliar
ggsave("resultados/ingresos_categoria.pdf", plot = g_resalte,
       width = 8, height = 4.5)

file.exists(c("resultados/ingresos_categoria.png",
              "resultados/ingresos_categoria.pdf"))

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # install.packages("plotly")      # solo la primera vez
# library(plotly)
#
# # Convierte cualquier ggplot en interactiva: se abre en el visor
# # (panel Viewer) de RStudio o en el navegador
# ggplotly(g_linea)
#
# # Controla qué se muestra al pasar el ratón con la estética "text"
# g_interactiva <- ggplot(ventas_mes,
#                         aes(x = mes, y = ingresos,
#                             text = paste("Mes:", format(mes, "%B"),
#                                          "<br>Ingresos:", dollar(ingresos),
#                                          "<br>Tickets:", tickets))) +
#   geom_point(color = color_principal, size = 2) +
#   tema_empresa()
# ggplotly(g_interactiva, tooltip = "text")
#
# # Guardar como página HTML para compartir por correo o intranet
# htmlwidgets::saveWidget(ggplotly(g_linea),
#                         "resultados/ingresos_mensuales.html")

# ---- Bloque 42 --------------------------------------------------------
kpis <- ventas_detalle %>%
  summarise(ingresos = sum(total_transaccion),
            tickets  = n(),
            ticket_promedio = mean(total_transaccion),
            clientes = n_distinct(cliente_id))   # clientes distintos
kpis

# ---- Bloque 43 --------------------------------------------------------
tarjetas <- tibble(
  x      = 1:4,                              # posición de cada tarjeta
  valor  = c(dollar(kpis$ingresos, accuracy = 1),
             comma(kpis$tickets),
             dollar(kpis$ticket_promedio, accuracy = 1),
             comma(kpis$clientes)),
  titulo = c("Ingresos 2023", "Tickets", "Ticket promedio",
             "Clientes distintos")
)

g_kpi <- ggplot(tarjetas, aes(x = x, y = 0)) +
  geom_tile(width = 0.92, height = 1, fill = "#f3f7fd",
            color = "#cde2fb") +                     # la tarjeta
  geom_text(aes(label = valor), vjust = -0.1, size = 7,
            fontface = "bold", color = "#0d366b") +  # el número grande
  geom_text(aes(label = titulo), vjust = 2.2, size = 3.5,
            color = "#52514e") +                     # la etiqueta
  theme_void()                                       # sin ejes
g_kpi

# ---- Bloque 44 --------------------------------------------------------
# 1. Evolución mensual contra la meta
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

# 2. Categorías, con Belleza resaltada
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

# ---- Bloque 45 --------------------------------------------------------
# 3. Tiendas: reutiliza g_tema con otra escala y otro título
t_tiendas <- g_tema +
  scale_x_continuous(labels = label_dollar(scale = 1e-3, suffix = "K"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "3. Outlet vende más", subtitle = NULL,
       caption = NULL)

# ---- Bloque 46 --------------------------------------------------------
# 4. Métodos de pago: participación en los ingresos
ingresos_pago <- ventas_detalle %>%
  group_by(metodo_pago) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop") %>%
  mutate(participacion = ingresos / sum(ingresos))
ingresos_pago

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

# 5. Mapa de calor: reutiliza g_calor, sin leyenda para ahorrar espacio
t_calor <- g_calor +
  labs(title = "5. Sin un día fijo fuerte", subtitle = NULL) +
  tema_empresa() +
  theme(panel.grid = element_blank(), legend.position = "none",
        axis.text.x = element_text(size = 7))

# ---- Bloque 47 --------------------------------------------------------
dir.create("resultados/tablero", showWarnings = FALSE, recursive = TRUE)

piezas <- list(kpis = g_kpi, mensual = t_mensual,
               categorias = t_categorias, tiendas = t_tiendas,
               pagos = t_pagos, calor = t_calor)

for (nombre in names(piezas)) {
  ggsave(file.path("resultados/tablero", paste0(nombre, ".png")),
         plot = piezas[[nombre]], width = 6, height = 4, dpi = 300,
         bg = "white")
}
list.files("resultados/tablero")

# ---- Solo referencia (no se ejecuta automáticamente) ----
# ggplot(ingresos_categoria, aes(x = categoria, y = ingresos))
#   + geom_col()

# ---- Solo referencia (no se ejecuta automáticamente) ----
# ggplot(ingresos_categoria, aes(x = categoria, y = ingresos)) %>%
#   geom_col()

# ---- Bloque 48 --------------------------------------------------------
g_rara <- ggplot(ventas_mes, aes(x = mes, y = ingresos)) +
  geom_line(aes(color = "azul"))     # MAL: "azul" se trata como dato
layer_data(g_rara)$colour[1]         # ¿qué color usó realmente?

# ---- Solo referencia (no se ejecuta automáticamente) ----
# ggplot(ingresos_categoria, aes(x = categoria, y = ingresos)) +
#   geom_bar()

# ---- Solo referencia (no se ejecuta automáticamente) ----
# ggplot(ingresos_categoria, aes(x = categoria, y = ingresos)) +
#   geom_col() +
#   scale_x_continuous(labels = comma)    # pero x es texto

# ---- Bloque 49 --------------------------------------------------------
meses_texto <- format(ventas_mes$mes, "%b")   # "ene", "feb"... (texto)
sort(meses_texto)                             # así los ordena ggplot2
