# ==========================================================================
# sol-03.tex
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/sol-03.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Bloque 1 --------------------------------------------------------
library(tidyverse)
library(scales)
color_principal <- "#2a78d6"

transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
tiendas <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)

tickets_tienda <- transacciones %>%
  left_join(tiendas, by = "tienda_id") %>%
  mutate(tienda = str_remove(nombre_tienda, "Sucursal ")) %>%
  group_by(tienda) %>%
  summarise(tickets  = n(),
            ingresos = sum(total_transaccion),
            .groups  = "drop") %>%
  arrange(desc(tickets))
head(tickets_tienda, 4)

s1 <- ggplot(tickets_tienda,
             aes(x = tickets, y = fct_reorder(tienda, tickets))) +
  geom_col(fill = color_principal, width = 0.7) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Sur y Plaza A atendieron más tickets en 2023",
       x = "Número de tickets", y = NULL,
       caption = "Fuente: transacciones.csv") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
s1

# ---- Bloque 2 --------------------------------------------------------
library(tidyverse)
color_principal <- "#2a78d6"
color_resalte   <- "#eb6834"

clientes <- read_csv("datasets/clientes.csv", show_col_types = FALSE)
mediana_edad <- median(clientes$edad)
mediana_edad

s2 <- ggplot(clientes, aes(x = edad)) +
  geom_histogram(binwidth = 5, boundary = 0,
                 fill = color_principal, color = "white") +
  geom_vline(xintercept = mediana_edad, linetype = "dashed",
             color = color_resalte, linewidth = 0.8) +
  annotate("text", x = mediana_edad, y = Inf, vjust = 1.5,
           hjust = -0.1, label = paste("Mediana:", mediana_edad),
           color = color_resalte, size = 3.5) +
  labs(title = "Edad de los clientes",
       subtitle = "Barras de 5 años", x = "Edad", y = "Clientes") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
s2

# ---- Bloque 3 --------------------------------------------------------
library(tidyverse)
library(scales)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)

ingresos_pago <- transacciones %>%
  group_by(metodo_pago) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop")

s3 <- ggplot(ingresos_pago, aes(x = metodo_pago, y = ingresos)) +
  geom_col(fill = "#2a78d6") +
  scale_y_continuous(labels = dollar)
s3

# ---- Bloque 4 --------------------------------------------------------
library(tidyverse)
color_principal <- "#2a78d6"
color_resalte   <- "#eb6834"
color_gris      <- "#b5b3ad"

transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)

tickets_mes <- transacciones %>%
  mutate(mes = floor_date(fecha, unit = "month")) %>%
  count(mes, name = "tickets")
promedio <- mean(tickets_mes$tickets)
mes_min  <- tickets_mes %>% slice_min(tickets, n = 1)
promedio
mes_min

s4 <- ggplot(tickets_mes, aes(x = mes, y = tickets)) +
  geom_hline(yintercept = promedio, color = color_gris,
             linetype = "dashed", linewidth = 0.6) +
  geom_line(color = color_principal, linewidth = 0.8) +
  geom_point(color = color_principal, size = 2) +
  annotate("text", x = mes_min$mes, y = mes_min$tickets,
           label = paste("Mínimo:", mes_min$tickets, "tickets"),
           vjust = 1.8, color = color_resalte, size = 3.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(title = "Abril tuvo el menor número de tickets del año",
       subtitle = "Tickets por mes, 2023 (línea punteada = promedio)",
       x = NULL, y = "Tickets") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
s4

# ---- Bloque 5 --------------------------------------------------------
library(tidyverse)
library(scales)
color_resalte <- "#eb6834"
color_gris    <- "#b5b3ad"

transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)

ingresos_dia <- transacciones %>%
  mutate(dia = wday(fecha, label = TRUE, week_start = 1)) %>%
  group_by(dia) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop") %>%
  mutate(destacar = if_else(ingresos == max(ingresos), "Máximo",
                            "Resto"))
ingresos_dia

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
s_dia

# ---- Bloque 6 --------------------------------------------------------
library(tidyverse)
library(scales)
empleados <- read_csv("datasets/empleados.csv", show_col_types = FALSE)

empleados %>%
  group_by(departamento) %>%
  summarise(mediana = median(salario_mensual),
            ric     = IQR(salario_mensual),   # rango intercuartílico
            n       = n(),
            .groups = "drop") %>%
  arrange(desc(mediana))

s6 <- ggplot(empleados,
             aes(x = salario_mensual,
                 y = fct_reorder(departamento, salario_mensual,
                                 .fun = median))) +
  geom_boxplot(fill = "#cde2fb", color = "#2a78d6", width = 0.6) +
  scale_x_continuous(labels = dollar) +
  labs(title = "IT y Operaciones tienen los salarios medianos más altos",
       x = "Salario mensual (MXN)", y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
s6

# ---- Bloque 7 --------------------------------------------------------
library(tidyverse)
library(scales)
color_principal <- "#2a78d6"
color_resalte   <- "#eb6834"
color_gris      <- "#b5b3ad"

productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)

productos_margen <- productos %>%
  mutate(situacion = if_else(costo > precio_catalogo,
                             "Costo mayor al precio",
                             "Margen positivo"))
count(productos_margen, situacion)

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
s_costo

# ---- Bloque 8 --------------------------------------------------------
library(tidyverse)
library(scales)
color_gris   <- "#b5b3ad"
paleta_libro <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                  "#e87ba4", "#008300", "#4a3aa7", "#e34948")

transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
tiendas <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)

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

# las 3 con más ingresos acumulados en el último mes
top3 <- acumulado %>%
  filter(mes == max(mes)) %>%
  slice_max(acumulado, n = 3) %>%
  pull(tienda)
top3

lideres <- acumulado %>%
  filter(tienda %in% top3) %>%
  mutate(tienda = factor(tienda, levels = top3))  # orden de la leyenda
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
s_top3

# ---- Bloque 9 --------------------------------------------------------
library(tidyverse)
library(scales)
color_principal <- "#2a78d6"

tema_empresa <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(plot.title = element_text(face = "bold", color = "#0b0b0b"),
          plot.title.position = "plot",
          axis.text = element_text(color = "#52514e"),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.margin = margin(10, 15, 10, 10))
}

grafica_ranking <- function(datos, categoria, valor, titulo) {
  ggplot(datos, aes(x = {{ valor }},
                    y = fct_reorder({{ categoria }}, {{ valor }}))) +
    geom_col(fill = color_principal, width = 0.7) +
    scale_x_continuous(labels = dollar,
                       expand = expansion(mult = c(0, 0.05))) +
    labs(title = titulo, x = NULL, y = NULL) +
    tema_empresa()
}

transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
tiendas <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)

por_pago <- transacciones %>%
  group_by(metodo_pago) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop")

por_tienda <- transacciones %>%
  left_join(tiendas, by = "tienda_id") %>%
  group_by(nombre_tienda) %>%
  summarise(ingresos = sum(total_transaccion), .groups = "drop")

g_pago <- grafica_ranking(por_pago, metodo_pago, ingresos,
                          "Tarjeta de crédito, el método más usado")
g_tienda <- grafica_ranking(por_tienda, nombre_tienda, ingresos,
                            "Sucursal Outlet lidera en ingresos")

dir.create("resultados", showWarnings = FALSE)
ggsave("resultados/ranking_pago.png", g_pago, width = 7, height = 4,
       dpi = 300, bg = "white")
ggsave("resultados/ranking_tienda.png", g_tienda, width = 7, height = 4,
       dpi = 300, bg = "white")
file.exists(c("resultados/ranking_pago.png",
              "resultados/ranking_tienda.png"))

# ---- Bloque 10 --------------------------------------------------------
library(tidyverse)
library(scales)
color_principal <- "#2a78d6"
color_resalte   <- "#eb6834"
color_gris      <- "#b5b3ad"

clientes <- read_csv("datasets/clientes.csv", show_col_types = FALSE)

# (a) KPIs en consola
cat("Clientes totales:", comma(nrow(clientes)), "\n")
cat("% premium:", percent(mean(clientes$es_premium), accuracy = 0.1),
    "\n")
cat("Edad mediana:", median(clientes$edad), "años\n")

# (b) clientes por ciudad, resaltando la mayor
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
s_ciudades

# ---- Bloque 11 --------------------------------------------------------
premium_segmento <- clientes %>%
  mutate(segmento = factor(segmento,
                           levels = c("Joven", "Adulto", "Maduro",
                                      "Senior"))) %>%
  group_by(segmento) %>%
  summarise(clientes = n(), pct_premium = mean(es_premium),
            .groups = "drop")
premium_segmento

s_premium <- ggplot(premium_segmento,
                    aes(x = segmento, y = pct_premium)) +
  geom_col(fill = color_principal, width = 0.6) +
  geom_text(aes(label = percent(pct_premium, accuracy = 1)),
            vjust = -0.5, size = 3.5, color = "#52514e") +
  scale_y_continuous(labels = percent, limits = c(0, 0.5),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "El segmento Maduro tiene más clientes premium",
       subtitle = "% de clientes premium por segmento de edad",
       x = NULL, y = NULL, caption = "Fuente: clientes.csv") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

dir.create("resultados", showWarnings = FALSE)
ggsave("resultados/clientes_ciudad.png", s_ciudades, width = 7,
       height = 4, dpi = 300, bg = "white")
ggsave("resultados/premium_segmento.png", s_premium, width = 7,
       height = 4, dpi = 300, bg = "white")
