# ==========================================================================
# sol-08.tex
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/sol-08.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Bloque 1 --------------------------------------------------------
library(tidyverse)
library(knitr)
clientes <- read_csv("datasets/clientes.csv", show_col_types = FALSE)

# ---- Bloque 2 --------------------------------------------------------
nrow(clientes)
sum(clientes$es_premium)
round(mean(clientes$es_premium) * 100, 1)
clientes %>%
  count(ciudad, sort = TRUE) %>%
  kable(format = "pipe", col.names = c("Ciudad", "Clientes"))

# ---- Bloque 3 --------------------------------------------------------
library(tidyverse)
library(knitr)
library(scales)

transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)

por_categoria <- transacciones %>%
  left_join(productos %>% select(producto_id, categoria),
            by = "producto_id") %>%
  group_by(categoria) %>%
  summarise(ventas = sum(total_transaccion),
            unidades = sum(cantidad), .groups = "drop") %>%
  arrange(desc(ventas)) %>%                  # ordenar con números
  mutate(participacion = percent(ventas / sum(ventas), accuracy = 0.1),
         ventas = dollar(ventas, accuracy = 1),
         unidades = comma(unidades))

kable(por_categoria, format = "pipe",
      col.names = c("Categoría", "Ventas", "Unidades", "% del total"),
      align = c("l", "r", "r", "r"),
      caption = "Ventas 2023 por categoría")

# ---- Bloque 4 --------------------------------------------------------
library(tidyverse)
library(scales)

frase_meta <- function(ventas, meta) {
  cumplimiento <- ventas / meta
  montos <- paste0("(ventas de ", dollar(ventas, accuracy = 1),
                   " contra meta de ", dollar(meta, accuracy = 1), ").")
  if (ventas >= meta) {
    paste("Se superó la meta en",
          percent(cumplimiento - 1, accuracy = 0.1), montos)
  } else if (cumplimiento >= 0.9) {
    paste("Se alcanzó el", percent(cumplimiento, accuracy = 0.1),
          "de la meta", montos)
  } else {
    paste("Alerta: solo se alcanzó el",
          percent(cumplimiento, accuracy = 0.1), "de la meta", montos)
  }
}

# Los tres casos
frase_meta(110000, 100000)
frase_meta(95000, 100000)
frase_meta(70000, 100000)

# Aplicación a los trimestres de 2023
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
trimestres <- transacciones %>%
  group_by(trimestre) %>%
  summarise(ventas = sum(total_transaccion), .groups = "drop") %>%
  mutate(frase = map_chr(ventas, frase_meta, meta = 220000))
cat(paste0(trimestres$trimestre, ": ", trimestres$frase), sep = "\n")

# ---- Solo referencia (no se ejecuta automáticamente) ----
# rmarkdown::render("reportes/ej4-opciones.Rmd", output_format = "all")

# ---- Bloque 5 --------------------------------------------------------
library(tidyverse)
library(scales)
ventas <- read_csv("datasets/ventas_retail.csv", show_col_types = FALSE)
por_mes <- ventas %>%
  group_by(mes) %>%
  summarise(ventas = sum(total), .groups = "drop")
top3 <- por_mes %>% arrange(desc(ventas)) %>% slice_head(n = 3)
cat(paste0("- **", top3$mes, "**: ", dollar(top3$ventas, accuracy = 1)),
    sep = "\n")

# ---- Solo referencia (no se ejecuta automáticamente) ----
# for (cat_actual in c("Ropa", "Hogar")) {
#   rmarkdown::render(
#     "reportes/ej5-categoria.Rmd",
#     params        = list(categoria = cat_actual),
#     output_file   = paste0("categoria_", tolower(cat_actual), ".html"),
#     output_dir    = "resultados",
#     knit_root_dir = getwd(),
#     envir         = new.env()
#   )
# }

# ---- Bloque 6 --------------------------------------------------------
library(tidyverse)
library(scales)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)
transacciones %>%
  left_join(productos, by = "producto_id") %>%
  filter(categoria %in% c("Ropa", "Hogar")) %>%
  group_by(categoria) %>%
  summarise(ventas = dollar(sum(total_transaccion), accuracy = 1),
            transacciones = n(), .groups = "drop")

# ---- Bloque 7 --------------------------------------------------------
library(tidyverse)
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
tiendas <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)

limpiar_nombre <- function(x) {
  x <- chartr("áéíóúüñÁÉÍÓÚÜÑ", "aeiouunAEIOUUN", x)
  x <- gsub("[^A-Za-z0-9]+", "-", x)
  tolower(gsub("^-|-$", "", x))
}

conteo <- transacciones %>% count(tienda_id, trimestre,
                                  name = "n_transacciones")

trabajos <- expand_grid(tienda_id = tiendas$tienda_id,
                        trimestre = paste0("Q", 1:4)) %>%
  left_join(tiendas %>% select(tienda_id, nombre_tienda),
            by = "tienda_id") %>%
  left_join(conteo, by = c("tienda_id", "trimestre")) %>%
  mutate(n_transacciones = replace_na(n_transacciones, 0),
         archivo = sprintf("2023-%s_T%02d_%s.pdf", trimestre, tienda_id,
                           limpiar_nombre(nombre_tienda)),
         generar = n_transacciones >= 20)

nrow(trabajos)
trabajos %>% arrange(n_transacciones) %>%
  select(archivo, n_transacciones, generar) %>%
  head(4)
sum(trabajos$generar)

# ---- Bloque 8 --------------------------------------------------------
library(tidyverse)
library(writexl)
library(readxl)

transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
tiendas <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)

detalle <- transacciones %>%
  left_join(tiendas %>% select(tienda_id, nombre_tienda),
            by = "tienda_id") %>%
  mutate(hoja = str_remove(nombre_tienda, "Sucursal ")) %>%
  arrange(tienda_id, fecha)

resumen <- detalle %>%
  group_by(hoja) %>%
  summarise(ventas = round(sum(total_transaccion), 2),
            transacciones = n(), .groups = "drop") %>%
  arrange(desc(ventas))

# Una hoja por tienda (sin la columna auxiliar "hoja")
por_tienda <- split(detalle %>% select(-hoja), detalle$hoja)

dir.create("resultados", showWarnings = FALSE)
archivo <- "resultados/ventas_por_tienda.xlsx"
write_xlsx(c(list(Resumen = resumen), por_tienda), path = archivo)

# Verificación
hojas <- excel_sheets(archivo)
length(hojas)
hojas
filas <- sapply(hojas[-1], function(h) nrow(read_excel(archivo, sheet = h)))
sum(filas)

# ---- Bloque 9 --------------------------------------------------------
library(tidyverse)
library(scales)

transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
tiendas <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)

revisar_tienda <- function(id, mes) {
  if (!id %in% tiendas$tienda_id) stop("La tienda ", id, " no existe.")
  if (!mes %in% transacciones$mes) stop("No hay datos del mes ", mes)
  mes_previo <- format(as.Date(paste0(mes, "-01")) - 1, "%Y-%m")
  if (!mes_previo %in% transacciones$mes) {
    stop("No hay datos del mes anterior (", mes_previo, ")")
  }
  v <- transacciones %>% filter(tienda_id == id)
  actual   <- sum(v$total_transaccion[v$mes == mes])
  anterior <- sum(v$total_transaccion[v$mes == mes_previo])
  variacion <- actual / anterior - 1
  tibble(tienda_id = id,
         tienda = tiendas$nombre_tienda[tiendas$tienda_id == id],
         mes = mes, variacion = variacion,
         estado = case_when(variacion >  0.10 ~ "Crecimiento sólido",
                            variacion >= 0    ~ "Estable",
                            variacion > -0.10 ~ "Ligera caída",
                            TRUE              ~ "Alerta"))
}

pendientes <- tibble(id = c(tiendas$tienda_id, 11, 1),
                     mes = c(rep("2023-06", 10), "2023-06", "2023-01"))
resultados <- list()
registro <- list()

for (fila in seq_len(nrow(pendientes))) {
  id  <- pendientes$id[fila]
  mes <- pendientes$mes[fila]
  estado <- tryCatch({
    resultados[[length(resultados) + 1]] <- revisar_tienda(id, mes)
    "OK"
  }, error = function(e) paste("ERROR:", conditionMessage(e)))
  registro[[fila]] <- tibble(id = id, mes = mes, estado = estado)
}

registro <- bind_rows(registro)
registro %>% count(estado)
registro %>% filter(estado != "OK")

resultados <- bind_rows(resultados)
resultados %>%
  mutate(variacion = percent(variacion, accuracy = 0.1)) %>%
  select(tienda, variacion, estado)

# Texto Markdown para el reporte (iría en un bloque results = "asis")
alertas <- resultados %>% filter(estado == "Alerta")
cat(paste0("- **", alertas$tienda, "**: las ventas cayeron ",
           percent(abs(alertas$variacion), accuracy = 0.1),
           " respecto a mayo."), sep = "\n")
