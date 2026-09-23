# ========================================================================
# App 02 - Explorador de ventas (fluidPage + sidebarLayout)
# Libro "Business Intelligence con R y RStudio", Módulo 6
#
# Cómo ejecutarla (con el directorio de trabajo en la carpeta del curso):
#   shiny::runApp("latex/codigo/apps/02-explorador-ventas")
# Requisito: crear antes los datos con
#   source("datasets/generar_datasets.R")
# ========================================================================

# 1. Paquetes ------------------------------------------------------------
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)   # floor_date() para agrupar por mes
library(scales)      # dollar(), comma() para dar formato

# 2. Encontrar la carpeta datasets/ --------------------------------------
# Shiny cambia el directorio de trabajo a la carpeta de la app, así que
# "datasets/..." no existe ahí. Esta función sube carpeta por carpeta
# ("..") hasta encontrar datasets/transacciones.csv.
buscar_datasets <- function(desde = ".", max_niveles = 6) {
  carpeta <- desde
  for (i in 0:max_niveles) {
    candidato <- file.path(carpeta, "datasets", "transacciones.csv")
    if (file.exists(candidato)) {
      return(file.path(carpeta, "datasets"))   # ¡encontrada!
    }
    carpeta <- file.path(carpeta, "..")        # sube un nivel
  }
  stop("No encontré datasets/transacciones.csv. Ejecuta primero ",
       "source('datasets/generar_datasets.R') en la carpeta del curso.",
       call. = FALSE)
}

# 3. Datos: se leen UNA sola vez, al arrancar la app ---------------------
carpeta_datos <- buscar_datasets()
tiendas   <- read_csv(file.path(carpeta_datos, "tiendas.csv"),
                      show_col_types = FALSE)
productos <- read_csv(file.path(carpeta_datos, "productos.csv"),
                      show_col_types = FALSE)
ventas <- read_csv(file.path(carpeta_datos, "transacciones.csv"),
                   show_col_types = FALSE) %>%
  left_join(select(tiendas, tienda_id, nombre_tienda), by = "tienda_id") %>%
  left_join(select(productos, producto_id, nombre_producto, categoria),
            by = "producto_id") %>%
  mutate(mes = floor_date(fecha, "month"))   # primer día de cada mes

# Opciones para los filtros (se calculan una vez a partir de los datos)
opciones_tienda    <- c("Todas", sort(unique(ventas$nombre_tienda)))
opciones_categoria <- sort(unique(ventas$categoria))

# 4. Funciones de apoyo (sin input/output: se prueban en consola) --------
filtrar_ventas <- function(datos, tienda = "Todas", categorias = NULL,
                           fechas = range(datos$fecha)) {
  resultado <- filter(datos, fecha >= fechas[1], fecha <= fechas[2])
  if (tienda != "Todas") {                 # "Todas" = no filtrar tienda
    resultado <- filter(resultado, nombre_tienda == tienda)
  }
  if (length(categorias) > 0) {            # vacío = todas las categorías
    resultado <- filter(resultado, categoria %in% categorias)
  }
  resultado
}

resumir_por_mes <- function(datos, metrica = "ventas") {
  datos %>%
    group_by(mes) %>%
    summarise(ventas        = sum(total_transaccion),
              unidades      = sum(cantidad),
              transacciones = n(),
              .groups = "drop") %>%
    mutate(valor = .data[[metrica]])   # la columna elegida por el usuario
}

grafica_mensual <- function(resumen, metrica = "ventas") {
  etiquetas <- c(ventas = "Ventas", unidades = "Unidades vendidas",
                 transacciones = "Transacciones")
  formato <- if (metrica == "ventas") label_dollar() else label_comma()
  ggplot(resumen, aes(x = mes, y = valor)) +
    geom_col(fill = "#2a78d6", width = 22) +    # ancho en días
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(labels = formato,
                       expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = etiquetas[[metrica]]) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
}

top_productos <- function(datos, n = 10) {
  datos %>%
    group_by(Producto = nombre_producto, `Categoría` = categoria) %>%
    summarise(Unidades = sum(cantidad),
              Ventas   = sum(total_transaccion), .groups = "drop") %>%
    slice_max(Ventas, n = n, with_ties = FALSE) %>%
    mutate(Ventas = dollar(Ventas))       # texto con formato de moneda
}

# 5. Interfaz ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Explorador de ventas 2023"),
  sidebarLayout(
    # Barra lateral: todos los filtros juntos
    sidebarPanel(
      width = 3,
      selectInput("tienda", "Tienda:", choices = opciones_tienda),
      selectizeInput("categorias", "Categorías:",
                     choices = opciones_categoria, multiple = TRUE,
                     options = list(placeholder = "Todas")),
      dateRangeInput("fechas", "Periodo:",
                     start = min(ventas$fecha), end = max(ventas$fecha),
                     min = min(ventas$fecha), max = max(ventas$fecha),
                     format = "dd/mm/yyyy", language = "es",
                     separator = " a "),
      radioButtons("metrica", "Medir por:",
                   choices = c("Ventas ($)" = "ventas",
                               "Unidades" = "unidades",
                               "Transacciones" = "transacciones")),
      sliderInput("n_top", "Productos en el top:",
                  min = 5, max = 20, value = 10, step = 5)
    ),
    # Panel principal: resumen arriba y resultados en pestañas
    mainPanel(
      width = 9,
      h4(textOutput("resumen")),
      tabsetPanel(
        tabPanel("Evolución mensual",
                 plotOutput("grafica", height = "380px")),
        tabPanel("Top productos", tableOutput("tabla_top"))
      )
    )
  )
)

# 6. Servidor ------------------------------------------------------------
server <- function(input, output, session) {

  # Expresión reactiva: filtra UNA vez y la reutilizan las tres salidas
  datos_filtrados <- reactive({
    req(input$fechas)                      # espera a que haya fechas
    filtrar_ventas(ventas, input$tienda, input$categorias, input$fechas)
  })

  output$resumen <- renderText({
    d <- datos_filtrados()                 # ¡con paréntesis!
    paste0(comma(nrow(d)), " transacciones  |  Ventas: ",
           dollar(sum(d$total_transaccion), accuracy = 1))
  })

  output$grafica <- renderPlot({
    validate(need(nrow(datos_filtrados()) > 0,
                  "No hay ventas con estos filtros. Amplía el periodo."))
    resumir_por_mes(datos_filtrados(), input$metrica) %>%
      grafica_mensual(input$metrica)
  }, res = 96)

  output$tabla_top <- renderTable({
    top_productos(datos_filtrados(), n = input$n_top)
  }, align = "llrr", digits = 0)
}

# 7. Lanzar la app -------------------------------------------------------
shinyApp(ui, server)
