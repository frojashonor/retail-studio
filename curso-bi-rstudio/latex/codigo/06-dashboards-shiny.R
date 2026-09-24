# ==========================================================================
# Dashboards interactivos con Shiny
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/06-dashboards-shiny.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Solo referencia (no se ejecuta automáticamente) ----
# install.packages(c("shiny", "shinydashboard", "DT", "plotly", "writexl"))

# ---- Bloque 1 --------------------------------------------------------
library(shiny)            # apps web con R
library(shinydashboard)   # plantillas de dashboard
library(DT)               # tablas interactivas
library(plotly)           # gráficas interactivas
library(writexl)          # exportar a Excel
library(tidyverse)        # dplyr, ggplot2, readr...
library(lubridate)        # fechas
library(scales)           # formatos de moneda y porcentaje
options(width = 70)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # 1. Paquete -------------------------------------------------------------
# library(shiny)          # el motor de las apps web con R
#
# # 2. Interfaz (ui): lo que el usuario VE en el navegador -----------------
# ui <- fluidPage(                       # página que se adapta al ancho
#   titlePanel("Calculadora de precio con descuento"),   # título
#
#   # Entrada 1: una caja para escribir un número (el precio de lista)
#   numericInput(inputId = "precio", label = "Precio de lista ($):",
#                value = 500, min = 0, step = 10),
#
#   # Entrada 2: un deslizador para elegir el descuento
#   sliderInput(inputId = "descuento", label = "Descuento (%):",
#               min = 0, max = 50, value = 10, step = 5),
#
#   # Salida: un hueco donde el servidor escribirá el resultado
#   h3(textOutput(outputId = "precio_final"))
# )
#
# # 3. Servidor (server): la lógica que CORRE en R -------------------------
# server <- function(input, output, session) {
#   # renderText() se vuelve a ejecutar SOLO cuando cambian las entradas
#   # que usa (input$precio o input$descuento): eso es la reactividad
#   output$precio_final <- renderText({
#     final <- input$precio * (1 - input$descuento / 100)
#     paste0("Precio final: ", scales::dollar(final))
#   })
# }
#
# # 4. Unir interfaz y servidor: esto crea (y lanza) la app ----------------
# shinyApp(ui = ui, server = server)

# ---- Bloque 2 --------------------------------------------------------
# Una entrada numérica, igual a la de la app, impresa en la consola
numericInput(inputId = "precio", label = "Precio de lista ($):",
             value = 500, min = 0, step = 10)

# ---- Bloque 3 --------------------------------------------------------
textOutput(outputId = "precio_final")
plotOutput(outputId = "grafica", height = "300px")

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # Directorio de trabajo: la carpeta del curso (la que contiene datasets/)
# getwd()
# shiny::runApp("latex/codigo/apps/01-primera-app")
# shiny::runApp("latex/codigo/apps/02-explorador-ventas")
# shiny::runApp("latex/codigo/apps/03-dashboard-ejecutivo")

# ---- Bloque 4 --------------------------------------------------------
# Busca datasets/transacciones.csv subiendo carpetas desde 'desde'
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

# ---- Bloque 5 --------------------------------------------------------
buscar_datasets()                    # desde la carpeta del curso
carpeta_app <- "latex/codigo/apps/02-explorador-ventas"
buscar_datasets(desde = carpeta_app) # como la vería la app
file.exists(file.path(buscar_datasets(carpeta_app), "tiendas.csv"))

# ---- Bloque 6 --------------------------------------------------------
# Datos para los ejemplos de esta sección
transacciones <- read_csv("datasets/transacciones.csv",
                          show_col_types = FALSE)
tiendas <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)

# Las opciones salen de los datos, ordenadas
opciones_tienda <- sort(tiendas$nombre_tienda)
opciones_pago   <- sort(unique(transacciones$metodo_pago))
opciones_pago

# ---- Bloque 7 --------------------------------------------------------
ui_catalogo <- fluidPage(
  selectInput("tienda", "Tienda:", choices = opciones_tienda,
              selected = "Sucursal Centro"),
  selectizeInput("pagos", "Métodos de pago:", choices = opciones_pago,
                 multiple = TRUE, options = list(placeholder = "Todos")),
  dateRangeInput("fechas", "Periodo:", start = "2023-01-01",
                 end = "2023-12-31", language = "es", separator = " a ",
                 format = "dd/mm/yyyy"),
  sliderInput("top", "Top N:", min = 5, max = 20, value = 10, step = 5),
  numericInput("meta", "Meta mensual ($):", value = 80000, step = 5000),
  checkboxGroupInput("trimestres", "Trimestres:",
                     choices = c("Q1", "Q2", "Q3", "Q4"),
                     selected = c("Q1", "Q2", "Q3", "Q4"), inline = TRUE),
  radioButtons("metrica", "Medir por:",
               choices = c("Ventas ($)" = "ventas",
                           "Unidades" = "unidades")),
  actionButton("calcular", "Calcular", icon = icon("play"))
)

# ---- Bloque 8 --------------------------------------------------------
radioButtons("metrica", "Medir por:",
             choices = c("Ventas ($)" = "ventas", "Unidades" = "unidades"))

# ---- Bloque 9 --------------------------------------------------------
server_catalogo <- function(input, output, session) {
  output$resumen <- renderText({
    datos <- transacciones %>%
      left_join(tiendas, by = "tienda_id") %>%
      filter(nombre_tienda == input$tienda)
    if (length(input$pagos) > 0) {          # vacío = todos los métodos
      datos <- filter(datos, metodo_pago %in% input$pagos)
    }
    paste0(input$tienda, ": ", nrow(datos), " transacciones, ",
           dollar(sum(datos$total_transaccion), accuracy = 1))
  })
}

testServer(server_catalogo, {
  session$setInputs(tienda = "Sucursal Centro", pagos = NULL)
  print(output$resumen)                      # todos los métodos
  session$setInputs(pagos = c("Efectivo", "Transferencia"))
  print(output$resumen)                      # solo dos métodos
})

# ---- Solo referencia (no se ejecuta automáticamente) ----
# shinyApp(ui = fluidPage(ui_catalogo, h3(textOutput("resumen"))),
#          server = server_catalogo)

# ---- Bloque 10 --------------------------------------------------------
server_kpis <- function(input, output, session) {
  ventas_tienda <- reactive({             # el conductor
    message("  (filtrando la tienda ", input$tienda, ")")  # rastro
    filter(transacciones, tienda_id == input$tienda)
  })

  output$n_ventas <- renderText(nrow(ventas_tienda()))
  output$total    <- renderText(
    dollar(sum(ventas_tienda()$total_transaccion), accuracy = 1))
}

testServer(server_kpis, {
  session$setInputs(tienda = 3)
  cat("Tienda 3:", output$n_ventas, "ventas,", output$total, "\n")
  session$setInputs(tienda = 8)
  cat("Tienda 8:", output$n_ventas, "ventas,", output$total, "\n")
})

# ---- Bloque 11 --------------------------------------------------------
server_limpiar <- function(input, output, session) {
  clics <- reactiveVal(0)                     # valor reactivo propio

  observeEvent(input$limpiar, {               # solo al pulsar el botón
    clics(clics() + 1)                        # suma uno
    updateSelectInput(session, "tienda", selected = "Todas")
    message("Filtros reiniciados (clic número ", clics(), ")")
  })

  output$uso <- renderText(paste("Clics en 'Quitar filtros':", clics()))
}

testServer(server_limpiar, {
  session$setInputs(limpiar = 1)              # primer clic
  session$setInputs(limpiar = 2)              # segundo clic
  print(output$uso)
})

# ---- Bloque 12 --------------------------------------------------------
server_meta <- function(input, output, session) {
  # Se recalcula SOLO cuando se pulsa input$calcular
  proyeccion <- eventReactive(input$calcular, {
    ventas_mes <- sum(transacciones$total_transaccion) / 12
    ventas_mes * (1 + input$crecimiento / 100)
  })
  output$proyeccion <- renderText(
    paste("Venta mensual proyectada:", dollar(proyeccion(), accuracy = 1)))
}

testServer(server_meta, {
  session$setInputs(crecimiento = 10)     # el usuario mueve el control
  resultado <- tryCatch(output$proyeccion,
                        error = function(e) "(vacío: aún no se pulsa)")
  print(resultado)
  session$setInputs(calcular = 1)         # ahora sí pulsa el botón
  print(output$proyeccion)
  session$setInputs(crecimiento = 20)     # cambia, pero no pulsa
  print(output$proyeccion)                # sigue el valor anterior
})

# ---- Bloque 13 --------------------------------------------------------
server_ticket <- function(input, output, session) {
  output$ticket <- renderText({
    req(input$metodo)                           # sin método: en blanco
    datos <- filter(transacciones, metodo_pago == input$metodo,
                    total_transaccion >= input$minimo)
    validate(need(nrow(datos) > 0,
                  "No hay transacciones con ese monto mínimo."))
    paste("Ticket promedio:", dollar(mean(datos$total_transaccion)))
  })
}

testServer(server_ticket, {
  session$setInputs(metodo = "Transferencia", minimo = 0)
  print(output$ticket)
  session$setInputs(minimo = 5000)              # nadie gasta tanto
  tryCatch(output$ticket, error = function(e) print(conditionMessage(e)))
})

# ---- Bloque 14 --------------------------------------------------------
precio <- reactiveVal(500)                   # un valor reactivo
precio_con_iva <- reactive(precio() * 1.16)  # depende de precio
isolate(precio_con_iva())
precio(1000)                                 # cambiamos la fuente
isolate(precio_con_iva())                    # se recalcula solo

# ---- Bloque 15 --------------------------------------------------------
tryCatch(precio_con_iva(),
         error = function(e) cat(conditionMessage(e), "\n"))

# ---- Bloque 16 --------------------------------------------------------
valores <- reactiveValues(n = 5)     # input funciona igual que esto
tryCatch(valores$n * 2,
         error = function(e) cat(conditionMessage(e), "\n"))

# ---- Bloque 17 --------------------------------------------------------
fluidRow(
  column(4, "Aquí van los filtros"),
  column(8, "Aquí va la gráfica")
)

# ---- Bloque 18 --------------------------------------------------------
ui_esqueleto <- navbarPage(
  "Ventas 2023",
  tabPanel("Explorar",
    sidebarLayout(
      sidebarPanel(width = 3,
                   selectInput("tienda", "Tienda:", c("Todas", "Centro"))),
      mainPanel(width = 9,
        tabsetPanel(
          tabPanel("Gráfica", plotOutput("grafica")),
          tabPanel("Tabla", tableOutput("tabla"))
        )
      )
    )
  ),
  tabPanel("Acerca de", p("Datos simulados del curso."))
)
class(ui_esqueleto)

# ---- Bloque 19 --------------------------------------------------------
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

opciones_tienda[1:4]
ventas %>%
  select(fecha, mes, nombre_tienda, categoria, total_transaccion) %>%
  head(3)

# ---- Bloque 20 --------------------------------------------------------
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

# ---- Bloque 21 --------------------------------------------------------
norte <- filtrar_ventas(ventas, tienda = "Sucursal Norte",
                        categorias = c("Electrónica", "Hogar"))
nrow(norte)
resumir_por_mes(norte, metrica = "unidades") %>% head(4)

# ---- Bloque 22 --------------------------------------------------------
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

# ---- Bloque 23 --------------------------------------------------------
p_mensual <- resumir_por_mes(ventas, "ventas") %>%
  grafica_mensual("ventas")
p_mensual

# ---- Bloque 24 --------------------------------------------------------
top_productos(ventas, n = 5)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# ui <- fluidPage(
#   titlePanel("Explorador de ventas 2023"),
#   sidebarLayout(
#     # Barra lateral: todos los filtros juntos
#     sidebarPanel(
#       width = 3,
#       selectInput("tienda", "Tienda:", choices = opciones_tienda),
#       selectizeInput("categorias", "Categorías:",
#                      choices = opciones_categoria, multiple = TRUE,
#                      options = list(placeholder = "Todas")),
#       dateRangeInput("fechas", "Periodo:",
#                      start = min(ventas$fecha), end = max(ventas$fecha),
#                      min = min(ventas$fecha), max = max(ventas$fecha),
#                      format = "dd/mm/yyyy", language = "es",
#                      separator = " a "),
#       radioButtons("metrica", "Medir por:",
#                    choices = c("Ventas ($)" = "ventas",
#                                "Unidades" = "unidades",
#                                "Transacciones" = "transacciones")),
#       sliderInput("n_top", "Productos en el top:",
#                   min = 5, max = 20, value = 10, step = 5)
#     ),
#     # Panel principal: resumen arriba y resultados en pestañas
#     mainPanel(
#       width = 9,
#       h4(textOutput("resumen")),
#       tabsetPanel(
#         tabPanel("Evolución mensual",
#                  plotOutput("grafica", height = "380px")),
#         tabPanel("Top productos", tableOutput("tabla_top"))
#       )
#     )
#   )
# )

# ---- Solo referencia (no se ejecuta automáticamente) ----
# server <- function(input, output, session) {
#
#   # Expresión reactiva: filtra UNA vez y la reutilizan las tres salidas
#   datos_filtrados <- reactive({
#     req(input$fechas)                      # espera a que haya fechas
#     filtrar_ventas(ventas, input$tienda, input$categorias, input$fechas)
#   })
#
#   output$resumen <- renderText({
#     d <- datos_filtrados()                 # ¡con paréntesis!
#     paste0(comma(nrow(d)), " transacciones  |  Ventas: ",
#            dollar(sum(d$total_transaccion), accuracy = 1))
#   })
#
#   output$grafica <- renderPlot({
#     validate(need(nrow(datos_filtrados()) > 0,
#                   "No hay ventas con estos filtros. Amplía el periodo."))
#     resumir_por_mes(datos_filtrados(), input$metrica) %>%
#       grafica_mensual(input$metrica)
#   }, res = 96)
#
#   output$tabla_top <- renderTable({
#     top_productos(datos_filtrados(), n = input$n_top)
#   }, align = "llrr", digits = 0)
# }
#
# # 7. Lanzar la app -------------------------------------------------------
# shinyApp(ui, server)

# ---- Bloque 25 --------------------------------------------------------
app02 <- shinyAppDir("latex/codigo/apps/02-explorador-ventas")
testServer(app02, {
  session$setInputs(tienda = "Todas", categorias = NULL,
                    fechas = as.Date(c("2023-01-01", "2023-12-31")),
                    metrica = "ventas", n_top = 10)
  print(output$resumen)
  session$setInputs(tienda = "Sucursal Centro",
                    fechas = as.Date(c("2023-07-01", "2023-12-31")))
  print(output$resumen)
  print(range(datos_filtrados()$fecha))       # un reactivo interno
})

# ---- Bloque 26 --------------------------------------------------------
valueBox(value = "$959,993", subtitle = "Ventas totales",
         icon = icon("dollar-sign"), color = "blue", width = 3)

# ---- Bloque 27 --------------------------------------------------------
# ---- Colores y tema corporativos ---------------------------------------
color_principal <- "#2a78d6"   # una sola serie
color_resalte   <- "#eb6834"   # para destacar un elemento
color_gris      <- "#b5b3ad"   # el resto, cuando se resalta uno
paleta_libro <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                  "#e87ba4", "#008300", "#4a3aa7", "#e34948")

tema_dashboard <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(),
          legend.position  = "top",
          plot.margin      = margin(5, 15, 5, 5))  # aire a la derecha
}

cargar_datos_dashboard <- function(carpeta = buscar_datasets()) {
  leer <- function(archivo) {
    read_csv(file.path(carpeta, archivo), show_col_types = FALSE)
  }
  productos <- leer("productos.csv") %>%
    select(producto_id, nombre_producto, categoria, costo)
  tiendas <- leer("tiendas.csv") %>%
    transmute(tienda_id, ciudad,
              tienda = sub("Sucursal ", "", nombre_tienda))  # nombre corto
  clientes <- leer("clientes.csv") %>%
    select(cliente_id, nombre_cliente = nombre, segmento, es_premium)

  leer("transacciones.csv") %>%
    left_join(productos, by = "producto_id") %>%
    left_join(tiendas,   by = "tienda_id") %>%
    left_join(clientes,  by = "cliente_id") %>%
    mutate(mes = floor_date(fecha, "month"),
           segmento = factor(segmento, levels = c("Joven", "Adulto",
                                                  "Maduro", "Senior"))) %>%
    select(transaccion_id, fecha, mes, tienda, ciudad, categoria,
           producto_id, nombre_producto, cliente_id, nombre_cliente,
           segmento, es_premium, metodo_pago, cantidad, precio_venta,
           costo, total_transaccion)
}

datos <- cargar_datos_dashboard()
glimpse(datos)

# ---- Bloque 28 --------------------------------------------------------
# ---- Filtros globales --------------------------------------------------
# Vacío (NULL o character(0)) significa "todas".
filtrar_datos <- function(datos, fechas, tiendas = NULL,
                          categorias = NULL) {
  datos <- filter(datos, fecha >= fechas[1], fecha <= fechas[2])
  if (length(tiendas) > 0) {
    datos <- filter(datos, tienda %in% tiendas)
  }
  if (length(categorias) > 0) {
    datos <- filter(datos, categoria %in% categorias)
  }
  datos
}

calcular_kpis <- function(datos) {
  datos %>%
    summarise(
      ventas          = sum(total_transaccion),
      transacciones   = n(),
      ticket_promedio = ventas / transacciones,
      unidades        = sum(cantidad),
      clientes        = n_distinct(cliente_id),
      pct_premium     = mean(es_premium[!duplicated(cliente_id)]),
      compras_cliente = transacciones / clientes
    )
}

# Formatos para mostrar los KPIs como texto ("--" si no hay dato)
formato_pesos <- function(x) {
  if_else(is.na(x), "--", dollar(x, accuracy = 1))
}
formato_numero <- function(x, decimales = 0) {
  if_else(is.na(x), "--", comma(x, accuracy = 10^-decimales))
}
formato_pct <- function(x) {
  if_else(is.na(x), "--", percent(x, accuracy = 0.1))
}

# ---- Bloque 29 --------------------------------------------------------
anio <- as.Date(c("2023-01-01", "2023-12-31"))
kpis_total <- calcular_kpis(filtrar_datos(datos, anio))
kpis_total %>% select(ventas, transacciones, ticket_promedio, clientes)
formato_pesos(kpis_total$ventas)
formato_pct(kpis_total$pct_premium)

# Los mismos KPIs para dos tiendas y una categoría, primer semestre
filtrar_datos(datos, as.Date(c("2023-01-01", "2023-06-30")),
              tiendas = c("Centro", "Norte"),
              categorias = "Electrónica") %>%
  calcular_kpis() %>%
  select(ventas, transacciones, ticket_promedio, clientes)

# ---- Bloque 30 --------------------------------------------------------
grafica_evolucion <- function(datos) {
  por_mes <- datos %>%
    group_by(mes) %>%
    summarise(ventas = sum(total_transaccion), .groups = "drop")
  mejor_mes <- slice_max(por_mes, ventas, n = 1, with_ties = FALSE)
  ggplot(por_mes, aes(x = mes, y = ventas)) +
    geom_line(color = color_principal, linewidth = 0.8) +
    geom_point(color = color_principal, size = 2) +
    geom_point(data = mejor_mes, color = color_resalte, size = 3.5) +
    geom_text(data = mejor_mes, aes(label = dollar(ventas, accuracy = 1)),
              vjust = -1.2, size = 3.5, color = "gray25") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(labels = label_dollar(), limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.15))) +
    labs(x = NULL, y = "Ventas") +
    tema_dashboard()
}

p_evolucion <- grafica_evolucion(datos)
p_evolucion

# ---- Bloque 31 --------------------------------------------------------
grafica_barras <- function(resumen, etiqueta_y = NULL) {
  # resumen: columnas 'nombre' y 'ventas'; barras horizontales ordenadas
  ggplot(resumen, aes(x = ventas, y = reorder(nombre, ventas))) +
    geom_col(fill = color_principal, width = 0.7) +
    scale_x_continuous(labels = label_dollar(),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Ventas", y = etiqueta_y) +
    tema_dashboard()
}

grafica_top_productos <- function(datos, n = 10) {
  datos %>%
    group_by(nombre = nombre_producto) %>%
    summarise(ventas = sum(total_transaccion), .groups = "drop") %>%
    slice_max(ventas, n = n, with_ties = FALSE) %>%
    grafica_barras()
}

p_top <- grafica_top_productos(datos)
p_top

# ---- Bloque 32 --------------------------------------------------------
grafica_tiendas <- function(datos) {
  por_tienda <- datos %>%
    group_by(tienda) %>%
    summarise(ventas = sum(total_transaccion), .groups = "drop") %>%
    mutate(es_mejor = ventas == max(ventas))    # la tienda líder
  ggplot(por_tienda, aes(x = ventas, y = reorder(tienda, ventas),
                         fill = es_mejor)) +
    geom_col(width = 0.7) +
    geom_text(data = filter(por_tienda, es_mejor),
              aes(label = dollar(ventas, accuracy = 1)),
              hjust = 1.1, color = "white", size = 3.5) +
    scale_fill_manual(values = c(`TRUE` = color_resalte,
                                 `FALSE` = color_gris), guide = "none") +
    scale_x_continuous(labels = label_dollar(),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Ventas", y = NULL) +
    tema_dashboard()
}

p_tiendas <- grafica_tiendas(datos)
p_tiendas

# ---- Bloque 33 --------------------------------------------------------
grafica_segmentos <- function(datos) {
  datos %>%
    mutate(tipo = if_else(es_premium, "Premium", "Regular")) %>%
    group_by(segmento, tipo) %>%
    summarise(ventas = sum(total_transaccion), .groups = "drop") %>%
    ggplot(aes(x = segmento, y = ventas, fill = tipo)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = c(Premium = paleta_libro[2],
                                 Regular = paleta_libro[1])) +
    scale_y_continuous(labels = label_dollar(),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Segmento de edad", y = "Ventas", fill = "Tipo de cliente") +
    tema_dashboard()
}

p_segmentos <- grafica_segmentos(datos)
p_segmentos

# ---- Bloque 34 --------------------------------------------------------
# ---- Tablas ------------------------------------------------------------
tabla_tiendas <- function(datos) {
  datos %>%
    group_by(Tienda = tienda) %>%
    summarise(Ventas = sum(total_transaccion),
              Transacciones = n(),
              Ticket = Ventas / Transacciones, .groups = "drop") %>%
    arrange(desc(Ventas)) %>%
    mutate(Ventas = formato_pesos(Ventas), Ticket = formato_pesos(Ticket))
}

tabla_top_clientes <- function(datos, n = 10) {
  datos %>%
    group_by(Cliente = nombre_cliente, Segmento = segmento) %>%
    summarise(Compras = n(), Ventas = sum(total_transaccion),
              .groups = "drop") %>%
    slice_max(Ventas, n = n, with_ties = FALSE) %>%
    mutate(Ventas = formato_pesos(Ventas))
}

# Datos listos para la tabla interactiva y para las descargas
datos_detalle <- function(datos) {
  datos %>%
    arrange(fecha, transaccion_id) %>%
    select(Fecha = fecha, Tienda = tienda, `Categoría` = categoria,
           Producto = nombre_producto, Cliente = nombre_cliente,
           Cantidad = cantidad, Precio = precio_venta,
           Total = total_transaccion, `Método de pago` = metodo_pago)
}

tabla_tiendas(datos) %>% head(4)
tabla_top_clientes(datos, n = 3)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # 1. Paquetes ------------------------------------------------------------
# library(shiny)
# library(shinydashboard)   # dashboardPage(), box(), valueBox()...
# library(dplyr)
# library(readr)
# library(ggplot2)
# library(lubridate)
# library(scales)
# library(DT)               # tablas interactivas
# library(writexl)          # descarga en Excel
#
# # 2. Datos: se leen UNA vez, fuera del server ----------------------------
# datos <- cargar_datos_dashboard()
# fecha_min <- min(datos$fecha)
# fecha_max <- max(datos$fecha)
# opciones_tiendas    <- sort(unique(datos$tienda))
# opciones_categorias <- sort(unique(datos$categoria))
#
# # 3. Interfaz: encabezado y barra lateral --------------------------------
# encabezado <- dashboardHeader(title = "Ventas 2023")
#
# barra_lateral <- dashboardSidebar(
#   sidebarMenu(
#     id = "menu",
#     menuItem("Resumen",   tabName = "resumen",   icon = icon("gauge")),
#     menuItem("Productos", tabName = "productos", icon = icon("box")),
#     menuItem("Tiendas",   tabName = "tiendas",   icon = icon("store")),
#     menuItem("Clientes",  tabName = "clientes",  icon = icon("users")),
#     menuItem("Datos",     tabName = "datos",     icon = icon("table"))
#   ),
#   # Filtros globales: afectan a TODAS las pestañas
#   dateRangeInput("fechas", "Periodo:", start = fecha_min, end = fecha_max,
#                  min = fecha_min, max = fecha_max, format = "dd/mm/yyyy",
#                  language = "es", separator = " a "),
#   selectizeInput("tiendas", "Tiendas:", choices = opciones_tiendas,
#                  multiple = TRUE, options = list(placeholder = "Todas")),
#   selectizeInput("categorias", "Categorías:",
#                  choices = opciones_categorias, multiple = TRUE,
#                  options = list(placeholder = "Todas")),
#   actionButton("limpiar", "Quitar filtros", icon = icon("rotate-left"))
# )

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # 4. Interfaz: cuerpo con una tabItem por pestaña del menú ---------------
# cuerpo <- dashboardBody(
#   tabItems(
#     tabItem(tabName = "resumen",
#       fluidRow(                                   # fila de KPIs arriba
#         valueBoxOutput("kpi_ventas", width = 3),
#         valueBoxOutput("kpi_transacciones", width = 3),
#         valueBoxOutput("kpi_ticket", width = 3),
#         valueBoxOutput("kpi_clientes", width = 3)
#       ),
#       fluidRow(
#         box(title = "Ventas por mes", width = 8, status = "primary",
#             solidHeader = TRUE, plotOutput("graf_evolucion", height = 300)),
#         box(title = "Ventas por método de pago", width = 4,
#             status = "primary", solidHeader = TRUE,
#             plotOutput("graf_pago", height = 300))
#       )
#     ),
#     # ... pestañas "productos", "tiendas" y "clientes": misma idea,
#     # ... una fluidRow() con dos box() cada una (ver app.R)
#     tabItem(tabName = "datos",
#       fluidRow(
#         box(title = "Detalle de transacciones", width = 12,
#             status = "primary",
#             downloadButton("descargar_csv", "Descargar CSV"),
#             downloadButton("descargar_excel", "Descargar Excel"),
#             br(), br(),
#             DTOutput("tabla_detalle"))
#       )
#     )
#   )
# )
#
# ui <- dashboardPage(encabezado, barra_lateral, cuerpo, skin = "blue")

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # 5. Servidor: datos filtrados, KPIs y botón para limpiar filtros --------
# server <- function(input, output, session) {
#
#   # La pieza central: TODO lo demás depende de datos_filtrados()
#   datos_filtrados <- reactive({
#     req(input$fechas)
#     filtrar_datos(datos, input$fechas, input$tiendas, input$categorias)
#   }) %>%
#     bindCache(input$fechas, input$tiendas, input$categorias)
#
#   kpis <- reactive(calcular_kpis(datos_filtrados()))
#
#   # Igual que datos_filtrados(), pero si los filtros no dejan ninguna
#   # venta, las salidas que lo usan muestran un mensaje amable
#   datos_validos <- reactive({
#     validate(need(nrow(datos_filtrados()) > 0,
#                   "No hay ventas con estos filtros."))
#     datos_filtrados()
#   })
#
#   observeEvent(input$limpiar, {
#     updateDateRangeInput(session, "fechas", start = fecha_min,
#                          end = fecha_max)
#     updateSelectizeInput(session, "tiendas", selected = character(0))
#     updateSelectizeInput(session, "categorias", selected = character(0))
#   })
#
#   output$kpi_ventas <- renderValueBox({
#     valueBox(formato_pesos(kpis()$ventas), "Ventas totales",
#              icon = icon("dollar-sign"), color = "blue")
#   })
#   output$kpi_transacciones <- renderValueBox({
#     valueBox(formato_numero(kpis()$transacciones), "Transacciones",
#              icon = icon("receipt"), color = "blue")
#   })
#   output$kpi_ticket <- renderValueBox({
#     valueBox(formato_pesos(kpis()$ticket_promedio), "Ticket promedio",
#              icon = icon("cart-shopping"), color = "blue")
#   })
#   output$kpi_clientes <- renderValueBox({
#              icon = icon("users"), color = "blue")
#   })

# ---- Solo referencia (no se ejecuta automáticamente) ----
#   # 6. Servidor: gráficas y tablas de cada pestaña -----------------------
#   output$graf_evolucion <- renderPlot({
#     grafica_evolucion(datos_validos())
#   }, res = 96)
#   output$graf_pago <- renderPlot({
#     grafica_metodo_pago(datos_validos())
#   }, res = 96)
#   # ... las demás gráficas siguen el mismo patrón de una línea ...
#   output$tabla_tiendas <- renderTable(tabla_tiendas(datos_filtrados()))
#   output$info_premium <- renderInfoBox({
#     infoBox("Clientes premium", formato_pct(kpis()$pct_premium),
#             icon = icon("star"), color = "blue")
#   })
#   # ... infoBox de clientes y frecuencia, gráfica de segmentos ...
#   output$tabla_detalle <- renderDT(tabla_detalle_dt(datos_filtrados()))
#
#   # 7. Servidor: descargas (respetan los filtros activos) ----------------
#   output$descargar_csv <- downloadHandler(
#     filename = function() paste0("ventas_filtradas_", Sys.Date(), ".csv"),
#     content  = function(file) {
#       write_excel_csv(datos_detalle(datos_filtrados()), file)
#     }
#   )
#   output$descargar_excel <- downloadHandler(
#     filename = function() paste0("ventas_filtradas_", Sys.Date(), ".xlsx"),
#     content  = function(file) {
#       write_xlsx(datos_detalle(datos_filtrados()), file)
#     }
#   )
# }
#
# # 8. Lanzar la app -------------------------------------------------------
# shinyApp(ui, server)

# ---- Bloque 35 --------------------------------------------------------
app03 <- shinyAppDir("latex/codigo/apps/03-dashboard-ejecutivo")
testServer(app03, {
  session$setInputs(fechas = as.Date(c("2023-01-01", "2023-12-31")),
                    tiendas = NULL, categorias = NULL)
  cat("Ventas:", kpis()$ventas, "| Clientes:", kpis()$clientes, "\n")

  # El usuario elige dos tiendas y una categoría
  session$setInputs(tiendas = c("Centro", "Norte"),
                    categorias = "Electrónica")
  cat("Ventas:", round(kpis()$ventas, 2),
      "| Transacciones:", kpis()$transacciones, "\n")

  # Filtros sin ventas: las gráficas muestran el mensaje de validate()
  session$setInputs(tiendas = "Express", categorias = "Ropa",
                    fechas = as.Date(c("2023-01-01", "2023-01-31")))
  tryCatch(output$graf_evolucion,
           error = function(e) cat("Gráfica:", conditionMessage(e), "\n"))
  cat("Ticket en el valueBox:", formato_pesos(kpis()$ticket_promedio), "\n")
})

# ---- Bloque 36 --------------------------------------------------------
dir.create("resultados", showWarnings = FALSE)
detalle <- datos_detalle(filtrar_datos(datos, anio, tiendas = "Outlet"))
write_excel_csv(detalle, "resultados/ventas_outlet.csv")
write_xlsx(detalle, "resultados/ventas_outlet.xlsx")
dim(detalle)
names(detalle)

# ---- Bloque 37 --------------------------------------------------------
# Textos de la tabla DT en español (sin depender de internet)
idioma_dt <- list(
  search = "Buscar:", lengthMenu = "Mostrar _MENU_ filas",
  info = "Filas _START_ a _END_ de _TOTAL_",
  infoEmpty = "Sin resultados", zeroRecords = "No hay coincidencias",
  paginate = list(previous = "Anterior", `next` = "Siguiente")
)

tabla_detalle_dt <- function(datos) {
  datatable(datos_detalle(datos), rownames = FALSE,
            options = list(pageLength = 10, language = idioma_dt)) %>%
    formatCurrency(c("Precio", "Total"), currency = "$", digits = 2)
}

tabla_dt <- tabla_detalle_dt(datos)
class(tabla_dt)

# ---- Bloque 38 --------------------------------------------------------
graf_interactiva <- ggplotly(p_tiendas)
class(graf_interactiva)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # En la ui, en lugar de plotOutput("graf_tiendas"):
# plotly::plotlyOutput("graf_tiendas", height = 380)
#
# # En el server, en lugar de renderPlot():
# output$graf_tiendas <- plotly::renderPlotly({
#   ggplotly(grafica_tiendas(datos_validos()))
# })

# ---- Solo referencia (no se ejecuta automáticamente) ----
# datos_filtrados <- reactive({
#   req(input$fechas)
#   filtrar_datos(datos, input$fechas, input$tiendas, input$categorias)
# }) %>%
#   bindCache(input$fechas, input$tiendas, input$categorias)

# ---- Bloque 39 --------------------------------------------------------
# Parte visual del módulo: todos los ids pasan por ns()
tarjeta_kpi_ui <- function(id, ancho = 3) {
  ns <- NS(id)
  valueBoxOutput(ns("caja"), width = ancho)
}

# Parte lógica: recibe un reactivo 'valor' desde fuera
tarjeta_kpi_server <- function(id, valor, titulo, icono,
                               formato = formato_pesos) {
  moduleServer(id, function(input, output, session) {
    output$caja <- renderValueBox({
      valueBox(formato(valor()), titulo, icon = icon(icono),
               color = "blue")
    })
  })
}

tarjeta_kpi_ui("ventas")          # el id interno se vuelve "ventas-caja"

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # ui
# fluidRow(
#   tarjeta_kpi_ui("ventas"),
#   tarjeta_kpi_ui("ticket"),
#   tarjeta_kpi_ui("clientes")
# )
# # server
# tarjeta_kpi_server("ventas", reactive(kpis()$ventas),
#                    "Ventas totales", "dollar-sign")
# tarjeta_kpi_server("ticket", reactive(kpis()$ticket_promedio),
#                    "Ticket promedio", "cart-shopping")
# tarjeta_kpi_server("clientes", reactive(kpis()$clientes),
#                    "Clientes activos", "users", formato = formato_numero)

# ---- Bloque 40 --------------------------------------------------------
testServer(tarjeta_kpi_server,
           args = list(valor = reactive(kpis_total$clientes),
                       titulo = "Clientes activos", icono = "users",
                       formato = formato_numero), {
  cat(as.character(output$caja$html))
})

# ---- Solo referencia (no se ejecuta automáticamente) ----
# install.packages("rsconnect")
# rsconnect::setAccountInfo(name   = "tu_usuario",
#                           token  = "TOKEN_DE_EJEMPLO",
#                           secret = "SECRETO_DE_EJEMPLO")

# ---- Solo referencia (no se ejecuta automáticamente) ----
# rsconnect::deployApp("latex/codigo/apps/03-dashboard-ejecutivo",
#                      appName = "dashboard-ventas")

# ---- Bloque 41 --------------------------------------------------------
server_error <- function(input, output, session) {
  datos <- reactive(filter(transacciones, tienda_id == input$tienda))
  output$total <- renderText(sum(datos$total_transaccion))  # ¡error!
}
testServer(server_error, {
  session$setInputs(tienda = 1)
  tryCatch(output$total,
           error = function(e) cat("Error:", conditionMessage(e), "\n"))
})

# ---- Bloque 42 --------------------------------------------------------
codigo_ui <- 'fluidPage(
  titlePanel("Ventas")
  sliderInput("n", "Top N:", 5, 20, 10)
)'
tryCatch(parse(text = codigo_ui),
         error = function(e) cat(conditionMessage(e), "\n"))
