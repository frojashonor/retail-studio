# ==========================================================================
# sol-06.tex
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/sol-06.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Bloque 1 --------------------------------------------------------
library(shiny)

ui <- fluidPage(
  titlePanel("Calculadora de precio con descuento"),
  numericInput("precio", "Precio de lista ($):", value = 500,
               min = 0, step = 10),
  numericInput("cantidad", "Cantidad de piezas:", value = 1, min = 1),
  sliderInput("descuento", "Descuento (%):", min = 0, max = 50,
              value = 10, step = 5),
  checkboxInput("iva", "Incluir IVA (16%)", value = FALSE),
  h3(textOutput("total"))
)

server <- function(input, output, session) {
  output$total <- renderText({
    req(input$precio, input$cantidad)       # sin valores, en blanco
    subtotal <- input$precio * input$cantidad * (1 - input$descuento / 100)
    total <- if (input$iva) subtotal * 1.16 else subtotal
    paste0("Total a pagar: ", scales::dollar(total))
  })
}

testServer(server, {
  session$setInputs(precio = 500, cantidad = 3, descuento = 10,
                    iva = FALSE)
  print(output$total)
  session$setInputs(iva = TRUE)
  print(output$total)
})

# ---- Solo referencia (no se ejecuta automáticamente) ----
# shinyApp(ui, server)

# ---- Bloque 2 --------------------------------------------------------
library(tidyverse)
library(scales)
ventas <- read_csv("datasets/transacciones.csv", show_col_types = FALSE)

# (1) filtrar_ventas() con el argumento nuevo 'pagos' (resto igual)
filtrar_pagos <- function(datos, pagos = NULL) {
  if (length(pagos) > 0) {                 # vacío = todos los métodos
    datos <- filter(datos, metodo_pago %in% pagos)
  }
  datos
}
solo_efectivo <- filtrar_pagos(ventas, pagos = "Efectivo")
nrow(solo_efectivo)
dollar(sum(solo_efectivo$total_transaccion), accuracy = 1)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # (1) En filtrar_ventas(): nuevo argumento y nuevo if
# filtrar_ventas <- function(datos, tienda = "Todas", categorias = NULL,
#                            fechas = range(datos$fecha), pagos = NULL) {
#   # ... filtros de fecha, tienda y categoría como antes ...
#   if (length(pagos) > 0) {
#     resultado <- filter(resultado, metodo_pago %in% pagos)
#   }
#   resultado
# }
#
# # (2) En sidebarPanel(), después del selectizeInput de categorías:
# checkboxGroupInput("pagos", "Método de pago:",
#                    choices = sort(unique(ventas$metodo_pago)),
#                    selected = sort(unique(ventas$metodo_pago))),
#
# # (3) En el server:
# datos_filtrados <- reactive({
#   req(input$fechas)
#   filtrar_ventas(ventas, input$tienda, input$categorias, input$fechas,
#                  input$pagos)
# })

# ---- Bloque 3 --------------------------------------------------------
library(tidyverse)
library(scales)
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)
datos <- read_csv("datasets/transacciones.csv", show_col_types = FALSE) %>%
  left_join(select(productos, producto_id, categoria, costo),
            by = "producto_id")

calcular_margen <- function(datos) {
  datos %>%
    summarise(ventas = sum(total_transaccion),
              margen = sum(total_transaccion - costo * cantidad),
              pct_margen = margen / ventas)
}

calcular_margen(datos)
datos %>%
  group_by(categoria) %>%
  calcular_margen() %>%
  arrange(pct_margen) %>%
  head(3)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # ui: nueva fila dentro de tabItem(tabName = "resumen")
# fluidRow(
#   valueBoxOutput("kpi_margen", width = 6),
#   valueBoxOutput("kpi_pct_margen", width = 6)
# )
#
# # server
# margen <- reactive(calcular_margen(datos_filtrados()))
#
# output$kpi_margen <- renderValueBox({
#   m <- margen()$margen
#   valueBox(formato_pesos(m), "Margen bruto",
#            icon = icon("sack-dollar"),
#            color = if (isTRUE(m >= 0)) "green" else "red")
# })
# output$kpi_pct_margen <- renderValueBox({
#   valueBox(formato_pct(margen()$pct_margen), "% de margen",
#            icon = icon("percent"), color = "blue")
# })

# ---- Bloque 4 --------------------------------------------------------
library(shiny)
library(tidyverse)
library(scales)
tiendas <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)
ventas <- read_csv("datasets/transacciones.csv", show_col_types = FALSE) %>%
  left_join(select(tiendas, tienda_id, nombre_tienda), by = "tienda_id")

ui <- fluidPage(
  titlePanel("Ventas con botón Aplicar"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("tiendas", "Tiendas:",
                     choices = sort(unique(ventas$nombre_tienda)),
                     multiple = TRUE, options = list(placeholder = "Todas")),
      dateRangeInput("fechas", "Periodo:", start = "2023-01-01",
                     end = "2023-12-31", language = "es",
                     separator = " a "),
      actionButton("aplicar", "Aplicar", icon = icon("filter"))
    ),
    mainPanel(h3(textOutput("ventas")), h4(textOutput("transacciones")))
  )
)

server <- function(input, output, session) {
  # ignoreNULL = FALSE: calcula también al abrir la app (con el valor 0
  # del botón), para no empezar con la pantalla vacía
  datos <- eventReactive(input$aplicar, {
    d <- filter(ventas, fecha >= input$fechas[1], fecha <= input$fechas[2])
    if (length(input$tiendas) > 0) {
      d <- filter(d, nombre_tienda %in% input$tiendas)
    }
    d
  }, ignoreNULL = FALSE)

  output$ventas <- renderText(
    paste("Ventas:", dollar(sum(datos()$total_transaccion), accuracy = 1)))
  output$transacciones <- renderText(
    paste("Transacciones:", nrow(datos())))
}

testServer(server, {
  session$setInputs(tiendas = NULL, aplicar = 0,
                    fechas = as.Date(c("2023-01-01", "2023-12-31")))
  print(output$ventas)
  session$setInputs(tiendas = "Sucursal Norte")      # sin pulsar
  print(output$ventas)
  session$setInputs(aplicar = 1)                     # pulsa Aplicar
  print(output$ventas)
  print(output$transacciones)
})

# ---- Solo referencia (no se ejecuta automáticamente) ----
# shinyApp(ui, server)

# ---- Bloque 5 --------------------------------------------------------
library(tidyverse)
library(lubridate)
library(scales)
datos <- read_csv("datasets/transacciones.csv", show_col_types = FALSE)

ventas_por_dia <- function(datos) {
  datos %>%
    mutate(dia = wday(fecha, label = TRUE, abbr = FALSE,
                      week_start = 1)) %>%
    group_by(dia) %>%
    summarise(ventas = sum(total_transaccion), .groups = "drop") %>%
    mutate(es_mejor = ventas == max(ventas))
}

grafica_dias <- function(datos) {
  ggplot(ventas_por_dia(datos), aes(x = dia, y = ventas, fill = es_mejor)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = c(`TRUE` = "#eb6834", `FALSE` = "#b5b3ad"),
                      guide = "none") +
    scale_y_continuous(labels = label_dollar(),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = "Ventas") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

ventas_por_dia(datos)
p_dias <- grafica_dias(datos)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # En sidebarMenu(), después de "Clientes":
# menuItem("Días", tabName = "dias", icon = icon("calendar-week")),
#
# # En tabItems(), una pestaña nueva:
# tabItem(tabName = "dias",
#   fluidRow(
#     box(title = "Ventas por día de la semana", width = 12,
#         status = "primary", plotOutput("graf_dias", height = 380))
#   )
# ),
#
# # En el server:
# output$graf_dias <- renderPlot({
#   grafica_dias(datos_validos())
# }, res = 96)

# ---- Bloque 6 --------------------------------------------------------
library(tidyverse)
library(scales)
library(writexl)
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)
ventas <- read_csv("datasets/transacciones.csv", show_col_types = FALSE) %>%
  left_join(select(productos, producto_id, nombre_producto, categoria),
            by = "producto_id")

top_productos <- function(datos, n = 10) {
  datos %>%
    group_by(Producto = nombre_producto, `Categoría` = categoria) %>%
    summarise(Unidades = sum(cantidad),
              Ventas   = sum(total_transaccion), .groups = "drop") %>%
    slice_max(Ventas, n = n, with_ties = FALSE) %>%
    mutate(Ventas = dollar(Ventas))
}

dir.create("resultados", showWarnings = FALSE)
nombre <- paste0("top_productos_", Sys.Date())
top5 <- top_productos(ventas, n = 5)
write_excel_csv(top5, file.path("resultados", paste0(nombre, ".csv")))
write_xlsx(top5, file.path("resultados", paste0(nombre, ".xlsx")))
read_csv(file.path("resultados", paste0(nombre, ".csv")),
         show_col_types = FALSE)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # ui: la pestaña "Top productos" ahora tiene los botones arriba
# tabPanel("Top productos",
#          br(),
#          downloadButton("bajar_csv", "Descargar CSV"),
#          downloadButton("bajar_excel", "Descargar Excel"),
#          br(), br(),
#          tableOutput("tabla_top"))
#
# # server: un reactivo con la tabla visible
# tabla_visible <- reactive(top_productos(datos_filtrados(), n = input$n_top))
#
# output$tabla_top <- renderTable(tabla_visible(), align = "llrr",
#                                 digits = 0)
#
# output$bajar_csv <- downloadHandler(
#   filename = function() paste0("top_productos_", Sys.Date(), ".csv"),
#   content  = function(file) write_excel_csv(tabla_visible(), file)
# )
# output$bajar_excel <- downloadHandler(
#   filename = function() paste0("top_productos_", Sys.Date(), ".xlsx"),
#   content  = function(file) writexl::write_xlsx(tabla_visible(), file)
# )

# ---- Solo referencia (no se ejecuta automáticamente) ----
# testServer(shinyAppDir("mis-apps/02-mi-explorador"), {
#   session$setInputs(tienda = "Todas", categorias = NULL, n_top = 5,
#                     fechas = as.Date(c("2023-01-01", "2023-12-31")),
#                     metrica = "ventas")
#   read_csv(output$bajar_csv, show_col_types = FALSE)   # 5 filas
# })

# ---- Bloque 7 --------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)
tiendas <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)
productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)
ventas <- read_csv("datasets/transacciones.csv", show_col_types = FALSE) %>%
  left_join(select(tiendas, tienda_id, ciudad), by = "tienda_id") %>%
  left_join(select(productos, producto_id, nombre_producto),
            by = "producto_id")

# ---- Módulo ------------------------------------------------------------
ranking_ui <- function(id, titulo) {
  ns <- NS(id)
  box(title = titulo, width = 6, status = "primary",
      sliderInput(ns("n"), "Mostrar top:", min = 3, max = 10, value = 5),
      plotOutput(ns("grafica"), height = 300))
}

ranking_server <- function(id, datos, columna) {
  moduleServer(id, function(input, output, session) {
    resumen <- reactive({
      datos() %>%
        group_by(nombre = .data[[columna]]) %>%
        summarise(ventas = sum(total_transaccion), .groups = "drop") %>%
        slice_max(ventas, n = input$n, with_ties = FALSE)
    })
    output$grafica <- renderPlot({
      ggplot(resumen(), aes(x = ventas, y = reorder(nombre, ventas))) +
        geom_col(fill = "#2a78d6", width = 0.7) +
        scale_x_continuous(labels = label_dollar()) +
        labs(x = "Ventas", y = NULL) +
        theme_minimal(base_size = 12)
    }, res = 96)
    resumen                    # el módulo devuelve su tabla (reactivo)
  })
}

# ---- App que usa el módulo dos veces -----------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Rankings"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(fluidRow(
    ranking_ui("productos", "Top productos"),
    ranking_ui("ciudades", "Top ciudades")
  ))
)
server <- function(input, output, session) {
  datos <- reactive(ventas)
  ranking_server("productos", datos, "nombre_producto")
  ranking_server("ciudades", datos, "ciudad")
}

# Cada copia del módulo se prueba por separado con su propio 'n'
testServer(ranking_server,
           args = list(datos = reactive(ventas), columna = "ciudad"), {
  session$setInputs(n = 3)
  print(resumen())
})
testServer(ranking_server,
           args = list(datos = reactive(ventas),
                       columna = "nombre_producto"), {
  session$setInputs(n = 4)
  print(nrow(resumen()))
})

# ---- Solo referencia (no se ejecuta automáticamente) ----
# shinyApp(ui, server)

# ---- Bloque 8 --------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)
library(DT)
library(writexl)

empleados <- read_csv("datasets/empleados.csv", show_col_types = FALSE)

# ---- Lógica --------------------------------------------------------------
filtrar_empleados <- function(datos, deptos = NULL, sucursales = NULL) {
  if (length(deptos) > 0) datos <- filter(datos, departamento %in% deptos)
  if (length(sucursales) > 0) {
    datos <- filter(datos, sucursal %in% sucursales)
  }
  datos
}

kpis_rrhh <- function(datos) {
  datos %>%
    summarise(plantilla = n(),
              salario_prom = mean(salario_mensual),
              antiguedad_prom = mean(antiguedad_anos),
              pct_posgrado = mean(nivel_educacion %in%
                                    c("Maestría", "Doctorado")))
}

grafica_salario_puesto <- function(datos) {
  datos %>%
    group_by(puesto) %>%
    summarise(salario = mean(salario_mensual), .groups = "drop") %>%
    ggplot(aes(x = salario, y = reorder(puesto, salario))) +
    geom_col(fill = "#2a78d6", width = 0.7) +
    scale_x_continuous(labels = label_dollar(),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Salario mensual promedio", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

tabla_empleados <- function(datos) {
  datos %>%
    select(Nombre = nombre, Departamento = departamento, Puesto = puesto,
           Sucursal = sucursal, Salario = salario_mensual,
           `Antigüedad` = antiguedad_anos) %>%
    arrange(desc(Salario))
}

kpis_rrhh(empleados)
kpis_rrhh(filtrar_empleados(empleados, deptos = "Ventas"))

# ---- Bloque 9 --------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Recursos Humanos"),
  dashboardSidebar(
    selectizeInput("deptos", "Departamentos:",
                   choices = sort(unique(empleados$departamento)),
                   multiple = TRUE, options = list(placeholder = "Todos")),
    selectizeInput("sucursales", "Sucursales:",
                   choices = sort(unique(empleados$sucursal)),
                   multiple = TRUE, options = list(placeholder = "Todas"))
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("plantilla", width = 3),
      valueBoxOutput("salario", width = 3),
      valueBoxOutput("antiguedad", width = 3),
      valueBoxOutput("posgrado", width = 3)
    ),
    fluidRow(
      box(title = "Salario promedio por puesto", width = 5,
          status = "primary", plotOutput("graf_puesto", height = 320)),
      box(title = "Empleados", width = 7, status = "primary",
          downloadButton("bajar", "Descargar Excel"), br(), br(),
          DTOutput("tabla"))
    )
  )
)

server <- function(input, output, session) {
  datos <- reactive(filtrar_empleados(empleados, input$deptos,
                                      input$sucursales))
  kpis <- reactive(kpis_rrhh(datos()))

  output$plantilla <- renderValueBox(
    valueBox(kpis()$plantilla, "Plantilla", icon = icon("users")))
  output$salario <- renderValueBox(
    valueBox(dollar(kpis()$salario_prom, accuracy = 1),
             "Salario mensual promedio", icon = icon("money-bill")))
  output$antiguedad <- renderValueBox(
    valueBox(paste(round(kpis()$antiguedad_prom, 1), "años"),
             "Antigüedad promedio", icon = icon("clock")))
  output$posgrado <- renderValueBox(
    valueBox(percent(kpis()$pct_posgrado, accuracy = 1),
             "Con maestría o doctorado", icon = icon("graduation-cap")))

  output$graf_puesto <- renderPlot({
    validate(need(nrow(datos()) > 0, "No hay empleados con esos filtros."))
    grafica_salario_puesto(datos())
  }, res = 96)

  output$tabla <- renderDT(
    datatable(tabla_empleados(datos()), rownames = FALSE,
              options = list(pageLength = 8)) %>%
      formatCurrency("Salario", currency = "$", digits = 0))

  output$bajar <- downloadHandler(
    filename = function() paste0("empleados_", Sys.Date(), ".xlsx"),
    content  = function(file) write_xlsx(tabla_empleados(datos()), file)
  )
}

testServer(server, {
  session$setInputs(deptos = c("IT", "Finanzas"), sucursales = NULL)
  cat("Plantilla IT + Finanzas:", kpis()$plantilla, "\n")
  cat("Filas en el Excel:",
      nrow(readxl::read_excel(output$bajar)), "\n")
})

# ---- Solo referencia (no se ejecuta automáticamente) ----
# shinyApp(ui, server)
