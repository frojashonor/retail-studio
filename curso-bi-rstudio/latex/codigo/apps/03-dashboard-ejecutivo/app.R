# ========================================================================
# App 03 - Dashboard ejecutivo de ventas (shinydashboard)
# Libro "Business Intelligence con R y RStudio", Módulo 6
#
# Cómo ejecutarla (con el directorio de trabajo en la carpeta del curso):
#   shiny::runApp("latex/codigo/apps/03-dashboard-ejecutivo")
# Las funciones de datos, KPIs y gráficas están en R/funciones_dashboard.R
# (Shiny las carga solas antes de este archivo).
# ========================================================================

# 1. Paquetes ------------------------------------------------------------
library(shiny)
library(shinydashboard)   # dashboardPage(), box(), valueBox()...
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(scales)
library(DT)               # tablas interactivas
library(writexl)          # descarga en Excel

# 2. Datos: se leen UNA vez, fuera del server ----------------------------
datos <- cargar_datos_dashboard()
fecha_min <- min(datos$fecha)
fecha_max <- max(datos$fecha)
opciones_tiendas    <- sort(unique(datos$tienda))
opciones_categorias <- sort(unique(datos$categoria))

# 3. Interfaz: encabezado y barra lateral --------------------------------
encabezado <- dashboardHeader(title = "Ventas 2023")

barra_lateral <- dashboardSidebar(
  sidebarMenu(
    id = "menu",
    menuItem("Resumen",   tabName = "resumen",   icon = icon("gauge")),
    menuItem("Productos", tabName = "productos", icon = icon("box")),
    menuItem("Tiendas",   tabName = "tiendas",   icon = icon("store")),
    menuItem("Clientes",  tabName = "clientes",  icon = icon("users")),
    menuItem("Datos",     tabName = "datos",     icon = icon("table"))
  ),
  # Filtros globales: afectan a TODAS las pestañas
  dateRangeInput("fechas", "Periodo:", start = fecha_min, end = fecha_max,
                 min = fecha_min, max = fecha_max, format = "dd/mm/yyyy",
                 language = "es", separator = " a "),
  selectizeInput("tiendas", "Tiendas:", choices = opciones_tiendas,
                 multiple = TRUE, options = list(placeholder = "Todas")),
  selectizeInput("categorias", "Categorías:",
                 choices = opciones_categorias, multiple = TRUE,
                 options = list(placeholder = "Todas")),
  actionButton("limpiar", "Quitar filtros", icon = icon("rotate-left"))
)

# 4. Interfaz: cuerpo con una tabItem por pestaña del menú ---------------
cuerpo <- dashboardBody(
  tabItems(
    tabItem(tabName = "resumen",
      fluidRow(                                   # fila de KPIs arriba
        valueBoxOutput("kpi_ventas", width = 3),
        valueBoxOutput("kpi_transacciones", width = 3),
        valueBoxOutput("kpi_ticket", width = 3),
        valueBoxOutput("kpi_clientes", width = 3)
      ),
      fluidRow(
        box(title = "Ventas por mes", width = 8, status = "primary",
            solidHeader = TRUE, plotOutput("graf_evolucion", height = 300)),
        box(title = "Ventas por método de pago", width = 4,
            status = "primary", solidHeader = TRUE,
            plotOutput("graf_pago", height = 300))
      )
    ),
    tabItem(tabName = "productos",
      fluidRow(
        box(title = "Top 10 productos", width = 6, status = "primary",
            plotOutput("graf_top_productos", height = 380)),
        box(title = "Ventas por categoría", width = 6, status = "primary",
            plotOutput("graf_categorias", height = 380))
      )
    ),
    tabItem(tabName = "tiendas",
      fluidRow(
        box(title = "Ventas por tienda", width = 7, status = "primary",
            plotOutput("graf_tiendas", height = 380)),
        box(title = "Indicadores por tienda", width = 5,
            status = "primary", tableOutput("tabla_tiendas"))
      )
    ),
    tabItem(tabName = "clientes",
      fluidRow(
        infoBoxOutput("info_clientes"),
        infoBoxOutput("info_premium"),
        infoBoxOutput("info_frecuencia")
      ),
      fluidRow(
        box(title = "Ventas por segmento", width = 6, status = "primary",
            plotOutput("graf_segmentos", height = 320)),
        box(title = "Top 10 clientes", width = 6, status = "primary",
            tableOutput("tabla_clientes"))
      )
    ),
    tabItem(tabName = "datos",
      fluidRow(
        box(title = "Detalle de transacciones", width = 12,
            status = "primary",
            downloadButton("descargar_csv", "Descargar CSV"),
            downloadButton("descargar_excel", "Descargar Excel"),
            br(), br(),
            DTOutput("tabla_detalle"))
      )
    )
  )
)

ui <- dashboardPage(encabezado, barra_lateral, cuerpo, skin = "blue")

# 5. Servidor: datos filtrados, KPIs y botón para limpiar filtros --------
server <- function(input, output, session) {

  # La pieza central: TODO lo demás depende de datos_filtrados()
  datos_filtrados <- reactive({
    req(input$fechas)
    filtrar_datos(datos, input$fechas, input$tiendas, input$categorias)
  }) %>%
    bindCache(input$fechas, input$tiendas, input$categorias)

  kpis <- reactive(calcular_kpis(datos_filtrados()))

  # Igual que datos_filtrados(), pero si los filtros no dejan ninguna
  # venta, las salidas que lo usan muestran un mensaje amable
  datos_validos <- reactive({
    validate(need(nrow(datos_filtrados()) > 0,
                  "No hay ventas con estos filtros."))
    datos_filtrados()
  })

  observeEvent(input$limpiar, {
    updateDateRangeInput(session, "fechas", start = fecha_min,
                         end = fecha_max)
    updateSelectizeInput(session, "tiendas", selected = character(0))
    updateSelectizeInput(session, "categorias", selected = character(0))
  })

  output$kpi_ventas <- renderValueBox({
    valueBox(formato_pesos(kpis()$ventas), "Ventas totales",
             icon = icon("dollar-sign"), color = "blue")
  })
  output$kpi_transacciones <- renderValueBox({
    valueBox(formato_numero(kpis()$transacciones), "Transacciones",
             icon = icon("receipt"), color = "blue")
  })
  output$kpi_ticket <- renderValueBox({
    valueBox(formato_pesos(kpis()$ticket_promedio), "Ticket promedio",
             icon = icon("cart-shopping"), color = "blue")
  })
  output$kpi_clientes <- renderValueBox({
    valueBox(formato_numero(kpis()$clientes), "Clientes activos",
             icon = icon("users"), color = "blue")
  })

  # 6. Servidor: gráficas y tablas de cada pestaña -----------------------
  output$graf_evolucion <- renderPlot({
    grafica_evolucion(datos_validos())
  }, res = 96)
  output$graf_pago <- renderPlot({
    grafica_metodo_pago(datos_validos())
  }, res = 96)
  output$graf_top_productos <- renderPlot({
    grafica_top_productos(datos_validos())
  }, res = 96)
  output$graf_categorias <- renderPlot({
    grafica_categorias(datos_validos())
  }, res = 96)
  output$graf_tiendas <- renderPlot({
    grafica_tiendas(datos_validos())
  }, res = 96)
  output$tabla_tiendas <- renderTable(tabla_tiendas(datos_filtrados()))

  output$info_clientes <- renderInfoBox({
    infoBox("Clientes activos", formato_numero(kpis()$clientes),
            icon = icon("users"), color = "blue")
  })
  output$info_premium <- renderInfoBox({
    infoBox("Clientes premium", percent(kpis()$pct_premium, accuracy = 0.1),
            icon = icon("star"), color = "blue")
  })
  output$info_frecuencia <- renderInfoBox({
    infoBox("Compras por cliente",
            formato_numero(kpis()$compras_cliente, decimales = 1),
            icon = icon("repeat"), color = "blue")
  })
  output$graf_segmentos <- renderPlot({
    grafica_segmentos(datos_validos())
  }, res = 96)
  output$tabla_clientes <- renderTable(
    tabla_top_clientes(datos_filtrados()))

  output$tabla_detalle <- renderDT(tabla_detalle_dt(datos_filtrados()))

  # 7. Servidor: descargas (respetan los filtros activos) ----------------
  output$descargar_csv <- downloadHandler(
    filename = function() paste0("ventas_filtradas_", Sys.Date(), ".csv"),
    content  = function(file) {
      write_excel_csv(datos_detalle(datos_filtrados()), file)
    }
  )
  output$descargar_excel <- downloadHandler(
    filename = function() paste0("ventas_filtradas_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      write_xlsx(datos_detalle(datos_filtrados()), file)
    }
  )
}

# 8. Lanzar la app -------------------------------------------------------
shinyApp(ui, server)
