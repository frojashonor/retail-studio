# ========================================================================
# App 01 - Mi primera app Shiny: calculadora de precio con descuento
# Libro "Business Intelligence con R y RStudio", Módulo 6
#
# Cómo ejecutarla (con el directorio de trabajo en la carpeta del curso):
#   shiny::runApp("latex/codigo/apps/01-primera-app")
# o abre este archivo en RStudio y pulsa el botón "Run App".
# Para detenerla: tecla Esc en la consola o el botón rojo "Stop".
# ========================================================================

# 1. Paquete -------------------------------------------------------------
library(shiny)          # el motor de las apps web con R

# 2. Interfaz (ui): lo que el usuario VE en el navegador -----------------
ui <- fluidPage(                       # página que se adapta al ancho
  titlePanel("Calculadora de precio con descuento"),   # título

  # Entrada 1: una caja para escribir un número (el precio de lista)
  numericInput(inputId = "precio", label = "Precio de lista ($):",
               value = 500, min = 0, step = 10),

  # Entrada 2: un deslizador para elegir el descuento
  sliderInput(inputId = "descuento", label = "Descuento (%):",
              min = 0, max = 50, value = 10, step = 5),

  # Salida: un hueco donde el servidor escribirá el resultado
  h3(textOutput(outputId = "precio_final"))
)

# 3. Servidor (server): la lógica que CORRE en R -------------------------
server <- function(input, output, session) {
  # renderText() se vuelve a ejecutar SOLO cuando cambian las entradas
  # que usa (input$precio o input$descuento): eso es la reactividad
  output$precio_final <- renderText({
    final <- input$precio * (1 - input$descuento / 100)
    paste0("Precio final: ", scales::dollar(final))
  })
}

# 4. Unir interfaz y servidor: esto crea (y lanza) la app ----------------
shinyApp(ui = ui, server = server)
