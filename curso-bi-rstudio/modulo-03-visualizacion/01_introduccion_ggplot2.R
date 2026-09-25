# ============================================================================
# MÓDULO 3 - LECCIÓN 1: INTRODUCCIÓN A GGPLOT2
# ============================================================================
# Objetivo: Dominar la gramática de gráficos para visualización profesional
# ============================================================================

# ggplot2 es el sistema de visualización más poderoso de R
# Basado en "Grammar of Graphics" - una forma consistente de crear gráficos

# ============================================================================
# 1. INSTALACIÓN Y CARGA
# ============================================================================

# Instalar (si no lo tienes)
# install.packages("ggplot2")
# install.packages("scales")

library(ggplot2)
library(dplyr)
library(scales)  # Para formatear ejes

# ============================================================================
# 2. LA GRAMÁTICA DE GRÁFICOS
# ============================================================================

# Todo gráfico en ggplot2 tiene 3 componentes esenciales:
#
# 1. DATA: Los datos a visualizar
# 2. AESTHETICS (aes): Mapeo de variables a propiedades visuales (x, y, color, tamaño)
# 3. GEOM: El tipo de gráfico (puntos, líneas, barras, etc.)
#
# Sintaxis básica:
# ggplot(data = datos, aes(x = variable_x, y = variable_y)) +
#   geom_tipo()

# ============================================================================
# 3. DATOS DE EJEMPLO
# ============================================================================

# Crear dataset de ventas mensuales
ventas_mes <- data.frame(
  mes = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
         "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"),
  ventas = c(45000, 52000, 48000, 61000, 58000, 67000,
            72000, 69000, 64000, 71000, 78000, 95000),
  meta = c(50000, 50000, 50000, 60000, 60000, 65000,
          70000, 70000, 65000, 70000, 75000, 90000),
  mes_num = 1:12
)

# Ver datos
print(ventas_mes)

# ============================================================================
# 4. TU PRIMER GRÁFICO: GRÁFICO DE LÍNEAS
# ============================================================================

# Gráfico básico de líneas
ggplot(data = ventas_mes, aes(x = mes_num, y = ventas)) +
  geom_line()

# Mejorar el gráfico agregando puntos
ggplot(data = ventas_mes, aes(x = mes_num, y = ventas)) +
  geom_line() +
  geom_point()

# Agregar color y tamaño
ggplot(data = ventas_mes, aes(x = mes_num, y = ventas)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "red", size = 3)

# ============================================================================
# 5. GRÁFICO DE BARRAS
# ============================================================================

# Gráfico de barras simple
ggplot(ventas_mes, aes(x = mes, y = ventas)) +
  geom_col()  # geom_col para barras con valores y

# Nota: También existe geom_bar() que cuenta frecuencias

# Ordenar meses correctamente
ventas_mes$mes <- factor(ventas_mes$mes,
                        levels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                                  "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))

# Gráfico mejorado
ggplot(ventas_mes, aes(x = mes, y = ventas)) +
  geom_col(fill = "steelblue") +
  theme_minimal()

# Barras horizontales
ggplot(ventas_mes, aes(x = ventas, y = mes)) +
  geom_col(fill = "darkgreen")

# ============================================================================
# 6. AGREGAR TÍTULOS Y ETIQUETAS
# ============================================================================

ggplot(ventas_mes, aes(x = mes, y = ventas)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Ventas Mensuales 2024",
    subtitle = "Análisis de rendimiento anual",
    x = "Mes",
    y = "Ventas (MXN)",
    caption = "Fuente: Sistema de Ventas"
  ) +
  theme_minimal()

# ============================================================================
# 7. FORMATEAR EJES
# ============================================================================

# Formatear eje Y con separadores de miles y símbolo de moneda
ggplot(ventas_mes, aes(x = mes, y = ventas)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(
    labels = dollar_format(prefix = "$", suffix = "", big.mark = ",")
  ) +
  labs(title = "Ventas Mensuales 2024", x = "Mes", y = "Ventas") +
  theme_minimal()

# ============================================================================
# 8. MÚLTIPLES CAPAS Y COMPARACIONES
# ============================================================================

# Comparar ventas vs meta
ggplot(ventas_mes, aes(x = mes)) +
  # Barras de ventas
  geom_col(aes(y = ventas), fill = "steelblue", alpha = 0.7) +
  # Línea de meta
  geom_line(aes(y = meta, group = 1), color = "red", linewidth = 1.2) +
  geom_point(aes(y = meta), color = "red", size = 3) +
  scale_y_continuous(
    labels = dollar_format(prefix = "$", big.mark = ",")
  ) +
  labs(
    title = "Ventas vs Meta 2024",
    x = "Mes",
    y = "Monto"
  ) +
  theme_minimal()

# ============================================================================
# 9. GRÁFICO DE DISPERSIÓN (SCATTER PLOT)
# ============================================================================

# Dataset de productos
productos <- data.frame(
  producto = paste("Prod", 1:20),
  precio = c(25, 45, 89, 120, 35, 67, 200, 150, 78, 95,
            110, 135, 48, 72, 155, 99, 185, 62, 140, 105),
  unidades_vendidas = c(450, 320, 180, 95, 410, 240, 65, 110, 220, 175,
                       130, 105, 350, 255, 88, 165, 72, 290, 98, 145)
)

# Gráfico de dispersión básico
ggplot(productos, aes(x = precio, y = unidades_vendidas)) +
  geom_point()

# Mejorado con color y tamaño
ggplot(productos, aes(x = precio, y = unidades_vendidas)) +
  geom_point(color = "darkblue", size = 3, alpha = 0.6) +
  labs(
    title = "Relación Precio vs Unidades Vendidas",
    x = "Precio (MXN)",
    y = "Unidades Vendidas"
  ) +
  theme_minimal()

# Agregar línea de tendencia
ggplot(productos, aes(x = precio, y = unidades_vendidas)) +
  geom_point(color = "darkblue", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # lm = linear model
  labs(
    title = "Relación Precio vs Unidades Vendidas",
    subtitle = "Con línea de tendencia",
    x = "Precio (MXN)",
    y = "Unidades Vendidas"
  ) +
  theme_minimal()

# ============================================================================
# 10. USAR COLORES POR CATEGORÍAS
# ============================================================================

# Agregar categoría a productos
productos$categoria <- c("Electrónica", "Ropa", "Hogar", "Electrónica", "Ropa",
                        "Hogar", "Electrónica", "Electrónica", "Ropa", "Hogar",
                        "Electrónica", "Electrónica", "Ropa", "Hogar", "Electrónica",
                        "Hogar", "Electrónica", "Ropa", "Electrónica", "Hogar")

# Colorear por categoría
ggplot(productos, aes(x = precio, y = unidades_vendidas, color = categoria)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(
    title = "Ventas por Categoría",
    x = "Precio (MXN)",
    y = "Unidades Vendidas",
    color = "Categoría"
  ) +
  theme_minimal()

# ============================================================================
# 11. GRÁFICO DE BARRAS POR GRUPOS
# ============================================================================

# Ventas por categoría
ventas_categoria <- productos %>%
  group_by(categoria) %>%
  summarise(
    total_ventas = sum(precio * unidades_vendidas),
    unidades_totales = sum(unidades_vendidas)
  )

# Gráfico de barras por categoría
ggplot(ventas_categoria, aes(x = reorder(categoria, -total_ventas), y = total_ventas)) +
  geom_col(aes(fill = categoria)) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  labs(
    title = "Ingresos Totales por Categoría",
    x = "Categoría",
    y = "Ingresos"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Quitar leyenda

# ============================================================================
# 12. HISTOGRAMA - DISTRIBUCIÓN DE DATOS
# ============================================================================

# Crear datos de salarios
set.seed(123)
salarios <- data.frame(
  salario = c(rnorm(50, mean = 25000, sd = 5000),
             rnorm(30, mean = 40000, sd = 8000),
             rnorm(20, mean = 60000, sd = 10000))
)

# Histograma básico
ggplot(salarios, aes(x = salario)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(
    title = "Distribución de Salarios",
    x = "Salario (MXN)",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Con línea de densidad
ggplot(salarios, aes(x = salario)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1.2) +
  scale_x_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  labs(
    title = "Distribución de Salarios",
    x = "Salario (MXN)",
    y = "Densidad"
  ) +
  theme_minimal()

# ============================================================================
# 13. BOXPLOT - IDENTIFICAR OUTLIERS
# ============================================================================

# Ventas por vendedor
ventas_vendedor <- data.frame(
  vendedor = rep(c("Ana", "Carlos", "Diana"), each = 10),
  venta = c(
    rnorm(10, mean = 50000, sd = 8000),   # Ana
    rnorm(10, mean = 45000, sd = 12000),  # Carlos
    rnorm(10, mean = 60000, sd = 7000)    # Diana
  )
)

# Boxplot
ggplot(ventas_vendedor, aes(x = vendedor, y = venta, fill = vendedor)) +
  geom_boxplot() +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  labs(
    title = "Distribución de Ventas por Vendedor",
    x = "Vendedor",
    y = "Venta (MXN)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ============================================================================
# 14. TEMAS Y PERSONALIZACIÓN
# ============================================================================

# ggplot2 incluye varios temas predefinidos:

# theme_minimal()
# theme_classic()
# theme_bw()
# theme_light()
# theme_dark()
# theme_void()

# Ejemplo con diferentes temas
p <- ggplot(ventas_mes, aes(x = mes, y = ventas)) +
  geom_col(fill = "steelblue") +
  labs(title = "Ventas Mensuales")

# Tema minimal (limpio)
p + theme_minimal()

# Tema clásico (ejes en L)
p + theme_classic()

# Tema blanco y negro
p + theme_bw()

# Tema oscuro
p + theme_dark()

# ============================================================================
# 15. GUARDAR GRÁFICOS
# ============================================================================

# Crear un gráfico
mi_grafico <- ggplot(ventas_mes, aes(x = mes, y = ventas)) +
  geom_col(fill = "steelblue") +
  geom_line(aes(y = meta, group = 1), color = "red", linewidth = 1.2) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  labs(
    title = "Ventas vs Meta 2024",
    x = "Mes",
    y = "Monto"
  ) +
  theme_minimal()

# Guardar como PNG
ggsave("ventas_2024.png", plot = mi_grafico, width = 10, height = 6, dpi = 300)

# Guardar como PDF (para impresión)
ggsave("ventas_2024.pdf", plot = mi_grafico, width = 10, height = 6)

# Guardar como JPG
ggsave("ventas_2024.jpg", plot = mi_grafico, width = 10, height = 6, dpi = 300)

# ============================================================================
# EJERCICIO PRÁCTICO 1: CREAR DASHBOARD VISUAL
# ============================================================================

# Crea un gráfico de barras mostrando:
# - Los 5 productos más vendidos (por unidades)
# - Barras coloreadas por categoría
# - Etiquetas y título profesionales

top_productos <- productos %>%
  arrange(desc(unidades_vendidas)) %>%
  slice(1:5)

ggplot(top_productos, aes(x = reorder(producto, unidades_vendidas),
                         y = unidades_vendidas,
                         fill = categoria)) +
  geom_col() +
  coord_flip() +  # Horizontal
  labs(
    title = "Top 5 Productos Más Vendidos",
    x = "Producto",
    y = "Unidades Vendidas",
    fill = "Categoría"
  ) +
  theme_minimal()

# ============================================================================
# EJERCICIO PRÁCTICO 2: ANÁLISIS DE TENDENCIA
# ============================================================================

# Crear gráfico mostrando:
# - Línea de ventas mensuales
# - Área sombreada debajo de la línea
# - Punto destacado en el mes de mayor venta

mes_max <- ventas_mes[which.max(ventas_mes$ventas), ]

ggplot(ventas_mes, aes(x = mes_num, y = ventas)) +
  geom_area(fill = "lightblue", alpha = 0.5) +
  geom_line(color = "darkblue", linewidth = 1.2) +
  geom_point(data = mes_max, aes(x = mes_num, y = ventas),
            color = "red", size = 5) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  labs(
    title = "Tendencia de Ventas 2024",
    subtitle = "El punto rojo indica el mes de mayor venta",
    x = "Mes",
    y = "Ventas"
  ) +
  theme_minimal()

# ============================================================================
# EJERCICIOS PARA PRACTICAR TÚ
# ============================================================================

# 1. Crea un gráfico de dispersión mostrando la relación entre
#    precio y unidades vendidas para cada categoría (usando facets)

# TU CÓDIGO AQUÍ:




# 2. Crea un gráfico de barras apiladas mostrando las ventas totales
#    por mes, divididas por categoría

# TU CÓDIGO AQUÍ:




# 3. Crea un boxplot comparando los precios de productos por categoría

# TU CÓDIGO AQUÍ:




# ============================================================================
# ¡EXCELENTE TRABAJO!
# ============================================================================
# Ahora dominas:
# ✓ La gramática de gráficos (data + aes + geom)
# ✓ Gráficos de líneas, barras y dispersión
# ✓ Histogramas y boxplots
# ✓ Personalizar títulos, etiquetas y colores
# ✓ Aplicar temas profesionales
# ✓ Guardar gráficos en alta calidad
#
# Continúa con: 02_graficos_barras.R
# ============================================================================
