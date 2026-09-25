# ============================================================================
# MÓDULO 1 - LECCIÓN 3: FUNCIONES BÁSICAS ESENCIALES
# ============================================================================
# Objetivo: Dominar las funciones más importantes para análisis de datos
# ============================================================================

# ============================================================================
# 1. FUNCIONES MATEMÁTICAS
# ============================================================================

ventas <- c(1200, 1500, 980, 1750, 2100, 1450, 1820)

# Suma total
sum(ventas)

# Promedio (media aritmética)
mean(ventas)

# Mediana (valor central)
median(ventas)

# Desviación estándar
sd(ventas)

# Varianza
var(ventas)

# Valor mínimo
min(ventas)

# Valor máximo
max(ventas)

# Rango (diferencia entre máximo y mínimo)
range(ventas)  # Devuelve ambos valores
diff(range(ventas))  # Diferencia

# Cuantiles (percentiles)
quantile(ventas)  # Por defecto: 0%, 25%, 50%, 75%, 100%
quantile(ventas, probs = 0.90)  # Percentil 90

# Valor absoluto
abs(-15.5)

# Redondeo
round(3.14159, digits = 2)  # 3.14
ceiling(3.2)  # Redondea hacia arriba: 4
floor(3.8)    # Redondea hacia abajo: 3

# Logaritmos
log(100)      # Logaritmo natural (base e)
log10(100)    # Logaritmo base 10
log2(64)      # Logaritmo base 2

# ============================================================================
# 2. FUNCIONES DE CARACTERES (TEXTO)
# ============================================================================

# Concatenar textos
paste("Ventas", "Totales", sep = " ")
paste("Q", 1:4, sep = "")  # Q1, Q2, Q3, Q4

# Concatenar sin separador
paste0("Producto_", 1:5)  # Producto_1, Producto_2, ...

# Convertir a mayúsculas
producto <- "laptop hp pavilion"
toupper(producto)

# Convertir a minúsculas
empresa <- "RETAIL SOLUTIONS INC"
tolower(empresa)

# Primera letra mayúscula de cada palabra
library(tools)  # Cargar paquete tools
toTitleCase("business intelligence con rstudio")

# Longitud de un texto
nchar("Análisis de Datos")

# Extraer parte de un texto (substring)
codigo_producto <- "ELEC-2024-1453"
substr(codigo_producto, 1, 4)  # Extraer "ELEC"
substr(codigo_producto, 6, 9)  # Extraer año "2024"

# Reemplazar texto
gsub("HP", "DELL", "Laptop HP Core i7")  # Reemplazar HP por DELL
gsub(" ", "_", "Ventas Primer Trimestre")  # Reemplazar espacios

# Dividir texto
strsplit("Rojo,Verde,Azul,Amarillo", split = ",")

# Eliminar espacios en blanco
texto_espacios <- "  Datos con espacios   "
trimws(texto_espacios)

# ============================================================================
# 3. FUNCIONES DE BÚSQUEDA Y COMPARACIÓN
# ============================================================================

productos <- c("Laptop", "Mouse", "Teclado", "Monitor", "Mouse", "Webcam")

# Verificar si un elemento está en el vector
"Mouse" %in% productos  # TRUE
"Tablet" %in% productos  # FALSE

# Encontrar posición de un elemento
which(productos == "Mouse")  # Devuelve todas las posiciones

# Encontrar la primera aparición
match("Mouse", productos)

# Valores únicos (eliminar duplicados)
unique(productos)

# Contar elementos únicos
length(unique(productos))

# Elementos duplicados
duplicated(productos)  # Vector lógico
productos[duplicated(productos)]  # Ver duplicados

# Ordenar valores
sort(ventas)  # Ascendente
sort(ventas, decreasing = TRUE)  # Descendente

# Orden de índices
order(ventas)  # Índices ordenados ascendente
ventas[order(ventas)]  # Equivalente a sort()

# ============================================================================
# 4. FUNCIONES CONDICIONALES
# ============================================================================

# if-else básico
venta_dia <- 1500
meta_dia <- 1200

if (venta_dia >= meta_dia) {
  print("Meta cumplida")
} else {
  print("Meta no cumplida")
}

# ifelse vectorizado (aplicar a todos los elementos)
ventas_semana <- c(1200, 1500, 980, 1750, 2100, 1450, 1820)
meta <- 1300

resultado <- ifelse(ventas_semana >= meta, "Cumplió", "No cumplió")
resultado

# Crear categorías con ifelse anidado
categorizar_venta <- function(venta) {
  ifelse(venta < 1000, "Baja",
         ifelse(venta < 1500, "Media",
                ifelse(venta < 2000, "Alta", "Muy Alta")))
}

categorias <- categorizar_venta(ventas_semana)
categorias

# switch (alternativa a múltiples if)
obtener_mensaje <- function(dia) {
  switch(dia,
         "Lunes" = "Inicio de semana",
         "Viernes" = "Fin de semana laboral",
         "Sábado" = "Fin de semana",
         "Domingo" = "Fin de semana",
         "Día laboral"  # Default
  )
}

obtener_mensaje("Lunes")
obtener_mensaje("Miércoles")

# ============================================================================
# 5. FUNCIONES DE REPETICIÓN (LOOPS)
# ============================================================================

# For loop - iterar sobre elementos
dias <- c("Lun", "Mar", "Mié", "Jue", "Vie")

for (dia in dias) {
  print(paste("Hoy es:", dia))
}

# For con índices
ventas_semana <- c(1200, 1500, 980, 1750, 2100)

for (i in 1:length(ventas_semana)) {
  print(paste("Día", i, "- Ventas:", ventas_semana[i]))
}

# While loop - mientras se cumpla condición
contador <- 1
while (contador <= 5) {
  print(paste("Iteración:", contador))
  contador <- contador + 1
}

# Ejemplo práctico: acumular ventas hasta alcanzar meta
ventas_diarias <- c(300, 450, 280, 520, 390, 410)
meta_acumulada <- 2000
dia <- 1
acumulado <- 0

while (acumulado < meta_acumulada && dia <= length(ventas_diarias)) {
  acumulado <- acumulado + ventas_diarias[dia]
  print(paste("Día", dia, "- Acumulado:", acumulado))
  dia <- dia + 1
}

# ============================================================================
# 6. FUNCIONES APPLY (ALTERNATIVA EFICIENTE A LOOPS)
# ============================================================================

# La familia apply es más eficiente que loops en R

# Matriz de ventas
ventas_matriz <- matrix(c(100, 120, 110, 130,
                          90, 95, 100, 105,
                          150, 160, 155, 170),
                        nrow = 3, byrow = TRUE)
rownames(ventas_matriz) <- c("Producto_A", "Producto_B", "Producto_C")
colnames(ventas_matriz) <- paste("Mes", 1:4)
ventas_matriz

# apply: aplicar función a filas o columnas
# MARGIN: 1 = filas, 2 = columnas

# Total por producto (suma por filas)
apply(ventas_matriz, MARGIN = 1, FUN = sum)

# Total por mes (suma por columnas)
apply(ventas_matriz, MARGIN = 2, FUN = sum)

# Promedio por producto
apply(ventas_matriz, 1, mean)

# lapply: aplicar función a lista, devuelve lista
ventas_tiendas <- list(
  tienda1 = c(100, 120, 110),
  tienda2 = c(150, 140, 160),
  tienda3 = c(90, 95, 100)
)

# Calcular promedio de cada tienda
lapply(ventas_tiendas, mean)

# sapply: igual que lapply pero simplifica el resultado
sapply(ventas_tiendas, mean)  # Devuelve vector

# tapply: aplicar función por grupos
empleados_ventas <- c(5000, 6000, 4500, 7000, 5500, 6200)
departamentos <- c("A", "B", "A", "B", "A", "B")

# Promedio de ventas por departamento
tapply(empleados_ventas, departamentos, mean)

# ============================================================================
# 7. CREAR TUS PROPIAS FUNCIONES
# ============================================================================

# Sintaxis básica de una función
# nombre_funcion <- function(parametros) {
#   código
#   return(resultado)
# }

# Función simple: calcular margen de ganancia
calcular_margen <- function(precio_venta, costo) {
  ganancia <- precio_venta - costo
  margen <- (ganancia / precio_venta) * 100
  return(margen)
}

# Usar la función
calcular_margen(100, 60)  # 40% margen
calcular_margen(precio_venta = 150, costo = 90)

# Función con valores por defecto
calcular_precio_final <- function(precio, descuento = 0, impuesto = 0.16) {
  precio_con_descuento <- precio * (1 - descuento)
  precio_final <- precio_con_descuento * (1 + impuesto)
  return(precio_final)
}

calcular_precio_final(100)  # Sin descuento, con impuesto por defecto
calcular_precio_final(100, descuento = 0.20)  # Con 20% descuento
calcular_precio_final(100, descuento = 0.10, impuesto = 0.18)

# Función que devuelve múltiples valores (lista)
analizar_ventas <- function(ventas) {
  resultado <- list(
    total = sum(ventas),
    promedio = mean(ventas),
    maximo = max(ventas),
    minimo = min(ventas),
    dias_sobre_promedio = sum(ventas > mean(ventas))
  )
  return(resultado)
}

ventas_mes <- c(1200, 1500, 980, 1750, 2100, 1450, 1820)
analisis <- analizar_ventas(ventas_mes)
analisis

# Acceder a resultados
analisis$total
analisis$promedio

# Función con validación de datos
calcular_roi <- function(inversion, ganancia) {
  # Validar que inversion no sea cero
  if (inversion == 0) {
    stop("La inversión no puede ser cero")
  }

  # Validar que sean números positivos
  if (inversion < 0 || ganancia < 0) {
    warning("Se detectaron valores negativos")
  }

  roi <- ((ganancia - inversion) / inversion) * 100
  return(roi)
}

calcular_roi(10000, 13500)  # ROI del 35%
# calcular_roi(0, 5000)  # Generaría un error

# ============================================================================
# 8. FUNCIONES ÚTILES PARA MISSING VALUES (NA)
# ============================================================================

# Datos con valores faltantes
ventas_incompletas <- c(1200, NA, 980, 1750, NA, 1450, 1820)

# Identificar NAs
is.na(ventas_incompletas)

# Contar NAs
sum(is.na(ventas_incompletas))

# Eliminar NAs
na.omit(ventas_incompletas)

# Funciones que manejan NAs
mean(ventas_incompletas)  # Devuelve NA
mean(ventas_incompletas, na.rm = TRUE)  # Ignora NAs

sum(ventas_incompletas, na.rm = TRUE)
max(ventas_incompletas, na.rm = TRUE)

# Reemplazar NAs con un valor
ventas_completas <- ventas_incompletas
ventas_completas[is.na(ventas_completas)] <- mean(ventas_completas, na.rm = TRUE)
ventas_completas

# ============================================================================
# 9. FUNCIONES PARA DATA FRAMES
# ============================================================================

# Crear data frame de ejemplo
df_productos <- data.frame(
  producto = c("A", "B", "C", "D", "E"),
  precio = c(100, 150, 80, 200, 120),
  unidades = c(50, 30, 80, 25, 60),
  categoria = c("Electrónica", "Ropa", "Alimentos",
                "Electrónica", "Ropa")
)

# Ver estructura
str(df_productos)

# Primeras filas
head(df_productos, n = 3)

# Últimas filas
tail(df_productos, n = 2)

# Resumen estadístico
summary(df_productos)

# Ver nombres de columnas
names(df_productos)

# Cambiar nombres de columnas
colnames(df_productos)[2] <- "precio_unitario"
df_productos

# Filtrar filas
subset(df_productos, precio_unitario > 100)
subset(df_productos, categoria == "Electrónica")

# Agregar columna
df_productos$ingresos <- df_productos$precio_unitario * df_productos$unidades
df_productos

# Ordenar data frame
df_productos[order(df_productos$ingresos, decreasing = TRUE), ]

# ============================================================================
# EJERCICIO PRÁCTICO 1: ANÁLISIS DE RENDIMIENTO
# ============================================================================

# Tienes ventas mensuales de un equipo de vendedores

vendedor_1 <- c(5000, 5500, 6200, 5800, 6500, 7000)
vendedor_2 <- c(4500, 4800, 5000, 4900, 5200, 5500)
vendedor_3 <- c(6000, 6200, 5800, 6500, 6800, 7200)

# CREA UNA FUNCIÓN que analice el desempeño de un vendedor:
# - Total vendido
# - Promedio mensual
# - Mes con mejor venta
# - Mes con peor venta
# - Crecimiento (última venta vs primera venta)

analizar_vendedor <- function(ventas) {
  list(
    total = sum(ventas),
    promedio = round(mean(ventas), 2),
    mejor_mes = which.max(ventas),
    mejor_venta = max(ventas),
    peor_mes = which.min(ventas),
    peor_venta = min(ventas),
    crecimiento_pct = round(((ventas[length(ventas)] - ventas[1]) / ventas[1]) * 100, 2)
  )
}

# Analizar cada vendedor
print("Vendedor 1:")
analizar_vendedor(vendedor_1)

print("Vendedor 2:")
analizar_vendedor(vendedor_2)

print("Vendedor 3:")
analizar_vendedor(vendedor_3)

# ============================================================================
# EJERCICIO PRÁCTICO 2: CATEGORIZACIÓN DE CLIENTES
# ============================================================================

# Crea una función que categorice clientes según su gasto total
# - Menos de $1000: "Bronce"
# - $1000 a $5000: "Plata"
# - $5000 a $10000: "Oro"
# - Más de $10000: "Platino"

categorizar_cliente <- function(gasto_total) {
  ifelse(gasto_total < 1000, "Bronce",
         ifelse(gasto_total < 5000, "Plata",
                ifelse(gasto_total < 10000, "Oro", "Platino")))
}

# Probar con diferentes clientes
gastos_clientes <- c(800, 2500, 7500, 15000, 950, 12000, 4200)
categorias <- categorizar_cliente(gastos_clientes)
categorias

# Crear data frame con resultados
df_clientes <- data.frame(
  cliente_id = 1:length(gastos_clientes),
  gasto_total = gastos_clientes,
  categoria = categorias
)
df_clientes

# ============================================================================
# EJERCICIOS PARA PRACTICAR TÚ
# ============================================================================

# 1. Crea una función que calcule el precio con IVA
#    Parámetros: precio, tasa_iva (por defecto 16%)

# TU CÓDIGO AQUÍ:




# 2. Crea una función que determine si un año es bisiesto
#    Regla: divisible por 4, excepto años divisibles por 100
#          (a menos que también sean divisibles por 400)

# TU CÓDIGO AQUÍ:




# 3. Usa lapply para calcular la suma de ventas de cada tienda
ventas_tiendas <- list(
  sucursal_norte = c(1200, 1500, 1300),
  sucursal_sur = c(1800, 1600, 1750),
  sucursal_este = c(1000, 1100, 950)
)

# TU CÓDIGO AQUÍ:




# ============================================================================
# ¡EXCELENTE TRABAJO!
# ============================================================================
# Ahora dominas:
# ✓ Funciones matemáticas y estadísticas
# ✓ Manipulación de texto
# ✓ Búsqueda y ordenamiento
# ✓ Condicionales y loops
# ✓ Familia apply
# ✓ Crear tus propias funciones
# ✓ Manejo de valores faltantes
#
# Continúa con: 04_importar_exportar.R
# ============================================================================
