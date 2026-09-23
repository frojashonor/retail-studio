# ==========================================================================
# Fundamentos de R y RStudio
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/01-fundamentos.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Bloque 1 --------------------------------------------------------
2 + 2

# ---- Bloque 2 --------------------------------------------------------
# Esto es un comentario: R no lo ejecuta
120 + 95 + 140   # Ventas de tres días (también puede ir al final)

# ---- Sección: cálculo de IVA -------------------------------------------
1500 * 0.16      # IVA de una venta de $1,500

# ---- Bloque 3 --------------------------------------------------------
getwd()   # ¿En qué carpeta está trabajando R?

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # Crea los archivos CSV y Excel de la carpeta datasets/ (solo una vez)
# source("datasets/generar_datasets.R")

# ---- Bloque 4 --------------------------------------------------------
file.exists("datasets/ventas_retail.csv")   # ¿Existe el archivo?
list.files("datasets")                      # ¿Qué hay en la carpeta?

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # Cambiar el directorio de trabajo a mano (NO recomendado)
# setwd("C:/Users/Ana/Documents/curso-bi-rstudio")        # Windows
# setwd("/Users/ana/Documents/curso-bi-rstudio")          # Mac

# ---- Bloque 5 --------------------------------------------------------
15000 + 18500      # Ventas de enero más febrero
485000 - 291000    # Ingresos menos costo de lo vendido
349 * 85           # Precio de un mouse por unidades vendidas
103700 / 7         # Venta promedio diaria de una semana
2^10               # Potencia: 2 elevado a la 10
125 %/% 12         # 125 piezas en cajas de 12: cajas completas
125 %% 12          # ... y piezas sueltas que sobran

# ---- Bloque 6 --------------------------------------------------------
# Precio de $1,000 más 16% de IVA
1000 + 1000 * 0.16      # Correcto: primero el IVA, luego la suma
(1000 + 1000) * 0.16    # Incorrecto: suma primero y luego multiplica
1000 * (1 + 0.16)       # Forma compacta y correcta

# ---- Bloque 7 --------------------------------------------------------
round(37.77778, 1)     # Redondear a 1 decimal
round(16149.15)        # Sin segundo argumento redondea a entero
ceiling(125 / 12)      # Redondear hacia arriba: cajas necesarias
floor(9.99)            # Redondear hacia abajo
sqrt(144)              # Raíz cuadrada
abs(95000 - 102000)    # Valor absoluto: error del pronóstico
log(100)               # Logaritmo natural
log10(1000)            # Logaritmo base 10
exp(1)                 # El número e

# ---- Bloque 8 --------------------------------------------------------
1250 * 0.16            # IVA de un producto de $1,250
1250 * 1.16            # Total que paga el cliente
2320 / 1.16            # Precio sin IVA de un ticket de $2,320
2320 - 2320 / 1.16     # IVA incluido en ese ticket
18999 * (1 - 0.15)     # Laptop con 15% de descuento

# ---- Bloque 9 --------------------------------------------------------
50000 * (1 + 0.10)^3           # Capital después de 3 años al 10%
50000 * (1 + 0.12 / 12)^36     # 12% anual capitalizable cada mes
(1800000 / 1200000)^(1 / 3) - 1  # Crecimiento anual compuesto (TCAC)
log(2) / log(1.08)             # Años para duplicar ventas creciendo 8%

# ---- Bloque 10 --------------------------------------------------------
450 - 280                        # Ganancia por unidad
(450 - 280) / 450 * 100          # Margen sobre el precio (%)
(450 - 280) / 280 * 100          # Markup sobre el costo (%)
sqrt(2 * 12000 * 500 / 12)       # Cantidad económica de pedido (EOQ)

# ---- Bloque 11 --------------------------------------------------------
ventas_enero <- 15000      # ventas_enero "recibe" 15000
ventas_febrero <- 18500
ventas_marzo <- 22000

# ---- Bloque 12 --------------------------------------------------------
ventas_enero                 # Escribir el nombre muestra el valor
print(ventas_febrero)        # print() hace lo mismo de forma explícita
total_trimestre <- ventas_enero + ventas_febrero + ventas_marzo
total_trimestre
promedio_mensual <- total_trimestre / 3
promedio_mensual

# ---- Bloque 13 --------------------------------------------------------
caja <- 5000             # Fondo de caja al abrir la tienda
caja <- caja + 1850      # Primera venta del día
caja <- caja + 920       # Segunda venta
caja <- caja - 300       # Pago a un proveedor en efectivo
caja

# ---- Bloque 14 --------------------------------------------------------
ls(pattern = "ventas")    # Objetos cuyo nombre contiene "ventas"
rm(caja)                  # Borra la variable caja
exists("caja")            # ¿Todavía existe?

# ---- Solo referencia (no se ejecuta automáticamente) ----
# rm(list = ls())   # Borra TODOS los objetos del entorno (¡cuidado!)

# ---- Bloque 15 --------------------------------------------------------
# ---- Datos de entrada ----------------------------------------------------
ingresos <- 485000
costo_ventas <- 291000
gastos_operacion <- 98500
meta_ventas <- 520000

# ---- KPIs ----------------------------------------------------------------
utilidad_bruta <- ingresos - costo_ventas
utilidad_operacion <- utilidad_bruta - gastos_operacion
margen_bruto_pct <- utilidad_bruta / ingresos * 100
margen_operacion_pct <- utilidad_operacion / ingresos * 100
cumplimiento_pct <- ingresos / meta_ventas * 100
faltante_meta <- meta_ventas - ingresos

# ---- Resultados ----------------------------------------------------------
utilidad_bruta
utilidad_operacion
round(margen_bruto_pct, 1)
round(margen_operacion_pct, 1)
round(cumplimiento_pct, 1)
faltante_meta

# ---- Bloque 16 --------------------------------------------------------
precio <- 349.90                 # numeric: número con decimales
unidades <- 12                   # también numeric, aunque sea entero
tickets <- 42L                   # integer: la L fuerza un entero
sucursal <- "Sucursal Centro"    # character: texto entre comillas
es_premium <- TRUE               # logical: verdadero o falso

class(precio)
class(unidades)
class(tickets)
class(sucursal)
class(es_premium)

# ---- Bloque 17 --------------------------------------------------------
typeof(precio)      # "double": número de doble precisión
typeof(tickets)

# ---- Bloque 18 --------------------------------------------------------
ventas_hoy <- 15600
meta_diaria <- 14000
ventas_hoy >= meta_diaria          # ¿Cumplimos la meta de hoy?
sucursal == "Sucursal Norte"       # ¿Es la sucursal Norte?
TRUE + TRUE + FALSE                # TRUE vale 1 y FALSE vale 0

# ---- Bloque 19 --------------------------------------------------------
is.numeric(precio)             # ¿Es número?
is.character(precio)           # ¿Es texto?
as.character(2023)             # Número -> texto
as.numeric("349.90")           # Texto -> número
as.integer(3.9)                # ¡Cuidado! Trunca, no redondea
as.logical(c("TRUE", "FALSE", "T", "sí"))

# ---- Solo referencia (no se ejecuta automáticamente) ----
# as.numeric("1,234")            # La coma de miles confunde a R

# ---- Bloque 20 --------------------------------------------------------
as.numeric(gsub(",", "", "1,234"))         # Quita la coma y convierte
monto_texto <- "$12,850.50"
as.numeric(gsub("[$,]", "", monto_texto))  # Quita $ y comas

# ---- Solo referencia (no se ejecuta automáticamente) ----
# precio_texto <- "100"
# precio_texto + 1

# ---- Bloque 21 --------------------------------------------------------
tallas <- c("M", "G", "CH", "M", "M", "G", "CH", "EG")
tallas_f <- factor(tallas)            # Niveles en orden alfabético
tallas_f
levels(tallas_f)
table(tallas_f)                       # Conteo por categoría

# ---- Bloque 22 --------------------------------------------------------
tallas_f <- factor(tallas, levels = c("CH", "M", "G", "EG"))
table(tallas_f)                       # Ahora en el orden correcto

# ---- Bloque 23 --------------------------------------------------------
precios_f <- factor(c("150", "80", "150", "200"))
as.numeric(precios_f)                 # ¡MAL! Devuelve los códigos
as.numeric(as.character(precios_f))   # BIEN: primero a texto

# ---- Bloque 24 --------------------------------------------------------
fecha_pedido <- as.Date("2023-03-15")     # Formato ISO: AAAA-MM-DD
fecha_pedido
class(fecha_pedido)
typeof(fecha_pedido)                      # Por dentro es un número
as.numeric(fecha_pedido)                  # Días desde el 1-ene-1970

# ---- Bloque 25 --------------------------------------------------------
fecha_factura <- as.Date("28/02/2023", format = "%d/%m/%Y")
fecha_factura
vencimiento <- fecha_factura + 30          # Crédito a 30 días
vencimiento
fecha_pago <- as.Date("2023-04-10")
fecha_pago - vencimiento                   # ¿Cuántos días de atraso?
as.numeric(fecha_pago - vencimiento)       # Solo el número

# ---- Bloque 26 --------------------------------------------------------
hoy <- Sys.Date()                  # Fecha de hoy según tu computadora
dias_desde_pedido <- as.numeric(hoy - fecha_pedido)

# ---- Bloque 27 --------------------------------------------------------
format(fecha_pedido, "%d/%m/%Y")           # Formato mexicano
format(fecha_pedido, "%A %d de %B de %Y")  # Formato largo
format(fecha_pedido, "%Y-%m")              # Año-mes para agrupar
weekdays(fecha_pedido)                     # Día de la semana
months(fecha_pedido)                       # Nombre del mes

# ---- Bloque 28 --------------------------------------------------------
pedidos <- as.Date(c("2023-01-05", "2023-01-19", "2023-02-09",
                     "2023-02-23", "2023-03-18"))
diff(pedidos)                          # Días entre pedidos consecutivos
dias_promedio <- as.numeric(mean(diff(pedidos)))
dias_promedio
proximo_pedido <- max(pedidos) + dias_promedio
proximo_pedido
format(proximo_pedido, "%A %d/%m/%Y")

# ---- Bloque 29 --------------------------------------------------------
ventas_semana <- c(12500, 9800, 11200, 13400, 18900, 22300, 15600)
dias <- c("lunes", "martes", "miércoles", "jueves", "viernes",
          "sábado", "domingo")
ventas_semana
dias
length(ventas_semana)      # ¿Cuántos elementos tiene?

# ---- Bloque 30 --------------------------------------------------------
c(1500, "2300", 980)       # Un solo texto "contamina" el vector

# ---- Bloque 31 --------------------------------------------------------
1:12                                   # Del 1 al 12 (números de mes)
seq(0, 100000, by = 25000)             # De 0 a 100 mil, de 25 en 25 mil
seq(10, 100, length.out = 4)           # 4 valores equiespaciados
rep("Norte", times = 3)                # Repetir un valor
rep(c("Q1", "Q2"), times = 2)          # Repetir el vector completo
rep(c("Q1", "Q2"), each = 2)           # Repetir cada elemento
seq(as.Date("2023-01-01"), by = "month", length.out = 4)  # Fechas

# ---- Bloque 32 --------------------------------------------------------
precios <- c(18999, 349, 1299, 4599, 899, 2499, 159)
precios * 0.85                    # 15% de descuento a los 7 de golpe
round(precios * 0.85 * 1.16, 2)   # Precio con descuento e IVA
unidades <- c(3, 40, 12, 5, 25, 8, 60)
ingresos <- precios * 0.85 * unidades   # Elemento por elemento
ingresos
sum(ingresos)                     # Ingreso total esperado

# ---- Bloque 33 --------------------------------------------------------
c(1000, 2000, 3000, 4000) * c(1, 0.5)   # 1, 0.5, 1, 0.5

# ---- Solo referencia (no se ejecuta automáticamente) ----
# c(1000, 2000, 3000) * c(1, 0.5)         # No es múltiplo: advertencia

# ---- Bloque 34 --------------------------------------------------------
names(ventas_semana) <- dias      # Cada venta con su día
ventas_semana
sum(ventas_semana)                # Total de la semana
mean(ventas_semana)               # Promedio diario
median(ventas_semana)             # Valor central
sd(ventas_semana)                 # Desviación estándar
min(ventas_semana)
max(ventas_semana)
range(ventas_semana)              # Mínimo y máximo juntos

# ---- Bloque 35 --------------------------------------------------------
quantile(ventas_semana)                    # Cuartiles
quantile(ventas_semana, probs = 0.9)       # Percentil 90
cumsum(ventas_semana)                      # Acumulado de la semana
diff(ventas_semana)                        # Cambio respecto al día previo
summary(ventas_semana)                     # Resumen en una línea

# ---- Bloque 36 --------------------------------------------------------
ventas_semana[1]              # El primer elemento (R empieza en 1)
ventas_semana[c(5, 6)]        # Posiciones 5 y 6
ventas_semana[1:5]            # De la 1 a la 5: días entre semana
ventas_semana[-7]             # Todos menos el séptimo

# ---- Bloque 37 --------------------------------------------------------
ventas_semana["sábado"]
ventas_semana[c("sábado", "domingo")]

# ---- Bloque 38 --------------------------------------------------------
meta_diaria <- 14000
ventas_semana > meta_diaria                  # Vector lógico
ventas_semana[ventas_semana > meta_diaria]   # Solo los días que cumplen

# ---- Bloque 39 --------------------------------------------------------
which(ventas_semana > meta_diaria)   # Posiciones que cumplen
which.max(ventas_semana)             # Posición del mejor día
names(which.max(ventas_semana))      # Solo el nombre del mejor día
names(which.min(ventas_semana))      # Nombre del peor día

# ---- Bloque 40 --------------------------------------------------------
fin_de_semana <- names(ventas_semana) %in% c("sábado", "domingo")
fin_de_semana
ventas_semana[fin_de_semana]                      # Sábado y domingo
ventas_semana[!fin_de_semana & ventas_semana > 12000]  # Entre semana
ventas_semana[ventas_semana < 10000 | ventas_semana > 20000]  # Extremos

# ---- Bloque 41 --------------------------------------------------------
sum(ventas_semana > meta_diaria)       # ¿Cuántos días cumplieron?
mean(ventas_semana > meta_diaria)      # ¿Qué proporción de días?

# ---- Bloque 42 --------------------------------------------------------
ventas_tienda <- c(8200, 7650, NA, 9100, 10400, NA, 8800)
mean(ventas_tienda)                   # NA: falta información
mean(ventas_tienda, na.rm = TRUE)     # Ignora los NA
sum(ventas_tienda, na.rm = TRUE)
is.na(ventas_tienda)                  # ¿Dónde hay NA?
sum(is.na(ventas_tienda))             # ¿Cuántos NA hay?
which(is.na(ventas_tienda))           # ¿En qué posiciones?

# ---- Bloque 43 --------------------------------------------------------
# Mejor día y su venta
names(which.max(ventas_semana))
max(ventas_semana)

# Días que superaron la meta
names(ventas_semana)[ventas_semana > meta_diaria]

# Crecimiento porcentual respecto al día anterior
crecimiento_pct <- diff(ventas_semana) / head(ventas_semana, -1) * 100
round(crecimiento_pct, 1)

# Los tres mejores días (ordenados de mayor a menor)
sort(ventas_semana, decreasing = TRUE)[1:3]

# ---- Bloque 44 --------------------------------------------------------
paste("Sucursal", "Norte")               # Une con un espacio
paste("Q", 1:4, sep = "")                # sep = "" no deja espacio
paste0("SKU-", 1:3)                      # paste0() = paste(sep = "")
paste("ventas", c("enero", "febrero"), "2023", sep = "_")
paste(c("CDMX", "Monterrey", "Puebla"), collapse = ", ")

# ---- Bloque 45 --------------------------------------------------------
ventas_norte <- 468900.5
cumplimiento <- 97.6875
sprintf("La sucursal Norte vendió $%.2f", ventas_norte)
sprintf("Cumplimiento de meta: %.1f%%", cumplimiento)
sprintf("%s: %d tickets", c("Centro", "Norte"), c(1520L, 1204L))
sprintf("SKU-%04d", c(7, 42, 315))       # Rellena con ceros a 4 dígitos

# ---- Bloque 46 --------------------------------------------------------
monto <- 1234567.891
format(monto, big.mark = ",")                  # Separador de miles
format(monto, big.mark = ",", nsmall = 2)      # Con 2 decimales
montos <- c(513916.6, 1407.99, 54056.19)
format(montos, big.mark = ",", nsmall = 2)     # Rellena con espacios
format(montos, big.mark = ",", nsmall = 2, trim = TRUE)

# ---- Bloque 47 --------------------------------------------------------
paste0("$", format(round(monto, 2), big.mark = ",", nsmall = 2))
paste0("$", format(round(montos, 2), big.mark = ",", nsmall = 2,
                   trim = TRUE))

# ---- Bloque 48 --------------------------------------------------------
scales::dollar(monto)                        # Redondea a pesos
scales::dollar(monto, accuracy = 0.01)       # Con centavos
scales::dollar(montos)
scales::dollar(15000, suffix = " MXN")       # Agrega un sufijo
scales::percent(0.9327, accuracy = 0.1)      # Proporción -> porcentaje
scales::comma(1234567)                       # Solo separador de miles

# ---- Bloque 49 --------------------------------------------------------
producto <- "  Laptop HP Pavilion 15  "
toupper(producto)                  # Todo a mayúsculas
tolower("SUCURSAL CENTRO")         # Todo a minúsculas
trimws(producto)                   # Quita espacios al inicio y al final
nchar(trimws(producto))            # Número de caracteres

sku <- "ELEC-2023-0153"            # Categoría-año-consecutivo
substr(sku, 1, 4)                  # Caracteres del 1 al 4
substr(sku, 6, 9)                  # Caracteres del 6 al 9
gsub("-", "", sku)                 # Reemplaza "-" por nada
gsub("Sucursal ", "", c("Sucursal Centro", "Sucursal Norte"))
grepl("Premium", c("Sucursal Premium", "Sucursal Outlet"))

# ---- Bloque 50 --------------------------------------------------------
ciudades <- c("CDMX", " cdmx", "Cdmx ", "Monterrey", "monterrey ",
              "Puebla")
table(ciudades)                              # Seis "ciudades" distintas
ciudades_limpias <- toupper(trimws(ciudades))
table(ciudades_limpias)                      # Tres ciudades reales

# ---- Bloque 51 --------------------------------------------------------
productos <- data.frame(
  nombre = c("Laptop Pro 14", "Mouse inalámbrico", "Monitor 27",
             "Silla ergonómica", "Escritorio", "Playera básica",
             "Chamarra", "Tenis running"),
  categoria = c("Electrónica", "Electrónica", "Electrónica", "Hogar",
                "Hogar", "Ropa", "Ropa", "Ropa"),
  precio = c(18999, 349, 5499, 3899, 4299, 199, 1299, 1899),
  costo = c(14250, 120, 4100, 2300, 2950, 85, 690, 1050),
  unidades = c(12, 85, 20, 15, 9, 240, 38, 44)
)
productos

# ---- Bloque 52 --------------------------------------------------------
head(productos, 3)        # Primeras 3 filas (por defecto, 6)
nrow(productos)           # Número de filas
ncol(productos)           # Número de columnas
dim(productos)            # Filas y columnas
names(productos)          # Nombres de las columnas

# ---- Bloque 53 --------------------------------------------------------
str(productos)            # Estructura: tipo de cada columna
summary(productos[, c("precio", "costo", "unidades")])

# ---- Solo referencia (no se ejecuta automáticamente) ----
# View(productos)     # Abre la tabla en una pestaña, como en Excel

# ---- Bloque 54 --------------------------------------------------------
productos$precio                        # Una columna (vector)
mean(productos$precio)                  # ... y con ella calculas
productos[2, ]                          # Fila 2, todas las columnas
productos[2, "precio"]                  # Una celda: fila 2, col. precio
productos[1:3, c("nombre", "precio")]   # Filas 1 a 3, dos columnas

# ---- Bloque 55 --------------------------------------------------------
# Productos con precio mayor a $3,000
productos[productos$precio > 3000, ]

# Ropa con más de 40 unidades vendidas (dos condiciones con &)
productos[productos$categoria == "Ropa" & productos$unidades > 40, ]

# Hogar o Ropa, mostrando solo nombre y precio
productos[productos$categoria %in% c("Hogar", "Ropa"),
          c("nombre", "precio")]

# ---- Bloque 56 --------------------------------------------------------
subset(productos, precio < 1000, select = c(nombre, precio, unidades))

# ---- Bloque 57 --------------------------------------------------------
# De menor a mayor precio
productos[order(productos$precio), c("nombre", "precio")]

# Los 3 productos más caros (de mayor a menor)
caros <- productos[order(productos$precio, decreasing = TRUE), ]
caros[1:3, c("nombre", "categoria", "precio")]

# ---- Bloque 58 --------------------------------------------------------
productos$ingreso <- productos$precio * productos$unidades
productos$utilidad <- (productos$precio - productos$costo) *
  productos$unidades
productos$margen_pct <- round((productos$precio - productos$costo) /
                                productos$precio * 100, 1)
productos[, c("nombre", "ingreso", "utilidad", "margen_pct")]

# ---- Bloque 59 --------------------------------------------------------
table(productos$categoria)                       # Conteo por grupo
tapply(productos$ingreso, productos$categoria, sum)   # Suma por grupo
aggregate(ingreso ~ categoria, data = productos, FUN = sum)

# ---- Bloque 60 --------------------------------------------------------
resumen_categoria <- aggregate(cbind(ingreso, utilidad) ~ categoria,
                               data = productos, FUN = sum)
resumen_categoria$margen_pct <- round(resumen_categoria$utilidad /
                                        resumen_categoria$ingreso * 100, 1)
resumen_categoria[order(resumen_categoria$margen_pct,
                        decreasing = TRUE), ]

# ---- Bloque 61 --------------------------------------------------------
productos_tb <- tibble::tibble(
  nombre = c("Laptop Pro 14", "Mouse inalámbrico", "Monitor 27"),
  precio = c(18999, 349, 5499),
  unidades = c(12, 85, 20),
  ingreso = precio * unidades     # Usa columnas recién creadas
)
productos_tb
tibble::as_tibble(productos)      # Convertir un data frame a tibble

# ---- Bloque 62 --------------------------------------------------------
ventas_mat <- matrix(
  c(150, 200, 180, 220,     # Laptop
    430, 390, 510, 480,     # Mouse
    210, 240, 190, 260),    # Monitor
  nrow = 3, byrow = TRUE,   # 3 filas, llenando renglón por renglón
  dimnames = list(c("Laptop", "Mouse", "Monitor"),     # Filas
                  c("ene", "feb", "mar", "abr"))       # Columnas
)
ventas_mat
ventas_mat["Mouse", "mar"]      # Una celda: [fila, columna]
ventas_mat[, "abr"]             # Una columna completa
rowSums(ventas_mat)             # Total por producto
colSums(ventas_mat)             # Total por mes
round(rowMeans(ventas_mat), 1)  # Promedio mensual por producto
ventas_mat * 1.10               # Pronóstico: +10% en todas las celdas

# ---- Bloque 63 --------------------------------------------------------
cliente <- list(
  id = 1045,
  nombre = "María González",
  ciudad = "Guadalajara",
  premium = TRUE,
  fecha_alta = as.Date("2021-06-14"),
  compras = c(1250.00, 830.50, 2410.00, 675.25)
)
str(cliente)                   # Estructura de la lista
cliente$nombre                 # Un elemento por nombre
cliente[["compras"]]           # Otra forma: doble corchete
sum(cliente$compras)           # Total comprado
mean(cliente$compras)          # Ticket promedio
cliente$canal <- "Tienda en línea"   # Agregar un elemento nuevo
names(cliente)

# ---- Bloque 64 --------------------------------------------------------
monto_venta <- 3250

if (monto_venta >= 5000) {
  tipo_venta <- "Grande"
} else if (monto_venta >= 1000) {
  tipo_venta <- "Mediana"
} else {
  tipo_venta <- "Chica"
}
tipo_venta

# ---- Bloque 65 --------------------------------------------------------
ventas_semana
etiqueta_meta <- ifelse(ventas_semana >= meta_diaria,
                        "Cumplió", "No cumplió")
etiqueta_meta

# Anidar ifelse() para tres categorías
nivel_dia <- ifelse(ventas_semana >= 18000, "Alto",
                    ifelse(ventas_semana >= 12000, "Medio", "Bajo"))
table(nivel_dia)

# ---- Bloque 66 --------------------------------------------------------
sucursales <- c("Centro", "Norte", "Sur", "Plaza A", "Outlet")
ventas_mes <- c(485000, 468900, 391200, 552300, 298700)
metas_mes <- c(520000, 450000, 420000, 540000, 280000)

cat("REPORTE DE CUMPLIMIENTO - MARZO 2023\n")
for (k in seq_along(sucursales)) {
  cumpl <- ventas_mes[k] / metas_mes[k] * 100
  alerta <- if (cumpl < 95) "  <- REVISAR" else ""
  cat(sprintf("%-8s $%9s  %6.1f%%%s\n", sucursales[k],
              format(ventas_mes[k], big.mark = ","), cumpl, alerta))
}

# ---- Bloque 67 --------------------------------------------------------
venta_actual <- 350000
crecimiento_mensual <- 0.03
meta_mensual <- 450000
meses <- 0

while (venta_actual < meta_mensual) {
  venta_actual <- venta_actual * (1 + crecimiento_mensual)
  meses <- meses + 1
}
meses
round(venta_actual, 2)

# ---- Bloque 68 --------------------------------------------------------
round(ventas_mes / metas_mes * 100, 1)       # Cumplimiento de las 5
sucursales[ventas_mes / metas_mes < 0.95]    # Las que hay que revisar

# ---- Bloque 69 --------------------------------------------------------
ventas_por_tienda <- list(
  Centro = c(15200, 13900, 16800),
  Norte = c(12100, 12900),
  Outlet = c(8400, 9100, 7600, 9900)
)
sapply(ventas_por_tienda, sum)                  # Total de cada tienda
sapply(ventas_por_tienda, length)               # Días con datos
vapply(ventas_por_tienda, mean, numeric(1))     # Promedio (seguro)

# ---- Bloque 70 --------------------------------------------------------
# Calcula el margen de utilidad de uno o varios productos.
# Argumentos:
#   precio: precio de venta (número o vector)
#   costo:  costo unitario (número o vector)
#   en_pct: TRUE devuelve porcentaje (0-100); FALSE, proporción (0-1)
# Devuelve: el margen, redondeado a 1 decimal si es porcentaje
calcular_margen <- function(precio, costo, en_pct = TRUE) {
  margen <- (precio - costo) / precio
  if (en_pct) {
    return(round(margen * 100, 1))
  }
  margen
}

# ---- Bloque 71 --------------------------------------------------------
calcular_margen(450, 280)                        # Posicional
calcular_margen(precio = 450, costo = 280, en_pct = FALSE)
calcular_margen(productos$precio, productos$costo)  # Vectorizada

# ---- Bloque 72 --------------------------------------------------------
# ROI (retorno sobre la inversión) en porcentaje
calcular_roi <- function(ganancia, inversion) {
  if (!is.numeric(ganancia) || !is.numeric(inversion)) {
    stop("ganancia e inversion deben ser números")
  }
  if (any(inversion <= 0)) {
    stop("La inversión debe ser mayor que cero")
  }
  (ganancia - inversion) / inversion * 100
}

# Variación porcentual entre un periodo y el anterior
variacion_porcentual <- function(actual, anterior) {
  if (any(anterior == 0)) {
    warning("Hay periodos anteriores en 0; su variación queda como NA")
  }
  resultado <- (actual - anterior) / anterior * 100
  resultado[anterior == 0] <- NA
  round(resultado, 1)
}

# ---- Solo referencia (no se ejecuta automáticamente) ----
# calcular_roi(5000, 0)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# variacion_porcentual(c(120, 50), c(100, 0))

# ---- Bloque 73 --------------------------------------------------------
# Segmenta clientes por gasto anual:
# Bronce < $5,000 <= Plata < $20,000 <= Oro < $50,000 <= Platino
clasificar_cliente <- function(gasto_anual,
                               cortes = c(5000, 20000, 50000)) {
  if (any(gasto_anual < 0, na.rm = TRUE)) {
    stop("El gasto no puede ser negativo")
  }
  ifelse(gasto_anual >= cortes[3], "Platino",
         ifelse(gasto_anual >= cortes[2], "Oro",
                ifelse(gasto_anual >= cortes[1], "Plata", "Bronce")))
}

# ---- Bloque 74 --------------------------------------------------------
calcular_roi(ganancia = 116000, inversion = 80000)

ventas_trim <- c(Q1 = 126274, Q2 = 120717, Q3 = 134427, Q4 = 132498)
variacion_porcentual(ventas_trim[-1], ventas_trim[-4])

gastos <- c(3200, 18500, 52000, 7800, 25000, 950)
segmentos <- clasificar_cliente(gastos)
segmentos
table(factor(segmentos, levels = c("Bronce", "Plata", "Oro",
                                    "Platino")))

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # Instalar (solo una vez; necesita internet). Nota las comillas.
# install.packages("readxl")
# install.packages(c("tidyverse", "writexl", "scales"))  # Varios a la vez

# ---- Bloque 75 --------------------------------------------------------
library(readxl)                        # Cargar (en cada sesión)
library(writexl)
"readxl" %in% rownames(installed.packages())  # ¿Está instalado?
as.character(packageVersion("readxl"))  # ¿Qué versión tengo?

# ---- Bloque 76 --------------------------------------------------------
scales::dollar(513916.6)      # Sin library(scales)

# ---- Bloque 77 --------------------------------------------------------
library(tidyverse)

# ---- Bloque 78 --------------------------------------------------------
ventas_base <- read.csv("datasets/ventas_retail.csv")
dim(ventas_base)
head(ventas_base[, c("fecha", "dia_semana", "cantidad", "total")], 3)
class(ventas_base$fecha)

# ---- Bloque 79 --------------------------------------------------------
ventas <- read_csv("datasets/ventas_retail.csv")

# ---- Bloque 80 --------------------------------------------------------
ventas <- read_csv("datasets/ventas_retail.csv", show_col_types = FALSE)
class(ventas$fecha)
ventas

# ---- Bloque 81 --------------------------------------------------------
excel_sheets("datasets/datos_empresa.xlsx")      # ¿Qué hojas tiene?
tiendas <- read_excel("datasets/datos_empresa.xlsx", sheet = "Tiendas")
tiendas[, c("nombre_tienda", "ciudad", "tamano_m2", "empleados")]
productos_xl <- read_excel("datasets/datos_empresa.xlsx", sheet = 3)
dim(productos_xl)

# ---- Bloque 82 --------------------------------------------------------
dir.create("resultados", showWarnings = FALSE)

# Un resumen pequeño para exportar: ventas por trimestre
por_trimestre <- aggregate(total ~ trimestre, data = ventas_base, FUN = sum)
por_trimestre$total <- round(por_trimestre$total, 2)
por_trimestre

# CSV con R base (row.names = FALSE evita una columna extra de números)
write.csv(por_trimestre, "resultados/ventas_trimestre.csv",
          row.names = FALSE)
# CSV con readr (nunca escribe nombres de fila)
write_csv(por_trimestre, "resultados/ventas_trimestre_readr.csv")

# Excel con varias hojas: una lista con nombre = una hoja por elemento
write_xlsx(list(Trimestres = por_trimestre,
                Tiendas = tiendas,
                Catalogo = productos),
           "resultados/reporte_modulo1.xlsx")
excel_sheets("resultados/reporte_modulo1.xlsx")

# ---- Bloque 83 --------------------------------------------------------
saveRDS(ventas, "resultados/ventas_2023.rds")        # Guardar
ventas_copia <- readRDS("resultados/ventas_2023.rds") # Leer
class(ventas_copia$fecha)                             # La fecha sigue
all.equal(ventas, ventas_copia)                       # ¿Son iguales?
file.exists(c("resultados/ventas_trimestre.csv",      # ¿Se crearon
              "resultados/reporte_modulo1.xlsx",      #  los archivos?
              "resultados/ventas_2023.rds"))

# ---- Bloque 84 --------------------------------------------------------
dir.create("datos", showWarnings = FALSE)
writeLines(c("producto;precio;unidades",
             "Café molido;145,50;12",
             "Té verde;89,90;30",
             "Azúcar;32,00;45"),
           "datos/proveedor.csv")
read.csv("datos/proveedor.csv")          # ¡Mal! Todo en una columna

# ---- Bloque 85 --------------------------------------------------------
read.csv2("datos/proveedor.csv")                           # R base
read_csv2("datos/proveedor.csv", show_col_types = FALSE)   # readr

# ---- Bloque 86 --------------------------------------------------------
read_delim("datos/proveedor.csv", delim = ";",
           locale = locale(decimal_mark = ","), show_col_types = FALSE)

# ---- Bloque 87 --------------------------------------------------------
# Simulamos un archivo exportado por un sistema antiguo (latin1)
con <- file("datos/sistema_viejo.csv", open = "w", encoding = "latin1")
writeLines(c("producto,precio", "Café molido,145.5", "Té verde,89.9"), con)
close(con)

read_csv("datos/sistema_viejo.csv", show_col_types = FALSE)
read_csv("datos/sistema_viejo.csv", show_col_types = FALSE,
         locale = locale(encoding = "latin1"))

# ---- Solo referencia (no se ejecuta automáticamente) ----
# ruta <- "C:\Users\ana\ventas.csv"

# ---- Bloque 88 --------------------------------------------------------
"C:/Users/ana/ventas.csv"              # Diagonal normal: funciona
"C:\\Users\\ana\\ventas.csv"           # Diagonal doble: también
file.path("datasets", "ventas_retail.csv")   # Ruta relativa armada

# ---- Solo referencia (no se ejecuta automáticamente) ----
# ?mean                    # Ayuda de una función (abre el panel Help)
# help("read_csv")         # Lo mismo, con el nombre como texto
# ??"moving average"       # Busca un tema en TODA la documentación
# example(mean)            # Ejecuta los ejemplos de la ayuda
# vignette("readxl")       # Tutoriales largos de un paquete (viñetas)

# ---- Bloque 89 --------------------------------------------------------
args(round)            # ¿Qué argumentos acepta round()?
apropos("mean")        # Funciones cuyo nombre contiene "mean"

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # Pregunta: ¿por qué mean() me da NA si mi columna tiene números?
# ventas <- data.frame(tienda = c("A", "B", "C"),
#                      monto = c(1500, NA, 2300))
# mean(ventas$monto)
# #> [1] NA
# # Esperaba 1900. Uso R 4.3.3 en Windows 11.

# ---- Bloque 90 --------------------------------------------------------
dput(productos[1:3, c("nombre", "precio")])

# ---- Bloque 91 --------------------------------------------------------
# ---- 1. Cargar -----------------------------------------------------------
ventas_2023 <- read.csv("datasets/ventas_retail.csv")
ventas_2023$fecha <- as.Date(ventas_2023$fecha)   # Texto -> fecha
str(ventas_2023)

# ---- Bloque 92 --------------------------------------------------------
range(ventas_2023$fecha)            # ¿Qué periodo cubre?
sum(is.na(ventas_2023))             # ¿Hay datos faltantes?
sum(duplicated(ventas_2023$fecha))  # ¿Fechas repetidas?

# ---- Bloque 93 --------------------------------------------------------
# ---- 2. KPIs del año -----------------------------------------------------
venta_total <- sum(ventas_2023$total)
ticket_promedio <- mean(ventas_2023$total)
unidades_totales <- sum(ventas_2023$cantidad)
descuentos_otorgados <- sum(ventas_2023$descuento_monto)

scales::dollar(venta_total)
scales::dollar(ticket_promedio, accuracy = 0.01)
unidades_totales
scales::dollar(descuentos_otorgados)
summary(ventas_2023$total)

# ---- Bloque 94 --------------------------------------------------------
# ---- 3. Por trimestre ----------------------------------------------------
venta_trimestre <- tapply(ventas_2023$total, ventas_2023$trimestre, sum)
round(venta_trimestre, 2)
round(venta_trimestre / venta_total * 100, 1)     # Participación %

# ---- Bloque 95 --------------------------------------------------------
# ---- 4. Por mes ----------------------------------------------------------
venta_mes <- tapply(ventas_2023$total, ventas_2023$mes, sum)
round(venta_mes)
names(which.max(venta_mes))
names(which.min(venta_mes))
round(max(venta_mes) / min(venta_mes), 2)   # ¿Cuántas veces más?

# ---- Bloque 96 --------------------------------------------------------
# ---- 5. Con y sin descuento ----------------------------------------------
ventas_2023$con_descuento <- ifelse(ventas_2023$descuento_pct > 0,
                                    "Con descuento", "Sin descuento")
table(ventas_2023$con_descuento)
round(tapply(ventas_2023$total, ventas_2023$con_descuento, mean), 2)
round(tapply(ventas_2023$cantidad, ventas_2023$con_descuento, mean), 2)

# ---- Bloque 97 --------------------------------------------------------
# ---- 6. Top 5 días -------------------------------------------------------
orden <- order(ventas_2023$total, decreasing = TRUE)
top5 <- ventas_2023[orden[1:5],
                    c("fecha", "dia_semana", "cantidad",
                      "precio_unitario", "descuento_pct", "total")]
top5

# ---- Bloque 98 --------------------------------------------------------
# ---- 7. Exportar ---------------------------------------------------------
kpis <- data.frame(
  indicador = c("Venta total", "Ticket promedio", "Unidades vendidas",
                "Descuentos otorgados", "Mejor mes", "Peor mes"),
  valor = c(scales::dollar(venta_total),
            scales::dollar(ticket_promedio, accuracy = 0.01),
            scales::comma(unidades_totales),
            scales::dollar(descuentos_otorgados),
            names(which.max(venta_mes)), names(which.min(venta_mes)))
)
tabla_trimestre <- data.frame(trimestre = names(venta_trimestre),
                              venta = round(as.numeric(venta_trimestre), 2))
tabla_mes <- data.frame(mes = names(venta_mes),
                        venta = round(as.numeric(venta_mes), 2))
tabla_descuento <- aggregate(total ~ con_descuento, data = ventas_2023,
                             FUN = mean)

dir.create("resultados", showWarnings = FALSE)
write_xlsx(list(KPIs = kpis, Trimestres = tabla_trimestre,
                Meses = tabla_mes, Descuentos = tabla_descuento,
                Top5 = top5),
           "resultados/resumen_ventas_2023.xlsx")
kpis
excel_sheets("resultados/resumen_ventas_2023.xlsx")

# ---- Solo referencia (no se ejecuta automáticamente) ----
# ventas_totales <- 1000
# ventas_total                     # Falta la "s"

# ---- Solo referencia (no se ejecuta automáticamente) ----
# # En una sesión nueva, sin haber cargado readr
# total <- read_csv("datasets/ventas_retail.csv")

# ---- Solo referencia (no se ejecuta automáticamente) ----
# ventas enero <- 15000

# ---- Solo referencia (no se ejecuta automáticamente) ----
# precio <- "100"
# precio + 1

# ---- Solo referencia (no se ejecuta automáticamente) ----
# ventas <- read.csv("datos/ventas_retail.csv")    # La carpeta es datasets

# ---- Solo referencia (no se ejecuta automáticamente) ----
# library(janitor)

# ---- Solo referencia (no se ejecuta automáticamente) ----
# mean(c("1500", "2300"))          # Números guardados como texto
