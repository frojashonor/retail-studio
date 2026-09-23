# ==========================================================================
# sol-01.tex
# Código del libro 'Business Intelligence con R y RStudio'
# Generado automáticamente a partir de capitulos/sol-01.tex
# Ejecuta este script con el directorio de trabajo en la carpeta
# del curso (la que contiene datasets/).
# ==========================================================================

# ---- Bloque 1 --------------------------------------------------------
# ---- Datos de la compra --------------------------------------------------
precio_audifonos <- 899
precio_teclado <- 1299
precio_usb <- 189
tasa_iva <- 0.16
cupon <- 0.10

# ---- Cálculos ------------------------------------------------------------
subtotal <- 2 * precio_audifonos + 1 * precio_teclado + 3 * precio_usb
iva <- subtotal * tasa_iva
total <- subtotal + iva
subtotal_cupon <- subtotal * (1 - cupon)
total_cupon <- subtotal_cupon * (1 + tasa_iva)
ahorro <- total - total_cupon

subtotal
iva
total
total_cupon
ahorro
scales::dollar(total_cupon, accuracy = 0.01)

# ---- Bloque 2 --------------------------------------------------------
ingresos <- 612400
costo_ventas <- 379700
gastos_fijos <- 158000
meta <- 580000

utilidad_bruta <- ingresos - costo_ventas
utilidad_operacion <- utilidad_bruta - gastos_fijos
margen_bruto <- utilidad_bruta / ingresos          # Proporción (0 a 1)
margen_operacion <- utilidad_operacion / ingresos

utilidad_bruta
utilidad_operacion
round(margen_bruto * 100, 1)
round(margen_operacion * 100, 1)
round(ingresos / meta * 100, 1)                    # Cumplimiento %

punto_equilibrio <- gastos_fijos / margen_bruto
round(punto_equilibrio, 2)
round(ingresos - punto_equilibrio, 2)              # Colchón en pesos
round((ingresos / punto_equilibrio - 1) * 100, 1)  # Colchón en %

# ---- Bloque 3 --------------------------------------------------------
montos_texto <- c("$1,250.50", "$980.00", "$12,400.75", "$3,105.20")
fechas_texto <- c("05/01/2023", "19/01/2023", "02/02/2023", "28/02/2023")

# (a) Montos a número
montos <- as.numeric(gsub("[$,]", "", montos_texto))
montos
sum(montos)

# (b) Fechas a Date
fechas <- as.Date(fechas_texto, format = "%d/%m/%Y")
fechas
class(fechas)

# (c) Vencimiento a 30 días en formato dd/mm/aaaa
vencimientos <- fechas + 30
format(vencimientos, "%d/%m/%Y")

# (d) Día de la semana del vencimiento
weekdays(vencimientos)

# ---- Bloque 4 --------------------------------------------------------
meses <- c("ene", "feb", "mar", "abr", "may", "jun",
           "jul", "ago", "sep", "oct", "nov", "dic")
ventas_mes <- c(420, 385, 455, 402, 478, 510, 495, 530, 468, 501, 560, 640)
names(ventas_mes) <- meses

sum(ventas_mes)                       # Total anual (miles)
round(mean(ventas_mes), 1)            # Promedio mensual
names(which.max(ventas_mes))          # Mejor mes
names(which.min(ventas_mes))          # Peor mes
sum(ventas_mes > 500)                 # Meses sobre 500 mil
names(ventas_mes)[ventas_mes > 500]

crecimiento <- diff(ventas_mes) / head(ventas_mes, -1) * 100
round(crecimiento, 1)

acumulado <- cumsum(ventas_mes)
acumulado
names(which(acumulado > 3000)[1])     # Primer mes que supera 3 millones

# ---- Bloque 5 --------------------------------------------------------
ventas_dias <- c(8200, 7650, NA, 9100, 10400, NA, 8800, 12100, NA, 9650)

# (a) Días sin dato
sum(is.na(ventas_dias))
which(is.na(ventas_dias))

# (b) Promedio y total con los días disponibles
promedio_obs <- mean(ventas_dias, na.rm = TRUE)
round(promedio_obs, 2)
sum(ventas_dias, na.rm = TRUE)

# (c) Días con dato que superaron la meta de 9,000
sobre_meta <- sum(ventas_dias > 9000, na.rm = TRUE)
sobre_meta
round(sobre_meta / sum(!is.na(ventas_dias)) * 100, 1)

# (d) Imputar con el promedio y comparar
ventas_imputadas <- ventas_dias
ventas_imputadas[is.na(ventas_imputadas)] <- promedio_obs
round(sum(ventas_imputadas), 2)
sum(ifelse(is.na(ventas_dias), 0, ventas_dias))   # Si imputáramos con 0

# ---- Bloque 6 --------------------------------------------------------
nombres <- c("  ana LÓPEZ", "Carlos pérez ", "MARÍA gonzález")
ciudades <- c("cdmx", "Monterrey ", " CDMX")
compras <- c(15230.5, 8900, 42100.75)

# (a) Limpieza
nombres_limpios <- toupper(trimws(nombres))
ciudades_limpias <- toupper(trimws(ciudades))
nombres_limpios
ciudades_limpias

# (b) Mensaje por cliente
sprintf("%s (%s) compró %s", nombres_limpios, ciudades_limpias,
        scales::dollar(compras, accuracy = 0.01))

# (c) Códigos de cliente
codigos <- sprintf("CLI-%03d", seq_along(nombres))
codigos

# (d) Clientes de CDMX
sum(ciudades_limpias == "CDMX")

# ---- Bloque 7 --------------------------------------------------------
empleados <- data.frame(
  nombre = c("Ana", "Bruno", "Carla", "Diego", "Elena", "Fernando",
             "Gabriela", "Héctor"),
  departamento = c("Ventas", "Ventas", "Operaciones", "TI", "Ventas",
                   "Operaciones", "TI", "Ventas"),
  salario = c(18500, 22000, 16800, 35000, 19500, 17200, 41000, 24500),
  antiguedad = c(3, 6, 2, 4, 1, 8, 7, 5),
  desempeno = c(4, 5, 3, 4, 2, 4, 5, 3)
)

# (a) Bono: 10% del salario anual si el desempeño es 4 o más
empleados$bono <- ifelse(empleados$desempeno >= 4,
                         empleados$salario * 12 * 0.10, 0)
empleados

# (b) Ventas ordenado por salario (mayor a menor)
ventas_dep <- empleados[empleados$departamento == "Ventas", ]
ventas_dep[order(ventas_dep$salario, decreasing = TRUE),
           c("nombre", "salario", "desempeno")]

# (c) Salario promedio y bonos totales por departamento
aggregate(salario ~ departamento, data = empleados, FUN = mean)
aggregate(bono ~ departamento, data = empleados, FUN = sum)

# (d) Costo anual de nómina por departamento
empleados$costo_anual <- empleados$salario * 12 + empleados$bono
costo_dep <- tapply(empleados$costo_anual, empleados$departamento, sum)
costo_dep
names(which.max(costo_dep))

# ---- Bloque 8 --------------------------------------------------------
vendedores <- c("Luis", "Marta", "Nora", "Oscar", "Paula", "Raul")
ventas_trim <- c(185, 240, 132, 310, 198, 260)
meta <- 200

# (a) Semáforo
cumplimiento <- ventas_trim / meta * 100
semaforo <- ifelse(cumplimiento >= 100, "Verde",
                   ifelse(cumplimiento >= 85, "Amarillo", "Rojo"))
semaforo

# (b) Reporte con for
for (k in seq_along(vendedores)) {
  cat(sprintf("%-6s %4d mil  %6.1f%%  %s\n", vendedores[k],
              ventas_trim[k], cumplimiento[k], semaforo[k]))
}

# (c) Trimestres para que el de menor venta alcance la meta
venta <- min(ventas_trim)
trimestres <- 0
while (venta < meta) {
  venta <- venta * 1.08
  trimestres <- trimestres + 1
}
vendedores[which.min(ventas_trim)]
trimestres
round(venta, 1)

# ---- Bloque 9 --------------------------------------------------------
# Precio final: aplica descuento (proporción) y luego IVA
precio_final <- function(precio, descuento = 0, iva = 0.16) {
  if (any(precio < 0)) stop("El precio no puede ser negativo")
  if (descuento < 0 || descuento > 1) {
    stop("El descuento debe estar entre 0 y 1 (0.15 = 15%)")
  }
  round(precio * (1 - descuento) * (1 + iva), 2)
}

# Unidades mínimas para no perder dinero
punto_equilibrio <- function(costos_fijos, precio, costo_variable) {
  if (precio <= costo_variable) {
    stop("El precio debe ser mayor que el costo variable")
  }
  ceiling(costos_fijos / (precio - costo_variable))
}

# (c) Uso
catalogo <- c(laptop = 18999, mouse = 349, monitor = 5499, silla = 3899)
precio_final(catalogo, descuento = 0.20)
precio_final(1000)                          # Solo IVA
punto_equilibrio(85000, precio = 450, costo_variable = 280)

# (d) ¿Qué pasa con un descuento inválido? tryCatch() atrapa el error
tryCatch(precio_final(1000, descuento = 1.5),
         error = function(e) conditionMessage(e))

# ---- Solo referencia (no se ejecuta automáticamente) ----
# precio_final(1000, descuento = 1.5)

# ---- Bloque 10 --------------------------------------------------------
library(readr)
library(writexl)

productos <- read_csv("datasets/productos.csv", show_col_types = FALSE)

# (a) Tamaño
dim(productos)

# (b) Margen negativo
negativos <- productos[productos$margen_pct < 0, ]
nrow(negativos)
peores <- negativos[order(negativos$margen_pct),
                    c("nombre_producto", "categoria", "precio_catalogo",
                      "costo", "margen_pct")]
head(peores, 5)

# (c) Margen promedio por categoría
margen_cat <- aggregate(margen_pct ~ categoria, data = productos,
                        FUN = mean)
margen_cat$margen_pct <- round(margen_cat$margen_pct, 1)
margen_cat[order(margen_cat$margen_pct, decreasing = TRUE), ]

# (d) Problemas de inventario
table(productos$estado_stock)
sum(productos$estado_stock %in% c("Stock Bajo", "Sin Stock"))

# (e) Exportar
dir.create("resultados", showWarnings = FALSE)
write_xlsx(list(Margen_negativo = as.data.frame(peores),
                Por_categoria = margen_cat),
           "resultados/calidad_catalogo.xlsx")

# ---- Bloque 11 --------------------------------------------------------
library(readr)

trans <- read_csv("datasets/transacciones.csv", show_col_types = FALSE)

# (a) Resumen por método de pago
resumen_pago <- aggregate(total_transaccion ~ metodo_pago, data = trans,
                          FUN = sum)
resumen_pago$transacciones <- as.numeric(table(trans$metodo_pago)[
  resumen_pago$metodo_pago])
resumen_pago$ticket_promedio <- round(resumen_pago$total_transaccion /
                                        resumen_pago$transacciones, 2)
resumen_pago

# (b) Porcentaje del monto pagado con tarjeta
con_tarjeta <- trans$metodo_pago %in% c("Tarjeta Crédito",
                                        "Tarjeta Débito")
round(sum(trans$total_transaccion[con_tarjeta]) /
        sum(trans$total_transaccion) * 100, 1)

# (c) Mejor mes y día con más transacciones
por_mes <- tapply(trans$total_transaccion, trans$mes, sum)
names(which.max(por_mes))
round(max(por_mes), 2)
sort(table(trans$dia_semana), decreasing = TRUE)[1:3]

# (d) Exportar y comprobar
dir.create("resultados", showWarnings = FALSE)
write_csv(resumen_pago, "resultados/resumen_metodo_pago.csv")
saveRDS(resumen_pago, "resultados/resumen_metodo_pago.rds")
identical(resumen_pago, readRDS("resultados/resumen_metodo_pago.rds"))

# ---- Bloque 12 --------------------------------------------------------
library(readr)
library(writexl)

ventas <- read_csv("datasets/ventas_retail.csv", show_col_types = FALSE)
tiendas <- read_csv("datasets/tiendas.csv", show_col_types = FALSE)

# Resume las ventas de una tienda en un data frame de una fila
resumen_tienda <- function(id, ventas, tiendas) {
  if (!id %in% tiendas$tienda_id) stop("La tienda no existe: ", id)
  v <- ventas[ventas$tienda_id == id, ]
  fila_tienda <- match(id, tiendas$tienda_id)
  venta_mes <- tapply(v$total, v$mes, sum)
  data.frame(
    tienda = tiendas$nombre_tienda[fila_tienda],
    ciudad = tiendas$ciudad[fila_tienda],
    num_ventas = nrow(v),
    venta_total = round(sum(v$total), 2),
    ticket_promedio = round(mean(v$total), 2),
    mejor_mes = names(which.max(venta_mes)),
    venta_m2 = round(sum(v$total) / tiendas$tamano_m2[fila_tienda], 2)
  )
}

resumen_tienda(1, ventas, tiendas)          # Prueba con una tienda

# Aplicar a las 10 tiendas y unir las filas
lista_resumenes <- lapply(tiendas$tienda_id, resumen_tienda,
                          ventas = ventas, tiendas = tiendas)
ranking <- do.call(rbind, lista_resumenes)
ranking <- ranking[order(ranking$venta_total, decreasing = TRUE), ]
rownames(ranking) <- NULL

# Ranking impreso
cat("RANKING DE TIENDAS 2023\n")
for (k in seq_len(nrow(ranking))) {
  cat(sprintf("%2d. %-17s %11s  %3d ventas  %8s por m2\n", k,
              ranking$tienda[k], scales::dollar(ranking$venta_total[k]),
              ranking$num_ventas[k], scales::dollar(ranking$venta_m2[k],
                                                     accuracy = 0.01)))
}

# Exportar
dir.create("resultados", showWarnings = FALSE)
write_xlsx(ranking, "resultados/ranking_tiendas.xlsx")
ranking$tienda[which.max(ranking$venta_m2)]
