# ============================================================================
# GENERADOR DE DATASETS PARA EL CURSO DE BI
# ============================================================================
# Este script genera todos los datasets de ejemplo usados en el curso
# Ejecuta este archivo UNA VEZ para crear todos los archivos de datos
# ============================================================================

# Cargar librerías necesarias
library(writexl)

# Carpeta de salida:
# - Si ejecutas el script desde la carpeta del curso (la que contiene
#   "datasets/"), los archivos se guardan dentro de datasets/.
# - Si lo ejecutas desde la propia carpeta datasets/, se guardan ahí mismo.
carpeta_salida <- if (dir.exists("datasets")) "datasets" else "."
ruta <- function(archivo) file.path(carpeta_salida, archivo)

# Configurar semilla para reproducibilidad
set.seed(123)

# ============================================================================
# 1. VENTAS RETAIL
# ============================================================================

# Generar datos de ventas de comercio minorista (1 año de datos)
ventas_retail <- data.frame(
  fecha = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"),
  dia_semana = weekdays(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")),
  producto_id = sample(1:50, 365, replace = TRUE),
  cliente_id = sample(1:200, 365, replace = TRUE),
  cantidad = sample(1:10, 365, replace = TRUE),
  precio_unitario = round(runif(365, 10, 500), 2),
  descuento_pct = sample(c(0, 5, 10, 15, 20), 365, replace = TRUE, prob = c(0.5, 0.2, 0.15, 0.1, 0.05)),
  tienda_id = sample(1:10, 365, replace = TRUE),
  vendedor_id = sample(1:25, 365, replace = TRUE)
)

# Calcular columnas derivadas
ventas_retail$subtotal <- ventas_retail$cantidad * ventas_retail$precio_unitario
ventas_retail$descuento_monto <- ventas_retail$subtotal * (ventas_retail$descuento_pct / 100)
ventas_retail$total <- ventas_retail$subtotal - ventas_retail$descuento_monto
ventas_retail$mes <- format(ventas_retail$fecha, "%Y-%m")
ventas_retail$trimestre <- paste0("Q", ceiling(as.numeric(format(ventas_retail$fecha, "%m")) / 3))

# Guardar
write.csv(ventas_retail, ruta("ventas_retail.csv"), row.names = FALSE)
print("✓ ventas_retail.csv creado")

# ============================================================================
# 2. CLIENTES
# ============================================================================

nombres <- c("María", "Juan", "Ana", "Carlos", "Laura", "Pedro", "Sofia", "Diego",
             "Carmen", "Luis", "Elena", "Miguel", "Isabel", "Jorge", "Patricia",
             "Fernando", "Rosa", "Alberto", "Marta", "Ricardo")

apellidos <- c("García", "Rodríguez", "Martínez", "López", "González", "Pérez",
               "Sánchez", "Ramírez", "Torres", "Flores", "Rivera", "Gómez",
               "Díaz", "Cruz", "Morales", "Reyes", "Jiménez", "Hernández")

clientes <- data.frame(
  cliente_id = 1:200,
  nombre = paste(
    sample(nombres, 200, replace = TRUE),
    sample(apellidos, 200, replace = TRUE)
  ),
  edad = sample(18:75, 200, replace = TRUE),
  genero = sample(c("M", "F"), 200, replace = TRUE),
  ciudad = sample(c("CDMX", "Guadalajara", "Monterrey", "Puebla", "Querétaro",
                   "Tijuana", "León", "Mérida", "Cancún", "Toluca"), 200, replace = TRUE),
  fecha_registro = sample(seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day"),
                         200, replace = TRUE),
  es_premium = sample(c(TRUE, FALSE), 200, replace = TRUE, prob = c(0.3, 0.7)),
  email = paste0(tolower(gsub(" ", ".", paste(
    sample(nombres, 200, replace = TRUE),
    sample(apellidos, 200, replace = TRUE)
  ))), "@email.com"),
  telefono = paste0("55-", sample(1000:9999, 200, replace = TRUE), "-", sample(1000:9999, 200, replace = TRUE))
)

# Segmento de cliente basado en edad
clientes$segmento <- cut(clientes$edad,
                         breaks = c(0, 25, 40, 60, 100),
                         labels = c("Joven", "Adulto", "Maduro", "Senior"))

write.csv(clientes, ruta("clientes.csv"), row.names = FALSE)
print("✓ clientes.csv creado")

# ============================================================================
# 3. PRODUCTOS
# ============================================================================

categorias <- c("Electrónica", "Ropa", "Alimentos", "Hogar", "Deportes",
                "Libros", "Juguetes", "Belleza", "Automotriz", "Mascotas")

productos <- data.frame(
  producto_id = 1:50,
  nombre_producto = paste("Producto", sprintf("%02d", 1:50)),
  categoria = sample(categorias, 50, replace = TRUE),
  subcategoria = paste("Sub", sample(1:5, 50, replace = TRUE)),
  marca = sample(c("Marca A", "Marca B", "Marca C", "Marca D", "Marca E"),
                50, replace = TRUE),
  precio_catalogo = round(runif(50, 15, 800), 2),
  costo = round(runif(50, 10, 400), 2),
  stock_actual = sample(0:500, 50, replace = TRUE),
  stock_minimo = sample(10:50, 50, replace = TRUE),
  proveedor = sample(c("Proveedor 1", "Proveedor 2", "Proveedor 3", "Proveedor 4"),
                    50, replace = TRUE),
  activo = sample(c(TRUE, FALSE), 50, replace = TRUE, prob = c(0.9, 0.1))
)

# Calcular margen
productos$margen_pct <- round(((productos$precio_catalogo - productos$costo) /
                               productos$precio_catalogo) * 100, 2)

# Estado de stock
productos$estado_stock <- ifelse(productos$stock_actual == 0, "Sin Stock",
                                ifelse(productos$stock_actual < productos$stock_minimo,
                                      "Stock Bajo", "Stock OK"))

write.csv(productos, ruta("productos.csv"), row.names = FALSE)
print("✓ productos.csv creado")

# ============================================================================
# 4. EMPLEADOS
# ============================================================================

empleados <- data.frame(
  empleado_id = 1:100,
  nombre = paste(
    sample(nombres, 100, replace = TRUE),
    sample(apellidos, 100, replace = TRUE)
  ),
  departamento = sample(c("Ventas", "Marketing", "IT", "RRHH", "Operaciones",
                         "Finanzas", "Atención al Cliente"), 100, replace = TRUE),
  puesto = sample(c("Analista", "Especialista", "Gerente", "Coordinador", "Asistente"),
                 100, replace = TRUE),
  salario_mensual = round(runif(100, 8000, 45000), 2),
  fecha_ingreso = sample(seq(as.Date("2015-01-01"), as.Date("2023-12-31"), by = "day"),
                        100, replace = TRUE),
  sucursal = sample(c("Matriz", "Sucursal Norte", "Sucursal Sur", "Sucursal Este",
                     "Sucursal Oeste"), 100, replace = TRUE),
  genero = sample(c("M", "F"), 100, replace = TRUE),
  edad = sample(22:65, 100, replace = TRUE),
  nivel_educacion = sample(c("Preparatoria", "Licenciatura", "Maestría", "Doctorado"),
                          100, replace = TRUE, prob = c(0.2, 0.5, 0.25, 0.05))
)

# Calcular antigüedad en años
empleados$antiguedad_anos <- as.numeric(difftime(as.Date("2024-01-01"),
                                                 empleados$fecha_ingreso,
                                                 units = "days")) / 365
empleados$antiguedad_anos <- round(empleados$antiguedad_anos, 1)

# Salario anual
empleados$salario_anual <- empleados$salario_mensual * 12

write.csv(empleados, ruta("empleados.csv"), row.names = FALSE)
print("✓ empleados.csv creado")

# ============================================================================
# 5. TRANSACCIONES DETALLADAS
# ============================================================================

n_transacciones <- 1000

transacciones <- data.frame(
  transaccion_id = 1:n_transacciones,
  fecha = sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"),
                n_transacciones, replace = TRUE),
  hora = paste0(sample(8:20, n_transacciones, replace = TRUE), ":",
               sample(c("00", "15", "30", "45"), n_transacciones, replace = TRUE)),
  cliente_id = sample(1:200, n_transacciones, replace = TRUE),
  producto_id = sample(1:50, n_transacciones, replace = TRUE),
  cantidad = sample(1:5, n_transacciones, replace = TRUE),
  precio_venta = round(runif(n_transacciones, 20, 600), 2),
  metodo_pago = sample(c("Efectivo", "Tarjeta Crédito", "Tarjeta Débito", "Transferencia"),
                      n_transacciones, replace = TRUE,
                      prob = c(0.3, 0.35, 0.25, 0.1)),
  tienda_id = sample(1:10, n_transacciones, replace = TRUE),
  vendedor_id = sample(1:25, n_transacciones, replace = TRUE)
)

# Calcular total
transacciones$total_transaccion <- transacciones$cantidad * transacciones$precio_venta

# Agregar mes y trimestre
transacciones$mes <- format(transacciones$fecha, "%Y-%m")
transacciones$trimestre <- paste0("Q", ceiling(as.numeric(format(transacciones$fecha, "%m")) / 3))
transacciones$dia_semana <- weekdays(transacciones$fecha)

write.csv(transacciones, ruta("transacciones.csv"), row.names = FALSE)
print("✓ transacciones.csv creado")

# ============================================================================
# 6. TIENDAS
# ============================================================================

tiendas <- data.frame(
  tienda_id = 1:10,
  nombre_tienda = paste("Sucursal", c("Centro", "Norte", "Sur", "Este", "Oeste",
                                      "Plaza A", "Plaza B", "Outlet", "Premium", "Express")),
  ciudad = c("CDMX", "Guadalajara", "Monterrey", "CDMX", "Puebla",
            "CDMX", "Guadalajara", "Tijuana", "CDMX", "Querétaro"),
  zona = c("Centro", "Norte", "Norte", "Este", "Centro",
          "Oeste", "Norte", "Norte", "Sur", "Centro"),
  tamano_m2 = c(500, 450, 600, 400, 550, 700, 480, 350, 800, 300),
  empleados = c(25, 20, 30, 18, 22, 35, 21, 15, 40, 12),
  fecha_apertura = as.Date(c("2010-03-15", "2012-06-20", "2011-09-10", "2015-02-28",
                            "2013-11-05", "2016-08-12", "2014-04-22", "2018-10-30",
                            "2009-01-15", "2020-07-01")),
  tipo = c("Principal", "Estándar", "Estándar", "Estándar", "Estándar",
          "Principal", "Estándar", "Outlet", "Premium", "Express")
)

write.csv(tiendas, ruta("tiendas.csv"), row.names = FALSE)
print("✓ tiendas.csv creado")

# ============================================================================
# 7. CREAR ARCHIVO EXCEL CON MÚLTIPLES HOJAS
# ============================================================================

lista_datos <- list(
  "Ventas" = ventas_retail[1:100, ],  # Muestra de ventas
  "Clientes" = clientes[1:50, ],       # Muestra de clientes
  "Productos" = productos,
  "Empleados" = empleados[1:50, ],     # Muestra de empleados
  "Tiendas" = tiendas
)

write_xlsx(lista_datos, ruta("datos_empresa.xlsx"))
print("✓ datos_empresa.xlsx creado")

# ============================================================================
# 8. RESUMEN
# ============================================================================

cat("\n")
cat("============================================\n")
cat("  DATASETS GENERADOS EXITOSAMENTE\n")
cat("============================================\n")
cat("Archivos creados:\n")
cat("  1. ventas_retail.csv (", nrow(ventas_retail), "filas )\n")
cat("  2. clientes.csv (", nrow(clientes), "filas )\n")
cat("  3. productos.csv (", nrow(productos), "filas )\n")
cat("  4. empleados.csv (", nrow(empleados), "filas )\n")
cat("  5. transacciones.csv (", nrow(transacciones), "filas )\n")
cat("  6. tiendas.csv (", nrow(tiendas), "filas )\n")
cat("  7. datos_empresa.xlsx (multi-hoja)\n")
cat("============================================\n")
cat("¡Listo para usar en los módulos del curso!\n")
cat("============================================\n")
