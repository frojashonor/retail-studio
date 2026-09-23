# ========================================================================
# R/funciones_dashboard.R - Lógica del dashboard ejecutivo
# Shiny carga AUTOMÁTICAMENTE los archivos .R de la carpeta R/ antes de
# ejecutar app.R. Aquí solo hay funciones: ninguna usa input ni output,
# así que todas se pueden probar en la consola, sin abrir la app.
# ========================================================================

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

# ---- Encontrar datasets/ subiendo carpetas -----------------------------
buscar_datasets <- function(desde = ".", max_niveles = 6) {
  carpeta <- desde
  for (i in 0:max_niveles) {
    candidato <- file.path(carpeta, "datasets", "transacciones.csv")
    if (file.exists(candidato)) return(file.path(carpeta, "datasets"))
    carpeta <- file.path(carpeta, "..")        # sube un nivel
  }
  stop("No encontré datasets/transacciones.csv. Ejecuta primero ",
       "source('datasets/generar_datasets.R') en la carpeta del curso.",
       call. = FALSE)
}

# ---- Cargar y unir los datos (una sola vez, al arrancar) ---------------
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

# ---- KPIs --------------------------------------------------------------
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

# ---- Gráficas (cada una recibe los datos YA filtrados) -----------------
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

grafica_metodo_pago <- function(datos) {
  datos %>%
    group_by(metodo_pago) %>%
    summarise(ventas = sum(total_transaccion), .groups = "drop") %>%
    mutate(participacion = ventas / sum(ventas)) %>%
    ggplot(aes(x = participacion,
               y = reorder(metodo_pago, participacion))) +
    geom_col(fill = color_principal, width = 0.7) +
    scale_x_continuous(labels = label_percent(),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(x = "% de las ventas", y = NULL) +
    tema_dashboard()
}

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

grafica_categorias <- function(datos) {
  datos %>%
    group_by(nombre = categoria) %>%
    summarise(ventas = sum(total_transaccion), .groups = "drop") %>%
    grafica_barras()
}

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
