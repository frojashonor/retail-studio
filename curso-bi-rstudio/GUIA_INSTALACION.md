# Guía de Instalación y Configuración 🚀

## Requisitos del Sistema

### Hardware Mínimo
- **RAM**: 4 GB (recomendado 8 GB o más)
- **Espacio en disco**: 2 GB libres
- **Procesador**: Dual-core 2.0 GHz o superior

### Sistemas Operativos Soportados
- Windows 10/11
- macOS 10.13 o superior
- Ubuntu 18.04 o superior / otras distribuciones Linux

---

## Paso 1: Instalar R

### Windows

1. Ve a [https://cran.r-project.org/](https://cran.r-project.org/)
2. Click en "Download R for Windows"
3. Click en "base"
4. Descarga "Download R-X.X.X for Windows"
5. Ejecuta el instalador
6. Sigue el asistente de instalación (usa las opciones por defecto)

### macOS

1. Ve a [https://cran.r-project.org/](https://cran.r-project.org/)
2. Click en "Download R for macOS"
3. Descarga el archivo .pkg apropiado para tu sistema
4. Abre el archivo y sigue las instrucciones

### Linux (Ubuntu/Debian)

```bash
# Actualizar repositorios
sudo apt update

# Instalar R
sudo apt install r-base r-base-dev

# Verificar instalación
R --version
```

---

## Paso 2: Instalar RStudio

RStudio es el IDE (Entorno de Desarrollo Integrado) que usaremos.

1. Ve a [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)
2. Descarga RStudio Desktop (versión gratuita)
3. Instala siguiendo el asistente
4. Abre RStudio para verificar que funcione

---

## Paso 3: Configurar RStudio

### Configuraciones Recomendadas

1. Abre RStudio
2. Ve a `Tools > Global Options`

#### General
- [ ] **Workspace**: Desmarcar "Restore .RData into workspace at startup"
- [ ] **Workspace**: En "Save workspace to .RData on exit" seleccionar "Never"
- [ ] Esto evita problemas de variables antiguas

#### Code
- [ ] **Editing**: Marcar "Insert spaces for tab" (usar espacios, no tabs)
- [ ] **Display**: Marcar "Show line numbers"
- [ ] **Display**: Marcar "Highlight selected line"

#### Appearance
- [ ] Selecciona un tema que te guste (recomendado: "Tomorrow Night" o "Cobalt")
- [ ] Ajusta el tamaño de fuente (recomendado: 12-14)

#### Pane Layout
- [ ] Organiza los paneles a tu preferencia
- [ ] Recomendado: Console abajo-izquierda, Source arriba, Environment/History arriba-derecha, Files/Plots abajo-derecha

---

## Paso 4: Instalar Paquetes Esenciales

Copia y pega este código en la consola de R:

```r
# Lista completa de paquetes del curso
paquetes <- c(
  # Core tidyverse
  "tidyverse",      # Incluye dplyr, ggplot2, tidyr, readr, purrr, tibble

  # Importación de datos
  "readxl",         # Leer Excel
  "writexl",        # Escribir Excel
  "haven",          # SPSS, Stata, SAS
  "data.table",     # Datos grandes

  # Manipulación de datos
  "lubridate",      # Fechas
  "stringr",        # Texto
  "forcats",        # Factores

  # Visualización
  "scales",         # Formateo de ejes
  "RColorBrewer",   # Paletas de colores
  "plotly",         # Gráficos interactivos
  "ggthemes",       # Temas adicionales
  "patchwork",      # Combinar gráficos

  # Bases de datos
  "DBI",            # Interfaz de BD
  "RSQLite",        # SQLite
  "odbc",           # ODBC
  "RMySQL",         # MySQL
  "RPostgreSQL",    # PostgreSQL

  # Dashboards
  "shiny",          # Apps web
  "shinydashboard", # Dashboards
  "flexdashboard",  # Dashboards en RMarkdown
  "DT",             # Tablas interactivas

  # Machine Learning
  "caret",          # Framework ML
  "randomForest",   # Random Forest
  "rpart",          # Árboles de decisión
  "rpart.plot",     # Visualizar árboles
  "cluster",        # Clustering
  "factoextra",     # Visualización clustering
  "forecast",       # Series de tiempo
  "prophet",        # Forecasting de Facebook

  # Reportes
  "rmarkdown",      # Reportes dinámicos
  "knitr",          # Generación de reportes
  "kableExtra",     # Tablas bonitas

  # Utilidades
  "janitor",        # Limpieza de datos
  "skimr"           # Resúmenes rápidos
)

# Instalar paquetes que no estén instalados
nuevos_paquetes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]

if(length(nuevos_paquetes) > 0) {
  cat("Instalando", length(nuevos_paquetes), "paquetes...\n")
  install.packages(nuevos_paquetes, dependencies = TRUE)
  cat("✓ Instalación completa\n")
} else {
  cat("✓ Todos los paquetes ya están instalados\n")
}

# Verificar instalación
cat("\nVerificando instalación...\n")
for (pkg in paquetes) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("✓", pkg, "\n")
  } else {
    cat("✗", pkg, "ERROR\n")
  }
}
```

**Nota**: La instalación puede tomar 10-30 minutos dependiendo de tu conexión a internet.

---

## Paso 5: Configurar Directorio de Trabajo

### Opción 1: Usar Proyectos de RStudio (Recomendado)

1. Descarga o clona este repositorio
2. En RStudio: `File > Open Project`
3. Navega a la carpeta del curso y abre `curso-bi-rstudio.Rproj`

### Opción 2: Configurar manualmente

```r
# Ver directorio actual
getwd()

# Cambiar directorio (ajusta la ruta)
# Windows
setwd("C:/Users/TuUsuario/Documents/curso-bi-rstudio")

# Mac/Linux
setwd("/Users/tuusuario/Documents/curso-bi-rstudio")
```

---

## Paso 6: Generar Datasets de Ejemplo

```r
# Ejecutar el script generador de datos
source("datasets/generar_datasets.R")
```

Esto creará todos los archivos CSV necesarios para el curso.

---

## Paso 7: Verificar Instalación

Ejecuta este código para verificar que todo funcione:

```r
# Test completo del entorno
cat("==========================================\n")
cat("  VERIFICACIÓN DEL ENTORNO\n")
cat("==========================================\n\n")

# 1. Versión de R
cat("Versión de R:", R.version.string, "\n")

# 2. Paquetes principales
paquetes_test <- c("tidyverse", "ggplot2", "dplyr", "shiny", "caret")
cat("\nPaquetes principales:\n")
for (pkg in paquetes_test) {
  version <- packageVersion(pkg)
  cat("  ✓", pkg, version, "\n")
}

# 3. Test de lectura de datos
cat("\nTest de importación:\n")
if (file.exists("datasets/ventas_retail.csv")) {
  ventas_test <- read.csv("datasets/ventas_retail.csv")
  cat("  ✓ Datos cargados:", nrow(ventas_test), "filas\n")
} else {
  cat("  ✗ No se encontraron los datasets. Ejecuta generar_datasets.R\n")
}

# 4. Test de gráficos
cat("\nTest de visualización:\n")
tryCatch({
  library(ggplot2)
  df_test <- data.frame(x = 1:10, y = rnorm(10))
  p <- ggplot(df_test, aes(x, y)) + geom_line()
  print(p)
  cat("  ✓ ggplot2 funciona correctamente\n")
}, error = function(e) {
  cat("  ✗ Error en ggplot2:", e$message, "\n")
})

cat("\n==========================================\n")
cat("✓ ENTORNO CONFIGURADO CORRECTAMENTE\n")
cat("==========================================\n")
```

---

## Solución de Problemas Comunes

### Problema: Error al instalar paquetes

**Solución**:
```r
# Intentar con otro mirror
chooseCRANmirror()

# O especificar un mirror manualmente
install.packages("nombre_paquete", repos = "https://cloud.r-project.org")
```

### Problema: "Error in library()"

**Solución**: El paquete no está instalado
```r
install.packages("nombre_del_paquete")
```

### Problema: Errores de compilación en Linux

**Solución**: Instalar dependencias del sistema
```bash
# Ubuntu/Debian
sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev

# Fedora/CentOS
sudo yum install libcurl-devel openssl-devel libxml2-devel
```

### Problema: RStudio no encuentra R

**Solución**:
1. Ve a `Tools > Global Options > General`
2. En "R version" asegúrate de que apunte a la instalación correcta de R

### Problema: Memoria insuficiente

**Solución**:
```r
# Aumentar límite de memoria (solo Windows)
memory.limit(size = 8000)  # 8 GB

# Limpiar objetos no usados
rm(objeto_grande)
gc()  # Garbage collection
```

---

## Recursos Adicionales

### Documentación
- [R Documentation](https://www.rdocumentation.org/)
- [RStudio Cheatsheets](https://www.rstudio.com/resources/cheatsheets/)
- [tidyverse Documentation](https://www.tidyverse.org/)

### Comunidad
- [Stack Overflow - R Tag](https://stackoverflow.com/questions/tagged/r)
- [RStudio Community](https://community.rstudio.com/)
- [r/rstats en Reddit](https://www.reddit.com/r/rstats/)

### Tutoriales
- [R for Data Science, 2.ª ed. (libro gratuito)](https://r4ds.hadley.nz/)
- [Datacamp: Intro to R](https://www.datacamp.com/courses/free-introduction-to-r)
- [Coursera: R Programming](https://www.coursera.org/learn/r-programming)

---

## Atajos de Teclado Útiles en RStudio

### Ejecución de Código
- `Ctrl + Enter` (Win/Linux) / `Cmd + Return` (Mac): Ejecutar línea actual
- `Ctrl + Shift + Enter`: Ejecutar todo el script
- `Ctrl + Shift + S`: Ejecutar script desde el inicio

### Edición
- `Ctrl + Shift + M`: Insertar pipe %>%
- `Alt + -`: Insertar operador de asignación <-
- `Ctrl + Shift + C`: Comentar/descomentar líneas
- `Ctrl + I`: Re-indentar código

### Navegación
- `Ctrl + 1`: Mover cursor a Source
- `Ctrl + 2`: Mover cursor a Console
- `Ctrl + F`: Buscar en archivo
- `Ctrl + Shift + F`: Buscar en archivos

### Otros
- `Tab`: Autocompletar
- `F1`: Ayuda sobre función
- `Ctrl + Shift + A`: Reformatear código

---

## ¡Listo para Comenzar! 🎉

Una vez completados todos los pasos, estarás listo para comenzar el curso.

**Siguiente paso**: Abre `modulo-01-fundamentos/01_introduccion_rstudio.R`

¡Mucho éxito en tu aprendizaje! 📊✨
