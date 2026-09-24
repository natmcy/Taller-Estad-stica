install.packages(c("readr", "dplyr", "janitor", "ggplot2", "moments", "scales", "writexl"))

library(readr)      # lectura de archivos csv
library(dplyr)      # manipulacion de datos
library(janitor)    # limpieza de nombres de columnas
library(ggplot2)    # graficos
library(moments)    # asimetria (skewness) y curtosis
library(scales)     # formato de numeros en graficos
library(writexl)

#PUNTO 1 Conjunto de datos, tamano de muestra y numero de variables

datos <- read_csv("Evaluaciones_Agropecuarias_Municipales_–_EVA._2019_-_2025._Base_Agrícola_20260909.csv",
                  locale = locale(encoding = "UTF-8"))

#CAMBIO DE NOMBRES DE COLUMNAS
datos <- clean_names(datos)

# PARA SABER CON QUE NOMBRES QUEDARON LAS COLUMNAS
names(datos)

#TAMBAÑO DE MUESTRA: NÚMERO DE FILAS 
nfilas <- nrow(datos)
nfilas

#NÚMERO DE VARIABLES: NÚMERO DE COLUMAS 
n_variables <- ncol(datos)
n_variables

# PUNTO 2: Descripcion de cada variable (tipo, escala de medicion, unidad)

## MANUAL DE TABLA DE DESCRIPCIÓN DE VARIABLES

tabla_variables <- data.frame(
  variable = c("codigo_dane_departamento", "departamento", "codigo_dane_municipio",
               "municipio", "grupo_cultivo", "subgrupo", "cultivo",
               "desagregacion_cultivo", "ano", "periodo", "area_sembrada",
               "area_cosechada", "produccion", "rendimiento", "ciclo_del_cultivo",
               "estado_fisico_del_cultivo", "codigo_del_cultivo",
               "nombre_cientifico_del_cultivo"),
  tipo = c("Cualitativa", "Cualitativa", "Cualitativa", "Cualitativa",
           "Cualitativa", "Cualitativa", "Cualitativa", "Cualitativa",
           "Cuantitativa", "Cualitativa", "Cuantitativa", "Cuantitativa",
           "Cuantitativa", "Cuantitativa", "Cualitativa", "Cualitativa",
           "Cualitativa", "Cualitativa"),
  escala_medicion = c("Nominal (codigo identificador)", "Nominal", "Nominal (codigo identificador)",
                      "Nominal", "Nominal", "Nominal", "Nominal", "Nominal",
                      "Discreta (de razon)", "Ordinal (semestre dentro del ano)",
                      "Continua (de razon)", "Continua (de razon)",
                      "Continua (de razon)", "Continua (de razon)",
                      "Nominal", "Nominal", "Nominal", "Nominal"),
  unidad = c("Sin unidad (codigo)", "Sin unidad", "Sin unidad (codigo)", "Sin unidad",
             "Sin unidad", "Sin unidad", "Sin unidad", "Sin unidad", "Ano",
             "Sin unidad (etiqueta de semestre)", "Hectareas (ha)", "Hectareas (ha)",
             "Toneladas (t)", "Toneladas por hectarea (t/ha)", "Sin unidad",
             "Sin unidad", "Sin unidad (codigo)", "Sin unidad")
)

print(tabla_variables)

# Guardamos la tabla como CSV 
write.csv(tabla_variables, "tabla_descripcion_variables.csv", row.names = FALSE)


# PUNTO 3: Inconsistencias, errores de digitacion, datos faltantes, limpieza

#Conversion de las variables numericas
# Area sembrada, area cosechada, produccion y rendimiento vienen como texto
# con formato colombiano (punto de miles y coma decimal).
# Hay que convertirlas a numero antes de poder analizarlas.

convertir_numero_co <- function(x) {
  x <- gsub("\\.", "", x)   # se quitan los puntos de miles
  x <- gsub(",", ".", x)    # se cambia la coma decimal por punto
  as.numeric(x)
}

datos <- datos %>%
  mutate(
    area_sembrada  = convertir_numero_co(area_sembrada),
    area_cosechada = convertir_numero_co(area_cosechada),
    produccion     = convertir_numero_co(produccion),
    rendimiento    = convertir_numero_co(rendimiento)
  )

#Valores faltantes (NA) 
# Conteo de NA por columna, despues de la conversion
colSums(is.na(datos))

#Valores "faltantes disfrazados"

tabla_no_aplica <- datos %>%
  filter(nombre_cientifico_del_cultivo == "No aplica") %>%
  nrow()
cat("Registros con 'No aplica' en nombre cientifico:", tabla_no_aplica, "\n")

# ##### INCONSISTEENCIAS LÓGICAS #####

# a) Area cosechada mayor que el area sembrada (no es posible en la realidad)
inconsistencia_area <- datos %>%
  filter(area_cosechada > area_sembrada) %>%
  nrow()
cat("Registros con area cosechada > area sembrada:", inconsistencia_area, "\n")


# b) Area sembrada igual a cero
area_cero <- datos %>% filter(area_sembrada == 0) %>% nrow()
cat("Registros con area sembrada = 0:", area_cero, "\n")


# c) Rendimiento igual a cero
rendimiento_cero <- datos %>% filter(rendimiento == 0) %>% nrow()
cat("Registros con rendimiento = 0:", rendimiento_cero, "\n")


# d) Duplicados exactos
duplicados <- sum(duplicated(datos))
cat("Filas duplicadas:", duplicados, "\n")


datos_limpios <- datos %>%
  mutate(nombre_cientifico_del_cultivo = na_if(nombre_cientifico_del_cultivo, "No aplica")) %>%
  filter(area_cosechada <= area_sembrada)

cat("Filas antes de la limpieza:", nrow(datos), "\n")
cat("Filas despues de la limpieza:", nrow(datos_limpios), "\n")


# PUNTO 4: ESTADISTICAS DESCRIPTIVAS UNIVARIADAS

# Variables cuantitativas
# Tendencia central: media, mediana
# Dispersion: desviacion estandar, varianza, rango, coeficiente de variacion
# Posicion: cuartiles (Q1, Q2, Q3)

resumen_cuantitativas <- function(x) {
  data.frame(
    media       = mean(x, na.rm = TRUE),
    mediana     = median(x, na.rm = TRUE),
    desv_estandar = sd(x, na.rm = TRUE),
    varianza    = var(x, na.rm = TRUE),
    minimo      = min(x, na.rm = TRUE),
    maximo      = max(x, na.rm = TRUE),
    rango       = max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
    Q1          = quantile(x, 0.25, na.rm = TRUE),
    Q3          = quantile(x, 0.75, na.rm = TRUE),
    coef_variacion_pct = (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100,
    asimetria   = skewness(x, na.rm = TRUE),
    curtosis    = kurtosis(x, na.rm = TRUE)
  )
}

resumen_area_sembrada  <- resumen_cuantitativas(datos_limpios$area_sembrada)
resumen_area_cosechada <- resumen_cuantitativas(datos_limpios$area_cosechada)
resumen_produccion     <- resumen_cuantitativas(datos_limpios$produccion)
resumen_rendimiento    <- resumen_cuantitativas(datos_limpios$rendimiento)

tabla_cuantitativas <- bind_rows(
  "Area sembrada (ha)"  = resumen_area_sembrada,
  "Area cosechada (ha)" = resumen_area_cosechada,
  "Produccion (t)"      = resumen_produccion,
  "Rendimiento (t/ha)"  = resumen_rendimiento,
  .id = "variable"
)

print(tabla_cuantitativas)
write_xlsx(tabla_cuantitativas, "estadisticas_cuantitativas.xlsx")


#=======================================================
#4.2 variables cualitativas: conteo y porcentaje 

tabla_grupo_cultivo <- datos_limpios %>%
  count(grupo_cultivo, name = "frecuencia") %>%
  mutate(porcentaje = round(100 * frecuencia / sum(frecuencia), 2)) %>%
  arrange(desc(frecuencia))

print(tabla_grupo_cultivo)
write_xlsx(tabla_grupo_cultivo, "tabla_grupo_cultivo.xlsx")

tabla_ciclo_cultivo <- datos_limpios %>%
  count(ciclo_del_cultivo, name = "frecuencia") %>%
  mutate(porcentaje = round(100 * frecuencia / sum(frecuencia), 2))

print(tabla_ciclo_cultivo)
write_xlsx(tabla_ciclo_cultivo, "tabla_ciclo_cultivo.xlsx")

tabla_departamento <- datos_limpios %>%
  count(departamento, name = "frecuencia") %>%
  mutate(porcentaje = round(100 * frecuencia / sum(frecuencia), 2)) %>%
  arrange(desc(frecuencia))

print(head(tabla_departamento, 10))   # se muestran solo los 10 primeros
write_xlsx(tabla_departamento, "tabla_departamento.xlsx")

# PUNTO 5: GRAFICOS PARA TENER UNA VISION GENERAL DE LOS DATOS
#Histograma: distribucion del rendimiento
# Se usa escala logaritmica en el eje x porque el rendimiento tiene
# valores muy dispersos (algunos cultivos rinden mucho mas que otros).

ggplot(datos_limpios %>% filter(rendimiento > 0), aes(x = rendimiento)) +
  geom_histogram(bins = 40, fill = "#2E8B57", color = "white") +
  scale_x_log10() +
  labs(title = "Distribucion del rendimiento por registro (escala log)",
       x = "Rendimiento (t/ha, escala log)", y = "Frecuencia") +
  theme_minimal()

#Boxplot: rendimiento por grupo de cultivo

ggplot(datos_limpios %>% filter(rendimiento > 0), 
       aes(x = reorder(grupo_cultivo, rendimiento, FUN = median), y = rendimiento)) +
  geom_boxplot(fill = "#4682B4", outlier.alpha = 0.1, outlier.size = 0.8) +
  scale_y_log10() +
  coord_flip() + # Hace el gráfico horizontal para leer fácil los nombres
  labs(
    title = "Rendimiento por grupo de cultivo (escala logarítmica)",
    x = "Grupo de cultivo",
    y = "Rendimiento (t/ha)"
  ) +
  theme_minimal()

# Barras: frecuencia por grupo de cultivo

##########
# Opción 1: Graficar directamente contando la frecuencia en R
datos_limpios %>%
  count(grupo_cultivo, name = "frecuencia") %>%
  ggplot(aes(x = reorder(grupo_cultivo, frecuencia), y = frecuencia)) +
  geom_col(fill = "#D2691E") +
  coord_flip() +
  labs(
    title = "Número de registros por grupo de cultivo",
    x = "Grupo de cultivo",
    y = "Número de registros"
  ) +
  theme_minimal()

#########
### GRÁFICOS DE BARRAS PARA VARIABLES CUALITATIVAS

## grupo de cultivo ###

datos_limpios %>%
  count(grupo_cultivo, name = "frecuencia") %>%
  ggplot(aes(x = reorder(grupo_cultivo, frecuencia), y = frecuencia)) +
  geom_col(fill = "#D2691E") +
  coord_flip() +
  labs(
    title = "Número de registros por grupo de cultivo",
    x = "Grupo de cultivo",
    y = "Número de registros"
  ) +
  theme_minimal()

## ciclo de cultivo
datos_limpios %>%
  count(ciclo_del_cultivo, name = "frecuencia") %>%
  ggplot(aes(x = reorder(ciclo_del_cultivo, frecuencia), y = frecuencia)) +
  geom_col(fill = "#2E8B57") +
  coord_flip() +
  labs(
    title = "Distribución por Ciclo del Cultivo",
    x = "Ciclo del cultivo",
    y = "Número de registros"
  ) +
  theme_minimal()

## top 10 departamentos
datos_limpios %>%
  count(departamento, name = "frecuencia") %>%
  slice_max(order_by = frecuencia, n = 10) %>%
  ggplot(aes(x = reorder(departamento, frecuencia), y = frecuencia)) +
  geom_col(fill = "#4682B4") +
  coord_flip() +
  labs(
    title = "Top 10 Departamentos por número de registros",
    x = "Departamento",
    y = "Número de registros"
  ) +
  theme_minimal()

## estado fisico del cultivo 
datos_limpios %>%
  count(estado_fisico_del_cultivo, name = "frecuencia") %>%
  ggplot(aes(x = reorder(estado_fisico_del_cultivo, frecuencia), y = frecuencia)) +
  geom_col(fill = "#8B4513") +
  coord_flip() +
  labs(
    title = "Distribución por Estado Físico del Cultivo",
    x = "Estado físico",
    y = "Número de registros"
  ) +
  theme_minimal()

datos_limpios %>%
  count(codigo_dane_departamento, name = "frecuencia") %>%
  slice_max(order_by = frecuencia, n = 10) %>%
  ggplot(aes(x = reorder(as.character(codigo_dane_departamento), frecuencia), y = frecuencia)) +
  geom_col(fill = "#4682B4") + coord_flip() +
  labs(title = "Top 10 Códigos DANE Departamento", x = "Código DANE dpto", y = "Frecuencia") + theme_minimal()

datos_limpios %>%
  count(codigo_dane_municipio, name = "frecuencia") %>%
  slice_max(order_by = frecuencia, n = 10) %>%
  ggplot(aes(x = reorder(as.character(codigo_dane_municipio), frecuencia), y = frecuencia)) +
  geom_col(fill = "#5F9EA0") + coord_flip() +
  labs(title = "Top 10 Códigos DANE Municipio", x = "Código DANE mpio", y = "Frecuencia") + theme_minimal()

datos_limpios %>%
  count(municipio, name = "frecuencia") %>%
  slice_max(order_by = frecuencia, n = 10) %>%
  ggplot(aes(x = reorder(municipio, frecuencia), y = frecuencia)) +
  geom_col(fill = "#6495ED") + coord_flip() +
  labs(title = " Top 10 Municipios con más Registros", x = "Municipio", y = "Frecuencia") + theme_minimal()


datos_limpios %>%
  count(subgrupo, name = "frecuencia") %>%
  slice_max(order_by = frecuencia, n = 10) %>%
  ggplot(aes(x = reorder(subgrupo, frecuencia), y = frecuencia)) +
  geom_col(fill = "#CD853F") + coord_flip() +
  labs(title = "10.  Subgrupos de Cultivo", x = "Subgrupo", y = "Frecuencia") + theme_minimal()

datos_limpios %>%
  count(cultivo, name = "frecuencia") %>%
  slice_max(order_by = frecuencia, n = 10) %>%
  ggplot(aes(x = reorder(cultivo, frecuencia), y = frecuencia)) +
  geom_col(fill = "#E9967A") + coord_flip() +
  labs(title = " Cultivos Específicos", x = "Cultivo", y = "Frecuencia") + theme_minimal()

datos_limpios %>%
  count(desagregacion_cultivo, name = "frecuencia") %>%
  slice_max(order_by = frecuencia, n = 10) %>%
  ggplot(aes(x = reorder(desagregacion_cultivo, frecuencia), y = frecuencia)) +
  geom_col(fill = "#FA8072") + coord_flip() +
  labs(title = "Desagregaciones de Cultivo", x = "Desagregación", y = "Frecuencia") + theme_minimal()

datos_limpios %>%
  count(ano, name = "frecuencia") %>%
  ggplot(aes(x = factor(ano), y = frecuencia)) +
  geom_col(fill = "#4682B4") +
  labs(title =  "Distribución de Registros por Año", x = "Año", y = "Frecuencia") + theme_minimal()


datos_limpios %>%
  count(periodo, name = "frecuencia") %>%
  ggplot(aes(x = reorder(periodo, frecuencia), y = frecuencia)) +
  geom_col(fill = "#6A5ACD") + coord_flip() +
  labs(title = "Registros por Periodo Semestral", x = "Periodo", y = "Frecuencia") + theme_minimal()

datos_limpios %>%
  count(ano, name = "frecuencia") %>%
  ggplot(aes(x = factor(ano), y = frecuencia)) +
  geom_col(fill = "#4682B4") +
  labs(title = "Distribución de Registros por Año", x = "Año", y = "Frecuencia") + theme_minimal()

datos_limpios %>%
  count(periodo, name = "frecuencia") %>%
  ggplot(aes(x = reorder(periodo, frecuencia), y = frecuencia)) +
  geom_col(fill = "#6A5ACD") + coord_flip() +
  labs(title = " Registros por Periodo Semestral", x = "Periodo", y = "Frecuencia") + theme_minimal()

datos_limpios %>%
  count(codigo_del_cultivo, name = "frecuencia") %>%
  slice_max(order_by = frecuencia, n = 10) %>%
  ggplot(aes(x = reorder(as.character(codigo_del_cultivo), frecuencia), y = frecuencia)) +
  geom_col(fill = "#708090") + coord_flip() +
  labs(title = " Top 10 Códigos de Cultivo", x = "Código cultivo", y = "Frecuencia") + theme_minimal()

datos_limpios %>%
  count(nombre_cientifico_del_cultivo, name = "frecuencia") %>%
  slice_max(order_by = frecuencia, n = 10) %>%
  ggplot(aes(x = reorder(nombre_cientifico_del_cultivo, frecuencia), y = frecuencia)) +
  geom_col(fill = "#2F4F4F") + coord_flip() +
  labs(title = " Top 10 Nombres Científicos", x = "Nombre científico", y = "Frecuencia") + theme_minimal()

datos_limpios %>%
  count(subgrupo, name = "frecuencia") %>%
  slice_max(order_by = frecuencia, n = 10) %>%
  ggplot(aes(x = reorder(subgrupo, frecuencia), y = frecuencia)) +
  geom_col(fill = "#CD5C5C") + coord_flip() +
  labs(title = " Top 10 Subgrupos de Cultivo", x = "Subgrupo", y = "Frecuencia") + theme_minimal()


#########

### cuantitativas

ggplot(datos_limpios %>% filter(area_sembrada > 0), aes(x = area_sembrada)) +
  geom_histogram(bins = 40, fill = "#2E8B57", color = "white") +
  scale_x_log10() +
  labs(title = "Distribución del Área Sembrada (Escala Log)", x = "Área sembrada (ha)", y = "Frecuencia") +
  theme_minimal()

ggplot(datos_limpios %>% filter(area_cosechada > 0), aes(x = area_cosechada)) +
  geom_histogram(bins = 40, fill = "#3CB371", color = "white") +
  scale_x_log10() +
  labs(title = "Distribución del Área Cosechada (Escala Log)", x = "Área cosechada (ha)", y = "Frecuencia") +
  theme_minimal()

ggplot(datos_limpios %>% filter(produccion > 0), aes(x = produccion)) +
  geom_histogram(bins = 40, fill = "#20B2AA", color = "white") +
  scale_x_log10() +
  labs(title = " Distribución de la Producción (Escala Log)", x = "Producción (t)", y = "Frecuencia") +
  theme_minimal()

ggplot(datos_limpios %>% filter(rendimiento > 0), aes(x = rendimiento)) +
  geom_histogram(bins = 40, fill = "#008080", color = "white") +
  scale_x_log10() +
  labs(title = " Distribución del Rendimiento (Escala Log)", x = "Rendimiento (t/ha)", y = "Frecuencia") +
  theme_minimal()

