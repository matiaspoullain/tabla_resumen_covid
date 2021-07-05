#library(data.table)
#library(dplyr)
#datos <- fread("data/Covid19Casos2021 reducido.csv", encoding = "UTF-8", stringsAsFactors = TRUE)
datos_r_edad <- fread("data/base_grande_r_edad.csv")
datos <- fread("data/base_grande.csv")

#Fechas maximas y minimas
fecha_minima <- as.Date("2021-01-01")
fecha_maxima <- max(datos$fecha_min)

#Codificacion provincia-id y depto id -----
cod_prov_depto <- fread("data/codigo_prov_depto.csv", encoding = "UTF-8", stringsAsFactors = TRUE)
#Cuando se necesite se joinea con esta base

#Poblaciones deptos:
poblaciones <- fread("data/poblacion_deptos.csv", encoding = "UTF-8")
