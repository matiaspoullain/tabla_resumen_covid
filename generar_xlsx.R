
#Librerias
library(dplyr)
library(lubridate)
library(data.table)
library(xlsx)


#Preprocesamiento:
download.file("https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.zip", "covid_comprimido.zip")
unzip("covid_comprimido.zip", "Covid19Casos.csv")
datos <- fread("Covid19Casos.csv", encoding = "UTF-8", stringsAsFactors = TRUE)
file.remove("Covid19Casos.csv")
file.remove("covid_comprimido.zip")
#Borro columnas que no nos interesan:
datos <- datos[, -c("residencia_pais_nombre",
                    "residencia_provincia_nombre",
                    "residencia_departamento_nombre",
                    "carga_provincia_nombre",
                    "sepi_apertura",
                    "origen_financiamiento",
                    "clasificacion",
                    "fecha_diagnostico",
                    "ultima_actualizacion",
                    "cuidado_intensivo",
                    "fallecido",
                    "asistencia_respiratoria_mecanica",
                    "fecha_cui_intensivo",
                    "fecha_internacion")]

#todos los que no especificaron la provincia se las cambio por provincia de carga
nuevo <- fifelse(datos$residencia_provincia_id == 99, datos$carga_provincia_id, datos$residencia_provincia_id)
datos$residencia_provincia_id <- nuevo

#edad
datos$edad <- round(fifelse(datos$edad_años_meses == "Meses", datos$edad/12, as.numeric(datos$edad)),2)
datos <- datos[, -c("edad_años_meses")]

#fecha_min es la fecha de inicio de sintomas y si no la tiene es la de apertura del caso
datos$fecha_min <- fifelse(is.na(datos$fecha_inicio_sintomas), datos$fecha_apertura, datos$fecha_inicio_sintomas)
datos <- datos[fecha_min >= as.Date("2021-01-01")]
#Saco las variables que use recien:
datos <- datos[, -c("carga_provincia_id", "fecha_inicio_sintomas",
                    "fecha_apertura")]

#Saco repetidos
datos <- unique(datos)

#Saco la variable id:
datos <- datos[, -c("id_evento_caso")]

#Agrego rangos de edad:
datos[, r_edad := fcase(edad <= 17 , "0-17",
                        edad > 17 & edad <= 39, "18-39",
                        edad > 39 & edad <= 59, "40-59",
                        edad > 59, "60 o más",
                        default = "N/R")]

#Si quiero guardar la base cruda poco procesada:
#fwrite(datos, file = "data/Covid19Casos2021 reducido.csv")

# Calculos de metricas
source("Scripts/funciones preprocesamiento.R", encoding = "UTF-8")

datos <- datos %>%
  base_grande_r_edad() %>%
  base_grande()

poblaciones <- fread("data/poblacion_deptos.csv", encoding = "UTF-8")
poblaciones.prov <- poblaciones[, .(province_poblacion = sum(department_poblacion)), by = province_code]
source("Scripts/funciones de analisis.R", encoding = "UTF-8")

final <- datos %>%
  razon_incidencia() %>%
  merge(datos[fecha_min == fecha_maxima, .(prov, depto, letalidad)], by.x = c("province_code", "department_code"), by.y = c("prov", "depto")) %>%
  rename(Provincia_codigo = province_code,
         Departamento_codigo = department_code,
         Provincia_nombre = prov_name,
         Departamento_nombre = department_name,
         Razon = razon,
         Incidencia = incidencia,
         Letalidad = letalidad)

letalidad.prov <- datos %>% 
  agrupamiento_provincias() %>%
  agregar_acumulados_positividad_letalidad()

letalidad.prov <- letalidad.prov[fecha_min == fecha_maxima, .(prov, letalidad)][, prov := as.character(prov)]


a.caracter <- datos %>%
  razon_incidencia_prov()
a.caracter[, province_code := as.character(province_code)]

final.prov <- merge(a.caracter, letalidad.prov, by.x = "province_code", by.y = "prov") %>%
  rename(Provincia_codigo = province_code,
         Provincia_nombre = prov_name,
         Razon = razon,
         Incidencia = incidencia,
         Letalidad = letalidad)

nombre.archivo <- paste0("Tablas resumen/Resumen COVID Argentina Departamental ", as.character(today()), ".xlsx")

write.xlsx(x = final, file = nombre.archivo, row.names = FALSE, showNA = FALSE)

nombre.archivo.prov <- paste0("Tablas resumen/Resumen COVID Argentina Provincial ", as.character(today()), ".xlsx")

write.xlsx(x = final.prov, file = nombre.archivo.prov, row.names = FALSE, showNA = FALSE)
