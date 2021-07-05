#Leo esta base para saber las combinaciones posibles de provincia-depto
cod_prov_depto <- fread("data/codigo_prov_depto.csv", encoding = "UTF-8", stringsAsFactors = TRUE)

#Objetos y funciones utiles ----
niveles_r_edad <- c("0-17", "18-39", "40-59", "60 o más", "N/R")
fecha_minima <- as.Date("2020-03-01")
fecha_maxima <- max(datos$fecha_min)

# Funcion de relleno:

todas_fechas_prov_depto <- function(fecha_inicial, fecha_final, nombre_fecha = "fecha_min"){
  e <- data.table(expand.grid(interaction(cod_prov_depto$prov_code, cod_prov_depto$codigo, sep = ","), as.IDate(seq(fecha_inicial, fecha_final, by ="days"))))
  names(e) <- c("prov_depto", nombre_fecha)
  e[, c("prov", "depto") := tstrsplit(prov_depto, ",", fixed=TRUE)]
  e[, c("prov", "depto") := .(as.integer(prov), as.integer(depto))]
  e[, -c("prov_depto")]
}

todas_fechas_prov_depto_r_edad <- function(fecha_inicial, fecha_final, nombre_fecha = "fecha_min"){
  e <- data.table(expand.grid(interaction(cod_prov_depto$prov_code, cod_prov_depto$codigo, sep = ","), as.IDate(seq(fecha_inicial, fecha_final, by ="days")), niveles_r_edad))
  names(e) <- c("prov_depto", nombre_fecha, "r_edad")
  e[, c("prov", "depto") := tstrsplit(prov_depto, ",", fixed=TRUE)]
  e[, c("prov", "depto") := .(as.integer(prov), as.integer(depto))]
  e[, -c("prov_depto")]
}

########### Creacion de base_grande_r_edad ########################
#Tendra toda la info por prov, depto, fecha_min y r_edad

# Confirmados por prov, depto y r_edad:

confirmados_prov_depto_r_edad <- function(base){
  c <- base
  c <- c[clasificacion_resumen == "Confirmado" ,][, .(confirmados = .N), by = .(residencia_provincia_id, residencia_departamento_id, fecha_min, r_edad)]
  relleno <- todas_fechas_prov_depto_r_edad(fecha_minima, fecha_maxima)
  c <- merge(relleno, c, by.x = c("prov", "depto", "fecha_min", "r_edad"), by.y = c("residencia_provincia_id", "residencia_departamento_id", "fecha_min", "r_edad"), all.x = TRUE)
  c[ , "confirmados" := lapply(.SD, nafill, fill=0), .SDcols = "confirmados"]
  c <- c %>% arrange(fecha_min)
  c[, "confirmados_acumulados" := cumsum(confirmados), by = .(prov, depto, r_edad)]
  for(i in 14:1){
    if(i == 14){
      c[, "confirmados_14" := frollapply(confirmados, n = 14, FUN = sum, align = "right"), by = .(prov, depto, r_edad)]
    }else{
      filas <- i * length(niveles_r_edad) * nrow(cod_prov_depto)
      chico <- c[1:filas]
      chico[, "confirmados_14" := frollapply(confirmados, n = i, FUN = sum, align = "right"), by = .(prov, depto, r_edad)]
      c$confirmados_14[1:filas] <- chico$confirmados_14[1:filas]
    }
  }
  c %>% arrange(fecha_min, prov, depto, r_edad)
}

# Casos posibles por prov, depto y r_edad:

casos_posibles_prov_depto_r_edad <- function(base){
  c <- base
  c <- c[, .(casos_posibles = .N), by = .(residencia_provincia_id, residencia_departamento_id, fecha_min, r_edad)]
  relleno <- todas_fechas_prov_depto_r_edad(fecha_minima, fecha_maxima)
  c <- merge(relleno, c, by.x = c("prov", "depto", "fecha_min", "r_edad"), by.y = c("residencia_provincia_id", "residencia_departamento_id", "fecha_min", "r_edad"), all.x = TRUE)
  c[ , "casos_posibles" := lapply(.SD, nafill, fill=0), .SDcols = "casos_posibles"]
  c <- c %>% arrange(fecha_min)
  c[, "casos_posibles_acumulados" := cumsum(casos_posibles), by = .(prov, depto, r_edad)]
  for(i in 14:1){
    if(i == 14){
      c[, "casos_posibles_14" := frollapply(casos_posibles, n = 14, FUN = sum, align = "right"), by = .(prov, depto, r_edad)]
    }else{
      filas <- i * length(niveles_r_edad) * nrow(cod_prov_depto)
      chico <- c[1:filas]
      chico[, "casos_posibles_14" := frollapply(casos_posibles, n = i, FUN = sum, align = "right"), by = .(prov, depto, r_edad)]
      c$casos_posibles_14[1:filas] <- chico$casos_posibles_14[1:filas]
    }
  }
  c %>% arrange(fecha_min, prov, depto, r_edad)
}



#Fallecidos por prov, depto y r_edad:

fallecidos_prov_depto_r_edad <- function(base){
  c <- base
  c <- c[!is.na(fecha_fallecimiento) ,][, .(fallecidos = .N), by = .(residencia_provincia_id, residencia_departamento_id, fecha_fallecimiento, r_edad)]
  relleno <- todas_fechas_prov_depto_r_edad(fecha_minima, fecha_maxima, "fecha_fallecimiento")
  c <- merge(relleno, c, by.x = c("prov", "depto", "fecha_fallecimiento", "r_edad"), by.y = c("residencia_provincia_id", "residencia_departamento_id", "fecha_fallecimiento", "r_edad"), all.x = TRUE)
  c[ , "fallecidos" := lapply(.SD, nafill, fill=0), .SDcols = "fallecidos"]
  c <- c %>% arrange(fecha_fallecimiento)
  c[, "fallecidos_acumulados" := cumsum(fallecidos), by = .(prov, depto, r_edad)]
  c <- c %>% arrange(fecha_fallecimiento)
  for(i in 14:1){
    if(i == 14){
      c[, "fallecidos_14" := frollapply(fallecidos, n = 14, FUN = sum, align = "right"), by = .(prov, depto, r_edad)]
    }else{
      filas <- i * length(niveles_r_edad) * nrow(cod_prov_depto)
      chico <- c[1:filas]
      chico[, "fallecidos_14" := frollapply(fallecidos, n = i, FUN = sum, align = "right"), by = .(prov, depto, r_edad)]
      c$fallecidos_14[1:filas] <- chico$fallecidos_14[1:filas]
    }
  }
  c %>% arrange(fecha_fallecimiento, prov, depto, r_edad)
}


#Funcion de union:
base_grande_r_edad <- function(base){
  c <- cbind(confirmados_prov_depto_r_edad(base), casos_posibles_prov_depto_r_edad(base)[, -c("prov", "depto", "fecha_min", "r_edad")], fallecidos_prov_depto_r_edad(base)[, -c("prov", "depto", "fecha_fallecimiento", "r_edad")])
  c[, c("positividad", "letalidad") := .(100 * confirmados_acumulados / casos_posibles_acumulados, 100 * fallecidos_acumulados / confirmados_acumulados)]
  c
}


########### Creacion de base_grande ########################
#Tendra toda la info por prov, depto y fecha_min

base_grande <- function(base){
  c <- base[, .(confirmados = sum(confirmados),
                confirmados_acumulados = sum(confirmados_acumulados),
                confirmados_14 = sum(confirmados_14),
                casos_posibles = sum(casos_posibles),
                casos_posibles_acumulados = sum(casos_posibles_acumulados),
                casos_posibles_14 = sum(casos_posibles_14),
                fallecidos = sum(fallecidos),
                fallecidos_acumulados = sum(fallecidos_acumulados),
                fallecidos_14 = sum(fallecidos_14)), by = .(prov, depto, fecha_min)]
  c[, c("positividad", "letalidad") := .(100 * confirmados_acumulados / casos_posibles_acumulados, 100 * fallecidos_acumulados / confirmados_acumulados)]
  c
}
