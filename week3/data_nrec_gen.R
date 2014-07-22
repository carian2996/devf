# Ian Castillo Rosales
# 21072014
# Generacion de datos para prueba del modelo

data_nrec_gen <- function(plazo, ndatos, prom, vari){
      
      # ENTRADA
      # plazo = Plazo para la generacion de datos
      # ndatos = Numero de registros a generar
      # prom = Promeio de gasto no recurrentes
      # vari = Parianza de gasto no recurrentes
      
      # SALIDA
      # resultado = Dataframe con los resultados generados
      
      fecha_ini <- as.Date("01/01/2014", format = "%d/%m/%Y")
      fechas <- fecha_ini + 0:(plazo - 1)
      
      registros <- sort(sample(fechas, ndatos, T))
      
      data <- data.frame(date=registros)
      
      data$amount <- abs(rnorm(ndatos, prom, vari))
      
      data$inc <- sample(c(1, 0), ndatos, T, c(0.1, 0.9))
      
      data$exp <- 0
      data$exp[data$inc==0] <- 1
      
      data$concepto <- "Aqui va un concepto"
      
      data
}