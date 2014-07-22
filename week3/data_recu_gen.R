# Ian Castillo Rosales
# 21072014
# Generacion de datos para prueba del modelo

beca <- list(cantidad=1000, dias="01", tipo=1)
banxico <- list(cantidad=1000, dias=c("15", "30"), tipo=1)
mesada <- list(cantidad=500, dias="Sunday", tipo=1)
recurrentes <- list(beca=beca, banxico=banxico, mesada=mesada)

data_nrec_gen <- function(plazo, recurrentes){
      
      # ENTRADA
      # plazo = Plazo para la generacion de datos
      # ndatos = Numero de registros a generar
      # recurrentes = Lista con los gastos recurrentes
            # cantidad 
            # dia
            # 1 (income) / 0 (expense)
      
      # SALIDA
      # resultado = Dataframe con los resultados generados
      
      fecha_ini <- as.Date("01/01/2014", format = "%d/%m/%Y")
      fechas <- fecha_ini + 0:(plazo - 1)
      
      for(i in 1:length(recurrentes))
      beca <- data.frame(dates=fechas[format(fechas, "%d")=="01"])
      beca$cantidad <- 1000
      
      if(tipo = 1){
            beca$inc <- 1
            beca$exp <- 0
      } else{
            beca$inc <- 0
            beca$exp <- 1
      }
      
      beca$concepto <- "Aqui va un concepto"
      
      
}