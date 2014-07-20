# Ian Castillo Rosales
# 16072014 - 17072014

responde <- function(meta, plazo, data_recu, data_nrec){
      
      # ENTRADA
      
      # SALIDA
      
      dias <- unique(sort(format(as.Date(data_recu$date, "%d/%m/%Y"), "%d")))
      meses <- unique(sort(format(as.Date(data_recu$date, "%d/%m/%Y"), "%m")))
      
      # ===== Fechas =====
      data_recu$date <- format(as.Date(data_recu$date, "%d/%m/%Y"), "%d/%m/%Y")
      data_nrec$date <- format(as.Date(data_nrec$date, "%d/%m/%Y"), "%d/%m/%Y")
      
      # ===== Meta diaria =====
      daily <- meta/plazo
      
      # ==================== RECURRENTES ====================
      # ===== Ingresos recurrentes diarios =====
      income_r <- c()
      for(i in 1:length(meses)){
            income_dias <- c()
            new_data <- subset(data_recu, format(as.Date(data_recu$date, "%d/%m/%Y"), "%m") == meses[i])
            new_dias <- unique(sort(format(as.Date(new_data$date, "%d/%m/%Y"), "%d")))
            
            for(j in 1:length(dias)){
                  income_dias[j] <- sum(new_data$amount[format(as.Date(new_data$date, "%d/%m/%Y"), "%d") == dias[j]
                                                        & new_data$inc == 01])
            }
            
            income_r <- c(income_r, income_dias)
      }
      
      # ===== Egresos recurrentes diarios =====
      expense_r <- c()
      for(i in 1:length(meses)){
            expense_dias <- c()
            new_data <- subset(data_recu, format(as.Date(data_recu$date, "%d/%m/%Y"), "%m") == meses[i])
            new_dias <- unique(sort(format(as.Date(new_data$date, "%d/%m/%Y"), "%d")))
            
            for(j in 1:length(dias)){
                  expense_dias[j] <- sum(new_data$amount[format(as.Date(new_data$date, "%d/%m/%Y"), "%d") == dias[j]
                                                         & new_data$exp == 1])
            }
            
            expense_r <- c(expense_r, expense_dias)
      }
      
      # ==================== NO RECURRENTES ====================
      # ===== Ingresos no recurrentes diarios =====
      income_n <- c()
      for(i in 1:length(meses)){
            income_dias <- c()
            new_data <- subset(data_nrec, format(as.Date(data_nrec$date, "%d/%m/%Y"), "%m") == meses[i])
            new_dias <- unique(sort(format(as.Date(new_data$date, "%d/%m/%Y"), "%d")))
            
            for(j in 1:length(dias)){
                  income_dias[j] <- sum(new_data$amount[format(as.Date(new_data$date, "%d/%m/%Y"), "%d") == dias[j]
                                                        & new_data$inc == 01])
            }
            
            income_n <- c(income_n, income_dias)
      }
      
      # ===== Egresos recurrentes diarios =====
      expense_n <- c()
      for(i in 1:length(meses)){
            expense_dias <- c()
            new_data <- subset(data_nrec, format(as.Date(data_nrec$date, "%d/%m/%Y"), "%m") == meses[i])
            new_dias <- unique(sort(format(as.Date(new_data$date, "%d/%m/%Y"), "%d")))
            
            for(j in 1:length(dias)){
                  expense_dias[j] <- sum(new_data$amount[format(as.Date(new_data$date, "%d/%m/%Y"), "%d") == dias[j]
                                                         & new_data$exp == 1])
            }
            
            expense_n <- c(expense_n, expense_dias)
      }
      
      # ===== Indicadores =====
      indicador_nrec <- income_n - expense_n
      indicador_recu <- income_r - expense_r
      ndias <- as.numeric(max(as.Date(data_recu$date, "%d/%m/%Y"))-min(as.Date(data_recu$date, "%d/%m/%Y")))
      
      cochinometro <- sum(indicador_recu + indicador_nrec)/ndias
      
      if(cochinometro >= daily){
            "GASTA! GASTA! GASTA!"
      } else{
            "Para con las chelas"
      }
}