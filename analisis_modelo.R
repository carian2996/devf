# Ian Castillo Rosales
# 15072014

setwd("~/Desktop/repos/devf")
data <- read.csv("data.csv", header = T, quote = "", as.is=T)
data$date <- as.Date("1900-01-01") + data$date
data <- data[order(data$date), ]
data <- data[-246, ]

meses <- unique(months(data$date))

# ===== DIR =====

inc_rec_month <- c()
for(i in 1:12){
      cantidad <- subset(data[data$recurr==1 & data$inc_exp==1, ], 
                         months(data$date[data$recurr==1 & data$inc_exp==1])==meses[i])$amount
      inc_rec_month[i] <- sum(cantidad, na.rm = T)
}

exp_rec_month <- c()
for(i in 1:12){
      cantidad <- subset(data[data$recurr==1 & data$inc_exp==-1, ], 
                         months(data$date[data$recurr==1 & data$inc_exp==-1])==meses[i])$amount
      exp_rec_month[i] <- sum(cantidad, na.rm = T)
}

exp_rec_month
inc_rec_month

(puercometro <- exp_rec_month/inc_rec_month)

# ===== Net Worth =====
ingresos <- c()
for(i in 1:12){
      cantidad <- subset(data[data$inc_exp==1, ], months(data$date[data$inc_exp==1])==meses[i])$amount
      ingresos[i] <- sum(cantidad, na.rm = T)
}

egresos <- c()
for(i in 1:12){
      cantidad <- subset(data[data$inc_exp==-1, ], months(data$date[inc_exp==-1])==meses[i])$amount
      egresos[i] <- sum(cantidad, na.rm = T)
}

(nw <- ingresos - egresos)
