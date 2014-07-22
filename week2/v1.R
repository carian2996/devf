responde <- function(){

setwd("~/Desktop/repos/devf/week3")
source("model.R")

data_recu <- read.csv("data_recu.csv", as.is = T)
data_nrec <- read.csv("data_nrec.csv", as.is = T)

responde(meta = 400, plazo = 45, data_recu = data_recu, data_nrec = data_nrec)

}