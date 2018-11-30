library(dplyr)

#1. naloga
#1.a
#uvozimo podatke set za srebro
srebro <- read.csv("srebro.csv")
zakljucni <- srebro[123:1,c("Close")]
zakljucni <- as.numeric(gsub("\\$", "", zakljucni))

#1.b
casovna_vrsta <- ts(data = zakljucni) 
graf_casovna_vrsta <- ts.plot(casovna_vrsta)

#2. naloga
#2.a
glajenje<- function(vrsta, k){
  T <- length(vrsta)
  vrsta1 <- c()
  for(i in (k+1):(T+1) ){
    vrsta1[i] <- sum(vrsta[(i-1):(i-k)])/k
  }
  vrsta1
} 

#2.b
gl <- glajenje(casovna_vrsta,5)
zadnji <- tail(gl,1)
gl <- c(gl,rep(zadnji,10))
gl <- ts(gl)
#2.c
graf2 <- ts.plot(casovna_vrsta,gl)

#2.d
mse <- function(vrsta,k){
  T <- length(vrsta)
  gladka <- glajenje(vrsta,k)
   MSE <- 0
  for(t in k:(T-1)){
    MSE <- MSE + (vrsta[t+1] - gladka[t+1])^2
  }
   MSE / (T-k)
}
mse(casovna_vrsta, 5)

#2.e

#3. naloga
#3.a
EG <- function(vrsta, alpha){
  T <- length(vrsta)
  L <- c()
  L[2] <- vrsta[1]
  for(t in 2:T){
    L[t+1] <- alpha * vrsta[t] + (1 - alpha) * L[t]
  }
  L
}
#3.b
eg <- ts(EG(casovna_vrsta,0.2))
graf3 <- ts.plot(casovna_vrsta,eg)

#3.c
funkcija <- function(alpha){
    return (mean((casovna_vrsta - EG(casovna_vrsta,alpha)[2:(length(casovna_vrsta)+1)])^2))
  }
opt <- optimize(funkcija,c(0,1))[1]
opt <- as.numeric(opt)

#3.d
eg2 <- ts(EG(casovna_vrsta,opt))
graf4 <- ts.plot(casovna_vrsta, eg2, col = c("green", "blue"))

