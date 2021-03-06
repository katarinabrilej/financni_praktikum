#1. naloga
#1.a
#uvozimo podatke o ceni srebra v zadnjem letu
srebro <- read.csv("srebro.csv")
#zaključni tečaji trgovanja s srebrom za obdobje zadnjih šestih mesecev
zakljucni <- srebro[123:1,c("Close")]
zakljucni <- as.numeric(gsub("\\$", "", zakljucni))

#1.b
#graf časovne vrste
casovna_vrsta <- ts(data = zakljucni) 
graf_casovna_vrsta <- ts.plot(casovna_vrsta, ylab = "USD", main = "Srebro")
points(casovna_vrsta, pch = 20)

#2. naloga
#2.a
#funkcija G časovni vrsti priredi zglajene vrednosti
#naredimo še napoved za naslednji dan
G <- function(vrsta, k){
  T <- length(vrsta)
  zglajena_vrsta <- c()
  for(i in (k+1):(T+1) ){
    zglajena_vrsta [i] <- sum(vrsta[(i-1):(i-k)])/k
  }
  zglajena_vrsta 
} 

#2.b
#dano časovno vrsto zgladimo z drsečim povprečjem 5
gl <- G(casovna_vrsta,5)
zadnji <- tail(gl,1)
#napovedano vrednost 10x ponovimo
gl <- ts(c(gl,rep(zadnji,10)))
#zglajena časovna vrsta
gl

#2.c
#na graf iz naloge 1b dodamo še zglajeno vrsto 
graf2 <- ts.plot(casovna_vrsta,gl, main = "Drsece povprecje", ylab = "USD", col = c("black","red"))
points(casovna_vrsta, pch = 20)

#2.d
#izračunamo srednjo kvadratno napako
mse <- function(vrsta,k){
  T <- length(vrsta)
  zglajena_vrsta <- G(vrsta,k)
   MSE <- 0
  for(t in k:(T-1)){
    MSE <- MSE + (vrsta[t+1] - zglajena_vrsta[t+1])^2
  }
   MSE / (T-k)
}
mse(casovna_vrsta, 5)

#2.e

#red = 15 
gl15 <- G(casovna_vrsta,15)
gl15 <- ts(c(gl15, rep(tail(gl15,1),10)))
#zglajena časovna vrsta
gl15
#srednja kvadratna napaka
mse(casovna_vrsta, 15)

#red = 30
gl30 <- G(casovna_vrsta,30)
gl30 <- ts(c(gl30, rep(tail(gl30,1),10)))
#zglajena časovna vrsta
gl30

#srednja kvadratna napaka
mse(casovna_vrsta, 30)

#grafi za drseče povprečja reda 5, 15 in 30
par(mfrow=c(2,2))

graf2 <- ts.plot(casovna_vrsta,gl, main = "Drsece povprecje reda 5", ylab = "USD", 
                 col = c("black","red"), lwd = c(1,1.8))
points(casovna_vrsta, pch = 20)


graf3 <- ts.plot(casovna_vrsta,gl15, main = "Drsece povprecje reda 15", ylab = "USD", 
                 col = c("black","red"), lwd = c(1,1.8))
points(casovna_vrsta, pch = 20)

graf4 <- ts.plot(casovna_vrsta,gl30, main = "Drsece povprecje reda 30", ylab = "USD", 
                 col = c("black","red"), lwd = c(1,1.8))
points(casovna_vrsta, pch = 20)

par(mfrow=c(1,1))

#3. naloga
#3.a
#funkcija EG sprejme časovno vrsto in parameter alpha in vrne zglajene vrednosti
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
# izberemo si alpha med 0.1 in 0.3 -> npr. 0.2
eg <- EG(casovna_vrsta,0.2)
eg <- ts(c(eg, rep(tail(eg,1),10)))
graf5 <- ts.plot(casovna_vrsta,eg, main = "Eksponentno glajenje", ylab = "USD", 
                 col = c("black","red"), lwd = c(1,1.8))
points(casovna_vrsta, pch = 20)

#3.c
#določimo vrednost alpha, pri kateri zglajena časovna vrsta najmanj odstopa od opazovanih vrednosti
funkcija <- function(alpha){
    return (mean((casovna_vrsta - EG(casovna_vrsta,alpha)[2:(length(casovna_vrsta)+1)])^2))
  }
opt <- optimize(funkcija,c(0,1))[1]
opt <- as.numeric(opt)
#optimalna vrednost alpha
opt

#3.d
eg2 <- ts(EG(casovna_vrsta,opt))
eg2 <- ts(c(eg2, rep(tail(eg2,1),10)))
graf4 <- ts.plot(casovna_vrsta, eg2, ylab = "USD", main = "Eksponentno glajenje, minimalen MSE",
                 col = c("black","green"), lwd = c(1,1.8))
points(casovna_vrsta, pch = 20)
