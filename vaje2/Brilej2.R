library(actuar)

#1. naloga
#1.a - histogram, ki prikazuje vzorčno porazdelitev individualnih škodnih zahtevkov
vzorec1 <- scan(file = "vzorec1.txt")
histogram_a <- hist(vzorec1, breaks = c(0:25), main = "Histogram odskodnin", xlab = "Visina odskodnine", col = "cadetblue2") 

#1.b - porazdelitev, ki se prilega mojim podatkom je Paretova s parametri shape in min
#izračunamo oba parametra po metodi mde
parametri <- mde(vzorec1,dpareto1,start = list("shape" = 1,"min" = 1), measure = "CvM")
shape <- parametri$estimate[1]
min <- parametri$estimate[2]
shape # = 1.524951 
min # = 2.590723 

#1.c - histogram iz a + gostota spremenljivke Y
histogram_b <- hist(vzorec1, breaks = c(0:25),probability = TRUE, main = "Histogram odskodnin", 
                    xlab = "Visina odskodnine", col = "cadetblue2", ylim = c(0,0.6))
curve(dpareto1(x,shape,min), col = "blue", add = TRUE, lwd = 2)
legend(14,0.5, legend = c("Paretova porazdelitev"), col = "blue",lty=1)

#grafična primerjava vzorčne in teoretične porazdelitvene funkcije
plot(ecdf(vzorec1), main = "Porazdelitvena funkcija odskodnin", ylab = "Porazdelitvena funkcija", 
     xlab = "Visina odskodnine",cex=.6)
curve(ppareto1(x,shape,min), col = "blue",add = TRUE, lwd = 2)
legend(13,0.5, legend = c("empiricna porazdelitev","Paretova porazdelitev"), box.lty = 0, col = c("black", "blue"),
       lty = 1:1, pch = c(16,NA))

#1.d - uporabimo Waldovi identiteti in izračunamo upanje in disperijo kolektivne škode S
size = 20
prob = 1/2

#upanje in varianca binomske porazdelitve
upanje_N <- size * prob
varianca_N <- upanje_N * (1 - prob)

#upanje in varianca Pareto porazdelitve
upanje_Y <- (shape * min) / (shape - 1)
varianca_Y <- Inf # ker je shape < 2
upanje_Y_kvadrat = varianca_Y + upanje_Y^2

#upanje in varianca kolektivne škode S
upanje_S <- upanje_N * upanje_Y # =75.25889
upanje_S
varianca_S <- varianca_Y * upanje_N + upanje_Y_kvadrat * varianca_N
varianca_S

#2. naloga
#2.a - z zaokroževanjem diskretiziramo porazdelitev spremenljivke y
h = 0.25
n = 80
dis <- discretize(ppareto1(x,shape,min),from = 0, to = n*h, step = h, method = "rounding")

#2.b - graf porazdelitvene funkcije Y in njene dikretizacije
plot(stepfun(seq(0,(n-1)*h,h),diffinv(dis)), main = "Paretova porazdelitev", pch = NA, col  = "goldenrod1",
     ylab = "Porazdelitvena funkcija", ylim = c(0,1), lwd = 2, xlim = c(0,20))
curve(ppareto1(x,shape,min), add = TRUE)

#2.c - Panjerjev algoritem 

dis2 <- discretize(ppareto1(x,shape, min),from = 0, to = 100000, step = h, method = "rounding")
S <- aggregateDist(method = "recursive", model.freq = "binom",model.sev = dis2,size = size,
              prob = prob, maxit=1000000,tol = 0.002, convolve = 0, x.scale = h)

#2.d - upanje in disperzija kumulativne škode
S_upanje<- sum( knots(S) * diff(S))
S_upanje # = 69.84754
S_upanje_kvadrat <- sum( knots(S)^2 * diff(S))
S_upanje_kvadrat # = 7455.399
S_varianca <- S_upanje_kvadrat - S_upanje^2
S_varianca # = 2576.72

#3. naloga
#3.a - simuliramo 1000 vrednosti s.s. S


#simulacija spremenljivke N
simulacija_N <- rbinom(10000, 20, 1/2)

#simulacija spremenljivke S
simulacija_S <- c()

for (n in simulacija_N){
  simulacija_S <- c(simulacija_S, sum(rpareto1(n, shape, min) ))
}

#3.b - ocena za upanje in disperzijo spremenljivke S

upanje_simulacija_S <- mean(simulacija_S)
upanje_simulacija_S # = 75.56874

varianca_simulacija_S <- var(simulacija_S)
varianca_simulacija_S # = 32748.6

#Z Monte Carlo simulacijai smo v primerjavi s Panjerjevim algoritmom dobili ustreznejšo 
#vrednost za pričakovano vrednost kumulativne škode S, saj je ta bliže vrednosti
#izračunani v nalogi 1.d. Tudi za varianco bi lahko rekli, da smo dobili boljšo oceno, saj je skoraj 
#10x krat večja kot pa v nalogi 2.d, v nalogi 1.d pa je neskončno. 
#3.c - 
graf_simulacija <- plot(S)
plot(ecdf(simulacija_S), add = TRUE, col = "green")
legend(400,0.3, legend = c("Panjerjev algoritem","Monte Carlo simulacija"), box.lty = 0, 
       col = c("black", "green"), lty = 1:1)







