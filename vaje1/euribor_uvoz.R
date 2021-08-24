library(dplyr)
library(ggplot2)
require(ggplot2)
require(reshape2)

#1. naloga


#uvozimo podatke za euribor za leto 2009 #
euribor_2009 <- read.csv2("euribor_2009.csv", header = TRUE) %>% .[(1:15),]
rownames(euribor_2009) <- euribor_2009$X
euribor_2009$X <- NULL
euribor_2009 <- t(euribor_2009)
print(euribor_2009)

#uvozimo podatke za euribor za leto 2010
euribor_2010 <- read.csv2("euribor_2010.csv", header = TRUE) %>% .[(1:15),]
rownames(euribor_2010) <- euribor_2010$X
euribor_2010$X <- NULL
euribor_2010 <- t(euribor_2010)
print(euribor_2010)

#uvozimo podatke za euribor za leto 2011
euribor_2011 <- read.csv2("euribor_2011.csv", header = TRUE) %>% .[(1:15),]
rownames(euribor_2011) <- euribor_2011$X
euribor_2011$X <- NULL
euribor_2011 <- t(euribor_2011)
print(euribor_2011)

#podatki za 3m in 6m
euribor_2009_3_6 <- euribor_2009[,c("3m","6m")]
euribor_2010_3_6 <- euribor_2010[,c("3m","6m")]
euribor_2011_3_6 <- euribor_2011[,c("3m","6m")]

euribor_3_6 <- rbind(euribor_2009_3_6,euribor_2010_3_6,euribor_2011_3_6, id = NULL)

#graf, ki prikazuje kako se spreminjata trimesečna in šestmesečna obrestna mera
casovna_vrsta_3 <- ts(data = euribor_3_6[,c("3m")], frequency = 12, start = c(2009,1)) 
casovna_vrsta_6 <- ts(data = euribor_3_6[,c("6m")], frequency = 12, start = c(2009,1)) 
graf_casovna_vrsta <- ts.plot(casovna_vrsta_3, casovna_vrsta_6, col = c("darkolivegreen","deeppink"),main = "Euribor", ylab = "%")
legend(2011.4,3 ,legend = c("6m", "3m"), col = c("deeppink","darkolivegreen"), lty = 1, box.lty = 0)

#2. naloga

#izberemo 3 datume: januar 2009, maj 2010 in april 2011
#podatki za izbrane datume za vse dospelosti 
januar_2009 <- euribor_2009[(1),]
maj_2010 <- euribor_2010[(5),]
april_2011 <- euribor_2011[(4),]

#graf, ki prikazuje časovno strukturo obrestnih mer na izbrane datume
df <- data.frame(dospetje = c(1/4,1/2,3/4,1:12),
                 januar_2009 = januar_2009,
                 maj_2010 = maj_2010,
                 april_2011 = april_2011)
df <- melt(df ,  id.vars = 'dospetje', variable.name = 'datum', value.name = "obrestna_mera")
theme_update(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position="none")
ggplot(df, aes(dospetje,obrestna_mera)) + geom_point(aes(colour = datum)) + ggtitle("Casovna struktura Euribor") + 
  xlab("Dospetje[mesec]") + ylab("%") + scale_color_manual(values=c("deepskyblue3", "mediumpurple", "lightgoldenrod")) + 
  geom_line(aes(colour = datum)) + geom_text(data = df %>% filter(dospetje == 11), vjust =3, aes(label = c("2.1.2009","3.5.2010","1.4.2011")))+
  guides(fill=FALSE)

#oblike prikazanih krivulj
#za vsa tri leta velja, da na izbran datum obrestna mera narašča skupaj z dospetjem. na začetku krivulje, ko se dospetja
#razlikujejo v tednih raste hitreje, potem pa počasneje, za leto 2009 je tako na koncu skoraj že ravna
#najvišje obrestne mere lahko zasledimo v letu 2009, potem pa sledi velik padec v letu 2010 in ponovna rast v letu 2011
#sicer imajo vse krivulje dokaj podobno obliko, prav tako so vse v večini delov konkavne
#za leto 2009 za dospetja med 3 in 12 mesecev krivulja skoraj ravna, kar pomeni da so obrestne mere skoraj enake, to pa
#je signal za negotovost v ekonomiji
#tako za leto 2010 kot tudi 2011 imata krivulji normalno obliko, to pomeni da obrestna mera narašča z dospetjem (naklon
#krivulje je pozitiven). Pozitiven naklon odraža pričakovanja investitorjev o prihodnji rasti ekonomije, rast pa je pogosto povezana
#s pričakovanji višje inflacije. Višja inflacija pa pomeni, da bo centralna banka zaostrila monetarno politiko z višanjem
#kratkotrajnih obrestnih mer v prihodnosti z namenom upočasnitve ekonomske rasti. Prav tako pa ustvari potrebo po zavarovalni premiji 
#povezani z negotovostjo o prihodnji stopnji inflacije, ki ima vpliv na vrednost prihodnjih denarnih tokov. Investitorji zato zahtevajo
#višje obrestne mere za dospetja dlje v prihododnost. 

#3. naloga

#izračunamo terminske obrestne mere
tri = euribor_3_6[,c("3m")]
sest = euribor_3_6[,c("6m")]
#stolpec, ki prikazuje vse možne terminske obrestne mere 3x6
napoved = (((1 + 0.5*sest/100)/(1 + 0.25*tri/100) - 1)/0.25)*100
print(napoved)
napoved1 = napoved[1:32]
leto = c(1:36)

#dodamo stolpec napoved in leto
tabela_napoved <- cbind(euribor_3_6,napoved,leto, id = NULL)
tabela_napoved[,"napoved"] <- NA
tabela_napoved[5:36,"napoved"] <- napoved1
tabela_napoved[1:12,"leto"] <- 2009
tabela_napoved[13:24,"leto"] <- 2010
tabela_napoved[25:36,"leto"] <- 2011
#tabela, ki prikazuje datum, opazovano trenutno 3-mesečno obrestno mero in napoved
tabela_napoved1 <- tabela_napoved[,c("3m","6m","napoved")]
print(tabela_napoved1)

tabela_napoved_3m <- tabela_napoved[,c("3m","napoved","leto")]
colnames(tabela_napoved_3m)[1] <- "tri_m"
tabela_napoved_3m <- data.frame(tabela_napoved_3m)

#razsevni grafikon
tabela_napoved_3m$leto = as.factor(tabela_napoved_3m$leto)
barve = c("red", "blue", "aquamarine2")[tabela_napoved_3m$leto]
plot(tabela_napoved_3m$napoved, tabela_napoved_3m$tri_m,main = "3m Euribor 2009-2011", 
                      ylab = "Opazovano", xlab = "Napoved", pch = 16, col = barve, xlim=c(0,3), ylim=c(0,3))
abline(0,1, lty = 'dashed')
abline(lm(tabela_napoved_3m$tri_m ~ tabela_napoved_3m$napoved))
legend("topleft", c("2009", "2010", "2011"), col=c('red','blue', 'aquamarine2'), pch=c(16, 16, 16), bty = "n")
 

#grafikon za leto 2009
podatki_2009 <- tabela_napoved_3m %>% filter(leto == 2009)
plot(podatki_2009$napoved, podatki_2009$tri_m,main = "3m Euribor 2009", 
     ylab = "Opazovano", xlab = "Napoved", pch = 16, col = "red", xlim=c(0,3), ylim=c(0,3))
abline(0,1, lty = 'dashed')
abline(lm(podatki_2009$tri_m ~ podatki_2009$napoved),col = "red")

#grafikon za leto 2010
podatki_2010 <- tabela_napoved_3m %>% filter(leto == 2010)
plot(podatki_2010$napoved, podatki_2010$tri_m,main = "3m Euribor 2010", 
     ylab = "Opazovano", xlab = "Napoved", pch = 16, col = "blue", xlim=c(0,3), ylim=c(0,3))
abline(0,1, lty = 'dashed')
abline(lm(podatki_2010$tri_m ~ podatki_2010$napoved),col = "blue")


#grafikon za leto 2011
podatki_2011 <- tabela_napoved_3m %>% filter(leto == 2011)
plot(podatki_2011$napoved, podatki_2011$tri_m,main = "3m Euribor 2011", 
     ylab = "Opazovano", xlab = "Napoved", pch = 16, col = "aquamarine2", xlim=c(0,3), ylim=c(0,3))
abline(0,1, lty = 'dashed')
abline(lm(podatki_2011$tri_m ~ podatki_2011$napoved),col = "aquamarine2")

#hipoteza pričakovanj trga
#pod pogojem, da ta hipoteza velja bi točke grafa morale ležati na simetrali lihih kvadrantov, vendar pa večinoma ležijo pod
#simetralo. Torej hipoteze na podlagi empiričnih podatkov ne moremo potrditi. Hipotezo še najbolj potrjujejo podatki za leto 2011,
#ta regresijska premica se namreč še najbolje približa simetrali lihih kvadrantov. 
#Dejstvo, da točke ležijo pod simetralo pomeni, da so bile naše napovedi preveč optimistične, kar pa je razumljivo, saj je na
#začetku opazovanega obdobja moč opaziti strm padec obrestne mere

