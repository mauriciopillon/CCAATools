### CCAA: Comissão de Credenciamento/Des/Re Acompanhamento e Autoavaliação 
### Coordenação PPGCAP:
###      Isabela Gasparini
###      Guilherme Koslovski
### Comissão:
###      Avanilde Kemczinski
###      Marcelo Hounsell
###      Maurício Pillon

### CCAA2021: Comissão de Credenciamento/Des/Re Acompanhamento e Autoavaliação 
### Coordenação PPGCAP:
###      Charles Christian Miers
### Comissão:
###      Avanilde Kemczinski
###      Adriano Fiorese
###      Isabela Gasparini
###      Maurício Pillon


########################################
# Definições:
### período de 4 anos: 2018-2021 (até 31/12/2021)
### Periódico tem bonus de 25%
### Ordem de prioridade para coleta das melhores produções:
   ### P.A1, E.A1, P.A2, E.A2, P.B1, E.B1 ...
### P/ Pq1x3E: Somente periódico restrito libera evento!
### Qualis 2016 (A1,A2,B1,B2,B3,B4,B5)
#########################################

######
# Script R desenvolvido por:
### Mauricio Pillon
# Autorizo uso, cópia e modificação, desde que as melhorias sejam disponibilizadas publicamente.
### 

library(dplyr)
library("readxl")
library(gplots)
library(plotly)
library(plotrix)
library(RColorBrewer)
source ("mcalibQ16.R")

planilha <- "jcr/Produção Individual 2020-teste.xlsx"

mysheets <- excel_sheets(planilha)
p20_4 <- read_excel(planilha, sheet="2021")
p20_3 <- read_excel(planilha, sheet="2020")
p20_2 <- read_excel(planilha, sheet="2019")
p20_1 <- read_excel(planilha, sheet="2018")


#remover submenu, A3, A4 (periodico e eventos), valores Qualis 2020
p20_1 <- p20_1[c(-1, -4, -5, -14, -15),seq (4, 34, 2)*-(1)]
p20_2 <- p20_2[c(-1, -4, -5, -14, -15),seq (4, 34, 2)*-(1)]
p20_3 <- p20_3[c(-1, -4, -5, -14, -15),seq (4, 34, 2)*-(1)]
p20_4 <- p20_4[c(-1, -4, -5, -14, -15),seq (4, 34, 2)*-(1)]

# remover colunas prof (9) ocultas na planilha de origem
p20_1 <- p20_1[-9]
p20_2 <- p20_2[-9]
p20_3 <- p20_3[-9]
p20_4 <- p20_4[-9]

# remover "A3 e A4" 

legQualis <- c ("A1","A2","B1","B2","B3","B4","B5","NC")
vtposicao <- c ("P.A1","P.A2","P.B1","P.B2","P.B3","P.B4","P.B5","P.NC",
                "E.A1","E.A2","E.B1","E.B2","E.B3","E.B4","E.B5","E.NC")
tabpesos <- c  ( 125,  106.25,  87.5,  62.5,  25,  12.5,   6.25,   0,
                 100,  85,  70,  50,  20,  10,   5,   0)

# P.A1, E.A2, P.A2, E.A2, P.B1, E.B1, P.B2, E.B2, P.B3, E.B3, P.B4, E.B4, P.B5, E.B5, P.NC, E.NC
vpriori <- c (1,9,2,10,3,11,4,12,5,13,6,14,7,15,8,16)

# profnames <- c ("Adriano", "André", "Avanilde", "Carla",
#                 "Charles", "Fabiano", "Guilherme",
#                 "Isabela", "Janine", "Marcelo", "Maurício", 
#                 "Parpinelli", "R. Obelheiro", "Rosso")

pd2016_2019 <- c (1682.5, 450, 443.75, 403.75, 
                  1702.5, 395, 2168.75,
                  2696.25, 580, 2097.5, 1697.5, 1730,
                  550, 352.5)

top52016_2019 <- c (455, 300, 290, 217.5,
                     476.25, 350, 491.25,
                     488.75, 325, 515, 476.25,
                     472.5, 415, 352.5)

top102016_2019 <- c (857.5, 450, 385, 317.5,
                     906.25, 395, 918.75,
                     838.75, 515, 825, 903.75,
                     840, 535, 352.5)

top312016_2019 <- c (997.5, 0, 0, 197.5,
                     1046.25, 330, 1788.75,
                     978.75, 0, 925, 1061.25,
                     700, 505, 352.5)

pd2017_2020 <- c (1510, 400, 362.5, 580, 
                  1651.25, 580, 1931.25,
                  2581.25, 760, 1575, 1716.25, 1692.5,
                  335, 333.75)

top52017_2020 <- c (455, 300, 245, 217.5, 
                  476.25, 455, 491.25,
                  543.75, 400, 515, 476.25, 560,
                  282.5, 333.75)

top102017_2020 <- c (840, 400, 311.25, 283.75, 
                    901.25, 575, 916.25,
                    893.75, 650, 785, 901.25, 945,
                    335, 333.75)

top312017_2020 <- c (700, 0, 0, 197.5, 
                     733.75, 545, 1091.25,
                     1313.75, 550, 885, 733.75, 1465,
                     262.5, 333.75)

# profnames <- c ("A", "C", "B", "V",
#                 "W", "I", "F",
#                 "M", "Q", "V", "X", "D",
#                 "H", "P")


profnames <- c ("Antonio", "Caroline", "Danubia",
                "Edicleuza", "Fábio", "Gabriel", "Henrique",
                "Isolde", "Joaquim", "Larissa", "Norma", "Marta",
                "Oswaldo", "Pedro")

cor <- c(brewer.pal(n = 10, name = "Set3"),brewer.pal(n = length(profnames)-10, name = "Spectral"))

######## Main Procedure #####

topprod <- matrix (NA, nc = 18, nr=length(profnames))
allprod <- matrix (NA, nc = 18, nr=length(profnames))
colnames(topprod) <- unlist (c("Prof",
                               "A1","A2","B1","B2","B3","B4","B5","NC",
                               "A1","A2","B1","B2","B3","B4","B5","NC","Cor"))
colnames(allprod) <- unlist (c("Prof",
                               "A1","A2","B1","B2","B3","B4","B5","NC",
                               "A1","A2","B1","B2","B3","B4","B5","NC","Cor"))


# tprof (Adriano[4], André[5] ... )
for (tprof in 4:(length(profnames)+3)) {
  # qualquer periódico 1:8
  # nrper <- sum (p20_4[1:8,tprof],p20_1[1:8,tprof],p20_2[1:8,tprof],p20_3[1:8,tprof])
  # somente periódico restrito 1:3 (A1, A2, B1)
  #print ("TPROF")
  #print (tprof)
  #print (profnames[tprof-3])
  nrper <- sum (as.numeric(unlist(p20_1[1:3,tprof])),
                as.numeric(unlist(p20_2[1:3,tprof])),
                as.numeric(unlist(p20_3[1:3,tprof])),
                as.numeric(unlist(p20_4[1:3,tprof])))
  #print (nrper)
  nevt <- as.numeric (nrper * 3)
  sop <<- data.frame(
    p20_1[1:16,tprof],
    p20_2[1:16,tprof],
    p20_3[1:16,tprof],
    p20_4[1:16,tprof])
  #print (sop)
  # Eventos E.A1, E.A2, E.B1, E.B2, E.B3, E.B4, E.B5, E.NC
  eventos <- c (9,10,11,12,13,14,15,16)
  #print (nevt)
  top(nevt,sop,(tprof - 3),eventos)
  #print ("DEB 4")
  # soperiodico(sop,(tprof - 3))
}
print("DEB 5")
pub3par1 <- topprod
# apagar periódico B2-NC
pub3par1[,5:9] <- 0

################ Extraindo Melhores publicacoes (top5 e 10) ##############

topcapes()

extrairtop(5)
top5 <- topprod

extrairtop(10)
top10 <- topprod

############################################
### Gráficos ####
pdf ("PDFs/ccaa-prodQ2016.pdf", width = 11)
par(mfrow = c(1,1))

##############
contabiliza(allprod)
vtall <- ret
tpall <- vtall[order(as.numeric(vtall[,2])),]
nvtall <- data.frame (vtall,pd2016_2019, (100-(as.numeric(vtall[,2])*100/as.numeric(pd2016_2019)))*(-1))
nvtall[order(as.numeric(nvtall[,4])),1:5]
nvtall2 <- data.frame (vtall,pd2017_2020, (100-(as.numeric(vtall[,2])*100/as.numeric(pd2017_2020)))*(-1))
nvtall2[order(as.numeric(nvtall2[,4])),1:5]

bp <- barplot2(as.numeric(vtall[order(as.numeric(vtall[,2])),2]),
               beside = TRUE, horiz = FALSE,
               col = vtall[order(as.numeric(vtall[,2])),3],
               #col = cor,
               main = c("Pontuação Produção Completa 2018-2021")#,
               #ylim = c(0, 2000)
)
text(bp-0.5,-75, vtall[order(as.numeric(vtall[,2])),1],cex=1,pos=1, xpd=TRUE, srt=45)
text(3,-300, planilha,cex=1,pos=1, xpd=TRUE)
text(bp-0.15,50+(as.numeric(vtall[order(as.numeric(vtall[,2])),2])/2),
     vtall[order(as.numeric(vtall[,2])),2],cex=1.4,pos=1, xpd=TRUE, srt=90)
text(bp-0.15,50+(as.numeric(vtall[order(as.numeric(vtall[,2])),2])/2),
     vtall[order(as.numeric(vtall[,2])),2],cex=1.4,pos=1, xpd=TRUE, srt=90)

text(bp-0.15,350+(as.numeric(vtall[order(as.numeric(vtall[,2])),2])),
     sprintf ("%.f%%**", nvtall2[order(as.numeric(vtall[,2])),5]),cex=1,pos=1, xpd=TRUE, srt=40)
text(12,-380, c("** Valores % correspondentes a variação em relação ao período (2017-2020).") ,cex=0.8,pos=1, xpd=TRUE)

text(bp-0.15,200+(as.numeric(vtall[order(as.numeric(vtall[,2])),2])),
     sprintf ("%.f%%*", nvtall[order(as.numeric(vtall[,2])),5]),cex=1,pos=1, xpd=TRUE, srt=40)
text(12,-300, c("* Valores % correspondentes a variação em relação ao período (2016-2019).") ,cex=0.8,pos=1, xpd=TRUE)

#text(9,-340, sprintf ("A produção 2018-2021 do PPGCAP teve %.f%% em relação ao período (2016-2019).", (100-sum(as.numeric(vtall[,2]))*100/sum(pd2016_2019))*(-1)) ,cex=0.8,pos=1, xpd=TRUE)

#abline(h=120, col = "blue")


##############
contabiliza(topcapes)
vcapes <- matrix (NA, nc = 3, nr=length(profnames))
vcapes[,1] <- profnames
vcapes[,2] <- ret[,2]
vcapes[,3] <- cor
ttcapes <- vcapes[order(as.numeric(vcapes[,2])),]

bp <- barplot2(as.numeric(ttcapes[order(as.numeric(ttcapes[,2])),2]),
               beside = TRUE, horiz = FALSE,
               col = ttcapes[order(as.numeric(ttcapes[,2])),3],
               main = c("TopCapes 2018-2021")#,
               #ylim = c(0, 2000)
)
text(bp-0.5,-15, ttcapes[order(as.numeric(ttcapes[,2])),1],cex=1,pos=1, xpd=TRUE, srt=45)
text(3,-50, planilha,cex=1,pos=1, xpd=TRUE)
text(bp-0.15,50+(as.numeric(ttcapes[order(as.numeric(ttcapes[,2])),2])/2), 
      ttcapes[order(as.numeric(ttcapes[,2])),2],cex=1.4,pos=1, xpd=TRUE, srt=90)
#abline(h=120, col = "blue")


##############
contabiliza(top5)
vtprofpt5 <- ret
tpall <- vtprofpt5[order(as.numeric(vtprofpt5[,2])),]
top5nvtall <- data.frame (tpall[order(tpall[,1]),],top52016_2019, (100-(as.numeric(tpall[order(tpall[,1]),2])*100/as.numeric(top52016_2019)))*(-1))
top5nvtall2 <- data.frame (tpall[order(tpall[,1]),],top52017_2020, (100-(as.numeric(tpall[order(tpall[,1]),2])*100/as.numeric(top52017_2020)))*(-1))
#top5nvtall2[order(as.numeric(nvtall2[,4])),1:5]

bp <- barplot2(as.numeric(vtprofpt5[order(as.numeric(vtprofpt5[,2])),2]),
               #xlab = "Produção de cada professor do PPGCAP",
               beside = TRUE, horiz = FALSE,
               #names.arg = rep ("Prof",15),
               #ylab = "#Publicações",
               col = vtprofpt5[order(as.numeric(vtprofpt5[,2])),3],
               main = c("Pontuação Top5 2018-2021") #,ylim = c(0, 500)
)

text(3,-70, planilha,cex=1,pos=1, xpd=TRUE)
text(bp-0.5,-15, vtprofpt5[order(as.numeric(vtprofpt5[,2])),1],cex=1,pos=1, xpd=TRUE, srt=45)
text(bp-0.15,0+(as.numeric(vtprofpt5[order(as.numeric(vtprofpt5[,2])),2])/2), 
     vtprofpt5[order(as.numeric(vtprofpt5[,2])),2],cex=1.4,pos=1, xpd=TRUE, srt=90)

text(bp-0.15,50+(as.numeric(type.convert(top5nvtall[order(as.numeric(top5nvtall[,2])),2]))),
     sprintf ("%.f%%*", top5nvtall[order(as.numeric(top5nvtall[,2])),5]),cex=1,pos=1, xpd=TRUE, srt=40)
text(12,-70, c("* Valores % correspondentes a variação em relação ao período (2016-2019).") ,cex=0.8,pos=1, xpd=TRUE)

text(bp-0.15,80+(as.numeric(type.convert(top5nvtall2[order(as.numeric(top5nvtall2[,2])),2]))),
     sprintf ("%.f%%**", top5nvtall2[order(as.numeric(top5nvtall2[,2])),5]),cex=1,pos=1, xpd=TRUE, srt=40)
text(12,-90, c("** Valores % correspondentes a variação em relação ao período (2017-2020).") ,cex=0.8,pos=1, xpd=TRUE)

#abline(h=120, col = "blue")

##############
contabiliza(top10)
vtprofpt10 <- ret
top10nvtall <- data.frame (vtprofpt10[order(vtprofpt10[,1]),],top102016_2019, (100-(as.numeric(vtprofpt10[order(vtprofpt10[,1]),2])*100/as.numeric(top102016_2019)))*(-1))
top10nvtall2 <- data.frame (vtprofpt10[order(vtprofpt10[,1]),],top102017_2020, (100-(as.numeric(vtprofpt10[order(vtprofpt10[,1]),2])*100/as.numeric(top102017_2020)))*(-1))
#top10nvtall2[order(as.numeric(nvtall2[,4])),1:5]

bp <- barplot2(as.numeric(vtprofpt10[order(as.numeric(vtprofpt10[,2])),2]),
               beside = TRUE, horiz = FALSE,
               #names.arg = rep ("Prof",15),
               #ylab = "#Publicações",
               #xlab = "Produção de cada professor do PPGCAP",
               col = vtprofpt10[order(as.numeric(vtprofpt10[,2])),3],
               main = c("Pontuação Top10 2018-2021")#, ylim = c(0, 900)
)
text(3,-120, planilha,cex=1,pos=1, xpd=TRUE)
text(bp-0.5,-30, vtprofpt10[order(as.numeric(vtprofpt10[,2])),1],cex=1,pos=1, xpd=TRUE, srt=45)
text(bp-0.15,50+(as.numeric(vtprofpt10[order(as.numeric(vtprofpt10[,2])),2])/2), 
     vtprofpt10[order(as.numeric(vtprofpt10[,2])),2],cex=1.4,pos=1, xpd=TRUE, srt=90)

text(bp-0.15,80+(as.numeric(type.convert(top10nvtall[order(as.numeric(type.convert(top10nvtall[,2]))),2]))),
     sprintf ("%.f%%*", top10nvtall[order(as.numeric(type.convert(top10nvtall[,2]))),5]),cex=1,pos=1, xpd=TRUE, srt=40)
text(12,-100, c("* Valores % correspondentes a variação em relação ao período (2016-2019).") ,cex=0.8,pos=1, xpd=TRUE)

text(bp-0.15,135+(as.numeric(type.convert(top10nvtall2[order(as.numeric(type.convert(top10nvtall2[,2]))),2]))),
     sprintf ("%.f%%**", top10nvtall2[order(as.numeric(type.convert(top10nvtall2[,2]))),5]),cex=1,pos=1, xpd=TRUE, srt=40)
text(12,-130, c("** Valores % correspondentes a variação em relação ao período (2017-2020).") ,cex=0.8,pos=1, xpd=TRUE)

#abline(h=120, col = "blue")

##############

contabiliza(pub3par1)
vtpub3par1 <- ret
top31nvtall <- data.frame (vtpub3par1[order(vtpub3par1[,1]),],top312016_2019, (100-(as.numeric(vtpub3par1[order(vtpub3par1[,1]),2])*100/as.numeric(top312016_2019)))*(-1))
top31nvtall[is.na(top31nvtall)] <-0
top31nvtall[is.infinite(top31nvtall[,5]),5] <-0
top31nvtall2 <- data.frame (vtpub3par1[order(vtpub3par1[,1]),],top312017_2020, (100-(as.numeric(vtpub3par1[order(vtpub3par1[,1]),2])*100/as.numeric(top312017_2020)))*(-1))
top31nvtall2[is.na(top31nvtall2)] <-0
top31nvtall2[is.infinite(top31nvtall2[,5]),5] <-0


bp <- barplot2(as.numeric(vtpub3par1[order(as.numeric(vtpub3par1[,2])),2]),
               beside = TRUE, horiz = FALSE,
               #names.arg = rep ("Prof",15),
               #ylab = "#Publicações",
               #xlab = "Produção de cada professor do PPGCAP",
               col = vtpub3par1[order(as.numeric(vtpub3par1[,2])),3],
               main = c("Pontuação Pq1xPer. 3xEvt 2018-2021") #,ylim = c(0, 1700)
)

text(3,-200, planilha,cex=1,pos=1, xpd=TRUE)
text(bp-0.5,-60, vtpub3par1[order(as.numeric(vtpub3par1[,2])),1],cex=1,pos=1, xpd=TRUE, srt=45)
text(bp-0.15,50+(as.numeric(vtpub3par1[order(as.numeric(vtpub3par1[,2])),2])/2), 
     vtpub3par1[order(as.numeric(vtpub3par1[,2])),2],cex=1.4,pos=1, xpd=TRUE, srt=90)

text(bp-0.15,200+(as.numeric(type.convert(top31nvtall[order(as.numeric(type.convert(top31nvtall[,2]))),2]))),
     sprintf ("%.f%%*", top31nvtall[order(as.numeric(type.convert(top31nvtall[,2]))),5]),cex=1,pos=1, xpd=TRUE, srt=40)
text(12,-200, c("* Valores % correspondentes a variação em relação ao período (2016-2019).") ,cex=0.8,pos=1, xpd=TRUE)

text(bp-0.15,280+(as.numeric(type.convert(top31nvtall2[order(as.numeric(type.convert(top31nvtall2[,2]))),2]))),
     sprintf ("%.f%%**", top31nvtall2[order(as.numeric(type.convert(top31nvtall2[,2]))),5]),cex=1,pos=1, xpd=TRUE, srt=40)
text(12,-260, c("** Valores % correspondentes a variação em relação ao período (2017-2020).") ,cex=0.8,pos=1, xpd=TRUE)

#abline(h=120, col = "blue")

mediaProd <- c()
fmedias(length(profnames), vcapes)
print (mediaProd)
mediasCapes <- mediaProd

mediaProd <- c()
fmedias(length(profnames), vtprofpt5)
print (mediaProd)
mediasTop5 <- mediaProd

mediaProd <- c()
fmedias(length(profnames), vtprofpt10)
print (mediaProd)
mediasTop10 <- mediaProd

mediaProd <- c()
fmedias(length(profnames), vtpub3par1)
print (mediaProd)
mediasTopPq1x3 <- mediaProd

medianaProd <- c()
fmedianas(length(profnames), vcapes)
medianasCapes <- medianaProd

medianaProd <- c()
fmedianas(length(profnames), vtprofpt5)
medianasTop5 <- medianaProd

medianaProd <- c()
fmedianas(length(profnames), vtprofpt10)
medianasTop10 <- medianaProd

medianaProd <- c()
fmedianas(length(profnames), vtpub3par1)
medianasTopPq1x3 <- medianaProd

plotchar = c (17, 18, 19, 20)
linetype = c (1, 2, 3, 4)
color = brewer.pal(n = 4, name = "Dark2"); 

##########################

plot (mediasTopPq1x3, xlab = "Número de permanentes reclassificados (p/ colaborador + desligado)", ylab = "Média PPGCAP", ylim = c(0,10), lwd=1.5, col = color[1], lty = linetype[1], pch = plotchar[1], 
      xaxt = 'n', panel.first = grid())
lines (mediasTopPq1x3, lwd=1.5, col = color[1], lty = linetype[1], pch = plotchar[1])
lines(mediasTop5, type = "b", lwd=1.5, col = color[2], lty = linetype[2], pch = plotchar[2]) 
lines(mediasTop10, type = "b", lwd=1.5, col = color[3], lty = linetype[3], pch= plotchar[3]) 
lines(mediasCapes, type = "b", lwd=1.5, col = color[4], lty = linetype[4], pch= plotchar[4]) 
title("Médias Top5, Top10, Pq1x3 & Bottom (1-15)")
legend("bottomright", c("Pq1x3", "Top5", "Top10", "TopCapes"), col = color, pch=plotchar, lty=linetype)
#axis(1, at=sort (c(rep(1:16)), decreasing = TRUE),labels=c(rep(0:15)), las=1)
axis(1, at=sort (c(rep(1:16))),labels=c(rep(0:15)), las=1)
text(3,-1.2, planilha,cex=1,pos=1, xpd=TRUE)
#abline(v=9, col = "blue")

##########################

plot (medianasTopPq1x3, xlab = "Número de permanentes reclassificados (p/ colaborador + desligado)", ylab = "Medianas PPGCAP", ylim=c(0,10), lwd=1.5, col = color[1], lty = linetype[1], pch = plotchar[1], 
      xaxt = 'n', panel.first = grid()) 
lines (medianasTopPq1x3, lwd=1.5, col = color[1], lty = linetype[1], pch = plotchar[1])
lines(medianasTop5, type = "b", lwd=1.5, col = color[2], lty = linetype[2], pch = plotchar[2])
lines(medianasTop10, type = "b", lwd=1.5, col = color[3], lty = linetype[3], pch= plotchar[3])
lines(medianasCapes, type = "b", lwd=1.5, col = color[4], lty = linetype[4], pch= plotchar[4])
title("Medianas Top5, Top10, Pq1x3 & Bottom (1-15)")
legend("bottomright", c("Pq1x3", "Top5", "Top10", "TopCapes"), col = color, pch=plotchar, lty=linetype)
axis(1, at=sort (c(rep(1:16))),labels=c(rep(0:15)), las=1)
text(3,-1.2, planilha,cex=1,pos=1, xpd=TRUE)
#abline(v=3, col = "blue")
#abline(v=5, col = "blue")

##########################
####################################################################

#Draw the boxplot, with the number of individuals per group
boxCapes <- sort(as.numeric(vcapes[,2]))
boxplot(
        boxCapes[1:15],boxCapes[2:15],boxCapes[3:15],boxCapes[4:15],boxCapes[5:15],
        boxCapes[6:15],boxCapes[7:15],boxCapes[8:15],boxCapes[9:15],boxCapes[10:15],
        boxCapes[11:15],boxCapes[12:15],boxCapes[13:15],boxCapes[14:15],boxCapes[15:15], 
        col=cor, ylim = c(0,max(boxCapes)),
        xlab = "Número de permanentes reclassificados (p/ colaborador + desligado)")
title("TopCapes 2018-2021")
grid (NA,10,col = "black")
axis(1, at=sort (c(rep(1:16))),labels=c(rep(0:15)), las=1)
text(3,-50, planilha,cex=1,pos=1, xpd=TRUE)

##########################
box5 <- sort(as.numeric(vtprofpt5[,2]))
boxplot(
        box5[1:15],box5[2:15],box5[3:15],box5[4:15],box5[5:15],
        box5[6:15],box5[7:15],box5[8:15],box5[9:15],box5[10:15],
        box5[11:15],box5[12:15],box5[13:15],box5[14:15],box5[15:15], 
        col=cor, ylim = c(0,max(box5)),
        xlab = "Número de permanentes reclassificados (p/ colaborador + desligado)")
title("Top5 2018-2021")
grid (NA,10,col = "black")
axis(1, at=sort (c(rep(1:16))),labels=c(rep(0:15)), las=1)
text(3,-70, planilha,cex=1,pos=1, xpd=TRUE)

##########################
box10 <- sort(as.numeric(vtprofpt10[,2]))
boxplot(
        box10[1:15],box10[2:15],box10[3:15],box10[4:15],box10[5:15],
        box10[6:15],box10[7:15],box10[8:15],box10[9:15],box10[10:15],
        box10[11:15],box10[12:15],box10[13:15],box10[14:15],box10[15:15], 
        col=cor,ylim = c(0,max(box10)),
        xlab = "Número de permanentes reclassificados (p/ colaborador + desligado)")
title("Top10 2018-2021")
grid (NA,10,col = "black")
axis(1, at=sort (c(rep(1:16))),labels=c(rep(0:15)), las=1)
text(3,-120, planilha,cex=1,pos=1, xpd=TRUE)

##########################
boxPq <- sort(as.numeric(vtpub3par1[,2]))
boxplot(
        boxPq[1:15],boxPq[2:15],boxPq[3:15],boxPq[4:15],boxPq[5:15],
        boxPq[6:15],boxPq[7:15],boxPq[8:15],boxPq[9:15],boxPq[10:15],
        boxPq[11:15],boxPq[12:15],boxPq[13:15],boxPq[14:15],boxPq[15:15], 
        col=cor, ylim = c(0,max(boxPq)),
        xlab = "Número de permanentes reclassificados (p/ colaborador + desligado)")
grid (NA,10,col = "black")
title("TopPq13E 2018-2021")
axis(1, at=sort (c(rep(1:16))),labels=c(rep(0:15)), las=1)
text(3,-220, planilha,cex=1,pos=1, xpd=TRUE)


dev.off()

