### CCAA: Comissão de Credenciamento/Des/Re Acompanhamento e Autoavaliação 
### Coordenação PPGCA:
###      Isabela Gasparini
###      Guilherme Koslovski
### Comissão:
###      Avanilde Kemczinski
###      Marcelo Hounsell
###      Maurício Pillon

########################################
# Definições:
### período de 4 anos: 2017-2020 (até 31/12/2020)
### Ordem de prioridade para coleta das melhores produções:
   ### P.A1, E.A1, P.A2, E.A2, P.A3, E.A3 ...
### P/ Pq1x3E: Somente periódico restrito libera evento!
### Qualis 2020 (A1,A2,A3,A4,B1,B2,B3,B4)
#########################################

######
# Script R desenvolvido por:
### Mauricio Pillon
# Autorizo uso, cópia e modificação, desde que as melhorias sejam disponibilizadas publicamente.
### 

library("readxl")
library(gplots)
library(plotly)
library(plotrix)
library(RColorBrewer)
source ("mcalibQ20.R")

planilha <- "jcr/Produção Individual 2020-teste.xlsx"

mysheets <- excel_sheets(planilha)
p2020 <- read_excel(planilha, sheet="2020")
p2017 <- read_excel(planilha, sheet="2017")
p2018 <- read_excel(planilha, sheet="2018")
p2019 <- read_excel(planilha, sheet="2019")

#remover submenu, B5 (periodico e eventos), valores Qualis 2016
p2017 <- p2017[c(-1, -10, -20),seq (3, 34, 2)*-(1)]
p2018 <- p2018[c(-1, -10, -20),seq (3, 34, 2)*-(1)]
p2019 <- p2019[c(-1, -10, -20),seq (3, 34, 2)*-(1)]
p2020 <- p2020[c(-1, -10, -20),seq (3, 34, 2)*-(1)]

legQualis <- c ("A1","A2","A3","A4","B1","B2","B3","B4","NC")
vtposicao <- c ("P.A1","P.A2","P.A3","P.A4","P.B1","P.B2","P.B3","P.B4","P.NC",
                "E.A1","E.A2","E.A3","E.A4","E.B1","E.B2","E.B3","E.B4","E.NC")

#                 1,    2,   3,   4,   5,   6,   7,    8, 9, 
tabpesos <- c  ( 100,	87.5,	75,	62.5,	50,	37.5,	25,	12.5,	0,
                 100,	87.5,	75,	62.5,	50,	37.5,	25,	12.5,	0)

# P.A1, E.A1, P.A2, E.A2, P.A3, E.A3, P.A4, E.A4, P.B1, E.B1, P.B2, E.B2, P.B3, E.B3, P.B4, E.B4, P.NC, E.NC
vpriori <- c (1,10, 2,11, 3,12, 4,13, 5,14, 6,15, 7,16, 8,17, 9,18)

# profnames <- c ("Adriano", "André", "Avanilde", "Carla",
#                 "Charles", "Cristiano", "Fabiano", "Guilherme",
#                 "Isabela", "Janine", "Marcelo", "Maurício", "Parpinelli",
#                 "Obelheiro", "Rosso")

profnames <- c ("A", "C", "B", "V",
                "W", "I", "F", "K",
                "M", "Q", "V", "X", "D",
                "H", "P")


# cores dos professores
cor <- c(brewer.pal(n = 10, name = "Set3"),brewer.pal(n = length(profnames)-10, name = "Spectral"))

######## Main Procedure #####
# 
# nc = nro de categorias do Qualis (vtposicao) + nome do professor + cor
# nr = nro de professores
topprod <- matrix (NA, nc = length(vtposicao)+2, nr=length(profnames))
allprod <- matrix (NA, nc = length(vtposicao)+2, nr=length(profnames))
colnames(topprod) <- unlist (c("Prof",vtposicao,"Cor"))
colnames(allprod) <- unlist (c("Prof",vtposicao,"Cor"))

# tprof (Adriano[4], André[5] ... )
#for (tprof in 4:18) {
for (tprof in 1:length(profnames)) {
  # qualquer periódico 1:8
  # nrper <- sum (p2020[1:8,tprof],p2017[1:8,tprof],p2018[1:8,tprof],p2019[1:8,tprof])
  # somente periódico restrito 1:4 (A1, A2, A3, A4)
  print (profnames[tprof])
  nrper <- sum (as.numeric(unlist(p2017[1:4,(tprof+3)])),
                as.numeric(unlist(p2018[1:4,(tprof+3)])),
                as.numeric(unlist(p2019[1:4,(tprof+3)])),
                as.numeric(unlist(p2020[1:4,(tprof+3)])))
  nevt <- as.numeric (nrper * 3)
  sop <<- data.frame(
    p2017[1:length(vtposicao),(tprof+3)],
    p2018[1:length(vtposicao),(tprof+3)],
    p2019[1:length(vtposicao),(tprof+3)],
    p2020[1:length(vtposicao),(tprof+3)])
  # Eventos E.A1, E.A2, E.B1, E.B2, E.B3, E.B4, E.B5, E.NC
  eventos <- c (10,11,12,13,14,15,16,17,18)
  top(nevt,sop,tprof,eventos)
  # soperiodico(sop,(tprof - 3))
}
pub3par1 <- topprod
# apagar periódico B1-NC
pub3par1[,6:9] <- 0

################ Extraindo Melhores publicacoes (top5 e 10) ##############

topcapes()

extrairtop(5)
top5 <- topprod

extrairtop(10)
top10 <- topprod

############################################
### Gráficos ####
pdf ("PDFs/ccaa-prodQ2020.pdf", width = 11)
par(mfrow = c(1,1))

##############
contabiliza(allprod)
vtall <- ret
tpall <- vtall[order(as.numeric(vtall[,2])),]

bp <- barplot2(as.numeric(vtall[order(as.numeric(vtall[,2])),2]),
               beside = TRUE, horiz = FALSE,
               col = vtall[order(as.numeric(vtall[,2])),3],
               #col = cor,
               main = c("Pontuação Produção Completa 2017-2020 (Qualis 2020)")#,
               #ylim = c(0, 2000)
)
text(bp-0.5,-75, vtall[order(as.numeric(vtall[,2])),1],cex=1,pos=1, xpd=TRUE, srt=45)
text(2,-250, planilha,cex=1,pos=1, xpd=TRUE)
text(bp-0.15,50+(as.numeric(vtall[order(as.numeric(vtall[,2])),2])/2),
     vtall[order(as.numeric(vtall[,2])),2],cex=1.4,pos=1, xpd=TRUE, srt=90)
abline(h=120, col = "blue")


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
               main = c("TopCapes 2017-2020 (Qualis 2020)")#,
               #ylim = c(0, 2000)
)
text(bp-0.5,-15, ttcapes[order(as.numeric(ttcapes[,2])),1],cex=1,pos=1, xpd=TRUE, srt=45)
text(2,-50, planilha,cex=1,pos=1, xpd=TRUE)
text(bp-0.15,50+(as.numeric(ttcapes[order(as.numeric(ttcapes[,2])),2])/2), 
      ttcapes[order(as.numeric(ttcapes[,2])),2],cex=1.4,pos=1, xpd=TRUE, srt=90)
abline(h=120, col = "blue")


##############
contabiliza(top5)
vtprofpt5 <- ret
tpall <- vtprofpt5[order(as.numeric(vtprofpt5[,2])),]
bp <- barplot2(as.numeric(vtprofpt5[order(as.numeric(vtprofpt5[,2])),2]),
               #xlab = "Produção de cada professor do PPGCA",
               beside = TRUE, horiz = FALSE,
               #names.arg = rep ("Prof",15),
               #ylab = "#Publicações",
               col = vtprofpt5[order(as.numeric(vtprofpt5[,2])),3],
               main = c("Pontuação Top5 2017-2020 (Qualis 2020)") #,ylim = c(0, 500)
)

text(2,-50, planilha,cex=1,pos=1, xpd=TRUE)
text(bp-0.5,-15, vtprofpt5[order(as.numeric(vtprofpt5[,2])),1],cex=1,pos=1, xpd=TRUE, srt=45)
text(bp-0.15,50+(as.numeric(vtprofpt5[order(as.numeric(vtprofpt5[,2])),2])/2), 
     vtprofpt5[order(as.numeric(vtprofpt5[,2])),2],cex=1.4,pos=1, xpd=TRUE, srt=90)
abline(h=120, col = "blue")

##############
contabiliza(top10)
vtprofpt10 <- ret
bp <- barplot2(as.numeric(vtprofpt10[order(as.numeric(vtprofpt10[,2])),2]),
               beside = TRUE, horiz = FALSE,
               #names.arg = rep ("Prof",15),
               #ylab = "#Publicações",
               #xlab = "Produção de cada professor do PPGCA",
               col = vtprofpt10[order(as.numeric(vtprofpt10[,2])),3],
               main = c("Pontuação Top10 2017-2020 (Qualis 2020)")#, ylim = c(0, 900)
)
text(2,-90, planilha,cex=1,pos=1, xpd=TRUE)
text(bp-0.5,-30, vtprofpt10[order(as.numeric(vtprofpt10[,2])),1],cex=1,pos=1, xpd=TRUE, srt=45)
text(bp-0.15,50+(as.numeric(vtprofpt10[order(as.numeric(vtprofpt10[,2])),2])/2), 
     vtprofpt10[order(as.numeric(vtprofpt10[,2])),2],cex=1.4,pos=1, xpd=TRUE, srt=90)
abline(h=120, col = "blue")

##############

contabiliza(pub3par1)
vtpub3par1 <- ret
bp <- barplot2(as.numeric(vtpub3par1[order(as.numeric(vtpub3par1[,2])),2]),
               beside = TRUE, horiz = FALSE,
               #names.arg = rep ("Prof",15),
               #ylab = "#Publicações",
               #xlab = "Produção de cada professor do PPGCA",
               col = vtpub3par1[order(as.numeric(vtpub3par1[,2])),3],
               main = c("Pontuação Pq1xPer. 3xEvt 2017-2020 (Qualis 2020)") #,ylim = c(0, 1700)
)

text(2,-200, planilha,cex=1,pos=1, xpd=TRUE)
text(bp-0.5,-60, vtpub3par1[order(as.numeric(vtpub3par1[,2])),1],cex=1,pos=1, xpd=TRUE, srt=45)
text(bp-0.15,50+(as.numeric(vtpub3par1[order(as.numeric(vtpub3par1[,2])),2])/2), 
     vtpub3par1[order(as.numeric(vtpub3par1[,2])),2],cex=1.4,pos=1, xpd=TRUE, srt=90)
abline(h=120, col = "blue")

mediaProd <- c()
fmedias(15, vcapes)
print (mediaProd)
mediasCapes <- mediaProd

mediaProd <- c()
fmedias(15, vtprofpt5)
print (mediaProd)
mediasTop5 <- mediaProd

mediaProd <- c()
fmedias(15, vtprofpt10)
print (mediaProd)
mediasTop10 <- mediaProd

mediaProd <- c()
fmedias(15, vtpub3par1)
print (mediaProd)
mediasTopPq1x3 <- mediaProd

medianaProd <- c()
fmedianas(15, vcapes)
medianasCapes <- medianaProd

medianaProd <- c()
fmedianas(15, vtprofpt5)
medianasTop5 <- medianaProd

medianaProd <- c()
fmedianas(15, vtprofpt10)
medianasTop10 <- medianaProd

medianaProd <- c()
fmedianas(15, vtpub3par1)
medianasTopPq1x3 <- medianaProd

plotchar = c (17, 18, 19, 20)
linetype = c (1, 2, 3, 4)
color = brewer.pal(n = 4, name = "Dark2"); 

##########################

plot (mediasTopPq1x3, xlab = "Número de permanentes reclassificados (p/ colaborador + desligado)", ylab = "Média PPGCA", ylim = c(0,10), lwd=1.5, col = color[1], lty = linetype[1], pch = plotchar[1], 
      xaxt = 'n', panel.first = grid())
lines (mediasTopPq1x3, lwd=1.5, col = color[1], lty = linetype[1], pch = plotchar[1])
lines(mediasTop5, type = "b", lwd=1.5, col = color[2], lty = linetype[2], pch = plotchar[2]) 
lines(mediasTop10, type = "b", lwd=1.5, col = color[3], lty = linetype[3], pch= plotchar[3]) 
lines(mediasCapes, type = "b", lwd=1.5, col = color[4], lty = linetype[4], pch= plotchar[4]) 
title("Médias Top5, Top10, Pq1x3 & Bottom (1-15)")
legend("bottomright", c("Pq1x3", "Top5", "Top10", "TopCapes"), col = color, pch=plotchar, lty=linetype)
#axis(1, at=sort (c(rep(1:16)), decreasing = TRUE),labels=c(rep(0:15)), las=1)
axis(1, at=sort (c(rep(1:16))),labels=c(rep(0:15)), las=1)
text(2,-1.2, planilha,cex=1,pos=1, xpd=TRUE)
#abline(v=9, col = "blue")

##########################

plot (medianasTopPq1x3, xlab = "Número de permanentes reclassificados (p/ colaborador + desligado)", ylab = "Medianas PPGCA", ylim=c(0,10), lwd=1.5, col = color[1], lty = linetype[1], pch = plotchar[1], 
      xaxt = 'n', panel.first = grid()) 
lines (medianasTopPq1x3, lwd=1.5, col = color[1], lty = linetype[1], pch = plotchar[1])
lines(medianasTop5, type = "b", lwd=1.5, col = color[2], lty = linetype[2], pch = plotchar[2])
lines(medianasTop10, type = "b", lwd=1.5, col = color[3], lty = linetype[3], pch= plotchar[3])
lines(medianasCapes, type = "b", lwd=1.5, col = color[4], lty = linetype[4], pch= plotchar[4])
title("Medianas Top5, Top10, Pq1x3 & Bottom (1-15)")
legend("bottomright", c("Pq1x3", "Top5", "Top10", "TopCapes"), col = color, pch=plotchar, lty=linetype)
axis(1, at=sort (c(rep(1:16))),labels=c(rep(0:15)), las=1)
text(2,-1.2, planilha,cex=1,pos=1, xpd=TRUE)
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
title("TopCapes 2017-2020")
grid (NA,10,col = "black")
axis(1, at=sort (c(rep(1:16))),labels=c(rep(0:15)), las=1)
text(2,-50, planilha,cex=1,pos=1, xpd=TRUE)

##########################
box5 <- sort(as.numeric(vtprofpt5[,2]))
boxplot(
        box5[1:15],box5[2:15],box5[3:15],box5[4:15],box5[5:15],
        box5[6:15],box5[7:15],box5[8:15],box5[9:15],box5[10:15],
        box5[11:15],box5[12:15],box5[13:15],box5[14:15],box5[15:15], 
        col=cor, ylim = c(0,max(box5)),
        xlab = "Número de permanentes reclassificados (p/ colaborador + desligado)")
title("Top5 2017-2020")
grid (NA,10,col = "black")
axis(1, at=sort (c(rep(1:16))),labels=c(rep(0:15)), las=1)
text(2,-60, planilha,cex=1,pos=1, xpd=TRUE)

##########################
box10 <- sort(as.numeric(vtprofpt10[,2]))
boxplot(
        box10[1:15],box10[2:15],box10[3:15],box10[4:15],box10[5:15],
        box10[6:15],box10[7:15],box10[8:15],box10[9:15],box10[10:15],
        box10[11:15],box10[12:15],box10[13:15],box10[14:15],box10[15:15], 
        col=cor,ylim = c(0,max(box10)),
        xlab = "Número de permanentes reclassificados (p/ colaborador + desligado)")
title("Top10 2017-2020")
grid (NA,10,col = "black")
axis(1, at=sort (c(rep(1:16))),labels=c(rep(0:15)), las=1)
text(2,-100, planilha,cex=1,pos=1, xpd=TRUE)

##########################
boxPq <- sort(as.numeric(vtpub3par1[,2]))
boxplot(
        boxPq[1:15],boxPq[2:15],boxPq[3:15],boxPq[4:15],boxPq[5:15],
        boxPq[6:15],boxPq[7:15],boxPq[8:15],boxPq[9:15],boxPq[10:15],
        boxPq[11:15],boxPq[12:15],boxPq[13:15],boxPq[14:15],boxPq[15:15], 
        col=cor, ylim = c(0,max(boxPq)),
        xlab = "Número de permanentes reclassificados (p/ colaborador + desligado)")
grid (NA,10,col = "black")
title("TopPq13E 2017-2020")
axis(1, at=sort (c(rep(1:16))),labels=c(rep(0:15)), las=1)
text(2,-220, planilha,cex=1,pos=1, xpd=TRUE)


dev.off()
