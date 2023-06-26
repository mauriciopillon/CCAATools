####### Functions ######

fmedianas <- function (nprof, producao) {
  #median(sort(as.numeric(producao[,2])))
  mytmp <- sort(as.numeric(producao[1:nprof,2]))
  producao <- c()
  for (outi in 1:nprof) {
    #print(outi)
    #print(mytmp[outi:nprof])
    #print(median(sort(as.numeric(mytmp[outi:nprof]))))
    mt1 <- median(sort(as.numeric(mytmp[outi:nprof])))
    m1 <- (mt1/mytmp[nprof])*10
    #print ("oi")
    #print (mt1)
    #print (mytmp[nprof])
    #print (m1)
    producao <- c(producao, m1)
  }
  medianaProd <<- producao
}


fmedias <- function (nprof, producao) {
  #mean(sort(as.numeric(producao[,2])))
  mytmp <- sort(as.numeric(producao[1:nprof,2]))
  producao <- c()
  for (outi in 1:nprof) {
    #print(outi)
    print(mytmp[outi:nprof])
    #print(mean(sort(as.numeric(mytmp[outi:nprof]))))
    m1 <- ((mean(sort(as.numeric(mytmp[outi:nprof]))))/mytmp[nprof])*10
    print (m1)
    producao <- c(producao, m1)
  }
  mediaProd <<- producao
}

top <- function (topnro, prod, nprof, prioridade) {
  ctprod <<- 0
  profSum <- c()
  
  #print (prod)
  print (profnames [nprof])
  for (sumii in 1:18) {
    profSum <- c (profSum, sum(as.numeric(prod[sumii,])))
    # print (profSum[sumii])
  }
  profSum <- c (profSum, cor[nprof])
  allprodprof <- profSum
  
  print ("Produção Total:")
  print ("%%%%%%%%%%%%%%%%%%%%")
  # print (topnro)
  print (allprodprof)
  
  for (tii in prioridade) {
    if (ctprod > (topnro - 1)) {
      profSum[tii] <- 0
    } else {
      if (as.numeric(profSum[tii]) > 0) {
        #print (sprintf("%s %s (%s)", profnames [nprof], p2017$Qualis[tii], profSum[tii]))
        ctprod <<- as.numeric(profSum[tii]) + ctprod
        #print (sprintf("%s (%d) %d: (Nao sou zero)", top5prod[nprof,1], tii, ctprod))
        if (ctprod > (topnro - 1)) {
          temp <- ctprod - topnro
          #print (sprintf("%s ind(%d) %d %d", profSum[tii], (topnro - 1), ctprod, temp))
          profSum[tii] <- as.numeric(profSum[tii]) - temp
          print (sprintf("%s %s (%s)", profnames [nprof], pAno1$Qualis[tii], profSum[tii]))
        }
      }
    }
  }
  # print ("## DEB 3 ###")
  # profSum[18] <- cor[nprof]
  # print (profSum)
  
  # print (sprintf("%s (Produção total): ",profnames [nprof], profSum))
  
  topprod[nprof,] <<- c(profnames [nprof], profSum)
  #topprod[nprof,] <<- c(cor [nprof], profSum)
  allprod[nprof,] <<- c(profnames [nprof], allprodprof)
  #print (allprodprof[nprof])
  print ("Produção Top:")
  print (profSum)
}

topcapes <- function () {
  for (tprof in 4:(length(profnames)+3)) {
    trienio <<- data.frame(pAno1[1:18,tprof])
    top(1,trienio,(tprof - 3),vpriori)
  }
  tmpcapes1 <- topprod
  for (tprof in 4:(length(profnames)+3)) {
    trienio <<- data.frame(pAno2[1:18,tprof])
    top(1,trienio,(tprof - 3),vpriori)
  }
  tmpcapes2 <- topprod
  for (tprof in 4:(length(profnames)+3)) {
    trienio <<- data.frame(pAno3[1:18,tprof])
    top(1,trienio,(tprof - 3),vpriori)
  }
  tmpcapes3 <- topprod
  for (tprof in 4:(length(profnames)+3)) {
    trienio <<- data.frame(pAno4[1:18,tprof])
    top(1,trienio,(tprof - 3),vpriori)
  }
  tmpcapes4 <- topprod
  
  tmpcapes1 <- matrix(as.numeric(tmpcapes1[,2:19]), ncol=18)
  tmpcapes2 <- matrix(as.numeric(tmpcapes2[,2:19]), ncol=18)
  tmpcapes3 <- matrix(as.numeric(tmpcapes3[,2:19]), ncol=18)
  tmpcapes4 <- matrix(as.numeric(tmpcapes4[,2:19]), ncol=18)
  tmpcapes0 <- tmpcapes1+tmpcapes2+tmpcapes3+tmpcapes4
  topcapes <<- data.frame(profnames,tmpcapes0,cor)
  # colnames(topcapes) <<- unlist (c("Prof",
  #                                  "A1","A2","B1","B2","B3","B4","B5","NC",
  #                                  "A1","A2","B1","B2","B3","B4","B5","NC","Cor"))
  
  colnames(topcapes) <<- unlist (c("Prof",vtposicao,"Cor"))
  
}


extrairtop <- function (topnro) {
  for (tprof in 4:(length(profnames)+3)) {
    trienio <<- data.frame(pAno1[1:18,tprof],
                           pAno2[1:18,tprof],
                           pAno3[1:18,tprof],
                           pAno4[1:18,tprof])
    top(topnro,trienio,(tprof - 3),vpriori)
  }
}

contabiliza <- function (producao) {
  ret <<- matrix (NA, nc = 3, nr=length(profnames))
  for (npro in 1:length(profnames)) {
    profpt <- 0
    for (pt in 2:19) {
      profpt <- profpt + (as.numeric(producao[npro,pt]) * tabpesos[(pt-1)])
    } 
    print (profpt)
    ret[npro,] <<- c (producao[npro,1], profpt, producao[npro,20])
  }
  #print(vtall)
}

