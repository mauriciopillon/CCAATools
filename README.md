### CCAA2021: Comissão de Credenciamento/Des/Re Acompanhamento e Autoavaliação 
### Coordenação PPGCA:
###      Charles Christian Miers
### Comissão:
###      Avanilde Kemczinski
###      Adriano Fiorese
###      Isabela Gasparini
###      Maurício Pillon
###
### Alterações 2021:
###    - Número de professores na base de cálculo (14);
###    - Adequação ao formato da nova planilha de entrada PPGCAP-ProducaoDocente_teste-2021.xls


### CCAA2020: Comissão de Credenciamento/Des/Re Acompanhamento e Autoavaliação 
### Coordenação PPGCA:
###      Guilherme Koslovski
### Comissão:
###      Isabela Gasparini
###      Avanilde Kemczinski
###      Marcelo Hounsell
###      Maurício Pillon
###
### Desenvolvido por: Mauricio Pillon
### Versão: Junho/2020
### Em caso de inconsistência nos cálculos, favor entrar em contato por e-mail, indicando o erro.

A ferramenta de análise dos dados da autoavaliação foi desenvolvida em R. Atualmente, a ferramenta é composta por 4 arquivos, sendo dois, bibliotecas de funções (mcalibQ16.R e mcalibQ20.R) e, dois scripts principais (prof-prodQ16.R e prof-prodQ20.R). A ferramenta baseia-se na mesma fonte de dados, planilha compartilhada com produção dos professores do PPGCA. Na versão 2020, a comissão disponibilizou ferramentas para o cômputo das produções de acordo com o Qualis 2016 (prof-prodQ20.R) e o Qualis 2020 (prof-prodQ20.R).

Qualis 2016
########################################
# Definições:
### período de 4 anos: 2017-2020 (até 31/12/2020)
### Periódico tem bonus de 25%
### Ordem de prioridade para coleta das melhores produções:
   ### P.A1, E.A1, P.A2, E.A2, P.B1, E.B1 ...
### P/ Pq1x3E: Somente periódico restrito libera evento!
### Qualis 2016 (A1,A2,B1,B2,B3,B4,B5)
#########################################


Qualis 2020:
########################################
# Definições:
### período de 4 anos: 2017-2020 (até 31/12/2020)
### Ordem de prioridade para coleta das melhores produções:
   ### P.A1, E.A1, P.A2, E.A2, P.A3, E.A3 ...
### P/ Pq1x3E: Somente periódico restrito libera evento!
### Qualis 2020 (A1,A2,A3,A4,B1,B2,B3,B4)
#########################################

INSTALAÇÃO DO AMBIENTE:
   https://cran.r-project.org/doc/manuals/r-release/R-admin.html

DEPENDÊNCIAS BIBLIOTECAS 
    library("readxl")
    library(gplots)
    library(plotly)
    library(plotrix)
    library(RColorBrewer)


DESCRIÇÃO DOS ARQUIVOS:
   Raiz: fontes do R
   jcr/: planilhas com dados de entrada (formato da planilha 2020 disponibilizada pela Coordenação)
   PDFs/: gráficos gerados pelos scripts.
    

EXECUÇÃO (para gerar os gráficos):
   <console>$ Rscript prof-prodQ20.R
   <console>$ Rscript prof-prodQ16.R   


R for Beginners:
   https://cran.r-project.org/doc/contrib/Paradis-rdebuts_en.pdf
