#####Definindo diretório de trabalho, caso tenha que trabalhar em Windows, acertar o diretório###

setwd("/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/")


#############################################################################################################
#############################################################################################################
###################################DEFININDO A SEMANA EPIDEMIOLÓGICA#########################################
#######################################E DATA DA BASE DBF####################################################
#############################################################################################################

###Definir fonte para ser utilizada nos gráficos (ggplot ira buscar o objeto Fonte para "labs(caption = Fonte...")###

Fonte <- "SINAN. BASE DBF: Acesso em 04/11/2022"

###Objeto SE irá ser utilizado como auxiliar definidor de ponto, a partir do qual, os histogramas de casos Notificados/Confirmados/Prováveis
###nas últimas 10 semanas irá buscar os dados.###

SE <- as.data.frame("48")

SE <- as.numeric(SE)

#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################

library(patchwork)
library(foreign)
library (dplyr)
library (googlesheets4)
library (ggplot2)
###Não sei usar o httpuv!!!###
library (httpuv)

####Importando as tabelas da Tabulação Primária para construção das tabelas base do Informe Epidemiológico ###

BASE_IBGE<-read.table(file="/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/CSV/Planilha_Base_IBGE.csv", 
                      header=TRUE, 
                      sep=",")

DENGON2022 <- read.dbf(file = "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Arboviroses/DBF/DENGON2022.dbf", 
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, SEM_NOT, DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, DT_INTERNA, DT_OBITO, DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, ALRM_VOM, ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST, GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)

DENGON2023 <- read.dbf(file = "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Arboviroses/DBF/DENGON2023.dbf", 
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, SEM_NOT, DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, DT_INTERNA, DT_OBITO, DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, ALRM_VOM, ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST, GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)

RS22_Serie_Historica_Base <- read.csv(file = "RS22_Serie_Historica_Base.csv",
                                      header = TRUE,
                                      sep = ",")

RS22_CE_Notificados_Base <- read.csv(file = "RS22_CE_Notificados_Base.csv",
                                     header = TRUE,
                                     sep = ",")

RS22_CE_Confirmados_Base <- read.csv(file = "RS22_CE_Confirmados_Base.csv",
                                     header = TRUE,
                                     sep = ",")

#####################################################################################################################
###########
###Transformando coluna de semana epidemiológica de fator para numérica (passando por texto, se for direto, ela transforma ###200905 em 06. Seria possível realizar busca de SE passando direto de fator para numérica utilizando as.integer(DENGON2009 ###%>% filter(ID_MUNICIP == 410165, SEM_PRI == 6) -1, para buscar SE 05?
##Será usado para buscar semanas epidemiológicas
##

DENGON2022$SEM_PRI <-as.numeric(as.character(DENGON2022$SEM_PRI))

DENGON2023$SEM_PRI <-as.numeric(as.character(DENGON2023$SEM_PRI))

######################################################################################################################

#####################################################################################################################
########################################         2022/23        ###################################################
#####################################################################################################################

#####################################################################################################################
######### A partir deste ponto do script, o código terá como função, além de criar a ################################
######### a base do período sazonal atual, servirá para, via ctrl -c / ctrl -v, ser  ################################
######### incluído no script contido na Tabulação GERAL para servir de base histórica################################
######### do Programa Regional de Controle das Arboviroses da 22ª RS. Falta trabalhar################################
######### com a BASE DBF Chikungunya e Zika. ########################################################################
#####################################################################################################################

################ O código a ser copiado para a Tabulação Geral irá até o ponto determinado com FIM ##################
#####################################################################################################################

####Tabela de notificações SINAN###

RS22_22_23_AUX01 <- DENGON2022 %>% 
  filter(ID_REGIONA == 1376 | ID_RG_RESI == 1376, 
         SEM_PRI >= 202231)

RS22_22_23_AUX02 <- DENGON2023 %>% 
  filter(ID_REGIONA == 1376 | ID_RG_RESI == 1376, 
         SEM_PRI <=202330)

RS22_22_23_SINAN <- rbind(RS22_22_23_AUX01, RS22_22_23_AUX02)

rm(RS22_22_23_AUX01, RS22_22_23_AUX02)

###Removendo tabela DENGON2021 já utilizada###

rm (DENGON2022)
rm (DENGON2023)

###Construindo um for loop para realizar a tabela de notificados por semana epidemiológica###

RS22_22_23_SE_Notificados <- matrix(data = NA, 
                                    nrow = 16, 
                                    ncol = 54)

RS22_22_23_SE_Notificados <- as.data.frame(RS22_22_23_SE_Notificados)

colnames(RS22_22_23_SE_Notificados)[1] <- "Município" 

RS22_22_23_SE_Notificados[,1] <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

colnames (RS22_22_23_SE_Notificados)[2:24] <- c(31:53)

colnames (RS22_22_23_SE_Notificados)[25:54] <- c(1:30)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 2] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i,
                                                                                             SEM_PRI ==202231)%>%
                                                                                      count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 3] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             SEM_PRI ==202232) %>% 
                                                                                      count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 4] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                      filter(ID_MN_RESI == i,
                                                                                             SEM_PRI ==202233) %>% 
                                                                                      count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i),5] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                     filter(ID_MN_RESI == i,
                                                                                            SEM_PRI ==202234) %>% 
                                                                                     count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 6] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                      filter(ID_MN_RESI == i,
                                                                                             SEM_PRI ==202235) %>% 
                                                                                      count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 7] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             SEM_PRI ==202236) %>%
                                                                                      count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 8] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             SEM_PRI ==202237) %>% 
                                                                                      count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 9] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             SEM_PRI ==202238) %>% 
                                                                                      count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 10] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202239) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 11] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202240) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 12] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202241) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 13] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202242) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 14] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202243) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 15] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202244) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 16] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202245) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 17] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202246) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 18] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202247) %>%  
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 19] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202248) %>%      
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 20] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202249) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i),  21] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                        filter(ID_MN_RESI == i, 
                                                                                               SEM_PRI ==202250) %>%
                                                                                        count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 22] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202251) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 23] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202252) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 24] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202253) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 25] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202301) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 26] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202302) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 27] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202303) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 28] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202304) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 29] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202305) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 30] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202306) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 31] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202307) %>% 
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 32] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202308) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 33] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202309) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 34] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202310) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 35] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202311) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 36] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202312) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 37] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202313) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 38] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202314) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 39] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202315) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 40] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202316) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 41] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202317) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 42] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202318) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 43] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202319) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 44] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202320) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 45] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202321) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 46] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202322) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 47] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202323) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 48] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202324) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 49] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202325) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 50] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202326) %>% 
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 51] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202327) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 52] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202328) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 53] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202329) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 54] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202330) %>%
                                                                                       count() 
  )
}

RS22_22_23_SE_Notificados[,1] <- BASE_IBGE[which(BASE_IBGE$RS == 22), 3]

RS22_22_23_SE_Notificados[17,2:54] <- apply(RS22_22_23_SE_Notificados[,2:54], 2, sum)

RS22_22_23_SE_Notificados[17,1] <- "Total"


###Construindo um for loop para realizar a tabela de Confirmados por semana epidemiológica###

RS22_22_23_SE_Confirmados <- matrix(data = NA, 
                                    nrow = 16, 
                                    ncol = 54)

RS22_22_23_SE_Confirmados <- as.data.frame(RS22_22_23_SE_Confirmados)

colnames(RS22_22_23_SE_Confirmados)[1] <- "Município" 

RS22_22_23_SE_Confirmados[,1] <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

colnames (RS22_22_23_SE_Confirmados)[2:24] <- c(31:53)

colnames (RS22_22_23_SE_Confirmados)[25:54] <- c(1:30)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 2] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i,
                                                                                             CLASSI_FIN == 10 
                                                                                             | 
                                                                                               CLASSI_FIN == 11 
                                                                                             |
                                                                                               CLASSI_FIN == 12,
                                                                                             SEM_PRI ==202231)%>%
                                                                                      count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 3] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             CLASSI_FIN == 10 
                                                                                             | 
                                                                                               CLASSI_FIN == 11 
                                                                                             |
                                                                                               CLASSI_FIN == 12,
                                                                                             SEM_PRI ==202232) %>% 
                                                                                      count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 4] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                      filter(ID_MN_RESI == i,
                                                                                             CLASSI_FIN == 10 
                                                                                             | 
                                                                                               CLASSI_FIN == 11 
                                                                                             |
                                                                                               CLASSI_FIN == 12,
                                                                                             SEM_PRI ==202233) %>% 
                                                                                      count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i),5] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                     filter(ID_MN_RESI == i,
                                                                                            CLASSI_FIN == 10 
                                                                                            | 
                                                                                              CLASSI_FIN == 11 
                                                                                            |
                                                                                              CLASSI_FIN == 12,
                                                                                            SEM_PRI ==202234) %>% 
                                                                                     count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 6] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                      filter(ID_MN_RESI == i,
                                                                                             CLASSI_FIN == 10 
                                                                                             | 
                                                                                               CLASSI_FIN == 11 
                                                                                             |
                                                                                               CLASSI_FIN == 12,
                                                                                             SEM_PRI ==202235) %>% 
                                                                                      count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 7] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             CLASSI_FIN == 10 
                                                                                             | 
                                                                                               CLASSI_FIN == 11 
                                                                                             |
                                                                                               CLASSI_FIN == 12,
                                                                                             SEM_PRI ==202236) %>%
                                                                                      count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 8] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             CLASSI_FIN == 10 
                                                                                             | 
                                                                                               CLASSI_FIN == 11 
                                                                                             |
                                                                                               CLASSI_FIN == 12,
                                                                                             SEM_PRI ==202237) %>% 
                                                                                      count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 9] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             CLASSI_FIN == 10 
                                                                                             | 
                                                                                               CLASSI_FIN == 11 
                                                                                             |
                                                                                               CLASSI_FIN == 12,
                                                                                             SEM_PRI ==202238) %>% 
                                                                                      count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 10] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202239) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 11] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202240) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 12] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202241) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 13] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202242) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 14] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202243) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 15] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202244) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 16] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202245) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 17] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202246) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 18] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202247) %>%    
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 19] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202248) %>%  
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 20] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202249) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i),  21] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                        filter(ID_MN_RESI == i,
                                                                                               CLASSI_FIN == 10 
                                                                                               | 
                                                                                                 CLASSI_FIN == 11 
                                                                                               |
                                                                                                 CLASSI_FIN == 12,
                                                                                               SEM_PRI ==202250) %>%
                                                                                        count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 22] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202251) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 23] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202252) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 24] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202253) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 25] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202301) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 26] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202302) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 27] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202303) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 28] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202304) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 29] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202305) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 30] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202306) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 31] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202307) %>% 
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 32] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202308) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 33] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202309) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 34] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202310) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 35] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202311) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 36] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202312) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 37] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202313) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 38] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202314) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 39] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202315) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 40] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202316) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 41] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202317) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 42] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202318) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 43] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202319) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 44] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202320) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 45] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202321) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 46] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202322) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 47] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202323) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 48] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202324) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 49] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202325) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 50] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202326) %>% 
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 51] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202327) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 52] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202328) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 53] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202329) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 54] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202330) %>%
                                                                                       count() 
  )
}

RS22_22_23_SE_Confirmados[,1] <- BASE_IBGE[which(BASE_IBGE$RS == 22), 3]

RS22_22_23_SE_Confirmados[17,2:54] <- apply(RS22_22_23_SE_Confirmados[,2:54], 2, sum)

RS22_22_23_SE_Confirmados[17,1] <- "Total"

####Elaborando for loop para criar tabela de dados gerais de notificação da 22ª RS###

RS22_22_23_GERAL <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == 22), 3])

RS22_22_23_GERAL$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

RS22_22_23_GERAL$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == 22), 5]

RS22_22_23_GERAL$RS <- BASE_IBGE[which(BASE_IBGE$RS == 22), 1]

RS22_22_23_GERAL <- RS22_22_23_GERAL[,c(4, 1, 2, 3)]

RS22_22_23_GERAL$Notificados <- NA

RS22_22_23_GERAL$Dengue <- NA

RS22_22_23_GERAL$D_S_A <- NA

RS22_22_23_GERAL$Dengue_Grave <- NA

RS22_22_23_GERAL$Descartados <- NA

RS22_22_23_GERAL$Autoctones <- NA

RS22_22_23_GERAL$Incidencia <- NA

RS22_22_23_GERAL$Criterio_Encerramento_Lab <- NA

RS22_22_23_GERAL$Criterio_Encerramento_Clin_Epid <- NA

RS22_22_23_GERAL$DENV_I <- NA

RS22_22_23_GERAL$DENV_II <- NA

RS22_22_23_GERAL$DENV_III <- NA

RS22_22_23_GERAL$DENV_IV <- NA

RS22_22_23_GERAL$Hospitalizacao <- NA

RS22_22_23_GERAL$Obitos <- NA


for(i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  ###Notiicações###  
  RS22_22_23_GERAL[which(RS22_22_23_GERAL$COD_IBGE == i), 5] <- as.integer(RS22_22_23_SINAN %>% 
                                                                             filter(ID_MN_RESI == i) %>%   
                                                                             count()
  )    
  
  ###Dengue###
  
  RS22_22_23_GERAL[which(RS22_22_23_GERAL$COD_IBGE == i), 6] <-as.integer(RS22_22_23_SINAN %>% 
                                                                            filter(CLASSI_FIN == 10, 
                                                                                   ID_MN_RESI == i) %>%
                                                                            count() 
  )
  ###D.S.A.###
  
  RS22_22_23_GERAL[which(RS22_22_23_GERAL$COD_IBGE == i), 7] <- as.integer(RS22_22_23_SINAN %>%  
                                                                             filter(CLASSI_FIN == 11, 
                                                                                    ID_MN_RESI == i) %>% 
                                                                             count()
  )
  
  ###Dengue Grave###
  
  RS22_22_23_GERAL[which(RS22_22_23_GERAL$COD_IBGE == i), 8] <- as.integer(RS22_22_23_SINAN %>%  
                                                                             filter(CLASSI_FIN == 12, 
                                                                                    ID_MN_RESI == i) %>% 
                                                                             count()
  )
  
  ###Descartados###
  
  
  
  RS22_22_23_GERAL[which(RS22_22_23_GERAL$COD_IBGE == i), 9]<- as.integer(RS22_22_23_SINAN %>% 
                                                                            filter(CLASSI_FIN == 5,
                                                                                   ID_MN_RESI == i) %>% 
                                                                            count()
  )  
  
  ###Autóctones###
  
  
  RS22_22_23_GERAL[which(RS22_22_23_GERAL$COD_IBGE == i), 10]<- as.integer(RS22_22_23_SINAN %>% 
                                                                             filter(ID_MN_RESI == i, 
                                                                                    TPAUTOCTO == 1,
                                                                                    CLASSI_FIN == 10) %>% 
                                                                             count() 
  )
  
  ###Encerrados Laboratório###
  
  RS22_22_23_GERAL[which(RS22_22_23_GERAL$COD_IBGE == i), 12] <- as.integer(RS22_22_23_SINAN %>% 
                                                                              filter(ID_MN_RESI == i, 
                                                                                     CRITERIO == 1) %>% 
                                                                              count() 
  )
  
  ###Encerrados Clínico-Epidemiológico###
  
  RS22_22_23_GERAL[which(RS22_22_23_GERAL$COD_IBGE == i), 13] <- as.integer(RS22_22_23_SINAN %>% 
                                                                              filter(ID_MN_RESI == i,
                                                                                     CRITERIO == 2) %>% 
                                                                              count() 
  )
  
  ###DENV I###
  
  RS22_22_23_GERAL[which(RS22_22_23_GERAL$COD_IBGE == i), 14]<- as.integer(RS22_22_23_SINAN %>% 
                                                                             filter(ID_MN_RESI == i, 
                                                                                    SOROTIPO == 1) %>% 
                                                                             count() 
  )
  
  ###DENV II###
  
  
  RS22_22_23_GERAL[which(RS22_22_23_GERAL$COD_IBGE == i), 15] <- as.integer(RS22_22_23_SINAN %>% 
                                                                              filter(ID_MN_RESI == i, 
                                                                                     SOROTIPO == 2) %>% 
                                                                              count() 
  )
  
  ###DENV III###
  
  RS22_22_23_GERAL[which(RS22_22_23_GERAL$COD_IBGE == i), 16] <- as.integer(RS22_22_23_SINAN %>% 
                                                                              filter(ID_MN_RESI == i, 
                                                                                     SOROTIPO == 3) %>% 
                                                                              count() 
  )
  ###DENV IV###                                     
  
  RS22_22_23_GERAL[which(RS22_22_23_GERAL$COD_IBGE == i), 17]<- as.integer(RS22_22_23_SINAN %>% 
                                                                             filter(ID_MN_RESI == i, 
                                                                                    SOROTIPO == 4) %>% 
                                                                             count() 
  )
  ###Hospitalização###
  
  RS22_22_23_GERAL[which(RS22_22_23_GERAL$COD_IBGE == i), 18] <- as.integer(RS22_22_23_SINAN %>% 
                                                                              filter(ID_MN_RESI == i, 
                                                                                     HOSPITALIZ == 1) %>% 
                                                                              count() 
  )
  ###Óbitos###
  
  RS22_22_23_GERAL[which(RS22_22_23_GERAL$COD_IBGE == i), 19] <- as.integer(RS22_22_23_SINAN %>% 
                                                                              filter(ID_MN_RESI == i, 
                                                                                     EVOLUCAO == 2) %>% 
                                                                              count() 
  )
}

###Incidência###FORA DO LOOP###

RS22_22_23_GERAL$Incidencia <- (RS22_22_23_GERAL$Autoctones/RS22_22_23_GERAL$Populacao)*100000  
RS22_22_23_GERAL$Incidencia <- format(round(RS22_22_23_GERAL$Incidencia, 2))
RS22_22_23_GERAL$Incidencia <- as.numeric(RS22_22_23_GERAL$Incidencia)

######################################################################
####Incluindo coluna de CASOS EM INVESTIGAÇÂO na tabela RS22_GERAL####
#### Esta coluna só tem sentido no período sazonal atual #############
#### Casos em investigação de períodos anteriores são ################
#### INCONCLUSIVOS####################################################
######################################################################

RS22_22_23_GERAL$Em_Investigacao <- as.integer(RS22_22_23_GERAL$Notificados) - as.integer(RS22_22_23_GERAL$Dengue + RS22_22_23_GERAL$D_S_A + RS22_22_23_GERAL$Dengue_Grave + RS22_22_23_GERAL$Descartados)

RS22_22_23_GERAL$Sorotipos <- NA

###Elaborando Quadro com dados de sexo, idade, zona de moradia e escolaridade#####

RS22_22_23_EXTRA <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == 22), 1])

RS22_22_23_EXTRA$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == 22), 3]

RS22_22_23_EXTRA$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

RS22_22_23_EXTRA$Menos_1_ano <- NA

RS22_22_23_EXTRA$Um_a_Cinco_Anos <- NA

RS22_22_23_EXTRA$Cinco_a_Doze_Anos <- NA

RS22_22_23_EXTRA$Doze_a_Dezoito_Anos <- NA

RS22_22_23_EXTRA$Maior_Dezoito <- NA

RS22_22_23_EXTRA$Area_Urbana <- NA

RS22_22_23_EXTRA$Area_Rural <- NA

RS22_22_23_EXTRA$Sexo_Feminino <- NA

RS22_22_23_EXTRA$Sexo_Masculino <- NA

RS22_22_23_EXTRA$Analfabeto <- NA

RS22_22_23_EXTRA$Fundamental_Incompleto <- NA

RS22_22_23_EXTRA$Fundamental <- NA

RS22_22_23_EXTRA$Ens_Medio_Incompleto <- NA

RS22_22_23_EXTRA$Ens_Medio<- NA

RS22_22_23_EXTRA$Ens_Superior_Incompleto<- NA

RS22_22_23_EXTRA$Ens_Superior<- NA

RS22_22_23_EXTRA$Escolaridade_Ignorada<- NA

###For Loop para geração da tabela RS22_Extra###

for(i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  RS22_22_23_EXTRA[which(RS22_22_23_EXTRA$COD_IBGE == i), 4] <- as.integer(RS22_22_23_SINAN %>% 
                                                                             filter(ID_MN_RESI == i,  
                                                                                    NU_IDADE_N <=3012) %>% 
                                                                             count() 
  )
  
  RS22_22_23_EXTRA[which(RS22_22_23_EXTRA$COD_IBGE == i), 5] <- as.integer(RS22_22_23_SINAN %>% 
                                                                             filter(ID_MN_RESI == i,  
                                                                                    NU_IDADE_N > 4000 & NU_IDADE_N <=4005) %>% 
                                                                             count() 
  )
  
  RS22_22_23_EXTRA[which(RS22_22_23_EXTRA$COD_IBGE == i), 6] <- as.integer(RS22_22_23_SINAN %>% 
                                                                             filter(ID_MN_RESI == i,
                                                                                    NU_IDADE_N > 4005 & NU_IDADE_N <=4012) %>% 
                                                                             count() 
  )
  
  RS22_22_23_EXTRA[which(RS22_22_23_EXTRA$COD_IBGE == i), 7] <- as.integer(RS22_22_23_SINAN %>% 
                                                                             filter(ID_MN_RESI == i, 
                                                                                    NU_IDADE_N > 4012 & NU_IDADE_N <=4018) %>% 
                                                                             count() 
  )
  
  RS22_22_23_EXTRA[which(RS22_22_23_EXTRA$COD_IBGE == i), 8] <- as.integer(RS22_22_23_SINAN %>% 
                                                                             filter(ID_MN_RESI == i, 
                                                                                    NU_IDADE_N > 4018) %>%
                                                                             count() 
  )
  
  RS22_22_23_EXTRA[which(RS22_22_23_EXTRA$COD_IBGE == i), 9] <- as.integer(RS22_22_23_SINAN %>% 
                                                                             filter(ID_MN_RESI == i,
                                                                                    CS_ZONA == 1) %>% 
                                                                             count() 
  )
  RS22_22_23_EXTRA[which(RS22_22_23_EXTRA$COD_IBGE == i), 10] <- as.integer(RS22_22_23_SINAN %>% 
                                                                              filter(ID_MN_RESI == i,
                                                                                     CS_ZONA == 2) %>% 
                                                                              count() 
  )
  
  RS22_22_23_EXTRA[which(RS22_22_23_EXTRA$COD_IBGE == i), 11]  <- as.integer(RS22_22_23_SINAN %>% 
                                                                               filter(ID_MN_RESI == i, 
                                                                                      CS_SEXO == "F") %>% 
                                                                               count() 
  )
  
  RS22_22_23_EXTRA[which(RS22_22_23_EXTRA$COD_IBGE == i), 12] <- as.integer(RS22_22_23_SINAN %>% 
                                                                              filter(ID_MN_RESI == i, 
                                                                                     CS_SEXO == "M") %>% 
                                                                              count() 
  )
  
  RS22_22_23_EXTRA[which(RS22_22_23_EXTRA$COD_IBGE == i), 13]<- as.integer(RS22_22_23_SINAN %>% 
                                                                             filter(ID_MN_RESI == i, 
                                                                                    CS_ESCOL_N == 0) %>% 
                                                                             count() 
  )
  
  RS22_22_23_EXTRA[which(RS22_22_23_EXTRA$COD_IBGE == i), 14] <- as.integer(RS22_22_23_SINAN %>% 
                                                                              filter(ID_MN_RESI == i, 
                                                                                     CS_ESCOL_N == 1 | CS_ESCOL_N == 2 | CS_ESCOL_N == 3) %>%
                                                                              count()
  )
  
  RS22_22_23_EXTRA[which(RS22_22_23_EXTRA$COD_IBGE == i), 15] <- as.integer(RS22_22_23_SINAN %>% 
                                                                              filter(ID_MN_RESI == i, 
                                                                                     CS_ESCOL_N == 4) %>% 
                                                                              count() 
  )
  
  RS22_22_23_EXTRA[which(RS22_22_23_EXTRA$COD_IBGE == i), 16] <- as.integer(RS22_22_23_SINAN %>% 
                                                                              filter(ID_MN_RESI == i, 
                                                                                     CS_ESCOL_N == 5) %>% 
                                                                              count() 
  )
  
  RS22_22_23_EXTRA[which(RS22_22_23_EXTRA$COD_IBGE == i), 17] <- as.integer(RS22_22_23_SINAN %>% 
                                                                              filter(ID_MN_RESI == i, 
                                                                                     CS_ESCOL_N == 6) %>% 
                                                                              count() 
  )
  
  RS22_22_23_EXTRA[which(RS22_22_23_EXTRA$COD_IBGE == i), 18] <- as.integer(RS22_22_23_SINAN %>% 
                                                                              filter(ID_MN_RESI == i, 
                                                                                     CS_ESCOL_N == 7) %>% 
                                                                              count() 
  )
  
  RS22_22_23_EXTRA[which(RS22_22_23_EXTRA$COD_IBGE == i), 19] <- as.integer(RS22_22_23_SINAN %>% 
                                                                              filter(ID_MN_RESI == i, 
                                                                                     CS_ESCOL_N == 8) %>% 
                                                                              count() 
  )
  
  
  RS22_22_23_EXTRA[which(RS22_22_23_EXTRA$COD_IBGE == i), 20]<- as.integer(RS22_22_23_SINAN %>% 
                                                                             filter(ID_MN_RESI == i, 
                                                                                    CS_ESCOL_N == 9) %>% 
                                                                             count() 
  )
}                                             

###Elaborando tabelas de sinais e sintomas. Possível somente a partir de 2015.###

RS22_22_23_SINAIS_Notificados <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == 22), 1])

RS22_22_23_SINAIS_Notificados$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == 22), 3]

RS22_22_23_SINAIS_Notificados$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

RS22_22_23_SINAIS_Notificados$Febre <- NA

RS22_22_23_SINAIS_Notificados$Cefaleia <- NA

RS22_22_23_SINAIS_Notificados$Mialgia <- NA

RS22_22_23_SINAIS_Notificados$Exantema <- NA

RS22_22_23_SINAIS_Notificados$Vomitos <- NA

RS22_22_23_SINAIS_Notificados$Nausea <- NA

RS22_22_23_SINAIS_Notificados$Dor_nas_Costas <- NA

RS22_22_23_SINAIS_Notificados$Conjuntivite <- NA

RS22_22_23_SINAIS_Notificados$Artrite  <- NA

RS22_22_23_SINAIS_Notificados$Artralgia <- NA

RS22_22_23_SINAIS_Notificados$Petequias <- NA

RS22_22_23_SINAIS_Notificados$Leucopenia <- NA

RS22_22_23_SINAIS_Notificados$Dor_Retroorbital <- NA

RS22_22_23_SINAIS_Notificados$Prova_do_Laco_Positiva <- NA

###Elaborando for loop para sinais e sintomas.###

for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  RS22_22_23_SINAIS_Notificados[which(RS22_22_23_SINAIS_Notificados$COD_IBGE == i), 4] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                       filter(ID_MN_RESI == i,
                                                                                                              FEBRE == 1) %>%
                                                                                                       count()
  )
  
  
  RS22_22_23_SINAIS_Notificados[which(RS22_22_23_SINAIS_Notificados$COD_IBGE == i), 5] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                       filter(ID_MN_RESI == i,
                                                                                                              CEFALEIA == 1) %>%
                                                                                                       count()
  )
  
  RS22_22_23_SINAIS_Notificados[which(RS22_22_23_SINAIS_Notificados$COD_IBGE == i), 6] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                       filter(ID_MN_RESI == i,
                                                                                                              MIALGIA == 1) %>%
                                                                                                       count()
  )
  
  
  RS22_22_23_SINAIS_Notificados[which(RS22_22_23_SINAIS_Notificados$COD_IBGE == i), 7] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                       filter(ID_MN_RESI == i,
                                                                                                              EXANTEMA == 1) %>%
                                                                                                       count()
  )
  
  RS22_22_23_SINAIS_Notificados[which(RS22_22_23_SINAIS_Notificados$COD_IBGE == i), 8] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                       filter(ID_MN_RESI == i,
                                                                                                              VOMITO == 1) %>%
                                                                                                       count()
  )
  
  RS22_22_23_SINAIS_Notificados[which(RS22_22_23_SINAIS_Notificados$COD_IBGE == i), 9]<- as.integer(RS22_22_23_SINAN %>%
                                                                                                      filter(ID_MN_RESI == i,
                                                                                                             NAUSEA == 1) %>%
                                                                                                      count()
  )
  
  RS22_22_23_SINAIS_Notificados[which(RS22_22_23_SINAIS_Notificados$COD_IBGE == i), 10]<- as.integer(RS22_22_23_SINAN %>%
                                                                                                       filter(ID_MN_RESI == i,
                                                                                                              DOR_COSTAS == 1) %>%
                                                                                                       count()
  )
  
  RS22_22_23_SINAIS_Notificados[which(RS22_22_23_SINAIS_Notificados$COD_IBGE == i), 11] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                        filter(ID_MN_RESI == i,
                                                                                                               CONJUNTVIT == 1) %>%
                                                                                                        count()
  )
  
  RS22_22_23_SINAIS_Notificados[which(RS22_22_23_SINAIS_Notificados$COD_IBGE == i), 12] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                        filter(ID_MN_RESI == i,
                                                                                                               ARTRITE == 1) %>%
                                                                                                        count()
  )
  
  RS22_22_23_SINAIS_Notificados[which(RS22_22_23_SINAIS_Notificados$COD_IBGE == i), 13] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                        filter(ID_MN_RESI == i,
                                                                                                               ARTRALGIA == 1) %>%
                                                                                                        count()
  )
  
  RS22_22_23_SINAIS_Notificados[which(RS22_22_23_SINAIS_Notificados$COD_IBGE == i), 14] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                        filter(ID_MN_RESI == i,
                                                                                                               PETEQUIA_N == 1) %>%
                                                                                                        count()
  )
  
  RS22_22_23_SINAIS_Notificados[which(RS22_22_23_SINAIS_Notificados$COD_IBGE == i), 15] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                        filter(ID_MN_RESI == i,
                                                                                                               LEUCOPENIA == 1) %>%
                                                                                                        count()
  )
  
  RS22_22_23_SINAIS_Notificados[which(RS22_22_23_SINAIS_Notificados$COD_IBGE == i), 16] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                        filter(ID_MN_RESI == i,
                                                                                                               DOR_RETRO == 1) %>%
                                                                                                        count()
  )
  
  RS22_22_23_SINAIS_Notificados[which(RS22_22_23_SINAIS_Notificados$COD_IBGE == i), 17] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                        filter(ID_MN_RESI == i,
                                                                                                               LACO == 1) %>%
                                                                                                        count()
  )
}

###Elaborando tabelas de sinais e sintomas. Possível somente a partir de 2015.###

RS22_22_23_SINAIS_Confirmados <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == 22), 1])

RS22_22_23_SINAIS_Confirmados$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == 22), 3]

RS22_22_23_SINAIS_Confirmados$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

RS22_22_23_SINAIS_Confirmados$Febre <- NA

RS22_22_23_SINAIS_Confirmados$Cefaleia <- NA

RS22_22_23_SINAIS_Confirmados$Mialgia <- NA

RS22_22_23_SINAIS_Confirmados$Exantema <- NA

RS22_22_23_SINAIS_Confirmados$Vomitos <- NA

RS22_22_23_SINAIS_Confirmados$Nausea <- NA

RS22_22_23_SINAIS_Confirmados$Dor_nas_Costas <- NA

RS22_22_23_SINAIS_Confirmados$Conjuntivite <- NA

RS22_22_23_SINAIS_Confirmados$Artrite  <- NA

RS22_22_23_SINAIS_Confirmados$Artralgia <- NA

RS22_22_23_SINAIS_Confirmados$Petequias <- NA

RS22_22_23_SINAIS_Confirmados$Leucopenia <- NA

RS22_22_23_SINAIS_Confirmados$Dor_Retroorbital <- NA

RS22_22_23_SINAIS_Confirmados$Prova_do_Laco_Positiva <- NA

###Elaborando for loop para sinais e sintomas.###

for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  RS22_22_23_SINAIS_Confirmados[which(RS22_22_23_SINAIS_Confirmados$COD_IBGE == i), 4] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                       filter(ID_MN_RESI == i,
                                                                                                              CLASSI_FIN == 10 
                                                                                                              | 
                                                                                                                CLASSI_FIN == 11 
                                                                                                              |
                                                                                                                CLASSI_FIN == 12,
                                                                                                              FEBRE == 1) %>%
                                                                                                       count()
  )
  
  
  RS22_22_23_SINAIS_Confirmados[which(RS22_22_23_SINAIS_Confirmados$COD_IBGE == i), 5] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                       filter(ID_MN_RESI == i,
                                                                                                              CLASSI_FIN == 10 
                                                                                                              | 
                                                                                                                CLASSI_FIN == 11 
                                                                                                              |
                                                                                                                CLASSI_FIN == 12,
                                                                                                              CEFALEIA == 1) %>%
                                                                                                       count()
  )
  
  RS22_22_23_SINAIS_Confirmados[which(RS22_22_23_SINAIS_Confirmados$COD_IBGE == i), 6] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                       filter(ID_MN_RESI == i,
                                                                                                              CLASSI_FIN == 10 
                                                                                                              | 
                                                                                                                CLASSI_FIN == 11 
                                                                                                              |
                                                                                                                CLASSI_FIN == 12,
                                                                                                              MIALGIA == 1) %>%
                                                                                                       count()
  )
  
  
  RS22_22_23_SINAIS_Confirmados[which(RS22_22_23_SINAIS_Confirmados$COD_IBGE == i), 7] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                       filter(ID_MN_RESI == i,
                                                                                                              CLASSI_FIN == 10 
                                                                                                              | 
                                                                                                                CLASSI_FIN == 11 
                                                                                                              |
                                                                                                                CLASSI_FIN == 12,
                                                                                                              EXANTEMA == 1) %>%
                                                                                                       count()
  )
  
  RS22_22_23_SINAIS_Confirmados[which(RS22_22_23_SINAIS_Confirmados$COD_IBGE == i), 8] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                       filter(ID_MN_RESI == i,
                                                                                                              CLASSI_FIN == 10 
                                                                                                              | 
                                                                                                                CLASSI_FIN == 11 
                                                                                                              |
                                                                                                                CLASSI_FIN == 12,
                                                                                                              VOMITO == 1) %>%
                                                                                                       count()
  )
  
  RS22_22_23_SINAIS_Confirmados[which(RS22_22_23_SINAIS_Confirmados$COD_IBGE == i), 9]<- as.integer(RS22_22_23_SINAN %>%
                                                                                                      filter(ID_MN_RESI == i,
                                                                                                             CLASSI_FIN == 10 
                                                                                                             | 
                                                                                                               CLASSI_FIN == 11 
                                                                                                             |
                                                                                                               CLASSI_FIN == 12,
                                                                                                             NAUSEA == 1) %>%
                                                                                                      count()
  )
  
  RS22_22_23_SINAIS_Confirmados[which(RS22_22_23_SINAIS_Confirmados$COD_IBGE == i), 10]<- as.integer(RS22_22_23_SINAN %>%
                                                                                                       filter(ID_MN_RESI == i,
                                                                                                              CLASSI_FIN == 10 
                                                                                                              | 
                                                                                                                CLASSI_FIN == 11 
                                                                                                              |
                                                                                                                CLASSI_FIN == 12,
                                                                                                              DOR_COSTAS == 1) %>%
                                                                                                       count()
  )
  
  RS22_22_23_SINAIS_Confirmados[which(RS22_22_23_SINAIS_Confirmados$COD_IBGE == i), 11] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                        filter(ID_MN_RESI == i,
                                                                                                               CLASSI_FIN == 10 
                                                                                                               | 
                                                                                                                 CLASSI_FIN == 11 
                                                                                                               |
                                                                                                                 CLASSI_FIN == 12,
                                                                                                               CONJUNTVIT == 1) %>%
                                                                                                        count()
  )
  
  RS22_22_23_SINAIS_Confirmados[which(RS22_22_23_SINAIS_Confirmados$COD_IBGE == i), 12] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                        filter(ID_MN_RESI == i,
                                                                                                               CLASSI_FIN == 10 
                                                                                                               | 
                                                                                                                 CLASSI_FIN == 11 
                                                                                                               |
                                                                                                                 CLASSI_FIN == 12,
                                                                                                               ARTRITE == 1) %>%
                                                                                                        count()
  )
  
  RS22_22_23_SINAIS_Confirmados[which(RS22_22_23_SINAIS_Confirmados$COD_IBGE == i), 13] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                        filter(ID_MN_RESI == i,
                                                                                                               CLASSI_FIN == 10 
                                                                                                               | 
                                                                                                                 CLASSI_FIN == 11 
                                                                                                               |
                                                                                                                 CLASSI_FIN == 12,
                                                                                                               ARTRALGIA == 1) %>%
                                                                                                        count()
  )
  
  RS22_22_23_SINAIS_Confirmados[which(RS22_22_23_SINAIS_Confirmados$COD_IBGE == i), 14] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                        filter(ID_MN_RESI == i,
                                                                                                               CLASSI_FIN == 10 
                                                                                                               | 
                                                                                                                 CLASSI_FIN == 11 
                                                                                                               |
                                                                                                                 CLASSI_FIN == 12,
                                                                                                               PETEQUIA_N == 1) %>%
                                                                                                        count()
  )
  
  RS22_22_23_SINAIS_Confirmados[which(RS22_22_23_SINAIS_Confirmados$COD_IBGE == i), 15] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                        filter(ID_MN_RESI == i,
                                                                                                               CLASSI_FIN == 10 
                                                                                                               | 
                                                                                                                 CLASSI_FIN == 11 
                                                                                                               |
                                                                                                                 CLASSI_FIN == 12,
                                                                                                               LEUCOPENIA == 1) %>%
                                                                                                        count()
  )
  
  RS22_22_23_SINAIS_Confirmados[which(RS22_22_23_SINAIS_Confirmados$COD_IBGE == i), 16] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                        filter(ID_MN_RESI == i,
                                                                                                               CLASSI_FIN == 10 
                                                                                                               | 
                                                                                                                 CLASSI_FIN == 11 
                                                                                                               |
                                                                                                                 CLASSI_FIN == 12,
                                                                                                               DOR_RETRO == 1) %>%
                                                                                                        count()
  )
  
  RS22_22_23_SINAIS_Confirmados[which(RS22_22_23_SINAIS_Confirmados$COD_IBGE == i), 17] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                        filter(ID_MN_RESI == i,
                                                                                                               CLASSI_FIN == 10 
                                                                                                               | 
                                                                                                                 CLASSI_FIN == 11 
                                                                                                               |
                                                                                                                 CLASSI_FIN == 12,
                                                                                                               LACO == 1) %>%
                                                                                                        count()
  )
}

###Montando tabela de doenças pré-existentes###

RS22_22_23_DOENCAS_PRE_EXISTENTES <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == 22), 1])

RS22_22_23_DOENCAS_PRE_EXISTENTES$Município <- BASE_IBGE[which(BASE_IBGE$RS == 22), 3]

RS22_22_23_DOENCAS_PRE_EXISTENTES$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

RS22_22_23_DOENCAS_PRE_EXISTENTES$Diabetes <- NA

RS22_22_23_DOENCAS_PRE_EXISTENTES$Doencas_Hematologicas <- NA

RS22_22_23_DOENCAS_PRE_EXISTENTES$Hepatopatias <- NA

RS22_22_23_DOENCAS_PRE_EXISTENTES$DRC <- NA

RS22_22_23_DOENCAS_PRE_EXISTENTES$Hipertensao <- NA

RS22_22_23_DOENCAS_PRE_EXISTENTES$Doenca_Acido_Peptica <- NA

RS22_22_23_DOENCAS_PRE_EXISTENTES$Doenca_Auto_Imune <- NA

###Construindo o for loop###

for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  RS22_22_23_DOENCAS_PRE_EXISTENTES[which(RS22_22_23_DOENCAS_PRE_EXISTENTES$COD_IBGE == i), 4] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                               filter(ID_MN_RESI == i, 
                                                                                                                      DIABETES == 1) %>%
                                                                                                               count()
  )
  
  RS22_22_23_DOENCAS_PRE_EXISTENTES[which(RS22_22_23_DOENCAS_PRE_EXISTENTES$COD_IBGE == i), 5] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                               filter(ID_MN_RESI == i,
                                                                                                                      HEMATOLOG == 1) %>%
                                                                                                               count()
  )
  
  RS22_22_23_DOENCAS_PRE_EXISTENTES[which(RS22_22_23_DOENCAS_PRE_EXISTENTES$COD_IBGE == i), 6] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                               filter(ID_MN_RESI == i,
                                                                                                                      HEPATOPAT == 1) %>%
                                                                                                               count()
  )
  
  RS22_22_23_DOENCAS_PRE_EXISTENTES[which(RS22_22_23_DOENCAS_PRE_EXISTENTES$COD_IBGE == i), 7]<- as.integer(RS22_22_23_SINAN %>%
                                                                                                              filter(ID_MN_RESI == i,
                                                                                                                     RENAL == 1) %>%
                                                                                                              count()
  )
  
  RS22_22_23_DOENCAS_PRE_EXISTENTES[which(RS22_22_23_DOENCAS_PRE_EXISTENTES$COD_IBGE == i), 8]<- as.integer(RS22_22_23_SINAN %>%
                                                                                                              filter(ID_MN_RESI == i,
                                                                                                                     HIPERTENSA == 1) %>%
                                                                                                              count()
  )
  
  RS22_22_23_DOENCAS_PRE_EXISTENTES[which(RS22_22_23_DOENCAS_PRE_EXISTENTES$COD_IBGE == i), 9] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                               filter(ID_MN_RESI == i,
                                                                                                                      ACIDO_PEPT == 1) %>%
                                                                                                               count()
  )
  
  RS22_22_23_DOENCAS_PRE_EXISTENTES[which(RS22_22_23_DOENCAS_PRE_EXISTENTES$COD_IBGE == i), 10] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                                filter(ID_MN_RESI == i,
                                                                                                                       AUTO_IMUNE == 1) %>%
                                                                                                                count()
  )
}

###Construindo tabela sinais de alarme###

RS22_22_23_SINAIS_DE_ALARME <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == 22), 1])

RS22_22_23_SINAIS_DE_ALARME$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == 22), 3]

RS22_22_23_SINAIS_DE_ALARME$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

RS22_22_23_SINAIS_DE_ALARME$Hipotensao_Lipotimia <- NA

RS22_22_23_SINAIS_DE_ALARME$Queda_Abrupta_Plaquetas <- NA

RS22_22_23_SINAIS_DE_ALARME$Vomitos_Persistentes <- NA

RS22_22_23_SINAIS_DE_ALARME$Dor_Abdominal <- NA

RS22_22_23_SINAIS_DE_ALARME$Letargia <- NA

RS22_22_23_SINAIS_DE_ALARME$Aumento_Hematocrito <- NA

RS22_22_23_SINAIS_DE_ALARME$hemorragias <- NA

RS22_22_23_SINAIS_DE_ALARME$Hepatomegalia <- NA

RS22_22_23_SINAIS_DE_ALARME$Acumulo_Liquidos <- NA

###Construindo o for loop###

for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  RS22_22_23_SINAIS_DE_ALARME[which(RS22_22_23_SINAIS_DE_ALARME$COD_IBGE == i), 4] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                   filter(ID_MN_RESI == i,
                                                                                                          ALRM_HIPOT == 1) %>%
                                                                                                   count()
  )
  
  RS22_22_23_SINAIS_DE_ALARME[which(RS22_22_23_SINAIS_DE_ALARME$COD_IBGE == i), 5] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                   filter(ID_MN_RESI == i,
                                                                                                          ALRM_PLAQ == 1) %>%
                                                                                                   count()
  )
  
  RS22_22_23_SINAIS_DE_ALARME[which(RS22_22_23_SINAIS_DE_ALARME$COD_IBGE == i), 6] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                   filter(ID_MN_RESI == i,
                                                                                                          ALRM_VOM == 1) %>%
                                                                                                   count()
  )
  
  RS22_22_23_SINAIS_DE_ALARME[which(RS22_22_23_SINAIS_DE_ALARME$COD_IBGE == i), 7] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                   filter(ID_MN_RESI == i,
                                                                                                          ALRM_ABDOM == 1) %>%
                                                                                                   count()
  )
  
  RS22_22_23_SINAIS_DE_ALARME[which(RS22_22_23_SINAIS_DE_ALARME$COD_IBGE == i), 8] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                   filter(ID_MN_RESI == i,
                                                                                                          ALRM_LETAR == 1) %>%
                                                                                                   count()
  )
  
  RS22_22_23_SINAIS_DE_ALARME[which(RS22_22_23_SINAIS_DE_ALARME$COD_IBGE == i), 9] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                   filter(ID_MN_RESI == i,
                                                                                                          ALRM_HEMAT == 1) %>%
                                                                                                   count()
  )
  
  RS22_22_23_SINAIS_DE_ALARME[which(RS22_22_23_SINAIS_DE_ALARME$COD_IBGE == i), 10] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                    filter(ID_MN_RESI == i,
                                                                                                           ALRM_SANG == 1) %>%
                                                                                                    count()
  )
  
  RS22_22_23_SINAIS_DE_ALARME[which(RS22_22_23_SINAIS_DE_ALARME$COD_IBGE == i), 11] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                    filter(ID_MN_RESI == i,
                                                                                                           ALRM_HEPAT == 1) %>%
                                                                                                    count()
  )
  
  RS22_22_23_SINAIS_DE_ALARME[which(RS22_22_23_SINAIS_DE_ALARME$COD_IBGE == i), 12] <- as.integer(RS22_22_23_SINAN %>%
                                                                                                    filter(ID_MN_RESI == i,
                                                                                                           ALRM_LIQ == 1) %>%
                                                                                                    count()
                                                                                                  
  )
}

###Construindo tabela Dengue Grave###

RS22_22_23_DENGUE_GRAVE <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == 22), 1])

RS22_22_23_DENGUE_GRAVE$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == 22), 3]

RS22_22_23_DENGUE_GRAVE$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

RS22_22_23_DENGUE_GRAVE$Pulso_Debil <- NA

RS22_22_23_DENGUE_GRAVE$PA_Convergente <- NA

RS22_22_23_DENGUE_GRAVE$TPC <- NA

RS22_22_23_DENGUE_GRAVE$Acumulo_Liquidos_Insuf_Respiratoria <- NA

RS22_22_23_DENGUE_GRAVE$Taquicardia <- NA

RS22_22_23_DENGUE_GRAVE$Extremidades_Frias <- NA

RS22_22_23_DENGUE_GRAVE$Hipotensão_Arterial <- NA

RS22_22_23_DENGUE_GRAVE$Hematemese <- NA

RS22_22_23_DENGUE_GRAVE$Melena <- NA

RS22_22_23_DENGUE_GRAVE$Metrorragia <- NA

RS22_22_23_DENGUE_GRAVE$Sangramento_SNC <- NA

RS22_22_23_DENGUE_GRAVE$Aumento_ALT_AST <- NA

RS22_22_23_DENGUE_GRAVE$Miocardite <- NA

RS22_22_23_DENGUE_GRAVE$Alteracao_Consciencia <- NA

###Construindo o for loop###

for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  RS22_22_23_DENGUE_GRAVE[which(RS22_22_23_DENGUE_GRAVE$COD_IBGE == i), 4] <- as.integer(RS22_22_23_SINAN %>%
                                                                                           filter(ID_MN_RESI == i,
                                                                                                  GRAV_PULSO == 1) %>%
                                                                                           count()
  )     
  
  RS22_22_23_DENGUE_GRAVE[which(RS22_22_23_DENGUE_GRAVE$COD_IBGE == i), 5] <- as.integer(RS22_22_23_SINAN %>%
                                                                                           filter(ID_MN_RESI == i,
                                                                                                  GRAV_CONV == 1) %>%
                                                                                           count()
  )   
  
  RS22_22_23_DENGUE_GRAVE[which(RS22_22_23_DENGUE_GRAVE$COD_IBGE == i), 6] <- as.integer(RS22_22_23_SINAN %>%
                                                                                           filter(ID_MN_RESI == i,
                                                                                                  GRAV_ENCH == 1) %>%
                                                                                           count()
  )  
  
  RS22_22_23_DENGUE_GRAVE[which(RS22_22_23_DENGUE_GRAVE$COD_IBGE == i), 7] <- as.integer(RS22_22_23_SINAN %>%
                                                                                           filter(ID_MN_RESI == i,
                                                                                                  GRAV_INSUF == 1) %>%
                                                                                           count()
  ) 
  
  RS22_22_23_DENGUE_GRAVE[which(RS22_22_23_DENGUE_GRAVE$COD_IBGE == i), 8] <- as.integer(RS22_22_23_SINAN %>%
                                                                                           filter(ID_MN_RESI == i,
                                                                                                  GRAV_TAQUI == 1) %>%
                                                                                           count()
  ) 
  
  RS22_22_23_DENGUE_GRAVE[which(RS22_22_23_DENGUE_GRAVE$COD_IBGE == i), 9] <- as.integer(RS22_22_23_SINAN %>%
                                                                                           filter(ID_MN_RESI == i,
                                                                                                  GRAV_EXTRE == 1) %>%
                                                                                           count()
  ) 
  
  RS22_22_23_DENGUE_GRAVE[which(RS22_22_23_DENGUE_GRAVE$COD_IBGE == i), 10] <- as.integer(RS22_22_23_SINAN %>%
                                                                                            filter(ID_MN_RESI == i,
                                                                                                   GRAV_HIPOT == 1) %>%
                                                                                            count()
  ) 
  
  RS22_22_23_DENGUE_GRAVE[which(RS22_22_23_DENGUE_GRAVE$COD_IBGE == i), 11] <- as.integer(RS22_22_23_SINAN %>%
                                                                                            filter(ID_MN_RESI == i,
                                                                                                   GRAV_HEMAT == 1) %>%
                                                                                            count()
  ) 
  
  RS22_22_23_DENGUE_GRAVE[which(RS22_22_23_DENGUE_GRAVE$COD_IBGE == i), 12]<- as.integer(RS22_22_23_SINAN %>%
                                                                                           filter(ID_MN_RESI == i,
                                                                                                  GRAV_MELEN == 1) %>%
                                                                                           count()
  ) 
  
  RS22_22_23_DENGUE_GRAVE[which(RS22_22_23_DENGUE_GRAVE$COD_IBGE == i), 13] <- as.integer(RS22_22_23_SINAN %>%
                                                                                            filter(ID_MN_RESI == i,
                                                                                                   GRAV_METRO == 1) %>%
                                                                                            count()
  ) 
  
  RS22_22_23_DENGUE_GRAVE[which(RS22_22_23_DENGUE_GRAVE$COD_IBGE == i), 14] <- as.integer(RS22_22_23_SINAN %>%
                                                                                            filter(ID_MN_RESI == i,
                                                                                                   GRAV_SANG == 1) %>%
                                                                                            count()
  )
  
  RS22_22_23_DENGUE_GRAVE[which(RS22_22_23_DENGUE_GRAVE$COD_IBGE == i), 15] <- as.integer(RS22_22_23_SINAN %>%
                                                                                            filter(ID_MN_RESI == i,
                                                                                                   GRAV_AST == 1) %>%
                                                                                            count()
  )
  
  RS22_22_23_DENGUE_GRAVE[which(RS22_22_23_DENGUE_GRAVE$COD_IBGE == i), 16] <- as.integer(RS22_22_23_SINAN %>%
                                                                                            filter(ID_MN_RESI == i,
                                                                                                   GRAV_MIOC == 1) %>%
                                                                                            count()
  )
  
  RS22_22_23_DENGUE_GRAVE[which(RS22_22_23_DENGUE_GRAVE$COD_IBGE == i), 17] <- as.integer(RS22_22_23_SINAN %>%
                                                                                            filter(ID_MN_RESI == i,
                                                                                                   GRAV_CONSC == 1) %>%
                                                                                            count()
  )
}

#########################################################################################################################
####################    FIM FIM FIM FIM FIM FIM FIM FIM FIM FIM FIM FIM FIM FIM #########################################
#################### Daqui para baixo são tabulações que servirão somente para  #########################################
#################### o Informe Epidemiológico. ##########################################################################
#########################################################################################################################

####Trabalhando com a tabela RS22_SINAN do período atual. Realizando a decodificação dos fatores em linguagem mais acessível aos municípios####

RS22_22_23_SINAN_DECODIFICADO <- RS22_22_23_SINAN

RS22_22_23_SINAN_DECODIFICADO$ID_AGRAVO <- factor(RS22_22_23_SINAN_DECODIFICADO$ID_AGRAVO,
                                                  label = c("Dengue", "Chikungunya"), 
                                                  levels = c("A90", "A92")
)

###Sintomas###
RS22_22_23_SINAN_DECODIFICADO$FEBRE <- factor(RS22_22_23_SINAN_DECODIFICADO$FEBRE,
                                              label = c("SIM", "NÃO"), 
                                              levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$MIALGIA <- factor(RS22_22_23_SINAN_DECODIFICADO$MIALGIA,
                                                label = c("SIM", "NÃO"), 
                                                levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$CEFALEIA <- factor(RS22_22_23_SINAN_DECODIFICADO$CEFALEIA,
                                                 label = c("SIM", "NÃO"), 
                                                 levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$EXANTEMA <- factor(RS22_22_23_SINAN_DECODIFICADO$EXANTEMA,
                                                 label = c("SIM", "NÃO"), 
                                                 levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$VOMITO <- factor(RS22_22_23_SINAN_DECODIFICADO$VOMITO,
                                               label = c("SIM", "NÃO"), 
                                               levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$NAUSEA <- factor(RS22_22_23_SINAN_DECODIFICADO$NAUSEA,
                                               label = c("SIM", "NÃO"), 
                                               levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$DOR_COSTAS <- factor(RS22_22_23_SINAN_DECODIFICADO$DOR_COSTAS,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$DOR_RETRO <- factor(RS22_22_23_SINAN_DECODIFICADO$DOR_RETRO,
                                                  label = c("SIM", "NÃO"), 
                                                  levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$CONJUNTVIT <- factor(RS22_22_23_SINAN_DECODIFICADO$CONJUNTVIT,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$ARTRALGIA <- factor(RS22_22_23_SINAN_DECODIFICADO$ARTRALGIA,
                                                  label = c("SIM", "NÃO"), 
                                                  levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$ARTRITE <- factor(RS22_22_23_SINAN_DECODIFICADO$ARTRITE,
                                                label = c("SIM", "NÃO"), 
                                                levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$PETEQUIA_N <- factor(RS22_22_23_SINAN_DECODIFICADO$PETEQUIA_N,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$LEUCOPENIA <- factor(RS22_22_23_SINAN_DECODIFICADO$LEUCOPENIA,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$LACO <- factor(RS22_22_23_SINAN_DECODIFICADO$LACO,
                                             label = c("SIM", "NÃO"), 
                                             levels = c(1, 2)
)

###Doenças Pré-existentes

RS22_22_23_SINAN_DECODIFICADO$DIABETES <- factor(RS22_22_23_SINAN_DECODIFICADO$DIABETES,
                                                 label = c("SIM", "NÃO"), 
                                                 levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$HEMATOLOG <- factor(RS22_22_23_SINAN_DECODIFICADO$HEMATOLOG,
                                                  label = c("SIM", "NÃO"), 
                                                  levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$HEPATOPAT <- factor(RS22_22_23_SINAN_DECODIFICADO$HEPATOPAT,
                                                  label = c("SIM", "NÃO"), 
                                                  levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$RENAL <- factor(RS22_22_23_SINAN_DECODIFICADO$RENAL,
                                              label = c("SIM", "NÃO"), 
                                              levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$HIPERTENSA <- factor(RS22_22_23_SINAN_DECODIFICADO$HIPERTENSA,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$ACIDO_PEPT <- factor(RS22_22_23_SINAN_DECODIFICADO$ACIDO_PEPT,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$AUTO_IMUNE <- factor(RS22_22_23_SINAN_DECODIFICADO$AUTO_IMUNE,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

####Outros####

RS22_22_23_SINAN_DECODIFICADO$CS_GESTANT <- factor(RS22_22_23_SINAN_DECODIFICADO$CS_GESTANT,
                                                   label = c("1_TRI", "2_TRI", "3_TRI", "IDADE GESTACIONAL IGN", "NÃO", "NÃO SE APLICA", "IGNORADO"), 
                                                   levels = c(1, 2, 3, 4, 5, 6, 9)
)

RS22_22_23_SINAN_DECODIFICADO$CS_ESCOL_N <- factor(RS22_22_23_SINAN_DECODIFICADO$CS_ESCOL_N,
                                                   label = c("ANALFABETO", "1 a 4 SÉRIE DO FUNDAMENTAL INCOMPLETA", "4 SÉRIE DO FUNDAMENTAL COMPLETA", "5 a 8 SÉRIE DO FUNDAMENTAL INCOMPLETA", "FUNDAMENTAL COMPLETO", "ENSINO MÉDIO INCOMPLETO", "ENSINO MÉDIO COMPLETO", "SUPERIOR INCONPLETO", "SUPERIOR COMPLETO", "IGNORADO", "NÃO SE APLICA"), 
                                                   levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
)

RS22_22_23_SINAN_DECODIFICADO$RESUL_SORO <- factor(RS22_22_23_SINAN_DECODIFICADO$RESUL_SORO,
                                                   label = c("REAGENTE", "NÃO REAGENTE", "INCONCLUSIVO", "NÃO REALIZADO"), 
                                                   levels = c(1, 2, 3, 4)
)


RS22_22_23_SINAN_DECODIFICADO$RESUL_PCR_ <- factor(RS22_22_23_SINAN_DECODIFICADO$RESUL_PCR_,
                                                   label = c("DETECTÁVEL", "NÃO DETECTÀVEL", "INCONCLUSIVO", "NÃO REALIZADO"), 
                                                   levels = c(1, 2, 3, 4)
)

RS22_22_23_SINAN_DECODIFICADO$SOROTIPO <- factor(RS22_22_23_SINAN_DECODIFICADO$SOROTIPO,
                                                 label = c("I", "II", "III", "IV"), 
                                                 levels = c(1, 2, 3, 4)
)

RS22_22_23_SINAN_DECODIFICADO$CLASSI_FIN <- factor(RS22_22_23_SINAN_DECODIFICADO$CLASSI_FIN,
                                                   label = c("DESCARTADO", "DENGUE", "D.S.A.", "IDENGUE_GRAVE", "CHIKUNGUNYA"), 
                                                   levels = c(5, 10, 11, 12, 13)
)

RS22_22_23_SINAN_DECODIFICADO$CRITERIO <- factor(RS22_22_23_SINAN_DECODIFICADO$CRITERIO,
                                                 label = c("LABORATORIAL", "CLÍNICO-EPIDEMIOLÓGICO", "EM INVESTIGAÇÃO"), 
                                                 levels = c(1, 2, 3)
)

RS22_22_23_SINAN_DECODIFICADO$TPAUTOCTO <- factor(RS22_22_23_SINAN_DECODIFICADO$TPAUTOCTO,
                                                  label = c("SIM", "NÃO"), 
                                                  levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$HOSPITALIZ <- factor(RS22_22_23_SINAN_DECODIFICADO$HOSPITALIZ,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$EVOLUCAO <- factor(RS22_22_23_SINAN_DECODIFICADO$EVOLUCAO,
                                                 label = c("CURA", "ÓBITO PELO AGRAVO", "ÓBITO POR OUTRAS CAUSAS","ÓBITO EM INVESTIGAÇÃO", "INDETERMINADO"), 
                                                 levels = c(1, 2, 3, 4, 9)
)

RS22_22_23_SINAN_DECODIFICADO$CS_ZONA <- factor(RS22_22_23_SINAN_DECODIFICADO$CS_ZONA,
                                                label = c("URBANA", "RURAL", "PERIURBANA","INDETERMINADO"), 
                                                levels = c(1, 2, 3, 9)
)

RS22_22_23_SINAN_DECODIFICADO$ALRM_LETAR <- factor(RS22_22_23_SINAN_DECODIFICADO$ALRM_LETAR,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$ALRM_HEPAT <- factor(RS22_22_23_SINAN_DECODIFICADO$ALRM_HEPAT,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$ALRM_LIQ <- factor(RS22_22_23_SINAN_DECODIFICADO$ALRM_LIQ,
                                                 label = c("SIM", "NÃO"), 
                                                 levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$ALRM_HIPOT <- factor(RS22_22_23_SINAN_DECODIFICADO$ALRM_HIPOT,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$ALRM_PLAQ <- factor(RS22_22_23_SINAN_DECODIFICADO$ALRM_PLAQ,
                                                  label = c("SIM", "NÃO"), 
                                                  levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$ALRM_VOM <- factor(RS22_22_23_SINAN_DECODIFICADO$ALRM_VOM,
                                                 label = c("SIM", "NÃO"), 
                                                 levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$ALRM_SANG <- factor(RS22_22_23_SINAN_DECODIFICADO$ALRM_SANG,
                                                  label = c("SIM", "NÃO"), 
                                                  levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$ALRM_HEMAT <- factor(RS22_22_23_SINAN_DECODIFICADO$ALRM_LETAR,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO$ALRM_ABDOM <- factor(RS22_22_23_SINAN_DECODIFICADO$ALRM_ABDOM,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)
####RS22_22_23_SINAN_DECODIFICADO$Municipio 

RS22_22_23_SINAN_DECODIFICADO_AUX <- data.frame(COD = RS22_22_23_SINAN_DECODIFICADO[,12], 
                                                Municipio = NA)

for (i in RS22_22_23_SINAN_DECODIFICADO[,12]){
  RS22_22_23_SINAN_DECODIFICADO_AUX[which(RS22_22_23_SINAN_DECODIFICADO_AUX$COD == i), 2] <- BASE_IBGE[which(BASE_IBGE$Código_IBGE == i),3]
  
}

RS22_22_23_SINAN_DECODIFICADO[,12] <- RS22_22_23_SINAN_DECODIFICADO_AUX[, 2]

####Município de Residência

RS22_22_23_SINAN_DECODIFICADO_AUX <- data.frame(COD = RS22_22_23_SINAN_DECODIFICADO[,20], 
                                                Municipio = NA)

for (i in RS22_22_23_SINAN_DECODIFICADO[,20]){
  RS22_22_23_SINAN_DECODIFICADO_AUX[which(RS22_22_23_SINAN_DECODIFICADO_AUX$COD == i), 2] <- BASE_IBGE[which(BASE_IBGE$Código_IBGE == i),3]
  
}

RS22_22_23_SINAN_DECODIFICADO[,20] <- RS22_22_23_SINAN_DECODIFICADO_AUX[, 2]

rm (RS22_22_23_SINAN_DECODIFICADO_AUX)

colnames(RS22_22_23_SINAN_DECODIFICADO)<- c("RS", "SINAN", "Latitude", "Longitude", "Agravo", "Data_Notificacao", "ANO", "SE_Notificacao", "Data_Primeiros_Sintomas", "SE_Primeiros_Sintomas", "UF_Notificacao", "Municipio", "Nome", "Data_Nascimento", "Idade", "Sexo", "Gestante", "Escolaridade", "Nome_Mae", "Municipio_Residencia", "UF_Residencia", "RS_Residencia", "Logradouro", "Numero", "Bairro", "CEP", "Zona", "Data_Digitacao", "Data_Investigacao", "Febre", "Mialgia", "Cefaleia", "Exantema", "Vomito", "Nausea", "Dor_nas_Costas", "Conjuntivite", "Artrite", "Artralgia_Intensa", "Petequias", "Leucopenia", "Prova_do_Laco_Positiva", "Dor_retroorbital", "Diabetes", "Doenca_Hematologica", "Hepatopatia", "Doenca_Renal", "Hipertensao", "Doenca_Acido_Peptica", "Doenca_Auto_Imune", "Data_Sorologia", "Resultado_Sorologia", "Data_PCR", "Resultado_PCR", "Sorotipo", "Classificacao_Final", "Critério_Encerramento", "Autoctone", "UF_Infeccao", "Municipio_Infeccao", "Bairro_Infeccao", "Evolucao", "Hospitalizado", "Data_Internamento", "Data_Obito", "Data_Encerramento", "Data_SNA", "Letargia", "Hepatomegalia", "Acumulo_Liquidos", "Hipotensao_Lipotimia", "Queda_Abrupta_Plaquetas", "Vomitos_Persistentes", "Hemorragias", "Aumento_Hematocrito", "Dor_Abdominal", "Data_Dengue_Grave", "Pulso_Debil", "PA_Convergente", "TPC", "Acumulo_Liq_Insuficiencia_Resp", "Taquicardia", "Extremidades_Frias", "Hipotensao", "Hematemese", "Melena", "Metrorragia_", "Sangramento_SNC", "Aumento_AST_ALT", "Miocardite", "Alteracao_Consciencia", "Outros_Orgaos", "Manifestacao_Hemorragica", "Epistaxe", "Gengivorragia", "Metrorragia", "Observacoes" )



####Adicionando Coluna unificadora de sorotipos virais no RS22_22_23_Geral para uso exclusivo do QGIS####

####Adicionando os dados do período atual na tabela Série Histórica####

RS22_Serie_Historica_Base[1, 15] <- sum(RS22_22_23_GERAL$Notificados)
RS22_Serie_Historica_Base[2, 15] <- sum(RS22_22_23_GERAL$Dengue)
RS22_Serie_Historica_Base[3, 15] <- sum(RS22_22_23_GERAL$D_S_A)
RS22_Serie_Historica_Base[4, 15] <- sum(RS22_22_23_GERAL$Dengue_Grave)
RS22_Serie_Historica_Base[5, 15] <- sum(RS22_22_23_GERAL$Hospitalizacao)
RS22_Serie_Historica_Base[6, 15] <- sum(RS22_22_23_GERAL$Autoctones)
RS22_Serie_Historica_Base[7, 15] <- sum(RS22_22_23_GERAL$DENV_I)
RS22_Serie_Historica_Base[8, 15] <- sum(RS22_22_23_GERAL$DENV_II)
RS22_Serie_Historica_Base[9, 15] <- sum(RS22_22_23_GERAL$DENV_III)
RS22_Serie_Historica_Base[10, 15] <- sum(RS22_22_23_GERAL$DENV_IV)
RS22_Serie_Historica_Base[11, 15] <- sum(RS22_22_23_GERAL$Obitos)


AUX <- as.data.frame(t(RS22_Serie_Historica_Base))

colnames(AUX) <- AUX[1,]

AUX <- AUX[-1,]

AUX[,12] <- c("2009/10", "2010/11", "2011/12", "2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23")

colnames(AUX)[12] <- "Periodo"

AUX <- AUX[,c(12, 1:11)]

rownames(AUX) <- c(1:14)

RS22_Serie_Historica <- AUX

rm(AUX, RS22_Serie_Historica_Base)

RS22_Serie_Historica[,2] <- as.numeric(RS22_Serie_Historica[,2])
RS22_Serie_Historica[,3] <- as.numeric(RS22_Serie_Historica[,3])
RS22_Serie_Historica[,4] <- as.numeric(RS22_Serie_Historica[,4])
RS22_Serie_Historica[,5] <- as.numeric(RS22_Serie_Historica[,5])
RS22_Serie_Historica[,6] <- as.numeric(RS22_Serie_Historica[,6])
RS22_Serie_Historica[,7] <- as.numeric(RS22_Serie_Historica[,7])
RS22_Serie_Historica[,8] <- as.numeric(RS22_Serie_Historica[,8])
RS22_Serie_Historica[,9] <- as.numeric(RS22_Serie_Historica[,9])
RS22_Serie_Historica[,10] <- as.numeric(RS22_Serie_Historica[,10])
RS22_Serie_Historica[,11] <- as.numeric(RS22_Serie_Historica[,11])
RS22_Serie_Historica[,12] <- as.numeric(RS22_Serie_Historica[,12])

write.csv (RS22_Serie_Historica, "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_Serie_Historica.csv", row.names = FALSE)

####################################################################################################################
################Trabalhando a tabela base do Canal Endêmico#########################################################
####################################################################################################################

######Canal Endêmico NOTIFICADOS#####

RS22_CE_Notificados_Base[14, 1] <- "2022/23"
RS22_CE_Notificados_Base[14, 2:54] <- as.integer(data.frame(RS22_22_23_SE_Notificados[17, 2:54]))

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- RS22_CE_Notificados_Base[,-1]

AUX <- t(AUX)

AUX2 <- RS22_CE_Notificados_Base[,1]

colnames(AUX) <- AUX2

RS22_CE_Notificados <- AUX

######Criando a coluna de média no data.frame#####################

AUX <- apply(RS22_CE_Notificados[,], 1 , mean)

RS22_CE_Notificados <- as.data.frame(RS22_CE_Notificados)

RS22_CE_Notificados$Media <- AUX

######Criando a coluna de Desvio Padrão no data frame###############

AUX <- apply(RS22_CE_Notificados[,], 1 , sd)

RS22_CE_Notificados$Desvio_Padrao <- AUX

###### Criando a coluna de Média + 2(DP)

AUX <- RS22_CE_Notificados[, 15:16]

AUX <- AUX %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

RS22_CE_Notificados$Lim_Superior <- AUX$Lim_Superior

RS22_CE_Notificados[,18] <- rownames(RS22_CE_Notificados)

RS22_CE_Notificados <- RS22_CE_Notificados[, c(18, 1:17)]

RS22_CE_Notificados[,1] <- c(31:53, 1:30)

colnames(RS22_CE_Notificados)[1] <- "Semana_Epidemiológica"

rownames(RS22_CE_Notificados) <- c(1:nrow(RS22_CE_Notificados))

rm(AUX, AUX2, RS22_CE_Notificados_Base)

write.csv (RS22_CE_Notificados, "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_CE_Notificados.csv", row.names = FALSE)


####Canal Endêmico CONFIRMADOS####


RS22_CE_Confirmados_Base[14, 1] <- "2022/23"
RS22_CE_Confirmados_Base[14, 2:54] <- as.integer(data.frame(RS22_22_23_SE_Confirmados[17, 2:54]))

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- RS22_CE_Confirmados_Base[,-1]

AUX <- t(AUX)

AUX2 <- RS22_CE_Confirmados_Base[,1]

colnames(AUX) <- AUX2

RS22_CE_Confirmados <- AUX

######Criando a coluna de média no data.frame#####################

AUX <- apply(RS22_CE_Confirmados[,], 1 , mean)

RS22_CE_Confirmados <- as.data.frame(RS22_CE_Confirmados)

RS22_CE_Confirmados$Media <- AUX

######Criando a coluna de Desvio Padrão no data frame###############

AUX <- apply(RS22_CE_Confirmados[,], 1 , sd)

RS22_CE_Confirmados$Desvio_Padrao <- AUX

###### Criando a coluna de Média + 2(DP)

AUX <- RS22_CE_Confirmados[, 15:16]

AUX <- AUX %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

RS22_CE_Confirmados$Lim_Superior <- AUX$Lim_Superior

RS22_CE_Confirmados[,18] <- rownames(RS22_CE_Confirmados)

RS22_CE_Confirmados <- RS22_CE_Confirmados[, c(18, 1:17)]

RS22_CE_Confirmados[,1] <- c(31:53, 1:30)

colnames(RS22_CE_Confirmados)[1] <- "Semana_Epidemiológica"

rownames(RS22_CE_Confirmados) <- c(1:nrow(RS22_CE_Confirmados))

rm(AUX, AUX2, RS22_CE_Confirmados_Base)

write.csv (RS22_CE_Confirmados, "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_CE_Confirmados.csv", row.names = FALSE)

#####Planilhas Google Sheets. Realizando o download das planilhas do google sheets e fazendo o upload da planilha de notificações e Geral Resumida######

###Upload de Notificações para posterior download da mesma planilha com as coordenadas. A planilha que irá subir para o google sheets é derivada 
###da BASE DBF do SINAN e NÃO CONTÉM COORDENADAS. As coordenadas estão em planilha própria no google drive, preenchida pelos municípios, e é vinculada 
###no google sheets com esta planilha.####

RS22_22_23_SINAN_DECODIFICADO$SINAN <- as.numeric(as.character(RS22_22_23_SINAN_DECODIFICADO$SINAN))

sheet_write(RS22_22_23_SINAN_DECODIFICADO, ss = "https://docs.google.com/spreadsheets/d/1z-cXrCe0ZRMRBG2rqW2BmG09HEa4tMmplXhHMnLbRg4/edit#gid=668044240", sheet = "Registros_SINAN")

PR_22_23_DENGUE_MUNICIPIOS <- read_sheet ("https://docs.google.com/spreadsheets/d/1DEidGb0GgH7jVWFQ3e4HS7-DPxu2969ydJAcoMpboUQ/edit#gid=0", sheet ="Dengue")

PR_22_23_DENGUE_MUNICIPIOS <- PR_22_23_DENGUE_MUNICIPIOS[, -c(17:24)]

PR_22_23_CHIKUNGUNYA_MUNICIPIOS <- read_sheet ("https://docs.google.com/spreadsheets/d/1DEidGb0GgH7jVWFQ3e4HS7-DPxu2969ydJAcoMpboUQ/edit#gid=0", sheet ="Chikungunya")

PR_22_23_ZIKA_MUNICIPIOS <- read_sheet ("https://docs.google.com/spreadsheets/d/1DEidGb0GgH7jVWFQ3e4HS7-DPxu2969ydJAcoMpboUQ/edit#gid=0", sheet ="Zika")


RS22_22_23_REDE_OVITRAMPAS <- read_sheet("https://docs.google.com/spreadsheets/d/1mAiDoUdGgVsTB1DK5LumL2oStZZvIkbCp5XsuafaEvE/edit#gid=863361484", sheet = "Consolidado")

RS22_22_23_CICLOS_LOCALIDADES <- read_sheet("https://docs.google.com/spreadsheets/d/18hJMQnlFcRHeqNbtJObD0UCx8L-lOcJpOTYkn849Fcs/edit#gid=764914932")

###Substituindo NA por 200 na planilha. O QGIS não está reconhecendo NA e as variáveis ficam como String no SIG.###

RS22_22_23_CICLOS_LOCALIDADES[is.na(RS22_22_23_CICLOS_LOCALIDADES)] <- 200

RS22_22_23_CICLOS_MUNICIPIOS <- read_sheet("https://docs.google.com/spreadsheets/d/1P1fC2Z3R8yyaF2_P7wUcDEtwKqfq8MWcf2R5vmmw-kM/edit#gid=1734395963")

###Substituindo NA por 200 (QGIS não está reconhecendo NA. O SIG importa a planilha como character e não possibilita realizar análise dos dados de forma numérica)
RS22_22_23_CICLOS_MUNICIPIOS[is.na(RS22_22_23_CICLOS_MUNICIPIOS)] <- 200

RS22_22_23_RG_MUNICIPIOS <- read_sheet("https://docs.google.com/spreadsheets/d/1QILHWnVa1m2Lr4qqf02VejbCrPQaqkr1w51QNbxkaXk/edit#gid=1585473376")

###Por alguma razão a planilha veio como lista###

RS22_22_23_RG_MUNICIPIOS <- as.data.frame(lapply(RS22_22_23_RG_MUNICIPIOS, 
                                                 unlist))

RS22_22_23_RG_LOCALIDADES <- read_sheet("https://docs.google.com/spreadsheets/d/1js80T20EU2FvfqTLsjutPI7Zg93AaOhhbl8-wBD18FA/edit#gid=877642872")

RS22_22_23_PE <- read_sheet("https://docs.google.com/spreadsheets/d/1fW7nGY_h17JqeuV37rnk71sZvxyB1_D0rynci5m-dI8/edit#gid=863361484", sheet = "Consolidado")

####Por alguma razão a planilha veio como uma lista####

RS22_22_23_PE <- as.data.frame(lapply(RS22_22_23_PE, 
                                      unlist))

RS22_22_23_ASSISTENCIA <- read_sheet("https://docs.google.com/spreadsheets/d/1SCe_xImlW3cExZ2AzbfCQc51OPo9JETqAJkSvbGn8rk/edit#gid=863361484",
                                     sheet = "Consolidado")

###Criando tabela Geral Resumida para utilização no Informe###

RS22_22_23_Resumida <- data_frame(Notificados = sum(RS22_22_23_GERAL$Notificados),
                                  Dengue = sum(RS22_22_23_GERAL$Dengue),
                                  DSA = sum(RS22_22_23_GERAL$D_S_A),
                                  Dengue_Grave = sum(RS22_22_23_GERAL$Dengue_Grave),
                                  Obitos = sum(RS22_22_23_GERAL$Obitos)
)

sheet_write(RS22_22_23_Resumida, ss = "https://docs.google.com/spreadsheets/d/1bAPfOaZfUOf7ZP8-sxNLXa91YRGJ9QtnBL5cFeLpJPc/edit#gid=0", sheet = "Resumo")


###########Incluindo Sorotipos na Planilha RS22_22_23_GERAL. Essa etapa está sendo realizada somente agora pois depende da tabela 
###########PR_22_23_DENGUE_MUNICÍPIOS, a qual só foi realizado o download neste ponto do script###########

for (i in RS22_22_23_GERAL$Município){
  RS22_22_23_GERAL[which(RS22_22_23_GERAL$Município  == i), 21] <- as.character(PR_22_23_DENGUE_MUNICIPIOS[which(PR_22_23_DENGUE_MUNICIPIOS$MUNICÍPIO  == i), 17])
}


######Gráficos para serem utilizados no Informe ####

####Construção de data frame auxiliar para ser utilizado pelo R na construção dos gráficos###

AUX_GRAF <- as.data.frame(RS22_Serie_Historica$Periodo)

AUX_GRAF <- AUX_GRAF %>% mutate(Notificados = RS22_Serie_Historica$Notificados)

AUX_GRAF <- AUX_GRAF %>% mutate(Confirmados = RS22_Serie_Historica$Dengue 
                                + 
                                  RS22_Serie_Historica$D.S.A. 
                                + 
                                  RS22_Serie_Historica$Dengue_Grave)

colnames(AUX_GRAF) <- c("Periodo", "Notificados", "Confirmados")

####Criando gráfico de barras lado a lado Série Histórica Notificados e Confirmados, e fazendo upload para o Google drive####

RS22_Serie_Historica_GRAF_Not_Conf <- ggplot (AUX_GRAF, 
                                              aes(x = Periodo)) + 
  theme(axis.text.x = element_text(angle = 50, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 14)) +
  labs(caption = Fonte, 
       x = "Período Sazonal",
       y = "Número de Casos",
       title = "CASOS NOTIFICADOS/CONFIRMADOS 22ªRS (2009/10 - 2022/23)") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 24,
                                   colour = "#556B2F")) +
  geom_bar(
    aes( y = Notificados, fill = "Notificados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = Notificados,
                 label = Notificados),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             nudge_y = 30) + 
  scale_fill_manual(name = "", values = c("Notificados" = "#1E5D18", "Confirmados" = "#746343")) +
  geom_bar(
    aes( y = Confirmados, fill = "Confirmados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = Confirmados,
                 label = Confirmados),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             nudge_y = 30) 


#####Série Histórica de casos Hospitalizados#######

RS22_Serie_Historica_GRAF_Hospitalizados <- ggplot (RS22_Serie_Historica, 
                                                    aes(x = Periodo, 
                                                        y = Hospitalizados)) + 
  theme(axis.text.x = element_text(angle = 50, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 12)) +
  labs(caption = Fonte, 
       x = "Período Sazonal",
       y = "Número de Casos",
       title = "CASOS HOSPITALIZADOS 22ªRS (2009/10 - 2022/23)") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 14,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity", 
           fill = "#0F815D",
           color = "black") + 
  geom_label(aes(label = Hospitalizados),
             alpha = 0.5,
             size =3,
             nudge_y = 3) 


#####Série Histórica Sorotipo Circulante######

RS22_Serie_Historica_GRAF_Sorotipo <- ggplot (RS22_Serie_Historica, 
                                              aes(x = Periodo)) + 
  theme(axis.text.x = element_text(angle = 50, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 12)) +
  labs(caption = Fonte, 
       x = "Período Sazonal",
       y = "Número de Casos",
       title = "SOROTIPO CIRCULANTE 22ªRS (2009/10 - 2022/23)") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 14,
                                   colour = "#556B2F")) +
  geom_bar(
    aes( y = DENV_I, fill = "DENV I"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = DENV_I,
                 label = DENV_I),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             nudge_y = .5) + 
  scale_fill_manual(name = "", 
                    values = c("DENV I" = "#2F657E", "DENV II" = "#6D0B21")) +
  geom_bar(
    aes( y = DENV_II, 
         fill = "DENV II"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = DENV_II,
                 label = DENV_II),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             nudge_y = .5) 


###Sintomas Confirmados e Notificados

AUX_GRAF <- data.frame(Sintomas = colnames(RS22_22_23_SINAIS_Notificados)[-c(1, 2, 3)],
                       Notificados = NA,
                       Confirmados = NA)
AUX_GRAF[1,2] <- sum(RS22_22_23_SINAIS_Notificados[, 4])
AUX_GRAF[2,2] <- sum(RS22_22_23_SINAIS_Notificados[, 5])
AUX_GRAF[3,2] <- sum(RS22_22_23_SINAIS_Notificados[, 6])
AUX_GRAF[4,2] <- sum(RS22_22_23_SINAIS_Notificados[, 7])
AUX_GRAF[5,2] <- sum(RS22_22_23_SINAIS_Notificados[, 8])
AUX_GRAF[6,2] <- sum(RS22_22_23_SINAIS_Notificados[, 9])
AUX_GRAF[7,2] <- sum(RS22_22_23_SINAIS_Notificados[, 10])
AUX_GRAF[8,2] <- sum(RS22_22_23_SINAIS_Notificados[, 11])
AUX_GRAF[9,2] <- sum(RS22_22_23_SINAIS_Notificados[, 12])
AUX_GRAF[10,2] <- sum(RS22_22_23_SINAIS_Notificados[, 13])
AUX_GRAF[11,2] <- sum(RS22_22_23_SINAIS_Notificados[, 14])
AUX_GRAF[12,2] <- sum(RS22_22_23_SINAIS_Notificados[, 15])
AUX_GRAF[13,2] <- sum(RS22_22_23_SINAIS_Notificados[, 16])
AUX_GRAF[14,2] <- sum(RS22_22_23_SINAIS_Notificados[, 17])

AUX_GRAF[1,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 4])
AUX_GRAF[2,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 5])
AUX_GRAF[3,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 6])
AUX_GRAF[4,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 7])
AUX_GRAF[5,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 8])
AUX_GRAF[6,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 9])
AUX_GRAF[7,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 10])
AUX_GRAF[8,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 11])
AUX_GRAF[9,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 12])
AUX_GRAF[10,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 13])
AUX_GRAF[11,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 14])
AUX_GRAF[12,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 15])
AUX_GRAF[13,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 16])
AUX_GRAF[14,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 17])

RS22_22_23_GRAF_SINAIS <- ggplot (AUX_GRAF, 
                                  aes(x = Sintomas)) + 
  theme(axis.text.x = element_text(angle = 80, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 14)) +
  labs(caption = Fonte, 
       x = "Sintomas",
       y = "Número de Casos",
       title = "SINTOMAS CASOS NOTIFICADOS/CONFIRMADOS 22ªRS - 2022/23") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(
    aes( y = Notificados, fill = "Notificados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = Notificados,
                 label = Notificados),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             nudge_y = .5) + 
  scale_fill_manual(name = "", 
                    values = c("Notificados" = "#4D5656", "Confirmados" = "#B03A2E")) +
  geom_bar(
    aes( y = Confirmados, 
         fill = "Confirmados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = Confirmados,
                 label = Confirmados),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             nudge_y = .5) 

#####Casos Notificados por município - Período sazonal atual####

RS22_22_23_GRAF_Notificados <- ggplot (RS22_22_23_GERAL, 
                                       aes(x = Município, 
                                           y = Notificados)) + 
  theme(axis.text.x = element_text(angle = 50, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte, 
       x = "Municípios",
       y = "Número de Casos",
       title = "CASOS NOTIFICADOS/MUNICÍPIO - 2022/23") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#046236") + 
  geom_label(aes(label = Notificados), 
             size = 3, 
             alpha = 0.5,
             nudge_y = 0.9)  

AUX_GRAF <- data.frame (Municípios = RS22_22_23_GERAL[, 2],
                        Confirmados = (RS22_22_23_GERAL[, 6] + RS22_22_23_GERAL[, 7] + RS22_22_23_GERAL[, 8])
)
RS22_22_23_GRAF_Confirmados <- ggplot (AUX_GRAF, 
                                       aes(x = Municípios, 
                                           y = Confirmados)) + 
  theme(axis.text.x = element_text(angle = 50, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte, 
       x = "Municípios",
       y = "Número de Casos",
       title = "CASOS CONFIRMADOS/MUNICÍPIO - 2022/23") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8E1C21") + 
  geom_label(aes(label = Confirmados), 
             size = 3, 
             alpha = 0.5,
             nudge_y = 0.2)  

RS22_22_23_GRAF_Autoctones <- ggplot (RS22_22_23_GERAL, 
                                      aes(x = Município, 
                                          y = Autoctones)) + 
  theme(axis.text.x = element_text(angle = 50, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte, 
       x = "Municípios",
       y = "Número de Casos",
       title = "CASOS AUTÓCTONES/MUNICÍPIO - 2022/23") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#300A01") + 
  geom_label(aes(label = Autoctones), 
             size = 3, 
             alpha = 0.5,
             nudge_y = 0.07) 

RS22_22_23_GRAF_Investigacao <- ggplot (RS22_22_23_GERAL, 
                                        aes(x = Município, 
                                            y = Em_Investigacao)) + 
  theme(axis.text.x = element_text(angle = 50, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte, 
       x = "Municípios",
       y = "Número de Casos",
       title = "CASOS EM INVESTIGAÇÃO/MUNICÍPIO - 2022/23") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#9B665A") + 
  geom_label(aes(label = Em_Investigacao), 
             size = 3, 
             alpha = 0.5,
             nudge_y = 0.2) 

RS22_22_23_GRAF_Incidencia <- ggplot (RS22_22_23_GERAL, 
                                      aes(x = Município, 
                                          y = Incidencia)) + 
  theme(axis.text.x = element_text(angle = 50, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte, 
       x = "Municípios",
       y = "Número de Casos",
       title = "INCIDÊNCIA/MUNICÍPIO - 2022/23") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Incidencia), 
             size = 3, 
             alpha = 0.5,
             nudge_y = 2) 

RS22_22_23_GRAF_Descartados <- ggplot (RS22_22_23_GERAL, 
                                       aes(x = Município, 
                                           y = Descartados)) + 
  theme(axis.text.x = element_text(angle = 50, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte, 
       x = "Municípios",
       y = "Número de Casos",
       title = "CASOS DESCARTADOS/MUNICÍPIO - 2022/23") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Descartados),
             alpha = 0.5,
             nudge_y = 0.6)

RS22_22_23_GRAF_Hospitalizados <- ggplot (RS22_22_23_GERAL, 
                                          aes(x = Município, 
                                              y = Hospitalizacao)) + 
  theme(axis.text.x = element_text(angle = 50, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte, 
       x = "Municípios",
       y = "Número de Casos",
       title = "Casos Hospitalizados/Município - 2022/23") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#D2B48C") + 
  geom_label(aes(label = Hospitalizacao),
             alpha = 0.5,
             nudge_y = 0.02) 

###CANAL ENDÊMICO NOTIFICADOS####

###Puxando os dados da tabela RS22_CE_Notificados e excluindo os períodos epidêmicos: 2015/16, 2019/20 e 2021/22

AUX_GRAF <- RS22_CE_Notificados[, c(2, 3, 4, 5, 6, 7, 9, 10, 11, 13)]

###Usando apply para tirar a média por semana epidemiológica

AUX_GRAF$Media <- apply(AUX_GRAF[,], 1 , mean)

###Usando apply para tirar o desvio padrão por semana epidemiológica

AUX_GRAF$Desvio_Padrao <- apply(AUX_GRAF[,], 1 , sd)

###### Criando a coluna de Média + 2(DP)

AUX_GRAF <- AUX_GRAF[, c(11:12)]

AUX_GRAF$Lim_Superior <- NA

AUX_GRAF <- AUX_GRAF %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.
AUX_GRAF$Ordem <- c(1: nrow(RS22_CE_Notificados))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2022/23` <- RS22_CE_Notificados$`2022/23`

AUX_GRAF$Sem_Epidemiológica <- RS22_CE_Notificados$Semana_Epidemiológica

AUX_GRAF$Sem_EPI <-as.character(c("2022/31",  "2022/32", "2022/33",  "2022/34",  "2022/35",  "2022/36",  "2022/37",  "2022/38",  "2022/39",  "2022/40",  "2022/41",  "2022/42",  "2022/43",  "2022/44",  "2022/45",  "2022/46",  "2022/47",  "2022/48",  "2022/49",  "2022/50",  "2022/51",  "2022/52",  "2022/53",  "2023/01",  "2023/02",  "2023/03",  "2023/04",  "2023/05",  "2023/06",  "2023/07",  "2023/08",  "2023/09",  "2023/10", "2023/11",  "2023/12",  "2023/13",  "2023/14",  "2023/15",  "2023/16",  "2023/17",  "2023/18",  "2023/19",  "2023/20",  "2023/21",  "2023/22",  "2023/23",  "2023/24",  "2023/25",  "2023/26",  "2023/27",  "2023/28",  "2023/29",  "2023/30"))

RS22_22_23_GRAF_CE_Notificados <- ggplot(AUX_GRAF, aes(Ordem))  +
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos Notificados - 2022/23") +
  theme(
    panel.grid.major = element_line(color = "#C0C0C0"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#DC143C"),
    plot.title = element_text(face = "bold",
                              size = 19)
  ) +
  geom_area(aes(,Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
  geom_area(aes(,Media), fill = "#556B2F") +
  geom_line(aes(,`2022/23`), stat = "identity", color = "black", linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI)

####Elaborando tabela de casos Prováveis (Casos Notificados - Casos Descartados) por semana epidemiológica. Será usado para elaboração do diagrama de controle de casos prováveis###
RS22_22_23_SE_Descartados <- matrix(data = NA, 
                                    nrow = 16, 
                                    ncol = 54)

RS22_22_23_SE_Descartados <- as.data.frame(RS22_22_23_SE_Descartados)

colnames(RS22_22_23_SE_Descartados)[1] <- "Município" 

RS22_22_23_SE_Descartados[,1] <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

colnames (RS22_22_23_SE_Descartados)[2:24] <- c(31:53)

colnames (RS22_22_23_SE_Descartados)[25:54] <- c(1:30)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 2] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i,
                                                                                             SEM_PRI ==202231,
                                                                                             CLASSI_FIN == 5)%>%
                                                                                      count()
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 3] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             SEM_PRI ==202232,
                                                                                             CLASSI_FIN == 5) %>% 
                                                                                      count()
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 4] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                      filter(ID_MN_RESI == i,
                                                                                             SEM_PRI ==202233,
                                                                                             CLASSI_FIN == 5) %>% 
                                                                                      count()
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i),5] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                     filter(ID_MN_RESI == i,
                                                                                            SEM_PRI ==202234,
                                                                                            CLASSI_FIN == 5) %>% 
                                                                                     count()
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 6] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                      filter(ID_MN_RESI == i,
                                                                                             SEM_PRI ==202235,
                                                                                             CLASSI_FIN == 5) %>% 
                                                                                      count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 7] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             SEM_PRI ==202236,
                                                                                             CLASSI_FIN == 5) %>%
                                                                                      count()
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 8] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             SEM_PRI ==202237,
                                                                                             CLASSI_FIN == 5) %>% 
                                                                                      count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 9] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             SEM_PRI ==202238,
                                                                                             CLASSI_FIN == 5) %>% 
                                                                                      count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 10] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202239,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 11] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202240,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 12] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202241,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 13] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202242,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 14] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202243,
                                                                                              CLASSI_FIN == 5) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 15] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202244,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 16] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202245,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 17] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202246,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 18] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202247,
                                                                                              CLASSI_FIN == 5) %>%       
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 19] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202248,
                                                                                              CLASSI_FIN == 5) %>%     
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 20] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202249,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i),  21] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                        filter(ID_MN_RESI == i, 
                                                                                               SEM_PRI ==202250,
                                                                                               CLASSI_FIN == 5) %>%
                                                                                        count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 22] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202251,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 23] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202252,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 24] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202253,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 25] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202301,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 26] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202302,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 27] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202303,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 28] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202304,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 29] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202305,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 30] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202306,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 31] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202307,
                                                                                              CLASSI_FIN == 5) %>% 
                                                                                       count()
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 32] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202308,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 33] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202309,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 34] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202310,
                                                                                              CLASSI_FIN == 5) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 35] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202311,
                                                                                              CLASSI_FIN == 5) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 36] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202312,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 37] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202313,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 38] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202314,
                                                                                              CLASSI_FIN == 5) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 39] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202315,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 40] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202316,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 41] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202317,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 42] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202318,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 43] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202319,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 44] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202320,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 45] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202321,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 46] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202322,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 47] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202323,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 48] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202324,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 49] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202325,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 50] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202326,
                                                                                              CLASSI_FIN == 5) %>% 
                                                                                       count()
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 51] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202327,
                                                                                              CLASSI_FIN == 5) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 52] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202328,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 53] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202329,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Descartados[which(RS22_22_23_SE_Descartados == i), 54] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202330,
                                                                                              CLASSI_FIN == 5) %>%
                                                                                       count() 
  )
}

RS22_22_23_SE_Descartados[,1] <- BASE_IBGE[which(BASE_IBGE$RS == 22), 3]

RS22_22_23_SE_Descartados[17,2:54] <- apply(RS22_22_23_SE_Descartados[,2:54], 2, sum)

RS22_22_23_SE_Descartados[17,1] <- "Total"

####Casos prováveis por semana epidemiológica. Este objeto será apagado assim que for incluso no AUX_GRAF####

RS22_22_23_Casos_Provaveis <- (RS22_22_23_SE_Notificados[17, 2: 54] - RS22_22_23_SE_Descartados[17, 2: 54])

rownames(RS22_22_23_Casos_Provaveis)[1] <- "Provaveis"

RS22_22_23_Casos_Provaveis <- t(as.data.frame(RS22_22_23_Casos_Provaveis))

###CANAL ENDÊMICO CONFIRMADOS####

###Puxando os dados da tabela RS22_CE_Notificados e excluindo os períodos epidêmicos: 2015/16, 2019/20 e 2021/22

AUX_GRAF <- RS22_CE_Notificados[, c(2, 3, 4, 5, 6, 7, 9, 10, 11, 13)]

###Usando apply para tirar a média por semana epidemiológica

AUX_GRAF$Media <- apply(AUX_GRAF[,], 1 , mean)

###Usando apply para tirar o desvio padrão por semana epidemiológica

AUX_GRAF$Desvio_Padrao <- apply(AUX_GRAF[,], 1 , sd)

###### Criando a coluna de Média + 2(DP)

AUX_GRAF <- AUX_GRAF[, c(11:12)]

AUX_GRAF$Lim_Superior <- NA

AUX_GRAF <- AUX_GRAF %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.
AUX_GRAF$Ordem <- c(1: nrow(RS22_CE_Notificados))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2022/23` <- RS22_CE_Notificados$`2022/23`

AUX_GRAF$Sem_Epidemiológica <- RS22_CE_Notificados$Semana_Epidemiológica

AUX_GRAF$Sem_EPI <-as.character(c("2022/31",  "2022/32", "2022/33",  "2022/34",  "2022/35",  "2022/36",  "2022/37",  "2022/38",  "2022/39",  "2022/40",  "2022/41",  "2022/42",  "2022/43",  "2022/44",  "2022/45",  "2022/46",  "2022/47",  "2022/48",  "2022/49",  "2022/50",  "2022/51",  "2022/52",  "2022/53",  "2023/01",  "2023/02",  "2023/03",  "2023/04",  "2023/05",  "2023/06",  "2023/07",  "2023/08",  "2023/09",  "2023/10", "2023/11",  "2023/12",  "2023/13",  "2023/14",  "2023/15",  "2023/16",  "2023/17",  "2023/18",  "2023/19",  "2023/20",  "2023/21",  "2023/22",  "2023/23",  "2023/24",  "2023/25",  "2023/26",  "2023/27",  "2023/28",  "2023/29",  "2023/30"))

AUX_GRAF[, 8] <- RS22_22_23_Casos_Provaveis[,1]

colnames(AUX_GRAF)[8] <- "Provaveis"

RS22_22_23_GRAF_CE_Provaveis <- ggplot(AUX_GRAF, aes(Ordem))  +
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos PROVÁVEIS - 2022/23") +
  theme(
    panel.grid.major = element_line(color = "#C0C0C0"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#DC143C"),
    plot.title = element_text(face = "bold",
                              size = 19)
  ) +
  geom_area(aes(,Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
  geom_area(aes(,Media), fill = "#556B2F") +
  geom_line(aes(,Provaveis), stat = "identity", color = "black", linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI)

rm(RS22_22_23_Casos_Provaveis)

###CANAL ENDÊMICO CONFIRMADOS####

###Puxando os dados da tabela RS22_CE_Confirmados e excluindo os períodos epidêmicos: 2015/16, 2019/20 e 2021/22

AUX_GRAF <- RS22_CE_Confirmados[, c(2, 3, 4, 5, 6, 7, 9, 10, 11, 13)]

###Usando apply para tirar a média por semana epidemiológica

AUX_GRAF$Media <- apply(AUX_GRAF[,], 1 , mean)

###Usando apply para tirar o desvio padrão por semana epidemiológica

AUX_GRAF$Desvio_Padrao <- apply(AUX_GRAF[,], 1 , sd)

###### Criando a coluna de Média + 2(DP)

AUX_GRAF <- AUX_GRAF[, c(11:12)]

AUX_GRAF$Lim_Superior <- NA

AUX_GRAF <- AUX_GRAF %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.
AUX_GRAF$Ordem <- c(1: nrow(RS22_CE_Confirmados))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2022/23` <- RS22_CE_Confirmados$`2022/23`

AUX_GRAF$Sem_Epidemiológica <- RS22_CE_Confirmados$Semana_Epidemiológica

AUX_GRAF$Sem_EPI <-as.character(c("2022/31",  "2022/32", "2022/33",  "2022/34",  "2022/35",  "2022/36",  "2022/37",  "2022/38",  "2022/39",  "2022/40",  "2022/41",  "2022/42",  "2022/43",  "2022/44",  "2022/45",  "2022/46",  "2022/47",  "2022/48",  "2022/49",  "2022/50",  "2022/51",  "2022/52",  "2022/53",  "2023/01",  "2023/02",  "2023/03",  "2023/04",  "2023/05",  "2023/06",  "2023/07",  "2023/08",  "2023/09",  "2023/10", "2023/11",  "2023/12",  "2023/13",  "2023/14",  "2023/15",  "2023/16",  "2023/17",  "2023/18",  "2023/19",  "2023/20",  "2023/21",  "2023/22",  "2023/23",  "2023/24",  "2023/25",  "2023/26",  "2023/27",  "2023/28",  "2023/29",  "2023/30"))

RS22_22_23_GRAF_CE_Confirmados <- ggplot(AUX_GRAF, aes(Ordem))  +
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos CONFIRMADOS - 2022/23") +
  theme(
    panel.grid.major = element_line(color = "#C0C0C0"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#DC143C"),
    plot.title = element_text(face = "bold",
                              size = 19)
  ) +
  geom_area(aes(,Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
  geom_area(aes(,Media), fill = "#556B2F") +
  geom_line(aes(,`2022/23`), stat = "identity", color = "black", linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI)

######Histogramas

###NOTIFICADOS

AUX_GRAF <- as.data.frame(RS22_22_23_SE_Notificados$Município)

AUX_GRAF[, 2] <- as.data.frame(SE - 9)
AUX_GRAF[, 3] <- as.data.frame(SE - 8)
AUX_GRAF[, 4] <- as.data.frame(SE - 7)
AUX_GRAF[, 5] <- as.data.frame(SE - 6)
AUX_GRAF[, 6] <- as.data.frame(SE - 5)
AUX_GRAF[, 7] <- as.data.frame(SE - 4)
AUX_GRAF[, 8] <- as.data.frame(SE - 3)
AUX_GRAF[, 9] <- as.data.frame(SE - 2)
AUX_GRAF[, 10] <- as.data.frame(SE - 1)
AUX_GRAF[, 11] <- as.data.frame(SE)

colnames(AUX_GRAF)[1] <- "Municipios"
colnames(AUX_GRAF)[2] <- as.data.frame(SE - 9)
colnames(AUX_GRAF)[3] <- as.data.frame(SE - 8)
colnames(AUX_GRAF)[4] <- as.data.frame(SE - 7)
colnames(AUX_GRAF)[5] <- as.data.frame(SE - 6)
colnames(AUX_GRAF)[6] <- as.data.frame(SE - 5)
colnames(AUX_GRAF)[7] <- as.data.frame(SE - 4)
colnames(AUX_GRAF)[8] <- as.data.frame(SE - 3)
colnames(AUX_GRAF)[9] <- as.data.frame(SE - 2)
colnames(AUX_GRAF)[10] <- as.data.frame(SE - 1)
colnames(AUX_GRAF)[11] <- as.data.frame(SE)

AUX_GRAF[, 2] <- RS22_22_23_SE_Notificados[, which(colnames(RS22_22_23_SE_Notificados) == SE - 9)]
AUX_GRAF[, 3] <- RS22_22_23_SE_Notificados[, which(colnames(RS22_22_23_SE_Notificados) == SE - 8)]
AUX_GRAF[, 4] <- RS22_22_23_SE_Notificados[, which(colnames(RS22_22_23_SE_Notificados) == SE - 7)]
AUX_GRAF[, 5] <- RS22_22_23_SE_Notificados[, which(colnames(RS22_22_23_SE_Notificados) == SE - 6)]
AUX_GRAF[, 6] <- RS22_22_23_SE_Notificados[, which(colnames(RS22_22_23_SE_Notificados) == SE - 5)]
AUX_GRAF[, 7] <- RS22_22_23_SE_Notificados[, which(colnames(RS22_22_23_SE_Notificados) == SE - 4)]
AUX_GRAF[, 8] <- RS22_22_23_SE_Notificados[, which(colnames(RS22_22_23_SE_Notificados) == SE - 3)]
AUX_GRAF[, 9] <- RS22_22_23_SE_Notificados[, which(colnames(RS22_22_23_SE_Notificados) == SE - 2)]
AUX_GRAF[, 10] <- RS22_22_23_SE_Notificados[, which(colnames(RS22_22_23_SE_Notificados) == SE - 1)]
AUX_GRAF[, 11] <- RS22_22_23_SE_Notificados[, which(colnames(RS22_22_23_SE_Notificados) == SE)]

AUX_GRAF[17,] <- colnames(AUX_GRAF)

AUX_GRAF <- AUX_GRAF[c(17, 1:16),]

AUX_GRAF <- t(AUX_GRAF)

colnames(AUX_GRAF) <- AUX_GRAF[1,]

AUX_GRAF <- AUX_GRAF[-1,]

colnames(AUX_GRAF)[1] <- "SE"

AUX_GRAF <- data.frame(SE = AUX_GRAF[, 1],
                       Arapuã = as.numeric(AUX_GRAF[, 2]),
                       Ariranha_do_Ivaí = as.numeric(AUX_GRAF[, 3]),
                       Cândido_de_Abreu = as.numeric(AUX_GRAF[, 4]),
                       Cruzmaltina = as.numeric(AUX_GRAF[, 5]),
                       Godoy_Moreira = as.numeric(AUX_GRAF[, 6]),
                       Ivaiporã = as.numeric(AUX_GRAF[, 7]),
                       Jardim_Alegre = as.numeric(AUX_GRAF[, 8]),
                       Lidianópolis = as.numeric(AUX_GRAF[, 9]),
                       Lunardelli = as.numeric(AUX_GRAF[, 10]),
                       Manoel_Ribas = as.numeric(AUX_GRAF[, 11]),
                       Mato_Rico = as.numeric(AUX_GRAF[, 12]),
                       Nova_Tebas = as.numeric(AUX_GRAF[, 13]),
                       Rio_Branco_do_Ivaí = as.numeric(AUX_GRAF[, 14]),
                       Rosário_do_Ivaí = as.numeric(AUX_GRAF[, 15]),
                       Santa_Maria_do_Oeste = as.numeric(AUX_GRAF[, 16]),
                       São_João_do_Ivaí = as.numeric(AUX_GRAF[, 17]))

SE_HIST_NOT_ARAPUÃ <- ggplot (AUX_GRAF, 
                              aes(x = SE, 
                                  y = Arapuã)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "ARAPUÃ") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Arapuã),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_NOT_ARIRANHA <- ggplot (AUX_GRAF, 
                                aes(x = SE, 
                                    y = Ariranha_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "ARIRANHA DO IVAÍ") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Ariranha_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_NOT_CANDIDO <- ggplot (AUX_GRAF, 
                               aes(x = SE, 
                                   y = Cândido_de_Abreu)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "CÂNDIDO DE ABREU") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Cândido_de_Abreu),
             alpha = 0.5,
             nudge_y = 1)

SE_HIST_NOT_CRUZMALTINA <- ggplot (AUX_GRAF, 
                                   aes(x = SE, 
                                       y = Cruzmaltina)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "CRUZMALTINA") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Cruzmaltina),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_NOT_GODOY <- ggplot (AUX_GRAF, 
                             aes(x = SE, 
                                 y = Godoy_Moreira)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "GODOY MOREIRA") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Godoy_Moreira),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_NOT_IVAIPORA <- ggplot (AUX_GRAF, 
                                aes(x = SE, 
                                    y = Ivaiporã)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "IVAIPORÃ") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Ivaiporã),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_NOT_JARDIM <- ggplot (AUX_GRAF, 
                              aes(x = SE, 
                                  y = Jardim_Alegre)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "JARDIM ALEGRE") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Jardim_Alegre),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_NOT_LIDIANÓPOLIS <- ggplot (AUX_GRAF, 
                                    aes(x = SE, 
                                        y = Lidianópolis)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "LIDIANÓPOLIS") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Lidianópolis),
             alpha = 0.5,
             nudge_y = 0.01)

RS22_22_23_GRAF_Histograma_Notificados_01 <- (SE_HIST_NOT_ARAPUÃ + SE_HIST_NOT_ARIRANHA) / (SE_HIST_NOT_CANDIDO + SE_HIST_NOT_CRUZMALTINA) / (SE_HIST_NOT_GODOY + SE_HIST_NOT_IVAIPORA) / (SE_HIST_NOT_JARDIM + SE_HIST_NOT_LIDIANÓPOLIS) 

SE_HIST_NOT_LUNARDELLI <- ggplot (AUX_GRAF, 
                                  aes(x = SE, 
                                      y = Lunardelli)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "LUNARDELLI") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Lunardelli),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_NOT_MANOEL_RIBAS <- ggplot (AUX_GRAF, 
                                    aes(x = SE, 
                                        y = Manoel_Ribas)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "MANOEL RIBAS") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Manoel_Ribas),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_NOT_MATO_RICO <- ggplot (AUX_GRAF, 
                                 aes(x = SE, 
                                     y = Mato_Rico)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "MATO RICO") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Mato_Rico),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_NOT_NOVA_TEBAS <- ggplot (AUX_GRAF, 
                                  aes(x = SE, 
                                      y = Nova_Tebas)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "NOVA TEBAS") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Nova_Tebas),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_NOT_RBI <- ggplot (AUX_GRAF, 
                           aes(x = SE, 
                               y = Rio_Branco_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "RIO BRANCO DO IVAÍ") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Rio_Branco_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_NOT_RSI <- ggplot (AUX_GRAF, 
                           aes(x = SE, 
                               y = Rosário_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "ROSÁRIO DO IVAÍ") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Rosário_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_NOT_SMO <- ggplot (AUX_GRAF, 
                           aes(x = SE, 
                               y = Santa_Maria_do_Oeste)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "SANTA MARIA DO OESTE") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Santa_Maria_do_Oeste),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_NOT_SJI <- ggplot (AUX_GRAF, 
                           aes(x = SE, 
                               y = São_João_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "SÃO JOÃO DO IVAÍ") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = São_João_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01)

RS22_22_23_GRAF_Histograma_Notificados_02 <- (SE_HIST_NOT_LUNARDELLI + SE_HIST_NOT_MANOEL_RIBAS) / (SE_HIST_NOT_MATO_RICO + SE_HIST_NOT_NOVA_TEBAS) / (SE_HIST_NOT_RBI + SE_HIST_NOT_RSI) / (SE_HIST_NOT_SMO + SE_HIST_NOT_SJI) 

###CONFIRMADOS

AUX_GRAF <- as.data.frame(RS22_22_23_SE_Confirmados$Município)

AUX_GRAF[, 2] <- as.data.frame(SE - 9)
AUX_GRAF[, 3] <- as.data.frame(SE - 8)
AUX_GRAF[, 4] <- as.data.frame(SE - 7)
AUX_GRAF[, 5] <- as.data.frame(SE - 6)
AUX_GRAF[, 6] <- as.data.frame(SE - 5)
AUX_GRAF[, 7] <- as.data.frame(SE - 4)
AUX_GRAF[, 8] <- as.data.frame(SE - 3)
AUX_GRAF[, 9] <- as.data.frame(SE - 2)
AUX_GRAF[, 10] <- as.data.frame(SE - 1)
AUX_GRAF[, 11] <- as.data.frame(SE)

colnames(AUX_GRAF)[1] <- "Municipios"
colnames(AUX_GRAF)[2] <- as.data.frame(SE - 9)
colnames(AUX_GRAF)[3] <- as.data.frame(SE - 8)
colnames(AUX_GRAF)[4] <- as.data.frame(SE - 7)
colnames(AUX_GRAF)[5] <- as.data.frame(SE - 6)
colnames(AUX_GRAF)[6] <- as.data.frame(SE - 5)
colnames(AUX_GRAF)[7] <- as.data.frame(SE - 4)
colnames(AUX_GRAF)[8] <- as.data.frame(SE - 3)
colnames(AUX_GRAF)[9] <- as.data.frame(SE - 2)
colnames(AUX_GRAF)[10] <- as.data.frame(SE - 1)
colnames(AUX_GRAF)[11] <- as.data.frame(SE)

AUX_GRAF[, 2] <- RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE - 9)]
AUX_GRAF[, 3] <- RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE - 8)]
AUX_GRAF[, 4] <- RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE - 7)]
AUX_GRAF[, 5] <- RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE - 6)]
AUX_GRAF[, 6] <- RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE - 5)]
AUX_GRAF[, 7] <- RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE - 4)]
AUX_GRAF[, 8] <- RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE - 3)]
AUX_GRAF[, 9] <- RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE - 2)]
AUX_GRAF[, 10] <- RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE - 1)]
AUX_GRAF[, 11] <- RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE)]

AUX_GRAF[17,] <- colnames(AUX_GRAF)

AUX_GRAF <- AUX_GRAF[c(17, 1:16),]

AUX_GRAF <- t(AUX_GRAF)

colnames(AUX_GRAF) <- AUX_GRAF[1,]

AUX_GRAF <- AUX_GRAF[-1,]

colnames(AUX_GRAF)[1] <- "SE"

AUX_GRAF <- data.frame(SE = AUX_GRAF[, 1],
                       Arapuã = as.numeric(AUX_GRAF[, 2]),
                       Ariranha_do_Ivaí = as.numeric(AUX_GRAF[, 3]),
                       Cândido_de_Abreu = as.numeric(AUX_GRAF[, 4]),
                       Cruzmaltina = as.numeric(AUX_GRAF[, 5]),
                       Godoy_Moreira = as.numeric(AUX_GRAF[, 6]),
                       Ivaiporã = as.numeric(AUX_GRAF[, 7]),
                       Jardim_Alegre = as.numeric(AUX_GRAF[, 8]),
                       Lidianópolis = as.numeric(AUX_GRAF[, 9]),
                       Lunardelli = as.numeric(AUX_GRAF[, 10]),
                       Manoel_Ribas = as.numeric(AUX_GRAF[, 11]),
                       Mato_Rico = as.numeric(AUX_GRAF[, 12]),
                       Nova_Tebas = as.numeric(AUX_GRAF[, 13]),
                       Rio_Branco_do_Ivaí = as.numeric(AUX_GRAF[, 14]),
                       Rosário_do_Ivaí = as.numeric(AUX_GRAF[, 15]),
                       Santa_Maria_do_Oeste = as.numeric(AUX_GRAF[, 16]),
                       São_João_do_Ivaí = as.numeric(AUX_GRAF[, 17]))

SE_HIST_CONF_ARAPUÃ <- ggplot (AUX_GRAF, 
                               aes(x = SE, 
                                   y = Arapuã)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "ARAPUÃ") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Arapuã),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_CONF_ARIRANHA <- ggplot (AUX_GRAF, 
                                 aes(x = SE, 
                                     y = Ariranha_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "ARIRANHA DO IVAÍ") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Ariranha_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_CONF_CANDIDO <- ggplot (AUX_GRAF, 
                                aes(x = SE, 
                                    y = Cândido_de_Abreu)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "CÂNDIDO DE ABREU") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Cândido_de_Abreu),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_CONF_CRUZMALTINA <- ggplot (AUX_GRAF, 
                                    aes(x = SE, 
                                        y = Cruzmaltina)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "CRUZMALTINA") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Cruzmaltina),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_CONF_GODOY <- ggplot (AUX_GRAF, 
                              aes(x = SE, 
                                  y = Godoy_Moreira)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "GODOY MOREIRA") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Godoy_Moreira),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_CONF_IVAIPORA <- ggplot (AUX_GRAF, 
                                 aes(x = SE, 
                                     y = Ivaiporã)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "IVAIPORÃ") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Ivaiporã),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_CONF_JARDIM <- ggplot (AUX_GRAF, 
                               aes(x = SE, 
                                   y = Jardim_Alegre)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "JARDIM ALEGRE") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Jardim_Alegre),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_CONF_LIDIANÓPOLIS <- ggplot (AUX_GRAF, 
                                     aes(x = SE, 
                                         y = Lidianópolis)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "LIDIANÓPOLIS") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Lidianópolis),
             alpha = 0.5,
             nudge_y = 0.01)

RS22_22_23_GRAF_Histograma_Confirmados_01 <- (SE_HIST_CONF_ARAPUÃ + SE_HIST_CONF_ARIRANHA) / (SE_HIST_CONF_CANDIDO + SE_HIST_CONF_CRUZMALTINA) / (SE_HIST_CONF_GODOY + SE_HIST_CONF_IVAIPORA) / (SE_HIST_CONF_JARDIM + SE_HIST_CONF_LIDIANÓPOLIS) 

SE_HIST_CONF_LUNARDELLI <- ggplot (AUX_GRAF, 
                                   aes(x = SE, 
                                       y = Lunardelli)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "LUNARDELLI") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Lunardelli),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_CONF_MANOEL_RIBAS <- ggplot (AUX_GRAF, 
                                     aes(x = SE, 
                                         y = Manoel_Ribas)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "MANOEL RIBAS") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Manoel_Ribas),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_CONF_MATO_RICO <- ggplot (AUX_GRAF, 
                                  aes(x = SE, 
                                      y = Mato_Rico)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "MATO RICO") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Mato_Rico),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_CONF_NOVA_TEBAS <- ggplot (AUX_GRAF, 
                                   aes(x = SE, 
                                       y = Nova_Tebas)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "NOVA TEBAS") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Nova_Tebas),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_CONF_RBI <- ggplot (AUX_GRAF, 
                            aes(x = SE, 
                                y = Rio_Branco_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "RIO BRANCO DO IVAÍ") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Rio_Branco_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_CONF_RSI <- ggplot (AUX_GRAF, 
                            aes(x = SE, 
                                y = Rosário_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "ROSÁRIO DO IVAÍ") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Rosário_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_CONF_SMO <- ggplot (AUX_GRAF, 
                            aes(x = SE, 
                                y = Santa_Maria_do_Oeste)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "SANTA MARIA DO OESTE") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Santa_Maria_do_Oeste),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_CONF_SJI <- ggplot (AUX_GRAF, 
                            aes(x = SE, 
                                y = São_João_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "SÃO JOÃO DO IVAÍ") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = São_João_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01)

RS22_22_23_GRAF_Histograma_Confirmados_02 <- (SE_HIST_CONF_LUNARDELLI + SE_HIST_CONF_MANOEL_RIBAS) / (SE_HIST_CONF_MATO_RICO + SE_HIST_CONF_NOVA_TEBAS) / (SE_HIST_CONF_RBI + SE_HIST_CONF_RSI) / (SE_HIST_CONF_SMO + SE_HIST_CONF_SJI) 

###Construindo um for loop para realizar a tabela de Prováveis por semana epidemiológica###

RS22_22_23_SE_Provaveis <- matrix(data = NA, 
                                  nrow = 16, 
                                  ncol = 54)

RS22_22_23_SE_Provaveis <- as.data.frame(RS22_22_23_SE_Provaveis)

colnames(RS22_22_23_SE_Provaveis)[1] <- "Município" 

RS22_22_23_SE_Provaveis[,1] <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

colnames (RS22_22_23_SE_Provaveis)[2:24] <- c(31:53)

colnames (RS22_22_23_SE_Provaveis)[25:54] <- c(1:30)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 2] <- as.integer(RS22_22_23_SINAN %>%
                                                                                  filter(ID_MN_RESI == i,
                                                                                         SEM_PRI ==202231)%>%
                                                                                  count()
                                                                                -
                                                                                  RS22_22_23_SINAN %>%
                                                                                  filter(ID_MN_RESI == i,
                                                                                         SEM_PRI == 202231,
                                                                                         CLASSI_FIN == 5) %>%
                                                                                  count()
  )                                                                                       
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 3] <- as.integer(RS22_22_23_SINAN %>%
                                                                                  filter(ID_MN_RESI == i,
                                                                                         SEM_PRI ==202231)%>%
                                                                                  count()
                                                                                -
                                                                                  RS22_22_23_SINAN %>%
                                                                                  filter(ID_MN_RESI == i,
                                                                                         SEM_PRI == 202231,
                                                                                         CLASSI_FIN == 5) %>%
                                                                                  count()
  )  
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 4] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                  filter(ID_MN_RESI == i,
                                                                                         SEM_PRI ==202233) %>% 
                                                                                  count()
                                                                                -
                                                                                  RS22_22_23_SINAN %>%
                                                                                  filter(ID_MN_RESI == i,
                                                                                         SEM_PRI == 202233,
                                                                                         CLASSI_FIN == 5) %>%
                                                                                  count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i),5] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                 filter(ID_MN_RESI == i,
                                                                                        SEM_PRI ==202234) %>% 
                                                                                 count()
                                                                               -
                                                                                 RS22_22_23_SINAN %>%
                                                                                 filter(ID_MN_RESI == i,
                                                                                        SEM_PRI == 202234,
                                                                                        CLASSI_FIN == 5) %>%
                                                                                 count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 6] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                  filter(ID_MN_RESI == i,
                                                                                         SEM_PRI ==202235) %>% 
                                                                                  count()
                                                                                -
                                                                                  RS22_22_23_SINAN %>%
                                                                                  filter(ID_MN_RESI == i,
                                                                                         SEM_PRI == 202235,
                                                                                         CLASSI_FIN == 5) %>%
                                                                                  count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 7] <- as.integer(RS22_22_23_SINAN %>%
                                                                                  filter(ID_MN_RESI == i, 
                                                                                         SEM_PRI ==202236) %>%
                                                                                  count()
                                                                                -
                                                                                  RS22_22_23_SINAN %>%
                                                                                  filter(ID_MN_RESI == i,
                                                                                         SEM_PRI == 202236,
                                                                                         CLASSI_FIN == 5) %>%
                                                                                  count()
  ) 
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 8] <- as.integer(RS22_22_23_SINAN %>%
                                                                                  filter(ID_MN_RESI == i, 
                                                                                         SEM_PRI ==202237) %>% 
                                                                                  count() 
                                                                                -
                                                                                  RS22_22_23_SINAN %>%
                                                                                  filter(ID_MN_RESI == i,
                                                                                         SEM_PRI == 202237,
                                                                                         CLASSI_FIN == 5) %>%
                                                                                  count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 9] <- as.integer(RS22_22_23_SINAN %>%
                                                                                  filter(ID_MN_RESI == i, 
                                                                                         SEM_PRI ==202238) %>% 
                                                                                  count() 
                                                                                -
                                                                                  RS22_22_23_SINAN %>%
                                                                                  filter(ID_MN_RESI == i,
                                                                                         SEM_PRI == 202238,
                                                                                         CLASSI_FIN == 5) %>%
                                                                                  count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 10] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202239) %>%
                                                                                   count()
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202239,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 11] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202240) %>%
                                                                                   count()
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202240,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 12] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202241) %>%
                                                                                   count()
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202241,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 13] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202242) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202242,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )  
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 14] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202243) %>% 
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202243,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )   
  
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 15] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202244) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202244,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 16] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202245) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202245,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )  
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 17] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202246) %>%
                                                                                   count()
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202246,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 18] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202247) %>%  
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202247,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  ) 
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 19] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202248) %>%   
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202248,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )   
  
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 20] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202249) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202249,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i),  21] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                    filter(ID_MN_RESI == i, 
                                                                                           SEM_PRI ==202250) %>%
                                                                                    count() 
                                                                                  -
                                                                                    RS22_22_23_SINAN %>%
                                                                                    filter(ID_MN_RESI == i,
                                                                                           SEM_PRI == 202250,
                                                                                           CLASSI_FIN == 5) %>%
                                                                                    count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 22] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202251) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202251,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 23] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202252) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202252,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 24] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202253) %>%
                                                                                   count()
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202253,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 25] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202301) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202301,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 26] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202302) %>%
                                                                                   count()
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202302,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 27] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202303) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202303,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 28] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202304) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202304,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 29] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202305) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202305,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 30] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202306) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202306,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )   
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 31] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202307) %>% 
                                                                                   count()
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202307,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  ) 
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 32] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202308) %>%
                                                                                   count()
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202308,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  ) 
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 33] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202309) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202309,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  ) 
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 34] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202310) %>% 
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202310,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  ) 
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 35] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202311) %>% 
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202311,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  ) 
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 36] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202312) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202312,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  ) 
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 37] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202313) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202313,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 38] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202314) %>% 
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202314,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 39] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202315) %>%
                                                                                   count()
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202315,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 40] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202316) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202316,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 41] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202317) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202317,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 42] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202318) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202318,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  ) 
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 43] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202319) %>%
                                                                                   count()
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202319,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 44] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202320) %>%
                                                                                   count()
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202320,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 45] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202321) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202321,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 46] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202322) %>%
                                                                                   count()
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202322,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 47] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202323) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202323,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 48] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202324) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202324,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 49] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202325) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202325,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 50] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202326) %>% 
                                                                                   count()
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202326,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  ) 
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 51] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202327) %>% 
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202327,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 52] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202328) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202328,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 53] <- as.integer(RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI ==202329) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202329,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  ) 
  
  RS22_22_23_SE_Provaveis[which(RS22_22_23_SE_Provaveis == i), 54] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          SEM_PRI ==202330) %>%
                                                                                   count() 
                                                                                 -
                                                                                   RS22_22_23_SINAN %>%
                                                                                   filter(ID_MN_RESI == i,
                                                                                          SEM_PRI == 202330,
                                                                                          CLASSI_FIN == 5) %>%
                                                                                   count()
  ) 
}

RS22_22_23_SE_Provaveis[,1] <- BASE_IBGE[which(BASE_IBGE$RS == 22), 3]

RS22_22_23_SE_Provaveis[17,2:54] <- apply(RS22_22_23_SE_Provaveis[,2:54], 2, sum)

RS22_22_23_SE_Provaveis[17,1] <- "Total"

AUX_GRAF <- as.data.frame(RS22_22_23_SE_Provaveis$Município)

AUX_GRAF[, 2] <- as.data.frame(SE - 9)
AUX_GRAF[, 3] <- as.data.frame(SE - 8)
AUX_GRAF[, 4] <- as.data.frame(SE - 7)
AUX_GRAF[, 5] <- as.data.frame(SE - 6)
AUX_GRAF[, 6] <- as.data.frame(SE - 5)
AUX_GRAF[, 7] <- as.data.frame(SE - 4)
AUX_GRAF[, 8] <- as.data.frame(SE - 3)
AUX_GRAF[, 9] <- as.data.frame(SE - 2)
AUX_GRAF[, 10] <- as.data.frame(SE - 1)
AUX_GRAF[, 11] <- as.data.frame(SE)

colnames(AUX_GRAF)[1] <- "Municipios"
colnames(AUX_GRAF)[2] <- as.data.frame(SE - 9)
colnames(AUX_GRAF)[3] <- as.data.frame(SE - 8)
colnames(AUX_GRAF)[4] <- as.data.frame(SE - 7)
colnames(AUX_GRAF)[5] <- as.data.frame(SE - 6)
colnames(AUX_GRAF)[6] <- as.data.frame(SE - 5)
colnames(AUX_GRAF)[7] <- as.data.frame(SE - 4)
colnames(AUX_GRAF)[8] <- as.data.frame(SE - 3)
colnames(AUX_GRAF)[9] <- as.data.frame(SE - 2)
colnames(AUX_GRAF)[10] <- as.data.frame(SE - 1)
colnames(AUX_GRAF)[11] <- as.data.frame(SE)

AUX_GRAF[, 2] <- RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE - 9)]
AUX_GRAF[, 3] <- RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE - 8)]
AUX_GRAF[, 4] <- RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE - 7)]
AUX_GRAF[, 5] <- RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE - 6)]
AUX_GRAF[, 6] <- RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE - 5)]
AUX_GRAF[, 7] <- RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE - 4)]
AUX_GRAF[, 8] <- RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE - 3)]
AUX_GRAF[, 9] <- RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE - 2)]
AUX_GRAF[, 10] <- RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE - 1)]
AUX_GRAF[, 11] <- RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE)]

AUX_GRAF[17,] <- colnames(AUX_GRAF)

AUX_GRAF <- AUX_GRAF[c(17, 1:16),]

AUX_GRAF <- t(AUX_GRAF)

colnames(AUX_GRAF) <- AUX_GRAF[1,]

AUX_GRAF <- AUX_GRAF[-1,]

colnames(AUX_GRAF)[1] <- "SE"

AUX_GRAF <- data.frame(SE = AUX_GRAF[, 1],
                       Arapuã = as.numeric(AUX_GRAF[, 2]),
                       Ariranha_do_Ivaí = as.numeric(AUX_GRAF[, 3]),
                       Cândido_de_Abreu = as.numeric(AUX_GRAF[, 4]),
                       Cruzmaltina = as.numeric(AUX_GRAF[, 5]),
                       Godoy_Moreira = as.numeric(AUX_GRAF[, 6]),
                       Ivaiporã = as.numeric(AUX_GRAF[, 7]),
                       Jardim_Alegre = as.numeric(AUX_GRAF[, 8]),
                       Lidianópolis = as.numeric(AUX_GRAF[, 9]),
                       Lunardelli = as.numeric(AUX_GRAF[, 10]),
                       Manoel_Ribas = as.numeric(AUX_GRAF[, 11]),
                       Mato_Rico = as.numeric(AUX_GRAF[, 12]),
                       Nova_Tebas = as.numeric(AUX_GRAF[, 13]),
                       Rio_Branco_do_Ivaí = as.numeric(AUX_GRAF[, 14]),
                       Rosário_do_Ivaí = as.numeric(AUX_GRAF[, 15]),
                       Santa_Maria_do_Oeste = as.numeric(AUX_GRAF[, 16]),
                       São_João_do_Ivaí = as.numeric(AUX_GRAF[, 17]))

SE_HIST_PROV_ARAPUÃ <- ggplot (AUX_GRAF, 
                               aes(x = SE, 
                                   y = Arapuã)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "ARAPUÃ") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Arapuã),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_PROV_ARIRANHA <- ggplot (AUX_GRAF, 
                                 aes(x = SE, 
                                     y = Ariranha_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "ARIRANHA DO IVAÍ") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Ariranha_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_PROV_CANDIDO <- ggplot (AUX_GRAF, 
                                aes(x = SE, 
                                    y = Cândido_de_Abreu)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "CÂNDIDO DE ABREU") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Cândido_de_Abreu),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_PROV_CRUZMALTINA <- ggplot (AUX_GRAF, 
                                    aes(x = SE, 
                                        y = Cruzmaltina)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "CRUZMALTINA") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Cruzmaltina),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_PROV_GODOY <- ggplot (AUX_GRAF, 
                              aes(x = SE, 
                                  y = Godoy_Moreira)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "GODOY MOREIRA") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Godoy_Moreira),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_PROV_IVAIPORA <- ggplot (AUX_GRAF, 
                                 aes(x = SE, 
                                     y = Ivaiporã)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "IVAIPORÃ") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Ivaiporã),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_PROV_JARDIM <- ggplot (AUX_GRAF, 
                               aes(x = SE, 
                                   y = Jardim_Alegre)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "JARDIM ALEGRE") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Jardim_Alegre),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_PROV_LIDIANÓPOLIS <- ggplot (AUX_GRAF, 
                                     aes(x = SE, 
                                         y = Lidianópolis)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "LIDIANÓPOLIS") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Lidianópolis),
             alpha = 0.5,
             nudge_y = 0.01)

RS22_22_23_GRAF_Histograma_Provaveis_01 <- (SE_HIST_PROV_ARAPUÃ + SE_HIST_PROV_ARIRANHA) / (SE_HIST_PROV_CANDIDO + SE_HIST_PROV_CRUZMALTINA) / (SE_HIST_PROV_GODOY + SE_HIST_PROV_IVAIPORA) / (SE_HIST_PROV_JARDIM + SE_HIST_PROV_LIDIANÓPOLIS) 

SE_HIST_PROV_LUNARDELLI <- ggplot (AUX_GRAF, 
                                   aes(x = SE, 
                                       y = Lunardelli)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "LUNARDELLI") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Lunardelli),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_PROV_MANOEL_RIBAS <- ggplot (AUX_GRAF, 
                                     aes(x = SE, 
                                         y = Manoel_Ribas)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "MANOEL RIBAS") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Manoel_Ribas),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_PROV_MATO_RICO <- ggplot (AUX_GRAF, 
                                  aes(x = SE, 
                                      y = Mato_Rico)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "MATO RICO") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Mato_Rico),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_PROV_NOVA_TEBAS <- ggplot (AUX_GRAF, 
                                   aes(x = SE, 
                                       y = Nova_Tebas)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "NOVA TEBAS") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Nova_Tebas),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_PROV_RBI <- ggplot (AUX_GRAF, 
                            aes(x = SE, 
                                y = Rio_Branco_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "RIO BRANCO DO IVAÍ") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Rio_Branco_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_PROV_RSI <- ggplot (AUX_GRAF, 
                            aes(x = SE, 
                                y = Rosário_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "ROSÁRIO DO IVAÍ") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Rosário_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_PROV_SMO <- ggplot (AUX_GRAF, 
                            aes(x = SE, 
                                y = Santa_Maria_do_Oeste)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "SANTA MARIA DO OESTE") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = Santa_Maria_do_Oeste),
             alpha = 0.5,
             nudge_y = 0.01)

SE_HIST_PROV_SJI <- ggplot (AUX_GRAF, 
                            aes(x = SE, 
                                y = São_João_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "SÃO JOÃO DO IVAÍ") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#98FB98") + 
  geom_label(aes(label = São_João_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01)

RS22_22_23_GRAF_Histograma_Provaveis_02 <- (SE_HIST_PROV_LUNARDELLI + SE_HIST_PROV_MANOEL_RIBAS) / (SE_HIST_PROV_MATO_RICO + SE_HIST_PROV_NOVA_TEBAS) / (SE_HIST_PROV_RBI + SE_HIST_PROV_RSI) / (SE_HIST_PROV_SMO + SE_HIST_PROV_SJI) 

rm(AUX_GRAF)

#####Salvando as tabelas#####

write.csv(PR_22_23_DENGUE_MUNICIPIOS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/PR_22_23_DENGUE_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(PR_22_23_CHIKUNGUNYA_MUNICIPIOS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/PR_22_23_CHIKUNGUNYA_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(PR_22_23_ZIKA_MUNICIPIOS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/PR_22_23_ZIKA_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(RS22_22_23_REDE_OVITRAMPAS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_REDE_OVITRAMPAS.csv",
          row.names = FALSE)

write.csv(RS22_22_23_CICLOS_LOCALIDADES, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_CICLOS_LOCALIDADES.csv",
          row.names = FALSE)

write.csv(RS22_22_23_CICLOS_MUNICIPIOS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_CICLOS_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(RS22_22_23_RG_MUNICIPIOS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_RG_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(RS22_22_23_RG_LOCALIDADES, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_RG_LOCALIDADES.csv",
          row.names = FALSE)

write.csv(RS22_22_23_PE, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_PE.csv",
          row.names = FALSE)

write.csv(RS22_22_23_ASSISTENCIA, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_ASSISTENCIA.csv",
          row.names = FALSE)

write.csv(RS22_22_23_GERAL, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_GERAL.csv",
          row.names = FALSE)

write.csv(RS22_22_23_EXTRA, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_EXTRA.csv",
          row.names = FALSE)

write.csv(RS22_22_23_SINAN, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_SINAN.csv",
          row.names = FALSE)

write.csv(RS22_22_23_SINAN_DECODIFICADO, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_SINAN_DECODIFICADO.csv",
          row.names = FALSE)

write.csv(RS22_22_23_SINAIS_DE_ALARME, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_SINAIS_DE_ALARME.csv",
          row.names = FALSE)

write.csv(RS22_22_23_SINAIS_Notificados, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_SINAIS_Notificados.csv",
          row.names = FALSE)

write.csv(RS22_22_23_SINAIS_Confirmados, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_SINAIS_Confirmados.csv",
          row.names = FALSE)

write.csv(RS22_22_23_SE_Confirmados, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_SE_Confirmados.csv",
          row.names = FALSE)

write.csv(RS22_22_23_SE_Notificados, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_SE_Notificados.csv",
          row.names = FALSE)

write.csv(RS22_22_23_DENGUE_GRAVE, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_SE_DENGUE_GRAVE.csv",
          row.names = FALSE)
~ 
  write.csv(RS22_22_23_DOENCAS_PRE_EXISTENTES, 
            "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_DOENCAS_PRE_EXISTENTES.csv",
            row.names = FALSE)


####Buscando a planilha RS22_22_23_SINAN_DECODIFICADO do google sheets com as coordenadas geográficas inseridas pelos municípios####

RS22_22_23_SINAN_DECODIFICADO <- read_sheet("https://docs.google.com/spreadsheets/d/16TyAkaCH-ZE6Xq7AfEGXGvX0jRDbxj8QUvzNQcw8zFQ/edit#gid=1437725143")


####Gravando a planilha RS22_22_23_SINAN_DECODIFICADO no diretório para ser utilizada pelo QGIS###

write.csv(RS22_22_23_SINAN_DECODIFICADO,  "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_SINAN_DECODIFICADO.csv",
          row.names = FALSE)

#####Salvando os Gráficos

###Série Histórica

RS22_22_23_GRAF_SERIES_HISTORICAS <- (RS22_Serie_Historica_GRAF_Not_Conf / (RS22_Serie_Historica_GRAF_Sorotipo + RS22_Serie_Historica_GRAF_Hospitalizados))

ggsave(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Graficos_Mapas/Serie_Historica.png", 
       plot = RS22_22_23_GRAF_SERIES_HISTORICAS,
       width = 12.51,
       height = 15.51)


###Notificados/Municípios

ggsave(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Graficos_Mapas/RS22_GRAF_22_23_Notificados_.png", 
       plot = RS22_22_23_GRAF_Notificados,
       width = 15.51,
       height = 8.51) 

###Autóctones

ggsave(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Graficos_Mapas/RS22_GRAF_22_23_Autoctones_.png", 
       plot = RS22_22_23_GRAF_Autoctones,
       width = 15.51,
       height = 8.51) 

###Em Investigação

ggsave(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Graficos_Mapas/RS22_GRAF_22_23_Investigacao_.png", 
       plot = RS22_22_23_GRAF_Investigacao,
       width = 15.51,
       height = 8.51) 

###Confirmados

ggsave(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Graficos_Mapas/RS22_GRAF_22_23_IConfirmados_.png", 
       plot = RS22_22_23_GRAF_Confirmados,
       width = 15.51,
       height = 8.51) 

###Incidência

ggsave(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Graficos_Mapas/RS22_GRAF_22_23_Incidencia_.png", 
       plot = RS22_22_23_GRAF_Incidencia,
       width = 15.51,
       height = 8.51) 

###Descartados

ggsave(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Graficos_Mapas/RS22_GRAF_22_23_Descartados_.png", 
       plot = RS22_22_23_GRAF_Descartados,
       width = 15.51,
       height = 8.51) 

###Hospitalização

ggsave(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Graficos_Mapas/RS22_GRAF_22_23_Hospitalizados_.png", 
       plot = RS22_22_23_GRAF_Hospitalizados,
       width = 15.51,
       height = 8.51) 

###Sintomas

ggsave(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Graficos_Mapas/RS22_GRAF_22_23_SINAIS_.png", 
       plot = RS22_22_23_GRAF_SINAIS,
       width = 15.51,
       height = 8.51) 

###Histogramas Notificados

##PAG 01

ggsave(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Graficos_Mapas/RS22_22_23_GRAF_Histograma_Notificados_01.png", 
       plot = RS22_22_23_GRAF_Histograma_Notificados_01,
       width = 12.51,
       height = 15.51) 

##PAG 02

ggsave(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Graficos_Mapas//RS22_22_23_GRAF_Histograma_Notificados_02.png", 
       plot = RS22_22_23_GRAF_Histograma_Notificados_02,
       width = 12.51,
       height = 15.51) 

###Histogramas Confirmados

##PAG 01

ggsave(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Graficos_Mapas/RS22_22_23_GRAF_Histograma_Confirmados_01.png", 
       plot = RS22_22_23_GRAF_Histograma_Confirmados_01,
       width = 12.51,
       height = 15.51) 

##PAG 02

ggsave(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Graficos_Mapas/RS22_22_23_GRAF_Histograma_Confirmados_02.png", 
       plot = RS22_22_23_GRAF_Histograma_Confirmados_02,
       width = 12.51,
       height = 15.51) 

###Canal Endêmicos Notificados/Confirmados

RS22_22_23_GRAF_CANAIS_ENDEMICOS <- (RS22_22_23_GRAF_CE_Notificados / RS22_22_23_GRAF_CE_Confirmados)

ggsave(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Graficos_Mapas/Canal_Endemico_22_23_CE.png", 
       plot = RS22_22_23_GRAF_CANAIS_ENDEMICOS,
       width = 12.51,
       height = 15.51)

####Canal Endêmico Prováveis

RS22_22_23_GRAF_CANAL_ENDEMICO_PROVAVEIS <- RS22_22_23_GRAF_CE_Provaveis

ggsave(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Graficos_Mapas/Canal_Endemico_22_23_Provaveis.png", 
      plot = RS22_22_23_GRAF_CANAL_ENDEMICO_PROVAVEIS,
      width = 15.51,
      height = 8.51)
