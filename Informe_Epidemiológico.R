#####Definindo diretório de trabalho, caso tenha que trabalhar em Windows, acertar o diretório###

setwd("/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/")


library(foreign)
library (dplyr)
library (googlesheets4)

####Importando as tabelas da Tabulação Primária para construção das tabelas base do Informe Epidemiológico ###

BASE_IBGE<-read.table(file="/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/CSV/Planilha_Base_IBGE.csv", 
                      header=TRUE, 
                      sep=",")


DENGON2022 <- read.dbf(file = "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Arboviroses/DBF/DENGON2022.dbf", 
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, SEM_NOT, DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, DT_INTERNA, DT_OBITO, DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, ALRM_VOM, ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST, GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, PETEQUIAS, HEMATURA, SANGRAM)

DENGON2023 <- read.dbf(file = "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Arboviroses/DBF/DENGON2023.dbf", 
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, SEM_NOT, DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, DT_INTERNA, DT_OBITO, DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, ALRM_VOM, ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST, GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, PETEQUIAS, HEMATURA, SANGRAM)

RS22_Serie_Historica_Base <- read.csv(file = "RS22_Serie_Historica_Base.csv",
                                 header = TRUE,
                                 sep = ",")

RS22_CE_Notificados_Base <- read.csv(file = "RS22_CE_Notificados_Base.csv",
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

colnames (RS22_22_23_SE_Notificados)[2:25] <- c(30:53)

colnames (RS22_22_23_SE_Notificados)[26:54] <- c(1:29)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 2] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i,
                                                                                             SEM_PRI ==202230)%>%
                                                                                      count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 3] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             SEM_PRI ==202231) %>% 
                                                                                      count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 4] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                      filter(ID_MN_RESI == i,
                                                                                             SEM_PRI ==202232) %>% 
                                                                                      count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i),5] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                     filter(ID_MN_RESI == i,
                                                                                            SEM_PRI ==202233) %>% 
                                                                                     count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 6] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                      filter(ID_MN_RESI == i,
                                                                                             SEM_PRI ==202234) %>% 
                                                                                      count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 7] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             SEM_PRI ==202235) %>%
                                                                                      count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 8] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             SEM_PRI ==202236) %>% 
                                                                                      count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 9] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             SEM_PRI ==202237) %>% 
                                                                                      count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 10] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202238) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 11] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202239) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 12] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202240) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 13] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202241) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 14] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202242) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 15] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202243) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 16] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202244) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 17] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202245) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 18] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202246) %>%                                                                                        count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 19] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202247) %>%                                                                                        count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 20] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202248) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i),  21] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                        filter(ID_MN_RESI == i, 
                                                                                               SEM_PRI ==202249) %>%
                                                                                        count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 22] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202250) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 23] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202251) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 24] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202252) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 25] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202253) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 26] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202301) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 27] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202302) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 28] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202303) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 29] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202304) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 30] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202305) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 31] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202306) %>% 
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 32] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202307) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 33] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202308) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 34] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202309) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 35] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202310) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 36] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202311) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 37] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202312) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 38] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202313) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 39] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202314) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 40] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202315) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 41] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202316) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 42] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202317) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 43] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202318) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 44] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202319) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 45] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202320) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 46] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202321) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 47] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202322) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 48] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202323) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 49] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202324) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 50] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202325) %>% 
                                                                                       count()
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 51] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202326) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 52] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202327) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 53] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              SEM_PRI ==202328) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Notificados[which(RS22_22_23_SE_Notificados == i), 54] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              SEM_PRI ==202329) %>%
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

colnames (RS22_22_23_SE_Confirmados)[2:25] <- c(30:53)

colnames (RS22_22_23_SE_Confirmados)[26:54] <- c(1:29)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 2] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i,
                                                                                             CLASSI_FIN == 10 
                                                                                             | 
                                                                                               CLASSI_FIN == 11 
                                                                                             |
                                                                                               CLASSI_FIN == 12,
                                                                                             SEM_PRI ==202230)%>%
                                                                                      count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 3] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             CLASSI_FIN == 10 
                                                                                             | 
                                                                                               CLASSI_FIN == 11 
                                                                                             |
                                                                                               CLASSI_FIN == 12,
                                                                                             SEM_PRI ==202231) %>% 
                                                                                      count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 4] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                      filter(ID_MN_RESI == i,
                                                                                             CLASSI_FIN == 10 
                                                                                             | 
                                                                                               CLASSI_FIN == 11 
                                                                                             |
                                                                                               CLASSI_FIN == 12,
                                                                                             SEM_PRI ==202232) %>% 
                                                                                      count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i),5] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                     filter(ID_MN_RESI == i,
                                                                                            CLASSI_FIN == 10 
                                                                                            | 
                                                                                              CLASSI_FIN == 11 
                                                                                            |
                                                                                              CLASSI_FIN == 12,
                                                                                            SEM_PRI ==202233) %>% 
                                                                                     count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 6] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                      filter(ID_MN_RESI == i,
                                                                                             CLASSI_FIN == 10 
                                                                                             | 
                                                                                               CLASSI_FIN == 11 
                                                                                             |
                                                                                               CLASSI_FIN == 12,
                                                                                             SEM_PRI ==202234) %>% 
                                                                                      count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 7] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             CLASSI_FIN == 10 
                                                                                             | 
                                                                                               CLASSI_FIN == 11 
                                                                                             |
                                                                                               CLASSI_FIN == 12,
                                                                                             SEM_PRI ==202235) %>%
                                                                                      count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 8] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             CLASSI_FIN == 10 
                                                                                             | 
                                                                                               CLASSI_FIN == 11 
                                                                                             |
                                                                                               CLASSI_FIN == 12,
                                                                                             SEM_PRI ==202236) %>% 
                                                                                      count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 9] <- as.integer(RS22_22_23_SINAN %>%
                                                                                      filter(ID_MN_RESI == i, 
                                                                                             CLASSI_FIN == 10 
                                                                                             | 
                                                                                               CLASSI_FIN == 11 
                                                                                             |
                                                                                               CLASSI_FIN == 12,
                                                                                             SEM_PRI ==202237) %>% 
                                                                                      count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 10] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202238) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 11] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202239) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 12] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202240) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 13] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202241) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 14] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202242) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 15] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202243) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 16] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202244) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 17] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202245) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 18] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202246) %>%                                                                                        count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 19] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202247) %>%                                                                                        count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 20] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202248) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i),  21] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                        filter(ID_MN_RESI == i,
                                                                                               CLASSI_FIN == 10 
                                                                                               | 
                                                                                                 CLASSI_FIN == 11 
                                                                                               |
                                                                                                 CLASSI_FIN == 12,
                                                                                               SEM_PRI ==202249) %>%
                                                                                        count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 22] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202250) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 23] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202251) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 24] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202252) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 25] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202253) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 26] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202301) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 27] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202302) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 28] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202303) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 29] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202304) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 30] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202305) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 31] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202306) %>% 
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 32] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202307) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 33] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202308) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 34] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202309) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 35] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202310) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 36] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202311) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 37] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202312) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 38] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202313) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 39] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202314) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 40] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202315) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 41] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202316) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 42] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202317) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 43] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202318) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 44] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202319) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 45] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202320) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 46] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202321) %>%
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 47] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202322) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 48] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202323) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 49] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202324) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 50] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202325) %>% 
                                                                                       count()
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 51] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202326) %>% 
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 52] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202327) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 53] <- as.integer(RS22_22_23_SINAN %>%
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202328) %>%
                                                                                       count() 
  )
  
  RS22_22_23_SE_Confirmados[which(RS22_22_23_SE_Confirmados == i), 54] <- as.integer(RS22_22_23_SINAN %>% 
                                                                                       filter(ID_MN_RESI == i,
                                                                                              CLASSI_FIN == 10 
                                                                                              | 
                                                                                                CLASSI_FIN == 11 
                                                                                              |
                                                                                                CLASSI_FIN == 12,
                                                                                              SEM_PRI ==202329) %>%
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

######################################################################
####Incluindo coluna de CASOS EM INVESTIGAÇÂO na tabela RS22_GERAL####
#### Esta coluna só tem sentido no período sazonal atual #############
#### Casos em investigação de períodos anteriores são ################
#### INCONCLUSIVOS####################################################
######################################################################

RS22_22_23_GERAL$Em_Investigacao <- as.integer(RS22_22_23_GERAL$Notificados) - as.integer(RS22_22_23_GERAL$Dengue + RS22_22_23_GERAL$D_S_A + RS22_22_23_GERAL$Dengue_Grave + RS22_22_23_GERAL$Descartados)

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

RS22_22_23_SINAIS <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == 22), 1])

RS22_22_23_SINAIS$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == 22), 3]

RS22_22_23_SINAIS$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

RS22_22_23_SINAIS$Febre <- NA

RS22_22_23_SINAIS$Cefaleia <- NA

RS22_22_23_SINAIS$Mialgia <- NA

RS22_22_23_SINAIS$Exantema <- NA

RS22_22_23_SINAIS$Vomitos <- NA

RS22_22_23_SINAIS$Nausea <- NA

RS22_22_23_SINAIS$Dor_nas_Costas <- NA

RS22_22_23_SINAIS$Conjuntivite <- NA

RS22_22_23_SINAIS$Artrite  <- NA

RS22_22_23_SINAIS$Artralgia <- NA

RS22_22_23_SINAIS$Petequias <- NA

RS22_22_23_SINAIS$Leucopenia <- NA

RS22_22_23_SINAIS$Dor_Retroorbital <- NA

RS22_22_23_SINAIS$Prova_do_Laco_Positiva <- NA

###Elaborando for loop para sinais e sintomas.###

for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  RS22_22_23_SINAIS[which(RS22_22_23_SINAIS$COD_IBGE == i), 4] <- as.integer(RS22_22_23_SINAN %>%
                                                                               filter(ID_MN_RESI == i,
                                                                                      FEBRE == 1) %>%
                                                                               count()
  )
  
  
  RS22_22_23_SINAIS[which(RS22_22_23_SINAIS$COD_IBGE == i), 5] <- as.integer(RS22_22_23_SINAN %>%
                                                                               filter(ID_MN_RESI == i,
                                                                                      CEFALEIA == 1) %>%
                                                                               count()
  )
  
  RS22_22_23_SINAIS[which(RS22_22_23_SINAIS$COD_IBGE == i), 6] <- as.integer(RS22_22_23_SINAN %>%
                                                                               filter(ID_MN_RESI == i,
                                                                                      MIALGIA == 1) %>%
                                                                               count()
  )
  
  
  RS22_22_23_SINAIS[which(RS22_22_23_SINAIS$COD_IBGE == i), 7] <- as.integer(RS22_22_23_SINAN %>%
                                                                               filter(ID_MN_RESI == i,
                                                                                      EXANTEMA == 1) %>%
                                                                               count()
  )
  
  RS22_22_23_SINAIS[which(RS22_22_23_SINAIS$COD_IBGE == i), 8] <- as.integer(RS22_22_23_SINAN %>%
                                                                               filter(ID_MN_RESI == i,
                                                                                      VOMITO == 1) %>%
                                                                               count()
  )
  
  RS22_22_23_SINAIS[which(RS22_22_23_SINAIS$COD_IBGE == i), 9]<- as.integer(RS22_22_23_SINAN %>%
                                                                              filter(ID_MN_RESI == i,
                                                                                     NAUSEA == 1) %>%
                                                                              count()
  )
  
  RS22_22_23_SINAIS[which(RS22_22_23_SINAIS$COD_IBGE == i), 10]<- as.integer(RS22_22_23_SINAN %>%
                                                                               filter(ID_MN_RESI == i,
                                                                                      DOR_COSTAS == 1) %>%
                                                                               count()
  )
  
  RS22_22_23_SINAIS[which(RS22_22_23_SINAIS$COD_IBGE == i), 11] <- as.integer(RS22_22_23_SINAN %>%
                                                                                filter(ID_MN_RESI == i,
                                                                                       CONJUNTVIT == 1) %>%
                                                                                count()
  )
  
  RS22_22_23_SINAIS[which(RS22_22_23_SINAIS$COD_IBGE == i), 12] <- as.integer(RS22_22_23_SINAN %>%
                                                                                filter(ID_MN_RESI == i,
                                                                                       ARTRITE == 1) %>%
                                                                                count()
  )
  
  RS22_22_23_SINAIS[which(RS22_22_23_SINAIS$COD_IBGE == i), 13] <- as.integer(RS22_22_23_SINAN %>%
                                                                                filter(ID_MN_RESI == i,
                                                                                       ARTRALGIA == 1) %>%
                                                                                count()
  )
  
  RS22_22_23_SINAIS[which(RS22_22_23_SINAIS$COD_IBGE == i), 14] <- as.integer(RS22_22_23_SINAN %>%
                                                                                filter(ID_MN_RESI == i,
                                                                                       PETEQUIA_N == 1) %>%
                                                                                count()
  )
  
  RS22_22_23_SINAIS[which(RS22_22_23_SINAIS$COD_IBGE == i), 15] <- as.integer(RS22_22_23_SINAN %>%
                                                                                filter(ID_MN_RESI == i,
                                                                                       LEUCOPENIA == 1) %>%
                                                                                count()
  )
  
  RS22_22_23_SINAIS[which(RS22_22_23_SINAIS$COD_IBGE == i), 16] <- as.integer(RS22_22_23_SINAN %>%
                                                                                filter(ID_MN_RESI == i,
                                                                                       DOR_RETRO == 1) %>%
                                                                                count()
  )
  
  RS22_22_23_SINAIS[which(RS22_22_23_SINAIS$COD_IBGE == i), 17] <- as.integer(RS22_22_23_SINAN %>%
                                                                                filter(ID_MN_RESI == i,
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
                                                  label = c("CLÍNICO", "CLÍNICO-EPIDEMIOLÓGICO", "EM INVESTIGAÇÃO"), 
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

RS22_22_23_SINAN_DECODIFICADO_AUX <- data.frame(COD = RS22_22_23_SINAN_DECODIFICADO[,10], 
                                                Municipio = NA)

for (i in RS22_22_23_SINAN_DECODIFICADO[,10]){
  RS22_22_23_SINAN_DECODIFICADO_AUX[which(RS22_22_23_SINAN_DECODIFICADO_AUX$COD == i), 2] <- BASE_IBGE[which(BASE_IBGE$Código_IBGE == i),3]
  
}

RS22_22_23_SINAN_DECODIFICADO[,10] <- RS22_22_23_SINAN_DECODIFICADO_AUX[, 2]

####Município de Residência

RS22_22_23_SINAN_DECODIFICADO_AUX <- data.frame(COD = RS22_22_23_SINAN_DECODIFICADO[,18], 
                                                Municipio = NA)

for (i in RS22_22_23_SINAN_DECODIFICADO[,18]){
  RS22_22_23_SINAN_DECODIFICADO_AUX[which(RS22_22_23_SINAN_DECODIFICADO_AUX$COD == i), 2] <- BASE_IBGE[which(BASE_IBGE$Código_IBGE == i),3]
  
}

RS22_22_23_SINAN_DECODIFICADO[,18] <- RS22_22_23_SINAN_DECODIFICADO_AUX[, 2]

rm (RS22_22_23_SINAN_DECODIFICADO_AUX)

colnames(RS22_22_23_SINAN_DECODIFICADO)<- c("RS", "SINAN", "Agravo", "Data_Notificacao", "ANO", "SE_Notificacao", "Data_Primeiros_Sintomas", "SE_Primeiros_Sintomas", "UF_Notificacao", "Municipio", "Nome", "Data_Nascimento", "Idade", "Sexo", "Gestante", "Escolaridade", "Nome_Mae", "Municipio_Residencia", "UF_Residencia", "RS_Residencia", "Logradouro", "Numero", "Bairro", "CEP", "Zona", "Data_Digitacao", "Data_Investigacao", "Febre", "Mialgia", "Cefaleia", "Exantema", "Vomito", "Nausea", "Dor_nas_Costas", "Conjuntivite", "Artrite", "Artralgia_Intensa", "Petequias", "Leucopenia", "Prova_do_Laco_Positiva", "Dor_retroorbital", "Diabetes", "Doenca_Hematologica", "Hepatopatia", "Doenca_Renal", "Hipertensao", "Doenca_Acido_Peptica", "Doenca_Auto_Imune", "Data_Sorologia", "Resultado_Sorologia", "Data_PCR", "Resultado_PCR", "Sorotipo", "Classificacao_Final", "Critério_Encerramento", "Autoctone", "UF_Infeccao", "Municipio_Infeccao", "Bairro_Infeccao", "Evolucao", "Hospitalizado", "Data_Internamento", "Data_Obito", "Data_Encerramento", "Data_SNA", "Letargia", "Hepatomegalia", "Acumulo_Liquidos", "Hipotensao_Lipotimia", "Queda_Abrupta_Plaquetas", "Vomitos_Persistentes", "Hemorragias", "Aumento_Hematocrito", "Dor_Abdominal", "Data_Dengue_Grave", "Pulso_Debil", "PA_Convergente", "TPC", "Acumulo_Liq_Insuficiencia_Resp", "Taquicardia", "Extremidades_Frias", "Hipotensao", "Hematemese", "Melena", "Metrorragia", "Sangramento_SNC", "Aumento_AST_ALT", "Miocardite", "Alteracao_Consciencia", "Outros_Orgaos", "Manifestacao_Hemorragica", "Epistaxe", "Gengivorragia", "Metrorragia", "Petequias", "Hematuria", "Sangramento" )


####################################################################################################################
################Trabalhando a tabela base do Canal Endêmico#########################################################
####################################################################################################################

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

AUX <- RS22_CE_Notificados$Media + (2 * RS22_CE_Notificados$Desvio_Padrao)

RS22_CE_Notificados$Lim_Superior <- AUX

RS22_CE_Notificados[,17] <- rownames(RS22_CE_Notificados)

RS22_CE_Notificados <- RS22_CE_Notificados[, c(17, 1:16)]

RS22_CE_Notificados[,1] <- c(30:53, 1:29)

colnames(RS22_CE_Notificados)[1] <- "Semana_Epidemiológica"

rownames(RS22_CE_Notificados) <- c(1:nrow(RS22_CE_Notificados))

rm(AUX, AUX2, RS22_CE_Notificados_Base)

write.csv (RS22_CE_Notificados, "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_CE_Notificados.csv", row.names = FALSE)

#####Planilhas Google Sheets. Realizando o download das planilhas do google sheets e fazendo o upload da planilha de notificações######

RS22_22_23_CICLOS_LOCALIDADES <- read_sheet("https://docs.google.com/spreadsheets/d/18hJMQnlFcRHeqNbtJObD0UCx8L-lOcJpOTYkn849Fcs/edit#gid=764914932")

###Substituindo NA por 200 na planilha. O QGIS não está reconhecendo NA e as variáveis ficam como String no SIG.###

RS22_22_23_CICLOS_LOCALIDADES[is.na(RS22_22_23_CICLOS_LOCALIDADES)] <- 200

write.csv(RS22_22_23_CICLOS_LOCALIDADES, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_CICLOS_LOCALIDADES.csv",
          row.names = FALSE)

RS22_22_23_CICLOS_MUNICIPIOS <- read_sheet("https://docs.google.com/spreadsheets/d/1P1fC2Z3R8yyaF2_P7wUcDEtwKqfq8MWcf2R5vmmw-kM/edit#gid=1734395963")

RS22_22_23_CICLOS_MUNICIPIOS[is.na(RS22_22_23_CICLOS_MUNICIPIOS)] <- 200

write.csv(RS22_22_23_CICLOS_MUNICIPIOS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_CICLOS_MUNICIPIOS.csv",
          row.names = FALSE)

RS22_22_23_RG_MUNICIPIOS <- read_sheet("https://docs.google.com/spreadsheets/d/1QILHWnVa1m2Lr4qqf02VejbCrPQaqkr1w51QNbxkaXk/edit#gid=1585473376")

RS22_22_23_RG_MUNICIPIOS <- as.data.frame(lapply(RS22_22_23_RG_MUNICIPIOS, 
                                                 unlist))

write.csv(RS22_22_23_RG_MUNICIPIOS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_RG_MUNICIPIOS.csv",
          row.names = FALSE)

RS22_22_23_RG_LOCALIDADES <- read_sheet("https://docs.google.com/spreadsheets/d/1js80T20EU2FvfqTLsjutPI7Zg93AaOhhbl8-wBD18FA/edit#gid=877642872")

write.csv(RS22_22_23_RG_LOCALIDADES, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_RG_LOCALIDADES.csv",
          row.names = FALSE)

RS22_22_23_PE <- read_sheet("https://docs.google.com/spreadsheets/d/1fW7nGY_h17JqeuV37rnk71sZvxyB1_D0rynci5m-dI8/edit#gid=863361484")

RS22_22_23_PE <- as.data.frame(lapply(RS22_22_23_PE, 
                                      unlist))

write.csv(RS22_22_23_PE, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_PE.csv",
          row.names = FALSE)

RS22_22_23_ASSISTENCIA <- read_sheet("https://docs.google.com/spreadsheets/d/1Ov9yeYwy-Xu7fkpxnxJG_AD_crHClGuk8OVFRi9FlPo/edit#gid=497857259")

write.csv(RS22_22_23_ASSISTENCIA, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_ASSISTENCIA.csv",
          row.names = FALSE)

####Gravando os arquivos CSV#####

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

write.csv(RS22_22_23_SINAIS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_SINAIS.csv",
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

write.csv(RS22_22_23_DOENCAS_PRE_EXISTENTES, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Tabulacoes_Primarias/RS22_22_23_DOENCAS_PRE_EXISTENTES.csv",
          row.names = FALSE)
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
######Códigos a serem trabalhados####


library ("ggplot2")
RS22_Serie_Historica$Notificados <- as.numeric(RS22_Serie_Historica$Notificados)
ggplot (RS22_Serie_Historica, aes(x = Periodo, y = Notificados)) + geom_bar(stat = "identity")

