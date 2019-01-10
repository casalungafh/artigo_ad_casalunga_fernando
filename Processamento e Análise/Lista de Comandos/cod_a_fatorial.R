#=====================================
# AnAlise fatorial e RegressAo Linear: 
# a importancia da aplicabilidade conjunta 
# de duas tecnicas de analise de dados
#====================================
# UNIVERSIDADE FEDERAL DE PERNAMBUCO
#-----------------------------------
# FERNANDO CASALUNGA - FERNANDOCASALUNGA@GMAIL.COM
# RECIFE 2018 - DEZEMBRO
#====================================
# Analise de componentes principais e fatorial #
#====================================

# Acesso a biblioteca #
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(foreign) == F) install.packages('foreign'); require(foreign)
if(require(FactoMineR) == F) install.packages('FactoMiner'); require(FactoMineR)
if(require(stats) == F) install.packages('stats'); require(stats)
if(require(reshape2) == F) install.packages('reshape2'); require(reshape2)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)


# Carrega base de dados QoG.csv #
data <-read.csv("qog_bas_cs_jan16.csv")


# MODELOS 1 e 2 utilizam a variavel dependente categorica - fh_cl para mensurar as liberdades civis  (1 mais livre - 7 menos livre)
# Modelo 1 - com variavel wvs_confgov
# Definindo as variaveis para compor os fatores
analysis_data <- data[c("fh_cl", "wvs_confgov", "wef_fgo", "diat_ati", "wbgi_cce", "wef_ebf", "wef_pr", "wbgi_pse", 
                        "eiu_fog", "fe_cultdiv", "fe_etfra", "al_language")]

# Retira os Dados Omissos da Base da Dados
analysis_data <- na.omit(analysis_data)


# Modelo 2 - sem variavel wvs_confgov
# Definindo as variaveis para compor os fatores
analysis_data_2 <- data[c("fh_cl", "wef_fgo", "diat_ati", "wbgi_cce", "wef_ebf", "wef_pr", "wbgi_pse", 
                          "eiu_fog", "fe_cultdiv", "fe_etfra", "al_language")]

# Retira os Dados Omissos da Base da Dados
analysis_data_2 <- na.omit(analysis_data_2)


# Gerar matriz de correlacao - modelo 1
cormat <- cor(analysis_data[, -c(1:4)])
melted_cormat <- melt(cormat)

# Grafico da matriz de correlacao - modelo 1
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()


# Gerar matriz de correlacao - modelo 2
cormat <- cor(analysis_data_2[, -c(1:3)])
melted_cormat <- melt(cormat)

# Grafico da matriz de correlacao - modelo 2
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()

#=========================
# MODELOS 3 e 4 utilizam a variavel dependente ordinal - eiu_cl para mensurar as liberdades civis  (1 menos livre - 10 mais livre)
# Modelo 3 - com variavel wvs_confgov
# Definindo as variaveis para compor os fatores
analysis_data_3 <- data[c("eiu_cl", "wvs_confgov", "wef_fgo", "diat_ati", 
                          "wbgi_cce", "wef_ebf", "wef_pr", "wbgi_pse", 
                          "eiu_fog", "fe_cultdiv", "fe_etfra", "al_language")]

# Retira os Dados Omissos da Base da Dados
analysis_data_3 <- na.omit(analysis_data_3)


# Modelo 4 - sem variavel wvs_confgov
# Definindo as variaveis para compor os fatores
analysis_data_4 <- data[c("eiu_cl", "wef_fgo", "diat_ati", "wbgi_cce", "wef_ebf", "wef_pr", "wbgi_pse", 
                          "eiu_fog", "fe_cultdiv", "fe_etfra", "al_language")]

# Retira os Dados Omissos da Base da Dados
analysis_data_4 <- na.omit(analysis_data_4)


# Gerar matriz de correlacao - modelo 3
cormat <- cor(analysis_data_3[, -c(1:4)])
melted_cormat <- melt(cormat)

# Grafico da matriz de correlacao - modelo 3
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()


# Gerar matriz de correlacao - modelo 4
cormat <- cor(analysis_data_4[, -c(1:3)])
melted_cormat <- melt(cormat)

# Grafico da matriz de correlaco - modelo 4
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()


#=========================================
# Analise Fatorial (PCA)
#=======================================
# Modelo 1
pca1 <- princomp(analysis_data[, -c(1:4)], scores=TRUE, cor=TRUE)
# Sumario do PCA
summary(pca1)

# Carregamento dos componentes principais
loadings(pca1)

# Verificando Quantos fatores devemos utilizar
plot(pca1) # cotovelo em 2 fatores

# Selecionando os 2 fatores - modelo 1
analysis_data$fac1 <- pca1$scores[,1]
analysis_data$fac2 <- pca1$scores[,2]

# Grafico Bidimensional dos Fatores
plot(analysis_data$fac1, analysis_data$fac2)

#Scree plot of eigenvalues
screeplot(pca1, type="line", main="Scree Plot")
abline(1,0, lty=2)

# Scores dos primeiros components
pca1$scores[1 : 10]

# Rotacao varimax(pca1$rotation) 
# Analise fatorial 
fatorial_varimax <- factanal(analysis_data[, -c(1:4,13:14)],factors=2, rotation="varimax", scores="regression")

# Descritivo dos fatores 1 e 2 
summary(analysis_data$fac1)
summary(analysis_data$fac2)

# Biplot do valor das variaveis
biplot(pca1)

#=======================
# Modelo 2 
pca2 <- princomp(analysis_data_2[, -c(1:3)], scores=TRUE, cor=TRUE)
# Sumario do PCA
summary(pca2)

# Carregamento dos componentes principais
loadings(pca2)

# Verificando Quantos fatores devemos utilizar
plot(pca2) # cotovelo em 2 fatores

# Selecionando os 2 fatores - modelo 1
analysis_data_2$fac1 <- pca2$scores[,1]
analysis_data_2$fac2 <- pca2$scores[,2]

# Grafico Bidimensional dos Fatores
plot(analysis_data_2$fac1, analysis_data_2$fac2)

#Scree plot of eigenvalues
screeplot(pca2, type="line", main="Scree Plot")
abline(1,0, lty=2)

# Scores dos primeiros components
pca2$scores[1 : 10]

# Rotacao varimax(pca1$rotation) 
# Analise fatorial 
fatorial_varimax_2 <- factanal(analysis_data_2[, -c(1:3,12:13)],factors=2, rotation="varimax", scores="regression")

# Biplot do valor das variaveis
biplot(pca2)

#=========================================
# Analise Fatorial (PCA)
#=======================================
# Modelo 3
pca3 <- princomp(analysis_data_3[, -c(1:4)], scores=TRUE, cor=TRUE)
# Sumario do PCA
summary(pca3)

# Carregamento dos componentes principais
loadings(pca3)

# Verificando Quantos fatores devemos utilizar
plot(pca3) # cotovelo em 2 fatores

# Selecionando os 2 fatores - modelo 3
analysis_data_3$fac1 <- pca3$scores[,1]
analysis_data_3$fac2 <- pca3$scores[,2]

# Grafico Bidimensional dos Fatores
plot(analysis_data_3$fac1, analysis_data_3$fac2)

#Scree plot of eigenvalues
screeplot(pca3, type="line", main="Scree Plot")
abline(1,0, lty=2)

# Scores dos primeiros components
pca3$scores[1 : 10]

# Rotacao varimax(pca1$rotation) 
# Analise fatorial 
fatorial_varimax <- factanal(analysis_data_3[, -c(1:4,13:14)],factors=2, rotation="varimax", scores="regression")

# Biplot do valor das variaveis
biplot(pca3)

#=======================
# Modelo 4
pca4 <- princomp(analysis_data_4[, -c(1:3)], scores=TRUE, cor=TRUE)
# Sumario do PCA
summary(pca4)

# Carregamento dos componentes principais
loadings(pca4)

# Verificando Quantos fatores devemos utilizar
plot(pca4) # cotovelo em 2 fatores

# Selecionando os 2 fatores - modelo 4
analysis_data_4$fac1 <- pca4$scores[,1]
analysis_data_4$fac2 <- pca4$scores[,2]

# Grafico Bidimensional dos Fatores
plot(analysis_data_4$fac1, analysis_data_4$fac2)

#Scree plot of eigenvalues
screeplot(pca4, type="line", main="Scree Plot")
abline(1,0, lty=2)

# Scores dos primeiros components
pca4$scores[1 : 10]

# Rotacao varimax(pca1$rotation) 
# Analise fatorial 
fatorial_varimax_2 <- factanal(analysis_data_4[, -c(1:3,12:13)],factors=2, rotation="varimax", scores="regression")

# Biplot do valor das variaveis
biplot(pca4)

#=============================
# Salvar banco de dados dos modelos 1 a 4
# modelo 1
write.table(analysis_data, "analysis_data.csv", sep = ";", row.names = F)

# modelo 2
write.table(analysis_data_2, "analysis_data_2.csv", sep = ";", row.names = F)

# modelo 3
write.table(analysis_data_3, "analysis_data_3.csv", sep = ";", row.names = F)

# modelo 4
write.table(analysis_data_4, "analysis_data_4.csv", sep = ";", row.names = F)