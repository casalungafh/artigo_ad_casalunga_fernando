#==================================================
# Analise fatorial e Regressao Linear: 
# a importancia da aplicabilidade conjunta 
# de duas tecnicas de analise de dados
#==================================================
# UNIVERSIDADE FEDERAL DE PERNAMBUCO
#-------------------------------------------------
# FERNANDO CASALUNGA - FERNANDOCASALUNGA@GMAIL.COM
# RECIFE 2018 - DEZEMBRO
#==================================================
# Estatistica descritiva e modelo de regressao
# =================================================

# Acesso a biblioteca
if(require(stargazer) == F) install.packages('stargazer'); require(stargazer)
if(require(readr) == F) install.packages('readr'); require(readr)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(car) == F) install.packages('car'); require(car)
if(require(MASS) == F) install.packages('MASS'); require(MASS)
if(require(xtable) == F) install.packages('xtable'); require(xtable)
if(require(lmtest) == F) install.packages('lmtest'); require(lmtest)

# Importando banco de dados em formato .csv - modelo 1
library(readr)
analysis_data <- read_delim("analysis_data.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)

# Estatistica descritiva das variaveis independentes do modelo de regressao 1
summary(analysis_data$wvs_confgov)
summary(analysis_data$wef_fgo)
summary(analysis_data$diat_ati)
summary(analysis_data$fac1)
summary(analysis_data$fac2)

# Estatistica descritia da variavel dependente do modelo de regressao 1
summary(analysis_data$fh_cl)

# Modelo de regressao linear multivariado 1
modelo_1 <- lm(fh_cl ~ wvs_confgov +
                 wef_fgo + 
                 diat_ati + 
                 fac1 + 
                 fac2 , 
               data = analysis_data)

summary(modelo_1)


# Visualizacao do modelo 1
stargazer(modelo_1, type = "text", title = "Resultados do modelo 1", style = "ajps", p.auto=FALSE)


# Grafico para a representaco dos intervalos de confianca dos coeficientes etimados
par(mfrow=c(1,1))
betas <- coefficients(modelo_1) 
IC <-  confint(modelo_1, level=0.95) 

y.axis <- seq(from=1, to=length(betas))
plot(betas, y.axis, type="p", pch=19, xlab="Magnitude dos Coeficientes", ylab="", 
     axes=F, xlim=c(min(IC-.4), max(IC+.4)), ylim=c(min(y.axis-.2), max(y.axis+.2)), 
     cex=1,yaxs="i",xaxs="i")
segments(IC[,1], y.axis, IC[,2], y.axis)
axis(1, at=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), 
     labels=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), tick=T, cex.axis=1, 
     mgp=c(2,.7,0))
axis(2, at=y.axis, label=names(betas), las=1, tick=T, line=-.5, cex.axis=1, 
     mgp=c(2,.7,0))
abline(v=0, lty=2, col="red")


# Grafico de dispersao do modelo 1
ggplot(analysis_data, aes(x = fac1, y = fh_cl)) +
  geom_point()+ 
  geom_smooth(method='lm')


# Grafico de dispersao do modelo 1 com fac1 logaritimizado
ggplot(analysis_data, aes(x = log(fac1), y = fh_cl)) +
  geom_point()+ 
  geom_smooth(method='lm')


### Avaliando homocedasticidade ###
# Analise dos Residuos do modelo de regressao linear multivariado 1
analysis_data$Preditos <- predict(modelo_1)
analysis_data$Residuo <- analysis_data$fh_cl - analysis_data$Preditos

# Teste do valor medio do termo de erro igual a zero
summary(analysis_data$Residuo)

# Grafico de dispersao dos residuos padronizados do modelo 1
plot(analysis_data$Preditos, analysis_data$Residuo, pch=21, bg="red", col="red")
abline(0,0, lty = 2)

# plot residuos padronizados e valores preditos
spreadLevelPlot(modelo_1)

### Teste para autocorrelacao dos erros 
durbinWatsonTest(modelo_1)

# Correlacao entre a variavel dependente e o fator 1 - variavel independente construida
cor(analysis_data$fh_cl, analysis_data$fac1)

# Correlacao entre a variavel dependente e o fator 2 - variavel independente construida
cor(analysis_data$fh_cl, analysis_data$fac2)

### Teste para variancia do erro nao constante
ncvTest(modelo_1)
### Teste para Colinearidade  ###
vif(modelo_1) # variance inflation factors 
sqrt(vif(modelo_1)) > 3
sqrt(vif(modelo_1)) < 1

### Teste de Nao-linearidade ###
# componente somado aos residuos  
crPlots(modelo_1)

### Avaliando distribuição dos resíduos ###
# Histograma de dispersao dos residuos modelo 1
hist(analysis_data$Residuo, freq=FALSE, ylim=c(0,0.6), main="Distribuicao dos residuos padronizados 1")
xfit <- seq(min(analysis_data$Residuo),max(analysis_data$Residuo),length=40) 
yfit <- dnorm(xfit) 
lines(xfit, yfit)

#=============================================

# Importando banco de dados em formato .csv - modelo 2
library(readr)
analysis_data_2 <- read_delim("analysis_data_2.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

# Modelo de regressao linear multivariado 2
modelo_2 <- lm(fh_cl ~
                 wef_fgo + 
                 diat_ati + 
                 fac1 + 
                 fac2 , 
               data = analysis_data_2)

summary(modelo_2)

# Visualizacao dos modelos 1 e 2
stargazer(modelo_1, modelo_2, type = "text", title = "Resultados do modelo 1", style = "ajps", p.auto=FALSE)


# Grafico para a representaco dos intervalos de confianca dos coeficientes etimados
par(mfrow=c(1,1))
betas <- coefficients(modelo_2) 
IC <-  confint(modelo_2, level=0.95) 

y.axis <- seq(from=1, to=length(betas))
plot(betas, y.axis, type="p", pch=19, xlab="Magnitude dos Coeficientes", ylab="", 
     axes=F, xlim=c(min(IC-.4), max(IC+.4)), ylim=c(min(y.axis-.2), max(y.axis+.2)), 
     cex=1,yaxs="i",xaxs="i")
segments(IC[,1], y.axis, IC[,2], y.axis)
axis(1, at=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), 
     labels=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), tick=T, cex.axis=1, 
     mgp=c(2,.7,0))
axis(2, at=y.axis, label=names(betas), las=1, tick=T, line=-.5, cex.axis=1, 
     mgp=c(2,.7,0))
abline(v=0, lty=2, col="red")


# Grafico de dispersao do modelo 2
ggplot(analysis_data_2, aes(x = fac1, y = fh_cl)) +
  geom_point()+ 
  geom_smooth(method='lm')


# Grafico de dispersao do modelo 2 com fac1 logaritimizado
ggplot(analysis_data_2, aes(x = log(fac1), y = fh_cl)) +
  geom_point()+ 
  geom_smooth(method='lm')


### Avaliando homocedasticidade ###
# Analise dos Residuos do modelo de regressao linear multivariado 2
analysis_data_2$Preditos <- predict(modelo_2)
analysis_data_2$Residuo <- analysis_data_2$fh_cl - analysis_data_2$Preditos

# Teste do valor medio do termo de erro igual a zero
summary(analysis_data_2$Residuo)

# Grafico de dispersao dos residuos modelo 2
plot(analysis_data_2$Preditos, analysis_data_2$Residuo, pch=21, bg="red", col="red")
abline(0,0, lty = 2)

# plot residuos padronizados e valores preditos
spreadLevelPlot(modelo_2)

### Teste para Colinearidade 
vif(modelo_2) # variance inflation factors 
sqrt(vif(modelo_2)) > 3
sqrt(vif(modelo_2)) < 1

### Teste para autocorrelacao dos erros 
durbinWatsonTest(modelo_2)

# Correlacao entre a variavel dependente e o fator 1 - variavel independente construida
cor(analysis_data_2$fh_cl, analysis_data_2$fac1)

# Correlacao entre a variavel dependente e o fator 2 - variavel independente construida
cor(analysis_data_2$fh_cl, analysis_data_2$fac2)

### Teste para variancia do erro nao constante
ncvTest(modelo_2)

### Teste de Nao-linearidade 
# componente somado aos residuos  
crPlots(modelo_2)

# Histograma de dispersao dos residuos modelo 2
hist(analysis_data_2$Residuo, freq=FALSE, ylim=c(0,0.6), main="Distribuicao dos residuos padronizados 2")
xfit <- seq(min(analysis_data_2$Residuo),max(analysis_data_2$Residuo),length=40) 
yfit <- dnorm(xfit) 
lines(xfit, yfit)

#============================================

# Importando banco de dados em formato .csv - modelo 3
library(readr)
analysis_data_3 <- read_delim("analysis_data_3.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

# Estatistica descritiva das variaveis independentes do modelo de regressao 3
summary(analysis_data_3$wvs_confgov)
summary(analysis_data_3$wef_fgo)
summary(analysis_data_3$diat_ati)
summary(analysis_data_3$fac1)
summary(analysis_data_3$fac2)

# Estatistica descritia da variavel dependente do modelo de regressao 3
summary(analysis_data_3$eiu_cl)

# Modelo de regressao linear multivariado 3
modelo_3 <- lm(eiu_cl ~ wvs_confgov +
                 wef_fgo + 
                 diat_ati + 
                 fac1 + 
                 fac2 , 
               data = analysis_data_3)

summary(modelo_3)


# Visualizacao do modelo 3
stargazer(modelo_3, type = "text", title = "Resultados do modelo 3", style = "ajps", p.auto=FALSE)


# Grafico para a representaco dos intervalos de confianca dos coeficientes etimados
par(mfrow=c(1,1))
betas <- coefficients(modelo_3) 
IC <-  confint(modelo_3, level=0.95) 

y.axis <- seq(from=1, to=length(betas))
plot(betas, y.axis, type="p", pch=19, xlab="Magnitude dos Coeficientes", ylab="", 
     axes=F, xlim=c(min(IC-.4), max(IC+.4)), ylim=c(min(y.axis-.2), max(y.axis+.2)), 
     cex=1,yaxs="i",xaxs="i")
segments(IC[,1], y.axis, IC[,2], y.axis)
axis(1, at=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), 
     labels=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), tick=T, cex.axis=1, 
     mgp=c(2,.7,0))
axis(2, at=y.axis, label=names(betas), las=1, tick=T, line=-.5, cex.axis=1, 
     mgp=c(2,.7,0))
abline(v=0, lty=2, col="red")


# Grafico de dispersao do modelo 3
ggplot(analysis_data_3, aes(x = fac1, y = eiu_cl)) +
  geom_point()+ 
  geom_smooth(method='lm')

# Grafico de dispersao do modelo 3 com fac1 logaritimizado
ggplot(analysis_data_3, aes(x = log(fac1), y = eiu_cl)) +
  geom_point()+ 
  geom_smooth(method='lm')

### Avaliando homocedasticidade ###
# Analise dos Residuos do modelo de regressao linear multivariado 3
analysis_data_3$Preditos <- predict(modelo_3)
analysis_data_3$Residuo <- analysis_data_3$eiu_cl - analysis_data_3$Preditos

# Teste do valor medio do termo de erro igual a zero
summary(analysis_data_3$Residuo)

# Grafico de dispersao dos residuos modelo 3
plot(analysis_data_3$Preditos, analysis_data_3$Residuo, pch=21, bg="red", col="red")
abline(0,0, lty = 2)

# plot residuos padronizados e valores preditos
spreadLevelPlot(modelo_3)

### Teste para autocorrelacao dos erros 
durbinWatsonTest(modelo_3)

# Correlacao entre a variavel dependente e o fator 1 - variavel independente construida
cor(analysis_data_3$eiu_cl, analysis_data_3$fac1)

# Correlacao entre a variavel dependente e o fator 2 - variavel independente construida
cor(analysis_data_3$eiu_cl, analysis_data_3$fac2)

# Teste para variancia do erro nao constante
ncvTest(modelo_3)

### Teste para Colinearidade 
vif(modelo_3) # variance inflation factors 
sqrt(vif(modelo_3)) > 3
sqrt(vif(modelo_3)) < 1

#A# Teste de Nao-linearidade 
# componente somado aos residuos  
crPlots(modelo_3)

### Avaliando distribuição dos resíduos ###
# Histograma de dispersao dos residulos modelo 3
hist(analysis_data_3$Residuo, freq=FALSE, ylim=c(0,0.4), main="Distribuicao dos residuos padronizados 3")
xfit <- seq(min(analysis_data_3$Residuo),max(analysis_data_3$Residuo),length=40) 
yfit <- dnorm(xfit) 
lines(xfit, yfit)

#===========================================

# Importando banco de dados em formato .csv - modelo 4
library(readr)
analysis_data_4 <- read_delim("analysis_data_4.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

# Modelo de regressao linear multivariado 4 sem wvs_confgov
modelo_4 <- lm(eiu_cl ~
                 wef_fgo + 
                 diat_ati + 
                 fac1 + 
                 fac2 , 
               data = analysis_data_4)

summary(modelo_4)

# Visualizacao dos modelos 1 a 4
stargazer(modelo_1, modelo_2, modelo_3, modelo_4, 
          type = "text", title = "Resultados do modelo 4", style = "ajps", p.auto=FALSE)


# Grafico para a representaco dos intervalos de confianca dos coeficientes etimados
par(mfrow=c(1,1))
betas <- coefficients(modelo_4) 
IC <-  confint(modelo_4, level=0.95) 

y.axis <- seq(from=1, to=length(betas))
plot(betas, y.axis, type="p", pch=19, xlab="Magnitude dos Coeficientes", ylab="", 
     axes=F, xlim=c(min(IC-.4), max(IC+.4)), ylim=c(min(y.axis-.2), max(y.axis+.2)), 
     cex=1,yaxs="i",xaxs="i")
segments(IC[,1], y.axis, IC[,2], y.axis)
axis(1, at=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), 
     labels=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), tick=T, cex.axis=1, 
     mgp=c(2,.7,0))
axis(2, at=y.axis, label=names(betas), las=1, tick=T, line=-.5, cex.axis=1, 
     mgp=c(2,.7,0))
abline(v=0, lty=2, col="red")

# Grafico de dispersao do modelo 4
ggplot(analysis_data_4, aes(x = fac1, y = eiu_cl)) +
  geom_point()+ 
  geom_smooth(method='lm')

# Grafico de dispersao do modelo 4 com fac1 logaritimizado
ggplot(analysis_data_4, aes(x = log(fac1), y = eiu_cl)) +
  geom_point()+ 
  geom_smooth(method='lm')


### Avaliando homocedasticidade ###
# Analise dos Residuos do modelo de regressao linear multivariado 4
analysis_data_4$Preditos <- predict(modelo_4)
analysis_data_4$Residuo <- analysis_data_4$eiu_cl - analysis_data_4$Preditos

# Teste do valor medio do termo de erro igual a zero
summary(analysis_data_4$Residuo)

# Grafico de dispersao dos residuos modelo 4
plot(analysis_data_4$Preditos, analysis_data_4$Residuo, pch=21, bg="red", col="red")
abline(0,0, lty = 2)

# plot residuos padronizados e valores preditos
spreadLevelPlot(modelo_4)

### Teste para autocorrelacao dos erros 
durbinWatsonTest(modelo_4)

# Correlacao entre a variavel dependente e o fator 1 - variavel independente construida
cor(analysis_data_4$eiu_cl, analysis_data_4$fac1)

# Correlacao entre a variavel dependente e o fator 2 - variavel independente construida
cor(analysis_data_4$eiu_cl, analysis_data_4$fac2)

# Teste para variancia do erro nao constante
ncvTest(modelo_4)

### Teste para Colinearidade 
vif(modelo_4) # variance inflation factors 
sqrt(vif(modelo_4)) > 3
sqrt(vif(modelo_4)) < 1

#Avaliando Nao-linearidade 
# componente somado aos residuos  
crPlots(modelo_4)

#Avaliando Nao-linearidade 
# componente somado aos residuos  
crPlots(modelo_4)

### Avaliando distribuição dos resíduos ###
# Histograma de dispersao dos residulos modelo 4
hist(analysis_data_4$Residuo, freq=FALSE, ylim=c(0,0.4), main="Distribuicao dos residuos padronizados 4")
xfit <- seq(min(analysis_data_4$Residuo),max(analysis_data_4$Residuo),length=40) 
yfit <- dnorm(xfit) 
lines(xfit, yfit)






