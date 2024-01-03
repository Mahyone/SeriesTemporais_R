library(tidyverse)
library(readxl)
library(fpp2)
library(BETS)
library(urca)
library(TSA)
library(forecast)
library(tmtest)
library(normtest)
library(xlsx)
library(fpp2)
library(TSstudio)
library(knitr)
library(readxl)
library(bayesforecast)
library(NlinTS)
library(ggplot2)

fert <- read_excel("Fertilizantes.xlsx")
View(fert)

head(fert, n=2)

fert_ts <- ts(fert$fertilizantes, frequency = 12, start = c(1998, 1))

fert_ts

class(fert_ts)

autoplot(fert_ts)


janela <- window(fert_ts, start=c(2007,1))

autoplot(janela)


###modelo de Holt com h = 1 anos###


seasonplot(janela, 4, col=rainbow(4),
           year.labels=TRUE, 
           main="Fertilizantes")

boxplot(janela ~ cycle(janela),col="orange",xlab="Ano", 
        ylab="Quantidade")
title("Fertilizantes")

ggsubseriesplot(janela)      


hw.a <- hw(janela, seasonal = "additive", h= 1, level = 95) #previsao

hw.a # previsão

hw.a$fitted # ajuste do modelo à série

hw.a$model

hw.m <- hw(janela, seasonal = "multiplicative", h= 1, level = 95 ) #previsao

hw.m
hw.m$fitted
hw.m$model

autoplot(janela, series = "Série real")+
  autolayer(hw.a$fitted, series = "HW Aditivo")+
  autolayer(hw.m$fitted, series = "HW Multiplicativo")+
  xlab("Anual")+
  ylab("Quantidade")+
  ggtitle("Fertilzantes")      


accuracy(hw.a)
accuracy(hw.m)


hw.a$fitted
janela

autoplot(janela, series = "Serie Original", lwd=1.2)+
  autolayer(hw.a$fitted, series = "Modelo", lwd=1.2)+
  autolayer(hw.a, series = "Predição", lwd=1.2, showgap = FALSE)+
  labs(x= "Anos", y= "Toneladas", title = "Fertilzantes")


###Sarima###

library(gridExtra)

# verificando as autocorrela??es seriais

acf  <- ggAcf(janela, lag.max = 60)
pacf <- ggPacf(janela,lag.max= 60)

grid.arrange(acf, pacf, nrow =2)

ts_cor(janela)



# Efetuando os testes de estacionariedade

library(tseries)

############################
# p-valor baixo rejeita Ho.#  Regra de Ouro
############################

# TESTE ADF

# Ho. A s?rie n?o ? estacionaria
# H1: A serie ? estacionaria

adf.test(janela)


# Pelo KPSS

# Ho. A s?rie ? estacionaria
# H1: A serie n?o ? estacionaria

kpss.test(janela)

# PHILLP-PERRON
# Ho. A série n?o ? estacionaria
# H1: A serie é estacionaria

pp.test(janela)

# O TESTE KPSS EST? DIVERGENTE DO ADF E DO PP

# Eefetuando a Diferencia??o 



# A fun??o  diff efetua a primeira diferen?a no lag 1. 




a1 <- autoplot(diff(janela, lag = 1, differences = 1))+
  ylab("diff(janela)")+
  ggtitle("Fertilizantes")

a2 <- ggAcf(diff(janela, lag = 1, differences = 1),
            lag.max = 36)+
  ggtitle("Fetilizantes")

grid.arrange(a1, a2, nrow =2)

adf.test(diff(janela, lag = 1, differences = 1))
# H0 do Adf é a série não é estacionaria. p valor pequeno rejeitando h0.

kpss.test(diff(janela, lag = 1, differences = 1))
#A hipotese H0 do kpss é a serie estacionaria aceita a hipotese serie é estacionaria.

pp.test(diff(janela, lag = 1, differences = 1))
#H0 do Adf é a série não é estacionaria. p valor pequeno rejeitando h0.

ts_cor(diff(janela, lag = 1, differences = 1))

# o Pacf fala com a parte auto regressiva (AR) p = 1

#O acf fala com a parte MA(q) então q=2

# A defasagem e igual d=1

# A parte não sazonal do sarima (p,d,q) fica então (1,1,2).

# A parte sazonal do sarima (P,D,Q) [m] e dada pela seguinte sequencia: 
# P= 1 (ver PCAF linha vertical vermelha)
# Q= 1 (ver ACF linha vertical Vermelha)
# D= 1 foi realizado uma defasagem

# O modelo então fica SARIMA (p,d,q) (P,D,Q) [m] fica da seguinte forma
# SARIMA (1,1,2) (1,1,1) [12]

#PRIMEIRO MODELO SARIMA(1,1,1)(1,1,1)12

modelo.1 <- Arima(janela, 
                  order = c(1,1,2),#parte não sazonal 
                  seasonal = c(1,1,1),#parte sazonal
                  method = "ML", 
                  lambda = 0) # lambda = zero efetua a transforma??o de Box-Cox
modelo.1

previsao <- forecast(modelo.1, h=12, level = c(60,80))

autoplot(janela, series = "Observada")+
  autolayer(modelo.1$fitted, series = "Modelo")+
  autolayer(previsao,series = "Previsão", showgap = FALSE)


#####FIM Da AULA#######













# Passando o log (para reduzir a heterocedasticidade)

ts.plot(diff(log(AirPassengers),lag = 1,differences = 1))

autoplot((diff(log(AirPassengers),lag = 1,differences = 1)))+
  ylab("diff(log(AirPassengers)")+
  ggtitle("Log da ST de vendas de passagens aéreas (em milhares) com uma diferença")


g1 <- ggAcf((diff(log(AirPassengers),lag = 1,differences = 1)), lag.max = 36)
g2 <- ggPacf((diff(log(AirPassengers),lag = 1,differences = 1)), lag.max = 36)
grid.arrange(g1,g2, nrow = 2)

# Na parte não sazonal (p,d,q) => p = 1 (do Pacf),  d (1 defasagem) e q = do MA

# PARTE SAZONAL

# o lag = 12

# Faremos a difererencia??o, alterando a fun??o diff para o lag 12 em raz?o da 
# da s?rie ser mensal

# Especificação da Sazonalidade

acf <- ggAcf(diff(diff(log(AirPassengers), lag = 1, differences = 1),
                  lag = 12), # LAG = 12
             lag.max = 48)

pacf <- ggPacf (diff(diff(log(AirPassengers), lag = 1, differences = 1),
                     lag = 12), #LAG =12
                lag.max = 48)

grid.arrange(acf, pacf, nrow = 2)

# Parte sazonal Veja o lag 12 

# (P, D , Q) P = 1 (Pacf) D = 1, Q = 1 (Acf)


# O modelo fica com uma defasagem na parte n?o sazonal 
# e uma defasagem na pate sazonal
ggAcf(janela)




#PRIMEIRO MODELO SARIMA(1,1,1)(1,1,1)12

modelo.1 <- Arima(AirPassengers, 
                  order = c(1,1,1), 
                  seasonal = c(1,1,1),
                  method = "ML", 
                  lambda = 0) # lambda = zero efetua a transforma??o de Box-Cox
modelo.1

# teste de signific?ncia para o modelo SARIMA(1,1,1)(1,1,1)12

t_test(modelo.1)

# O teste indica que os parâmetros das partes autorregressivas dos par?metros
# n?o sazonal e sazonal n?o s?o significatimente estat?ticos.

# REDESENHANDO UM NOVO MODELO SARIMA (0,1,1)(0,1,1)12

modelo.2 <- Arima(AirPassengers,
                  order = c(0,1,1),
                  seasonal = c(0,1,1),
                  method = "ML", lambda=0)

# Lambda = zero faz a transforma??o logaritma

modelo.2

t_test(modelo.2)

# Par?metros estatisticamente significativos

# Efatuando a previsão
previsao.modelo.2 <- forecast(modelo.2, h= 24, level =60)

autoplot(AirPassengers, series = "Original", lwd =1.1)+
  autolayer(previsao.modelo.2$fitted, series = "Modelo.2", lwd= 1.1)+
  autolayer(previsao.modelo.2, series = "Previs?o")

# utizando a fun??o auto.arima() do pacote fpp2

auto.arima <- auto.arima(AirPassengers, seasonal= TRUE, stepwise = FALSE, approximation = FALSE)
auto.arima

#ARIMA(2,1,1)(0,1,0)[12] 

previsao.auto.arima <- forecast(auto.arima, h=24, level =60)

autoplot(AirPassengers, series = "Original", lwd =1.1)+
  autolayer(previsao.auto.arima$fitted, series = "Modelo Auto.ARIMA", lwd= 1.1)+
  autolayer(previsao.auto.arima, series = "Previsão Auto.ARIMA")

# COMPARANDO OS MODELOS PELO CRIT?RIO DE INFORMA??O DE AKAIKE

AIC(modelo.2) # O modelo 2 é melhor que o Autoarima
AIC(auto.arima)

# Escolha do melhor modelo: O menor AICc
# ARIMA(0,1,1)(0,1,1)[12]

autoplot(AirPassengers)+
  autolayer(previsao.modelo.2, serie = "Modelo2", showgap = FALSE)+
  autolayer(previsao.auto.arima, serie = "Auto.Arima", showgap = FALSE)

# Visualização dos res?duos do modelo.2

autoplot(modelo.2$residuals) + theme_minimal()+
  ylab("Res?duos")+
  ggtitle("ARIMA(0,1,1)(0,1,1)12 - Res?duos")

hist(modelo.2$residuals)

df <- data.frame(modelo.2$residuals)
df

ggplot(df, aes(x = modelo.2.residuals)) + 
  geom_histogram(aes(y=..density..), colour="darkblue", fill="white")+
  geom_density(alpha=.1, fill="steelblue", lwd=0.3)+
  ggtitle("Histograma e densidade dos res?duos")+
  xlab("Frequência")+
  ylab("densidade")+theme_minimal()

#TESTES DE DIAGN?STICOS

#Ljung Box

Box.test(x = previsao.modelo.2$residuals, lag = 24, type = "Lj", fitdf=2)

# n?o rejeitamos Ho: a s?riese comporta como um ru?do branco

checkresiduals(previsao.modelo.2)

# Heterocedasticidade
require(FinTS)

ArchTest(modelo.2$residuals,lags = 36)

# N?o rejeitamos Ho: a s?rie ? homoced?stica

require(normtest)
jb.norm.test(modelo.2$residuals, nrepl=2000)
shapiro.test(modelo.2$residuals)

# rejeita Ho: N(0,1)

# Vejamos o sum?rio estat?stico

round(summary(modelo.2$residuals), digits =4)

moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

result <- moda(as.numeric(modelo.2$residuals))
print(result)

m?dia~ mediana ~ moda ~ 0


# REAPRSENTA??O DO MODELO 2


#install.packages("ggthemes") # script com backgrounds bem interessantes
library(ggthemes)

autoplot(object=previsao.modelo.2,
         xlab="Meses", ylab="Vendas Passagens Aéreas por m?s em milhares",
         main="AirPassengers - ARIMA(0,1,1)(0,1,1)[12]",PI = TRUE, 
         fcol = "steelblue")+ 
  theme_fivethirtyeight()

# Altenativamente:

autoplot(AirPassengers, series = "Original")+
  autolayer(previsao.modelo.2$fitted, series = "Modelo")+
  autolayer(previsao.modelo.2, series = "Previsao")+
  ylab("Passageiros em Mil Unidades")+
  xlab("Meses")+
  ggtitle("AirPassagers - Modelo SARIMA proposto por Box - Jenkins")+
  theme_economist()


