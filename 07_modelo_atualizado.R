rm(list=ls())

library(stats)
library(lmtest)
library(corrplot)
library(car)
library(dplyr)
library(performance)

df <- read.csv("06_df_compilado_guiana.csv")

modelo <- lm(CPIscore ~ stability + propr + govint + taxb + govspend + g_GDP + busfree + labfree + monfree + tradfree + investfree + finfree, data=df)
summary(modelo)

model_performance(modelo)

png("check_model_output.png", width = 1920, height = 1080)
check_model(modelo)
dev.off()

check_collinearity(modelo)

#plot residuos
plot(modelo$fitted.values, modelo$residuals,
     xlab='Fitted', ylab='Residuais',
     main='Plot dos Residuos')
abline(0, 0)

#matriz correlacao
df_selected <- df %>% select(CPIscore, scofree, stability, propr, govint, taxb, govspend, g_GDP, busfree, labfree, monfree, tradfree, investfree, finfree, CPIrank)
M = cor(df_selected)
corrplot(M, method='number', tl.cex = 0.8, number.cex = 0.8)

#testes
dwtest(modelo) #a autocorrelação existe RLS.4 é

bptest(modelo) #p-valor 0.1001 - n rejeita hipotese nula, n há evidencias de heteroscedasticidade

vif(modelo) #scofree apresenta um valor alto de multicoli, mas isso faz sentido já que essa variável é calculada com base nas demais do Economic Freedom Index

#qqnorm e qqline
qqnorm(modelo$residuals)
qqline(modelo$residuals)
#distribuição aparente normal, ou possível de se aproximar à normal

#espaço para citações
citation("performance")
citation("corrplot")
