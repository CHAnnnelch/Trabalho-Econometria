rm(list=ls())

library(stats)
library(lmtest)
library(corrplot)
library(car)
library(dplyr)
library(performance)

df <- read.csv("06_df_compilado_guiana.csv")

modelo <- lm(CPIscore ~ 
               stability +
               propr +
               govint +
               taxb +
               govspend +
               g_GDP +
               busfree +
               labfree +
               monfree +
               tradfree +
               investfree +
               finfree,
             data=df)
summary(modelo)

model_performance(modelo)

png("check_model_output.png", width = 1920, height = 1080)
check_model(modelo)
dev.off()

check_collinearity(modelo)
check_heteroscedasticity(modelo)

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

#transformação pra log
CPIscore_log <- log(df$CPIscore)
stability_log <- log(df$stability)
propr_log <- log(df$propr)
govint_log <- log(df$govint)
taxb_log <- log(df$taxb)
govspend_log <- log(df$govspend)
g_GDP_log <- log(df$g_GDP)
busfree_log <- log(df$busfree)
labfree_log <- log(df$labfree)
monfree_log <- log(df$monfree)
tradfree_log <- log(df$tradfree)
investfree_log <- log(df$investfree)
finfree_log <- log(df$finfree)

#aplicação do modelo log
modelo_log <- lm(CPIscore_log ~ 
               stability_log +
               propr_log +
               govint_log +
               taxb_log +
               govspend_log +
               g_GDP_log +
               busfree_log +
               labfree_log +
               monfree_log +
               tradfree_log +
               investfree_log +
               finfree_log)

summary(modelo_log)

#avaliação da performance
model_performance(modelo_log)

png("check_model_log_output.png", width = 1920, height = 1080)
check_model(modelo_log)
dev.off()

dwtest(modelo_log) #p-valor = 0.07117, RLS.4 potencialmente atendida

check_collinearity(modelo_log) #multicoli alta

check_heteroscedasticity(modelo_log) #heteroscedasticidade detectada

#modelo log performou pior do que o modelo convencional
#possiveis saídas: regressão robusta/bootstrap/lasso sla