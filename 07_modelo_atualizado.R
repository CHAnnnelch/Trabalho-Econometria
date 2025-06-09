#limpando o ambiente de trabalho
rm(list=ls())

#importando bibliotecas
library(stats)
library(lmtest)
library(corrplot)
library(car)
library(dplyr)
library(performance)
library(tidyverse)
library(see)
library(plm)
library(stargazer)

df <- read.csv("06_df_compilado_guiana.csv")

#criação da coluna ID dos países
df <- df %>%
  mutate(ID_pais = group_indices(., Country))

#modelo com efeito fixo
modelo_red <- plm(CPIscore ~ 
                taxb +
                govspend +
                g_GDP +
                busfree +
                labfree +
                monfree +
                tradfree +
                investfree +
                finfree,
              data=df,
              model='within',
              index=c("ID_pais","Index.Year"))
summary(modelo_red)

#modelo com efeito aleatório
modelo_red_random <- plm(CPIscore ~ 
                           taxb +
                           govspend +
                           g_GDP +
                           busfree +
                           labfree +
                           monfree +
                           tradfree +
                           investfree +
                           finfree,
                         data=df,
                         model='random',
                         index=c("ID_pais","Index.Year"))

#teste de hausman
phtest(modelo_red, modelo_red_random)

#performance
model_performance(modelo_red)

check_model(modelo_red)

#heteroscedasticidade

bptest(modelo_red)

#autocorrelação

pdwtest(modelo_red)

#correção dos coeficientes (heteroscedasticidade e autocorrelação) =usando uma matriz robusta
coef <- coeftest(modelo_red, vcov = vcovSCC(modelo_red, type = "HC1"))
coef

#importação automática para LaTeX
stargazer(modelo_red, coef, type='latex', align=TRUE,
          title="Resultados da regressão em painel", single.row=TRUE)

#linearidade 
#plot dos resíduos
res <- modelo_red$residuals
fitted_vals <- fitted(modelo_red)

ggplot(data.frame(res, fitted_vals), aes(x = fitted_vals, y = res)) +
  geom_point() +
  geom_smooth(se = TRUE, method = "loess", color = "green") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals")

#normalidade dos resíduos
media_res <- mean(res)
dp_res <- sd(res)

#teste de kolmogorov-smirnov
ks.test(res, "pnorm", mean = media_res, sd = dp_res)

#matriz correlacao
library(xtable)

df_selected <- df %>% select(CPIscore, taxb, govspend, g_GDP, busfree, labfree, monfree, tradfree, investfree, finfree)
M = cor(df_selected)

xtable(M, caption="Matriz de Correlação", label="tab:matriz_corr")

corrplot(M, method='number', tl.cex = 0.8, number.cex = 0.8)

#equacao da regressao
library(equatiomatic)
extract_eq(modelo)

#interpretação r2
library(effectsize)

interpret_r2(0.237, rules = "cohen1988")

#estatísticas descritivas
df_summary <- df %>% select(CPIscore, 
                              taxb,
                              govspend,
                              g_GDP,
                              busfree,
                              labfree,
                              monfree,
                              tradfree,
                              investfree,
                              finfree)

sumtable(df_summary, out='latex')