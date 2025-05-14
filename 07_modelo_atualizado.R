rm(list=ls())

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

df <- df %>%
  mutate(ID_pais = group_indices(., Country))

#modelo completo
modelo <- plm(CPIscore ~ 
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
             data=df,
             model='within',
             index=c("ID_pais","Index.Year"))
summary(modelo)

model_performance(modelo)

check_model(modelo)

#modelo reduzido com variávies de gasto e liberadade
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

phtest(modelo_red, modelo_red_random)

resettest(modelo_red)

model_performance(modelo_red)

check_model(modelo_red)

stargazer(modelo_red, type='latex', align=TRUE,
          title="Resultados da regressão em painel", ci=TRUE,
          ci.level=0.95, single.row=TRUE)

summary(modelo_red)

model_performance(modelo_red)


#modelo com variáveis de gastos do governo
modelo_gastos <- plm(CPIscore ~ 
                       taxb +
                       govspend +
                       g_GDP,
                     data=df,
                     model='within',
                     index=c("ID_pais","Index.Year"))
summary(modelo_gastos)

model_performance(modelo_gastos)

check_model(modelo_gastos)


#modelo reduzido com variávies de liberdade
modelo_free <- plm(CPIscore ~ 
                    busfree +
                    labfree +
                    monfree +
                    tradfree +
                    investfree +
                    finfree,
                  data=df,
                  model='within',
                  index=c("ID_pais","Index.Year"))
summary(modelo_free)

model_performance(modelo_free)

check_model(modelo_free)

pdwtest(modelo_red)

compare_performance(modelo_red, modelo_red_random, modelo, modelo_gastos, modelo_free)

#model = 'within' - modelo de efeito fixo

fig_performance = plot(check_model(modelo))
ggsave(filename = "plot_performance.jpg",
       plot = fig_performance,
       width = 15,
       height = 10,
       dpi = 500)

#resultados do modelo em LaTeX
stargazer(modelo, type="latex", no.space=TRUE, align=TRUE)

check_collinearity(modelo)
check_heteroscedasticity(modelo)

#matriz correlacao
library(xtable)

df_selected <- df %>% select(CPIscore, taxb, govspend, g_GDP, busfree, labfree, monfree, tradfree, investfree, finfree)
M = cor(df_selected)

xtable(M, caption="Matrix de Correlação", label="tab:matriz_corr")

corrplot(M, method='number', tl.cex = 0.8, number.cex = 0.8)

#equacao da regressao
library(equatiomatic)
extract_eq(modelo)

#mais coisas
library(emmeans)
library(sjPlot)
library(effectsize)
library(vtable)

tab_model(modelo, show.intercept = F)

interpret_r2(0.262, rules = "hair2011")

?interpret_r2

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

#modelo random forest
library(randomForest)
library(vip)

modelo_rf <- randomForest(CPIscore ~ 
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
               data = df)

modelo_rf

vip(modelo_rf)

#espaço pra citações
citation("performance")