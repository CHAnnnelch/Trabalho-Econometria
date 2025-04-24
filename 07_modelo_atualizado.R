rm(list=ls())

library(stats)
library(lmtest)
library(corrplot)
library(car)
library(dplyr)
library(performance)
library(tidyverse)

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

check_normality(modelo)
check_collinearity(modelo)
check_heteroscedasticity(modelo)
check_outliers(modelo)

#matriz correlacao
df_selected <- df %>% select(CPIscore, scofree, stability, propr, govint, taxb, govspend, g_GDP, busfree, labfree, monfree, tradfree, investfree, finfree, CPIrank)
M = cor(df_selected)

png("matriz_de_corr.png", width = 1920, height = 1080)
corrplot(M, method='number', tl.cex = 0.8, number.cex = 0.8)
dev.off()

#testes
dwtest(modelo) #a autocorrelação existe RLS.4 é

# visualizar predicoes
library(ggeffects)
ggeffect(modelo)

plot_pred <- ggeffect(modelo) %>%
  plot() %>%
  sjPlot::plot_grid()

plot_pred

ggsave(
  'plot_predicoes.png',
  plot = plot_pred,
  device = png,
  dpi = 999,
  width = 9,
  height = 7
)

# tabelas
library(gtsummary)
tabela_pred <- tbl_regression(
  modelo,
  add_pairwise_contrasts = T,
  pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  bold_p()

#salvar tabela
library(flextable)
tabela_pred %>% 
  as_flex_table() %>%
  save_as_image(path =  "tabela_pred.png")

#equacao da regressao
library(equatiomatic)
extract_eq(modelo)

#mais coisas
library(emmeans)
library(sjPlot)
library(effectsize)

tab_model(modelo, show.intercept = F)

ef_size <- eta_squared(modelo) %>%
  mutate(Interpret = interpret_eta_squared(Eta2_partial))

ef_size

ef_size %>%
  ggplot(aes(x= reorder(Parameter, Eta2_partial),
             y= Eta2_partial))+
  geom_bar(stat="identity")+
  geom_text(aes(label=Interpret),
            size = 4, hjust = -0.1, fontface = "bold")+
  coord_flip()+
  xlab("")+
  ylim(0, .99)

performance(modelo)

interpret_r2(0.924, rules = "hair2011")

?interpret_r2

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

png("check_model_log_output.png", width = 1920, height = 1080)
check_model(modelo_log)
dev.off()

check_normality(modelo_log)
check_collinearity(modelo_log)
check_heteroscedasticity(modelo_log)
check_outliers(modelo_log)

dwtest(modelo_log) #p-valor = 0.07117, RLS.4 potencialmente atendida

# visualizar predicoes
library(ggeffects)
ggeffect(modelo_log)

plot_pred <- ggeffect(modelo_log) %>%
  plot() %>%
  sjPlot::plot_grid()

plot_pred

ggsave(
  'plot_predicoes_log.png',
  plot = plot_pred,
  device = png,
  dpi = 999,
  width = 9,
  height = 7
)

# tabelas
library(gtsummary)
tabela_pred <- tbl_regression(
  modelo_log,
  add_pairwise_contrasts = T,
  pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  bold_p()

#salvar tabela
library(flextable)
tabela_pred %>% 
  as_flex_table() %>%
  save_as_image(path =  "tabela_pred_log.png")

#equacao da regressao
library(equatiomatic)
extract_eq(modelo_log)

#mais coisas
library(emmeans)
library(sjPlot)
library(effectsize)

tab_model(modelo_log, show.intercept = F)

ef_size <- eta_squared(modelo_log) %>%
  mutate(Interpret = interpret_eta_squared(Eta2_partial))

ef_size

ef_size %>%
  ggplot(aes(x= reorder(Parameter, Eta2_partial),
             y= Eta2_partial))+
  geom_bar(stat="identity")+
  geom_text(aes(label=Interpret),
            size = 4, hjust = -0.1, fontface = "bold")+
  coord_flip()+
  xlab("")+
  ylim(0, .99)

performance(modelo_log)

interpret_r2(0.944, rules = "hair2011")

?interpret_r2


