library(readxl)
df <- read_excel("parte 3 o aumento de Bolsonaro.xlsx")
df$nivel_apoio_turno1 <- ntile(df$Bolsonaro_turno1, 3)
df$nivel_apoio_turno1 <- as.factor(df$nivel_apoio_turno1)
levels(df$nivel_apoio_turno1) <- c('menor','mediano','alto')
summary(df$nivel_apoio_turno1)# os chamados tercis

banco22 <- subset(df, select = c(Cidade_Bairro, Localidade, nivel_apoio_turno1))
unique(banco22)
library(tidyverse)
library(knitr)
library(kableExtra)
b5 <- banco22 %>% 
  dplyr::select(Cidade_Bairro,Localidade,nivel_apoio_turno1) %>% 
  arrange(Cidade_Bairro)
b5 %>%
  kbl(caption = "Lista de localidades utilizada na análise") %>%
  kable_classic(full_width = F, html_font = "Garamond")

library(sjPlot)
modelo_nivel_apoio2 <- lm(aumento ~ nivel_apoio_turno1 + Amoedo +
                            Ciro + Localidade, data=df)
tab_model(modelo_nivel_apoio2)

fit <- modelo_nivel_apoio2

df$predito <- fit$fitted.values
df$residuos <- fit$residuals

b5 <- df %>% 
  dplyr::select(Cidade_Bairro,Localidade, aumento, predito, residuos) %>% 
  arrange(Cidade_Bairro)
b5 %>%
  kbl(caption = "Lista de preditos e resíduos") %>%
  kable_classic(full_width = F, html_font = "Garamond")


# modelo beta

library(lm.beta)
modelox <- lm.beta(fit)
tab_model(modelox, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")
variable <- c('ApoioMedianoTurno1', 'ApoioAltoTurno1', 'Amoedo', 'Ciro', 'LocalidadeCidadeAltoVale')
valor <- c(5,41,45,11,14)

horr <- data.frame(variable,valor)

bar <- ggplot(horr, aes(variable, valor))
bar + geom_bar(stat = "identity") + 
  labs(title = "Beta padronizado das variáveis na regressão", 
       subtitle = "A regressão utilizada no site",
       x = "Variável", y= "Beta padronizado",
       caption = "métrica vai de 0 a 100") + 
  theme_bw() + theme(text = element_text(size = 12))
