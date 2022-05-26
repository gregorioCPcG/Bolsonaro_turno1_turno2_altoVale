#Analisando a relacao part2_withCiro

library(readxl)
df <- read_excel("parte 2 CIRO.xlsx")


# Bolsonaro segundo turno e Amoedo primeiro turno(Ciro  também)

cor.test(df$Amoedo, df$Bolsonaro)
cor.test(df$Amoedo, df$Ciro)
cor.test(df$Ciro, df$Bolsonaro) # no sig

# gráficos parte 2 (with ciro)

library(ggplot2)
# ciro x Bolsona
grafic <- ggplot(df, aes(Bolsonaro, Ciro))

g <- grafic + geom_point()
g # locasso sem nenhuma lógica
g + geom_smooth(model=lm, se=FALSE)


grafic+ geom_text(label = df$Cidade_Bairro)
g + geom_text(label = df$Cidade_Bairro)
grafic + geom_text(label = df$Cidade_Bairro) + geom_smooth(model=lm, se=FALSE)

g2 <- g +  geom_smooth(model=lm, se=FALSE)
g2 + geom_point(aes(colour= df$Localidade))



 # ciro vs amoedo

grafic <- ggplot(df, aes(Amoedo, Ciro))

g <- grafic + geom_point()
g # tem certa lógica
g + geom_smooth(model=lm, se=FALSE)


grafic+ geom_text(label = df$Cidade_Bairro)
g + geom_text(label = df$Cidade_Bairro)
grafic + geom_text(label = df$Cidade_Bairro) + geom_smooth(model=lm, se=FALSE)

g2 <- g +  geom_smooth(model=lm, se=FALSE)
g2 + geom_point(aes(colour= df$Localidade))

# claramente mais votos nos bairros, do que nas cidades para ambos

# modelos de regressão 
model <- lm(Bolsonaro ~ Ciro, data=df)
summary(model)# no correlation

model2 <- lm(Bolsonaro ~ Ciro + Amoedo, data=df)
summary(model2) # ciro reduz, amoedo aumenta

model3 <- lm(Bolsonaro ~ Ciro + Amoedo + Localidade, data=df)
summary(model3) # ciro reduz, amoedo aumenta

model4 <- lm(Bolsonaro ~ Amoedo, data=df)

library(sjPlot)


tab_model(model, model2, model3, model4, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")

library(coefplot)
coefplot(model2, intercept=FALSE, interactive=TRUE)
coefplot(model3, intercept=FALSE, interactive=TRUE)

#interflex
library(interflex)
data(interflex)
#Amoedo e Bolso
ls()
Y = df$Bolsonaro
D = df$Localidade
X = df$Amoedo
df2 <- data.frame(Y,D,X)
s1<-cbind.data.frame(Y = Y, D = D, X = X)
a <- interflex::interflex(estimator = "raw",Y = "Y", D = "D", X = "X",
                          data = s1, weights = NULL, Ylabel = "Bolsonaro segundo turno",
                          Dlabel = " Bairro de Rio do Sul?", Xlabel="Amoedo 2018",
                          main = "Cidade Alto Vale/Bairro Rio do Sul 
          na relação voto Amoedo/Bolsonaro
          ", cex.main = 0.7, ncols=2)
plot(a)


#Ciro e Bolso
Y = df$Bolsonaro
D = df$Localidade
X = df$Ciro
df2 <- data.frame(Y,D,X)
s1<-cbind.data.frame(Y = Y, D = D, X = X)
b <- interflex::interflex(estimator = "raw",Y = "Y", D = "D", X = "X",
                          data = s1, weights = NULL, Ylabel = "Bolsonaro segundo turno",
                          Dlabel = " Bairro de Rio do Sul?", Xlabel="Ciro 2018",
                          main = "Cidade Alto Vale/Bairro Rio do Sul 
          na relação voto Ciro/Bolsonaro
          ", cex.main = 0.7, ncols=2)
plot(b)


AltoVale27 <- subset(df, Localidade = "Cidade AltoVale")


#comaprando modelos do alto vale 27 e dos bairros
library(tidyverse)
AltoVale27 <- df %>%
  filter(cid_rsl == "0")

Bairro_RSL <- df%>%
  filter(cid_rsl == "1")
model24 <- lm(Bolsonaro ~ Ciro + Amoedo, data=Bairro_RSL)
summary(model24) # ciro reduz, amoedo aumenta
model27 <- lm(Bolsonaro ~ Ciro + Amoedo, data=AltoVale27)
summary(model27) # ciro reduz, amoedo aumenta


tab_model(model24, model27, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")


# nova dep 
# AUMENTO BOLSONARO
# = votação no segundo turno de Bolsonaro - votação no segundo turno - ver parte 3
