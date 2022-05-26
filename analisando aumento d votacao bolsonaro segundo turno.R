# parte 3 nova variável dependente

library(readxl)
df <- read_excel("parte 3 o aumento de Bolsonaro.xlsx")
summary(df)
# aumento é o resultado da subtração bolsonaroturno2 - bolsonaroturno1
boxplot(df$aumento)
# correlação primeiro e segundo turno
cor.test(df$Bolsonaro_turno1, df$Bolsonaro_turno2)#esperado
# correlação aumento e localidade
cor.test(df$aumento, df$cid_rsl)# positivo, mas fraco
library(polycor) #pacote exigido
polyserial(df$aumento, df$cid_rsl)# fraco , mas tem
library(ggplot2)
#boxplot para ver mais visual
box <- ggplot(df, aes(aumento,Localidade))
box + geom_boxplot()
#bairros de rio do sul tiveram aumentos mais consideráveis, 
# votação no primeiro turno
box1 <- ggplot(df, aes(Bolsonaro_turno1,Localidade))
box1 + geom_boxplot()
# voytação no segundo turno
box2 <- ggplot(df, aes(Bolsonaro_turno2,Localidade))
box2 + geom_boxplot()

#suspeitamos que a localidade tem algum efeito, voltamos a esse tópico mais à frente

# guarda essa descoberta que ela é interessante!

# primeiro tudo separado

#depois eu junto
library(tidyverse)
# Bairros de Rio do Sul
Bairro_RSL <- df%>%
  filter(cid_rsl == "1")
cor.test(Bairro_RSL$Amoedo, Bairro_RSL$aumento)#nao sig
cor.test(Bairro_RSL$Ciro, Bairro_RSL$aumento)#nao sig

# nao seguirei por esse caminho


#27 cidades do Alto Valte
AltoVale27 <- df %>%
  filter(cid_rsl == "0")
cor.test(AltoVale27$Amoedo, AltoVale27$aumento)#nao sig
cor.test(AltoVale27$Ciro, AltoVale27$aumento)#nao sig

# nao seguirei por esse caminho

# tudo junto ##
cor.test(df$Amoedo, df$aumento)#Sig e positivo, ao aumentar o N
cor.test(df$Ciro, df$aumento)#Sig e positivo, mas fraco (louoo)

# vamos ver visual

# nas 51 localidades

#primeiro Amoedo

amoedo <- ggplot(df, aes(aumento, Amoedo))
amoedo + geom_point() + geom_smooth(model=lm, se=FALSE)
amoedo + geom_point(aes(colour= df$Localidade)) + geom_smooth(model=lm, se=FALSE)
ciro <- ggplot(df, aes(aumento, Ciro))
ciro + geom_point() + geom_smooth(model=lm, se=FALSE)
ciro + geom_point(aes(colour= df$Localidade)) + geom_smooth(model=lm, se=FALSE)
# para amoedo tem uma relação mais lógica,
# para ciro os bairros de rio do sul, parecem ter virado mais pró Bolsonaro do que
# era de se esperar

# modelos regressão

modelciro <- lm(aumento ~ Ciro, data=df)
modelamoedo <- lm(aumento ~ Amoedo, data =df)
model2 <- lm(aumento ~ Amoedo + Ciro, data=df)
modelfull <- lm(aumento ~ Amoedo + Ciro + Localidade, data=df)
library(sjPlot)
tab_model(modelciro, modelamoedo, model2, modelfull, show.ci = F,
          auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")

# a localidade nao melhoru os ajustes, logo o melor modelo é os que tem os dois

# na terceira coluna , a informação diz que numa situação hipotética com Ciro e Amoedo com 0, o aumento de Bolsonaro seria 6.8
# Ciro nao tem efeito estatisticamente significativo
# Já o modelo (da terceira coluna) prevê que a cada 1% a mais em Amoedo,
# aumentaria em 0.49 a votação de Bolsonaro nos segundo turno em uma das 51 localidades

#  a QUARTA COLUNA ADICIONA A LOCALIDADE  E NAO MUDA OS ACHADOS DA TERCEIRA COLUNA
# ABAIXO VEMOS OS EFEITOS DE CIRO, AMOEDO E LOCALIDADE (SE É BAIRRO OU CIDADE)
# E VERIFICAMOS QUE SOMENTE AMOEDO NÃO PASSA NA LINHA DO ZERO
library(coefplot)
coefplot (modelfull, intercept=FALSE, interactive = TRUE)

# outra descoberta importante é q a diferença entre cidades e bairros nao "sobrevive"
# nao segue sendo sig, quando controlada por Ciro e Amoedo
# Já amoedo segue "vivo", mesmo controlado por Localidade e Ciro

# outra forma de ver o que consta acima na imagem é apresentando os intervalos
tab_model(modelfull)

# o efeito de Amoedo é entre 0.06 e entre 0.80 (0.43 é a mediana, por isso o p está negritado menor que 0.05)
# em Ciro o efeito pode ser negativo e positivo (não se tem segurnad)
# o mesmo para Localidade
# o efeito pode variar desde reduzir -1.31 o aumento, até aumentar em 0.73 o efeito
# por isso nao se tem segurança

# interflex
# o bom desempenho de Amoedo na 'explicacação' dos ganhos de Bolsonaro do primeiro para o segundo turno eram esperados, até por estarem ambos os candidatos no campo da Direita.
# Ciro, apesar de suas brigas com o PT, consta mais à esquerda e suspeita-sse por isso os eleitores foram menos propensos a escolher Bolsonaro no segundo turno.

#  o interflex pode ajudar nesse aspecto ao apontar os efeitos por Localidade

library(interflex)
data(interflex)
#Amoedo e aumento
ls()
Y = df$aumento
D = df$Localidade
X = df$Amoedo
df2 <- data.frame(Y,D,X)
s1<-cbind.data.frame(Y = Y, D = D, X = X)
a <- interflex::interflex(estimator = "raw",Y = "Y", D = "D", X = "X",
                          data = s1, weights = NULL, Ylabel = "crescimento Bolsonaro segundo turno",
                          Dlabel = " Bairro de Rio do Sul?", Xlabel="Amoedo 2018",
                          main = "Cidade Alto Vale/Bairro Rio do Sul 
          na relação voto Amoedo/Bolsonaro
          ", cex.main = 0.7, ncols=2)
plot(a)


#Ciro e aumento
Y = df$aumento
D = df$Localidade
X = df$Ciro
df2 <- data.frame(Y,D,X)
s1<-cbind.data.frame(Y = Y, D = D, X = X)
b <- interflex::interflex(estimator = "raw",Y = "Y", D = "D", X = "X",
                          data = s1, weights = NULL, Ylabel = "crescimento Bolsonaro segundo turno",
                          Dlabel = " Bairro de Rio do Sul?", Xlabel="Ciro 2018",
                          main = "Cidade Alto Vale/Bairro Rio do Sul 
          na relação voto Ciro/Bolsonaro
          ", cex.main = 0.7, ncols=2)
plot(b)
# o interflex nao ajuda nesse caso


# para ampliar os modelos
# vamos dividir as localidades em 3 grupos, divididos pela votação de Bolsonaro no primeiro turno, vamos tentar verificar se esses níveis de apoio no primeiro turno tem rlação com a quantidade de aumento. Será que os bairros que votaram mais no primeiro aumentaram ou o oposto ? 
# é isso que esse dado ajudará a descobrir

#começando criando a variavel 'nivel_apoio_turno1'

df$nivel_apoio_turno1 <- ntile(df$Bolsonaro_turno1, 3)
df$nivel_apoio_turno1 <- as.factor(df$nivel_apoio_turno1)
levels(df$nivel_apoio_turno1) <- c('menor','mediano','alto')
summary(df$nivel_apoio_turno1)# os chamados tercis

modelo_nivel_apoio <- lm(aumento ~ nivel_apoio_turno1, data=df)
tab_model(modelo_nivel_apoio)# nao sig, mas quase
modelo_nivel_apoio2 <- lm(aumento ~ nivel_apoio_turno1 + Amoedo +
                           Ciro + Localidade, data=df)
tab_model(modelo_nivel_apoio2)# aí é sig e tem efeito negativo
# ou seja vamos interpretar o modelo
# Localidade  eCiro seguem não tem efeito
# o R2 é o melhor até agora
# abaixo apresentarei uma comparação de modelos, mas antes vamos interpretar

# Amoedo segue tendo efeito entre 0,13 e 0,83(estimado de 0,48)

# no caso do nivel de apoio, aqueles de nivel de apoio 1 (ou seja localidades que votaram menos em Bolsonaro no primeiro turno) sao comparadas com o nivel de apoio médio (2) e com o nível de apoio maior (maior votação em Bolsonaro [3])
# ou seja nivel 1 é referencia por isso nao aparece na tabela
# comparado com 2 não é significante estatisticamente (a imagem abaixo tornará mais claro)
# ter votado mais em Bolsonaro no primeiro turno (tercil superior) reduz em -1.29 o aumento em Bolsonaro no segundo turno em comparação ao primeiro (intervalo entre -2,2 e -0.37)

coefplot (modelo_nivel_apoio2, interactive = TRUE, intercept=FALSE)
# Ou seja o aumento maior se deu nas localidades em que Bolsonaro não foi tão votado


tab_model(modelamoedo, modelfull,modelo_nivel_apoio2, show.ci = F,
          auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")


# no brasil , Bolsonaro foi de 46,03 para 55,13
# percebam como ele é muito mais votado no ALto Vale
# o aumento dele no Brasil foi de 9.1 

# já em SC ele foi de 65,82 no primeiro turno (mesmo assim a maioria dos 51 casos aqui ficou acima, ou seja estamos falando um região altamente antipetista, maior até que a media do estado mais antipetista SC) no segundo turno os catarinentes deram 75,92 % para Bolsonaro
# ou seja o aumento de 10,1

# comparação votação primeiro turno
#maior que o Brasil
df$turno1_maiorqBra <- df$Bolsonaro_turno1 > 46.03
summary(df$turno1_maiorqBra) # todos os 51 tiveram maior votação que a votação nacional
df$turno1_maiorqSC <- df$Bolsonaro_turno1 > 65.82
summary(df$turno1_maiorqSC) # 7 foram as localidades com votação menor que SC no primeiro turno
ggplot(data =df) +
  geom_point(mapping = aes(x=Localidade, y =Bolsonaro_turno1 , color = Bolsonaro_turno1 > 65.82 ))# 

ggplot(data =df, aes(Localidade, Bolsonaro_turno1)) + geom_text(label = df$Cidade_Bairro)+
  geom_point(mapping = aes(x=Localidade, y =Bolsonaro_turno1 , color = Bolsonaro_turno1 > 65.82))#

# segundo turno

df$turno2_maiorqBra <- df$Bolsonaro_turno2 > 55.13
summary(df$turno2_maiorqBra) # todos os 51 tiveram maior votação que a votação nacional
df$turno2_maiorqSC <- df$Bolsonaro_turno2 > 75.92
summary(df$turno2_maiorqSC) # sete casos
ggplot(data =df) +
  geom_point(mapping = aes(x=Localidade, y =Bolsonaro_turno2 , color = Bolsonaro_turno2 > 75.92 ))# 
# no segundo turno as sete são cidades do Alto Vale

# agora o aumento
df$aumento_maiorqBra <- df$aumento > 9.1
summary(df$aumento_maiorqBra) # 19 tiveram maior aumento que no Brasil

ggplot(data =df) +
  geom_point(mapping = aes(x=Localidade, y =aumento , color = aumento > 9.1 ))# 
# a maior parte em Bairros


# maior que SC 
df$aumento_maiorqSC <- df$aumento > 10.1
summary(df$aumento_maiorqSC) # 5 tiveram maior aumento que em SC

ggplot(data =df) +
  geom_point(mapping = aes(x=Localidade, y =aumento , color = aumento > 10.1))# 
# todos em bairros da cidade de RiodoSUl

ggplot(data =df, aes(Localidade, aumento)) + geom_text(label = df$Cidade_Bairro)+
geom_point(mapping = aes(x=Localidade, y =aumento , color = aumento > 10.1))# 


# regressaõo logística binária para aumento maior que o Brasil
# 19 casos , interessante

bra1 <- glm(aumento_maiorqBra ~ Localidade,
             data = df, family=binomial(link=logit))

tab_model(bra1)# estar no Alto Vale(e não em bairros de Rio do Sul), aumenta em 0,16 % a chance de ter aumentado mais que no Brasil


bra2 <- glm(aumento_maiorqBra ~ Localidade + Amoedo + Ciro,
            data = df, family=binomial(link=logit))

tab_model(bra2)
# olha q interessante, Ciro é positivamente correlacionado com estar maior que o brasil

bra3 <- glm(aumento_maiorqBra ~ Localidade + Amoedo + Ciro + nivel_apoio_turno1,
            data = df, family=binomial(link=logit))

tab_model(bra3)# nao melhora muito

#Esses modelos dizem pouco, não usar


boxfinal <- ggplot(df, aes(aumento,nivel_apoio_turno1))
boxfinal + geom_boxplot()







