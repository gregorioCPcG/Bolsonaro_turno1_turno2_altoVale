library(readxl)
df <- read_excel("amoedo e Bolsonaro _4.xlsx")

summary(df) # rsl e numerico 1 bairro e zero cidade, RiodoSUl é character
# bolsonaro e biarros x cidades
cor.test(df$Bolsonaro, df$rsl)
library(polycor) #pacote exigido
polyserial(df$Bolsonaro, df$rsl)
# amoedo
cor.test(df$Amoedo2018, df$rsl)
polyserial(df$Amoedo2018, df$rsl)# menos q amoedo
# então ta de boa


# Bolsonaro segundo turno e Amoedo primeiro turno

cor.test(df$Amoedo2018, df$Bolsonaro)
# sig mais baixa

library(ggplot2)
grafic <- ggplot(df, aes(Bolsonaro, Amoedo2018))

g <- grafic + geom_point()
g
g + geom_smooth(model=lm, se=FALSE)


grafic+ geom_text(label = df$localidade)
g + geom_text(label = df$localidade)
grafic + geom_text(label = df$localidade) + geom_smooth(model=lm, se=FALSE)

df$`Bairro de Rio do Sul?` <- df$RioDoSul
g2 <- g +  geom_smooth(model=lm, se=FALSE)
g2 + geom_point(aes(colour= df$`Bairro de Rio do Sul?`))

grafic + geom_text(label = df$localidade) +
  geom_smooth(model=lm, se=FALSE) +
  geom_point(aes(colour= df$`Bairro de Rio do Sul?`))

model <- lm(Bolsonaro ~ Amoedo2018, data=df)
summary(model)

library(coefplot)
coefplot(model, intercept=TRUE, interactive=TRUE)

coefplot(model, intercept=FALSE, interactive=TRUE)


model2 <- lm(Bolsonaro ~ Amoedo2018 + RioDoSul + Amoedo2018*RioDoSul, data=df)
summary(model2)


# residuos
fit <- model
df$predicted <- predict(fit)   # Save the predicted values
df$residuals <- residuals(fit) # Save the residual values


cor(df$Bolsonaro, df$predicted)
cor(df$Bolsonaro, df$residuals)


ggplot(df, aes(x = Amoedo2018, y = Bolsonaro)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = Amoedo2018, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()


ggplot(df, aes(x = Amoedo2018, y = Bolsonaro)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = Amoedo2018, yend = predicted), alpha = .2) + 
  geom_text(label=df$localidade)+      
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()




# interflex

library(interflex)
data(interflex)
ls()
Y = df$Bolsonaro
D = df$RioDoSul
X = df$Amoedo2018
df2 <- data.frame(Y,D,X)
s1<-cbind.data.frame(Y = Y, D = D, X = X, Z1 = df$predicted)
a <- interflex::interflex(estimator = "raw",Y = "Y", D = "D", X = "X",
          data = s1, weights = NULL, Ylabel = "Bolsonaro segundo turno",
          Dlabel = " Bairro de Rio do Sul?", Xlabel="Amoedo 2018",
          main = "Cidade Alto Vale/Bairro Rio do Sul 
          na relação voto Amoedo/Bolsonaro
          ", cex.main = 0.7, ncols=2)
plot(a)

out <- interflex(Y = "Y", D = "D", X = "X", data = s1, estimator = "kernel",
                 nboots = 200, parallel = TRUE, cores = 4)

out$figure

out2 <- interflex(Y = "Y", D = "D", X = "X", data = s1, estimator = "linear",
                  vcov.type = "robust", main = "Marginal Effects",
                  ylim = c(-15, 15))
plot(out2)



out <- interflex(Y = "Y", D = "D", X = "X", data = s1, estimator = "binning",
                 vcov.type = "robust", main = "Marginal Effects",
                 ylim = c(-15, 15))

plot(out)
plot(out, xlab = "Moderate is X", Xdistr = "none", bin.labs = FALSE, cex.axis = 0.8, cex.lab = 0.8)
