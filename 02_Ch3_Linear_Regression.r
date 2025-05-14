#####################################
#### [121] 3.6 Linear Regression ####
#####################################

remove(list = ls())

# Attraverso il comando library() carichiamo i pacchetti MASS e ISLR2.
# I due pacchetti contengono vari dataset e funzioni utili.
library(MASS)
library(ISLR2)

# Il dataset Boston è contenuto in ISRL2. Questo dataset contiene la mediana
# dei prezzi delle case in 506 zone dell’area metropolitana di Boston.
# Attraverso il comando head() vediamo il contenuto delle prime righe di Boston
head(Boston)

# Attraverso il comando lm() è possibile adattare un modello lineare ai dati.

# Si assegna all'oggetto lm.fit l'output della funzione lm() con la quale si
# adatta un modello di regressione lineare ai dati, La varabile di risposta del
# modello è medv, mentre la variabile indipendente o regressore è lstat.
# Entrambe le variabili sono contenute nel dataset Boston
?lm()
lm.fit <- lm(medv ~ lstat, data = Boston)
(lm.fit)

# Attraverso la funzione summary applicata ad un oggetto di classe lm ci
# restituisce l'output di un modello di regressione lineare, in cui possiamo
# i principali attributi, come il min, max, 1,2,3 quartile dei resudui, i
# coefficienti del modello con std. error ecc. e altre metriche come R quadro
summary(lm.fit)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Il modello sopra è del tipo:
#                               medv = beta0 + beta1 * lstat + epsilon

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept) 34.55384    0.56263   61.41   <2e-16 ***
# lstat       -0.95005    0.03873  -24.53   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 6.216 on 504 degrees of freedom
# Multiple R-squared:  0.5441,    Adjusted R-squared:  0.5432
# F-statistic: 601.6 on 1 and 504 DF,  p-value: < 2.2e-16

# beta0 = 34.6
# beta1 = -0.95

# All'aumentare dell'1% della popolazione con lower-status il
# valore mediano delle case si riduce in media di circa 950 dollari

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

names(lm.fit)

coef(lm.fit)

confint(lm.fit)

predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "confidence")

predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "prediction")

plot(lstat, medv)
abline(lm.fit)
abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = "red")


plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)

par(mfrow = c(2, 2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit) 

lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)

library(car) # per il vif
vif(lm.fit)

lm.fit1 <- lm(medv ~ . - age, data = Boston)
summary(lm.fit1)


summary(lm(medv ~ lstat * age, data = Boston))


lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)

anova(lm.fit, lm.fit2)
lm.fit <- lm(medv ~ lstat)

par(mfrow = c(2, 2))
plot(lm.fit2)

lm.fit5 <- lm(medv ~ poly(lstat, 5))
summary(lm.fit5)

summary(lm(medv ~ log(rm), data = Boston))

head(Carseats)

summary(lm.fit)
lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)

contrasts(Carseats$ShelveLoc)