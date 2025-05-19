##############################################
#### [182] 4.7 Lab: Classifcation Methods ####
##############################################

remove(list = ls())

# Attraverso il comando library() carichiamo il pacchetto ISLR2,
# il quale contiene vari dataset e funzioni utili.
library(ISLR2)

# Attraverso il comando names() riportiamo in output i nomi delle
# colonne del data frame Smarket
names(Smarket)
# "Year" "Lag1" "Lag2" "Lag3" "Lag4" "Lag5" "Volume" "Today" "Direction"

# Il comando dim() restituisce le dimendioni del dataset Smarket
dim(Smarket)

# il comando summary applicato ad un oggetto di tipo data frame restituisce
# le statistiche più importanti per le variabili contenuto al suo interno.
# Per le variabili di tipo quantitativo min, max, quanrtili. Per le variabili
# qualitative le frequenze assolute di classe
summary(Smarket)

# il comando pairs() restituisce una matrice di scatterplot tra tutte le
# variabili del dataframe Smarket
pairs(Smarket)

cor(Smarket)

cor(Smarket[, -9])


attach(Smarket)

plot(Volume)

# Regressione logistica
glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)

summary(glm.fits)

coef(glm.fits)

summary(glm.fits)$coef

summary(glm.fits)$coef[, 4]

glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]

contrasts(Direction)

glm.pred <- rep("Down", 1250)

glm.pred[glm.probs > .5] = "Up"