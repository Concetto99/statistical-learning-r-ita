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
# modello è medv, mentre la variabile dipendente è lstat, contenute nel
# dataset Boston
?lm()
lm.fit <- lm(medv ~ lstat, data = Boston)
(lm.fit)

# Attraverso la funzione summary applicata ad un oggetto di classe lm ci
# restituisce l'output di un modello di regressione lineare, in cui possiamo
# i principali attributi, come il min, max, 1,2,3 quartile dei resudui, i
# coefficienti del modello con std. error ecc. e altre metriche come R quadro
summary(lm.fit)