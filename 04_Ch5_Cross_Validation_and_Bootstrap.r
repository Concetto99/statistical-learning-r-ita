###########################################################
#### [223] 5.3 Lab: Cross-Validation and the Bootstrap ####
###########################################################

remove(list = ls())

# install.packages("boot")

# Attraverso il comando library() carichiamo il pacchetto boot,
# il quale contiene le funzione utili per il bootstrap
library(boot)

# Attraverso il comando library() carichiamo il pacchetto ISLR,
# il quale contiene vari dataset e funzioni utili.
library(ISLR)


#############################
## Validation set Approach ##
#############################


# L'uso di set.seed(), definizione del seme, è dovuto all'utilizzo
# in seguito della funzione sample(), la quale estrae in maniera
# pseudo casuale dei valori da un set di dati
set.seed(1)

# attraverso la funzione sample, assegniamo all'oggeto train un vettore
# di lunghezza 196 contenente i numeri interi da 1 a 392 estratti
# casualmente senza reimmissione
?sample
train <- sample(392, 196)
train

# Altri esempi:

# attraverso la funzione sample, riporto in output un vettore
# di lunghezza 20 contenente i numeri interi da 1 a 20 estratti
# casualmente senza reimmissione
sample(20, replace = FALSE)
#  4  7  1  2 13 19 11 17 14  3 18  5  9 16  6 15 12 10 20  8
table(sample(20, replace = FALSE)) # Default
#  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
#  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1

# attraverso la funzione sample, riporto in output un vettore
# di lunghezza 20 contenente i numeri interi da 1 a 20 estratti
# casualmente con reimmissione
sample(20, replace = TRUE)
#  4  7  1  2 11 14 18 19  1 10 14 10  7  9 15  5  9 14  5  5
table(sample(20, replace = TRUE))
#  1  2  4  5  7  9 10 11 14 15 18 19
#  2  1  1  3  2  2  2  1  3  1  1  1

# attraverso la funzione sample, riporto in output un vettore
# di lunghezza 5 contenente i numeri interi da 1 a 20 estratti
# casualmente senza reimmissione
sample(20, 5, replace = FALSE)
#  4  7  1  2 13
table(sample(20, 5, replace = FALSE))
#  1  2  4  7 13
#  1  1  1  1  1

# si assegna all'oggetto lm.fit l'output della funzione lm(), attraverso
# la quale si adatta un modello di regressione lineare ai dati di train
# in cui la variabile di risposta è mpg, la viabile dipendente è horsepower
# entrambre del dataser Auto, si utilizzano per il modello esclusivamente
# le righe di mpg e horsepower i cui indici corrispondono ai valori di train
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
horsepower[1:10]

# attraverso il comando Attach rendiamo direttamente accessibili le colonne
# del dataframe Auto all'interno del global environment
attach(Auto)

# Attraverso la funzione mean applicata ad un vettore creato "on the fly"
# dove ogni elemento corrisponte alla differenza tra l'i-esimo elemento
# di mpg e l'i-esimo elemento del vettore delle previsioni dei dati
# di Auto utilizzando lm.fit al quadrato. Gli elementi, sia di mpg che di
# predict(lm.fit, Auto), selezionati saranno solamente tutti gli elementi
# dei due vettori eccetto gli indici corrispondenti ai valori contenuti
# all'interno dell'oggetto train.
mean((mpg - predict(lm.fit, Auto))[-train]^2) # 23.26601
# Nella pratica questo valore corrisponde allo scarto quadratico medio
# tra i valori osservati e i valori previsti di test, o per meglio
# dire l'MSE di test

# Si assegna all'oggetto lm.fit2 l'output del modello di regressione
# in cui la variabile dipendente è mpg, e come regressori vengono
# utilizzati horsepower e horsepower^2 opportunamente trasformate
# in variabili ortogonali tra loro tramite la funzione poly().
# Le variabili si trovano all'interno del data frame Auto e vengono
# selezionate solamente le righe i cui indici corrispondono agli
# elementi del vettore train.
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
poly(horsepower, 2)[1:10,]

# Ricalcoliamo il MSE di test
mean((mpg - predict(lm.fit2, Auto))[-train]^2) # 18.71646
# L'aggiunta della trasformazione al quadrato della variabile horsepower
# ha un effetto di riduzione per l'MSE di test. La variabile mpg può
# essere spiegata non solo da horsepower ma l'aggiunta della stessa variabile
# al quadrato può cogliere dell'informazione aggiuntiva rispetto alla
# compomente lineare

# lm.fit3 rappresenta un modello di regressione polinomiale del terzo grado,
# dove la variabile di risposta è mpg e la variabile esplicativa è horsepower,
# trasformata tramite la funzione poly(horsepower, 3), che restituisce tre
# componenti ortogonali (potenze fino al cubo).
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
poly(horsepower, 3)[1:10,]

# Ricalcoliamo il MSE di test
mean((mpg - predict(lm.fit3, Auto))[-train]^2) # 18.79401
# L’aggiunta della componente cubica non ha migliorato l’errore rispetto
# al secondo grado. Anzi, l’MSE è leggermente aumentato, il che ci porta
# a pensare che l’aggiunta di complessità non porta un guadagno in capacità
# predittiva, e può addirittura peggiorare la performance per overfitting.



##############################
## LOOCV - Leave One Out CV ##
##############################

# Assegniamo all'oggetto glm.fit l'output della funzione glm() attraverso la
# quale effettuiamo un modello di regressione lineare in cui la variabile di
# risposta è mpg e la variabile indipendente è horsepower.
# Di default la funzione glm() ha una family=gaussian, ciò ci permette di
# effettuare la stessa regressione ottenibile utilizzando la funzione lm()
?glm
glm.fit <- glm(mpg ~ horsepower, data = Auto)

# Attraverso il comando coef() applicato ad un oggetto di classe glm otteniamo
# in output i coefficienti del modello di regressione lineare
coef(glm.fit)

# Riproduciamo lo stesso output utilizzando lm()
lm.fit <- lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)

# I comandi utilizzati sopra permettono di effettuare la
# stessa analisi, ma con l'oggetto lm.fit non è poi possibile
# utilizzare la funzione cv.glm()

# Definiamo un seme per garantire la riproducibilità dei risultati
set.seed(123)

# assegniamo all'oggetto cv.err l'output della funzione cv.glm() attraverso
# la quale calcoliamo il cross-validation prediction error, suddividendo il
# dataset in k gruppi, stimando il modello per tutte eccetto il k-esimo gruppo
# e su questo calcolando l'errore di previsione tra i fitted values e le
# prevsioni ottenute adattando il modello ai dati lasciati fuori.
# Se non si specifica il parametro k, questo effettuerà una LOOCV dunque
# lascerà per ogni ciclo una osservazione fuori e su questa calcolerà
# l'errore di previsione commesso.
?cv.glm
cv.err <- cv.glm(Auto, glm.fit)

# Attraverso il comando names() riportiamo in output gli oggetti contenuti
# all'interno di cv.err, ovvero:
# - La chiamata alla funzione
# - Il numero di k gruppi in cui suddividiamo i nostri dati
# - I valori del cross validation prediction error, nello specifico il primo
# valore è il cross validation prediction error, mentre il secondo viene
# aggiustato
# - infine seed, ovvero il valore dei semi causali quando viene richiamata
# la funzione cv.glm
names(cv.err)
# "call"  "K"     "delta" "seed"

# Non avendo specificato il parametro k questo è 392, ovvero la dimensione
# del dataset utilizzato
cv.err$K # 392

# il primo valore è il cross validation prediction error, mentre il
# secondo viene aggiustato per compensare il bias introdotto se non usiamo
# leave-one-out cross-validation
cv.err$delta # 24.23151 24.23114

# semi causali quando viene richiamata la funzione cv.glm
cv.err$seed

# 
cv.error <- rep(0, 10)

#
for (i in 1:10) {
 glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
 cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error




#############################
## k-Fold Cross Validation ##
#############################

set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
 glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
 cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10



###############
## Bootstrap ##
###############

alpha.fn <- function(data, index) {
 X <- data$X[index]
 Y <- data$Y[index]
 (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
}

alpha.fn(Portfolio, 1:100)

set.seed(7)
alpha.fn(Portfolio, sample(100, 100, replace = T))

boot(Portfolio, alpha.fn, R = 1000)

boot.fn <- function(data, index)
 coef(lm(mpg ~ horsepower, data = data, subset = index))

boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))

boot(Auto, boot.fn, 1000)

summary(lm(mpg ~ horsepower, data = Auto))$coef

boot.fn <- function(data, index)
 coef(
 lm(mpg ~ horsepower + I(horsepower^2),
 data = data, subset = index)
 )
set.seed(1)
boot(Auto, boot.fn, 1000)

summary(
 lm(mpg ~ horsepower + I(horsepower^2), data = Auto)
 )$coef

