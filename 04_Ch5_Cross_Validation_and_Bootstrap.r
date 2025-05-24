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

# Assegniamo all'oggetto cv.error il vettore di lunghezza 10 avente 0
# come elementi
cv.error <- rep(0, 10)
cv.error

# Attraverso un ciclo for per i che va da 1 a 10 eseguiamo le
# istruzioni seguenti
# - Assegniamo all'oggetto glm.fit l'output di un modello lineare
# generalizzato con family = Gaussian (default), che ci permette di
# effettuare una regressione lineare polinomiale utilizzando il dataframe
# Auto in cui mpg è la variabile di risposta, mentre come regressori del
# modello vi sono tutte le variabili dal grado 1 a i per horsepower,
# ortogonali tra loro.
# - Updatiamo l'i-esimo elemente del vettore cv.error assegnando il
# primo elemento del oggetto delta della funzione cv.glm, attraverso la
# quale effettuiamo una LOOCV utilizzando glm.fit
for (i in 1:10) {
 glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
 cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}

cv.error
# 24.23151 19.24821 19.33498 19.42443 19.03321 18.97864 18.83305 18.96115 19.06863 19.49093

# Come possiamo notare dal grafico la spezzata scende rapidamente
# quando passiamo dall'ultilizzare horsepower e il polinomio di grado 2
# mentre vi è un minimo utilizzando un polinomio di grado 7, il prezzo da
# pagare è però una maggiore flessibilità, dunque perdita in interpretabilità
plot(cv.error)
lines(cv.error)



#############################
## k-Fold Cross Validation ##
#############################

# Definiamo un seme per garantire la riproducibilità dei risultati
set.seed(17)

# Assegniamo all'oggetto cv.error il vettore di lunghezza 10 avente 0
# come elementi
cv.error.10 <- rep(0, 10)

# Attraverso un ciclo for per i che va da 1 a 10 eseguiamo le
# istruzioni seguenti
# - Assegniamo all'oggetto glm.fit l'output di un modello lineare
# generalizzato con family = Gaussian (default), che ci permette di
# effettuare una regressione lineare polinomiale utilizzando il dataframe
# Auto in cui mpg è la variabile di risposta, mentre come regressori del
# modello vi sono tutte le variabili dal grado 1 a i per horsepower,
# ortogonali tra loro.
# - Updatiamo l'i-esimo elemente del vettore cv.error assegnando il
# primo elemento del oggetto delta della funzione cv.glm, attraverso la
# quale effettuiamo una 10-Fold Cross-Validation utilizzando glm.fit
for (i in 1:10) {
 glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
 cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

cv.error.10
# 24.20629 19.11172 19.23365 19.57416 19.53450 19.10526 18.88978 18.82748 19.07514 19.35349

plot(cv.error.10)
lines(cv.error.10)

# Confronto LOOCV vs. 10-Fold

# Creo la matrice avente come colonne i due vettore di cv.error e cv.error.10
cbind(cv.error,cv.error.10)

# Grafico per il confronto
plot(cbind(1:10, 1:10), cbind(cv.error, cv.error.10))
lines(cv.error, col="blue")
lines(cv.error.10, col="red")

# Come possiamo notare le linee seguono lo stesso andamento,
# con piccole variazioni per i CV predicition error ottenuti



###############
## Bootstrap ##
###############


head(Portfolio)
#            X          Y
# 1 -0.8952509 -0.2349235
# 2 -1.5624543 -0.8851760
# 3 -0.4170899  0.2718880
# 4  1.0443557 -0.7341975
# 5 -0.3155684  0.8419834
# 6 -1.7371238 -2.0371910

summary(Portfolio)
#       X                  Y
# Min.   :-2.43276   Min.   :-2.72528
# 1st Qu.:-0.88847   1st Qu.:-0.88572
# Median :-0.26889   Median :-0.22871
# Mean   :-0.07713   Mean   :-0.09694
# 3rd Qu.: 0.55809   3rd Qu.: 0.80671
# Max.   : 2.46034   Max.   : 2.56599

# si assegna all'oggetto alpha.fn una funzione avente due parametri,
# data e index. Una volta richiamata questa eseguirà le seguenti
# istruzioni:
# - Assegna all'ogetto X la variabile X del dataset passato come
# parametro data, selezionando solo gli indici contenuti nel
# vettore passato come parametro index
# - Assegna all'ogetto Y la variabile y del dataset passato come
# parametro data, selezionando solo gli indici contenuti nel
# vettore passato come parametro index
# - Riporta in output il risultato del comando
# (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y)) attraverso
# il quale è il rapporto tra la varianza di X meno la covarianza
# tra X e Y (al numeratore) su la somma della varianza di X, la
# varianza di Y e 2 volte la covarianza tra X e Y (al denominatore)
alpha.fn <- function(data, index) {
 X <- data$X[index]
 Y <- data$Y[index]
 (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
}

# Attraverso la funzione, precedentemente creata, alpha.fn()
# alla quale passiamo come parametri data = Portfolio e index =
# 1:100 (vettore di interi da 1 a 100), riportiamo in output
# il risultato della formula precedentemente citata
alpha.fn(Portfolio, 1:100) # 0.5758321

# Definiamo un seme per garantire la riproducibilità dei risultati
set.seed(7)

# Attraverso la funzione, precedentemente creata, alpha.fn()
# alla quale passiamo come parametri data = Portfolio e index =
# sample(100, 100, replace = T) (vettore di lunghezza 100 contenente
# i numeri interi da 1 a 100 estratti casualmente con ripetizione,
# attraverso la funzione sample()), riportiamo in output
# il risultato della formula precedentemente citata
alpha.fn(Portfolio, sample(100, 100, replace = T))

# Attraverso la funzione boot() contenuta nell'omonimo pacchetto,
# è possibile ottenere in output gli elementi utili per stimare
# la variabilità (cioè l'incertezza) della statistica alpha,
# calcolata sulla base di campioni bootstrap.
# In particolare, la funzione boot() esegue R = 1000 campionamenti
# casuali con ripetizione (bootstrap) dalle righe del dataset Portfolio.
# Per ciascun campione, viene ricalcolata la statistica alpha utilizzando
# la funzione alpha.fn (definita precedentemente).
# Il risultato è un oggetto che contiene:
# - la stima originale di alpha t0 (calcolata sull'intero dataset),
# - il vettore delle 1000 stime bootstrap di alpha (t),
# - e informazioni utili per stimare la sua deviazione standard,
#   costruire intervalli di confidenza o valutare la sua stabilità.
boot(Portfolio, alpha.fn, R = 1000)
# ORDINARY NONPARAMETRIC BOOTSTRAP
#
# Call:
# boot(data = Portfolio, statistic = alpha.fn, R = 1000)
#
# Bootstrap Statistics :
#      original      bias    std. error
# t1* 0.5758321 0.001767625  0.08988222

names(boot(Portfolio, alpha.fn, R = 1000))
# [1] "t0"        "t"         "R"         "data"      "seed"      "statistic"
# [7] "sim"       "call"      "stype"     "strata"    "weights"

# richiamando l'oggetto t contenuto nell'ouput della funzione boot()
# ci restituisce le 1000 stime della funzione alpha ottenuta utilizzando
# i dati dei 1000 campioni bootstrap
boot(Portfolio, alpha.fn, R = 1000)$t

# Definiamo una nuova funzione boot.fn che prende due argomenti:
# - data: un data frame, in questo caso presumibilmente il dataset Auto,
# - index: un vettore di indici da utilizzare per selezionare le righe del dataset.
#
# All'interno della funzione, si stima un modello di regressione lineare
# in cui mpg è la variabile risposta e horsepower è la variabile esplicativa.
# Vengono considerate solo le osservazioni i cui indici sono specificati
# nel vettore index.
# La funzione restituisce i coefficienti stimati del modello.
boot.fn <- function(data, index)
 coef(lm(mpg ~ horsepower, data = data, subset = index))

# Eseguiamo la funzione boot.fn passando:
# - il dataset Auto,
# - il vettore di indici da 1 a 392 (ovvero tutte le osservazioni),
# ottenendo così i coefficienti di regressione stimati utilizzando
# l’intero dataset senza campionamento.
boot.fn(Auto, 1:392)  # Output: (Intercept) e coefficiente di horsepower
# (Intercept)  horsepower
#  39.9358610  -0.1578447

# Fissiamo il seme per rendere riproducibili i risultati del bootstrap.
set.seed(1)

# Eseguiamo boot.fn con:
# - il dataset Auto,
# - un campione bootstrap di 392 osservazioni estratte con ripetizione.
# Questo simula una singola ripetizione della procedura bootstrap,
# stimando i coefficienti della regressione lineare su un campione
# casuale (con ripetizione) dello stesso dataset.
boot.fn(Auto, sample(392, 392, replace = T))
# (Intercept)  horsepower
#  40.6952857  -0.1625602

# Applichiamo la funzione boot() al dataset Auto con la funzione boot.fn
# (già definita in precedenza per stimare il modello lineare mpg ~ horsepower)
# e ripetiamo il processo di bootstrap R = 1000 volte.
# In questo modo otteniamo una stima dell'incertezza associata ai coefficienti
# del modello di regressione lineare semplice tramite campionamento bootstrap.
boot(Auto, boot.fn, 1000)

# Otteniamo i coefficienti stimati del modello di regressione lineare
# mpg ~ horsepower, calcolati sull'intero dataset Auto.
# La funzione summary() restituisce un oggetto complesso,
# dal quale estraiamo solo la matrice dei coefficienti (con stime,
# errori standard, t-statistiche e p-value) tramite $coef.
summary(lm(mpg ~ horsepower, data = Auto))$coef
#               Estimate  Std. Error   t value      Pr(>|t|)
# (Intercept) 39.9358610 0.717498656  55.65984 1.220362e-187
# horsepower  -0.1578447 0.006445501 -24.48914  7.031989e-81

# Ridefiniamo la funzione boot.fn per adattare un modello di regressione
# lineare in cui mpg è la variabile risposta e i predittori sono:
# horsepower e horsepower^2.
# La funzione I(horsepower^2) consente di includere horsepower al quadrato
# come termine esplicativo esplicito nel modello (senza usare poly()).
# Come prima, il parametro index permette di selezionare le righe del dataset
# da utilizzare per ciascun campione bootstrap.
boot.fn <- function(data, index)
 coef(lm(mpg ~ horsepower + I(horsepower^2), data = data, subset = index))

# Fissiamo il seme del generatore di numeri casuali per assicurare
# la replicabilità del bootstrap.
set.seed(1)

# Applichiamo la funzione boot() con la nuova versione di boot.fn,
# stimando tramite bootstrap (R = 1000 ripetizioni) i coefficienti del
# modello quadratico mpg ~ horsepower + horsepower^2, usando campioni
# con ripetizione estratti dal dataset Auto.
# L'obiettivo è ottenere una stima empirica della variabilità
# dei coefficienti del modello non lineare.
boot(Auto, boot.fn, 1000)
# Call:
# boot(data = Auto, statistic = boot.fn, R = 1000)
#
# Bootstrap Statistics :
#       original        bias    std. error
# t1* 39.9358610  0.0148776968 0.854981056
# t2* -0.1578447 -0.0002877407 0.007372505

# Estraiamo la matrice dei coefficienti stimati dal modello quadratico
# mpg ~ horsepower + horsepower^2 calcolato sull'intero dataset Auto.
# summary() restituisce un oggetto con varie informazioni, da cui
# selezioniamo solo i coefficienti tramite $coef.
# La tabella risultante include:
# - le stime dei coefficienti (intercetta, horsepower e horsepower^2),
# - gli errori standard,
# - le t-statistiche,
# - i p-value associati ai test di significatività.
summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef
#                     Estimate   Std. Error   t value      Pr(>|t|)
# (Intercept)     56.900099702 1.8004268063  31.60367 1.740911e-109
# horsepower      -0.466189630 0.0311246171 -14.97816  2.289429e-40
# I(horsepower^2)  0.001230536 0.0001220759  10.08009  2.196340e-21