#######################################
#### [364] 8.3 Lab: Decision Trees ####
#######################################

# Creazione directory per salvare i grafici

if (!dir.exists("img")) {
  dir.create("img")
}

if (!dir.exists("img/07_Ch8_Tree_Based_Methods")) {
  dir.create("img/07_Ch8_Tree_Based_Methods")
}

remove(list = ls())

img_path = "img/07_Ch8_Tree_Based_Methods" # nolint: assignment_linter.

# Attraverso il comando library() carichiamo il pacchetto ISLR,
# il quale contiene vari dataset e funzioni utili.
library(ISLR)

# Attraverso il comando library() carichiamo il pacchetto tree il quale
# contiene le funzioni utili per implementare degli alberi decisionali
# install.packages("tree")
library(tree)

# Attraverso il comando attach() rendiamo direttamente disponibili
# le variabili del dataset Carseats all'interno del nostro global
# environment
attach(Carseats)

# Si assegna all'oggeto High il vettore appositamente definito
# con classe factor tramite l'apposita funzione, il quale contiene come
# elementi la stringa "No" se l'i-esimo elemento di Sales è minore o
# uguale a 8 o la stringa "Yes" alternativamente
High <- factor(ifelse(Sales <= 8, "No", "Yes"))

# Si assegna all'oggeto Carseats il dataframe contenente le variabili
# di Carseats e la variabile High precedentemente creata
Carseats <- data.frame(Carseats, High)


#########################
## Classifcation Trees ##
#########################

?tree

# Si assegna all'oggetto tree.carseats l'output della funzione tree()
# alla quale viene passata la formula contenente High come variabile
# di risposta e tutte le altre colonne di Carseats eccetto High e Sales
# come variabili indipendete. La funzione tree ci permette di implementare
# un albero di classificazione in quanto la variabile High è qualitativa
# dicotomica
tree.carseats <- tree(High ~ . - Sales, Carseats)
tree.carseats
# node), split, n, deviance, yval, (yprob)
#       * denotes terminal node
# 
#   1) root 400 541.500 No ( 0.59000 0.41000 )  
#     2) ShelveLoc: Bad,Medium 315 390.600 No ( 0.68889 0.31111 )
#       4) Price < 92.5 46  56.530 Yes ( 0.30435 0.69565 )
#         8) Income < 57 10  12.220 No ( 0.70000 0.30000 )  
#          16) CompPrice < 110.5 5   0.000 No ( 1.00000 0.00000 ) *
#          17) CompPrice > 110.5 5   6.730 Yes ( 0.40000 0.60000 ) *
#         9) Income > 57 36  35.470 Yes ( 0.19444 0.80556 )  
#          18) Population < 207.5 16  21.170 Yes ( 0.37500 0.62500 ) *
#          19) Population > 207.5 20   7.941 Yes ( 0.05000 0.95000 ) *
#       5) Price > 92.5 269 299.800 No ( 0.75465 0.24535 )
#        10) Advertising < 13.5 224 213.200 No ( 0.81696 0.18304 )
#          20) CompPrice < 124.5 96  44.890 No ( 0.93750 0.06250 )
#            40) Price < 106.5 38  33.150 No ( 0.84211 0.15789 )
#              80) Population < 177 12  16.300 No ( 0.58333 0.41667 )  
#               160) Income < 60.5 6   0.000 No ( 1.00000 0.00000 ) *
#               161) Income > 60.5 6   5.407 Yes ( 0.16667 0.83333 ) *
#              81) Population > 177 26   8.477 No ( 0.96154 0.03846 ) *
#            41) Price > 106.5 58   0.000 No ( 1.00000 0.00000 ) *
#          21) CompPrice > 124.5 128 150.200 No ( 0.72656 0.27344 )
#            42) Price < 122.5 51  70.680 Yes ( 0.49020 0.50980 )
#              84) ShelveLoc: Bad 11   6.702 No ( 0.90909 0.09091 ) *
#              85) ShelveLoc: Medium 40  52.930 Yes ( 0.37500 0.62500 )  
#               170) Price < 109.5 16   7.481 Yes ( 0.06250 0.93750 ) *
#               171) Price > 109.5 24  32.600 No ( 0.58333 0.41667 )  
#                 342) Age < 49.5 13  16.050 Yes ( 0.30769 0.69231 ) *
#                 343) Age > 49.5 11   6.702 No ( 0.90909 0.09091 ) *
#            43) Price > 122.5 77  55.540 No ( 0.88312 0.11688 )
#              86) CompPrice < 147.5 58  17.400 No ( 0.96552 0.03448 ) *
#              87) CompPrice > 147.5 19  25.010 No ( 0.63158 0.36842 )
#               174) Price < 147 12  16.300 Yes ( 0.41667 0.58333 )
#                 348) CompPrice < 152.5 7   5.742 Yes ( 0.14286 0.85714 ) *
#                 349) CompPrice > 152.5 5   5.004 No ( 0.80000 0.20000 ) *
#               175) Price > 147 7   0.000 No ( 1.00000 0.00000 ) *
#        11) Advertising > 13.5 45  61.830 Yes ( 0.44444 0.55556 )  
#          22) Age < 54.5 25  25.020 Yes ( 0.20000 0.80000 )
#            44) CompPrice < 130.5 14  18.250 Yes ( 0.35714 0.64286 )  
#              88) Income < 100 9  12.370 No ( 0.55556 0.44444 ) *
#              89) Income > 100 5   0.000 Yes ( 0.00000 1.00000 ) *
#            45) CompPrice > 130.5 11   0.000 Yes ( 0.00000 1.00000 ) *
#          23) Age > 54.5 20  22.490 No ( 0.75000 0.25000 )
#            46) CompPrice < 122.5 10   0.000 No ( 1.00000 0.00000 ) *
#            47) CompPrice > 122.5 10  13.860 No ( 0.50000 0.50000 )
#              94) Price < 125 5   0.000 Yes ( 0.00000 1.00000 ) *
#              95) Price > 125 5   0.000 No ( 1.00000 0.00000 ) *
#     3) ShelveLoc: Good 85  90.330 Yes ( 0.22353 0.77647 )
#       6) Price < 135 68  49.260 Yes ( 0.11765 0.88235 )
#        12) US: No 17  22.070 Yes ( 0.35294 0.64706 )
#          24) Price < 109 8   0.000 Yes ( 0.00000 1.00000 ) *
#          25) Price > 109 9  11.460 No ( 0.66667 0.33333 ) *
#        13) US: Yes 51  16.880 Yes ( 0.03922 0.96078 ) *
#       7) Price > 135 17  22.070 No ( 0.64706 0.35294 )
#        14) Income < 46 6   0.000 No ( 1.00000 0.00000 ) *
#        15) Income > 46 11  15.160 Yes ( 0.45455 0.54545 ) *

# Attraverso la funzione summary applicata an un oggetto di classe tree
# stampiamo l'output di un albero di classificazione
summary(tree.carseats)
# Classification tree:
# tree(formula = High ~ . - Sales, data = Carseats)
# Variables actually used in tree construction:
# [1] "ShelveLoc"   "Price"       "Income"      "CompPrice"   "Population"
# [6] "Advertising" "Age"         "US"
# Number of terminal nodes:  27
# Residual mean deviance:  0.4575 = 170.7 / 373
# Misclassification error rate: 0.09 = 36 / 400

# Per salvare il grafico
png(paste(img_path, "/01_Classification_Tree.png", sep=""), width = 800, height = 600)


# Attraverso il comando plot() visualizziamo la struttura dell'albero
# decisionale ottenuto, attravesrso il comando text() aggiungiamo al
# grafico le etichette in ogni nodo
plot(tree.carseats)
text(tree.carseats, pretty = 0)
dev.off()

# Definiamo il seme tramite la funzione set.seed() per la riproducibilità
# in quanto utilizziamo la funzione sample() in seguito
set.seed(2)

# Assegniamo all'oggetto train un vettore di lunghezza 200, i
# cui elementi sono estratti in modo pseudo casuale e senza ripetizione
# da un vettore contenente tutti i numeri interi da 1 al numero di
# righe di Carseats.
train <- sample(1:nrow(Carseats), 200) # nolint: seq_linter.

# Si assegna all'oggetto Carseats.test tutte le colonne del dataframe
# Carseats ma ad esclusione delle righe i cui indici sono contenuti
# come elementi di train
Carseats.test <- Carseats[-train, ]

# Si assegna all'oggetto High.test il vettore avente gli elementi
# di High ad eccezione degli elementi i cui indici sono contenuti
# come elementi del vettore train
High.test <- High[-train]

# Si assegna all'oggetto tree.carseats l'output della funzione tree
# alla quale viene passata la formula contenente High come variabile
# di risposta e tutte le altre colonne di Carseats eccetto High e Sales
# come variabili indipendenti. Per l'implementazione vengono selezionate le
# sole righe per cui l'indice è presente all'interno del vettore train
# La funzione tree ci permette di implementare un albero di classificazione
# in quanto la variabile High è qualitativa dicotomica
tree.carseats <- tree(High ~ . - Sales, Carseats, subset = train)

# Si assegna alla funzione tree.pred il vettore delle previsioni per la
# variabile High sulle nuove osservazioni contenute in Carseats.test usando
# il modello salvato in tree.carseats. Il parametro passato alla funzione
# type = "class" ci permette di farci restituire la previsione con la classe
# prevista (quindi "Yes" oppure "No")
tree.pred <- predict(tree.carseats, Carseats.test, type = "class")


# Attraverso la funzione table applicata ai vettore tree.pred contenente le
# previsioni e High.test contenente i valori osservati di test, riportiamo
# in output la tabella di contingenza
table(tree.pred, High.test)
#          High.test
# tree.pred  No Yes
#       No  104  33
#       Yes  13  50

set.seed(7)
?cv.tree

# Si assegna all'oggetto cv.carseats l'output della funzione cv.tree(), la quale
# ci permette di implementare una k-fold cross validation come funzione del
# parametro k e trovare il missclassification error al variare di k.
# Il parametro k è un parametro che governa il costo complessità del modello
# e ci permette quindi di valutare dove è possibile eseguire una potatura
# dell'albero
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)

names(cv.carseats)
# "size"   "dev"    "k"      "method"

# L'output di cv.carseats contiene le dimesioni dell'albero corrispondenti al
# valore di k associato in size, il valore della devianza (in questo caso) in
# $dev ma in generale contiene i valori degli errori della validazione incrociata,
# il valore in $k corrispondente al parametro alpha utilizzato per effettuare il
# tuning del modello, il metodo utilizzato per scegliere il miglior sottoalbero,
# ovvero il misclassfication error e la classe dell'oggetto
cv.carseats
# $size
# [1] 21 19 14  9  8  5  3  2  1
# 
# $dev
# [1] 71 70 66 66 70 74 76 87 88
# 
# $k
# [1] -Inf  0.0  1.0  1.4  2.0  3.0  4.0  9.0 18.0
# 
# $method
# [1] "misclass"
# 
# attr(,"class")
# [1] "prune"         "tree.sequence"

# Per salvare il grafico
png(paste(img_path, "/02_Tree_Pruning_measure.png", sep=""), width = 800, height = 600)

# Riportiamo in output 2 grafici utili alla scelta del sottoalbero.
# Il primo grafico ci permette di visualizzare la spezzata relativa all'errore
# di cross validazione per le diverse dimensioni dell'albero
# Il secondo grafico ci permette di visualizzare la spezzata relativa all'errore
# di cross validazione per i diversi valori del parametro k
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")
dev.off()

# Si assegna all'oggetto prune.carseats l'output della funzione prune.misclass
# in cui vengono passati come parametro l'oggetto tree.carseats assegnato in
# precedenza e la dimensione del'albero pari a 9 in modo tale da assegnare
# all'oggetto prune.carseats l'output del miglior albero potato di dimensione 9
prune.carseats <- prune.misclass(tree.carseats, best = 9)

# Per salvare il grafico
png(paste(img_path, "/02_Sub_Tree_Pruned.png", sep=""), width = 800, height = 600)

# Attraverso il comando plot() visualizziamo la struttura del miglio albero
# decisionale potato al nono split, attravesrso il comando text() aggiungiamo al
# grafico le etichette in ogni nodo
plot(prune.carseats)
text(prune.carseats, pretty = 0)
dev.off()

# Si assegna alla funzione tree.pred il vettore delle previsioni per la
# variabile High sulle nuove osservazioni contenute in Carseats.test usando
# il modello salvato in prune.carseats. Il parametro passato alla funzione
# type = "class" ci permette di farci restituire la previsione con la classe
# prevista (quindi "Yes" oppure "No")
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")

# Attraverso la funzione table applicata ai vettore tree.pred contenente le
# previsioni e High.test contenente i valori osservati di test, riportiamo
# in output la tabella di contingenza
table(tree.pred, High.test)
#          High.test
# tree.pred No Yes
#       No  97  25
#       Yes 20  58

(97+58) / (nrow(Carseats.test))
#  0.775

# Rifacciamo la stessa operazione di prima ma con il miglior sottoalbero
# di dimensione 14 per vedere se l'accuracy aumenta
prune.carseats <- prune.misclass(tree.carseats, best = 14)

plot(prune.carseats)
text(prune.carseats, pretty = 0)

tree.pred <- predict(prune.carseats, Carseats.test, type = "class")

table(tree.pred, High.test)
#          High.test
# tree.pred  No Yes
#       No  102  31
#       Yes  15  52

(102+52) / (nrow(Carseats.test))
#  0.77



######################
## Regression Trees ##
######################

library(MASS) # Contiene il dataset Boston
set.seed(1)

# Si assegna all'oggetto train un vettore di lunghezza pari alla metà del numero di
# righe del dataset Boston i cui elementi sono estratti in maniera pseudo casuale
# senza ripetizione dal vettore di numeri interi da 1 al numero di righe del dataset
# Boston
train <- sample(1:nrow(Boston), nrow(Boston) / 2) # nolint: seq_linter.

# Si assegna all’oggetto tree.boston l’output della funzione tree(), che
# permette di stimare un albero di regressione in cui:
# - la variabile di risposta è medv (valore mediano delle abitazioni)
# - le variabili esplicative sono tutte le restanti variabili del dataset
# - l'argomento subset = train indica che il modello viene stimato
#   solo sul sottoinsieme di dati indicato dal vettore train
tree.boston <- tree(medv ~ ., Boston, subset = train)

# La funzione summary applicata all'oggetto tree.boston fornisce un
# riassunto del modello ad albero stimato:
# include i dettagli di ciascuna variabile utilizzata per la suddivisione
# dei nodi, il numero di nodi terminali, la residual mean deviance (che in
# questo caso equivale al valore dell'RSS, somma degli errori al quadrato) e
# la distribuzione dei residui
summary(tree.boston)
# Regression tree:
# tree(formula = medv ~ ., data = Boston, subset = train)
# Variables actually used in tree construction:
# [1] "rm"    "lstat" "black" "dis"
# Number of terminal nodes:  8
# Residual mean deviance:  14.29 = 2743 / 192
# Distribution of residuals:
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# -18.54000  -2.16900   0.08461   0.00000   2.15900  15.14000

# Attraverso il comando plot() visualizziamo la struttura dell'albero
# decisionale ottenuto, attravesrso il comando text() aggiungiamo al
# grafico le etichette in ogni nodo, senza abbreviazioni.
plot(tree.boston)
text(tree.boston, pretty = 0)

# Assegniamo all'oggetto cv.boston l'output della funzione cv.tree attraverso
# la quale è possibile implementare 
cv.boston <- cv.tree(tree.boston)

#
plot(cv.boston$size, cv.boston$dev, type = "b")

#
prune.boston <- prune.tree(tree.boston, best = 5)

#
plot(prune.boston)
text(prune.boston, pretty = 0)

#
yhat <- predict(tree.boston, newdata = Boston[-train, ])

#
boston.test <- Boston[-train, "medv"]

#
plot(yhat, boston.test)
abline(0, 1)

#
mean((yhat - boston.test)^2)


#############
## Bagging ##
#############

# Se mtry = n.totale di variabili allora Bagging
# Se mtry < n.totale di variabili allora Random Forest
# ma il comando resta sempre randomForest()

#
library(randomForest)

#
set.seed(1)

#
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 12, importance = TRUE) 
bag.boston

#
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])

#
plot(yhat.bag, boston.test)

#
abline(0, 1)

#
mean((yhat.bag - boston.test)^2)

#
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 12, ntree = 25)

#
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])

#
mean((yhat.bag - boston.test)^2)


###################
## Random Forest ##
###################

#
set.seed(1)

#
rf.boston <- randomForest(medv ~ ., data = Boston,subset = train, mtry = 6, importance = TRUE)

#
yhat.rf <- predict(rf.boston, newdata = Boston[-train, ])

#
mean((yhat.rf - boston.test)^2)

#
importance(rf.boston)

#
varImpPlot(rf.boston)


##############
## Boosting ##
##############

#
library(gbm)

#
set.seed(1)

#
boost.boston <- gbm(medv ~ ., data = Boston[train, ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)

#
summary(boost.boston)

#
plot(boost.boston, i = "rm")

#
plot(boost.boston, i = "lstat")

#
yhat.boost <- predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)

#
mean((yhat.boost - boston.test)^2)

#
yhat.boost <- predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)

#
boost.boston <- gbm(medv ~ ., data = Boston[train, ], distribution = "gaussian", n.trees = 5000, minteraction.depth = 4, shrinkage = 0.2, verbose = F)

#
mean((yhat.boost - boston.test)^2)
