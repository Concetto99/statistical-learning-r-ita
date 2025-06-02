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
png(paste(img_path, "/03_Sub_Tree_Pruned.png", sep=""), width = 800, height = 600)

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

# Per salvare il grafico
png(paste(img_path, "/04_Regression_Tree.png", sep=""), width = 800, height = 600)

# Attraverso il comando plot() visualizziamo la struttura dell'albero
# decisionale ottenuto, attravesrso il comando text() aggiungiamo al
# grafico le etichette in ogni nodo, senza abbreviazioni.
par(mfrow=c(1,1))
plot(tree.boston)
text(tree.boston, pretty = 0)
dev.off()

# Assegniamo all'oggetto cv.boston l'output della funzione cv.tree()
# che esegue la potatura dell'albero (cost complexity pruning) tramite
# validazione incrociata, per identificare la dimensione ottimale
# dell'albero che minimizza l'RSS
cv.boston <- cv.tree(tree.boston)

# Viene disegnato un grafico in cui:
# - sull'asse x è riportato il numero di nodi terminali (dimensione dell'albero)
# - sull'asse y il valore di deviance ottenuto tramite cross-validation
# - type = "b" indica che punti e linee devono essere tracciati entrambi
plot(cv.boston$size, cv.boston$dev, type = "b")

# Si assegna all'oggetto prune.boston l'output della funzione prune.tree()
# che restituisce una versione potata dell’albero iniziale, limitando
# il numero di nodi terminali a 5 (best = 5), come scelto in base al plot precedente.
prune.boston <- prune.tree(tree.boston, best = 5)

# Si visualizza graficamente l’albero potato
plot(prune.boston)
text(prune.boston, pretty = 0)

# Si assegna all’oggetto yhat il vettore delle predizioni del modello ad albero
# originale (non potato), calcolate sui dati di test (cioè sulle osservazioni non
# incluse nel training set)
yhat <- predict(tree.boston, newdata = Boston[-train, ])

# Si assegna all’oggetto boston.test il vettore dei valori osservati della
# variabile medv nel sottoinsieme di test (nella pratica si considerano tutte
# le righe eccetto quelle il cui indice è presente come elemento di train)
boston.test <- Boston[-train, "medv"]

# Si crea uno scatterplot in cui si confrontano le predizioni (yhat) con i valori
# reali (boston.test). Se il modello fosse perfetto, i punti cadrebbero tutti sulla
# retta y = x tracciata da abline(0,1).
plot(yhat, boston.test)
abline(0, 1)

# Si calcola l’errore quadratico medio (MSE) sulle osservazioni di test:
# è una misura sintetica della bontà delle predizioni.
mean((yhat - boston.test)^2)
# 28.21663

#############
## Bagging ##
#############

# Se mtry = n.totale di variabili allora Bagging
# Se mtry < n.totale di variabili allora Random Forest
# ma il comando resta sempre randomForest()

# Attraverso il comando library() carichiamo il pacchetto randomForest
# il quale contiene l'omonima funzione e altre funzione utili
# install.packages("randomForest")
library(randomForest)

set.seed(1)

# Si assegna all’oggetto bag.boston l’output della funzione randomForest(),
# che implementa un algoritmo di Bagging (Bootstrap Aggregating) tramite alberi
# di regressione: vengono generati più alberi su campioni bootstrap differenti
# del training set, e le loro previsioni vengono mediate per ridurre la varianza
# del modello. In particolare:
# - formula medv ~ . indica che vogliamo predire il valore mediano delle abitazioni
#   (medv) utilizzando tutte le altre variabili del dataset come predittori
# - data = Boston specifica il dataset da cui attingere i dati
# - subset = train indica che il modello viene stimato solo sul sottoinsieme di dati
#   definito dal vettore train (training set)
# - mtry = 12 impone che ciascun albero consideri tutte e 12 le variabili in ogni split,
#   condizione che definisce il Bagging (anziché una Random Forest, dove mtry < 12)
# - importance = TRUE consente di calcolare l’importanza relativa delle variabili predittive
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 12, importance = TRUE) 
bag.boston
# Call:
#  randomForest(formula = medv ~ .,data = Boston,mtry = 12,importance = TRUE,subset = train)
#                Type of random forest: regression
#                      Number of trees: 500
# No. of variables tried at each split: 12
# 
#           Mean of squared residuals: 15.74801
#                     % Var explained: 81.84

# Si assegna all'oggetto yhat.bag il vettore delle previsioni per le
# unità di test assegnate al parametro newdata, vengono selezionate
# tutte le colonne di Boston e le sole righe i cui indici non sono
# contenuti come elementi del dataset train
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])

# Per salvare il grafico
png(paste(img_path, "/05_Bagging_Scatterplot_Osservato_Previsto.png", sep=""), width = 800, height = 600)

# Attraverso la funzione plot() si ottiene lo scatterplot in cui nell'asse
# delle ascisse viene rappresentato il vettore delle previsioni e nell'asse
# delle ordinate il vettore dei valori osservati.
# Infine viene rappresentata la curva passante per l'origine degli assi, questo
# servirà a facilitare un confronto tra osservato e predetto
plot(yhat.bag, boston.test)
abline(0, 1)
dev.off()

# Attraverso il comando seguente è possibile calcolare la media degli
# scarti al quadrato tra previsto e osservato per i dati di test o più
# in generale l'MSE di test
mean((yhat.bag - boston.test)^2)
# 18.52237

# Si assegna adesso all'oggetto bag.boston l'output della funzione randomForest,
# come fatto in precedenza ma in questo caso si aggiunge come parametro della
# funzione ntree = 25, dunque solamente 25 alberi (di default sono 500)
# Ciò potrebbe far aumentare la varianza del previsore rispetto al caso precedente,
# in quanto prima venivano utilizzati 500 alberi
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 12, ntree = 25)

# Viene assegnato a yhat.bag il vettore delle previsioni per le osservazioni
# di test come fatto in precedenza
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])

# Calcoliamo l'MSE di test
mean((yhat.bag - boston.test)^2)
# 19.52548

# è leggermente aumentato passando da 500 alberi a 25, ma come possiamo
# notare non di tantissimo. 500 potrebbe essere quindi uno spreco di risorse,
# potremmo accontentarci di un numero relativamente più contenuto



###################
## Random Forest ##
###################

set.seed(1)

# Si assegna alla funzione rf.boston l'output della funzione randomForest() la quale
# ci permette di implementare l'algoritmo di Random forest su degli alberi di
# di regressione (medv quantitativa).
# La variabile di risposta del modello è medv, mentre le variabili indipendenti
# le restanti colonne di Boston. Si implementa l'algoritmo per la partizione
# di Boston i cui indici di riga sono contenuti nel vettore train.
# Attraverso il parametro mtry = 6, specifichiamo quante variabili devono essere
# estratte casualmente dalle 12 variabili a disposizione per la costruzione di ogni
# singolo albero di regressione, ciò ci permette di decorrelare gli alberi.
# Il parametro importance = TRUE permette di calcolare misure quantitative
# dell’importanza di ciascuna variabile predittiva, utili per l’interpretazione del modello
rf.boston <- randomForest(medv ~ ., data = Boston,subset = train, mtry = 6, importance = TRUE)
rf.boston

# Si assegna all'oggetto yhat.rf il vettore delle previsioni per le
# unità di test assegnate al parametro newdata, vengono selezionate
# tutte le colonne di Boston e le sole righe i cui indici non sono
# contenuti come elementi del dataset train
yhat.rf <- predict(rf.boston, newdata = Boston[-train, ])

# Calcoliamo l'MSE di test
mean((yhat.rf - boston.test)^2)
# 18.03941

# Si utilizza la funzione importance() per visualizzare l’importanza relativa
# delle variabili nel modello Random Forest (es. aumento dell’errore MSE o
# riduzione dell’impurità media quando la variabile è esclusa)
importance(rf.boston)
#           %IncMSE IncNodePurity
# crim    11.413450     532.13137
# zn       2.236034      97.82974
# indus   11.766260     914.71292
# chas     1.623781      41.24927
# nox      9.143657     569.29338
# rm      30.211650    6619.48681
# age     11.901297     620.24771
# dis     13.036057     840.75789
# rad      4.702573     135.18727
# tax      8.565916     366.86390
# ptratio 10.163513     473.95565
# black    6.641653     281.55768
# lstat   27.033827    5370.30884

# Per salvare il grafico
png(paste(img_path, "/06_Random_Forest_VarImpPlot.png", sep=""), width = 800, height = 600)

# Si utilizza la funzione varImpPlot() per produrre un grafico che mostra
# l’importanza stimata delle variabili predittive in ordine decrescente
varImpPlot(rf.boston)
dev.off()

# Quante volte ogni osservazione rimane Out of Bag nella costruzione dei
# campioni bootstrap
rf.boston$oob.times
#  [1] 194 175 172 183 174 176 197 188 175 184 176 183 167 152 200 180 182 180
#  [19] 178 191 199 188 182 180 185 207 182 172 177 177 190 196 194 200 181 188
#  [37] 188 188 192 177 178 188 188 184 190 186 192 204 183 200 176 177 171 178
#  [55] 204 171 184 177 187 189 182 179 187 190 190 187 180 165 171 179 208 197
#  [73] 168 176 178 184 188 205 181 183 186 182 171 176 195 172 192 185 167 172
#  [91] 170 174 173 168 184 169 212 187 189 184 188 183 190 169 182 183 180 178
# [109] 180 195 177 184 179 183 168 181 186 200 200 178 196 181 190 173 182 192
# [127] 192 171 180 209 192 167 174 194 185 179 202 187 172 198 197 199 178 146
# [145] 189 162 179 192 173 157 187 189 177 181 187 177 196 210 167 188 170 190
# [163] 178 177 153 206 179 187 179 175 174 172 186 182 185 174 175 182 179 196
# [181] 176 189 166 1

# In media ogni osservazione rimane out of bag 183 volte su 500
mean(rf.boston$oob.times)
# 183.055


##############
## Boosting ##
##############

# Attraverso la funzione library() carichiamo il pacchetto gbm, il quale
# contiene le funzione utili all'implementazione dell'algoritmo di Boosting
library(gbm)

set.seed(1)

# Si assegna all'oggetto boost.boston l'output della funzione gbm()
# attraverso la quale è possibile implementare il Boosting per degli
# alberi di regressione in cui la variabile di risposta è medv e
# come predittori le restanti variabili di Boston.
# Al parametro data assegniamo il dataset Boston in cui sono presenti tutte le
# colonne e solamente le righe i cui indici sono degli elementi del
# vettore train. Essendo in regressione passiamo il parametro distribution =
# "gaussian", assegniamo al parametro della funzione ntrees il numero di
# alberi da considerare ed infine la profondità fin quanto vogliamo andare,
# ovvero quanti split effettuare per ogni albero, in questo caso 4.
boost.boston <- gbm(medv ~ ., data = Boston[train, ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
boost.boston
# gbm(formula = medv ~ ., distribution = "gaussian", data = Boston[train,
#     ], n.trees = 5000, interaction.depth = 4)
# A gradient boosted model with gaussian loss function.
# 5000 iterations were performed.
# There were 13 predictors of which 13 had non-zero influence.

# Attraverso il comando summary() applicato ad un oggetto di classe gbm
# otteniamo la tabella (ed allo stesso tempo anche il grafico) della misura
# dell'infuenza relativa per ogni variabile, ovvero la riduzione dell'errore
# quadratico attribuito a ciascuna variabile.
# In pratica questa misura indica quanto vi è stata una riduzione nell'RSS
# quando ha giocato una determinata variabile.
summary(boost.boston)
# tat     lstat 35.0294509
# rm           rm 34.2227793
# dis         dis  6.6633758
# age         age  4.2752411
# crim       crim  3.6513515
# black     black  3.3118327
# indus     indus  3.2288380
# tax         tax  2.8526779
# nox         nox  2.7432323
# ptratio ptratio  2.2958185
# rad         rad  1.1699964
# zn           zn  0.3841339
# chas       chas  0.1712718

# Viene prodotto un grafico di dipendenza parziale per la variabile "rm" (numero medio di stanze)
# che mostra la relazione marginale tra "rm" e la variabile risposta, controllando per le altre variabili
plot(boost.boston, i = "rm")

# Per salvare il grafico
png(paste(img_path, "/07_Boosting_PartialDependencePlot.png", sep=""), width = 800, height = 600)

# Grafico di dipendenza parziale per la variabile "lstat" (percentuale di popolazione a basso reddito)
plot(boost.boston, i = "lstat")
dev.off()

# Si calcolano le previsioni del modello boost.boston sul test set (Boston[-train, ])
# specificando il numero di alberi da usare per il boosting (5000)
yhat.boost <- predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)
# 14.99186

# Si calcola l’errore quadratico medio tra le previsioni e i valori osservati nel test set
mean((yhat.boost - boston.test)^2)

# Si costruisce un nuovo modello di boosting con parametro shrinkage (tasso di apprendimento) fissato a 0.2
boost.boston <- gbm(medv ~ ., data = Boston[train, ], distribution = "gaussian",
                    n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = FALSE)

# Si calcolano le previsioni del modello boost.boston sul test set (Boston[-train, ])
# specificando il numero di alberi da usare per il boosting (5000)
yhat.boost <- predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)

# Si calcola nuovamente l’errore quadratico medio sul test set
mean((yhat.boost - boston.test)^2)
# 16.86628

# Si costruisce un secondo modello di boosting con shrinkage = 0.2 e numero di alberi pari a 1000
boost.boston2 <- gbm(medv ~ ., data = Boston[train, ], distribution = "gaussian",
                     n.trees = 1000, interaction.depth = 4, shrinkage = 0.2, verbose = FALSE)

# Si crea un vettore di valori che rappresentano diversi numeri di alberi
# da testare per valutare l’andamento dell’errore al crescere di n.trees
n.trees <- seq(from = 100, to = 1000, by = 100)

# Si ottiene una matrice di previsioni: ogni colonna corrisponde al numero di alberi
# usati nella predizione, ogni riga a un'osservazione nel test set
predmat <- predict(boost.boston2, newdata = Boston[-train, ], n.trees = n.trees)

# Verifica della dimensione della matrice delle previsioni
dim(predmat)
# 306  10

# Si calcola l’errore quadratico medio per ogni valore di n.trees (colonne di predmat)
# confrontando le previsioni con i valori osservati nel test set
berr <- with(Boston[-train, ], apply((predmat - medv)^2, 2, mean))

# Per salvare il grafico
png(paste(img_path, "/08_Boosting_TestError.png", sep=""), width = 800, height = 600)

par(mfrow=c(1,1))
# Si traccia il grafico dell’errore medio quadratico al variare del numero di alberi
# per visualizzare il comportamento del test error e identificare un buon compromesso
plot(n.trees, berr, pch = 19, ylab = "Mean Squared Error",
     xlab = "# Trees", main = "Boosting Test Error", type = "b")
dev.off()