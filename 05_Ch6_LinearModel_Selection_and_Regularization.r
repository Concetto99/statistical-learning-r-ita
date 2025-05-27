#################################################################
#### [278] 6.5 Lab: Linear Models and Regularization Methods ####
#################################################################

remove(list = ls())

# Attraverso il comando library() carichiamo il pacchetto ISLR,
# il quale contiene vari dataset e funzioni utili.
library(ISLR)

# Attraverso il comando View() apro la finestra dedicata alla
# visualizzazione delle variabili del dataset Hitters
View(Hitters)

# Attraverso il comando names() applicato ad un oggetto di tipo data
# frame, ricevo in output i nomi delle variabili di Hitters
names(Hitters)

# Attraverso il comando dim() visualizzo le dimensioni del dataset Hitters
dim(Hitters)
# 322  20

# Attraveso il comando sum() applicato ad un vettore booleano
# della stessa dimensione di Salary, che assume valore TRUE se l'elemento
# i-esimo è "NA" e FALSE altrimenti, riporto in output la somma degli
# elementi del vettore booleano che sono uguali a TRUE
sum(is.na(Hitters$Salary))
# 59

# assegno all'oggetto name.values un vettore di lunghezza pari alle colonne
# del dataser Hitters con elementi tutti uguali alla stringa "nome"
name.values <- rep("nome",dim(Hitters)[2])

# assegno all'oggetto na.values un vettore di lunghezza pari alle colonne
# del dataser Hitters con elementi tutti uguali a 0
na.values <- rep(0,dim(Hitters)[2])

# Attraverso un ciclo for, per i che va da 1 al numero di colonne del dataset
# Hitters, assegno all'i-esimo elemento dell'oggetto name.values il nome della
# colonna corrispondente nel dataset Hitters e all'oggetto na.values la somma
# dei valori mancanti presenti per l'i-esima colonna. Per fare ciò si utilizza
# l'operatore di selezione [] in cui ad ogni iterazione viene selezionata
# la colonna i-esima tramite la funzione names() la quale restituisce i nomi
# delle colonne di Hitters, e viene selezionata l'i-esimo nome della colonna
# del vettore contenente i nomi
for (i in 1:dim(Hitters)[2]) {
    name.values[i] <- names(Hitters)[i]
    na.values[i] <- sum(is.na(Hitters[names(Hitters)[i]]))
}

# Si stampa in output la matrice contente i due vettori name.values e na.values
# uniti per colonna
cbind(name.values, na.values)
#      name.values na.values
#  [1,] "AtBat"     "0"
#  [2,] "Hits"      "0"
#  [3,] "HmRun"     "0"
#  [4,] "Runs"      "0"
#  [5,] "RBI"       "0"
#  [6,] "Walks"     "0"
#  [7,] "Years"     "0"
#  [8,] "CAtBat"    "0"
#  [9,] "CHits"     "0"
# [10,] "CHmRun"    "0"
# [11,] "CRuns"     "0"
# [12,] "CRBI"      "0"
# [13,] "CWalks"    "0"
# [14,] "League"    "0"
# [15,] "Division"  "0"
# [16,] "PutOuts"   "0"
# [17,] "Assists"   "0"
# [18,] "Errors"    "0"
# [19,] "Salary"    "59"
# [20,] "NewLeague" "0"


# Assegniamo all'oggetto Hitters il dataset Hitters del pacchetto ISRL2
# ad esclusione delle righe aventi valori mancanti
Hitters <- na.omit(Hitters)

# Attraverso il comando dim() visualizzo le dimensioni del dataset Hitters
# con le righe aventi valori mancanti rimosse
dim(Hitters)
# 263  20



###########################
## Best Subset Selection ##
###########################

# Attraverso la funzione library() carichiamo il pacchetto leaps, il quale
# contiente le principali funzioni per implementare gli algoritmi di
# feature selection (Best, Fwd e Bwd Selection)
library(leaps)

# Si assegna all'oggetto regfit.full l'output della funzione regsubsets, la
# quale ci permette di implementare una Best Subset Selection a partire
# dal dataset Hitters, per il quale vogliamo trovare il miglior modello
# per stimare la variabile Salary utilizzando un sottoinsieme o la totalità
# delle altre variabili contenute nel dataset
regfit.full <- regsubsets(Salary ~ ., Hitters)
class(regfit.full) # "regsubsets"

# il comando summary applicato ad un oggetto di classe regsubset ci
# restituisce un elenco dei migliori modelli selezionati per ciascun numero
# di variabili (da 1 a nvmax, di default 8).
# con "Selection Algorithm: exhaustive", ci indica che stiamo implementando
# una best subset selection, quindi ad ogni step si ricerca il miglior modello
# tra tutte le combinazioni di modelli con n predittori, utilizzando il RSS
# minimo come misura per decidere quale considerare il miglior modello
summary(regfit.full)
names(summary(regfit.full))
# "which"  "rsq"    "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj"

# il comando summary applicato ad un oggetto di classe regsubset ci
# restituisce un elenco dei migliori modelli selezionati per ciascun numero
# di variabili, in questo caso da 1 fino a 19 ovvero tutti i possibili modelli.
# con "Selection Algorithm: exhaustive", ci indica che stiamo implementando
# una best subset selection, quindi ad ogni step si ricerca il miglior modello
# tra tutte le combinazioni di modelli con n predittori, utilizzando il RSS
# minimo come misura per decidere quale considerare il miglior modello
regfit.full <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)

# Assegniamo all'oggetto reg.summary l'output del summary di regfit.full
reg.summary <- summary(regfit.full)


# Selezioniamo dall'oggetto reg.summary l'attributo rsq, ovvero il residual
# sum of squares dei 19 migliori modelli per ogni numero fisso di variabili
# selezionate
reg.summary$rsq

# Il comando plot consente di visualizzare l'andamento della RSS (Residual
# Sum of Squares) al variare del numero di variabili incluse nel modello.
# Sull'asse delle ascisse sono riportati i numeri da 1 a 19, ciascuno
# corrispondente al miglior modello selezionato con quel numero di predittori.
# Sull'asse delle ordinate si trovano i valori di RSS, che indicano l'errore
# residuo dei modelli: più basso è il valore, migliore è il fit.
# L'argomento type = "l" specifica che i punti devono essere collegati da linee,
# in modo da ottenere un grafico a linea continua, per osservare il trend.
# Però, per questo tipo di grafico sarebbe più utile utilizzare type="b" per
# avere contezza della spezzata (x è una variabile discreta)
par(mfrow = c(1, 1))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
# plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "b")

# Attraverso il comando plot, in seguito alla creazione di una finestra grafica
# di due righe e 1 colonna, visualizziamo l'andamento dell'R2 e dell'R2
# aggiustato.
# Sull'asse delle ascisse sono riportati i numeri da 1 a 19, ciascuno
# corrispondente al miglior modello selezionato con quel numero di predittori.
# Sull'asse delle ordinate si trovano i valori di R2 e Adjusted R2, che
# indicano rispettivamente la proporzione di variabilità spiegata dal
# modello (R2) e una  sua versione corretta per il numero di
# variabili (Adjusted R2).
# Più alti sono questi valori, migliore è la capacità esplicativa del modello.
# L'argomento type = "l" specifica che i punti devono essere collegati da linee,
# in modo da ottenere un grafico a linea continua, per osservare il trend.
par(mfrow = c(2, 1))
plot(reg.summary$rsq, xlab = "Number of Variables", ylab = "R2", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

# Attraverso il comando which.max() applicato al vettore degli R2 adjusted
# riportiamo in output il valore massimo del vettore
which.max(reg.summary$adjr2) # 11

# Aggiungiamo al grafico il punto della curva il cui valored del R2
# Adjusted ha il valor massimo, ovvero per numero di variabili pari a 11
par(mfrow = c(1, 1))
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)

## Cp di Mallow
# Calcoliamo quale modello (in termini di numero di variabili) presenta
# il valore minimo del Cp di Mallow, un indicatore che bilancia bontà
# del fit e complessità del modello.
# Il valore ideale di Cp è vicino al numero di parametri del modello
# in generale, più basso è Cp, meglio è.
which.min(reg.summary$cp)  # 10, il modello con 10 variabili ha il Cp minimo.

# Impostiamo una finestra grafica singola
par(mfrow = c(1, 1))

# Tracciamo il grafico di Cp in funzione del numero di variabili
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")

# Evidenziamo il punto corrispondente al Cp minimo con un punto rosso grande
points(10, reg.summary$cp[10], col = "red", cex = 2, pch = 20)

## BIC (Bayesian Information Criterion)
# Analogamente, individuiamo il modello che minimizza il BIC,
# un criterio di selezione che penalizza fortemente la complessità del
# modello (più severo rispetto ad AIC o Cp).
# Il valore minimo di BIC suggerisce il modello più parsimonioso con buon fit.
which.min(reg.summary$bic)  # 6: il miglior modello secondo BIC ha 6 variabili.

# Ripristiniamo una finestra grafica singola
par(mfrow = c(1, 1))

# Tracciamo il grafico del BIC rispetto al numero di variabili
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")

# Evidenziamo il punto di BIC minimo con un punto rosso
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)


# Il comando plot() applicato a un oggetto di classe "regsubsets" consente di
# visualizzare i risultati della procedura di selezione del sottoinsieme di
# variabili.
# L'argomento "scale" specifica quale criterio di valutazione utilizzare
# nel grafico per confrontare i modelli di dimensioni diverse.

# Nel grafico risultante:
# - sull'asse delle ascisse c'è il numero di variabili nel modello
# - sull'asse delle ordinate il valore del criterio scelto (es. R2, Cp, BIC, ecc.)
# - i quadratini neri indicano le variabili incluse nel miglior modello
# per ciascuna dimensione

plot(regfit.full, scale = "r2")
# Mostra i migliori modelli per ciascun numero di variabili in base al valore
# dell'R2 (coefficiente di determinazione).
# l'R2 aumenta con l'aggiunta di variabili, ma non penalizza la complessità,
# quindi da solo non è sufficiente per scegliere.

plot(regfit.full, scale = "adjr2")
# Visualizza i modelli in base alL'R2 aggiustato, che penalizza l'inclusione
# di variabili inutili. Il picco nel grafico indica il modello con miglior
# compromesso tra fit e parsimonia.

plot(regfit.full, scale = "Cp")
# Mostra i modelli valutati secondo il Cp di Mallow.
# Un buon modello ha un valore di Cp vicino al numero di variabili più uno (p+1)
# Il modello con Cp minimo è quello con il miglior equilibrio tra errore e
# complessità.

plot(regfit.full, scale = "bic")
# Visualizza i modelli in base al BIC (Bayesian Information Criterion), che
# penalizza fortemente l’aggiunta di variabili.
# Il modello con BIC minimo è in genere molto parsimonioso (più semplice).

# Infine:
coef(regfit.full, 6)
# Restituisce i coefficienti del modello selezionato che utilizza 6 variabili
# (più l'intercetta, se presente). Questo è utile per sapere quali predittori
# sono stati inclusi nel modello con quella dimensione e quali sono i loro
# coefficienti stimati.


#############################################
## Forward and Backward Stepwise Selection ##
#############################################

# Si assegna all'oggetto regfit.fwd l'output della funzione regsubset con
# metodo forward che implementa una Forward Stepwise Selection, una
# procedura di selezione progressiva che parte da un modello
# nullo (nessuna variabile) e aggiunge a ogni passaggio la variabile che
# migliora maggiormente il modello secondo un criterio di bontà del fit
# (di default RSS).
# Il parametro nvmax = 19 specifica che il numero massimo di variabili
# selezionabili è 19, ovvero tutte quelle disponibili nel dataset.
regfit.fwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)

# Si assegna all'oggetto regfit.bwd l'output della funzione regsubset con
# metodo backward che implementa una Forward Stepwise Selection, una procedura
# di selezione progressiva che parte da un modello completo (tutte le variabili)
# e rimuove a ogni passaggio la variabile meno significativa, ovvero quella la
# cui eliminazione peggiora meno il modello.
# Anche in questo caso si impone come massimo numero di variabili nvmax = 19.
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)

?coef.regsubsets
# Il comando coef() al quale passiamo l'oggetto regfit.full, di classe
# regsubsets e l'id = 7 ovvero il miglior modello ottenuto tramite
# best subset selection avente 7 predittori, riporta in output i
# coefficienti del modello di regressione selezionato
coef(regfit.full, id = 7)
#  (Intercept)         Hits        Walks       CAtBat        CHits       CHmRun
#   79.4509472    1.2833513    3.2274264   -0.3752350    1.4957073    1.4420538
#    DivisionW      PutOuts
# -129.9866432    0.2366813

# Il comando coef() al quale passiamo l'oggetto regfit.fwd, di classe
# regsubsets e l'id = 7 ovvero il miglior modello ottenuto tramite
# forward selection avente 7 predittori, riporta in output i
# coefficienti del modello di regressione selezionato
coef(regfit.fwd, 7)
#  (Intercept)        AtBat         Hits        Walks         CRBI       CWalks
#  109.7873062   -1.9588851    7.4498772    4.9131401    0.8537622   -0.3053070
#    DivisionW      PutOuts
# -127.1223928    0.2533404

# Il comando coef() al quale passiamo l'oggetto regfit.bwd, di classe
# regsubsets e l'id = 7 ovvero il miglior modello ottenuto tramite
# backward selection avente 7 predittori, riporta in output i
# coefficienti del modello di regressione selezionato
coef(regfit.bwd, 7)
#  (Intercept)        AtBat         Hits        Walks        CRuns       CWalks 
#  105.6487488   -1.9762838    6.7574914    6.0558691    1.1293095   -0.7163346 
#    DivisionW      PutOuts
# -116.1692169    0.3028847


########################################################################
## Selezionare il miglior modello usando Cross Validation o Bootstrap ##
########################################################################

# A valle della scelta di ogni miglior modello a parità del numero
# di variabili, la scelta del miglior modello è stata effettuata
# utilizzando l'indice R2 Aggiustato (o alternativamente BIC e Cp)
# Un altro modo per effettuare questa scelta è possibile tramite
# la Cross Validation o il Bootstrap

# Si definisce il seme per la riproducibilità del vettore train
# per il quale utilizziamo la funzione sample()
set.seed(1)

# Si assegna all'oggetto train il vettore booleano di lunghezza pari
# al numero di righe del dataset Hitters, i cui elementi sono i valori
# TRUE o FALSE, estratti casualmente con reimmissione
train <- sample(c(TRUE, FALSE), nrow(Hitters), replace = TRUE)

# Si assegna all'oggetto test il vettore booleano di lunghezza pari
# a train, i cui elementi sono complementari all'oggetto train
test <- (!train)

# Si assegna all'oggetto regfit.best l'output della funzione regsubsets, la
# quale ci permette di effettuare una Best Subset Selection per predire
# la variabile Salary utilizzando un sottoinsieme delle variabili esplicative
# contenute nel dataset Hitters. A differenza della procedura precedente, qui
# la selezione viene effettuata solo sui dati di training (Hitters[train, ]),
# ovvero su un sottoinsieme casuale del dataset originale, individuato dal
# vettore booleano train. Il parametro nvmax = 19 indica che vogliamo
# considerare modelli con al massimo 19 variabili predittive
# (ossia tutte quelle disponibili).
regfit.best <- regsubsets(Salary ~ ., data = Hitters[train, ], nvmax = 19)

# Si assegna all'oggetto test.mat la matrice modello a partire dai dati
# di test del dataset Hitters. La matrice avrà quindi una prima colonna
# formata da 1 di lunghezza pari al vettore test, e successivamente
# tutte le colonne di Hitters eccetto Salary, e solamente le righe per
# cui all'i-esima posizione del vettore booleano test è presente il
# valore TRUE
test.mat <- model.matrix(Salary ~ ., data = Hitters[test, ])

# Assegniamo all'oggetto val.errors un vettore di lunghezza 19 i cui
# elementi saranno degli NA (missing values)
val.errors <- rep(NA, 19)

# Attraverso un ciclo for per i che va da 1 a 19 eseguiamo le seguenti istruzioni:
# - si assegna all'oggetto coefi il vettore dei coefficienti del
# miglior modello con un numero pari a i di regressori scelto
# usando l'R2 maggiore
# - si assegna all'oggetto pred l'output del prodotto matriciale
# tra la matrice test.mat in cui vengono estratte le sole colonne
# presenti nell'oggetto coefi moltiplicato per il vettore dei coefficienti.
# Si assegna all'i-esimo elemento dell'oggetto val.errors, sovrascrivendolo,
# la media degli errori al quadrato tra i valori osservati di Salary
# e i valori predetti, selezionando solo i dati di test
for (i in 1:19) {
 coefi <- coef(regfit.best, id = i)
 pred <- test.mat[, names(coefi)] %*% coefi
 val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}
val.errors
#  [1] 164377.3 144405.5 152175.7 145198.4 137902.1 139175.7 126849.0 136191.4
#  [9] 132889.6 135434.9 136963.3 140694.9 140690.9 141951.2 141508.2 142164.4
# [17] 141767.4 142339.6 142238.2

# Il comando which.min restituisce la posizione in cui vi è
# il valor minimo tra gli elementi del vettore val.errors
which.min(val.errors) # 7

# Attraverso il comando coef riportiamo in output il vettore
# dei coefficienti del modello migliore con 7 regressori
coef(regfit.best, 7)
#  (Intercept)        AtBat         Hits        Walks        CRuns       CWalks
#   67.1085369   -2.1462987    7.0149547    8.0716640    1.2425113   -0.8337844
#    DivisionW      PutOuts
# -118.4364998    0.2526925

# Si definisce una funzione custom denominata predict.regsubsets, utile per
# implementare una funzione di predizione compatibile con oggetti di classe
# "regsubsets", come quelli restituiti dalla funzione regsubsets() del
# pacchetto leaps.
# Questa funzione permette di effettuare previsioni sui dati newdata,
# specificando quale modello utilizzare tramite l'argomento id (numero
# di variabili da includere).
# All'interno della funzione:
# - Si estrae la formula originale utilizzata nella funzione regsubsets.
# - Si costruisce la matrice modello (mat) sui nuovi dati utilizzando
# model.matrix().
# - Si recuperano i coefficienti del modello selezionato (coefi)
# corrispondente a id.
# - Si selezionano dalla matrice mat solo le colonne relative alle
# variabili usate nel modello.
# - Infine si restituiscono le predizioni come prodotto tra la matrice
# delle variabili selezionate e i coefficienti del modello.
predict.regsubsets <- function(object, newdata, id, ...) {
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi
}

# Assegniamo all'oggetto regfit.best l'output della funzione
# regsubset che permette di effettuare una best subset selection
# alla quale passiamo la formula per il modello da considerare
# il dataset Hitters e il numero massimo di variabili da includere
regfit.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)

# con il comando coef() al quale passiamo l'oggetto di classe regsubsets
# e id = 7 riportiamo in output il vettore dei coefficienti per le
# variabili del miglior modello con 7 variabili
coef(regfit.best, 7)

# assegniamo a k il valore 10
k <- 10

# assegniamo ad n l'elemento corrispondente al numero di righe del
# dataset Hitters
n <- nrow(Hitters)
n # 263

# Definiamo il seme per la riproducibilità del vettore folds
set.seed(1)

# Assegniamo all'oggetto folds un vettore di lunghezza pari a n (263)
# contenente come elementi i medesimi valori di un vettore composto da numeri
# ripetuti da 1 a 10 (funzione rep()), ma permutati (funzione sample())
folds <- sample(rep(1:k, length = n))

rep(1:k, length = n)
#  [1]  1  2  3  4  5  6  7  8  9 10  1  2  3  4  5  6  7  8  9 10  1  2  3  4  5
# [26]  6  7  8  9 10  1  2  3  4  5  6  7  8  9 10  1  2  3  4  5  6  7  8  9 10
# ...
# [226]  6  7  8  9 10  1  2  3  4  5  6  7  8  9 10  1  2  3  4  5  6  7  8  9 10
# [251]  1  2  3  4  5  6  7  8  9 10  1  2  3

folds
# [1]  4  9  2  7  2 10  4  3 10  2  6  8  4  8  8  5  5  7  4  9  6 10  3  4  8
# [26]  1  3  7  1  5  5  8  2  4  2  5  5  9  7  5  3  1  3  1  7  8  1  8  9  7
...
# [226]  1  8  4  2  6  7  6  6  2  7  9  4  2  8  6  8 10  7 10  9  5  8  4  1  3
# [251] 10  7  2  1 10  4  2  3  4  3  9  5  4

# si assegnia all'oggetto cv.errors la matrice avente 10 righe e 19 colonne
# i cui elementi sono NA, le righe non hanno un nome, mentre le colonne
# assumono come nome le stringhe con numeri da "1" a "19"
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))
cv.errors
#       1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19
#  [1,] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#  [2,] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#  [3,] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#  [4,] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#  [5,] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#  [6,] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#  [7,] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#  [8,] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#  [9,] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
# [10,] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA

# Attraverso le seguenti righe di codice si eseguono due cicli for
# annidati, in cui per ogni j da 1 a k si eseguono le seguenti istruzioni:
# - si assegna all'oggetto best.fit l'output della funzione regsubset
# attraverso la quale si effettua una best subset selection delle variabili
# di Hitters a partire dal modello nullo, fino al modello completo con
# 19 variabili per predire la variabile Salary, si estraggono le sole righe
# di Salary per cui il vettore folds è diverso dall'indice j, nella pratica
# tutti gli elementi non facenti parte della k-esima fold
# - si procede con il ciclo for per i che va da 1 a 19, in cui:
#  1. si assegna alla variabile pred il vettore delle previsioni tramite
#     la funzione predict() creata in precedenza in cui passiamo come
#     parametri l'oggetto di classe regsubsets, e i dati di test ovvero
#     le righe del dataset Hitters per cui l'indice ha rispettivamente
#     nella medesima posizione di folds il numero j. Si passa come parametro
#     anche il numero di variabili considerate.
# 2. si aggiorna la matrice cv.errors alla riga j-esima e colonna i-esima
#    con la media degli scarti al quadrato dei valori di test osservati
#    e quelli predetti.
for (j in 1:k) {
    best.fit <- regsubsets(Salary ~ ., data = Hitters[folds != j, ], nvmax = 19)
    for (i in 1:19) {
        pred <- predict(best.fit, Hitters[folds == j, ], id = i)
        cv.errors[j, i] <- mean((Hitters$Salary[folds == j] - pred)^2)
    }
}

# Si assegna all'oggetto mean.cv.errors un vettore di lunghezza 19
# avente come elementi la media degli MSE di test per ogni miglior
# modello di j variabili per le 10 fold.
# La funzione apply() prende come argomenti l'oggetto su cui effettuare
# le operazioni richieste, overo la matrice cv.errors per la quale si
# calcola per colonna (secondo parametro uguale a 2) la media passata
# come terzo parametro tramite la funzione mean
mean.cv.errors <- apply(cv.errors, 2, mean)

mean.cv.errors
#       1        2        3        4        5        6        7        8
#143439.8 126817.0 134214.2 131782.9 130765.6 120382.9 121443.1 114363.7
#       9       10       11       12       13       14       15       16 
#115163.1 109366.0 112738.5 113616.5 115557.6 115853.3 115630.6 116050.0
#      17       18       19
#116117.0 116419.3 116299.1


# Attraverso la funzione plot() stampiamo lo scatterplot avente nell'asse
# delle ascisse l'indice degli elementi (1 a 19) e nell'asse delle ordintate
# i valori del vettore mean.cv.errors, con punti uniti da una spezzata
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")

# Si assegna all'oggetto reg.best l'output della funzione reg.best
# la quale ci permette di effettuare una best subset selection per
# tutti i modelli a partire dal modello nullo fino al modello con
# 19 variabili per predire la variabile Salary e utilizzando le
# colonne del dataset Hitters e tutte le righe
reg.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)

# Attraverso il comando coef applicato ad un oggetto di tipo regsubsets
# e id=10 riportiamo in output i coefficienti del miglior modello con
# 10 variabili
coef(reg.best, 10)



######################
## Ridge Regression ##
######################

# Assegniamo all'oggetto x la matrice modello composta da tutte
# le colonne del dataset Hitters eccetto Salary dalla quale selezioniamo
# tutte le righe ed escludiamo la colonna contenente l'intercetta
x <- model.matrix(Salary ~ ., Hitters)[, -1]

x[1:5,1:5]
#                   AtBat Hits HmRun Runs RBI
# -Alan Ashby         315   81     7   24  38
# -Alvin Davis        479  130    18   66  72
# -Andre Dawson       496  141    20   65  78
# -Andres Galarraga   321   87    10   39  42
# -Alfredo Griffin    594  169     4   74  51

model.matrix(Salary ~ ., Hitters)[1:5,1:5]
#                   (Intercept) AtBat Hits HmRun Runs
# -Alan Ashby                 1   315   81     7   24
# -Alvin Davis                1   479  130    18   66
# -Andre Dawson               1   496  141    20   65
# -Andres Galarraga           1   321   87    10   39
# -Alfredo Griffin            1   594  169     4   74


# Assegniamo all'oggetto y il vettore contente gli elementi della
# colonna Salary contenuta in Hitters
y <- Hitters$Salary

# Attraverso la funzione library() carichiamo il pacchetto glmnet
# contenente le funzioni per effettuare la Ridge Regression
# install.packages("glmnet")
library(glmnet)

# Assegniamo all'oggetto grid il vettore di lunghezza 100 contenente
# come elementi i numeri compresi tra 10^(10) e 10^(-2) dove l'apice
# è composto da un vettore di elementi tra 10 e -2 equidistatanti
# tra loro
grid <- 10^seq(10, -2, length = 100)

seq(10, -2, length = 100)[1:5]
# 10.000000  9.878788  9.757576  9.636364  9.515152
seq(10, -2, length = 100)[1] - seq(10, -2, length = 100)[2] # 0.1212121
seq(10, -2, length = 100)[2] - seq(10, -2, length = 100)[3] # 0.1212121

# Si assegna al'oggetto ridge.mod l'output della funzione glmnet attraverso
# la quale è possibile implementare una Ridge Regression. Si passano come
# parametri gli oggetti x (matrice contenente le colonne di Hitters ad
# eccezione della variabile Salary), y (vettore contenente il vettore dei
# valori di Salary), alpha = 0 indica che è richiesta la stima dei coefficienti
# via Ridge Regression, infine passiamo a lambda l'oggeto grid contenente
# la griglia di valori tra 10^10 a 10^-2, attraverso i quali verranno
# calcolati i coefficienti beta usando i 100 valori di lambda, ovvero
# la penalty
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
class(ridge.mod) #  "elnet"  "glmnet"

# attraverso il comando dim applicato alla matrice dei coefficienti
# ottenuta tramite coercizione con il comando coef() applicato ad
# un oggetto di classe glmnet, riportiamo in output le dimensioni
# della matrice dei coefficienti ottenuti tramite ridge regression
# Nello specifico avremo 20 righe, intercetta + 19 variabili e
# 100 colonne, ovvero ogni colonna corrisponde ai coefficienti ottenuti
# utilizzando un valore di lambda differente (del vettore grid)
dim(coef(ridge.mod)) # 20 100

# attraverso il seguente comando selezioniamo il 50esimo elemento
# dell'oggetto lambda contenuto all'interno di ridge.mod
ridge.mod$lambda[50] # 11497.57

grid[50] # 11497.57

# selezioniamo tutte le righe per la 50esima colonna di coef(ridge.mod)
# la quale contiene i coefficienti ottenuti via Ridge Regression
# usando come valore per lambda 11497.57
coef(ridge.mod)[, 50]
#   (Intercept)         AtBat          Hits         HmRun          Runs
# 407.356050200   0.036957182   0.138180344   0.524629976   0.230701523
#           RBI         Walks         Years        CAtBat         CHits
#   0.239841459   0.289618741   1.107702929   0.003131815   0.011653637
#        CHmRun         CRuns          CRBI        CWalks       LeagueN
#   0.087545670   0.023379882   0.024138320   0.025015421   0.085028114
#     DivisionW       PutOuts       Assists        Errors    NewLeagueN
#  -6.215440973   0.016482577   0.002612988  -0.020502690   0.301433531

# Si riporta in output la radice quadrata della somma dei coefficienti
# al quadrato per lambda = 11497.57 esclusa l'intercetta
sqrt(sum(coef(ridge.mod)[-1, 50]^2)) # 6.360612

# Riportiamo in output il 60 elemento del vettore dei valori di lambda
# conenuto nell'oggetto ridge.mod
ridge.mod$lambda[60] # 705.4802

# Riportiamo in output i coefficienti beta ottenuti tramite ridge
# regression per lambda = 705.4802 ovvero la 60esima colonna
# dell'oggetto coef(ridge.mod)
coef(ridge.mod)[, 60]

# Si riporta in output la radice quadrata della somma dei coefficienti
# al quadrato per lambda = 705.4802 esclusa l'intercetta
sqrt(sum(coef(ridge.mod)[-1, 60]^2)) # 57.11001

# i coefficienti beta sono più elevati utilizzando lambda = 705.4802
# piuttosto che lambda = 11497.57

# Attraverso il comando predict() applicato ad un oggetto di classe
# glmnet e passando come parametro s (cioè lambda) = 50 e type = coefficients
# riportiamo in output i coefficienti stimati via Ridge Regression
# usando un valore di lambda = 50.
# Seleziono le prime 20 righe e tutte le colonne: in realtà non servirebbe
# perchè abbiamo appunto 20 coefficienti (20 righe 1 colonna)
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]

dim(predict(ridge.mod, s = 50, type = "coefficients")) # 20  1

# Per valutare quale valore di lambda scegliere per trovare i
# coefficienti che minimizzano l'MSE di test o in generale il
# lambda che permette di trovare delle previsioni più accurate
# su dei nuovi dati è necessario utilizzare un metodo per la
# valutazione e scelta del parametro. Un metodo utile potrebbe
# essere il Validation Set Approach

# Definisco il seme per la riproducibilità del vettore train
set.seed(1)

# Assegno all'oggetto train il vettore di lunghezza pari alla
# metà del numero di righe di x contenente i numeri interi estratti
# casualmente senza ripetizione dal vettore contenente i valori
# da 1 al numero delle righe di x
train <- sample(1:nrow(x), nrow(x) / 2)
train[1:5]

# Assegno al vettore test i medesimi valori di train moltiplicati
# per -1
test <- (-train)
test[1:5]

# Assegno al vettore y.test i valori di y utilizzando il
# vettore test come operatore di selezione. Essendo test un
# vettore di interi negativi, escludo quindi quegli elementi
y.test <- y[test]

# Assegno all'oggetto ridge.mod l'output della funzione glmnet, alla
# quale vengono passati come parametri la matrice x in cui vengono
# selezionate solo le righe corrispondenti ai valori di train, il vettore
# y per i soli valori corrispondenti ai valori contenuti in train,
# alpha = 0 per stimare i coefficienti beta via Ridge Regression, il
# vettore grid contenente 100 valori da assegnare come valori di lambda
# e definire una threshold pari a 1^-12 come limite per la convergenza del
# metodo
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)

# Assegno all'oggetto ridge.pred l'output della funzione predict associata
# ad un elemento di classe glmnet, passando come valore di lambda 4, e
# effettuando le nuove previsioni per le osservazioni contenute all'interno
# dell'oggetto x per il quale selezioniamo solo le righe di test, ovvero
# escludendo le osservazioni con le quali è stato effettuato il training
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])
ridge.pred[1:10]
# [1] 698.008962 582.002735 734.518396 792.181204  -8.598833  15.674911
# [7] 566.404709 594.472755 305.001520 748.732632

# Attraverso il comando mean() riportiamo il output la media degli
# scarti al quadrato tra le previsioni e i valori osservati per il
# dataset di test (MSE di test)
mean((ridge.pred - y.test)^2) # 105670.4

# Attraverso il comando mean() riportiamo il output la media
# degli scarti al quadrato tra la media di Salary nel dataset
# di train e i valori osservati per il dataset di test
# (MSE di test utilizzando il modello nullo, solo intercetta)
mean((mean(y[train]) - y.test)^2) # 204464.6

# Si assegna all'oggetto ridge.pred l'output della funzione predict
# passando un elemento di classe glmnet, lambda = 1^10 e utilizzando
# la matrice x per i soli dati test come argnomento newdata.
# L'oggetto restituisce le previsioni per le nuove osservazioni
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test, ])

# Riportiamo in output la media degli scarti al quadrato tra i valori
# predetti e i valori osservati per il dataset di test.
# Utilizzando un valore di lambda = 1^10 costringiamo i coefficienti
# ad andare verso lo zero ottenendo un MSE pari a quello utilizzando
# un modello nullo
mean((ridge.pred - y.test)^2) # 204464.6

# Si assegna all'oggetto ridge.pred l'output della funzione predict
# passando un elemento di classe glmnet, lambda = 0 e utilizzando
# la matrice x per i soli dati test come argnomento newdata.
# In questo caso passando il parametri exact = T, x (matrice dei dati di train)
# e y (vettore della risposta di train) sto dicendo di usare sostanzialmente OLS
# in quanto non vi sarà nessuna penalizzazione.
# L'oggetto restituisce le previsioni per le nuove osservazioni
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ], exact = T, x = x[train, ], y = y[train])

# Riportiamo in output la media degli scarti al quadrato tra i valori
# predetti e i valori osservati per il dataset di test.
# Utilizzando un valore di lambda = 0 è equivalente ad ottenere
# l'MSE di test usando un modello di regressione tramite OLS
mean((ridge.pred - y.test)^2) # 168588.6

# Approfondimento:
lm.fit <- lm(y ~ x, subset = train)
lm.pred <- cbind(as.matrix(rep(1, nrow(x[test,])), ncol=1), x[test,] ) %*% coef(lm.fit)
cbind(ridge = ridge.pred , lm.pred)[1:10,]
#                          s1
# -Alvin Davis       763.28869  763.28911
# -Andre Dawson     1160.00033 1160.00098
# -Andres Galarraga  521.82730  521.82598
# -Alfredo Griffin   211.50308  211.48301
# -Al Newman         404.23666  404.24314
# -Argenis Salazar    77.66183   77.66343
# -Andres Thomas     200.61342  200.62029
# -Andre Thornton    990.57057  990.56405
# -Alan Trammell    1112.75029 1112.76264
# -Alex Trevino      193.57557  193.57015

# Attraverso il comando coef applicato ad un oggetto di classe lm
# si riportano in output i coefficienti del modello di regressione lineare
coef(lm(y ~ x, subset = train))

# Attraverso il comando predict associato ad un elemento di classe glmnet
# e specificando type="coefficients" si ottengono i coefficienti del
# modello ottenuto tramite Ridge regression. In questo caso specificando
# exact = T e s = 0 (ovvero lambda = 0), quindi il termine di penalizzazione
# è nullo, otteniamo i coefficienti del modello di regressione lineare
predict(ridge.mod, s = 0, exact = T, type = "coefficients", x = x[train, ], y = y[train])[1:20, ]

# Definiamo il seme per la riproducibilità dei risultati in quanto
# verrà utilizzata la funzione cv.glmnet
set.seed(1)

# Si assegna all'oggetto cv.out l'output della funzione cv.glmnet, la quale
# ci permette di effettuare una cross validazione per poter trovare il
# valore di lambda che minimizza la media degli scarti al quadrato.
# Passiamo come parametri della funzione la matrice dei regressori selezionando
# i soli dati di train, la variabile di risposta per i soli dati di train
# e alpha = 0 per trovare i coefficienti via Ridge Regression.
# Per ogni valore di lambda scelto da una griglia di default dalla funzione
# viene partizionato il dataset in k = 10 fold (di default) e per ogni lambda
# e ogni k fold vengono stimati i beta tramite ridge utilizzando i dati di
# tutte eccetto la k esima fold e valutando l'errore tra le previsioni
# delle osservazioni della k-esima fold e le y osservate. Infine viene
# fatta una media dei 10 errori per trovare l'errore medio per il lambda
# in questione. Questa operazione viene fatta per ogni lambda all'interno
# della griglia di valori (cv.out$lambda)
?cv.glmnet
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
class(cv.out)
names(cv.out)

length(cv.out$lambda) # 100

cv.out$lambda[1:10]
# [1] 264495.8 240998.7 219589.1 200081.4 182306.7 166111.1 151354.2 137908.3
# [9] 125656.9 114493.9

# Attraverso il comando plot() associato ad un oggetto di classe cv.glmnet
# otteniamo lo scatterplot che mostra l'andamento dell'MSE ottenuto via
# cross validazione al variare dei valori di log lambda.
# I pallini rossi indicano la media degli MSE ottenuti nei 10 gruppi
# (per ogni lambda). La banda attorno ai punti è data da +- standard error,
# ovvero l'incertezza associata alla misura (MSE) per ogni valore di lambda
plot(cv.out)

# assegniamo all'oggetto bestlam il valore di lambda.min contenuto all'interno
# dell'oggetto cv.out, corrispondente al valore di lambda che minimizza l'MSE
# ottenuto via cross validazione
bestlam <- cv.out$lambda.min
bestlam # 326.0828

# Assegniamo all'oggetto ridge.pred le previsioni utilizzando i coefficienti del
# modello ridge.mod, utilizzando il valore di lambda = 326.0828, per i valori
# di test
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test, ]) 

# Calcoliamo la media della somma degli errori al quadrato tra le previsioni e
# i valori osservati di test. Come si può notare il valore è molto più basso
# rispetto a quelli calcolati in precedenza
mean((ridge.pred - y.test)^2)
# 139856.6

# Assegniamo all'oggetto out l'output della funzione glmnet, la quale ci
# permette di stimare i coefficienti di un modello di regressione lineare
# via Ridge Regression (utilizzando il parametro alpha = 0) e passando
# come regressori la matrice dei dati x e come variabile di risposta y
out <- glmnet(x, y, alpha = 0)

# Attraverso il comando predict associato ad un oggetto di classe glmnet,
# passando type = "coefficients" e s = bestlam (ovvero il lambda scelto
# via cross validazione) otteniamo i primi 20 coefficienti del miglior modello
# di regressione lineare scelto ottenuto via Ridge Regression
predict(out, type = "coefficients", s = bestlam)[1:20, ]


###########
## LASSO ##
###########

# Si assegna all'oggetto lasso.mod l'output della funzione glmnet alla
# quale vengono passati come parametri la matrice x contenente i regressori
# selezionando le solo righe i cui indici sono presenti come elementi
# del vettore train, il vettore della variabile di risposta in cui vengono
# selezionati solo gli indici i cui elementi sono presenti nel vettore
# train, alpha = 1 affinchè i coefficienti beta dal modello lineare
# vengano stimati via LASSO e il vettore grid,
# contenente una griglia di valori per il parametro lambda
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)

# Attraverso la funzione plot() applicata su un oggetto di classe glmnet
# otteniamo il grafico che mostra come variano i coefficienti stimati
# dei regressori al variare della norma L1. Ogni linea nel grafico
# corrisponde ad un diverso predittore nel modello
plot(lasso.mod)

# Definiamo il seme per la riproducibilità dei risultati in quanto
# utilizziamo la funzione cv.glmnet()
set.seed(1)

# Si assegna all'oggetto cv.out l'output della funzione cv.glmnet, la quale
# ci permette di effettuare una cross validazione per poter trovare il
# valore di lambda che minimizza la media degli scarti al quadrato.
# Passiamo come parametri della funzione la matrice dei regressori selezionando
# i soli dati di train, la variabile di risposta per i soli dati di train
# e alpha = 1 per trovare i coefficienti via LASSO.
# Per ogni valore di lambda scelto da una griglia di default dalla funzione
# viene partizionato il dataset in k = 10 fold (di default) e per ogni lambda
# e ogni k fold vengono stimati i beta tramite ridge utilizzando i dati di
# tutte eccetto la k esima fold e valutando l'errore tra le previsioni
# delle osservazioni della k-esima fold e le y osservate. Infine viene
# fatta una media dei 10 errori per trovare l'errore medio per il lambda
# in questione. Questa operazione viene fatta per ogni lambda all'interno
# della griglia di valori (cv.out$lambda)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)

# Attraverso il comando plot() associato ad un oggetto di classe cv.glmnet
# otteniamo lo scatterplot che mostra l'andamento dell'MSE ottenuto via
# cross validazione al variare dei valori di log lambda.
# I pallini rossi indicano la media degli MSE ottenuti nei 10 gruppi
# (per ogni lambda). La banda attorno ai punti è data da +- standard error,
# ovvero l'incertezza associata alla misura (MSE) per ogni valore di lambda
plot(cv.out)

# assegniamo all'oggetto bestlam il valore di lambda.min contenuto all'interno
# dell'oggetto cv.out, corrispondente al valore di lambda che minimizza l'MSE
# ottenuto via cross validazione
bestlam <- cv.out$lambda.min
bestlam # 7.025241

# Assegniamo all'oggetto lasso.pred le previsioni utilizzando i coefficienti del
# modello lasso.mod, utilizzando il valore di lambda = 7.025241, per i valori
# di test
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test, ])

# Calcoliamo la media della somma degli errori al quadrato tra le previsioni e
# i valori osservati di test. Come si può notare il valore è molto più basso
# rispetto a quelli calcolati in precedenza
mean((lasso.pred - y.test)^2) # 144038.1

# Assegniamo all'oggetto out l'output della funzione glmnet, la quale ci
# permette di stimare i coefficienti di un modello di regressione lineare
# via LASSO (utilizzando il parametro alpha = 1) e passando
# come regressori la matrice dei dati x e come variabile di risposta y
out <- glmnet(x, y, alpha = 1, lambda = grid)

# Si assegna all'oggetto lasso.coef l'output delle funzione predict()
# associata ad un oggetto di classe glmnet, passando type = "coefficients"
# e s = bestlam (ovvero il lambda scelto via cross validazione) otteniamo
# i primi 20 coefficienti del miglior modello di regressione lineare scelto
# ottenuto via LASSO
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20, ]
lasso.coef
#   (Intercept)         AtBat          Hits         HmRun          Runs
#   23.68441793   -0.37178105    3.01391941    0.00000000    0.00000000
#           RBI         Walks         Years        CAtBat         CHits
#    0.00000000    2.68842856   -2.55317456    0.00000000    0.00000000
#        CHmRun         CRuns          CRBI        CWalks       LeagueN
#    0.09451476    0.28793069    0.41837709   -0.07284111   24.96194143
#     DivisionW       PutOuts       Assists        Errors    NewLeagueN
# -118.17301728    0.24407297    0.00000000   -0.80173475    0.00000000

# Riportiamo in output i coefficienti del modello di regressione lineare
# ottenuti via LASSO utilizzando per coercizione il vettore booleano
# che assume valore TRUE se l'elemento i-esimo di lasso.coef è diverso
# da zero, FALSE viceversa. In questo modo selezioniamo solo i coefficienti
# del modello diversi da 0. LASSO, a differenza di Ridge porta rapidamente
# i coefficienti verso lo zero per via del suo vincolo, dunque utilizzando
# il metodo LASSO, si crede (per ipotesi) nella sparsità dei coefficienti
lasso.coef[lasso.coef != 0]
#   (Intercept)         AtBat          Hits         Walks         Years
#   23.68441793   -0.37178105    3.01391941    2.68842856   -2.55317456
#        CHmRun         CRuns          CRBI        CWalks       LeagueN
#    0.09451476    0.28793069    0.41837709   -0.07284111   24.96194143
#     DivisionW       PutOuts        Errors
# -118.17301728    0.24407297   -0.80173475



##########################################
## Principal Component Regression (PCR) ##
##########################################



############################################
## Partial Least Squares Regression (PLS) ##
############################################