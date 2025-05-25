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

#
regfit.fwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
#
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)

#
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)





######################
## Ridge Regression ##
######################



###########
## LASSO ##
###########



##########################################
## Principal Component Regression (PCR) ##
##########################################



############################################
## Partial Least Squares Regression (PLS) ##
############################################