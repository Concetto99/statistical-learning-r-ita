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


library(leaps)
regfit.full <- regsubsets(Salary ~ ., Hitters)
summary(regfit.full)


regfit.full <- regsubsets(Salary ~ ., data = Hitters,
 nvmax = 19)
reg.summary <- summary(regfit.full)

names(reg.summary)

reg.summary$rsq

par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables",
 ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables",
 ylab = "Adjusted RSq", type = "l")


which.max(reg.summary$adjr2)


plot(reg.summary$adjr2, xlab = "Number of Variables",
 ylab = "Adjusted RSq", type = "l")
points(11, reg.summary$adjr2[11], col = "red", cex = 2,
 pch = 20)




mmary$cp, xlab = "Number of Variables",
 ylab = "Cp", type = "l")
which.min(reg.summary$cp)

points(10, reg.summary$cp[10], col = "red", cex = 2,
 pch = 20)

which.min(reg.summary$bic)

plot(reg.summary$bic, xlab = "Number of Variables",
 ylab = "BIC", type = "l")
points(6, reg.summary$bic[6], col = "red", cex = 2,
 pch = 20)

plot(regfit.full, scale = "r2")

plot(regfit.full, scale = "adjr2")

plot(regfit.full, scale = "Cp")

plot(regfit.full, scale = "bic")

coef(regfit.full, 6)

#############################################
## Forward and Backward Stepwise Selection ##
#############################################

regfit.fwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")

summary(regfit.fwd)

regfit.bwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)

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