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
# le statistiche più importanti per le variabili contenute al suo interno.
# Per le variabili di tipo quantitativo min, max, media, quartili.
# Per le variabili qualitative le frequenze assolute di classe
summary(Smarket)

# il comando pairs() restituisce una matrice di scatterplot tra tutte le
# variabili del dataframe Smarket
pairs(Smarket)

# Attraverso il comando cor() applicato ad un oggetto di tipo data frame
# calcoliamo la matrice di correlazione tra tutte le colonne di Smarket
# Essendo l'ultima colonna non di tipo quantitativo, applichiamo la funzione
# cor() a tutte le righe del dataframe e a tutte le colonne eccetto la nona
cor(Smarket)
cor(Smarket[, -9])

attach(Smarket)

# Attraverso il comando plot() stampiamo il grafico a dispersione avente come
# asse delle ascisse l'indice degli elementi di Volume, e nell'asse delle
# ordinate la variabile Volume
plot(Volume)

# Attraverso il comando boxplot() è possibile stampare in output un grafico
# di tipo boxplot per la variabile Volume nel primo caso e per la variabile
# Volume condizionata a Direction (Variabile dicotomica "Down" ed "Up")
par(mfrow=c(1,2))
boxplot(Volume)
boxplot(Volume ~ Direction)


#####################################
####### Regressione logistica #######
#####################################

# Attraverso la funzione glm() adattiamo un modello lineare generalizzato ai
# dati utilizzando come variabile di risposta Direction e come variabili
# indipendenti Lag1, Lag2, Lag3, Lag4, Lag5 e Volume, tutte contenute nel
# dataset Smarket.
# Attraverso l'argomento family = binomial è possibile esplicitare la natura
# binaria della variabile risposta (Direction assume due modalità: ad esempio
# "Up" e "Down"), indicando che desideriamo stimare un modello di regressione
# logistica.
# Dunque, si tratta di un modello logit, in cui si stima la probabilità che la
# risposta assuma una determinata modalità (es. "Up") in funzione delle
# variabili esplicative, tramite la funzione di collegamento logit
# L'output del modello viene salvato nell’oggetto glm.fits, che contiene tutte
# le informazioni utili per l’analisi
glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
    data = Smarket, family = binomial)

# Attraverso il comando summary() associato a un oggetto di classe glm,
# riportiamo in output le informazioni riassuntive del modello stimato,
# tra cui i coefficienti, gli errori standard, i valori z, i p-value, la
# devianza e l'AIC.
# Da Coefficients si può notare che nessuna delle variabili indipendenti (Lag1,
# Lag2, Lag3, Lag4, Lag5, Volume) risulta statisticamente significativa, poiché
# tutti i valori di Pr(>|z|) sono superiori al livello di significatività
# convenzionale (es. 0.05). Questo suggerisce che le variabili considerate non
# contribuiscono in modo rilevante alla spiegazione della variabile risposta
# Direction.
# Da Null deviance, che corrisponde a 1731.2, si può notare il grado di
# devianza (cioè la misura di "errore") del modello senza predittori, ovvero
# con solo l’intercetta. Indica quanto il modello più semplice si discosta dai
# dati osservati.
# Da Residual deviance, che corrisponde a 1727.6, si può notare la devianza
# del modello completo, cioè quello che include tutte le variabili indipendenti.
# Poiché la differenza tra Null deviance e Residual deviance è molto piccola, si
# deduce che l’aggiunta dei predittori non migliora significativamente il
# modello rispetto a quello con solo intercetta.
# Da AIC, che è pari a 1741.6, si può notare una misura della qualità del
# modello che tiene conto sia della bontà di adattamento sia della complessità
# (numero di parametri). Un AIC più basso è preferibile, ma in questo caso il
# valore elevato conferma la scarsa capacità predittiva del modello.
# Inoltre, l’algoritmo si vede che converge in 3 iterazioni, ovvero sono stati
# necessari tre passaggi del metodo di stima (Fisher Scoring) per raggiungere
# la convergenza, cioè una situazione in cui le stime dei parametri si
# stabilizzano. Questo è del tutto normale e indica che l’algoritmo ha
# funzionato correttamente.
summary(glm.fits)

# Call:
# glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 +
#     Volume, family = binomial, data = Smarket)
# 
# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)
# (Intercept) -0.126000   0.240736  -0.523    0.601
# Lag1        -0.073074   0.050167  -1.457    0.145
# Lag2        -0.042301   0.050086  -0.845    0.398
# Lag3         0.011085   0.049939   0.222    0.824
# Lag4         0.009359   0.049974   0.187    0.851
# Lag5         0.010313   0.049511   0.208    0.835
# Volume       0.135441   0.158360   0.855    0.392
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 1731.2  on 1249  degrees of freedom
# Residual deviance: 1727.6  on 1243  degrees of freedom
# AIC: 1741.6
# 
# Number of Fisher Scoring iterations: 3

# Attraverso il comando coef() stampiamo i coefficienti di glm.fits
coef(glm.fits)

# Applicando all'oggetto glm.fits il $ seguito da coefficients
# accediamo all'elemento di glm.fits che contiene i coefficienti
# del modello, i quali verranno riportati in output
glm.fits$coefficients

# Attraverso il comando summary() applicato ad un elemento di classe
# glm e dal quale richiamiamo l'elemento coef ci restituisce in
# output la matrice dei coefficienti e degli standard error,
# p-value ecc
summary(glm.fits)$coef

# Attraverso il comando summary() applicato ad un elemento di classe
# glm e dal quale richiamiamo l'elemento coef[,4] ci restituisce in
# output il vettore avente tutte le righe della 4a colonna della
# matrice dei coefficienti, ovvero i p-value.
summary(glm.fits)$coef[, 4]

# Assegniamo all'oggetto glm.probs l'output ottenuto attraverso la
# funzione predict() alla quale passiamo l'oggetto glm.fits e
# specifichiamo type = "response" affinchè ci restituisca le
# probabilità condizionate come previsioni.
# Ovvero la probabilità che xi appartenga alla classe codificata
# con 1 ("Up")
glm.probs <- predict(glm.fits, type = "response")

# stampiamo i primi 10 elementi di glm.probs
glm.probs[1:10]

# Attraverso il comando contrasts() verifichiamo come sono codificate
# le classi della variabile Direction
contrasts(Direction)

glm.pred <- rep("Down", 1250)

glm.pred[glm.probs > .5] = "Up"

table (glm.pred , Direction)

mean (glm.pred == Direction)

train <- (Year < 2005)

Smarket.2005 <- Smarket[!train , ]

dim (Smarket.2005)

Direction.2005 <- Direction[!train]

glm.fits <- glm ( Direction ∼ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data = Smarket, family = binomial , subset = train)

glm.probs <- predict(glm.fits, Smarket.2005, type = "response")

glm.pred <- rep ("Down", 252)
glm.pred[glm.probs > .5] <- "Up"

table (glm.pred , Direction.2005)

mean (glm.pred == Direction.2005)

mean (glm.pred != Direction.2005)

glm.fits <- glm (Direction ∼ Lag1 + Lag2 , data = Smarket, family = binomial , subset = train)

glm.probs <- predict (glm.fits , Smarket.2005, type = "response")
glm.pred <- rep ("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table (glm.pred , Direction.2005)
mean (glm.pred == Direction.2005)

install.packages("pROC")
library(pROC)

roc(Direction.2005, glm.probs, plot=T, print.auc=T, col = "blue")
predict (glm.fits, newdata = data.frame (Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type = "response")

