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
# Ovvero la probabilità: P(Y=1|X), dove Y=1 è la classe codificata
# con 1 da R (vedi sotto in contrasts)
# Non avendo specificato il parametro newdata, di default utilizza
# gli stessi dati utilizzati in fase di train, ovvero il data frame
# Smarket
glm.probs <- predict(glm.fits, type = "response")

# Se non si esplicita il parametro type, di default sarà type = link
# ovvero restituisce i valori del predittore lineare (cioè la
# combinazione lineare dei coefficienti b0 + b1x1 + b2x2 + ...),
# invece con type = response trasforma i valori lineari nel loro
# nel range [0,1], quindi in questo caso essendo
# family = binomial con link logit basta sostituire il valore di
# link alla funzione 1 / (1 + exp(-valore))
?predict.glm
predict(glm.fits)[1:10] # restituisce i valori del predittore lineare

predict(glm.fits, type = "response")[1:10]
# oppure
1 / (1 + exp(-predict(glm.fits)[1:10]))

# stampiamo i primi 10 elementi di glm.probs
glm.probs[1:10]

# Attraverso il comando contrasts() verifichiamo come sono codificate
# le classi della variabile Direction
contrasts(Direction)

# Assegniamo all'oggetto glm.pred un vettore di lunghezza 1250 con
# tutti gli elementi uguali a "Down"
glm.pred <- rep("Down", 1250)

# Agli elementi di glm.pred tali per cui il corrispettivo
# elemento del vettore glm.probs ha un valore maggiore di 0.5,
# assegniamo la stringa "Up" sovrascrivendoli.
# La condizione all'interno delle parentesi quadre genera un
# vettore booleano di T o F senza assegnarlo ad alcun oggetto
glm.pred[glm.probs > .5] = "Up"

# Attraverso il comando table() otteniamo una matrice di confusione,
# o tabella di contingenza, che consente di confrontare i valori
# predetti dal modello (glm.pred) con i valori osservati reali della
# variabile di risposta (Direction)
table(glm.pred , Direction)

# Attraverso il comando mean() applicato ad una condizione logica
# che restituisce un vettore booleano, dove gli elementi saranno
# TRUE se la condizione è verificato, FALSE se non lo è.
# Dunque, il comando seguente restituisce quanti elementi sono uguali
# a TRUE sul totale
mean(glm.pred == Direction)

# Si assegna all'oggetto train un vettore booleano di lunghezza pari a
# quella di Year e i cui elementi saranno TRUE se la condizione è
# verificata, FALSE altrimenti
train <- (Year < 2005)
train[1]

train[1] == 'TRUE'  # TRUE
train[1] == 'T'     # FALSE

# Assegniamo all'oggetto Smarket.2005 il data frame che sarà composto
# dalle medesime colonne di Smarket, ma selezionando solamente le righe
# tali per cui l'elemento i-esimo di !train sarà uguale a TRUE, ovvero
# tutte le righe i cui elementi di train sono = FALSE
Smarket.2005 <- Smarket[!train,]

class(Smarket.2005) # "data.frame"

?print
print(cbind(train[1:10],!train[1:10]))

# Attraverso il comando dim() stampiamo in output le dimensioni del
# dataframe Smarket.2005
dim(Smarket.2005) # 252 righe x 9 colonne

# Assegniamo all'oggetto Direction.2005 i soli elementi dell'oggetto
# Direction tali per cui l'elemento i-esimo del vettore booleano !train
# è uguale a TRUE, selezionando pertanto un sottoinsieme dell'oggetto
# Direction
Direction.2005 <- Direction[!train]
class(Direction.2005) # "factor"

# Assegniamo all'oggetto glm.fits l'output della funzione glm() la quale
# ci permette di adattare un modello di regressione logistica
# (family = binomial) ai dati del data frame Smarket dove le righe del
# data frame sono selezionate attraverso il parametro subset = train
# che permette di selezionare l'i-esima riga di Smarket se l'elemento
# i-esimo del vettore booleano train è uguale a TRUE
glm.fits <- glm ( Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data = Smarket, family = binomial , subset = train)

# Si assegna all'oggetto glm.probs il vettore delle previsioni utilizzando
# l'output del modello di regressione logistica salvato in glm.fits,
# adattando il modello ai nuovi dati contenuti in Smarket.2005
# utilizzando il parametro type = response verranno salvati all'interno
# di glm.probs le probabilità P(Y=1|X)
glm.probs <- predict(glm.fits, Smarket.2005, type = "response")
glm.probs
class(glm.probs)

# Ripetiamo i passaggi effettuati in precedenza, ma questa volta basandoci
# sui nuovi dati (di test), non utilizzati per il training del modello
glm.pred <- rep ("Down", 252)
glm.pred[glm.probs > .5] <- "Up"

table(glm.pred , Direction.2005) # Tavola di contingenza
# glm.pred Down Up
#     Down   77 97
#     Up     34 44

mean(glm.pred == Direction.2005) # Accuracy
# 0.48

mean(glm.pred != Direction.2005) # Test error
# 0.52

# Ripetiamo gli stessi passaggi fatti in precedenza per effettuare il train
# del modello sui dati di train e tenere da parte un dataset di test, ma
# queta volta utilizziamo un modello più parsimonioso, ovvero con 2
# variabili indipendenti, Lag1 e Lag2
glm.fits <- glm (Direction ~ Lag1 + Lag2 , data = Smarket,
    family = binomial , subset = train)

glm.probs <- predict(glm.fits , Smarket.2005, type = "response")

glm.pred <- rep ("Down", 252)

glm.pred[glm.probs > .5] <- "Up"

table(glm.pred , Direction.2005) # Tavola di contingenza
# glm.pred Down  Up
#     Down   35  35
#     Up     76 106

mean(glm.pred == Direction.2005) # Accuracy
# 0.56

# Adattando un modello con meno variabili ai dati di test otteniamo
# delle performance di accuratezza più elevate

###############
## ROC CURVE ##
###############
# install.packages("pROC")
library(pROC)

# Attraverso la funzione roc() contenuta all'interno del pacchetto
# pROC alla quale vengono passati come parametri la variabile di
# risposta Direction.2005, contenente i valori osservati di test,
# il vettore delle previsioni già codificate con gli stessi livelli
# di Direction.2005, plot = T per specificare di restituire il grafico
# print.auc per stampare il valore dell'area sotto la curva all'interno
# del grafico e infine settare come colore della curva il blu.
# Si ottiene, dunque, il relativo grafico in cui viene mostrato
# sull'asse delle ascisse la specificità e sull'asse delle
# ordinate la sensitività
?roc
par(mfrow=c(1,1))
roc(Direction.2005, glm.probs, plot=T, print.auc=T, col = "blue")