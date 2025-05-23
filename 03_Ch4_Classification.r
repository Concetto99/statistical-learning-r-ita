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


##################################################
####### Linear Discriminant Analysis (LDA) #######
##################################################

# L'analisi discriminante lineare (LDA) è una tecnica di apprendimento
# statistico supervisionato utilizzata per la classificazione.
# Permette di assegnare una nuova osservazione a una classe, date
# le sue caratteristiche.
# Per fare ciò si utilizza una funzione discriminante lineare, calcolata
# per ciascuna classe, che tiene conto delle probabilità a priori
# e delle densità di probabilità nella k-esima classe. L'osservazione
# viene assegnata alla classe per cui tale funzione assume il valore massimo.

library(MASS) # Carichiamo il pacchetto MASS

# Assegniamo all'oggetto lda.fit l'output della funzione lda() contenuta
# all'interno del pacchetto MASS, la quale permette di eseguire tutte
# le istruzioni per effettuare un'analisi discriminante lineare.
# Passiamo come parametri della funzione la specificazione del modello
# in cui abbiamo come variabile di risposta Direction e come variabili
# indipendenti Lag1 e Lag2, contenute nel dataset Smarket, nel quale
# verranno selezionate un sottoinsieme di righe per il training del
# modello, nello specifico le righe per cui alla posizione i-esima
# del vettore booleano train è presente TRUE
lda.fit <- lda(Direction ~ Lag1 + Lag2 , data = Smarket , subset = train)

# Eseguendo lda.fit otteniamo l'output della funzione lda() salvata
# in precedenza. lda.fit è un oggetto di classe lda.
lda.fit
class(lda.fit) # Per vedere di che classe
names(lda.fit) # Per vedere cosa contiene

# Call:
# lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
# 
# Prior probabilities of groups:
#     Down       Up
# 0.491984 0.508016
# 
# Group means:
#             Lag1        Lag2
# Down  0.04279022  0.03389409
# Up   -0.03954635 -0.03132544
# 
# Coefficients of linear discriminants:
#             LD1
# Lag1 -0.6420190
# Lag2 -0.5135293

# L'output mostra:
# - La chiamata alla funzione;
# - Le probabilità a priori di ciascuna classe, stimate dai dati di training;
# - Le medie delle variabili predittive per ciascuna classe ('Down' e 'Up');
# - I coefficienti della funzione discriminante lineare (LD1).

# La funzione discriminante LD1 può essere scritta esplicitamente come:
#   LD1 = -0.64 * Lag1 - 0.51 * Lag2

# Il plot() di un oggetto lda crea una rappresentazione
# grafica delle funzioni discriminanti lineari, cioè delle nuove variabili
# (combinazioni lineari dei predittori) che LDA utilizza per discriminare
# tra le classi.
?plot.lda
plot(lda.fit)

# Se le due curve (Down/Up) sono ben separate, allora LDA ha trovato una buona
# direzione discriminante.
# Se invece si sovrappongono molto, come in questo caso, significa che i
# predittori Lag1 e Lag2 non sono sufficienti a discriminare bene le due classi.

# Assegniamo all'oggetto lda.pred l'output della funzione predict()
# attraverso la quale otteniamo delle previsioni per le osservazioni
# contenute nel dataset Smarket.2005 utilizzando l'output dell'analisi
# discriminante lineare ottenuto utilizzando i dati di train precedentemente
lda.pred <- predict(lda.fit, Smarket.2005)

# la funzione names() ci restituisce i nomi degli argomenti contenuti
# nell'oggetto lda.pred
names(lda.pred)

# l'argomento class di lda.pred contiene le classi predette per ogni
# osservazione del dataset di test. Stampiamo solo i primi 10 elementi
lda.pred$class[1:10]

# l'argomento posterior di lda.pred contiene le probabilità a posteriori
# di ogni osservazione per ogni classe. stampiamo solo le prime 10 righe
# e tutte le colonne
lda.pred$posterior[1:10,]

# l'argomento x di lda.pred contiene i valori di LD1 ottenuti utilizzando
# i coefficienti delle discriminanti lineari, le variabili Lag1 e Lag2 sono
# centrate, quindi viene sottratta la loro media di gruppo
lda.pred$x[1:10]
#  0.08293096  0.59114102  1.16723063  0.83335022 -0.03792892 -0.08743142
# -0.14512719  0.21701324  0.05873792  0.35068642

# Il primo valore lda.pred$x[1] sarà:
-0.6420190 * (Smarket.2005[1,c("Lag1")] - mean(Smarket[train,c("Lag1")])) -0.5135293 * (Smarket.2005[1,c("Lag2")] - mean(Smarket[train,c("Lag2")]))
# 0.08293095

# Assegniamo all'oggetto lda.class il vettore contenente le previsioni
# di classe per ogni osservazione del dataset di test
lda.class <- lda.pred$class

# Attraverso il comando table() otteniamo una tabella di contingenza
# attraverso la quale possiamo vedere le osservazioni predette correttamente
# e quante di queste invece sono state misclassificate
table(lda.class, Direction.2005)
# lda.class Down  Up
#      Down   35  35
#      Up     76 106

# Accuracy
mean(lda.class == Direction.2005)
# oppure
(35+106)/(35+106+35+76)

# attraverso il comando sum() applicato ad vettore booleano che assume
# valore TRUE se l'elemento della prima colonna di posterior è > o =
# alla soglia di 0.5, FALSE altrimenti, otteniamo la somma di tutti
# gli elementi che rispettano la condizione appena citata
sum(lda.pred$posterior[, 1] >= .5)
# 70

# attraverso il comando sum() applicato ad vettore booleano che assume
# valore TRUE se l'elemento della prima colonna di posterior è <
# della soglia di 0.5, FALSE altrimenti, otteniamo la somma di tutti
# gli elementi che rispettano la condizione appena citata
sum(lda.pred$posterior[, 1] < .5)
# 182

# otteniamo in output le prime 20 righe della prima colonna di posterior
lda.pred$posterior[1:20, 1]

# otteniamo in output i primi 20 elementi di lda.class
lda.class[1:20]

# attraverso il comando sum() applicato ad vettore booleano che assume
# valore TRUE se l'elemento della prima colonna di posterior è >
# della soglia di 0.9, FALSE altrimenti, otteniamo la somma di tutti
# gli elementi che rispettano la condizione appena citata
sum(lda.pred$posterior[, 1] > .9)
# 0

###############
## ROC CURVE ##
###############

# Attraverso la funzione roc() contenuta all'interno del pacchetto
# pROC alla quale vengono passati come parametri la variabile di
# risposta Direction.2005, contenente i valori osservati di test,
# il vettore delle probabilità a posteriori lda.pred$posterior[,1]
# contenente le probabilità a posteriori per la classe "Down",
# plot = T per specificare di restituire il grafico.
# Si ottiene, dunque, il relativo grafico in cui viene mostrato
# sull'asse delle ascisse la specificità e sull'asse delle
# ordinate la sensitività
roc(Direction.2005, lda.pred$posterior[,1], plot=T)

#  direction: controls > cases
# 
# Call:
# roc.default(response = Direction.2005, predictor = lda.pred$posterior[,     1], plot = T)
# 
# Data: lda.pred$posterior[, 1] in 111 controls (Direction.2005 Down) > 141 cases (Direction.2005 Up).
# Area under the curve: 0.5584


#####################################################
####### Quadratic Discriminant Analysis (QDA) #######
#####################################################

# QDA (Quadratic Discriminant Analysis) è una tecnica di apprendimento
# supervisionato utilizzata per la classificazione. A differenza di LDA,
# che assume che le classi condividano una matrice di covarianza comune,
# QDA permette che ogni classe abbia la propria matrice di covarianza.
# Questo rende la funzione discriminante quadratica nei predittori.
# Di conseguenza, QDA è più flessibile di LDA, ma può richiedere più
# dati per stimare accuratamente i parametri.

# Si adatta un modello di analisi discriminante quadratica per mezzo della
# funzione qda() contenuta all'interno del pacchetto MASS.
# Il modello viene stimato utilizzando un sottoinsieme del dataset Smarket,
# in particolare le osservazioni in cui il vettore booleano 'train' assume
# valore TRUE.
# La variabile di risposta è 'Direction', mentre le variabili predittive
# sono 'Lag1' e 'Lag2'.
qda.fit <- qda(Direction ~ Lag1 + Lag2 , data = Smarket , subset = train)

# Stampa dell'oggetto qda.fit che contiene le informazioni del modello stimato
qda.fit

# Call:
# qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
# 
# Prior probabilities of groups:
#     Down       Up
# 0.491984 0.508016
# 
# Group means:
#             Lag1        Lag2
# Down  0.04279022  0.03389409
# Up   -0.03954635 -0.03132544

# L'output mostra:
# - La chiamata alla funzione utilizzata per stimare il modello
# - Le probabilità a priori dei gruppi: rappresentano la proporzione
# di osservazioni nel training set appartenenti a ciascuna classe (Down e Up)
# - Le medie condizionate di gruppo: rappresentano la media dei
# predittori (Lag1 e Lag2) all'interno di ciascuna classe (Down e Up).
# Questi valori vengono usati per stimare la densità normale multivariata
# per ciascun gruppo.

# Applichiamo il modello QDA stimato precedentemente (qda.fit) al sottoinsieme
# di dati contenuto in 'Smarket.2005', al fine di ottenere
# le previsioni per ciascuna osservazione. La funzione predict() restituisce
# un oggetto che include la classe predetta, le probabilità posteriori e i
# valori discriminanti.
qda.pred <- predict(qda.fit, Smarket.2005)

# Assegniamo all'oggetto qda.class il risultato della predizione (qda.pred)
# il vettore delle classi previste per ciascuna osservazione del dataset
# 'Smarket.2005'.
# Questo indica per ogni osservazione se il modello prevede 'Up' o 'Down'.
qda.class <- predict(qda.fit , Smarket.2005)$class
qda.class[1:10]  # Visualizziamo le prime 10 classi previste

# Assegniamo all'oggetto qda.posterior il risultato della predizione delle
# probabilità a posteriori per ciascuna classe.
# Ogni riga della matrice rappresenta un'osservazione e ogni colonna la
# probabilità stimata che l'osservazione appartenga a ciascuna delle classi,
# date le sue caratteristiche.
# Queste probabilità sono calcolate utilizzando la formula di Bayes.
qda.posterior <- predict(qda.fit , Smarket.2005)$posterior
qda.posterior[1:10,]  # Visualizziamo le prime 10 righe delle probabilità a posteriori


# tabella di contingenza
table(qda.class , Direction.2005)

#          Direction.2005
# qda.class Down  Up
#      Down   30  20
#      Up     81 121

# Accuracy
mean(qda.class == Direction.2005)

# Attraverso la funzione roc() contenuta all'interno del pacchetto
# pROC alla quale vengono passati come parametri la variabile di
# risposta Direction.2005, contenente i valori osservati di test,
# il vettore delle probabilità a posteriori lda.pred$posterior[,2]
# contenente le probabilità a posteriori per la classe "Up",
# plot = T per specificare di restituire il grafico.
# print.auc per stampare il valore dell'area sotto la curva all'interno
# del grafico e infine settare come colore della curva il magenta
# Si ottiene, dunque, il relativo grafico in cui viene mostrato
# sull'asse delle ascisse la specificità e sull'asse delle
# ordinate la sensitività
roc(Direction.2005, qda.pred$posterior[,2], plot=T, print.auc=T, col = "magenta")

# Setting levels: control = Down, case = Up
# Setting direction: controls < cases
# 
# Call:
# roc.default(response = Direction.2005, predictor = qda.pred$posterior[,     2], plot = T, print.auc = T, col = "magenta")
# 
# Data: qda.pred$posterior[, 2] in 111 controls (Direction.2005 Down) < 141 cases (Direction.2005 Up).
# Area under the curve: 0.562



###########################
####### Naive Bayes #######
###########################

# Carichiamo il pacchetto 'e1071' che contiene la funzione naiveBayes()
library(e1071)

# Adattiamo un modello Naive Bayes ai dati di training (Smarket[train, ])
# in cui la variabile di risposta è 'Direction' e i predittori sono 'Lag1'
# e 'Lag2'. L'oggetto nb.fit conterrà i parametri stimati del modello.
nb.fit <- naiveBayes(Direction ~ Lag1 + Lag2 , data = Smarket , subset = train)

# Stampiamo l'oggetto nb.fit per visualizzare:
# - Le probabilità a priori delle classi (Down/Up)
# - Le medie e deviazioni standard dei predittori condizionate alla classe
# (assumendo normalità)
nb.fit

# Call:
# naiveBayes.default(x = X, y = Y, laplace = laplace)
# 
# A-priori probabilities:
# Y
#     Down       Up
# 0.491984 0.508016
# 
# Conditional probabilities:
#       Lag1
# Y             [,1]     [,2]
#   Down  0.04279022 1.227446
#   Up   -0.03954635 1.231668
# 
#       Lag2
# Y             [,1]     [,2]
#   Down  0.03389409 1.239191
#   Up   -0.03132544 1.220765

# Output:
# - "A-priori probabilities": proporzioni delle classi nel training set
# - "Conditional probabilities": per ciascuna classe, media ([,1]) e
# deviazione standard ([,2]) di ogni predittore (Lag1 e Lag2), ipotizzando
# distribuzione normale.

# Verifichiamo che la media condizionata di Lag1 per la classe 'Down' coincida
# con quella nel modello
mean(Lag1[train][Direction[train] == "Down"])  # dovrebbe essere ≈ 0.04279

# Verifichiamo anche la deviazione standard stimata di Lag1 per la classe 'Down'
sd(Lag1[train][Direction[train] == "Down"])     # dovrebbe essere ≈ 1.227

# Usiamo il modello Naive Bayes per predire la direzione di mercato
# (Smarket.2005), 'nb.class' conterrà le classi previste ('Up' o 'Down')
# per ciascuna osservazione
nb.class <- predict(nb.fit , Smarket.2005)
nb.class[1:5]

# Confrontiamo le classi previste con quelle osservate (Direction.2005)
# tramite una tabella di contingenza
table(nb.class, Direction.2005)

# Calcoliamo la proporzione di previsioni corrette, cioè l'accuratezza del
# modello sul test set
mean(nb.class == Direction.2005)

# Possiamo anche richiedere le probabilità predette per ciascuna classe
# (anziché solo la classe più probabile)
# Impostando type = "raw", otteniamo una matrice con le probabilità a
# posteriori per ciascuna osservazione
nb.preds <- predict(nb.fit, Smarket.2005, type = "raw")
nb.preds[1:5, ]  # Mostriamo le probabilità delle prime 5 osservazioni

# Attraverso la funzione roc() contenuta all'interno del pacchetto
# pROC alla quale vengono passati come parametri la variabile di
# risposta Direction.2005, contenente i valori osservati di test,
# il vettore delle probabilità a posteriori lda.pred$posterior[,2]
# contenente le probabilità a posteriori per la classe "Up",
# plot = T per specificare di restituire il grafico.
# print.auc per stampare il valore dell'area sotto la curva all'interno
# del grafico e infine settare come colore della curva il verde
# Si ottiene, dunque, il relativo grafico in cui viene mostrato
# sull'asse delle ascisse la specificità e sull'asse delle
# ordinate la sensitività
roc(Direction.2005, nb.preds[,2], plot=T, print.auc=T, col = "green")


###################################
####### K-Nearest Neighbors #######
###################################

########################################################
# Esempio (1) con gli stessi dati usati in precedenza ##
########################################################

# Attraverso il comando library carico il pacchetto "class"
library(class)

# Si assegna all'oggetto train.X la matrice avente come colonne
# Lag1 e Lag2 contenute nel dataframe Smarket, in cui righe sono
# selezionate e coincidono con gli indici per cui il vettore train
# ha come elemento TRUE. Il comando cbind() ci permette di affiancare
# i due vettori Lag1 e Lag2 per colonna
train.X <- cbind(Lag1, Lag2)[train, ]
class(train.X)
train.X[1:10,]

# Si assegna all'oggetto test.X la matrice avente come colonne
# Lag1 e Lag2 contenute nel dataframe Smarket, in cui righe sono
# selezionate e coincidono con gli indici per cui la negazione del
# vettore train ha come elemento TRUE, ovvero dove train è FALSE.
# Il comando cbind() ci permette di affiancare i due vettori Lag1
# e Lag2 per colonna
test.X <- cbind(Lag1 , Lag2)[!train , ]
test.X[1:10,]

# Si assegna all'oggetto train.Direction in vettore contenente gli
# elementi del vettore Direction di Smarket selezionati in modo tale
# da estrarre solamente gli indici per i quali il vettore booleano train
# ha come elemento TRUE
train.Direction <- Direction[train]

# Attraverso il comando set.seed() si definisce un seme utile alla
# riproducibilità dei risultati, in questo caso perchè utilizziamo
# la funzione knn() in seguito che potrebbe avere dei vicini più
# vicini equidistanti e sceglierne dunque uno dei due
set.seed(1)

?knn
# Si assegna all'oggetto knn.pred l'output della funzione knn(),
# contenuta nel pacchetto 'class', che implementa il metodo
# k-Nearest Neighbors.
# Come parametri passiamo:
# - train.X: matrice (o data frame) delle variabili predittive del set di training (Lag1 e Lag2)
# - test.X: matrice delle variabili predittive del set di test
# - train.Direction: fattore contenente le etichette di classe ("Up" o "Down") del training set
# - k = 1: indica che si considera 1 solo vicino più vicino per la classificazione
# - prob = TRUE: specifica che si vogliono ottenere anche le probabilità a posteriori
#   (ossia, la proporzione di voti per la classe assegnata — che sarà sempre 1 quando k = 1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1, prob = TRUE)

# Si assegna all'oggetto knn.pred l'output della funzione knn(),
# contenuta nel pacchetto 'class', che implementa il metodo
# k-Nearest Neighbors.
# Come parametri passiamo:
# - train.X: matrice (o data frame) delle variabili predittive del set di training (Lag1 e Lag2)
# - test.X: matrice delle variabili predittive del set di test
# - train.Direction: fattore contenente le etichette di classe ("Up" o "Down") del training set
# - k = 1: indica che si considera 3 vicini più vicini per la classificazione
# - prob = TRUE: specifica che si vogliono ottenere anche le probabilità a posteriori
#   (ossia, la proporzione di voti per la classe assegnata)
knn.pred <- knn(train.X, test.X, train.Direction, k = 3,  prob = TRUE)
knn.pred

# Tabella di contingenza
table(knn.pred , Direction.2005)

# il seguente comando ci restituisce il numero totale dei casi per cui
# la condizione logica knn.pred == Direction.2005 è verificata sul
# totale (dimensione dei vettori)
mean(knn.pred == Direction.2005)
# Circa il 53% dei valori è uguale, mentre il 47% non lo è. Questo numero ci
# da un'indicazione su quanto il modello è in grado di predire correttamente
# la classe ad ogni osservazione

# Attraverso la funzione roc() contenuta all'interno del pacchetto
# pROC alla quale vengono passati come parametri la variabile di
# risposta Direction.2005, contenente i valori osservati di test,
# il vettore delle probabilità a posteriori attributes(knn.pred)$prob
# contenente le probabilità a posteriori per la classe "Up",
# plot = T per specificare di restituire il grafico.
# print.auc per stampare il valore dell'area sotto la curva all'interno
# del grafico e infine settare come colore della curva il nero
# Si ottiene, dunque, il relativo grafico in cui viene mostrato
# sull'asse delle ascisse la specificità e sull'asse delle
# ordinate la sensitività
roc(Direction.2005, attributes(knn.pred)$prob, plot=T, print.auc=T, col = "black")

################################################
## Esempio (2) con i dati del dataset Caravan ##
################################################

# Il comando summary() applicato ad un oggetto di tipo data frame ci restituisce
# min, max, 1,2,3 quartile e media se la delle variabili quantitative o le
# frequenze di classe per le variabili qualitative
summary(Caravan)

# il comando dim() restituisce le dimensioni [n. righe, n. colonne] del dataset
# Caravan
dim(Caravan)

# Assegniamo le variabili del data frame come oggetti accessibili sul nostro
# Environment
attach(Caravan)

# Il comando summary() applicato ad una variabile qualitativa ci restituisce
# le frequenze assolute per le classi di Purchase
summary(Purchase)

# Assegniamo all'oggetto standardized.X la matrice dei dati contenente tutte
# le righe di Caravan e tutte le colonne standardizzate, eccetto la 86esima
standardized.X <- scale(Caravan[, -86])
class(standardized.X) # "matrix" "array"

# Attraverso il comando var() applicato al vettore contente tutte le righe
# e la prima colonna di Caravan riportiamo in output la varianza della prima
# colonna di Carvan
var(Caravan[, 1])

# Attraverso il comando var() applicato al vettore contente tutte le righe
# e la seconda colonna di Caravan riportiamo in output la varianza della seconda
# colonna di Carvan
var(Caravan[, 2])

# Attraverso il comando var() applicato al vettore contente tutte le righe
# e la prima colonna di Caravan riportiamo in output la varianza della prima
# colonna di standardized.X
# Essendo una variabile standardizzata è a media pari a zero e deviazione
# standard pari a 1
var(standardized.X[, 1])

# Attraverso il comando var() applicato al vettore contente tutte le righe
# e la seconda colonna di Caravan riportiamo in output la varianza della seconda
# colonna di standardized.X
# Essendo una variabile standardizzata è a media pari a zero e deviazione
# standard pari a 1
var(standardized.X[, 2])

# Si assegna all'oggetto test, il vettore contenente i numeri interi da 1 a 1000
test <- 1:1000

# Si assegna all'oggetto train.X la matrice contenente tutte le righe eccetto le
# prime 1000 e tutte le colonne di standardized.X
train.X <- standardized.X[-test, ]
train.X[1:10,]

# Si assegna all'oggetto test.X la matrice contenente le prime 1000 righe
# e tutte le colonne di standardized.X
test.X <- standardized.X[test, ]
test.X[1:10,]

# Si assegna all'oggetto train.Y il vettore contenente tutti gli elementi
# eccetto i primi 1000 della variabile Purchase
train.Y <- Purchase[-test]

# Si assegna all'oggetto test.Y il vettore contenente i primi 1000 elementi
# della variabile Purchase
test.Y <- Purchase[test]

# Attraverso il comando set.seed() si definisce un seme utile alla
# riproducibilità dei risultati, in questo caso perchè utilizziamo
# la funzione knn() in seguito che potrebbe avere dei vicini più
# vicini equidistanti e sceglierne dunque uno dei due
set.seed(1)

?knn
# Si assegna all'oggetto knn.predCaravan l'output della funzione knn(),
# contenuta nel pacchetto 'class', che implementa il metodo
# k-Nearest Neighbors.
# Come parametri passiamo:
# - train.X: matrice (o data frame) delle variabili predittive del set di training
# - test.X: matrice delle variabili predittive del set di test
# - train.Y: fattore contenente le etichette di classe ("Yes" o "No") del training set
# - k = 1: indica che si considera 1 solo vicino più vicino per la classificazione
knn.predCaravan <- knn(train.X, test.X, train.Y, k = 1)

# il seguente comando ci restituisce il numero totale dei casi per cui
# la condizione logica test.Y != knn.pred è verificata sul
# totale (dimensione dei vettori)
mean(test.Y != knn.pred)
# Questo valore ci da un'indicazione di quanto sbaglia il classificatore,
# o anche definito come misclassification rate
# In questo caso vediamo che il classificatore sbaglia nel prevedere la
# variabile Purchase per le nuove osservazioni del set di test circa il
# 12% delle volte

# il seguente comando ci restituisce il numero totale di volte in cui
# gli elementi all'interno dell'oggetto test.Y sono diversi dalla stringa
# "No" sul totale degli elementi
mean(test.Y != "No")
test.Y[1:20]
# circa il 6% degli elementi in test.Y sono diversi da "No"

# Attraverso il comando table() applicato ad una variabile qualitativa
# ci restituisce le frequenze assolute di test.Y
table(test.Y)
#  No Yes
# 941  59

59 / (941 + 59) # 0.059

# Attraverso il comando table() applicato ad una variabile qualitativa
# ci restituisce le frequenze assolute di test.Y
table(train.Y)
#   No  Yes
# 4533  289

# Tabella di contingenza
table(knn.predCaravan, test.Y)
#         test.Y
# knn.pred  No Yes
#      No  873  50
#      Yes  68   9

# La classe Purchase=Yes viene predetta correttamente per le nuove osservazioni
# usando il classificatore KNN con K=1 solo l'11% delle volte, circa l'89% delle
# volte viene misclassificata
9 / (68 + 9) # 0.1168831

# Si assegna all'oggetto knn.predCaravan l'output della funzione knn(),
# contenuta nel pacchetto 'class', che implementa il metodo
# k-Nearest Neighbors.
# Come parametri passiamo:
# - train.X: matrice (o data frame) delle variabili predittive del set di training
# - test.X: matrice delle variabili predittive del set di test
# - train.Y: fattore contenente le etichette di classe ("Yes" o "No") del training set
# - k = 1: indica che si considera 3 vicini più vicini per la classificazione
knn.predCaravan <- knn(train.X, test.X, train.Y, k = 3)

# Tabella di contingenza
table(knn.predCaravan, test.Y)
#         test.Y
# knn.pred  No Yes
#      No  920  54
#      Yes  21   5

# Si assegna all'oggetto knn.predCaravan l'output della funzione knn(),
# contenuta nel pacchetto 'class', che implementa il metodo
# k-Nearest Neighbors.
# Come parametri passiamo:
# - train.X: matrice (o data frame) delle variabili predittive del set di training
# - test.X: matrice delle variabili predittive del set di test
# - train.Y: fattore contenente le etichette di classe ("Yes" o "No") del training set
# - k = 1: indica che si considera 5 vicini più vicini per la classificazione
knn.predCaravan <- knn(train.X, test.X, train.Y, k = 5)

# Tabella di contingenza
table(knn.predCaravan, test.Y)
#         test.Y
# knn.pred  No Yes
#      No  930  55
#      Yes  11   4



###############################################
## Confronto tra Logistica e KNN per Caravan ##
###############################################

# Attraverso la funzione glm() adattiamo un modello lineare generalizzato ai
# dati utilizzando come variabile di risposta Purchase e come variabili
# indipendenti tutte le altre colonne contenute nel dataset Caravan, per
# il training del modello si considerano quindi tutte le righe eccetto le
# prime 1000 (subset = -test)
# Attraverso l'argomento family = binomial è possibile esplicitare la natura
# binaria della variabile risposta (Purchase assume due modalità: ad esempio
# "Yes" e "No"), indicando che desideriamo stimare un modello di regressione
# logistica.
# Dunque, si tratta di un modello logit, in cui si stima la probabilità che la
# risposta assuma una determinata modalità (es. "Up") in funzione delle
# variabili esplicative, tramite la funzione di collegamento logit
# L'output del modello viene salvato nell’oggetto glm.fits, che contiene tutte
# le informazioni utili per l’analisi
attach(Caravan)
glm.fitsCaravan <- glm(Purchase ~ ., data = Caravan, family = binomial, subset = -test)

# Si assegna all'oggetto glm.probsCaravan il vettore delle probabilità a posteriori
# ottenute utilizzando la funzione predict attraverso la quale adattiamo il
# modello contenuto in glm.fitsCaravan ai nuovi dati Caravan[test, ]e con type =
# response viene restituita la probabilità che quell'osservazione appartenga
# alla classe categorizzata con 1 date le sue caratteristiche
glm.probsCaravan <- predict(glm.fitsCaravan, Caravan[test, ], type = "response")
class(glm.probs)

# Si assegna all'oggetto glm.predCaravan un vettore di lunghezza 1000 avente come
# elementi la stringa "No" ripetuta 1000 volte
glm.predCaravan <- rep("No", 1000)


# Updatiamo il vettore glm.predCaravan assegniando la stringa "Yes" per tutti quegli
# elementi per cui nel medesimo indice in glm.probs è presente un valore
# maggiore di 0.5
glm.predCaravan[glm.probsCaravan > .5] <- "Yes"

# Tabella di contingenza
table(glm.predCaravan , test.Y)
#         test.Y
# glm.pred  No Yes
#      No  934  59
#      Yes   7   0

# Si assegna all'oggetto glm.predCaravan un vettore di lunghezza 1000 avente come
# elementi la stringa "No" ripetuta 1000 volte
glm.predCaravan <- rep("No", 1000)

# Updatiamo il vettore glm.predCaravan assegniando la stringa "Yes" per tutti quegli
# elementi per cui nel medesimo indice in glm.probs è presente un valore
# maggiore di 0.25 (abbassiamo la soglia)
glm.predCaravan[glm.probsCaravan > .25] <- "Yes"

# tabella di contingenza 
table(glm.predCaravan , test.Y)
#         test.Y
# glm.pred  No Yes
#      No  934  59
#      Yes   7   0

# Il risultato non cambia

# Accuracy logistica
mean(glm.predCaravan == test.Y)

# Accuracy KNN
mean(test.Y == knn.pred)

# L'accuracy tra KNN e logistica è la stessa, cambia però la
# "capacità" per la logistica di prevedere la categoria
# Purchase= "Yes", qualcosa di simile accade anche utilizzando
# il knn come classificatore ma è meno accentuato

# Questo è un problema principalmente in quanto ci troviamo di
# fronte ad un dataset sbilanciato


####################################################
## Confronto tra curve ROC per il dataset Smarket ##
####################################################

# Si creano gli oggetti contenenti l'output per creare il grafico
# di confronto delle curve ROC tra i vari modelli per il dataset
# Smarket
?roc
roc.logistica <- roc(Direction.2005, glm.probs, plot=T, print.auc=T, col = "blue")
roc.lda <- roc(Direction.2005, lda.pred$posterior[,1], plot=T, print.auc=T, col = "yellow")
roc.qda <- roc(Direction.2005, qda.pred$posterior[,2], plot=T, print.auc=T, col = "magenta")
roc.nb <- roc(Direction.2005, nb.preds[,2], plot=T, print.auc=T, col = "green")
roc.knn <- roc(Direction.2005, attributes(knn.pred)$prob, plot=T, print.auc=T, col = "black")

# Si assegna all'oggetto lista.roc la lista contenente gli oggetti di classe
# roc salvati in precedenza
lista.roc <- list(Logistica=roc.logistica, LDA=roc.lda, QDA=roc.qda, NB=roc.nb, KNN=roc.knn)

# Si carica il pacchetto ggplot2 contenente le principali funzioni
# per la creazione di visualizzazioni grafiche
library(ggplot2)

# Si utilizza la funzione ggroc per creare il grafico contenti le
# curve ROC per i 5 modelli di classificazione
?ggroc
ggroc(lista.roc) + labs(color='Method') + theme(legend.position="bottom") + ggtitle("ROC curve")