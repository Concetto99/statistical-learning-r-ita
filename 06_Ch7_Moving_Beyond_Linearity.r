############################################
#### [322] 7.8 Lab: Non-linear Modeling ####
############################################

# Creazione directory per salvare i grafici

if (!dir.exists("img")) {
  dir.create("img")
}

if (!dir.exists("img/06_Ch7_Moving_Beyond_Linearity")) {
  dir.create("img/06_Ch7_Moving_Beyond_Linearity")
}

img_path = "img/06_Ch7_Moving_Beyond_Linearity"

remove(list = ls())

# Attraverso il comando library() carichiamo il pacchetto ISLR,
# il quale contiene vari dataset e funzioni utili.
library(ISLR)

# Attraverso il comando attach rendiamo direttamente disponibili
# le variabili del dataset Wage all'interno del global environment
attach(Wage)

# Attraverso il comando head() riportiamo in output le prime 6 righe
# del dataset Wage
head(Wage)

# Attraverso il comando summary() applicato ad un ogetto di tipo
# data frame riportiamo in output le principali informazioni delle
# colonne di Wage, per le variabili qualitative le frequenze assolute
# di classe, per le quantitative min, max, media, 1,2,3 quartile
summary(Wage)

# Attraverso la funzione pairs() stampiamo il grafico contenente una
# matrice di scatterplot tra tutte le colonne di Wage
pairs(Wage)


##############################################
## Polynomial Regression and Step Functions ##
##############################################

## Polynomial Regression

# Assegniamo all'oggetto fit l'output della funzione lm() attraverso
# la quale stimiamo i coefficienti di un modello di regressione
# polinomiale in cui la variabile di risposta è wage e come regressori
# le colonne di una matrice le quali costituiscono una base di polinomi
# ortogonali, tramite la funzione poly(), di default, otteniamo una base
# equivalente delle colonne della variabile age dal 1 al 4 grado,
# il dataset di riferimento è il dataset Wage contenuto in ISRL2
fit <- lm(wage ~ poly(age, 4), data = Wage)

# Attraverso la funzione coef() applicata al summary() dell'oggetto fit
# riportiamo in output la matrice contenente le stime dei coefficienti ottenuti
# stimando un modello di regressione polinomiale, gli standard error,
# t value e p value associati alle stime
coef(summary(fit))
#                 Estimate Std. Error    t value     Pr(>|t|)
# (Intercept)    111.70361  0.7287409 153.283015 0.000000e+00
# poly(age, 4)1  447.06785 39.9147851  11.200558 1.484604e-28
# poly(age, 4)2 -478.31581 39.9147851 -11.983424 2.355831e-32
# poly(age, 4)3  125.52169 39.9147851   3.144742 1.678622e-03
# poly(age, 4)4  -77.91118 39.9147851  -1.951938 5.103865e-02

# Si assegna all'oggetto fit2 l'output della funzione lm attraverso la quale
# implementa una regressione polinomiale in cui la variabile risposta è wage
# e i regressori del modello sono le colonne contenenti la variabile age dal
# primo al quarto grado ottenute tramite coercizione usando la funzione poly(),
# passando il parametro raw = T vengono restituite esattamente le colonne
# age, age^2, age^3 e age^4
fit2 <- lm(wage ~ poly(age, 4, raw = T), data = Wage)

# Attraverso la funzione coef() applicata al summary() dell'oggetto fit2
# riportiamo in output la matrice contenente le stime dei coefficienti ottenuti
# stimando un modello di regressione polinomiale, gli standard error,
# t value e p value associati alle stime
coef(summary(fit2))
#                             Estimate   Std. Error   t value     Pr(>|t|)
# (Intercept)            -1.841542e+02 6.004038e+01 -3.067172 0.0021802539
# poly(age, 4, raw = T)1  2.124552e+01 5.886748e+00  3.609042 0.0003123618
# poly(age, 4, raw = T)2 -5.638593e-01 2.061083e-01 -2.735743 0.0062606446
# poly(age, 4, raw = T)3  6.810688e-03 3.065931e-03  2.221409 0.0263977518
# poly(age, 4, raw = T)4 -3.203830e-05 1.641359e-05 -1.951938 0.0510386498

# Equivalentemente al fit2, in questo caso passiamo i regressori
# utilizzando la funzione I() attraverso la quale posiamo passare
# direttamente all'interno della funzione dei vettori senza doverli
# salvare in degli oggetti in anticipo
fit2a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage)
coef(fit2a)

# Alternativamente è possibile utilizzare la funzione cbind() la quale
# crea una matrice contenente le colonne age, age^2, age^3 e age^4
fit2b <- lm(wage ~ cbind(age, age^2, age^3, age^4), data = Wage)

# Assegniamo all'oggetto agelims il vettore contenente il valore minimo e
# massimo della variabile age
agelims <- range(age)
agelims # 18 80

# Assegniamo alla variabile age.grid un vettore avente come primo valore
# agelims[1] e di seguito tutti i possibili valori tra agelims[1] e
# agelims[2] con un passo di 1.
# In questo caso avremo un vettore del tipo (18, 19, 20, 21, ..., 79, 80)
age.grid <- seq(from = agelims[1], to = agelims[2])
age.grid

# Altri esempi:
seq(2.6, 12.2)
# 2.6  3.6  4.6  5.6  6.6  7.6  8.6  9.6 10.6 11.6
seq(2.2, 12.2)
# 2.2  3.2  4.2  5.2  6.2  7.2  8.2  9.2 10.2 11.2 12.2
seq(2.2, 3.1)
# 2.2

# Assegno all'oggetto preds la lista (perchè ci sono più attributi dato
# il parametro se = TRUE) delle previsioni per le nuove
# osservazioni passate tramite il parametro newdata, ovvero la lista
# contenente la variabile age a cui assegniamo il vettore age.grid
# creato in precedenza. Con il parametro se = TRUE facciamo in modo che
# l'output della funzione contenga anche i valori dello standard error
# per ogni previsione al fine di poter costruire un intervallo di
# previsione in seguito
preds <- predict(fit, newdata = list(age = age.grid), se = TRUE)

# Assegniamo all'oggetto se.bands una matrice con 2 colonne, le quali
# contengono i vettori formati dalle previsioni + 2 volte lo standard
# error associato alla i-esima previsione e -2 volte lo standard
# error associato alla i-esima previsione
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

# Attraverso la funzione plot() stampiamo il grafico della nuvola dei punti
# in cui nell'asse delle ascisse è rappresentata la varabile age e nelle
# ordinate la variabile wage, con cex assegniamo la grandezza dei punti e
# con col il colore.
# successivamente aggiungiamo il titolo al grafico con title()
# Aggiungiamo la curva interpolante i punti nello spazio definito
# dai vettori age.grid (ascisse) e preds.fit (ordinate), spessore = 2
# e di colore blu. Infine aggiungiamo le curve tratteggiate (lty) con
# il comando matlines, di spessore 1 e anch'esse di colore blu 
par(mfrow = c(1, 1), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
png(paste(img_path, "/01_Degree-4_Polynomial.png", sep=""), width = 800, height = 600)
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree-4 Polynomial")
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)
dev.off()

# Per dimostrare che otteniamo le medesime previsioni nonostante i
# coefficienti siano differenti in quanto nel secondo caso le variabili
# non erano una base ortogonale assegniamo a preds le previsioni ottenute
# utilizzando gli stessi dati ma il secondo modello di regressione
# polinomiale
preds2 <- predict(fit2, newdata = list(age = age.grid), se = TRUE)

# Il comando max() restituisce in output il valore massimo del vettore
# delle differenze in valore assoluto tra le previsioni ottenute sulla
# griglia di valori utilizzando il primo e il secondo modello 
max(abs(preds$fit - preds2$fit)) # 6.842527e-11
# è sostanzialmente uno zero di macchina

# Assegniamo agli oggetti fit.1 ... fit.5 l'output della funzione lm()
# attraverso la quale si stima un modello di regressione polinomiale
# in cui la variabile di risposta è wage e le varaibili indipendenti
# sono rispettivamente i polinomi dal grado uno (lineare) al 5o
fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ poly(age, 3), data = Wage)
fit.4 <- lm(wage ~ poly(age, 4), data = Wage)
fit.5 <- lm(wage ~ poly(age, 5), data = Wage)

# Si utilizza la funzione anova() per confrontare i modelli polinomiali stimati.
# Attraverso questo comando è possibile confrontare ogni modello con il
# precedente e viene verificato se l'aggiunta del termine polinomiale successivo
# migliora significativamente il fit del modello.
# Dunque, si esegue un test F, ovvero un test sulla nullità di una parte dei
# coefficienti.
# In output si ottiene una tabella con:
# - la Df (variazione dei gradi di libertà),
# - la somma dei quadrati residui (RSS),
# - il valore di F e il relativo p-value.
# Un p-value piccolo indica che il termine aggiunto migliora il modello.
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
# Analysis of Variance Table
#
# Model 1: wage ~ age
# Model 2: wage ~ poly(age, 2)
# Model 3: wage ~ poly(age, 3)
# Model 4: wage ~ poly(age, 4)
# Model 5: wage ~ poly(age, 5)
#   Res.Df     RSS Df Sum of Sq        F    Pr(>F)
# 1   2998 5022216
# 2   2997 4793430  1    228786 143.5931 < 2.2e-16 ***
# 3   2996 4777674  1     15756   9.8888  0.001679 **
# 4   2995 4771604  1      6070   3.8098  0.051046 .
# 5   2994 4770322  1      1283   0.8050  0.369682
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Si utilizza la funzione coef() applicata all'output di summary(fit.5)
# per ottenere la tabella dei coefficienti stimati dal modello polinomiale
# di grado 5. La tabella riporta:
# - gli stimatori dei coefficienti (Estimate),
# - gli errori standard (Std. Error),
# - i valori t (t value) calcolati come Estimate / Std. Error,
# - i p-value associati a ciascun test t, che misurano la significatività
#   individuale di ciascun coefficiente nel modello.
# I nomi dei coefficienti seguono la codifica ortogonale usata da poly().
coef(summary(fit.5))
#                 Estimate Std. Error     t value     Pr(>|t|)
# (Intercept)    111.70361  0.7287647 153.2780243 0.000000e+00
# poly(age, 5)1  447.06785 39.9160847  11.2001930 1.491111e-28
# poly(age, 5)2 -478.31581 39.9160847 -11.9830341 2.367734e-32
# poly(age, 5)3  125.52169 39.9160847   3.1446392 1.679213e-03
# poly(age, 5)4  -77.91118 39.9160847  -1.9518743 5.104623e-02
# poly(age, 5)5  -35.81289 39.9160847  -0.8972045 3.696820e-01

# Si calcola il quadrato del valore t del secondo termine polinomiale
# di fit.5, ovvero (-11.983)^2, per ottenere il corrispondente valore
# della statistica F nel caso in cui si stesse testando solo quella
# variabile in un modello semplice
(-11.983)^2
# 143.592089

# Si stima un modello lineare multivariato in cui il salario (wage) è
# spiegato dalle variabili education (qualifica/istruzione) e age (età)
fit.1 <- lm(wage ~ education + age, data = Wage)

# Si stima un secondo modello includendo education e sostituendo age con
# un polinomio di grado 2 per modellare una relazione non lineare tra età
# e salario
fit.2 <- lm(wage ~ education + poly(age, 2), data = Wage)

# Si stima un terzo modello includendo education e un polinomio
# di grado 3 per age
fit.3 <- lm(wage ~ education + poly(age, 3), data = Wage)

# Si utilizza la funzione anova() per confrontare i tre modelli specificati.
# L’output mostra come ogni aggiunta di complessità (passaggio da lineare
# a quadratico, poi a cubico) migliori la qualità del fit. Viene riportato:
# - RSS (residual sum of squares) residua,
# - la variazione della devianza (Sum of Sq),
# - il test F per ogni confronto successivo,
# - i p-value per testare se la complessità aggiunta è significativa.
anova(fit.1, fit.2, fit.3)
# Analysis of Variance Table
#
# Model 1: wage ~ education + age
# Model 2: wage ~ education + poly(age, 2)
# Model 3: wage ~ education + poly(age, 3)
#   Res.Df     RSS Df Sum of Sq        F Pr(>F)
# 1   2994 3867992
# 2   2993 3725395  1    142597 114.6969 <2e-16 ***
# 3   2992 3719809  1      5587   4.4936 0.0341 *



## Regressione Logistica Polinomiale

# Si assegna all'oggetto fit l'output della funzione glm() attraverso la
# quale viene implementata una Regressione Logistica Polinomiale, in cui
# la variabile di risposta viene creata direttamente all'interno della
# funzione attraverso la funzione I() ed è una variabile dummy che assume
# TRUE se la condizione wage > 250 viene soddisfatta, FALSE altrimenti.
# Le variabili di risposta sono le colonne ottenute tramite la funzione poly(),
# per cui di default, otteniamo una base equivalente delle colonne della
# variabile age dal 1 al 4 grado.
fit <- glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial)

# Assegniamo all'oggetto preds la lista contenente le previsioni per le
# nuove osservazioni contenute nella lista passata al parametro newdata
# e lo standard error associato alla stima utilizzando il modello salvato
# precendetemente in fit
preds <- predict(fit, newdata = list(age = age.grid), se = T)
preds$fit[1:10]

# Assegniamo all'oggetto pfit il vettore costituito per ogni elemento
# da e elevato alla previsione i-esima su 1 + e elevato alla previsione
# i-esima. Questo valore conterrà le probabilità a posteriori calcolate
# sui nuovi dati
pfit <- exp(preds$fit) / (1 + exp(preds$fit))
pfit[1:10]

# o equivalentemente usiamo type = "response"
predict(fit, newdata = list(age = age.grid), type = "response", se = T)$fit[1:10]

# Assegniamo all'oggetto se.bands.logit una matrice con 2 colonne, le quali
# contengono i vettori formati dalle previsioni + 2 volte lo standard
# error associato alla i-esima previsione e -2 volte lo standard
# error associato alla i-esima previsione
se.bands.logit <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

# Assegniamo all'oggetto se.bands una matrice con 2 colonne, le quali
# contengono i vettori formati dalle proabilità condizionate + 2 volte
# lo standard error associato alla i-esima probabilità condizionata
# e -2 volte lo standard error associato alla i-esima probabilità condizionata
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))



# Attraverso il comando plot() riporto in output il grafico avente nell'asse
# delle ascisse la variabile Age, Mentre nell'asse delle ordinate la variabile
# dummy create mediante coercizione con I(Wage > 50) con valori negli assi x e y
# rispettivamente pari al supporto del vettore agelims e nel secondo caso da 0 a 0.2
# Si arricchisce il grafico con la funzione points() in modo da disegnare i punti
# con rumore assegnato tramite jitters() per age e per i valori della variabile
# dummy trasformata da 0 - 1 a 0 - 0.2.
# Aggiungiamo inoltre le curve, nel primo caso interpolante tutti i punti
# del piano con coordinate [age.grid, pfit] quindi la curva stimata e
# successivamente utilizzando matlines() le 2 curve interpolanti i punti con
# +- 2 volte lo standard error
png(paste(img_path, "/02_Regressione_Logistica_Polinomiale.png", sep=""), width = 800, height = 600)
plot(age, I(wage > 250), xlim = agelims, type = "n", ylim = c(0, .2))
points(jitter(age), I((wage > 250) / 5), cex = .5, pch = "|", col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)
dev.off()


## Step Function

# La funzione table() applicata al risultato di cut(age, 4) restituisce
# una tabella con il conteggio degli individui che ricadono in ciascuno
# dei 4 intervalli costruiti automaticamente dalla funzione cut() sulla
# base del range di età (age). Gli intervalli sono equidistanti e non
# sovrapposti. Ogni osservazione di età viene assegnata a un intervallo.
table(cut(age, 4))
# (17.9,33.5]   (33.5,49]   (49,64.5] (64.5,80.1]
#         750        1399         779          72

# Si stima un modello lineare in cui la variabile age è suddivisa in 4
# classi tramite la funzione cut(). Il modello avrà un'intercetta per
# la prima classe e un coefficiente (effetto fisso) per ciascuna delle
# classi successive. Questo permette di stimare una funzione a gradini
# (step function), in cui ogni classe ha un salario medio distinto.
fit <- lm(wage ~ cut(age, 4), data = Wage)

# Con la funzione coef(summary(fit)) si riportano i coefficienti stimati
# del modello, insieme agli errori standard, valori t e p-value.
# L’intercetta rappresenta il salario medio previsto per il primo gruppo
# (età tra 17.9 e 33.5), mentre i coefficienti additivi rappresentano le
# differenze di salario medio rispetto al primo gruppo per le altre fasce.
coef(summary(fit))

# Si utilizza la funzione predict() per ottenere le stime del modello
# sulle età contenute in age.grid. Questo oggetto è una sequenza di valori
# su tutto il range della variabile age, usata per costruire una linea
# di previsione continua (a gradini). Il risultato viene salvato in pred_step.
pred_step <- predict(fit, newdata = list(age = age.grid))

# Per salvare il grafico
png(paste(img_path, "/03_Step_Function.png", sep=""), width = 800, height = 600)

# Si costruisce un grafico a dispersione dei dati originali: età (age)
# sull'asse x e salario (wage) sull'asse y. Il dataset di riferimento è Wage.
plot(wage ~ age, data = Wage, col = "gray", main = "Step Function")

# Aggiungiamo al grafico precedente la linea delle previsioni ottenute
# con la step function. La linea è costruita su age.grid (x) e pred_step (y).
# Lo spessore della linea è aumentato con lwd = 3 per renderla più visibile.
lines(age.grid, pred_step, lwd = 3, col = "blue")
dev.off()


########################
## Regression Splines ##
########################

# Attraverso il comando library() carichiamo il pacchetto splines
library(splines)

# Si assegna all'oggetto fit l'output della funzione lm() con cui si
# implementa una regression spline utilizzando la funzione bs() del pacchetto
# splines.
# La variabile di risposta è wage, mentre il regressore age viene trasformato
# in una base di spline cubiche con 3 nodi posizionati ai valori 25, 40 e 60.
# Il grado del polinomio utilizzato per costruire la spline è 3 (default).
# Secondo la teoria, il numero di gradi di libertà della spline è dato da:
# df = numero nodi + grado del polinomio + 1 = 3 + 3 + 1 = 7
# Tuttavia, bs() non include l'intercetta nella matrice di base, poiché è già
# presente nella formula lm(). Di conseguenza, vengono generate 6 variabili (colonne)
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)  # default: grado = 3

# Si assegna all'oggetto pred la lista composta dalle previsioni
# per la griglia di valori di age, age.grid utilizzando l'oggetto
# fit salvato in precedenza. Inoltre con se = T, la funzione restituisce
# anche il vettore degli standard error associati alle stime.
pred <- predict(fit, newdata = list(age = age.grid), se = T)

# Per salvare il grafico
png(paste(img_path, "/04_Regression_Splines_3-Degree_3-Knots.png", sep=""), width = 800, height = 600)

# Attraverso il comando plot() riporto in output il grafico avente nell'asse
# delle ascisse la variabile Age, Mentre nell'asse delle ordinate la variabile
# wage.
# Aggiungiamo inoltre le curve, nel primo caso interpolante tutti i punti
# del piano con coordinate [age.grid, pred$fit] quindi la curva stimata e
# successivamente le 2 curve interpolanti i punti con +- 2 volte lo standard error
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2 * pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se, lty = "dashed")
title("Regression Spline")
dev.off()

# Attraverso la funzione dim() riportiamo in output l'oggetto ottenuto
# tramite la funzione bs() al quale passiamo come parametri la variabile
# age e i 3 nodi
dim(bs(age, knots = c(25, 40, 60)))
# 3000    6

# Attraverso la funzione dim() riportiamo in output l'oggetto ottenuto
# tramite la funzione bs() al quale passiamo come parametri la variabile
# age e i gradi di libertà, df = 6, per cui verranno analogamente al caso
# precedente create 6 variabili (l'intercetta non viene considerata) ma
# in questo caso i nodi vengono messi di default utilizzando i quartili
# di Age
dim(bs(age, df = 6))
# 3000    6

# Attraverso la funzione attr() applicata sulla funzione bs() con parametri
# age e df = 6, alla quale passiamo l'attributo "knots" riportiamo in output
# i nodi di default della funzione, ovvero i quartili di Age
attr(bs(age, df = 6), "knots")
# 33.75 42.00 51.00



#####################
## Natural Splines ##
#####################

# Si assegna all'oggetto fit2 l'output della funzione lm() attraverso
# la quale si implementa una Natural Spline Regression, ovvero una
# regressione lineare in cui la variabile esplicativa age viene trasformata
# mediante la funzione ns() (natural spline) con 5 gradi di libertà.
# Nella teoria una Cubic Spline generica con 5 nodi dovrebbe avere
# K + d + 1 = 5 + 3 + 1 = 9 gradi di libertà. Una Natural Cubic Spline
# con 5 nodi (3 interni e 2 agli estremi dell'intervallo) possiamo
# dire che ha K + d + 1 = 5 + 3 + 1 = 9 gradi di libertà, ma dato che agli
# estremi dell'intervallo essendo delle funzioni lineari (e non cubiche)
# dobbiamo stimare solo b0 e b1 (quindi non b2 e b3) che togliendo questi
# parametri in entrambi gli estremi avremo 9 - 4 = 5 gradi di libertà.
# Inoltre, poichè come in bs() non vogliamo aggiungere l'intercetta
# ad R dobbiamo assegnare df = 4 (e non df = 5; ma questo è solo un
# tecnicismo utile all'implementazione)
# Le natural splines sono spline cubiche soggette a vincoli di linearità
# agli estremi (oltre i boundary), che garantiscono una maggiore stabilità
# della stima ai bordi del dominio.
fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)

# Si richiama la documentazione relativa all’argomento Boundary.knots,
# utile per comprendere dove vengono posizionati i nodi e come vengono
# gestiti gli estremi (vincolo di linearità fuori dall'intervallo).
?Boundary.knots

# Viene riportata la dimensione della base di regressione generata dalla
# funzione ns() applicata alla variabile age, con nodi fissati nei punti
# 25, 40 e 60. Il risultato mostra una matrice con 3000 righe (una per ogni
# osservazione) e 4 colonne, corrispondenti al numero di gradi di libertà
# specificato, ovvero 4 funzioni di base per la natural spline.
dim(ns(age, knots = c(25, 40, 60)))
# [1] 3000    4

# Si assegna all'oggetto pred2 l'output della funzione predict() applicata
# all’oggetto fit2 su un nuovo insieme di dati: la griglia di valori age.grid,
# in modo da ottenere le stime dei salari previsti lungo il dominio di age.
# L'opzione se = T permette inoltre di calcolare l’errore standard associato
# ad ogni previsione, utile per costruire bande di confidenza.
pred2 <- predict(fit2, newdata = list(age = age.grid), se = T)


# Esempio conteggio dei df per bs() e ns():
?bs
?ns # è necessariamente cubica

# K + d + 1 = 3 + 3 + 1 = 7 (o per R: 6 + intercetta)
dim(bs(age, knots = c(25, 40, 60), degree = 3))
# 3000    6

# K + d + 1 = (3+2) + 3 + 1 = 9 - 4 = 5 (o per R: 4 + intercetta)
dim(ns(age, knots = c(25, 40, 60)))
# 3000    4

#################

# K + d + 1 = 4 + 3 + 1 = 8 (o per R: 7 + intercetta)
dim(bs(age, knots = c(25, 30, 35, 60), degree = 3))
# 3000    7

# K + d + 1 = (4+2) + 3 + 1 = 10 - 4 = 6 (o per R: 5 + intercetta)
dim(ns(age, knots = c(25, 30, 35, 60)))
# 3000    5


# Per salvare il grafico
png(paste(img_path, "/05_Natural_Splines_3-Degree_3-Knots.png", sep=""), width = 800, height = 600)


# Si costruisce un grafico scatterplot utilizzando la funzione plot() in cui
# si rappresentano i valori osservati delle variabili age e wage, con i punti
# colorati in grigio.
# Si aggiunge al grafico la curva stimata dal modello natural spline
# precedentemente creato e salvato in pred2$fit, in rosso e con uno
# spessore della linea pari a 2.
# L'asse x è rappresentato da age.grid, cioè una griglia di valori su cui sono
# state effettuate le predizioni.
# Si assegna un titolo al grafico tramite la funzione title()
plot(age, wage, col = "gray")
lines(age.grid, pred2$fit, col = "red", lwd = 2)
title("Natural Spline")
dev.off()



#######################
## Smoothing Splines ##
#######################

?smooth.spline
# smooth.spline(x, y = NULL, w = NULL, df, spar = NULL, lambda = NULL, cv = FALSE,
#               all.knots = FALSE, nknots = .nknots.smspl,
#               keep.data = TRUE, df.offset = 0, penalty = 1,
#               control.spar = list(), tol = 1e-6 * IQR(x), keep.stuff = FALSE)

# Si assegna all’oggetto fit l’output della funzione smooth.spline().
# In questo caso:
# - age è il vettore della variabile indipendente (ascisse, sull’asse x),
# - wage è il vettore della variabile dipendente (ordinate, sull’asse y),
# - df = 16 specifica il numero di gradi di libertà desiderati per la curva.
#    Più df significa maggiore flessibilità: la curva seguirà più da vicino
# i dati.
#    Una spline con df troppo alto rischia però di overfittare (sovradattarsi).
fit <- smooth.spline(age, wage, df = 16)

# Si assegna all’oggetto fit2 l’output della funzione smooth.spline(),
# sempre specificando:
# - age (variabile indipendente),
# - wage (variabile dipendente),
# ma in questo caso:
# - cv = TRUE attiva la validazione incrociata leave-one-out per determinare
#   automaticamente il grado ottimale di smoothing (cioè il miglior compromesso
#   tra adattamento ai dati e semplicità della curva),
# - il parametro di smoothong scelto tramite CV determina internamente il valore
#   del parametro lambda, che controlla la penalizzazione sulla curvatura della
# spline.
fit2 <- smooth.spline(age, wage, cv = TRUE)
names(fit2)
#  [1] "x"          "y"          "w"          "yin"        "tol"
#  [6] "data"       "no.weights" "n"          "lev"        "cv"
# [11] "cv.crit"    "pen.crit"   "crit"       "df"         "spar"
# [16] "ratio"      "lambda"     "iparms"     "auxM"       "fit"
# [21] "call"

# Si verifica se è stata applicata la cross-validation nei due modelli.
# Nel primo caso (fit) è FALSE, nel secondo (fit2) è TRUE.
fit$cv # FALSE
fit2$cv # TRUE

# Si esamina il valore del parametro di penalizzazione lambda, utilizzato
# internamente per controllare la levigatezza della spline. Un valore basso
# indica una curva molto flessibile, un valore alto produce una
# curva più liscia.
fit$lambda # 0.0006537868
fit2$lambda # 0.02792303

# Si estraggono i gradi di libertà effettivi delle curve stimate.
# Nel primo caso (fit) sono stati fissati manualmente a 16.
# Nel secondo (fit2) il valore è stato scelto automaticamente dalla funzione
# in base al criterio di cross-validation, e risulta più basso.
fit$df # 16.00237
fit2$df # 6.794596

# Per salvare il grafico
png(paste(img_path, "/06_Smoothig_Splines.png", sep=""), width = 800, height = 600)


# Si costruisce un grafico scatterplot utilizzando la funzione plot() in cui
# si rappresentano i valori osservati delle variabili age e wage, con i punti
# colorati in grigio.
# Si assegna un titolo al grafico tramite la funzione title()
# Si aggiunge al grafico la curva stimata utilizzando la smoothing spline
# precedentemente creata e salvata in fit, in rosso e con uno
# spessore della linea pari a 2.
# Si aggiunge al grafico la curva stimata utilizzando la smoothing spline
# precedentemente creata e salvata in fit2, in blu e con uno
# spessore della linea pari a 2.
# Attraverso la funzione legend() aggiungiamo una legenda al grafico
# in posizione in alto a destra
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"), col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)
dev.off()


###########
## LOESS ##
###########

?loess
# loess(formula, data, weights, subset, na.action, model = FALSE,
#       span = 0.75, enp.target, degree = 2,
#       parametric = FALSE, drop.square = FALSE, normalize = TRUE,
#       family = c("gaussian", "symmetric"),
#       method = c("loess", "model.frame"),
#       control = loess.control(...), ...)

# Assegniamo all’oggetto fit l’output della funzione loess()
# per stimare una regressione locale di wage su age,
# utilizzando come span 0.2 → significa che per ogni punto di età,
# il 20% più vicino dei dati viene usato per stimare la curva.
# Un valore di span più piccolo produce una curva più flessibile,
# ma meno liscia.
fit <- loess(wage ~ age, span = .2, data = Wage)

# Assegniamo all’oggetto fit2 una stima LOESS con uno span più
# grande (0.5), che implica un maggiore livello di smoothing:
# ogni punto stimato tiene conto di una porzione più ampia di
# osservazioni. Questo riduce la varianza ma può aumentare il bias.
fit2 <- loess(wage ~ age, span = .5, data = Wage)

# Per salvare il grafico
png(paste(img_path, "/07_LOESS.png", sep=""), width = 800, height = 600)

# Attraverso la funzione plot() stampiamo lo scatterplot in cui è presente
# la variabile age sull'asse delle ascisse e wage sull'asse delle ordinate.
# I punti avranno spessore 0.5 e sono di colore grigio scuro
# Aggiungiamo il titolo al grafico con la funzione title().
# Aggiungiamo la curva stimata tramite LOESS con span = 0.2 con colore rosso e
# la curva con span = 0.5 con colore blu. Aggiungiamo infine la legenda al
# grafico in alto a destra
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression")
lines(age.grid, predict(fit, data.frame(age = age.grid)), col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)), col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"), col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)
dev.off()


#########
## GAM ##
#########

#
gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)

#
library(gam)

#
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)


#
par(mfrow = c(1, 3))
plot(gam.m3, se = TRUE, col = "blue")

#
plot.Gam(gam1, se = TRUE, col = "red")

#
gam.m1 <- gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data = Wage)

#
anova(gam.m1, gam.m2, gam.m3, test = "F")

#
summary(gam.m3)

#
preds <- predict(gam.m2, newdata = Wage)

#
gam.lo <- gam(
 wage ~ s(year, df = 4) + lo(age, span = 0.7) + education,
 data = Wage
 )

#
plot(gam.lo, se = TRUE, col = "green")

#
gam.lo.i <- gam(wage ~ lo(year, age, span = 0.5) + education, data = Wage)

#
library(akima)

#
plot(gam.lo.i)

gam.lr <- gam(
 I(wage > 250) ~ year + s(age, df = 5) + education,
 family = binomial, data = Wage
 )
par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = "green")

table(education, I(wage > 250))


gam.lr.s <- gam(
 I(wage > 250) ~ year + s(age, df = 5) + education,
 family = binomial, data = Wage,
 subset = (education != "1. < HS Grad")
 )
plot(gam.lr.s, se = T, col = "green")
