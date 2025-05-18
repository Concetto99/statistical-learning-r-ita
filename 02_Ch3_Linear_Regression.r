#####################################
#### [121] 3.6 Linear Regression ####
#####################################

remove(list = ls())

# Attraverso il comando library() carichiamo i pacchetti MASS e ISLR2.
# I due pacchetti contengono vari dataset e funzioni utili.
library(MASS)
library(ISLR2)

# Il dataset Boston è contenuto in ISRL2. Questo dataset contiene la mediana
# dei prezzi delle case in 506 zone dell’area metropolitana di Boston.
# Attraverso il comando head() vediamo il contenuto delle prime righe di Boston
head(Boston)

# Attraverso il comando lm() è possibile adattare un modello lineare ai dati.

# Si assegna all'oggetto lm.fit l'output della funzione lm() con la quale si
# adatta un modello di regressione lineare ai dati, La variabile di risposta del
# modello è medv, mentre la variabile indipendente o regressore è lstat.
# Entrambe le variabili sono contenute nel dataset Boston
?lm()
lm.fit <- lm(medv ~ lstat, data = Boston)
(lm.fit)

# Attraverso la funzione summary applicata ad un oggetto di classe lm ci
# restituisce alcune infomazioni di un modello di regressione lineare, in cui
# possiamo notare i principali attributi, come il min, max, 1,2,3 quartile dei
# resudui, i coefficienti del modello con std. error ecc. e altre metriche
# come ad esempio l'R quadro
summary(lm.fit)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Il modello sopra è del tipo:
#                               medv = beta0 + beta1 * lstat + epsilon

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept) 34.55384    0.56263   61.41   <2e-16 ***
# lstat       -0.95005    0.03873  -24.53   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 6.216 on 504 degrees of freedom
# Multiple R-squared:  0.5441,    Adjusted R-squared:  0.5432
# F-statistic: 601.6 on 1 and 504 DF,  p-value: < 2.2e-16

# beta0 = 34.6
# beta1 = -0.95

# All'aumentare dell'1% della popolazione con lower-status il
# valore mediano delle case si riduce in media di circa 950 dollari

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Attraverso la funzione names() è possibile visualizzare gli attributi
# dell'oggetto lm.fit
names(lm.fit)

# La funzione coef() applicata ad un oggetto di classe lm restituisce in output
# i coefficienti del modello lineare, ovvero beta0 e beta1 in questo caso
coef(lm.fit)

# Attraverso la funzione confint() applicata ad un oggetto di classe lm è
# possibile ottenere gli intervalli di confidenza per i coefficienti beta
# Default: level = 0.95
?confint.lm
confint(lm.fit)

# La funzione predict() applicata ad un oggetto di classe lm ci permette di
# calcolare i valori predetti ottenuti applicando la funzione di regressione
# a dei nuovi dati passati mediante un data frame

# Attraverso la funzione predict, con parametri object = lm.fit, newdata
# che viene passato mediante coercizione di un data frame a 3 valori distinti
# il che equivale a calcolare i valori per un ipotetico dataset di test e
# infine esplicitando il tipo di intervallo (confidenza o previsione) associato
# al valore predetto
?predict.lm
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "confidence")
# Restituisce i valori predetti della variabile dipendente per lstat = 5, 10, 15
# includendo l'intervallo di confidenza per il valore medio atteso della
# risposta
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "prediction")
# Restituisce i valori predetti per lstat = 5, 10, 15
# includendo l'intervallo di previsione per una singola nuova osservazione
# (più ampio rispetto all'intervallo di confidenza)


# Visualizzazione grafica della relazione tra la variabile indipendente 'lstat'
# (percentuale di popolazione a basso reddito) e la variabile dipendente 'medv'
# (valore mediano delle abitazioni in $1000)

# Attraverso il comando attach() si rendono direttamente accessibili le
# variabili del dataset Boston
attach(Boston)

# Attraverso la funzione plot() è possibile ottenere un grafico a dispersione
# tra le variabili lstat (asse delle ascisse) e medv (asse delle ordinate)
par(mfrow = c(1, 1)) # Settare la finestra grafica
plot(lstat, medv)

# Attraverso il comando abline() associato ad un oggetto di classe lm
# aggiungiamo la retta di regressione stimata del modello al grafico
abline(lm.fit)
abline(lm.fit, lwd = 3) # linea con spessore pari a 3
abline(lm.fit, lwd = 3, col = "red") # linea con spessore pari a 3 e colore rosso

# Attraverso i comandi sottostanti, in seguito all'impostazione di una
# finestra grafica di 4 grafici disposti in 2 righe e due colonne,
# è possibile visualizzare dei grafici con diversi attributi per poter
# apprezzare le differenze tra essi
par(mfrow = c(2, 2))
plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)

# La funzione plot associata ad un oggetto di classe lm restituisce in output
# alcuni grafici che permettono di effettuare la diagnostica dei residui.

# I grafici possibili sono:

    # 1. "Residuals vs Fitted", aka ‘Tukey-Anscombe’ plot
    # 
    # 2. "Residual Q-Q" plot
    # 
    # 3. "Scale-Location"
    # 
    # 4. "Cook's distance"
    # 
    # 5. "Residuals vs Leverage"
    # 
    # 6. "Cook's dist vs Lev./(1-Lev.)"

# I grafici di default  1, 2, 3 e 5. Possiamo decidere quali grafici farci
# restituire passando il vettore dei grafici che desideriamo come parametro
# "which" alla funzione plot()
?plot.lm
plot(lm.fit)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 1. "Residuals vs Fitted", aka ‘Tukey-Anscombe’ plot

# Il grafico in alto a sinistra mostra i residui (ordinate) in funzione dei
# valori predetti (ascisse) del modello. Questo grafico è molto utile in quanto
# aiuta a verificare se rimane struttura residua non spiegata dalle variabili
# del modello e quindi è necessario inglobarne delle altre
# Idealmente i punti dovrebbero essere distribuiti casualmente intorno alla
# linea orizzontale a zero. La curva all'interno del grafico viene ottenuta
# mediante il metodo LOESS (LOcal regrESSion)

# 2. "Residual Q-Q" plot

# Il grafico in alto a destra mostra nell'asse delle ascisse i quantili teorici
# di una distribuzione normale e nell'asse delle ordinate i residui
# studentizzati. Questo grafico è molto utile in quanto ci da un'indicazione su
# come i residui si distribuiscano. Se la curva formata fosse esattamente
# una linea retta passante per l'origine ci troveremmo di fronte ad una
# distribuzione dei residui esattamente normale. Viceversa si tratterebbe di
# una distribuzione asimmetrica. Questo controllo è abbastanza importante
# in quanto è richiesta l'ipotesi fondamentale di Normalità per la
# correttezza dei test statistici sui coefficienti, intervalli di confidenza
# e affidabilità delle previsioni

# 3. "Scale-Location"

# Il grafico in basso a sinistra mostra la radice quadrata dei residui
# standardizzati (asse delle ordinate) rispetto ai fitted values (asse delle
# ascisse). Questo grafico è utile per rilevare eventuali problemi di
# eteroschedasticità, ovvero una varianza non costante dei residui al
# variare dei valori predetti.

# 5. "Residuals vs Leverage"

# Il grafico in basso a destra mostra l'indice di leva (asse delle ascisse),
# ovvero la misura dell’influenza potenziale di un punto, e i residui
# studentizzati (asse delle ordinate). La misura di Leverage indica, quindi,
# quanto un'osservazione è estrema e quindi influenza le stime
# dei coefficienti.
# Le linee rosse tratteggiate rappresentano i contorni delle distanze di Cook,
# che combinano leverage e grandezza del residuo per identificare i punti
# potenzialmente influenti.
# In regressione lineare semplice, il valore medio del leverage è
# approssimativamente pari a (p+1)/n

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Attraverso la funzione plot avente come parametri il vettore ottenuto per
# mezzo della funzione predict() nell'asse delle ascisse e il vettore
# ottenuto tramite la funzione residuals() nell'asse delle ordinte
# possiamo riprodurre la nuvola dei punti presente nel grafico
# "Residuals vs Fitted"
plot(predict(lm.fit), residuals(lm.fit))
predict(lm.fit)
residuals(lm.fit)

# Attraverso la funzione plot avente come parametri il vettore ottenuto per
# mezzo della funzione predict() nell'asse delle ascisse e il vettore
# ottenuto tramite la funzione rstudent() nell'asse delle ordinte
# possiamo riprodurre la nuvola dei punti presente nel grafico
# "Scale-Location"
plot(predict(lm.fit), rstudent(lm.fit))

# Attraverso la funzione hatvalues() è possibile calcolare le statistiche di
# leva per ogni unità statistica. Ogni valore di questo vettore corrisponde
# ai punti della diagonale della HAT Matrix ("matrice che mette il
# cappello alla Y")
hatvalues(lm.fit)

# Attraverso la funzione plot() avente come parametro proprio il vettore
# ottenuto usando la funzione hatvalues() è possibile vedere graficamente
# per ogni unità il valore ottenuto
plot(hatvalues(lm.fit))

# Attraverso la funzione which.max() è possibile farsi restituire il valore
# massimo del vettore ottenuto tramite la funzione hatvalues() passando come
# parametro lm.fit
which.max(hatvalues(lm.fit))

# Regressione multipla
# Si assegna all'oggetto lm.fit l'output ottenuto attraverso la funzione lm()
# attraverso la quale è possibile adattare un modello di regressione lineare
# ai dati. La variabile di risposta del modello è medv, mentre i due regressori
# sono lstat ed age. Le 3 variabili sono contenute nel dataset Boston
lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

# Si assegna all'oggetto lm.fit l'output ottenuto attraverso la funzione lm()
# attraverso la quale è possibile adattare un modello di regressione lineare
# ai dati. La variabile di risposta del modello è medv, mentre i regressori
# sono tutte le variabili contenute all'interno del dataset Boston, al netto
# di medv
lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)
# Dal summary() precedente notiamo che il p-value di age è 0.8

# Attraverso la funzione library() carichiamo il pacchetto "car", il quale
# contiene tra i vari oggetti anche la funzione vif()
library(car)

# Attraverso la funzione vif() è possibile calcolare il variance inflation
# factor di ogni regressore del modello in lm.fit. La misura del VIF è data
# da 1/(1-R^2) dove R^2 è l'R quadro del modello ottenuto regredendo la
# variabile in oggetto su tutte le altre, ciò viene fatto per ogni variabile
# indipendente del modello.
# Attraverso il VIF si valuta quindi se vi sono potenziali problemi di
# collinearità, questo può accadere quando 2 o più variabili contengono
# delle informazioni simili, dunque ne basterebbe solamente una.
# Se il VIF è indicativamente un numero maggiore di 5 è possibile andare
# ad indagare cosa porta ad essere un numero così elevato.
# La variabile tax, ad esempio, potrebbe essere un punto da attenzionare
vif(lm.fit)

# Assegno all'oggetto lm.fit1 l'output di un modello di regressione lineare
# ottenuto per mezzo della funzione lm() attraverso la quale adattiamo un
# modello di regressione lineare ai dati in cui la variabile di risposta è
# medv, mentre i regresori sono tutte le altre variabili di Boston eccetto
# la variabile age (rimossa probabilmente in quanto il p-value era elevato)
lm.fit1 <- lm(medv ~ . - age, data = Boston)
summary(lm.fit1)

# Utilizziamo il comando summary() passando come argomento la funzione lm(),
# che adatta un modello di regressione lineare ai dati. In questo caso,
# la variabile di risposta è 'medv', mentre i regressori sono 'lstat',
# 'age' e la loro interazione (lstat * age).
# Questo modello ci consente di verificare se la variabile 'age' sia
# effettivamente da escludere oppure se, attraverso l'interazione con 'lstat',
# contribuisca in modo significativo alla spiegazione della variabilità
# di 'medv'
summary(lm(medv ~ lstat * age, data = Boston))


# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept) 36.0885359  1.4698355  24.553  < 2e-16 ***
# lstat       -1.3921168  0.1674555  -8.313 8.78e-16 ***
# age         -0.0007209  0.0198792  -0.036   0.9711
# lstat:age    0.0041560  0.0018518   2.244   0.0252 *
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Dall'output del summary() osserviamo che il termine di interazione tra 'lstat'
# e 'age' ha un p-value di circa 0.025. Questo valore è inferiore al consueto
# livello di significatività del 5%, suggerendo che l'interazione sia
# statisticamente significativa.
# Di conseguenza, non possiamo concludere con certezza che la variabile 'age'
# sia irrilevante: anche se il suo effetto principale non fosse significativo,
# la sua interazione con 'lstat' potrebbe comunque avere un ruolo importante
# nel modello.

# Trasformazioni non lineari dei predittori
# Assegno all'oggetto lm.fit2 l'output di un modello di regressione lineare
# ottenuto con la funzione lm(), in cui la variabile di risposta è medv
# e i predittori sono lstat e lstat al quadrato (per modellare una relazione
# non lineare).
# La funzione I() serve per calcolare lstat^2 direttamente nella formula,
# evitando che R interpreti l'operatore ^ come una formula speciale.
lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)

# Usiamo un’analisi della varianza per confrontare lm.fit (lineare) con
# lm.fit2 (quadratico):
lm.fit <- lm(medv ~ lstat)
anova(lm.fit, lm.fit2)

# Il test F confronta il modello più semplice (lm.fit) con quello più
#  complesso (lm.fit2). Ipotesi nulla: il termine quadratico (lstat^2) non
# aggiunge informazione significativa. Se il p-value è basso → rifiutiamo H0
# → il modello lm.fit2 è significativamente migliore.

# Attenzione: la validità della statistica F dipende dalla normalità dei
# residui. Infatti la F nasce dal rapporto tra due chi-quadro, che a loro
# volta derivano da variabili Normali.
# Quindi: se i residui non sono Normali, il test F è approssimato (non esatto).

# Soluzioni se la normalità è dubbia:
# - Applicare trasformazioni (es. log(medv)) per stabilizzare la varianza e
# migliorare la normalità
# - Affidarsi all’approssimazione asintotica: se n è grande, possiamo usare
# la teoria del limite centrale (CLT), Slutsky e altri teoremi che giustificano
# l’uso della F anche in caso di deviazioni moderate.

# CONCLUSIONE:
# Il modello lm.fit2, che include una trasformazione non lineare del
# predittore, mostra un miglioramento evidente nella qualità della regressione,
# sia a livello grafico (residui più casuali), sia a livello statistico
# (anova mostra un miglioramento significativo).
par(mfrow = c(2, 2))
plot(lm.fit2)

# Assegno all'oggetto lm.fit5 l'output della funzione lm() attraverso la quale
# adatto un modello di regressione lineare ai dati dove la variabile di risposta
# è medv, mentre le variabili indipendenti sono date dalla variabile lstat dal
# grado pari a 1 al grado pari a 5 ottenute per mezzo della funzione poly(),
# la quale viene utilizzata direttamente all'interno della funzione lm().
# Lstat viene trasformata internamente in un set di 5 variabili ortogonali.
# Questo permette di evitare problemi di collinearità tra
# lstat, lstat^2, ..., lstat^5.
lm.fit5 <- lm(medv ~ poly(lstat, 5))
summary(lm.fit5)

# Utilizzando un polinomio di grado 5 come variabili independenti del modello
# la flessibilità aumenta, ma si rischia l’overfitting.
# Il valore di R^2 aumenta, ma ciò non garantisce una migliore capacità
# predittiva su nuovi dati (MSE di test potrebbe peggiorare)
# Il compromesso flessibilità / generalizzazione è cruciale → si valuta con
# tecniche come validazione incrociata

# Attraverso il comando summary è possibile riportare in output un sommario
# dell'output di un modello lineare del tipo Y = B0 + B1*log(rm) + epsilon
# dove attraverso la funzione log() è possibile linearizzare una relazione
# non lineare e stabilizzare la varianza
summary(lm(medv ~ log(rm), data = Boston))

# Tramite il comando head() è possibile visualizzare le prime righe del
# dataset Carseats, disponibile nel pacchetto ISLR
head(Carseats)

# Assegno all'oggetto lm.fit l'output di un modello lineare ottenuto
# tramite la funzione lm(), quale permette di adattare un modello lineare ai
# dati. La variabile dipendente è Sales, mentre le variabili indipendenti
# sono tutte le variabili contenute in Carseats eccetto Sales e con l'aggiunta
# dei termini di iterazione Income:Advertising e Price:Age
lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

# Attraverso la funzione contrasts() ottengo sia le categorie di ShelveLoc, sia
# il modo in cui R codifica una variabile categoriale
contrasts(Carseats$ShelveLoc)
# ShelveLoc è una variabile categoriale con tre livelli: "Bad", "Medium", "Good"
# R la trasforma in variabili dummy (indicatori binari), usando per default
# la codifica a trattamento
# il primo livello alfabetico ("Bad") è il riferimento (baseline)
# vengono create due variabili: Medium vs Bad, Good vs Bad
# I coefficienti nella regressione indicano l'effetto differenziale rispetto
# al riferimento