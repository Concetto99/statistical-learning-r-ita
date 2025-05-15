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
# adatta un modello di regressione lineare ai dati, La varabile di risposta del
# modello è medv, mentre la variabile indipendente o regressore è lstat.
# Entrambe le variabili sono contenute nel dataset Boston
?lm()
lm.fit <- lm(medv ~ lstat, data = Boston)
(lm.fit)

# Attraverso la funzione summary applicata ad un oggetto di classe lm ci
# restituisce l'output di un modello di regressione lineare, in cui possiamo
# i principali attributi, come il min, max, 1,2,3 quartile dei resudui, i
# coefficienti del modello con std. error ecc. e altre metriche come R quadro
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

# I grafici di default  1, 2, 3 e 5. Possiamo decidere noi usando passando il
# vettore dei grafici che desideriamo come parametro "which"
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
# quanto un'osservazione è estrema e quindi influenza i valori delle stime
# dei coefficienti.
# Le linee rosse tratteggiate rappresentano i contorni delle distanze di Cook,
# che combinano leverage e grandezza del residuo per identificare i punti
# potenzialmente influenti.
# In regressione lineare semplice, il valore medio del leverage è
# approssimativamente pari a (p+1)/n

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit) 

lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)

library(car) # per il vif
vif(lm.fit)

lm.fit1 <- lm(medv ~ . - age, data = Boston)
summary(lm.fit1)


summary(lm(medv ~ lstat * age, data = Boston))


lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)

anova(lm.fit, lm.fit2)
lm.fit <- lm(medv ~ lstat)

par(mfrow = c(2, 2))
plot(lm.fit2)

lm.fit5 <- lm(medv ~ poly(lstat, 5))
summary(lm.fit5)

summary(lm(medv ~ log(rm), data = Boston))

head(Carseats)


lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

contrasts(Carseats$ShelveLoc)
