###################################
#### [43] 2.3.1 Basic Commands ####
###################################

# La funzione remove avente come parametro ls() ci permette di rimuovere
# tutti gli oggetti e pulire il Global Environment
remove(list = ls())

# La funzione getwd() restituisce la working directory sulla quale
# stiamo lavorando
getwd()

# La funzione installed.packages() ci permette di visualizzare tutte
# le librerie presenti
installed.packages()

# Attraverso la funzione install.packages() installiamo il pacchetto MASS
install.packages("MASS")

# Attraverso la funzione library() carichiamo la libreria MASS
library("MASS")

# Assegno all'oggetto x un vettore di lunghezza 4 i cui elementi sono 1,3,2 e 5
(x <- c(1, 3, 2, 5))
length(x)

# Assegno all'oggetto x, sovrascrivendolo, una matrice avente 2 righe
# e 2 colonne. L'attributo di default byrow = FALSE indica che gli elementi
# passati tramite l'attributo "data" vengano inseriti per colonna
(x <- matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2))

# Assegno all'oggetto x, sovrascrivendolo, un vettore di 50 elementi i quali
# saranno estratti casualmente da una distribuzione normale avente come
# parametri media = 0 e deviazione standard = 1
?rnorm() # Default, rnorm(n, mean = 0, sd = 1)
(x <- rnorm(50))

# Assegno all'oggetto y un vettore di lunghezza 50, dove gli elementi sono
# estratti casualmente da una distribuzione normale avente come
# parametri media = 50 e deviazione standard = 0.1
(y <- x + rnorm(50, mean = 50, sd = .1))

# Assegno all'oggetto corr il valore restituito dalla funzione cor()
# la quale calcola la correlazione tra i vettori x e y
?cor()
(corr <- cor(x, y))

# In precedenza abbiamo utilizzato delle funzioni, il cui output non è dato da
# un calcolo deterministico ma ha una componente casuale (o pseudocasuale),
# ovvero si basa su algoritmi che generano numeri casuali a partire da
# uno stato iniziale.
# Ad esempio, la funzione rnorm() è una di queste. Per garantire la
# riproducibilità dei risultati è necessario quindi impostare un seme di
# partenza per cui l'output è il medesimo ad ogni istante t+n ed in ogni
# macchina in cui viene riprodotto (su questo non vi è la certezza assoluta)
# Per questo è possibile utilizzare la funzione set.seed()

# Imposto un seme iniziale tramite la funzione set.seed() per garantire
# la riproducibilità dei risultati
set.seed(1)

# Eseguo nuovamente i comandi precedenti
x <- rnorm(50)
y <- x + rnorm(50, mean = 50, sd = .1)

# Assegno a corr_seme1 l'output della funzione di calcolo della correlazione
# di x e y (usando come seme di partenza 1)
corr_seme1 <- cor(x,y)

# Calcolo la differenza tra i risultati. Come si può notare, non sono
# esattamente identici
corr_seme1 - corr

# Attraverso le funzioni mean(), var() e sd() calcolo la media, varianza
# e deviazione standard del vettore x.
# Come si può noatare non sono esattamente media = 0 e sd = 1, questo perchè
# la numerosità è contenuta e perchè i numeri presenti in x sono le
# realizzazioni di una funzione pseudocasuale.
# Piccole deviazioni dalla media teorica sono normali.
mean(x)
var(x)
sd(x)


x <- rnorm(100)
y <- rnorm(100)

# Attraverso la funzione plot() rappresento lo scatterplot avente il vettore
# x come asse delle ascisse e il vettore y nell'asse delle ordinate
?plot()

plot(x, y)
plot(x, y, xlab = "this is the x-axis",
        ylab = "this is the y-axis",
        main = "Plot of X vs Y")

# Assegno all'oggetto x, sovrascrivendolo, per mezzo della funzione seq()
# un vettore avente tutti i numeri interi da 1 a 10
(x <- seq(1, 10))

# Assegno all'oggetto x, sovrascrivendolo, un vettore avente tutti
# i numeri interi da 1 a 10
(x <- 1:10)

# Assegno all'oggetto x, sovrascrivendolo, per mezzo della funzione seq()
# un vettore di lunghezza 50 con valori nell'intervallo -pi greco
# a +pi grego equidistanti tra loro
(x <- seq(-pi, pi, length = 50))

# Attraverso la funzione library() carico il pacchetto ISRL2
library(ISLR2)

# Assegno all'oggetto data il dataset Auto contenuto nel pacchetto ISRL2
data <- Auto # è già nel nostro Global Environment, parte del pacchetto ISRL

# Attraverso la funzione head() riporto in output le prime 6 righe del
# dataset Auto
head(Auto)

# Attraverso la funzione dim() riporto in output le dimensioni del
# dataset Auto, il primo valore corrisponde al numero di righe mentre
# il secondo al numero di colonne
dim(Auto)

# Assegniamo all'oggetto Auto, attraverso la funzione na.omit(), l'oggetto
# stesso al netto delle righe contenenti dei valori mancanti
?na.omit
Auto <- na.omit(Auto)

# Attraverso la funzione names() riportiamo in output i nomi delle
# colonne del dataset Auto
names(Auto)

# Attraverso il comando attach() sarà possibile richiamare le variabili del
# dataframe Auto utilizzando il nome della variabile e non necessariamente
# la sintassi Dataframe$variabile
attach(Auto)

# Assegniamo all'oggetto cylinders l'oggetto stesso che convertito in
# variabile da quantitativa in qualitativa attraverso la funzione as.factor()
cylinders <- as.factor(cylinders)

# La funzionee plot ci restituisce in output uno scatterplot in cui avremo
# nell'asse delle ascisse la variabile cylinders del df Auto e nell'asse
# delle ordinate la variabile mpg (meters per gallon)
plot(cylinders , mpg, col = "red", varwidth = T)

# La funzione hist() ci permette di rappresentare un istogramma, questo sarà
# rappresentato con 15 classi della variabile mpg
hist(mpg, col = 2, breaks = 15)

# Attraverso il comando pairs è possibile rappresentare una matrice di
# scatterplot tra tutte le colonne del data frame Auto
?pairs
pairs(Auto)

# Attraverso il comando sottostante è possibile rappresentare una matrice di
# scatterplot tra le sole colonne mpg, displacement, horsepower, weight
# e accelerationdel del data frame Auto
pairs(~ mpg + displacement + horsepower + weight + acceleration, data = Auto)

# Attraverso il comando summary(), applicato ad un oggetto di classe
# data frame, è possibile ottenere alcune informazioni rilevanti delle
# colonne del data frame Auto, se la variabile è quantitativa è possibile
# avere valore minimo, massimo, 1,2 e 3 quantile e media. Se qualitativa
# Le frequenze assolute di classe.
?summary
summary(Auto)