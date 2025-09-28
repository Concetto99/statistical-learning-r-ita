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

