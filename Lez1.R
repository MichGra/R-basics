# Homework Lezione 1 | Michele Grassi 898317 | Corso GSMD|


##################
# Esercizio n. 2 #
##################

#Inizializzo i vettori
x = c(7, 5, 10, 3, 2, 10, 15, 20, 5)
y = c(0.15, 0, 0.32, 0.51, 0.18, 0.22, 0.6, 0.98, 0.12)
w = c(0.1, 10, 5, 0.80, 0.19, 0.25, 20, 22, 30)
z = c(130, 315, 681, 543, 1100, 90, 44, 78, 410)

#Inizializzo la matrice e la mando a schermo
mtrx <- matrix(c(x, y, w, z), ncol = 4, byrow = 0)
mtrx

#Calcolo la media per ogni colonna della matrice
mean(mtrx[,1])
mean(mtrx[,2])
mean(mtrx[,3])
mean(mtrx[,4])

#Creo una seconda matrice estratta dalla prima
mtrx2 <- mtrx[c(1, 5, 6), c(1, 2, 4)]
mtrx2

#Trovo la diagonale principale della seconda matrice
diag(mtrx2)


##################
# Esercizio n. 3 #
##################

#Inizializzo tre componenti e mando a schermo
object1 <- 1:5
object2 <- c("Uomo", "Donna")
object3 <- matrix(1:15, ncol = 3, nrow = 5, byrow = 1)
object1
object2
object3

#Seleziono la terza colonna della terza componente
object3[,3]


##################
# Esercizio n. 4 #
##################

#Importo il data frame "crescita.csv": File -> Import Dataset -> From Text (readr)
#Il separatore di elenco del file "crescita.csv" e' il punto e vigola

#Estraggo le caratteristiche del data frame
class(crescita)
dim(crescita)
names(crescita)
str(crescita)  #comando unico per informazioni generali sulla struttura

#Estraggo i dati che si riferiscono alle sole azende lombarde e mando a schermo
lmbrd <- subset(crescita, Regione == "Lombardia")
lmbrd

#Calcolo il numero di aziende lombarde
dim(lmbrd)[1]

#Estraggo dal data frame "crescita" i dati che si riferiscono a tutte le aziende-
# escluse quelle con n. di dipendenti inferiore a 30 nel 2017 e mando a schermo
dipendenti30 <- subset(crescita, Dipendenti_2017 > 30)
dipendenti30

#Calcolo il numero di aziende nel data frame con n. dipendenti > 30 nel 2017
dim(dipendenti30)[1]

#Individuo le aziende con il fatturato minore negli anni 2014 e 2017
min2014 <- min(crescita[ ,"Fatturato_2014.migliaia."])
min2017 <- min(crescita[ ,"Fatturato_2017.migliaia."])

#Ne ricavo le regioni corrispondenti
subset(crescita,Fatturato_2014.migliaia. == min2014, Regione)
subset(crescita,Fatturato_2017.migliaia. == min2017, Regione)
