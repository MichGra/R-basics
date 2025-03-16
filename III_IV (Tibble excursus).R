# Homework Lezione III & IV | Michele Grassi 838917 | Corso GSMD

#################
# Esercizio n. 2#
#################

# Importo il dataset "rides.csv" tramite funzione "read.csv()" e lo assegno ad un oggetto
# (il separatore di elenco è ";")
taxi_rides <- read.csv("rides.csv", header = T, sep = ";", dec = "." )

# Analizzo l'oggetto inizializzato
str(taxi_rides)       #"data.frame"
typeof(taxi_rides)    #"list"

#_Osservazioni_______________________________________________________________________________________________

# Come si puo' osservare, le funzioni str() e typeof() restituiscono informazioni contrastanti sull'oggetto.
# Pur utilizzando la funzione "as.data.frame()", la situazione non cambia:
taxi_rides <- as.data.frame( read.csv("rides.csv" , header = T , sep = ";" , dec = ".") )
str(taxi_rides)
typeof(taxi_rides)

# Per spiegare questa discordanza, partiamo dalla funzione "str()":
# essa manda a schermo il risultato (fra gli altri) della funzione "class()".
# Con il comando "class()" otteniamo infatti lo stesso ritorno:
class(taxi_rides)     #"data.frame"

# Con questo, sostanzialemnte, capiamo che la differenza e' da ricercarsi nelle due funzioni:
# "class()" e "typeof()"
?class
?typeof

# Mentre "class()" restituisce la 'tipologia' in logica programmazione ad oggetti (OOP),
# la funzione "typeof()" esegue il medesimo compito ma secondo la logica di archiviazione di R.
# Cosi' capiamo che l'oggetto, mentre  si configura a tutti gli effetti come data frame, viene archiviato
# in R come "list". 
# Una possibile spiegazione: la funzione "read.csv()" ha restituito un valore tibble, ovvero una sotto-classe
# (piu' efficiente) di data frame che viene archiviata come "list" in R. 
# la funzione "storage.mode()" potrebbe confermare cio':
storage.mode(taxi_rides)
typeof(taxi_rides)

#_____________________________________________________________________________________________________________

# Estraggo dal data frame i dati riferiti ai soli pagamenti elettronici
# e li assegno ad un oggetto 
elec_pymnt <- subset(taxi_rides, payment_type == 1)

# Analizzo la struttura
str(elec_pymnt)

# Individuo la frequenza assoluta
print("Il numero di passeggeri che hanno utilizzato il pagamento elettronico e': ")
nrow(elec_pymnt)

# Calcolo la frequenza relativa
print("La percentuale di pagamenti elettronici sul totale e': ")
round((nrow(elec_pymnt)/nrow(taxi_rides))*100, 2)

# Estraggo dal data frame i dati relativi ai pagamenti pari o inferiori a 10$
# e li assegno ad un oggetto
pymnt_under10 <- subset(taxi_rides, fare_amount <= 10)

# Analizzo la struttura dell'oggetto
str(pymnt_under10)

# Calcolo la frequenza assoluta
print("Il numero di pagamenti pari o infriori a 10$ e': ")
nrow(pymnt_under10)

# Ricavo la frequenza relativa
print("La percentuale di pagamenti pari o inferiori a 10$ e': ")
round((nrow(pymnt_under10)/nrow(taxi_rides))*100, 2)

# Estraggo dal data frame i dati relativi alle corse con piu' di 3 passeggeri
# e li assegno ad un oggetto
over_3pssngr <- taxi_rides[!(taxi_rides$passenger_count <= 3), ]

# Analizzo la struttura dell'oggetto
str(over_3pssngr)

# Calcolo la frequenza assoluta
print("Il numero di corse con piu' di tre passeggeri e' ")
nrow(over_3pssngr)

# Ricavo la frequenza relativa rispetto al totale delle corse
print("La percentuale di corse con piu' di tre passeggeri e' ")
round((nrow(over_3pssngr)/nrow(taxi_rides))*100, 2)

# Calcolo l’ammontare medio pagato dai passeggeri per una corsa diurna
# con distanza percorsa maggiore di 5 km.
(a <- mean(taxi_rides[taxi_rides$day_time == 0 & taxi_rides$trip_distance > 5, "fare_amount"]))

# Calcolo l’ammontare medio pagato dai passeggeri per una corsa notturna
# con distanza percorsa maggiore di 5 km.
(b <- mean(taxi_rides[taxi_rides$day_time == 1 & taxi_rides$trip_distance > 5, "fare_amount"]))

# Ricavo la differenza in valore assoluto
print("La differenza fra le medie e': ")
abs(a-b)

# Calcolo la differenza percentuale
print("In media, le corse notturne sono piu' costose di punti percentuali pari a: ")
round((abs(a-b)/a)*100, 2)

# Rappresento graficamente la differenza fra le due distribuzioni
# Imposto i grafici in 2 righe e una colonna, margini di 2 in ogni direzione
par(mar = c(2, 2, 2, 2), mfrow = c(2, 1))

# Creo l'istogramma della distribuzione dei prezzi nelle corse diurne con 
# distanza maggiore di 5km
hist(taxi_rides[taxi_rides$day_time == 0 & taxi_rides$trip_distance > 5, "fare_amount"], 
     main = "Corse diurne > 5km",
     freq = T, 
     xlim = c(0,30), 
     xlab = "Prezzo")

# Evidenzio la media
abline(v = a, col = "red")

# Creo la legenda
legend("topleft", "media", 
       col = "red", 
       lwd = 1,
       bty = "n",
       seg.len = 1)

# Creo l'istogramma della distribuzione dei prezzi nelle corse notturne con 
# distanza maggiore di 5km
hist(taxi_rides[taxi_rides$day_time == 1 & taxi_rides$trip_distance > 5, "fare_amount"], 
     main = "Corse notturne > 5km",
     freq = T, 
     xlim = c(0,30), 
     xlab = "Prezzo")

# Evidenzio la media
abline(v = b, col = "blue")

# Creo la legenda
legend("topleft", "media", 
       col = "blue", 
       lwd = 1,
       bty = "n",
       seg.len = 1)
