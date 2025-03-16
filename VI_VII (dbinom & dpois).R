# Homework Lez VI e VII | Michele Grassi 838917 | GSMD

###############
# Esercizio 1 #
###############

# Una macchina produce pezzi difettosi con probabilità 0.4. 
# Si consideri un lotto di 20 pezzi.

# Osservazioni: in questo problema sono rispettate le quattro assunzioni della
# distribuzione binomiale:
#   1) Vi sono n ripetizioni (o prove) dell'esperimento.
#   2) Vi sono due esiti possibili (difettoso, non difettoso).
#   3) Ogni prova e' indipendente dalle altre.
#   4) La probabilita' di successo rimane costante per tutte le prove.

# Possiamo, quindi, asserire che: X ~ Bin(n, θ), ove "θ" e "n" sono 
# rispettivamente:
teta <- 0.4
n <- 20

#-------------------------------------------------------------------------------
# Quesito a)
# Qual è la probabilità di trovare 1 pezzo difettoso? Calcolare 𝑝(𝑥 = 1).

# Assegno all'oggetto x il valore specifico di cui calcolare la probabilita'
x <- 1

# Utilizzo la funzione "dbinom" per calcolare la probabilita' che la v.c. 
# binomiale di parametri "n" e "teta" assuma valore "x".
# Moltiplico per 100 il valore ottenuto e utilizzo la funzione "paste" per
# trasformarlo in char e concatenare il segno percentuale.
cat("La probabilita' di trovare 1 pezzo difettoso risulta:", 
    paste(round(dbinom(x,n,teta)*100, 3), 
          "%", 
          sep = ""))

#-------------------------------------------------------------------------------
# Quesito b)
# Qual è la probabilità che meno di 6 pezzi siano difettosi? 
# Calcolare 𝑝(𝑥 < 6).

# Assegno all'oggetto "x" un vettore contenente i valori desiderati
x <- 0:5

# Uso la funzione "sum()" per restituire la somma dei valori contenuti nel 
# vettore restituito da "dbinom()".
cat("La probabilita' che meno di 6 pezzi siano difettosi risulta:", 
    paste(round(sum(dbinom(x,n,teta)*100), 3), 
          "%",
          sep = ""))

#-------------------------------------------------------------------------------
###############
# Esercizio 2 #
###############

# Presso uno sportello bancomat nel centro di Bari, 3 clienti su 5 effettuano
# un’operazione di prelievo (𝑝 = 3/5). 
# Supponendo di estrarre a caso 10 persone:

# Osservazioni: anche in questo caso sono rispettate le quattro assunzioni della
# distribuzione binomiale. 

# Assegno i dati del problema ai rispettivi oggetti:
teta <- 3/5
n <- 10

#-------------------------------------------------------------------------------
# Quesito a)
# Determinare la distribuzione di probabilità della v. c. X="numero di clienti 
# che preleva allo sportello". 
# Rappresentare graficamente la distribuzione ottenuta;

# Creo un data frame per costruire la distribuzione di probabilita' della v.c.
# X = "numero di clienti che preleva allo sportello"
X <- as.data.frame(cbind(0:n, dbinom(0:10, n,teta)))

# Assegno i rispettivi nomi alle variabili e mando X a schermo
names(X) <- c("Numero_prelievi",
              "Probabilita'")
X

# Sviluppo un'appropriata rappresentazione grafica della distribuzione
barplot(X[,2], 
        names.arg = X[,1],
        col = "blue",
        xlab = "Numero di clienti",
        ylab = "Probabilita'",
        main = "Distribuzione di probabilita'\ndella v.c. \"X\"")

#-------------------------------------------------------------------------------
# Quesito b)
# Calcolare la probabilità che il numero totale di clienti che preleva sia
# esattamente pari a 2 e a 7, ossia determinare sia 𝑝(𝑥 = 2) che 𝑝(𝑥 = 7).

# Per ricavare i risultati, posso sfruttare il data frame precedente
cat("\nLa probabilita' che il numero totale di clienti che preleva sia",
    "esattamente pari a 2 e':",
    paste(round(X[X$Numero_prelievi == 2, 2]*100, 3), 
          "%", 
          sep = ""),
    "\n\nLa probabilita' che il numero totale di clienti che preleva sia", 
    "esattamente pari a 7 e':",
    paste(round(X[X$Numero_prelievi == 7, 2]*100, 3), 
          "%", 
          sep = ""))

# Alternativamente, posso calcolare direttamente la probabilita' che la v.c. 
# assuma i valori desiderati
cat("\nLa probabilita' che il numero totale di clienti che preleva sia", 
    "esattamente pari a 2 e':",
    paste(round(dbinom(2,n,teta)*100, 3), 
          "%", 
          sep = ""),
    "\n\nLa probabilita' che il numero totale di clienti che preleva sia", 
    "esattamente pari a 7 e':",
    paste(round(dbinom(7,n,teta)*100, 3), 
          "%",
          sep = ""))

#-------------------------------------------------------------------------------
# Quesito c)
# Calcolare la probabilità che il numero totale di persone che preleva sia 
# minore di 9, ossia determinare 𝑝(𝑥 < 9).
cat("\nLa probabilita' che il numero totale di clienti che preleva sia",
    "inferiore a 9 \ne':",
    paste(sum(round(X[X$Numero_prelievi < 9, 2]*100, 3)), 
          "%",
          sep = ""))

# In alternativa uso la funzione "dbinom()"
cat("\nLa probabilita' che il numero totale di clienti che preleva sia",
    "inferiore a 9 \ne':",
    paste(sum(round(dbinom(0:8,n,teta)*100, 3)), 
          "%", 
          sep = ""))

#-------------------------------------------------------------------------------
# Quesito d)
# Calcolare la probabilità che il numero totale di persone che preleva sia 
# uguale o superiore a 5, ossia determinare 𝑝(𝑥 ≥ 5).
cat("\nLa probabilita' che il numero totale di clienti che preleva sia",
    "pari o superiore a 5 e':",
    paste(sum(round(X[X$Numero_prelievi >= 5, 2]*100, 3)),
          "%", 
          sep = ""))

# In alternativa uso la funzione "dbinom()"
cat("\nLa probabilita' che il numero totale di clienti che preleva sia",
    "pari o superiore a 5 e':",
    paste(sum(round(dbinom(5:n,n,teta)*100, 3)), 
          "%", 
          sep = ""))

#-------------------------------------------------------------------------------
###############
# Esercizio 3 #
###############

# In una cartiera, sulla base dell’esperienza passata, si assume che il numero 
# di difetti per metro quadro nei rotoli di carta di alta qualità segua una 
# distribuzione di Poisson con media di un difetto per ogni 5 metri quadri di 
# carta (0.2 per metro quadro).

# Osservazioni: come esplicitato nella traccia, essendo in presenza di un'area
# di opportunita' e considerando l'indipendenza del numero di occorrenze 
# rispetto a quelle esterne a tale area, e' possibile stabilire che 
# X ~ Po(λ) ove lambda corrisponde a:
lambda <- 0.2

#-------------------------------------------------------------------------------
# Quesito a)
# Qual è la probabilità che in un metro quadro ci siano esattamente 2 difetti?
# Calcolare 𝑝(𝑥 = 2).

# Mediante la funzione "dpois()" ricavo la probabilita', data lambda, associata 
# allo specifico valore 
x <- 2
cat("La probabilità che in un metro quadro ci siano esattamente 2 difetti e':",
    paste(round(dpois(x, lambda)*100, 3), 
          "%", 
          sep = ""))

#-------------------------------------------------------------------------------
# Quesito b)
# Qual è la probabilità che in un metro quadro ci siano piu' di 2 difetti?
# Calcolare 𝑝(𝑥 > 2).


# Sapendo che i due eventi "𝑥 > 2" e "𝑥 <= 2" sono complementari, sara'
# sufficiente calcorare 𝑝(𝑥 <= 2) e sottrarre il valore ottenuto a "1" per 
# ottenere 𝑝(𝑥 > 2)
x <- 2
cat("La probabilità che in un metro quadro ci siano piu' di 2 difetti e':",
    paste(round( (1 - sum(dpois(0:x, lambda))) * 100, 3), 
          "%", 
          sep = ""))

# In alternativa, posso stabilire il valore limite della coda a destra della  
# distribuzione utilizzando un ciclo "while()".

# Inizializzo un contatore per il loop
i <- 0

# Imposto il while in modo tale da ripetersi fino ad individuare il valore cui 
# e' associata una probabilita' cosi' bassa da non essere computabile.
while(dpois(i, lambda) != 0){
  i <- i + 1
}

# Assegno all'oggetto "x" un vettore di numeri interi da 3 al valore massimo
x <- 3:i

# Mando a schermo il risultato della sommatoria delle probabilita' associate
# ai valori da 3 a "i", moltiplicato per 100 e arrotondato alla terza cifra 
# decimale per restituire la percentuale ottenuta
cat("La probabilità che in un metro quadro ci siano piu' di 2 difetti e':",
    paste(round(sum(dpois(x, lambda))*100, 3), 
          "%", 
          sep = ""))

#-------------------------------------------------------------------------------
# Quesito c)
# Qual è la probabilità che in 12 metri quadri ci sia piu' di un difetto?
# Calcolare 𝑝(𝑥 > 1)

# Osservazioni: in quest'ultima richiesta possiamo osservare una modifica 
# nell'area di opportunita' da considerare; proprio per questo e' congruo 
# aspettarsi una modifica anche nel valore atteso della distribuzione e, di
# conseguenza, anche del parametro "lambda"
lambda = 0.2 * 12

# Come nel quesito precedente, considerando i due eventi complementari  
# "𝑥 > 1" e "𝑥 <= 1", allora 𝑝(𝑥 > 1) = 1 - 𝑝(𝑥 <= 1)
x <- 1
cat("La probabilità che in un metro quadro ci siano piu' di 2 difetti e':",
    paste(round( (1 - sum(dpois(0:x, lambda))) * 100, 3), 
          "%", 
          sep = ""))

# In alternativa, posso ricavare il valore limite della coda destra della nuova
# distribuzione.
i <- 0
while(dpois(i, lambda) != 0){
  i <- i + 1
}

# Assegno all'oggetto "x" un vettore di numeri interi da 2 al valore massimo
x <- 2:i

# Mando a schermo il risultato della sommatoria delle probabilita' associate
# ai valori da 2 a "i", moltiplicato per 100 e arrotondato alla terza cifra 
# decimale per restituire la percentuale ottenuta
cat("La probabilità che in dodici metri quadri ci sia piu' di 1 difetto e':",
    paste(round(sum(dpois(x, lambda))*100, 3), 
          "%", 
          sep = ""))

#-------------------------------------------------------------------------------
