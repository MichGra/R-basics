# Homework lezione VI | Michele Grassi 838917 | Corso GESDM

# ____________
# Richiesta a)|

# Imposto la directory sulla cartella per le importazioni (non eseguire)
# setwd("C:/Users/miche/OneDrive/Desktop/Code/R studio/Data import")

# Importo il data set contenuto nel file "unhdi.csv" e lo assegno ad un oggetto
cntry_indxs <- read.csv("unhdi.csv", sep = ";")

# Analizzo la struttura dell'oggetto
str(cntry_indxs)

# ____________
# Richiesta b)|

# Trasformo la variabile "hdi_group" in fattore per poter ordinare il data frame
cntry_indxs$hdi_group <- factor(cntry_indxs$hdi_group, 
                                ordered = T,
                                levels = c("Low", "Medium", "High", "Very high"))


# Ordino il data frame sulla base dei livelli della variabile "hdi_group"
sorted_cn_idx <- cntry_indxs[order(cntry_indxs$hdi_group, decreasing = F),]

# Calcolo la distribuzione di frequenza della variabile "hdi_group"
freq_abs <- table(sorted_cn_idx$hdi_group)
freq_abs

# Ricavo quelle relative
freq_rel <- freq_abs/length(cntry_indxs$hdi_group)
freq_rel

# Le percentuali risultano:
freq_rel*100

# Calcolo le frequenze cumulate
cumsum(freq_abs)

# Faccio lo stesso per ricavare quelle percentuali
cumsum(round(freq_rel*100, 2))

# La percentuale di  di paesi il cui indice di sviluppo umano non supera il
# livello "Medium" risulta:
paste(cumsum(round(freq_rel*100, 2))[2], "%", sep = "")

# ____________
# Richiesta c)|

# Inverto l'ordinamento della variabile da crescente a decrescente
sorted_cn_idx <- cntry_indxs[order(cntry_indxs$hdi_group, decreasing = T),]

# Rappresento graficmanete la distribuzione di frequenze 
# della variabile "hdi_group"
barplot(freq_abs, width = 1,
        col = rev(heat.colors(4)),
        horiz = T,
        main = "Indice di sviluppo umano nei vari paesi",
        xlab = "Frequency")

# Rappresento graficamente la distribuzione di frequenze percentuali
pie(freq_rel*100, 
    col = palette.colors(4, "Paired"),
    labels = paste(round(freq_rel*100, 2), "%", sep = ""),
    main = "Indice di sviluppo umano nei vari paesi")

# Creo la legenda
par(xpd = T)
legend("topright",
       rev(levels(cntry_indxs$hdi_group)),
       inset = -0.06,
       bty = "o",
       fill = rev(palette.colors(4, "Paired")),
       title = "Legenda",
       title.cex = 1.4)

# ____________
# Richiesta d)|

# Costruisco la distribuzione in classi della variabile "gdp_percap".
# Quattro classi di eguale ampiezza da 0m$ a 120m$.
# Supponiamo di dover ricavare i breaks della funzione "cut" senza conoscere il
# valore limite (120m$).

# Ricavo l'ampiezza delle quattro classi approssimando per 
# eccesso (+4.99 dopo la divisione per 4).
width <- round(max(cntry_indxs$gdp_percap, na.rm = T)/4 + 4.99, -1)
width

# Preparo gli oggetti del loop per costruire il vettore dei breaks
i <- 0
classes <- 0

# Sviluppo il loop per ottenere il vettore
while (i < max(cntry_indxs$gdp_percap, na.rm = T)) {
  i <- i + width
  classes[(i/width)+1] <- i
}

# Ricavo la distribuzione di frequenza in classi
distr_class <- table(cut(round(cntry_indxs$gdp_percap), breaks = classes))
distr_class

# Ecco la classe con la maggior frequenza:
names(which.max(distr_class))

# Calcolo media e mediana della variabile "gdp_percap"
mean(cntry_indxs$gdp_percap, na.rm = T)
median(cntry_indxs$gdp_percap, na.rm = T)

# Vi e' una sostanziale differenza fra media e mediana, per questo e' possibile
# intuire che la distribuzione risulta asimmetrica. 

# Sviluppo un boxplot della variabile "gdp_percap"
boxplot(cntry_indxs$gdp_percap,
        col = "blue")

# Dalla raffigurazione del boxplot cogliamo: la sostanziale differenza fra
# media e mediana, la sostanziale asimmetria positiva della distribuzione,
# la presenza di molti outliers e una moderata variabilita' dei dati.

# ____________
# Richiesta e)|

# Analizzo la relazione fra le variabili "life" e "hdi" mediante uno
# scatterplot
plot(cntry_indxs$hdi, cntry_indxs$life,
     pch = 19,
     col = "darkred",
     main = "Aspettativa di vita su HDI",
     xlab = "Indice di sviluppo umano",
     ylab = "Aspettativa di vita")

# Dallo scatterplot possiamo notare di primo impatto l'esistenza di una
# relazione molto forte fra le due variabili. E' possibile ,quindi, dire 
# che: all'aumentare dell'indice di sviluppo umano, aumenta l'aspettativa di 
# vita. 

# Per confermare cio' analizziamo covarianza e correlazione
cov(cntry_indxs$hdi, cntry_indxs$life, use = "complete.obs")
cor(cntry_indxs$hdi, cntry_indxs$life, use = "complete.obs")

# Notiamo una covarianza positiva, indicando spostamenti dei valori 
# prevalentemente concordi.  
# L'indice di correlazione risulta prossima all'1, suggerendo la presenza 
# di una correlazione quasi perfetta e positiva fra le due variabili.

