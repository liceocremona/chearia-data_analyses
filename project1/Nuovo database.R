rm(list = ls(all = TRUE))
graphics.off()
#shell("cls")


library("rjson")
library("stringr")
library("ggplot2")

url <- 'https://api.progettochearia.it/v1/resources/datas?type=json&sort=desc&dataid=temperature&day=2022-05-20'
db1 <- fromJSON( file = url)

temperature <- 0
tempi <- 0
lunghezza <- length(db1[]$temperature)


for (i in 1:lunghezza ) {
  temperature[i] <- db1[1]$temperature[[i]]$value
  tempi[i] <- db1[1]$temperature[[i]]$time
}

Tempi2 <- gsub('[^[:alnum:] ]',' ',tempi)    #Tempi2 rimuove tutti i simboli brutti di tempi e li sostituisce con uno spazio

Z <- word(Tempi2, 4,6) # Z prende da Tempi2 solo i valori dell'ora, dei minuti e dei secondi separati da uno spazio

V <- word(Tempi2, 4)  # V prende solo i valori dell'ora in formato stringa, non numerico

z <- as.numeric(gsub(' ','', Z)) # z rende numerico il valore di Z e elimina tutti gli spazi, rendendo tutto un unico grande numero

B <- as.factor(gsub(' ', '-', Z)) # bo non so perche' l'ho messo

b <- ( word(Tempi2[1],1,3)) # prende il valore dell'anno, del mese e del giorno separati da spazi come stringa 

m <- gsub(' ', '', word(Z, 2,3)) # prende attaccati i valori dei minuti e dei secondi

M <- word(Z, 1) # prende solo i valori dell'ora

L <- paste(M, m) # incolla insieme l'ora e minuti-secondi, mettendo uno spazio tra i 2

l <- as.numeric(gsub(' ', '.', L)) #prende i valori come numeri, e mette una virgola tra le ore e i minuti, cosi' da poterli usare come ore nel grafico

  titolo <- "Temperatura il : " 
  Titolo <- paste(titolo, b ) # mette insieme 2 stringhe, la prima con la prima parte del titolo, e la seconda con la stringa dell'anno, mese, giorno

df <- data.frame(Ore = l, Ora = word(Tempi2, 4), Temperatura = temperature, b, Titolo) #macello mastodontico,  da rivedere

#2 esempi di possibili grafici
#nel primo sono presenti diverse cose. Si puo' notare come come in entrambi però i colori dei valori sono definiti in base all'ora a cui appartengono
#e lo spessore è definito dal valore della temperatura


#nel primo Ho fatto un tema piu' scuro, una linea che unisce i vari punti piu' spessa e anche una linea che approssima l'andamento dei dati
#alla linea viola posso cambiare colore, spessore, trasparenza e se si preferisce posso farla come una retta, invece che linea curva


ggplot(df,aes(Ore, Temperatura))+
  geom_point(aes(colour = Ora, size = Temperatura))+
  geom_smooth(se = F, aes(colour = "purple"))+
  geom_line(size = 0.8, alpha = 0.5)+
  labs(title  = Titolo)

#il secondo ha invece un tema piu' chiaro, linee meno spesse e piu' trasparenti, non e' presente la linea dell'andamento 
#Ovviamente posso cambiare a scelta il colore della linea, ma non dei punti (se si vogliono fare diversi per ogni ora)

ggplot(df, aes(Ore, Temperatura))+
  geom_point( alpha = 0.5,
              aes(size = Temperatura,colour = Ora, group=1))+
  theme_bw()+
  geom_line(colour = "purple")+
  labs(title = Titolo)

ggplot(df, aes(Ore, Temperatura))+
  geom_line(aes(colour = Temperatura))+
  theme_bw()+
  labs(title = Titolo)

       