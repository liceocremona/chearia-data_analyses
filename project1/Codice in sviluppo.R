rm(list = ls(all = TRUE))
graphics.off()
#shell("cls")


library("rjson")
library("stringr")
library("ggplot2")
url <- 'https://api.progettochearia.it/v1/resources/datas?type=json&sort=desc&dataid=temperature&dataid=humidity&dataid=pressure&dataid=CO&dataid=ozone&gte=2022-05-01_00%3A00%3A00&lte=2022-05-31_00%3A00%3A00'
db1 <- fromJSON( file = url)

temperature <- 0
humidity <- 0
pressure <- 0
co <- 0



tempiT <- 0
tempiU <-0
tempiP <- 0 
tempiC <- 0
lunghezzaT <- length(db1[]$temperature)
lunghezzaU <- length(db1[]$humidity)
lunghezzaP <- length(db1[]$pressure)
lunghezzaC <- length(db1[]$CO)




for (t in 1:lunghezzaT ) {
  temperature[t] <- db1[1]$temperature[[t]]$value
  tempiT[t] <- gsub('[^[:alnum:] ]',' ',db1[1]$temperature[[t]]$time)
}

for (u in 1:lunghezzaU ) {
  humidity[u] <- db1[2]$humidity[[u]]$value
  tempiU[u] <- gsub('[^[:alnum:] ]',' ',db1[2]$humidity[[u]]$time)
}

for (p in 1:lunghezzaP ) {
  pressure[p] <- db1[3]$pressure[[p]]$value
  tempiP[p] <- gsub('[^[:alnum:] ]',' ',db1[3]$pressure[[p]]$time)
}

for (i in 1:lunghezzaC ) {
  co[i] <- db1[4]$CO[[i]]$value
  tempiC[i] <- gsub('[^[:alnum:] ]',' ',db1[4]$CO[[i]]$time)
}


meseT <- as.numeric(word(tempiT, 2))
giornoT <- word(tempiT,3)
oreT <- as.numeric(word(tempiT, 4))
oraT <- gsub(' ','.', paste(oreT, gsub('[^[:alnum:] ]','',as.character((as.numeric(word(tempiT,5))*100/60)+(as.numeric(word(tempiT,6))*100/3600)))))
giornoEora <- as.numeric(gsub(' ','.', (paste(giornoT, gsub('[^[:alnum:] ]','',(as.numeric(gsub(' ','', word(tempiT,4,6)))*100/24))))))
meseEGiorno <- as.numeric(gsub(' ','.', paste(meseT, gsub('[^[:alnum:] ]','',as.numeric(giornoT)*100/24))))

dfT <- data.frame(Temperatura = temperature, Ore = as.numeric(oraT), Ora = oreT, Giorno = giornoT, Giornate = giornoEora )

dfT %>%
 # filter( Temperatura <100) %>%
  
  ggplot(aes(Ore, Temperatura))+
  geom_point(aes(color = Giorno))+
  geom_line()+
  labs(title  = "Temperatura")+
  facet_wrap(~Giorno, nrow = 3)+
  theme_bw()


dfT %>%
 # filter (Temperatura <100) %>%
  
  ggplot(aes(Giornate, Temperatura))+
  geom_point(aes(colour = Giorno, group = 1))+
  geom_smooth( se = F)+
  labs(title = "Temperatura il 2022 05")


meseU <- as.numeric(word(tempiU, 2))
giornoU <- word(tempiU,3)
oreU <- as.numeric(word(tempiU, 4))
oraU <- gsub(' ','.', paste(oreU, gsub('[^[:alnum:] ]','',as.character((as.numeric(word(tempiU,5))*100/60)+(as.numeric(word(tempiU,6))*100/3600)))))
giornoEoraU <- as.numeric(gsub(' ','.', (paste(giornoU, gsub('[^[:alnum:] ]','',(as.numeric(gsub(' ','', word(tempiU,4,6)))*100/24))))))
meseEGiornoU <- as.numeric(gsub(' ','.', paste(meseU, gsub('[^[:alnum:] ]','',as.numeric(giornoU)*100/24))))

dfU <- data.frame(Umidità = humidity, Ore = as.numeric(oraU), Ora = oreU, Giorno = giornoU, Giornate = giornoEoraU )
dfU %>%
 # filter(35< Umidità, Umidità<60)%>%
  ggplot(aes(Ore, Umidità))+
  geom_point(aes(color = Giorno))+
  geom_line()+
  labs(title  = "Umidità")+
  facet_wrap(~Giorno, nrow = 3)+
  theme_bw()


dfU %>%
  #filter(35< Umidità, Umidità<60) %>%
  ggplot(aes(Giornate, Umidità))+
  geom_point(aes(colour = Giorno, group = 1))+
  geom_smooth( se = F)+
  labs(title = "Umidità il 2022 05")



meseP <- as.numeric(word(tempiP, 2))
giornoP <- word(tempiP,3)
oreP <- as.numeric(word(tempiP, 4))
oraP <- gsub(' ','.', paste(oreP, gsub('[^[:alnum:] ]','',as.character((as.numeric(word(tempiP,5))*100/60)+(as.numeric(word(tempiP,6))*100/3600)))))
giornoEoraP <- as.numeric(gsub(' ','.', (paste(giornoP, gsub('[^[:alnum:] ]','',(as.numeric(gsub(' ','', word(tempiP,4,6)))*100/24))))))
meseEGiornoP <- as.numeric(gsub(' ','.', paste(meseP, gsub('[^[:alnum:] ]','',as.numeric(giornoP)*100/24))))

dfP <- data.frame(Pressione = pressure, Ore = as.numeric(oraP), Ora = oreP, Giorno = giornoP, Giornate = giornoEoraP )

dfP %>%
  
  ggplot(aes(Ore, Pressione))+
  geom_point(aes(color = Giorno))+
  geom_line()+
  labs(title  = "Pressione")+
  facet_wrap(~Giorno, nrow = 3)+
  theme_bw()


dfP %>%
  ggplot(aes(Giornate, Pressione))+
  geom_point(aes(colour = Giorno, group = 1))+
  geom_smooth( se = F)+
  labs(title = "Pressione il 2022 05")


meseC <- as.numeric(word(tempiC, 2))
giornoC <- word(tempiC,3)
oreC <- as.numeric(word(tempiC, 4))
oraC <- gsub(' ','.', paste(oreC, gsub('[^[:alnum:] ]','',as.character((as.numeric(word(tempiC,5))*100/60)+(as.numeric(word(tempiC,6))*100/3600)))))
giornoEoraC <- as.numeric(gsub(' ','.', (paste(giornoC, gsub('[^[:alnum:] ]','',(as.numeric(gsub(' ','', word(tempiC,4,6)))*100/24))))))
meseEGiornoC <- as.numeric(gsub(' ','.', paste(meseC, gsub('[^[:alnum:] ]','',as.numeric(giornoC)*100/24))))

dfC <- data.frame(CO <- co, Ore = as.numeric(oraC), Ora = oreC, Giorno = giornoC, Giornate = giornoEoraC )

dfC %>%
  filter(CO <150)%>%
  ggplot(aes(Ore, CO))+
  geom_point(aes(color = Giorno))+
  geom_line()+
  labs(title  = "CO")+
  facet_wrap(~Giorno, nrow = 3)+
  theme_bw()


dfC %>%
  filter(CO < 150)%>%
  ggplot(aes(Giornate, CO))+
  geom_point(aes(colour = Giorno))+
  geom_smooth( se = F)+
  labs(title = "CO il 2022 05")




















