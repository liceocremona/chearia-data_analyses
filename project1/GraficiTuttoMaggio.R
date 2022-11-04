rm(list = ls(all = TRUE))
graphics.off()


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
  humidity[u] <- db1[]$humidity[[u]]$value
  tempiU[u] <- db1[]$humidity[[u]]$time
}

for (p in 1:lunghezzaP ) {
  pressure[p] <- db1[]$pressure[[p]]$value
  tempiP[p] <- db1[]$pressure[[p]]$time
}

for (i in 1:lunghezzaC ) {
  co[i] <- db1[]$CO[[i]]$value
  tempiC[i] <- db1[]$CO[[i]]$time
}


meseT <- as.numeric(word(tempiT, 2))
giornoT <- word(tempiT,3)
oreT <- as.numeric(word(tempiT, 4))
oraT <- gsub(' ','.', paste(oreT, gsub('[^[:alnum:] ]','',as.character((as.numeric(word(tempiT,5))*100/60)+(as.numeric(word(tempiT,6))*100/3600)))))
giornoEora <- as.numeric(gsub(' ','.', (paste(giornoT, gsub('[^[:alnum:] ]','',(as.numeric(gsub(' ','', word(tempiT,4,6)))*100/24))))))
meseEGiorno <- as.numeric(gsub(' ','.', paste(meseT, gsub('[^[:alnum:] ]','',as.numeric(giornoT)*100/24))))


df <- data.frame(Temperatura = temperature, Ore = as.numeric(oraT), Ora = oreT, Giorno = giornoT, Giornate = giornoEora )

df %>%
  #filter(Temperatura < 100) %>% per qualche motivo che non posso conoscere mi dice che Temperatura non la trova
  
  ggplot(aes(Ore, Temperatura))+
  geom_point(aes(color = Giorno))+
  geom_line()+
  labs(title  = "Temperatura")+
  facet_wrap(~Giorno, nrow = 3)+
  theme_bw()


df %>%
  #filter (Temperatura <100) %>%
  ggplot(aes(Giornate, Temperatura))+
  geom_point(aes(colour = Giorno, group = 1))+
  geom_smooth( se = F)+
  labs(title = "Temperatura il 2022 05")































