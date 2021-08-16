# Libraries ----
rm(list=ls())

library(stringr)
library(readxl)

# Directory ----
dir <- "/Users/Ary/Documents/Data_Science/1st_year/DSLab/Progetto/Dati Energia (2)/"

# U1 ----
dir_u1 <- paste0(dir,list.files(dir)[1])
dir_u1 <- "/Users/Ary/Documents/Data_Science/1st_year/DSLab/Progetto/Dati Energia (2)/Dati U1"

dir_u1_18 <- paste0(dir_u1,'/',list.files(dir_u1)[1])
dir_u1_19 <- paste0(dir_u1,'/',list.files(dir_u1)[2])
dir_u1_20 <- paste0(dir_u1,'/',list.files(dir_u1)[3])

# 2018
u1_2018 <- data.frame()
lista <- list.files(dir_u1_18)
mesi_u1_18 <- paste0(c('Gennaio','Febbraio','Marzo','Aprile','Maggio','Giugno','Luglio','Agosto','Settembre',
                       'Ottobre','Novembre','Dicembre'),'2018')

for (m in mesi_u1_18){
  idx <- lista[str_detect(lista,m)]
  f <- paste0(dir_u1_18,'/',idx)
  df <- read_excel(f)[,1:8]
  u1_2018 <- rbind(u1_2018,df)
}


# 2019
u1_2019 <- data.frame()
lista <- list.files(dir_u1_19)
mesi_u1_19 <- paste0(c('Gennaio','Febbraio','Marzo','Aprile','Maggio','Giugno','Luglio','Agosto','Settembre',
                       'Ottobre','Novembre','dicembre'),'2019')

for (m in mesi_u1_19){
  idx <- lista[str_detect(lista,m)]
  for (i in idx){
    f <- paste0(dir_u1_19,'/',i)
    if (m=='dicembre2019'){
      df <- read.csv(f,sep=';')[,1:8]
    } else {df <- read_excel(f)[,1:8]}
    u1_2019 <- rbind(u1_2019,df)
  }
}

# 2020
u1_2020 <- data.frame()
lista <- list.files(dir_u1_20)
mesi_u1_20 <- lista[c(1,7,11,5,10,8,9,4,14,13,12,6)]


for (m in mesi_u1_20[1:6]){
  f <- paste0(dir_u1_20,'/',m)
  df <- read.csv(f,sep=';')
  u1_2020 <- rbind(u1_2020,df)
}

for (m in mesi_u1_20[7:12]){
  f <- paste0(dir_u1_20,'/',m)
  df <- read.csv(f,header = FALSE,skip = 1,sep = ';')[,1:8]
  colnames(df) <- colnames(u1_2020)
  u1_2020 <- rbind(u1_2020,df)
}

# controllo valori nulli e duplicati
sapply(u1_2018, function(x) sum(is.na(x)))
sapply(u1_2019, function(x) sum(is.na(x)))
sapply(u1_2020, function(x) sum(is.na(x)))

sum(duplicated(u1_2018))
sum(duplicated(u1_2019))
sum(duplicated(u1_2020))

# quando ci sono consumo attiva prelevata nulli
u1_2018[which(u1_2018$CONSUMO_ATTIVA_PRELEVATA == 0),c("DATA","ORA")] # nessuno
u1_2019[which(u1_2019$CONSUMO_ATTIVA_PRELEVATA == 0),c("DATA","ORA")] # possibili blackout?
u1_2020[which(u1_2020$CONSUMO_ATTIVA_PRELEVATA == 0),c("DATA","ORA")] # 31/07 completamente assente
                                                                      # 25/10 nullo dalle 3: ora legale, elimino dati nulli e tengo i rimanenti
                                                                      # altri casi sporadici: possibili balckout?

u1_2019[c(20444,20445,20446,20447),]

# pulizia datasets
u1_2019 = u1_2019[-which(is.na(u1_2019$POD) == T ),] # valori nulli eliminati
u1_2019 = u1_2019[!duplicated(u1_2019),] 
u1_2020 = u1_2020[-which(u1_2020$DATA == 20201025 & u1_2020$CONSUMO_ATTIVA_PRELEVATA==0),]


u1 = rbind(u1_2018,u1_2019,u1_2020)
u1$CONSUMO_ATTIVA_PRELEVATA = gsub(',','.',u1$CONSUMO_ATTIVA_PRELEVATA)


# U6 ----

dir_u6<- paste0(dir,list.files(dir)[2])
dir_u6 <- "/Users/Ary/Documents/Data_Science/1st_year/DSLab/Progetto/Dati Energia (2)/Dati U6"


dir_u6_18 <- paste0(dir_u6,'/',list.files(dir_u6)[1])
dir_u6_19 <- paste0(dir_u6,'/',list.files(dir_u6)[2])
dir_u6_20 <- paste0(dir_u6,'/',list.files(dir_u6)[3])

# 2018
u6_2018 <- data.frame()
lista <- list.files(dir_u6_18)
mesi_u6_18 <- paste0(c('Gennaio','Febbraio','Marzo','Aprile','Maggio','Giugno','Luglio','Agosto','Settembre',
                       'Ottobre','Novembre','Dicembre'),'2018')

for (m in mesi_u6_18){
  idx <- lista[str_detect(lista,m)]
  for (i in idx){
    f <- paste0(dir_u6_18,'/',i)
    df <- read_excel(f)[,1:8]
    u6_2018 <- rbind(u6_2018,df)
  }
}

# 2019
u6_2019 <- data.frame()
lista <- list.files(dir_u6_19)

mesi_u6_19 <- as.vector(lista[c(5,4,10,2,3,9,6,7,1,12,11,14,13)])

for (m in mesi_u6_19[1:11]){
  f <- paste0(dir_u6_19,'/',m)
  if (m==mesi_u6_19[8]){
    df <- read_excel(f,col_types = c("text", "skip", "numeric", "numeric", "skip", "skip", 
                                     "numeric","numeric", "numeric", "text", "text"))
  }
  else {df <- read_excel(f)[,1:8]}
  u6_2019 <- rbind(u6_2019,df)
}

for (m in mesi_u6_19[12:13]){
  f <- paste0(dir_u6_19,'/',m)
  df <- read.csv(f,header = FALSE,skip = 1,sep = ';')[,1:8]
  colnames(df) <- colnames(u6_2019)
  u6_2019 <- rbind(u6_2019,df)
}

# 2020
u6_2020 <- data.frame()
lista <- list.files(dir_u6_20)
lista
mesi_u6_20 <- lista[c(6,4,12,2,11,8,10,1,16,15,14,3)]

for (m in mesi_u6_20[1:7]){
  f <- paste0(dir_u6_20,'/',m)
  if (m==mesi_u6_20[7]){
    df <- read_excel(f,col_names = FALSE, col_types = c("text", 
                                                        "numeric", "skip", "skip", "skip", 
                                                        "skip", "skip", "numeric", 
                                                        "numeric", "text", "text", "numeric", 
                                                        "text"), skip = 2) #skip 2 guardare il file
    colnames(df) <- colnames(u6_2020)
  } else {df <- read.csv(f,sep=';')[,1:8]}
  u6_2020 <- rbind(u6_2020,df)
}
for (m in mesi_u6_20[8:12]){
  f <- paste0(dir_u6_20,'/',m)
  df <- read.csv(f,header = FALSE,skip = 1,sep = ';')[,1:8]
  colnames(df) <- colnames(u6_2020)
  u6_2020 <- rbind(u6_2020,df)
}

# controllo valori nulli e duplicati
sapply(u6_2018, function(x) sum(is.na(x)))
sapply(u6_2019, function(x) sum(is.na(x)))
sapply(u6_2020, function(x) sum(is.na(x)))

sum(duplicated(u6_2018))
sum(duplicated(u6_2019))
sum(duplicated(u6_2020))

# quando ci sono consumo attiva prelevata nulli

u6_2018[which(u6_2018$CONSUMO_ATTIVA_PRELEVATA == 0),c("DATA","ORA")] # possibili blackout?
u6_2019[which(u6_2019$CONSUMO_ATTIVA_PRELEVATA == 0),c("DATA","ORA")] # possibili blackout?
u6_2020[which(u6_2020$CONSUMO_ATTIVA_PRELEVATA == 0),c("DATA","ORA")] # 25/10 nullo dalle 3: ora legale, elimino dati nulli e tengo i rimanenti
u6_2020[which(u6_2020$CONSUMO_ATTIVA_PRELEVATA == '0.0'),c("DATA","ORA")] #31/07 tutto il giorno

#pulizia datasets
u6_2020 = u6_2020[-which(u6_2020$DATA == 20201025 & u6_2020$CONSUMO_ATTIVA_PRELEVATA==0),]

u6 = rbind(u6_2018, u6_2019, u6_2020)
u6$CONSUMO_ATTIVA_PRELEVATA = gsub(',','.',u6$CONSUMO_ATTIVA_PRELEVATA)

which(u6$DATA == "20200731")
u6[which(u6$DATA == "20200731"),]

# problema, giugno 2020 non c'è per U6 poiché si tratta di U1 ripetuto
table(u6$POD)

# Clear Output ----
setwd(dir)
write.csv(u1,"u1.csv", row.names = FALSE)
write.csv(u6,"u6.csv", row.names = FALSE)