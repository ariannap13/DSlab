# Libraries & Functions ----
library(tidyverse)
library(ggplot2)
library(lmtest)
library(forecast)
library(lubridate)
library(scales)
library(gridExtra)
library(car)
library(suncalc)
library(lubridate)
library(chron)

rm(list=ls())
sunset = function(date) {
  a = getSunlightTimes(date = date, lat = 45.520068121587855, lon = 9.211679574089654, tz = "CET")
  time <- str_split(a$sunset, " ")[[1]][2]
  return(time)
}
sunrise = function(date) {
  a = getSunlightTimes(date = date, lat = 45.520068121587855, lon = 9.211679574089654, tz = "CET")
  time <- str_split(a$sunrise, " ")[[1]][2]
  return(time)
}


# Import data----


setwd("/Users/Ary/Documents/Data_Science/1st_year/DSLab/Progetto/Dati Energia (2)")
u1 = read.csv('u1.csv')
u6 = read.csv('u6.csv')

# Pre - processing ----
data_u1 = data.frame(data = as.Date(substr(u1$DATA, 1, 10), tryFormats =  "%Y%m%d"))
data_u1$KWh = as.numeric(gsub(',','.',u1$CONSUMO_ATTIVA_PRELEVATA))*.25
data_u1$ora = u1$ORA

# creo colonna ora_bis, trasformando in formato orario più comprensibile
data_u1$ora_bis <- data_u1$ora/100
data_u1$ora_bis <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", data_u1$ora_bis), 
                                                     format="%H%M"), 12, 16),'%H:%M'),'%H:%M:%S')

data_u1$luce <- ifelse((times(data_u1$ora_bis) >= times(lapply(data_u1$data,sunrise))) &
                         (times(data_u1$ora_bis) <= times(lapply(data_u1$data,sunset))),1,0)


data_u6 = data.frame(data = as.Date(substr(u6$DATA, 1, 10), tryFormats =  "%Y%m%d"))
data_u6$KWh = as.numeric(gsub(',','.',u6$CONSUMO_ATTIVA_PRELEVATA))*.25
data_u6$ora = u6$ORA

data_u6$ora_bis <- data_u6$ora/100
data_u6$ora_bis <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", data_u6$ora_bis), 
                                                     format="%H%M"), 12, 16),'%H:%M'),'%H:%M:%S')

data_u6$luce <- ifelse((times(data_u6$ora_bis) >= times(lapply(data_u6$data,sunrise))) &
                         (times(data_u6$ora_bis) <= times(lapply(data_u6$data,sunset))),1,0)


data_u1$apertura = ifelse((weekdays(data_u1$data) != "sabato") &
                            (weekdays(data_u1$data) != "domenica") &
                            (data_u1$ora >= 73000) &
                            (data_u1$ora <= 203000),1,0)

data_u6$apertura = ifelse(((weekdays(data_u6$data) != "sabato") &
                             (weekdays(data_u6$data) != "domenica") &
                             (data_u6$ora >= 73000) &
                             (data_u6$ora <= 220000)) |
                            ((weekdays(data_u6$data) == "sabato") &
                               (data_u6$ora >= 80000) &
                               (data_u6$ora <= 140000)),1,0)

# save files

write.csv(data_u1,"dati_compl_u1.csv", row.names = FALSE)
write.csv(data_u6,"dati_compl_u6.csv", row.names = FALSE)

# Gli edifici sono regolarmente aperti dal lunedì al venerdì dalle ore 7:30 alle ore 20:30, 
# ad eccezione dell’edificio U6 che chiude alle ore 22:00.
# 
# Nelle giornate di sabato gli edifici resteranno chiusi, ad eccezione di quelli di seguito indicati:
#   
#   edificio U6 (Milano, Piazza Dell’Ateneo Nuovo, 1) dalle ore 8.00 alle ore 14.00
# edificio U7 (Milano, Via Bicocca degli Arcimboldi, 8) dalle ore 8:00 alle ore 18:30
# edificio U16 (Milano, Via T. Mann, 8) dalle ore 8:00 alle ore 18:30
# edificio U8 (Monza, Via Cadore, 48) dalle ore 8:00 alle ore 12:30
# In questi edifici potranno essere svolte e programmate tutte le attività didattiche.


#open data
setwd("/Users/Ary/Documents/Data_Science/1st_year/DSLab/Progetto")
u1 = read.csv('dati_compl_u1.csv')
u6 = read.csv('dati_compl_u6.csv')


# stagionalità multipla (giorno-settimana-anno)
u1_ts1 <- msts(u1$KWh, seasonal.periods=c(4*24,4*24*7,4*24*365))

u1_ts1 %>% mstl() %>%
  autoplot() 

plot(u1_ts1)

