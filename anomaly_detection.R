# Libraries & Functions ----
library(ggplot2)
library(lmtest)
library(forecast)
library(lubridate)
library(scales)
library(gridExtra)
library(car)
library(suncalc)
library(tidyverse)
library(tibbletime)
library(anomalize)
library(timetk)
library(dplyr)

rm(list=ls())

# Import data----


setwd("/Users/Ary/Documents/Data_Science/1st_year/DSLab/Progetto/Dati Energia (2)")
u1 = read.csv('u1.csv')
u6 = read.csv('u6.csv')

# Pre - processing ----
data_u1 = data.frame(data = as.Date(substr(u1$DATA, 1, 10), tryFormats =  "%Y%m%d"))
data_u1$KWh = as.numeric(gsub(',','.',u1$CONSUMO_ATTIVA_PRELEVATA))*.25
data_u1$ora = u1$ORA

# creo colonna ora_bis, trasformando in formato orario più comprensibile
library(chron)
data_u1$ora_bis <- data_u1$ora/100
data_u1$ora_bis <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", data_u1$ora_bis), 
                                                     format="%H%M"), 12, 16),'%H:%M'),'%H:%M:%S')

# Aggregazione ----
# by day
data_u1_day = data_u1 %>%
  group_by(data) %>%
  summarise(KWh = sum(KWh))

# prova anomaly detection, dal sito https://www.analyticsvidhya.com/blog/2020/12/a-case-study-to-detect-anomalies-in-time-series-using-anomalize-package-in-r/

# il pacchetto anomalize nella funzione time_decompose accetta due metodi:
#   * STL --> lavora bene con trend a lungo termine, ma performa meno bene 
#             quando la componente stagionale è più forte del trend
#   * twitter --> differenza con stl nella rimozione del trend, che viene qui
#                 trattato rimuovendo la mediana dei dati invece che facendo il
#                 fitting di uno smoother. Lavora meglio quando la componente
#                 stagionale è preponderante.


data_u1_day <- as_tibble(data_u1_day)
class(data_u1_day)

df_anomalized <- data_u1_day %>%
  time_decompose(KWh, method="twitter", merge = TRUE) %>%
  anomalize(remainder) %>%
  time_recompose()
df_anomalized %>% glimpse()

df_anomalized %>% plot_anomalies(ncol = 3, alpha_dots = 0.75)

p1 <- df_anomalized %>%
  plot_anomaly_decomposition() +
  ggtitle("Freq/Trend = 'auto'")
p1 #decomposizione in osservato, stagionale, residui ecc


table <- data_u1_day %>% 
  time_decompose(KWh, method="twitter") %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == 'Yes')
# 15 anomalie

# cambiando alpha, ottengono più o meno outlier (alfa maggiore, si restringe l'intervallo di normalità attorno alla serie)
p4 <- data_u1_day %>%
  time_decompose(KWh, method="twitter") %>%
  anomalize(remainder, alpha = 0.05, max_anoms = 0.3) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("alpha = 0.05")
p4


## approccio ARIMA

library(tsoutliers)

# prova serie multistagionalità 
u1_ts <- msts(data_u1_day$KWh, seasonal.periods=c(7,30,365))
u1_ts %>% mstl() %>%
  autoplot() 
# sembra esserci solo stagionalità annuale, si utilizza serie che ha frequency 365 (capire se è il metodo corretto)

u1_ts <- ts(data_u1_day$KWh, frequency=365, start=c(2018,1,1))
u1_ts

# pacchetto tsoutliers con fitting automatico modello arima
outliers_u1_ts <- tso(u1_ts, types = c("TC", "AO", "LS", "IO", "SLS"), tsmethod="auto.arima")
outliers_u1_ts

plot.tsoutliers(outliers_u1_ts)

list_ind <- outliers_u1_ts$outliers$ind

out <- data_u1_day[list_ind,]
out

