# Libraries & Functions ----
rm(list=ls())

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
library(chron)
library(tsoutliers)
library(seastests)
library(EnvStats)
library(isotree)


# Import data----

setwd("/Users/Ary/Documents/Data_Science/1st_year/DSLab/Progetto/Dati Energia (2)")
u1 = read.csv('u1.csv')


# Pre - processing ----
data_u1 = data.frame(data = as.Date(substr(u1$DATA, 1, 10), tryFormats =  "%Y%m%d"))
data_u1$KWh = as.numeric(gsub(',','.',u1$CONSUMO_ATTIVA_PRELEVATA))*.25
data_u1$ora = u1$ORA

# creo colonna ora_bis, trasformando in formato orario più comprensibile
data_u1$ora_bis <- data_u1$ora/100
data_u1$ora_bis <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", data_u1$ora_bis), 
                                                     format="%H%M"), 12, 16),'%H:%M'),'%H:%M:%S')

# Aggregazione ----
# by day
data_u1_day = data_u1 %>%
  group_by(data) %>%
  summarise(KWh = sum(KWh))


# studio della stagionalità

u1_ts <- ts(data_u1_day$KWh, frequency=365, start=c(2018,1,1))
stl <- decompose(u1_ts)
#seasonal(stl)
plot(stl)

# modello con stagionalità annuale
mod_tbats <- tbats(u1_ts)
mod_tbats

# modello con stagionalità multipla - settimana, mese, anno
u1_msts <- msts(data_u1_day$KWh, seasonal.periods=c(7,30,365))
u1_msts %>% mstl() %>%
  autoplot() 
mod_tbats_ms <- tbats(u1_msts)
mod_tbats_ms

# modello con stagionalità multipla - settimana, anno
u1_msts1 <- msts(data_u1_day$KWh, seasonal.periods=c(7,365))
u1_msts1 %>% mstl() %>%
  autoplot() 
mod_tbats_ms1 <- tbats(u1_msts1)
mod_tbats_ms1 #output migliore dei precedenti (rispetto a AIC)

# modello con stagionalità multipla - mese, anno
u1_msts2 <- msts(data_u1_day$KWh, seasonal.periods=c(30,365))
u1_msts2 %>% mstl() %>%
  autoplot() 
mod_tbats_ms2 <- tbats(u1_msts2)
mod_tbats_ms2 #output peggiore dei due precedenti

# modello con stagionalità multipla - settimana, mese
u1_msts3 <- msts(data_u1_day$KWh, seasonal.periods=c(7,30))
u1_msts3 %>% mstl() %>%
  autoplot() 
mod_tbats_ms3 <- tbats(u1_msts3)
mod_tbats_ms3 #output non ottimale

# Dai risultati osservati, sembrerebbe esserci una stagionalità multipla settimanale+annuale

# E' più importante stagionalità annuale o settimanale? Confronto modelli con stagionalità singola

# modello con stagionalità singola - settimana
u1_ts_w <- ts(data_u1_day$KWh, frequency=7, start=c(2018,1,1))
mod_tbats_ts_w <- tbats(u1_ts_w)
mod_tbats_ts_w # settimana
mod_tbats # anno
# stagionalità settimanale ha AIC più basso --> più importante


### APPROCCIO STL

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

# Nella funzione time_decompose, bisogna scegliere frequency = numero di osservazioni in un ciclo. 
# Nel nostro caso, modello migliore è con stagionalità multipla settimanale e annuale, 
# dovendo sceglierne solo una si sceglie la più importante --> settimanale

# metodo twitter --> dovrebbe essere il migliore per il nostro tipo di dati
df_anomalized <- data_u1_day %>%
  # dicendo che frequency = "1 week" il metodo automaticamente capisce se i dati
  # hanno una frequenza di 5 o 7 giorni a seconda dei dati a disposizione
  time_decompose(KWh, method="twitter", frequency="1 week", merge = TRUE) %>%
  anomalize(remainder) %>%
  time_recompose()
df_anomalized %>% glimpse()

df_anomalized %>% plot_anomalies(ncol = 3, alpha_dots = 0.75)

p1 <- df_anomalized %>%
  plot_anomaly_decomposition() +
  ggtitle("Freq = week, Trend = 'auto'")
p1 #decomposizione in osservato, stagionale, residui ecc

table <- data_u1_day %>% 
  time_decompose(KWh, method="twitter", frequency="1 week") %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == 'Yes')
# 15 anomalie

# metodo stl --> prova anche se sembra più adatto metodo twitter
df_anomalized1 <- data_u1_day %>%
  time_decompose(KWh, method="stl", frequency="1 week", merge = TRUE) %>%
  anomalize(remainder) %>%
  time_recompose()
df_anomalized1 %>% glimpse()

df_anomalized1 %>% plot_anomalies(ncol = 3, alpha_dots = 0.75)

p1 <- df_anomalized1 %>%
  plot_anomaly_decomposition() +
  ggtitle("Freq = week, Trend = 'auto'")
p1 #decomposizione in osservato, stagionale, residui ecc


# metodo twitter, cambio ampiezza intervallo di confidenza per identificare outliers

# cambiando alpha, ottengono più o meno outlier (alfa maggiore, si restringe l'intervallo di normalità attorno alla serie)
# se alpha ampio (quasi tutto è un outlier) posso giocare su percentuale max outliers da identificare nel dataset

# quasi tutto è outlier, gioco su max percentuale outliers nei dati
p4 <- data_u1_day %>%
  time_decompose(KWh, method="twitter", frequency="1 week") %>%
  anomalize(remainder, alpha = 0.3, max_anoms = 0.05) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("alpha = 0.3, max_anoms=5%")
p4

# con alpha=0.09, intervalli di normalità più stretti
p5 <- data_u1_day %>%
  time_decompose(KWh, method="twitter", frequency="1 week") %>%
  anomalize(remainder, alpha = 0.09, max_anoms = 0.3) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("alpha = 0.09, max_anoms=30%")
p5

# con alpha=0.07, intervalli di normalità più ampi
p6 <- data_u1_day %>%
  time_decompose(KWh, method="twitter", frequency="1 week") %>%
  anomalize(remainder, alpha = 0.07, max_anoms = 0.3) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("alpha = 0.07, max_anoms=30%")
p6

# tabella con date outliers (riferimento alpha=0.05)
table1 <- data_u1_day %>% 
  time_decompose(KWh, method="twitter", frequency="1 week") %>%
  anomalize(remainder, alpha = 0.05, max_anoms = 0.3) %>%
  time_recompose() %>%
  filter(anomaly == 'Yes')
# date identificate come giorni anomali
dates_anomalize <- table1$data


### APPROCCIO ARIMA

# plot serie temporale
plot(u1_ts,lwd=2,ylab="KWh")

# pacchetto tsoutliers con fitting automatico modello arima

# dovendo considerare una multi stagionalità, se si vuole utilizzare il modello arima
# è necessaria l'introduzione di termini di fourier come xreg

# identifico numero di termini di fourier
bestfit <- list(aicc=Inf)

# of transforms i should be less than half of seasonal period.

for (i in 1:3) {# weekly cycle
  for (j in 1:5) { #yearly cycle
    #specifying the fourier temrs
    myfourier <- c(i,j)
    xregressors <- fourier(u1_msts1, K=myfourier)
    #fitting the model
    fit_model <- auto.arima(y = u1_msts1, seasonal = FALSE, xreg = xregressors, stepwise = TRUE, lambda = "auto")
    #better model has lower aicc
    if (fit_model$aicc < bestfit$aicc) {
      bestfit <- fit_model
      bestfourier <- myfourier
    }
    #else break;
  }
}
bestfourier # (1,1)

# PROVARE SE FUNZIONA
outliers_u1_msts <- tso(u1_msts1, xreg=fourier(u1_msts1, K=c(1,1)), types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_u1_msts 


#u1_msts1
#prova1 <- tso(u1_msts1)
#plot.tsoutliers(prova1)

plot.tsoutliers(outliers_u1_msts)

# indici outliers
list_ind <- outliers_u1_msts$outliers$ind

# tabella con elementi anomali
out <- data_u1_day[list_ind,]
out

# save table output 
write.csv(out,"tso_table_u1.csv")


dates_tso <- out$data


### APPROCCIO TBATS - generalizzazione modello ETS che riesce e gestire dati ad alta frequenza

# considero multistagionalità settimanale e annuale
mod_tbats_ms <- tbats(u1_msts1)
mod_tbats_ms

fitted <- mod_tbats_ms$fitted.values

#create data frame with date and KWh for ets model
df <- data.frame(data = data_u1_day$data)
df$KWh <- fitted

colors <- c("Fitted values" = "blue", "Real values" = "darkorange1")

ggplot() + 
  geom_line(data = data_u1_day, aes(x = data, y = KWh, color = "Real values")) +
  geom_line(data = df, aes(x = data, y = KWh, color = "Fitted values")) +
  labs(x = "date",
       y = "KWh",
       color = "Legend") +
  scale_color_manual(values = colors)

df_errors <- data.frame(data = data_u1_day$data)
df_errors$error <- mod_tbats_ms$errors

# voglio selezionare come ouliers solo il 2% dei dati
summary(abs(df_errors$error))
quantile(abs(df_errors$error), probs=seq(0,1,0.02))
# 98% corrisponde a 437.91

# definiscon come outliers le osservazioni che hanno un errore associato maggiore di 500 in valore assoluto
table_tbats <- data_u1_day[which(abs(as.numeric(df_errors$error)) > 437.91), c("data","KWh")]
dates_tbats <- table_tbats$data

# metodo generalized extreme Studentized deviate test per identificare outliers
df <- data.frame(data = data_u1_day$data)
df$observed <- data_u1_day$KWh
df$fitted <- fitted
df$residuals <- mod_tbats_ms$errors

# il metodo è costruito considerando un upper bound per il numero di outliers che ci si aspetta
library(PMCMRplus)
gesd <- gesdTest(df$residuals, 20)
num_oss <- gesd$ix
data_u1_day[num_oss,]

# tabella con outliers
table_tbats_test <- data_u1_day[num_oss,]
dates_tbats_test <- table_tbats_test$data


### APPROCCIO CART

# algoritmo isolation forest (decision trees), non usa misure di distanza o densità 
# ma considera solo il fatto che le anomalie sono solitamente poche e abbastanza diverse 
# dagli altri dati

# modello 
iso <- isolation.forest(data_u1_day)

# predict outliers within dataset, soglia score outliers=0.6 (valori vicini a 1 sono outliers
# forti, vicini a 0.5 sono outliers nella media e vicino a 0 valori più normali/difficili da isolare)
data_u1_day$pred <- predict(iso, data_u1_day, type = "score")
data_u1_day$outlier <- as.factor(ifelse(data_u1_day$pred >= 0.6, "outlier", "normal"))
table(data_u1_day$outlier)

#plot
d <- ggplot(data_u1_day, aes(x = data, y = KWh)) + 
  geom_line(color="gray81")+
  geom_point(shape = 20, alpha = 0.5, aes(color=outlier)) +
  labs(x = "date", y = "KWh") +
  labs(alpha = "", colour="Legend")
d + scale_color_manual(values=c("gray81", "red3"))

# tabella e date outliers
table_out_cart <- data_u1_day[which(data_u1_day$outlier=="outlier"),]
dates_cart <- table_out_cart$data


# compare anomalies (daily data)

dates_anomalize
dates_tbats
dates_tbats_test
dates_cart
dates_tso

# date in comune agli algoritmi --->
# giorno 4/08/18
# giorno 21/06/20
# giorno 31/07/20
# (comunque i periodi di riferimento rimangono abbatanza simili)



