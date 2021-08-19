# Libraries & Functions ----
rm(list=ls())

library(ggplot2)
# library(lmtest)
library(forecast)
# library(lubridate)
library(scales)
library(gridExtra)
# library(car)
# library(suncalc)
library(tidyverse)
# library(tibbletime)
library(anomalize)
# library(timetk)
# library(dplyr)
# library(chron)
# library(seastests)
# library(EnvStats)
library(isotree)
# library(stats)
library(cluster)
library(viridis)
library(PMCMRplus)
library(data.table)

# Import data ----

setwd("/Users/Ary/Documents/Data_Science/1st_year/DSLab/Progetto/Dati Energia (2)")
u6 = read.csv('u6.csv')

# Pre - processing ----
data_u6 = data.frame(data = as.Date(substr(u6$DATA, 1, 10), tryFormats =  "%Y%m%d"))
data_u6$KWh = as.numeric(gsub(',','.',u6$CONSUMO_ATTIVA_PRELEVATA))*.25
data_u6$ora = u6$ORA

## creazione colonna ora_bis, trasformando in formato orario più comprensibile
data_u6$ora_bis <- data_u6$ora/100
data_u6$ora_bis <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", data_u6$ora_bis), 
                                                     format="%H%M"), 12, 16),'%H:%M'),'%H:%M:%S')

## Aggregazione by day
data_u6_day = data_u6 %>%
  group_by(data) %>%
  summarise(KWh = sum(KWh))

## mostro sovrapposizione giugno 2020 con u1

colors <- c("U1" = "deeppink4", "U6" = "darkorange2")
ggplot() + 
  geom_line(data = data_u1_day, aes(x = data, y = KWh, color = "U1")) +
  geom_line(data = data_u6_day, aes(x = data, y = KWh, color = "U6")) +
  labs(x = "date",
       y = "kWh",
       color = "Legend") +
  scale_color_manual(values = colors) +
  scale_x_date(breaks = date_breaks("6 months"),
                               labels = date_format("%b %y")) +
  theme_bw()


## filtering per togliere da giugno 2020
data_u6_day <- filter(data_u6_day, data < as.Date("2020-06-01"))

## studio della stagionalità

### decomposizione serie stagionalità annuale
u6_ts <- ts(data_u6_day$KWh, frequency=365, start=c(2018,1,1))
stl <- decompose(u6_ts)
#seasonal(stl)
plot(stl)

### modello con stagionalità annuale
mod_tbats <- tbats(u6_ts)
mod_tbats #AIC = 16446.53

### modello con stagionalità multipla - settimana, mese, anno
u6_msts <- msts(data_u6_day$KWh, seasonal.periods=c(7,30,365))
u6_msts %>% mstl() %>%
  autoplot() +
  theme_bw() 
mod_tbats_ms <- tbats(u6_msts)
mod_tbats_ms #AIC = 15822.21

### modello con stagionalità multipla - settimana, anno
u6_msts1 <- msts(data_u6_day$KWh, seasonal.periods=c(7,365))
u6_msts1 %>% mstl() %>%
  autoplot() +
  theme_bw()
mod_tbats_ms1 <- tbats(u6_msts1)
mod_tbats_ms1 #AIC = 15808.14

### modello con stagionalità multipla - mese, anno
u6_msts2 <- msts(data_u6_day$KWh, seasonal.periods=c(30,365))
u6_msts2 %>% mstl() %>%
  autoplot() +
  theme_bw()
mod_tbats_ms2 <- tbats(u6_msts2)
mod_tbats_ms2 #AIC = 16415.36

### modello con stagionalità multipla - settimana, mese
u6_msts3 <- msts(data_u6_day$KWh, seasonal.periods=c(7,30))
u6_msts3 %>% mstl() %>%
  autoplot() +
  theme_bw()
mod_tbats_ms3 <- tbats(u6_msts3)
mod_tbats_ms3 #AIC = 15809.85


### modello con stagionalità singola - settimana
u6_ts_w <- ts(data_u6_day$KWh, frequency=7, start=c(2018,1,1))
mod_tbats_ts_w <- tbats(u6_ts_w)
mod_tbats_ts_w # settimana AIC = 15797.05
# riporto AIC stagionalità annuale
mod_tbats # anno AIC = 16446.53

### modello con stagionalità singola - mese
u6_ts_m <- ts(data_u6_day$KWh, frequency=30, start=c(2018,1,1))
mod_tbats_ts_m <- tbats(u6_ts_m)
mod_tbats_ts_m #AIC = 16440.06

" Dai risultati osservati, sembrerebbe esserci una stagionalità settimanale.
  Si sceglie di realizzare il fitting di un modello arima con il metodo auto.arima."

### modello arima
model_arima <- auto.arima(u6_ts_w)
model_arima # viene selezionato un modello SARIMA
# viene indicata la stagionalità settimanale 

## studio dei residui del modello SARIMA selezionato
AutoCorrelation <- acf(resid(model_arima), plot = FALSE)
plot(AutoCorrelation, main=NA)
PartialAutoCorrelation <- pacf(resid(model_arima), plot = FALSE)
plot(PartialAutoCorrelation, main=NA)
# la situazione a livello di autocorrelazione non appare particolarmente problematica

checkresiduals(model_arima) # grande p-value per ljung-box test: i residui non possono essere considerati come diversi da una serie white noise
# i residui non sono esattamente normali ma la condizione di non normalità non è estrema

# Anomaly Detection ----

## APPROCCIO DECOMPOSIZIONE STAGIONALE

" 
# il pacchetto anomalize nella funzione time_decompose accetta due metodi:
#   * STL --> lavora bene con trend a lungo termine, ma performa meno bene 
#             quando la componente stagionale è più forte del trend
#   * twitter --> differenza con stl nella rimozione del trend, che viene qui
#                 trattato rimuovendo la mediana dei dati invece che facendo il
#                 fitting di uno smoother. Lavora meglio quando la componente
#                 stagionale è preponderante."


data_u6_day <- as_tibble(data_u6_day)
class(data_u6_day)

### metodo stl --> prova anche se sembra più adatto metodo twitter

# prova con valori di default e prime analisi grafiche
df_anomalized1 <- data_u6_day %>%
  time_decompose(KWh, method="stl", frequency="1 week", merge = TRUE) %>%
  anomalize(remainder) %>%
  time_recompose()
df_anomalized1 %>% glimpse()
df_anomalized1 %>% plot_anomalies(ncol = 3, alpha_dots = 0.75)
p1 <- df_anomalized1 %>%
  plot_anomaly_decomposition() +
  ggtitle("Freq = week, Trend = 'auto'")
p1 #decomposizione in osservato, stagionale, residui ecc


### metodo twitter --> il migliore per il nostro tipo di dati

# prova con valori di default e prime analisi grafiche
df_anomalized <- data_u6_day %>%
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

# prova con alpha=0.05, metodo detection IQR (default)
p6 <- data_u6_day %>%
  time_decompose(KWh, method="twitter", frequency="1 week") %>%
  anomalize(remainder, alpha = 0.05, max_anoms = 0.3) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("alpha = 0.05")
p6

# prova con alpha == 0.05, metodo detection GESD (GRAFICO DEFINITIVO)
p61 <- data_u6_day %>%
  time_decompose(KWh, method="twitter", frequency="1 week") %>%
  anomalize(remainder, alpha = 0.05, max_anoms = 0.30, method='gesd') %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = T)+
  theme_bw()+
  theme(legend.position = 'bottom')+
  xlab('date')
p61

# prova con alpha=0.09, metodo detection GESD
p5 <- data_u6_day %>%
  time_decompose(KWh, method="twitter", frequency="1 week") %>%
  anomalize(remainder, alpha = 0.09, max_anoms = 0.3, method="gesd") %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("alpha = 0.09, max_anoms=30%")
p5

# TABELLA DEFINITIVA, con alpha = 0.05 e metodo di detection GESD
table1 <- data_u6_day %>% 
  time_decompose(KWh, method="twitter", frequency="1 week") %>%
  anomalize(remainder, alpha = 0.05, max_anoms = 0.3, method="gesd") %>%
  time_recompose() %>%
  filter(anomaly == 'Yes') # 26 anomalie
dates_anomalize <- table1$data


## APPROCCIO SARIMA e analisi residui

# modello sarima identificato in precedenza
model_arima

# valori fitted
fitted <- model_arima$fitted

# creazione dataframe contenente i valori fitted
df <- data.frame(data = data_u6_day$data)
df$KWh <- fitted

# rappresentazione grafica serie fitted vs real
colors <- c("Fitted values" = "blue2", "Real values" = "darkorange1")
ggplot() + 
  geom_line(data = data_u6_day, aes(x = data, y = KWh, color = "Real values")) +
  geom_line(data = df, aes(x = data, y = KWh, color = "Fitted values")) +
  labs(x = "date",
       y = "kWh",
       color = "Legend") +
  scale_color_manual(values = colors)+
  theme_bw()+
  theme(legend.position = 'bottom')

# dataframe contente i residui
df_errors <- data.frame(data = data_u6_day$data)
df_errors$error <- model_arima$residuals

### METODO QUANTILI (top x% distribuzione dei residui, x=2,5,10)

# selezione top 2%
summary(abs(df_errors$error))
quantile(abs(df_errors$error), probs=seq(0,1,0.02))
# 98% corrisponde a 716.46
# definisco come outliers le osservazioni che hanno un errore associato maggiore di 716.46
table_sarima_2 <- data_u6_day[which(abs(as.numeric(df_errors$error)) > 716.46), c("data","KWh")]
dates_sarima_2 <- table_sarima_2$data

# selezione top 5%
summary(abs(df_errors$error))
quantile(abs(df_errors$error), probs=seq(0,1,0.05))
# 95% corrisponde a 500.58
# definisco come outliers le osservazioni che hanno un errore associato maggiore di 500.58
table_sarima_5 <- data_u6_day[which(abs(as.numeric(df_errors$error)) > 500.58), c("data","KWh")]
dates_sarima_5 <- table_sarima_5$data

# selezione top 10%
summary(abs(df_errors$error))
quantile(abs(df_errors$error), probs=seq(0,1,0.1))
# 90% corrisponde a 292.57
# definisco come outliers le osservazioni che hanno un errore associato maggiore di 292.57
table_sarima_10 <- data_u6_day[which(abs(as.numeric(df_errors$error)) > 292.57), c("data","KWh")]
dates_sarima_10 <- table_sarima_10$data

### METODO GESD

# dataframe con valori osservati, fitted e residui
df <- data.frame(data = data_u6_day$data)
df$observed <- data_u6_day$KWh
df$fitted <- fitted
df$residuals <- model_arima$residuals

# il metodo è costruito considerando un upper bound per il numero di outliers che ci si aspetta 
# (fatto corrispondere a numero trovato nel metodo dei quantili)

# corrispondente del top 2%
gesd <- gesdTest(df$residuals, 18)
num_oss <- gesd$ix
data_u6_day[num_oss,]
# tabella con outliers
table_sarima_test_2 <- data_u6_day[num_oss,]
dates_sarima_test_2 <- table_sarima_test_2$data

# corrispondente del top 5%
gesd <- gesdTest(df$residuals, 45)
num_oss <- gesd$ix
data_u6_day[num_oss,]
# tabella con outliers
table_sarima_test_5 <- data_u6_day[num_oss,]
dates_sarima_test_5 <- table_sarima_test_5$data

# corrispondente del top 10%
gesd <- gesdTest(df$residuals, 89)
num_oss <- gesd$ix
data_u6_day[num_oss,]
# tabella con outliers
table_sarima_test_10 <- data_u6_day[num_oss,]
dates_sarima_test_10 <- table_sarima_test_10$data


## APPROCCIO ISOLATION FOREST

# modello 
iso <- isolation.forest(data_u6_day)

# predict outliers within dataset
data_u6_day$pred <- predict(iso, data_u6_day, type = "score")
# soglia score outliers=0.6 (valori vicini a 1 sono outliers forti, vicini a 0.5 sono
# outliers nella media e vicino a 0 valori più normali/difficili da isolare)
data_u6_day$outlier <- as.factor(ifelse(data_u6_day$pred >= 0.6, "outlier", "normal"))
table(data_u6_day$outlier)

# plot
ggplot(data_u6_day, aes(x = data, y = KWh)) +
  geom_line(color="gray81") +
  geom_point(shape = 20, alpha = 0.5, aes(color=outlier), size = 2) +
  labs(x = "date", y = "kWh") +
  labs(alpha = "", colour="Legend") +
  scale_color_manual(values=c("gray81", "red3"))+
  theme_bw()+
  theme(legend.position = 'bottom')

# tabella e date outliers
table_out_cart <- data_u6_day[which(data_u6_day$outlier=="outlier"),]
dates_cart <- table_out_cart$data


## APPROCCIO K-MEANS 

# set seed per garantire riproducibilità
set.seed(123)

# definizioni valori within sum of squares e silhouette values per un numero di k da 2 a 15
wth = vector()
sil = vector()
kmax = 10
for (k in 2:kmax) {
  km = kmeans(as.data.frame(data_u6_day)[,2], k)
  wth[k-1] = km$tot.withinss
  ss = silhouette(km$cluster, dist(as.data.frame(data_u6_day)[,2]))
  sil[k-1] = mean(ss[,3])
}
df = data.frame(k = 2:10, wth, sil)

# grafico metodo elbow
el_plot = ggplot(data = df, aes(y = wth, x = k))+
  geom_line()+
  geom_point()+
  theme_classic()+
  scale_x_continuous(name = "Number of clusters K",
                     breaks = seq(2,10,1))+
  scale_y_continuous(name = "Total within-clusters sum of squares",
                     labels = comma,
                     breaks = seq(0,60000000000,10000000))+
  labs(title = 'Elbow method')

# grafico metodo silhouette
sil_plot = ggplot(data = df, aes(y = sil, x = k))+
  geom_line()+
  geom_point()+
  theme_classic()+
  scale_x_continuous(name = "Number of clusters K",
                     breaks = seq(2,10,1))+
  scale_y_continuous(name = "Average Silhouettes")+
  labs(title = 'Silhouette method')

grid.arrange(el_plot,sil_plot, nrow =1)

# clusterizzazione definitiva e assegnazione centroide del cluster ad ogni osservazione
cluster = kmeans(as.data.frame(data_u6_day)[,2], 3)
data_u6_day_center = cbind(as.data.frame(data_u6_day), cluster = cluster$cluster)
x = vector()
for (i in 1:nrow(data_u6_day_center)) {
  if (data_u6_day_center[i,5] == 1) {
    x[i] = cluster$centers[1]
  } else if (data_u6_day_center[i,5] == 2) {
    x[i] = cluster$centers[2]
  } else if (data_u6_day_center[i,5] == 3) {
    x[i] = cluster$centers[3]
  } 
}
data_u6_day_center = cbind(data_u6_day_center, center = x)

# distanza tra KWh osservazione e valore centroide del proprio cluster
data_u6_day_center$dist = apply(data_u6_day_center[,c(2,6)], 1, dist)
# ordine decrescente distanze
temp = data_u6_day_center %>%
  arrange(desc(dist))

# top x% della distribuzione delle distanze

# top 2%
outlier_fraction = 0.02
temp_2 = temp[1:round(nrow(temp)*outlier_fraction),]
nrow(temp_2)

# top 5%
outlier_fraction = 0.05
temp_5 = temp[1:round(nrow(temp)*outlier_fraction),]
nrow(temp_5)

# top 10%
outlier_fraction = 0.1
temp_10 = temp[1:round(nrow(temp)*outlier_fraction),]
nrow(temp_10)

# esempio plot anomalie con top 5%
ggplot()+
  geom_line(data = data_u6_day, aes(x = as.Date(data), y = KWh), size = 0.7, color="gray65")+
  geom_point(data = temp_5, aes(x = as.Date(data), y = KWh), color = 'red3')+
  labs(x = "date",
       y = "kWh") +
  scale_x_date(breaks=breaks_width("6 month"),
               labels=date_format("%b %y"))+
  theme(axis.text.x=element_text(angle=50, vjust=.7))+
  xlab(element_blank())+
  scale_y_continuous(breaks = seq(0,6000,500)) +
  theme_bw()


# salvataggio dati
dates_kmeans_2 <- temp_2$data
dates_kmeans_5 <- temp_5$data
dates_kmeans_10 <- temp_10$data

# Comparison ----

"Si paragonano i risultati ottenuti in termini di anomalie a blocchi: si crea un blocco per
ogni valore x% (x=2,5,10) e si inseriscono nei blocchi corrispondenti i risultati ottenuti 
dai metodi che si basano su valori relativi al top x% di una distribuzione, inserendo i risultati
non influenzati da tali percentuali (metodi di decomposizione stagionale e isolation forest)
in modo identico in ogni blocco.
Si vuole calcolare qual è la percentuale di metodi che riconoscono una certa data come anomala."

# top 2%
date_vec2 <- c(dates_anomalize,dates_sarima_2,dates_sarima_test_2,dates_cart,dates_kmeans_2)
table_dates2 <- table(date_vec2)/5
length(which(table_dates2>=0.4))
table_dates2

# top 5%
date_vec5 <- c(dates_anomalize,dates_sarima_5,dates_sarima_test_5,dates_cart,dates_kmeans_5)
table_dates5 <- table(date_vec5)/5
length(which(table_dates5>=0.4))
table_dates5

# top 10%
date_vec10 <- c(dates_anomalize,dates_sarima_10,dates_sarima_test_10,dates_cart,dates_kmeans_10)
table_dates10 <- table(date_vec10)/5
length(which(table_dates10>=0.4))
table_dates10

## Grafici finali

# top 2%, seleziono come anomalie quelle riconosciute almeno dal 40% del metodi
datatable2 <- as.data.table(table_dates2)
datatable2 <- datatable2[which(datatable2$N>=0.4),]

ggplot(datatable2, aes(x=N*100, y= date_vec2, fill=N*100)) +
  geom_histogram(stat="identity") +
  xlab("Frequency (%)") + 
  ylab("Dates") +
  labs(fill = "Outliers in methods (%)") +
  scale_fill_viridis(limits = c(30, 100), direction=-1) +
  theme_bw()

# 5%, seleziono come anomalie quelle riconosciute almeno dal 50% dei metodi
datatable5 <- as.data.table(table_dates5)
datatable5 <- datatable5[which(datatable5$N>=0.5),]

ggplot(datatable5, aes(x=N*100, y= date_vec5, fill=N*100)) +
  geom_histogram(stat="identity") +
  xlab("Frequency (%)") + 
  ylab("Dates") +
  labs(fill = "Outliers in methods (%)") +
  scale_fill_viridis(limits = c(30, 100), direction=-1)+
  theme_bw()

# 10%, seleziono come anomalie quelle riconosciute almeno dal 50% dei metodi
datatable10 <- as.data.table(table_dates10)
datatable10 <- datatable10[which(datatable10$N>=0.5),]

ggplot(datatable10, aes(x=N*100, y= date_vec10, fill=N*100)) +
  geom_histogram(stat="identity") +
  xlab("Frequency (%)") + 
  ylab("Dates") +
  labs(fill = "Outliers in methods (%)") +
  scale_fill_viridis(limits = c(30, 100), direction=-1)+
  theme_bw()


# Comparison U1-U6 ----

# si fa un confronto sul blocco top 5%, definito come spiegato in precedenza
# si valutano le anomalie definite da almeno il 40% dei metodi
table_dates5_u6 <- table_dates5
table_u6 <- as.data.table(table_dates5_u6)
table_u6 <- table_u6[which(table_u6$N>=0.4),]
table_u1 <- as.data.table(table_dates5_u1)
table_u1 <- table_u1[which(table_u1$N>=0.4),]

date_tot <- c(table_u1$date_vec5,table_u6$date_vec5)
table_dates_tot <- as.data.table(table(date_tot)/2)

# giorni anomali in comune
table_dates_tot[which(table_dates_tot$N==1.0),]

