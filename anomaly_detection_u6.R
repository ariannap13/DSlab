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
library(seastests)
library(EnvStats)
library(isotree)
library(stats)
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

## colonna ora_bis, trasformando in formato orario più comprensibile
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
       y = "KWh",
       color = "Legend") +
  scale_color_manual(values = colors) +
  scale_x_date(breaks = date_breaks("months"),
                               labels = date_format("%b"))


## filtering per togliere da giugno 2020
data_u6_day <- filter(data_u6_day, data < as.Date("2020-06-01"))



## studio della stagionalità

u6_ts <- ts(data_u6_day$KWh, frequency=365, start=c(2018,1,1))
stl <- decompose(u6_ts)
#seasonal(stl)
plot(stl)

# modello con stagionalità annuale
mod_tbats <- tbats(u6_ts)
mod_tbats #AIC = 16446.53

# modello con stagionalità multipla - settimana, mese, anno
u6_msts <- msts(data_u6_day$KWh, seasonal.periods=c(7,30,365))
u6_msts %>% mstl() %>%
  autoplot() 
mod_tbats_ms <- tbats(u6_msts)
mod_tbats_ms #AIC = 15822.21

# modello con stagionalità multipla - settimana, anno
u6_msts1 <- msts(data_u6_day$KWh, seasonal.periods=c(7,365))
u6_msts1 %>% mstl() %>%
  autoplot() 
mod_tbats_ms1 <- tbats(u6_msts1)
mod_tbats_ms1 #AIC = 15808.14

# modello con stagionalità multipla - mese, anno
u6_msts2 <- msts(data_u6_day$KWh, seasonal.periods=c(30,365))
u6_msts2 %>% mstl() %>%
  autoplot() 
mod_tbats_ms2 <- tbats(u6_msts2)
mod_tbats_ms2 #AIC = 16415.36

# modello con stagionalità multipla - settimana, mese
u6_msts3 <- msts(data_u6_day$KWh, seasonal.periods=c(7,30))
u6_msts3 %>% mstl() %>%
  autoplot() 
mod_tbats_ms3 <- tbats(u6_msts3)
mod_tbats_ms3 #AIC = 15809.85


# modello con stagionalità singola - settimana
u6_ts_w <- ts(data_u6_day$KWh, frequency=7, start=c(2018,1,1))
mod_tbats_ts_w <- tbats(u6_ts_w)
mod_tbats_ts_w # settimana AIC = 15797.05
mod_tbats # anno AIC = 16446.53

# modello con stagionalità singola - mese
u6_ts_m <- ts(data_u6_day$KWh, frequency=30, start=c(2018,1,1))
mod_tbats_ts_m <- tbats(u6_ts_m)
mod_tbats_ts_m #AIC = 16440.06

" Dai risultati osservati, sembrerebbe esserci una stagionalità settimanale.
  Si sceglie di realizzare il fitting di un modello arima che deduca in automatico
  la componente stagionale."

# modello arima

model_arima <- auto.arima(u6_ts_w)
model_arima #viene indicata la stagionalità settimanale (complessivamente, si ha un modello sarima)

## analisi esplorativa (studio dei residui + acf/pacf)

acf(resid(model_arima), lag.max = 1000)
pacf(resid(model_arima), lag.max = 1000)
# la situazione a livello di autocorrelazione non appare particolarmente problematica

checkresiduals(model_arima) # grande p-value per ljung-box test: i residui non possono essere considerati come diversi da una serie white noise
# i residui non sono esattamente normali ma la condizione di non normalità non è estrema

# Anomaly Detection ----

## APPROCCIO STL ----

" prova anomaly detection, dal sito https://www.analyticsvidhya.com/blog/2020/12/a-case-study-to-detect-anomalies-in-time-series-using-anomalize-package-in-r/

# il pacchetto anomalize nella funzione time_decompose accetta due metodi:
#   * STL --> lavora bene con trend a lungo termine, ma performa meno bene 
#             quando la componente stagionale è più forte del trend
#   * twitter --> differenza con stl nella rimozione del trend, che viene qui
#                 trattato rimuovendo la mediana dei dati invece che facendo il
#                 fitting di uno smoother. Lavora meglio quando la componente
#                 stagionale è preponderante."


data_u6_day <- as_tibble(data_u6_day)
class(data_u6_day)

# Nella funzione time_decompose, bisogna scegliere frequency = numero di osservazioni in un ciclo. 
# Nel nostro caso, modello migliore è con stagionalità multipla settimanale e annuale, 
# dovendo sceglierne solo una si sceglie la più importante --> settimanale

# metodo twitter --> dovrebbe essere il migliore per il nostro tipo di dati
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

table <- data_u6_day %>% 
  time_decompose(KWh, method="twitter", frequency="1 week") %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == 'Yes')

# metodo stl --> prova anche se sembra più adatto metodo twitter
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


# metodo twitter, cambio ampiezza intervallo di confidenza per identificare outliers

# cambiando alpha, ottengono più o meno outlier (alfa maggiore, si restringe l'intervallo di normalità attorno alla serie)
# se alpha ampio (quasi tutto è un outlier) posso giocare su percentuale max outliers da identificare nel dataset

# quasi tutto è outlier, gioco su max percentuale outliers nei dati
p4 <- data_u6_day %>%
  time_decompose(KWh, method="twitter", frequency="1 week") %>%
  anomalize(remainder, alpha = 0.3, max_anoms = 0.05) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("alpha = 0.3, max_anoms=5%")
p4


# con alpha=0.09, intervalli di normalità più stretti
p5 <- data_u6_day %>%
  time_decompose(KWh, method="twitter", frequency="1 week") %>%
  anomalize(remainder, alpha = 0.09, max_anoms = 0.3) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("alpha = 0.09, max_anoms=30%")
p5

# con alpha=0.05, intervalli di normalità più ampi
p6 <- data_u6_day %>%
  time_decompose(KWh, method="twitter", frequency="1 week") %>%
  anomalize(remainder, alpha = 0.05, max_anoms = 0.3) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("alpha = 0.05")
p6

# con alpha=0.05, intervalli di normalità più ampi
p61 <- data_u6_day %>%
  time_decompose(KWh, method="twitter", frequency="1 week") %>%
  anomalize(remainder, alpha = 0.05, max_anoms = 0.3, method="gesd") %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("alpha = 0.05")
p61

# tabella con date outliers (riferimento alpha=0.05)
table1 <- data_u6_day %>% 
  time_decompose(KWh, method="twitter", frequency="1 week") %>%
  anomalize(remainder, alpha = 0.05, max_anoms = 0.3, method="gesd") %>%
  time_recompose() %>%
  filter(anomaly == 'Yes')
# date identificate come giorni anomali, 26
dates_anomalize <- table1$data


## APPROCCIO SARIMA, stagionalità settimanale ----

model_arima

fitted <- model_arima$fitted

#create data frame with date and KWh for ets model
df <- data.frame(data = data_u6_day$data)
df$KWh <- fitted

# confronto
data_cf <- data.frame(data = data_u6_day$data)
data_cf$real <- data_u6_day$KWh
data_cf$fitted <- df$KWh

colors <- c("Fitted values" = "blue", "Real values" = "darkorange1")

ggplot() + 
  geom_line(data = data_u6_day, aes(x = data, y = KWh, color = "Real values")) +
  geom_line(data = df, aes(x = data, y = KWh, color = "Fitted values")) +
  labs(x = "date",
       y = "KWh",
       color = "Legend") +
  scale_color_manual(values = colors)

df_errors <- data.frame(data = data_u6_day$data)
df_errors$error <- model_arima$residuals

# voglio selezionare come ouliers solo il 2% dei dati

summary(abs(df_errors$error))
quantile(abs(df_errors$error), probs=seq(0,1,0.02))
# 98% corrisponde a 716.46

# definisco come outliers le osservazioni che hanno un errore associato maggiore di 716.46
table_sarima_2 <- data_u6_day[which(abs(as.numeric(df_errors$error)) > 716.46), c("data","KWh")]
dates_sarima_2 <- table_sarima_2$data


# voglio selezionare come ouliers solo il 5% dei dati

summary(abs(df_errors$error))
quantile(abs(df_errors$error), probs=seq(0,1,0.05))
# 95% corrisponde a 500.58

table_sarima_5 <- data_u6_day[which(abs(as.numeric(df_errors$error)) > 500.58), c("data","KWh")]
dates_sarima_5 <- table_sarima_5$data

prova <- data_u6_day[which(as.numeric(df_errors$error) > 500.58), c("data","KWh")]


# voglio selezionare come ouliers solo il 10% dei dati

summary(abs(df_errors$error))
quantile(abs(df_errors$error), probs=seq(0,1,0.1))
# 90% corrisponde a 292.57

table_sarima_10 <- data_u6_day[which(abs(as.numeric(df_errors$error)) > 292.57), c("data","KWh")]
dates_sarima_10 <- table_sarima_10$data

# metodo generalized extreme Studentized deviate test per identificare outliers
df <- data.frame(data = data_u6_day$data)
df$observed <- data_u6_day$KWh
df$fitted <- fitted
df$residuals <- model_arima$residuals

# il metodo è costruito considerando un upper bound per il numero di outliers che ci si aspetta (corrisponde a numero trovato nel metodo dei quantili)
# valore max = n. ottenuto con metodo quantili

#2%
gesd <- gesdTest(df$residuals, 18)
num_oss <- gesd$ix
data_u6_day[num_oss,]
# tabella con outliers
table_sarima_test_2 <- data_u6_day[num_oss,]
dates_sarima_test_2 <- table_sarima_test_2$data


#5%
gesd <- gesdTest(df$residuals, 45)
num_oss <- gesd$ix
data_u6_day[num_oss,]
# tabella con outliers
table_sarima_test_5 <- data_u6_day[num_oss,]
dates_sarima_test_5 <- table_sarima_test_5$data


#10%
gesd <- gesdTest(df$residuals, 89)
num_oss <- gesd$ix
data_u6_day[num_oss,]
# tabella con outliers
table_sarima_test_10 <- data_u6_day[num_oss,]
dates_sarima_test_10 <- table_sarima_test_10$data

## APPROCCIO CART ----

# algoritmo isolation forest (decision trees), non usa misure di distanza o densità 
# ma considera solo il fatto che le anomalie sono solitamente poche e abbastanza diverse 
# dagli altri dati

# modello 
iso <- isolation.forest(data_u6_day)

# predict outliers within dataset, soglia score outliers=0.6 (valori vicini a 1 sono outliers
# forti, vicini a 0.5 sono outliers nella media e vicino a 0 valori più normali/difficili da isolare)
data_u6_day$pred <- predict(iso, data_u6_day, type = "score")
data_u6_day$outlier <- as.factor(ifelse(data_u6_day$pred >= 0.6, "outlier", "normal"))
table(data_u6_day$outlier)

#plot
d <- ggplot(data_u6_day, aes(x = data, y = KWh)) + 
  geom_line(color="gray81")+
  geom_point(shape = 20, alpha = 0.5, aes(color=outlier)) +
  labs(x = "date", y = "KWh") +
  labs(alpha = "", colour="Legend")
d + scale_color_manual(values=c("gray81", "red3"))

# tabella e date outliers
table_out_cart <- data_u6_day[which(data_u6_day$outlier=="outlier"),]
dates_cart <- table_out_cart$data


## APPROCCIO K-MEANS ----


set.seed(123)

wth = vector()
sil = vector()
kmax = 15
for (k in 2:kmax) {
  km = kmeans(as.data.frame(data_u6_day)[,2], k)
  wth[k-1] = km$tot.withinss
  ss = silhouette(km$cluster, dist(as.data.frame(data_u6_day)[,2]))
  sil[k-1] = mean(ss[,3])
}
df = data.frame(k = 2:15, wth, sil)


el_plot = ggplot(data = df, aes(y = wth, x = k))+
  geom_line()+
  geom_point()+
  theme_classic()+
  scale_x_continuous(name = "Number of clusters K",
                     breaks = seq(2,15,1))+
  scale_y_continuous(name = "Total within-clusters sum of squares",
                     labels = comma,
                     breaks = seq(0,60000000,10000000))+
  labs(title = 'Optimal number of clusters', subtitle = 'Elbow method')

sil_plot = ggplot(data = df, aes(y = sil, x = k))+
  geom_line()+
  geom_point()+
  theme_classic()+
  scale_x_continuous(name = "Number of clusters K",
                     breaks = seq(2,15,1))+
  scale_y_continuous(name = "Average Silhouettes")+
  labs(title = 'Optimal number of clusters', subtitle = 'Silhouette method')

grid.arrange(el_plot,sil_plot, nrow =1) # 3 cluster

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


data_u6_day_center$dist = apply(data_u6_day_center[,c(2,6)], 1, dist)

temp = data_u6_day_center %>%
  arrange(desc(dist))

# definizione frazione di outliers sul totale da considerare

# 0.02
outlier_fraction = 0.02
temp_2 = temp[1:round(nrow(temp)*outlier_fraction),]
nrow(temp_2)

# 0.05
outlier_fraction = 0.05
temp_5 = temp[1:round(nrow(temp)*outlier_fraction),]
nrow(temp_5)

# 0.1
outlier_fraction = 0.1
temp_10 = temp[1:round(nrow(temp)*outlier_fraction),]
nrow(temp_10)

# esempio con temp 5
ggplot()+
  geom_line(data = data_u6_day, aes(x = as.Date(data), y = KWh), size = 0.7)+
  geom_point(data = temp_5, aes(x = as.Date(data), y = KWh), color = 'red')+
  theme_classic()+
  scale_x_date(breaks=breaks_width("6 month"),
               labels=date_format("%b %y"))+
  theme(axis.text.x=element_text(angle=50, vjust=.7))+
  labs(title = 'Anomaly detection', subtitle = 'k-means method')+
  xlab(element_blank())+
  scale_y_continuous(breaks = seq(0,3000,500))

dates_kmeans_2 <- temp_2$data
dates_kmeans_5 <- temp_5$data
dates_kmeans_10 <- temp_10$data

# Comparison ----

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


# grafico finale

# 2%, seleziono prima quelli con valore maggiore di 0.4
datatable2 <- as.data.table(table_dates2)
datatable2 <- datatable2[which(datatable2$N>=0.4),]

ggplot(datatable2, aes(x=N*100, y= date_vec2, fill=N*100)) +
  geom_histogram(stat="identity") +
  xlab("Frequency (%)") + 
  ylab("Dates") +
  labs(fill = "Outliers in methods (%)") +
  scale_fill_viridis(limits = c(30, 100), direction=-1)


# 5%, seleziono prima quelli con valore maggiore di 0.5, a scopo di miglior visualizzazione
datatable5 <- as.data.table(table_dates5)
datatable5 <- datatable5[which(datatable5$N>=0.5),]

ggplot(datatable5, aes(x=N*100, y= date_vec5, fill=N*100)) +
  geom_histogram(stat="identity") +
  xlab("Frequency (%)") + 
  ylab("Dates") +
  labs(fill = "Outliers in methods (%)") +
  scale_fill_viridis(limits = c(30, 100), direction=-1)


# 10%, seleziono prima quelli con valore maggiore di 0.5, a scopo di miglior visualizzazione
datatable10 <- as.data.table(table_dates10)
datatable10 <- datatable10[which(datatable10$N>=0.5),]

ggplot(datatable10, aes(x=N*100, y= date_vec10, fill=N*100)) +
  geom_histogram(stat="identity") +
  xlab("Frequency (%)") + 
  ylab("Dates") +
  labs(fill = "Outliers in methods (%)") +
  scale_fill_viridis(limits = c(30, 100), direction=-1)


# Comparison U1-U6 ----

#5%
table_dates5_u6 <- table_dates5
table_u6 <- as.data.table(table_dates5_u6)
table_u6 <- table_u6[which(table_u6$N>=0.4),]
table_u1 <- as.data.table(table_dates5_u1)
table_u1 <- table_u1[which(table_u1$N>=0.4),]

date_tot <- c(table_u1$date_vec5,table_u6$date_vec5)
table_dates_tot <- as.data.table(table(date_tot)/2)

# giorni in comune
table_dates_tot[which(table_dates_tot$N==1.0),]

