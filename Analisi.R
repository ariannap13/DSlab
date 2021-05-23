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

rm(list=ls())
sunset = function(date) {
  a = getSunlightTimes(date = date, lat = 45.520068121587855, lon = 9.211679574089654, tz = "CET")
  h = hour(a$sunset)
  m = minute(a$sunset)
  s = second(a$sunset)
  time = as.numeric(paste0(h,m,s))
  return(time)
}
sunrise = function(date) {
  a = getSunlightTimes(date = date, lat = 45.520068121587855, lon = 9.211679574089654, tz = "CET")
  h = hour(a$sunrise)
  m = minute(a$sunset)
  s = second(a$sunset)
  time = as.numeric(paste0(h,m,s))
  return(time)
}

# Import data----


setwd("C:/Users/franc/Google Drive/Uni/Data Science/Data Science Lab/Dati Energia (2)/")
u1 = read.csv('u1.csv')
u6 = read.csv('u6.csv')

# Pre - processing ----
data_u1 = data.frame(data = as.Date(substr(u1$DATA, 1, 10), tryFormats =  "%Y%m%d"))
data_u1$KWh = as.numeric(gsub(',','.',u1$CONSUMO_ATTIVA_PRELEVATA))*.25
data_u1$ora = u1$ORA
data_u1$luce = ifelse(((data_u1$ora >= sunrise(data_u1$data)) &
                         (data_u1$ora <= sunset(data_u1$data))),1,0)


data_u6 = data.frame(data = as.Date(substr(u6$DATA, 1, 10), tryFormats =  "%Y%m%d"))
data_u6$KWh = as.numeric(gsub(',','.',u6$CONSUMO_ATTIVA_PRELEVATA))*.25
data_u6$ora = u6$ORA
data_u6$luce = ifelse(((data_u6$ora >= sunrise(data_u6$data)) &
                         (data_u6$ora <= sunset(data_u6$data))),1,0)

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



# Aggregazione ----
# by day
data_u1_day = data_u1 %>%
  group_by(data) %>%
  summarise(KWh = sum(KWh))

data_u6_day = data_u6 %>%
  group_by(data) %>%
  summarise(KWh = sum(KWh))

# by month
data_u1_month = data_u1_day %>%
  group_by(data=floor_date(data, "month")) %>%
  summarise(KWh = sum(KWh))

data_u6_month = data_u6_day %>%
  group_by(data=floor_date(data, "month")) %>%
  summarise(KWh = sum(KWh))

# by week
data_u1_week = data_u1_day %>%
  group_by(data=floor_date(data, "week")) %>%
  summarise(KWh = sum(KWh))

data_u6_week = data_u6_day %>%
  group_by(data=floor_date(data, "week")) %>%
  summarise(KWh = sum(KWh))

# by year
data_u1_year = data_u1_day %>%
  group_by(data=floor_date(data, "year")) %>%
  summarise(KWh = sum(KWh),
            edif = 'u1')

data_u6_year = data_u6_day %>%
  group_by(data=floor_date(data, "year")) %>%
  summarise(KWh = sum(KWh),
            edif = 'u6')

data_year = rbind(data_u1_year, data_u6_year)
# Analisi Esplorativa----

a = rep(data_u1$ora[which(data_u1$data == '2018-01-11')],2)
plot(y = data_u1$KWh[which(data_u1$data >= '2018-01-11' & data_u1$data <= '2018-01-12')],
     x = a
     ,type = 'l')
data = data.frame(a = a, kWh = data_u1$KWh[which(data_u1$data >= '2018-01-11' & data_u1$data <= '2018-01-12')])
data1 = data.frame(y = data_u1$KWh[which(data_u1$data == '2018-01-11')],
                   x = data_u1$ora[which(data_u1$data == '2018-01-11')])
data2 = data.frame(y = data_u1$KWh[which(data_u1$data == '2018-01-12')],
                   x = data_u1$ora[which(data_u1$data == '2018-01-12')])

ggplot(data = data_u1[which(data_u1$data >= '2018-01-11' & data_u1$data <= '2018-01-14'),],
       aes(y = KWh, x = ora, group = data,color = data))+
  geom_line()


par(mfrow = c(2,1))
plot(data_u1_day$data,data_u1_day$KWh, type = 'l', ylab = 'U1', xlab = 'Data')
plot(data_u6_day$data,data_u6_day$KWh, type = 'l', ylab = 'U1', xlab = 'Data')
par(mfrow = c(1,1))

# dati settimanali
g_sett = ggplot()+
  geom_line(data = data_u1_week, aes(y = KWh, x = data), col = 'red')+
  geom_line(data = data_u6_week, aes(y = KWh, x = data), col = 'blue')+
  labs(y='kWh Consumati', x = element_blank(), title = 'Confronto consumi energetici tra edifici',
       subtitle = "In rosso l'U1 e in blue l'U6 (aggregazione per settimana)")+
  theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5),
        axis.ticks=element_blank())+
  scale_x_date(breaks=breaks_width("6 month"),
               labels=date_format("%b %y"))+
  theme_bw()

# dati mensili
g_mens = ggplot()+
  geom_line(data = data_u1_month, aes(y = KWh, x = data), col = 'red')+
  geom_line(data = data_u6_month, aes(y = KWh, x = data), col = 'blue')+
  labs(y='kWh Consumati', x = element_blank(), title = 'Confronto consumi energetici tra edifici',
       subtitle = "In rosso l'U1 e in blue l'U6 (aggregazione per mese)")+
  theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5),
        axis.ticks=element_blank())+
  scale_x_date(breaks=breaks_width("6 month"),
               labels=date_format("%b %y"))+
  theme_bw()

grid.arrange(g_sett, g_mens, ncol = 1)


ggplot(data = data_year, aes(x = data, y = KWh, fill = edif))+
  geom_bar(stat = 'identity', position = 'dodge', color = 'black')+
  theme_bw()+
  labs(title= 'Confronto annuale dei consumi fra edifici', 
       y = 'kWh Consumati', x = '')+
  scale_fill_discrete(name = "Edificio", labels = c("U1", "U6"))

# Data Modeling ----

serie = ts(data_u1$KWh)
m1 = auto.arima(serie)
m2 = auto.arima(data_u1$KWh)
summary(m1)
summary(m2)

par(mfrow = c(2,1))
plot(serie)
plot(data_u1$KWh, type = 'l')
par(mfrow = c(1,1))

mod = auto.arima(serie, trace = T, ic = 'bic',stepwise=FALSE, approximation=FALSE,
           nmodels = 1000)
summary(mod)
forecast(mod)

coeftest(mod)
auto.arima(data_u1_month$KWh, ic = 'bic',stepwise=FALSE, approximation=FALSE,
           nmodels = 1000,
           d = 5,
           D = 5,
           max.p = 10,
           max.q = 10,
           max.P = 10,
           max.Q = 10,
           max.order = 30,
           max.d = 10,
           max.D = 10,
           trace = T)

mod = Arima(data_u1_month$KWh, order = c(10,5,0))
summary(mod)
coeftest(mod)

pacf(mod$residuals)



prev = forecast(mod)
summary(prev)
plot(prev)



Box.test(prev$residuals) # rigetto l'autocorrelazione secondo il Box-Pierce test

checkresiduals(mod)


prev = forecast(mod, 12) %>%
  as.data.frame() %>%
  mutate(data=seq.Date(from = as.Date("2021-01-01"),
                       length.out = 12 , by = "month"))



ggplot(data = data_u1_month, aes(x = data))+
  geom_line(data = data_u1_month, aes(y = KWh)) +
  # geom_smooth(data = data_u1, aes(y = consumo_giornaliero_KWh))+
  geom_line(data = prev, aes(x = data, y = `Point Forecast`), color = 'red')+
  geom_ribbon(data = prev, aes(ymin = `Lo 95`, ymax = `Hi 95`, x = data), alpha = 0.1, fill = "red") +
  geom_ribbon(data = prev, aes(ymin = `Lo 80`, ymax = `Hi 80`, x = data), alpha = 0.4, fill = "red") +
  theme_pubclean()
