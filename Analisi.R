# Libraries----
library(tidyverse)
library(ggplot2)
library(lmtest)
library(forecast)
library(lubridate)
library(scales)
library(gridExtra)

# Import data----
rm(list=ls())

setwd("C:/Users/franc/Google Drive/Uni/Data Science/Data Science Lab/Dati Energia (2)/")
u1 = read.csv('u1.csv')
u6 = read.csv('u6.csv')

# Pre - processing ----
data_u1 = data.frame(data = as.Date(substr(u1$DATA, 1, 10), tryFormats =  "%Y%m%d"))
data_u1$KWh = as.numeric(gsub(',','.',u1$CONSUMO_ATTIVA_PRELEVATA))*.25

data_u6 = data.frame(data = as.Date(substr(u6$DATA, 1, 10), tryFormats =  "%Y%m%d"))
data_u6$KWh = as.numeric(gsub(',','.',u6$CONSUMO_ATTIVA_PRELEVATA))*.25

# Aggregazione by day
data_u1_day = data_u1 %>%
  group_by(data) %>%
  summarise(KWh = sum(KWh))

data_u6_day = data_u6 %>%
  group_by(data) %>%
  summarise(KWh = sum(KWh))


# Aggregazione by month
data_u1_week = data_u1_day %>%
  group_by(data=floor_date(data, "week")) %>%
  summarise(KWh = sum(KWh))

data_u6_week = data_u6_day %>%
  group_by(data=floor_date(data, "week")) %>%
  summarise(KWh = sum(KWh))

# Aggregazione by month
data_u1_month = data_u1_day %>%
  group_by(data=floor_date(data, "month")) %>%
  summarise(KWh = sum(KWh))

data_u6_month = data_u6_day %>%
  group_by(data=floor_date(data, "month")) %>%
  summarise(KWh = sum(KWh))

# Analisi Esplorativa----

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


# Data Modeling ----
mod = auto.arima(data_u1_month$KWh)
mod2 = ets(data_u1_month$KWh)
summary(mod)
coeftest(mod)

summary(mod2)

par(mfrow = c(1,2))
plot(data_u1_month$KWh, type = 'l')
plot(mod2$fitted)
par(mfrow = c(1,1))


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
