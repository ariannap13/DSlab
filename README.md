# DSlab

CALL CON FATTORE:
* potential outcome e controfactual analysis ("simulo mondo che non c'è stato"
* qualche scenario alternativo rispetto alla previsione (cambieranno anche altre abitudini delle persone sui cui però non abbiamo i dati), confronto robusto (non con troppa precisione sulle misure)


N.B.: codice in appendice piuttosto che in mezzo al testo (serve metterlo a quanto pare)


ANOMALY DETECTION:

* come aggregare i dati (provare per giorno, per settimana), aggregazione per mese eventualmente per detection anomalie covid
* capire la stagionalità (per giorno solo stagionalità annuale, per settimana va visto)
* capire come indicare la stagionalità nel caso di stagionalità singola con funzione ts (per stagionalità multipla si usa msts)
* applicare vari metodi (anomaly senza modello, tsoutlier (arima), ets, LSTM, CART, Prophet,...) --> nel caso di ETS, LSTM, CART, ... costruisco il modello e lo confronto con serie originale per vedere dove ci sono discostamenti importanti e quindi anomalie
