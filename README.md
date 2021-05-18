# DSlab

IDEE:

* Previsione riapertura anno accademico: aggregazione dati per mese (usare kwh) per prevedere consumi ad ottobre 2021 a seconda della situazione sanitaria. Per farlo si può aggiungere variabile categorica che indica quanti possono andare in uni (tutti, solo matricole, solo prof e ricercatori, nessuno);
* Confronto u1 e u6 nel 2020: come nel periodo covid sono cambiati i consumi di u1 e u6 (u1 ha laboratori e quindi ci aspettiamo che consumi di più anche con covid);
* Previsione consumi marzo-aprile 2020 senza covid e confrontare con come è andata realmente;
* Individuazione di anomalie - integrazione con dati meteo ecc;
* Come cambierebbero i consumi se togliessero l'ora legale?


IDEA scelta al momento: Come cambierebbero i consumi se togliessero l'ora legale? 
Vogliamo vedere se mantenere solo l'ora legale (summer time), come era stato proposto a livello europeo, possa comportare un risparmio in termini di consumi di energia elettrica oppure se la situazione sia migliore mantenendo le differenziazioni in winter time e summer time (teoricamente, le tesi che hanno portato al mantenimento di tale divisione sono proprio rispetto al fatto che così si dovrebbe avere un certo risparmio energetico, anche se pare che tale risparmio non sia poi così significativo).

Processo:
* utilizziamo i dati del 2018 e 2019 come training set
* consideriamo gennaio 2020 come mese target: vogliamo prevedere quali sarebbero i consumi a gennaio se ci fosse summer time invece che winter time
* costruiamo regressore luce/buio (1: ora di luce, 0: ora di buio) da attribuire ai diversi orari delle diverse rilevazioni, considerando una funzione che tenga conto di quanto avvengono alba e tramonto nelle diverse giornate
* costruiamo regressore orario lavorativo/apertura università (1: università aperta, 0: università chiusa) e lo attribuiamo ai diversi orari. A gennaio, consideriamo l'orario lavorativo shiftato di un'ora (come se ci fosse summer time)
* valutiamo se i consumi effettivamente siano ridotti oppure no, ed eventualemnte quale sia l'entità del risparmio


CALL CON FATTORE:
* potential outcome e controfactual analysis ("simulo mondo che non c'è stato"
* qualche scenario alternativo rispetto alla previsione (cambieranno anche altre abitudini delle persone sui cui però non abbiamo i dati), confronto robusto (non con troppa precisione sulle misure)


