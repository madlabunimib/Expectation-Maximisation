# MADlab - EM HARD

Questo script implementa tutte le funzioni necessarie per l'esecuzione dell'algoritmo EM HARD su dataset incompleti. 

## Introduzione
EM HARD (o EM HARD ASSIGNMENT) è una variante dell'algoritmo EM SOFT. La differenza è presente nella computazione delle expected sufficient statistics. EM HARD seleziona, per ogni istanza o[m] il singolo assegnamento h[m] che massimizza 𝑃(ℎ|𝑜[𝑚],𝜃^𝑡). Il passo di expectation è analogo a EM SOFT

## Parametri in input
EM HARD richiede i seguenti parametri in input:
* **initial_data** - *OBBLIGATORIO* - Si tratta di un dataframe dove è presente almeno un missing value. Si consiglia di passare in input all'interno del metodo dataframe che hanno come intestazioni delle righe il nome delle variabili; mentre, come intestazione delle colonne, i dati. Si consiglia inoltre di passare in input all'algoritmo dataframe numerici. In caso di valori nominali, si consiglia di convertire i valori in numerico
* **sctructure** - *OBBLIGATORIO* - La struttura della rete bayesiana. Si prega di definire la struttura della rete attraverso model2network di bnlearn
* **cpt** - *OBBLIGATORIO* - Si tratta dell'inizializzazione iniziale date alle CPT della rete. L'inizializzazione può essere uniforme, randomica etc.
* **ALPHA** - *FACOLTATIVO* - iperparametro che sottende lo stopping criteria. Se la differenza in valore assoluto tra le cpt calcolate all'iterazione corrente con le cpt calcolate all'iterazione precedente è minore di questo valore, l'algoritmo si arresta e termina la sua computazione. ALPHA deve essere definito positivo. Default ALPHA = 0.05
* **NUMBER_ITERACTION** - *FACOLTATIVO* - iperparametro che indica il massimo numero di iterazioni che devono essere effettuate se l'algoritmo non si è arrestato attraverso la politica di stopping criteria. Default NUMBER_ITERACTION = 10
