# Estrazione delle correlazioni

Procedura completa: [guida unica](../../MONTHLY_WORKFLOW.md), passaggi 5–6.

Ogni articolo ha uno script leggibile in `articles/`, con eventuali chiavi in `keys/`.
`templates/article_extraction.R` definisce le sezioni scientifiche comuni.
Gli script storici in `data/` restano conservati.

Da RStudio: `source("workflow.R")`, poi `estrai_articolo("0150")`.
`verifica_soltanto = TRUE` controlla senza salvare.
Gli originali e le loro date sono nel foglio fonti della codifica generale.

Gli export correnti sono dati individuali privati, matrice nella cartella comune,
N per coppia in `data/3.meta_data/pair_n/` e controlli aggregati in `extraction/summary.csv`.
Non servono una seconda copia della matrice, un manifest dei file derivati o un'integrazione mensile separata.
L'aggiornamento finale usa `anteprima_risultati()` e `aggiorna_risultati()`.

0145: Study 2, N=973, scoring delle faccette distinto, medie BESSI 1–5, N per coppia 790–973.
0150: N=30719 dopo 596 esclusioni per sovrapposizione con 0053d; N per coppia 29244–30719.
Le decisioni scientifiche restano in `config/extraction/<paper_id>_decisions.csv` e nel codice.

## Verifica storica (branch di audit)

`articles/0013.R` e `0018.R` verificano i dati scientifici senza modificarli:
un'esecuzione normale salva soltanto il riepilogo aggregato unico
`data/3.meta_data/extraction/historical_audit.csv`. `0120.R` esporta invece la
correzione BESSI approvata, con la stessa procedura compatta degli articoli nuovi:
matrici nella cartella comune, N per coppia, dati individuali privati e controlli
aggregati. Per tutti e tre, `--check-only` non salva nulla. Esempio dalla radice:

```sh
Rscript scripts/extraction/articles/0018.R
Rscript scripts/extraction/articles/0120.R --check-only
```

Le alternative non approvate restano in memoria. Gli originali sono
controllati tramite checksum; la loro data di acquisizione non viene inventata.
`keys/bessi192.R` contiene soltanto la chiave ufficiale e i pesi del BESSI-192.
In 0120 il riepilogo distingue riproduzione dei punteggi forniti dagli autori e
confronti separati su pesi, risposta fuori scala e ricostruzione BFI-2.
Le tre verifiche non costituiscono ancora una ricodifica
completa di tutte le misure degli articoli.

Per 0120, l'archivio OSF completo fornito dall'utente il 13 novembre 2026 è
conservato, insieme agli otto file estratti, in
`data/3.meta_data/open_data/raw/0120/osf_7efmp/` (locale, escluso da Git).
I tre SAV sono identici agli input storici. La sintassi condivisa descrive
modelli fattoriali e differenze self/informant, ma non il calcolo della tabella 8
o dei compositi osservati: non risolve ancora le discrepanze. Gli script degli
autori sono stati esaminati senza eseguirli; il checksum dell'archivio è nel
codice di audit. La data del download originario resta sconosciuta.

La verifica 0120 ricostruisce anche le 15 faccette e i cinque domini BFI-2 dalla
chiave ufficiale, conservata come costanti con i riferimenti delle tabelle 8–9
in `keys/0120.R`. Nove punteggi di faccetta, in sei faccette e nove partecipanti,
differiscono: nei casi interessati il punteggio fornito coincide con la media
degli altri tre item, mentre il quarto è frazionario nel file condiviso.
Ciò è compatibile con un diverso ordine tra scoring e imputazione, non prova
un errore nelle inversioni. I valori frazionari restano conservati nella
ricostruzione. Anche così, 440 coefficienti su 480 della tabella 9 non coincidono
entro l'arrotondamento. Il confronto è nel riepilogo già esistente; questi
punteggi BFI-2 alternativi non sostituiscono quelli forniti. SWLS e RSES non
sono ancora ricostruite.

Decisione del 13 novembre 2026: rimandare la discrepanza con le tabelle 8–9.
**Da fare:** contattare gli autori
per segnalare che le correlazioni ricalcolate dai dati condivisi non riproducono
quelle pubblicate e chiedere codice e versione dei dati usati per le tabelle.
Il contatto non è ancora stato effettuato.

Decisioni successive approvate il 13 novembre 2026: usare i pesi ufficiali BESSI
in entrambi i campioni 0120 e trattare il valore 32 di BESSI171 come mancante,
calcolando l'ottimismo sugli altri cinque item. Queste correzioni sono integrate
nelle matrici e negli output analitici, con N invariati (1103 e 123).
Il confronto iniziale (175 e 339 correlazioni modificate) resta nel riepilogo
come `approved_integration_2026_11_13`; `approved_source_correction` confronta
invece il calcolo con le matrici attualmente salvate. Da RStudio:
`source("workflow.R")`, `estrai_articolo("0120")`, `anteprima_risultati()`, poi
`aggiorna_risultati(motivo = "...")` se sono presenti nuove variazioni numeriche.
I punteggi BFI-2 forniti restano conservati. Le due revisioni sono registrate
anche nel ledger storico già esistente per la compatibilità con il builder
manuale; gli script storici non sono modificati.

0013: le correlazioni esistenti usano già tutte le osservazioni disponibili per
coppia, con N 573–840. I 897 record del file non sono l'N di nessuna correlazione.
N di riferimento 840 è mantenuto nello storico e nella review. Dopo la decisione
successiva sul campione condiviso, 0013 non entra più nei metadata attivi: il
rappresentante quantitativo è 0048. La discrepanza storica sul campione resta
documentata, senza selezioni arbitrarie dei record o modifiche alla matrice 0013a.

## Secondo gruppo storico: 0026, 0030 e 0034

La prima verifica ha riprodotto le quattro matrici esistenti: 4304 correlazioni.
0026 e 0030 restano verifiche in sola lettura degli output scientifici, che
salvano solo il riepilogo unico. Dopo le decisioni del 13 novembre, 0034 esporta
il mapping SNS corretto e i domini BESSI con i pesi ufficiali, con la stessa
procedura compatta di 0120. Per tutti, `--check-only` non scrive nulla.
Gli script degli autori sono esaminati, non eseguiti.

| Matrice | N di riferimento | N per coppia | Esito |
|---|---:|---:|---|
| 0026a | 2992 | 2978–2992 | Domini BESSI-45 verificati come medie dei blocchi di nove item del file; differenze solo da arrotondamento dei punteggi forniti. |
| 0030a | 990 | 990 | Faccette ricostruite dagli item e domini con i pesi ufficiali. |
| 0030b | 824 | 787–824 | Domini verificati dalle faccette fornite; gli item originali di Study 2 non sono nel file. |
| 0034a | 303 | 301–303 | Faccette ricostruite dai sei item spagnoli selezionati; pesi ufficiali e mapping SNS corretti, con tutti i partecipanti conservati. |

**0030 — sovrapposizione:** il paper (p.10) dichiara 83 adulti ricontattati da
Study 1. Nei file rimasti dopo le esclusioni, 68 profili coincidono su tutte le
32 faccette e sul sesso, tutti nel gruppo adulti. Mancano identificativi comuni:
nessuna esclusione o modifica di N è applicata sulla sola corrispondenza dei
punteggi. **Decisione approvata:** conservare entrambi i campioni, tutte le
persone e gli N attuali (990 e 824), mantenendo la nota sulla dipendenza.

**0034 — correzioni approvate e integrate:** lo storico scambiava i mapping di confronto sociale
negativo e conseguenze addictive dei social. Le etichette SPSS e il contenuto
dei dieci item NC e otto item AC identificano i costrutti; il testo del paper
(p.5) riporta invece le numerosità degli item invertite. La sola correzione del
mapping modifica 136 correlazioni rispetto allo storico (scarto massimo circa 0.379).
I vecchi domini usavano nove faccette principali per self-management e pesi uguali per
le cinque faccette degli altri domini. I pesi ufficiali modificano 335
correlazioni rispetto allo storico (scarto massimo circa 0.084), mantenendo la regola originale che
richiede tutte le faccette costituenti. Le colonne WEIGHT degli autori confermano
lo scoring ufficiale sui record completi. Le due correzioni sono esportate
insieme: 461 correlazioni modificate, con N per coppia sempre 301–303.
Il confronto iniziale resta come `approved_integration_2026_11_13` nel riepilogo
esistente; le altre righe confrontano le alternative con la matrice corrente.
Usare `estrai_articolo("0034")`, non il vecchio blocco storico, per lo scoring
approvato. I dati individuali sono locali e privati, con età conservata e senza ID.

Nel file preparato 0034 ci sono otto età di 17 anni, a fronte del range 18–85
dichiarato; inoltre la versione precedente contiene 322 righe contro le 313
iniziali descritte nel paper. Per decisione esplicita si conserva il campione
preparato di 303 persone, inclusi tutti gli otto minorenni, senza nuove esclusioni,
registrando la discrepanza. Il retest resta
escluso. I totali BFI-2 riproducono le somme dei valori già presenti in entrambi
i file; la storia delle inversioni prima del rilascio non è verificabile.

## Terzo gruppo storico: 0035, 0043 e 0045

La verifica iniziale ha riprodotto tutte le 281 correlazioni storiche. Dopo
l'approvazione del 13 novembre, i tre script esportano le correzioni descritte
sotto: 40 correlazioni aggiunte e otto modificate. N e partecipanti restano
invariati; `--check-only` non scrive nulla. I vecchi script sono conservati.

| Matrice | N di riferimento | N per coppia attuale | Verifica e proposta |
|---|---:|---:|---|
| 0035a | 642 | 469–627 | Cinque faccette BESSI e cinque scale BFI-44 ricostruite dagli item; sesso incluso con 11 nuove correlazioni. |
| 0043a | 975 | 627–910 | Corretto il nome `ParentsEducation` in `parentEducation`: 20 correlazioni recuperate. |
| 0045a | 248 | 236–242 | Inclusa `capacityforconsistency` (nove correlazioni) e usate le ore totali continue di volontariato (otto correlazioni esistenti modificate). |

0035: i dati includono 468 donne, 159 uomini e 15 valori mancanti per sesso.
Le faccette BESSI sono disponibili per 480–482 persone: il numero 642 descrive
il campione del paper, non l'N di ogni correlazione. La proposta sul sesso
ha portato gli N per coppia a 469–627; i punteggi e le correlazioni esistenti restano
immutati. Le cinque misure SEB sono faccette rappresentative, non domini completi.

0043: il file è già preparato dopo esclusioni e imputazione; non contiene gli
item originali. Le misure di tratto sono le versioni con istruzioni di frequenza
degli stessi item BESSI-20, non BFI-2. `ParentsEducation` ha 835 valori validi e
140 mancanti; l'aggiunta approvata conserva tutte le persone e i punteggi forniti.

0045: la faccetta omessa è documentata nel paper (Tabella 3 e appendice item).
Il vecchio `volunteering` usa `hour_group`, cioè tre livelli ordinati: nessuna
ora, alcune ore, raggiungimento del benchmark (40 ore estive, 20 autunnali).
Sono disponibili anche le ore totali continue. È registrato separatamente il
confronto con queste ultime: otto correlazioni storiche cambiano, con scarto
massimo circa 0.098. Per l'estrazione approvata si usano le ore totali. La Tabella 3
pubblica correlazioni latenti; non è un riferimento di arrotondamento per le
nostre correlazioni tra medie osservate. Si conserva la regola storica delle
medie sugli item disponibili, con faccette interamente mancanti lasciate mancanti;
nessuna soglia nuova o esclusione applicata.

Le tre revisioni sono integrate in metadata e review tramite il workbook
generale e `workflow.R`. Il primo confronto è conservato come
`approved_integration_2026_11_13`; `approved_extraction` confronta lo scoring
approvato con le matrici correnti. Per ricalcolare usare `estrai_articolo("0035")`,
`estrai_articolo("0043")` e `estrai_articolo("0045")`, poi anteprima e aggiornamento.

Conteggio dopo questo gruppo: 10 articoli storici e 13 matrici passati in verifica;
con 0145 e 0150 già codificati/verificati, 12 articoli e 15 matrici su 31 e 39
attivi nella metaanalisi. Restano 19 articoli e 24 matrici. I chiarimenti o le
proposte ancora aperti sono distinti dalla riproduzione numerica delle matrici.

## Quarto gruppo storico: 0048, 0049 e 0050

La verifica iniziale ha riprodotto tutte le 3629 correlazioni di quattro matrici.
Dopo le decisioni del 13 novembre, i tre script esportano le estrazioni approvate,
i dati individuali privati, gli N per coppia e i confronti nel riepilogo unico.
`--check-only` non scrive nulla. Gli originali e gli script manuali sono conservati.

| Matrice | N di riferimento | N per coppia | Estrazione corrente |
|---|---:|---:|---|
| 0048a | 897 | 428–840 | Rappresentante quantitativo del campione condiviso con 0013. |
| 0049a | 137 | 128–136 | Recuperata `bf_emotionalvolatility`: 24 correlazioni aggiunte. |
| 0050a | 940 | 631–791 | Domini ponderati sulle faccette disponibili, senza perdita di copertura. |
| 0050b | 767 | 469–755 | Chiave rivista delle faccette e domini aggiornati; tutte le 768 righe conservate. |

**0013/0048:** tutti i 897 identificativi Case, i 20 punteggi con nomi identici
e la loro mancanza di dati coincidono. Uniformata la direzione di neuroticismo,
tutte le 210 correlazioni comuni coincidono. Anche 0013 (metodi, p.3) dichiara
di rianalizzare gli stessi dati. Decisione approvata: **0013 solo nella review**,
0048 rappresentante quantitativo con N=897 dichiarato dagli autori e Ns per
coppia in nota. La matrice 0013a rimane nella cartella comune come storico
inattivo, registrato in `config/coding/matrix_exclusions.csv`. Il contratto
numerico continua a proteggere tutte le matrici attive. I dati 0048 contengono
504 donne, 387 uomini e sei mancanti, con percentuali diverse da quelle del
paper: si mantengono i valori della fonte, senza ricodificarli dalle percentuali.

**0049:** lo script calcolava la volatilità emotiva da BFI14_R invertito e BFI29,
ma la chiamava `emotionalvolatility`. La selezione del codebook accetta
`bf_emotionalvolatility`: il nome errato scartava la variabile già calcolata.
La correzione approvata aggiunge 24 correlazioni senza cambiare le 276 precedenti.
Nove coefficienti osservati di riferimento nelle Tabelle 3–5 coincidono entro
l'arrotondamento, inclusi quelli della faccetta omessa. I tre compositi SEB sono
medie di due faccette ciascuno; le performance osservate rimangono distinte.
L'età è stata rimossa dalla fonte per anonimizzazione. Gli altri item BFI e le
autovalutazioni della performance restano per l'ampliamento futuro.

**0050 — originali:** l'archivio completo fornito dall'utente è conservato in
`data/3.meta_data/open_data/raw/0050/german/german.zip`, con 89 file di origine
estratti in `files/german/`. Le cache IDE rimangono nell'archivio senza copie
estratte. Tutto è locale e Git-ignorato. L'Rdata dell'archivio è identico al
file storico. Ricezione: 13 novembre 2026; download originario sconosciuto.
Gli script degli autori sono ispezionati senza eseguirli integralmente.

**0050 — faccette:** tutti i punteggi T4 sono ricostruibili. Le otto differenze
T2 sono spiegate da un errore nello script originale: dopo aver calcolato le
medie `_prm` con la chiave rivista v0.2, la successiva assegnazione con
`scale_builder(data = adults_teens_t2, suffix = "")` riparte dall'oggetto
precedente, ereditando le vecchie medie `_prm`. I punteggi forniti coincidono
esattamente con la chiave iniziale, mentre gli item finali sono quelli rivisti.
L'estrazione corrente ricostruisce la v0.2 dagli item originali non rinominati:
medie degli item disponibili, tutto mancante resta mancante, senza imputazione.
Corrette otto faccette (2954 valori; scarto massimo 1.5). Le 20 scale BFI
riproducono le medie dei valori già ricodificati; non si invertono nuovamente.
I totali cognitivi coincidono nei 607 casi ammessi: i 120 casi con diverso
protocollo temporale restano esclusi dai punteggi cognitivi, non dal dataset.

**0050 — domini e mancanza pianificata:** si mantiene la copertura delle
faccette principali usata dagli autori (nove per self-management, quattro per
gli altri domini). Si aggiungono le faccette interstiziali disponibili con peso
0.5 e si divide per la somma dei pesi osservati. Un dominio non viene dedotto
dalle sole faccette interstiziali. Con tutti i costituenti presenti i pesi sono
quelli ufficiali; sui dati incompleti questa è una convenzione esplicita di
riproporzionamento. La precedente alternativa che richiedeva tutti i costituenti
rimane solo come confronto: avrebbe ridotto l'N minimo di 0050a a 482.
La soluzione esportata mantiene esattamente la mancanza di dati e tutti gli N
per coppia precedenti, modificando 175 e 702 correlazioni. La sola correzione
della chiave T2, senza nuovi domini, ne modifica 605.

**0050 — campioni e tempi:** il campo `new_id` del file rilasciato, documentato
da MatchedIDs.xlsx, collega 238 partecipanti T4 a Study 3; sono presenti 203
record appaiati per il retest. Per decisione esplicita si conservano entrambe
le matrici e tutte le persone, con nota sulla dipendenza. Si conserva N=767
dichiarato per 0050b, distinguendolo dalle 768 righe rilasciate (766 con almeno
due punteggi selezionati). L'export registra `input_rows` separatamente da N.
Si includono le associazioni con BESSI T2, cognizione T1 e BFI suddiviso T0/T2:
codifica `cross`, con rilevazioni sfalsate; nessun retest è contato come nuova
matrice indipendente. Le nostre correlazioni restano Pearson pairwise tra
punteggi osservati, non i coefficienti PIM/FIML degli autori.

La codifica generale, metadata, review e registro delle ricerche sono aggiornati
tramite `workflow.R`. Le revisioni numeriche esatte sono nel ledger esistente;
il primo confronto approvato resta come `approved_integration_2026_11_13`.
Il dataset attivo ha 30 articoli quantitativi e 38 matrici, con 51 articoli nella
review. Dei quantitativi attivi ne sono stati esaminati 14 (18 matrici);
restano 16 articoli e 20 matrici. Anche 0013 è stato esaminato, ma non è più
conteggiato fra gli articoli quantitativi attivi.
