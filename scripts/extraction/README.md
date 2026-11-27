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

## Verifica storica conclusa e integrata

Stato al 27 novembre 2026: esaminati tutti i 30 articoli quantitativi attivi e
le 38 matrici; codifica ampliata e topics completati per i 51 articoli della
review. Le correzioni approvate sono integrate in `main`. La copia manuale
aggiornata resta locale e viene sincronizzata da `aggiorna_risultati()`.

Le sezioni successive conservano il registro cronologico dei gruppi di audit,
dei confronti e delle decisioni. I conteggi intermedi e le prove poi superate
descrivono quei checkpoint; per lo stato operativo corrente usa
`MONTHLY_WORKFLOW.md`, la codifica generale e le decisioni finali registrate.
Esaminare tutte le matrici non significa aver ricostruito ogni misura dagli
item originali: fonti disponibili, sovrapposizioni e discrepanze residue restano
esplicite nelle note. La richiesta agli autori di 0120 descritta sotto è ancora
da effettuare.

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
Queste prime tre verifiche non costituiscono una ricodifica completa di tutte
le misure degli articoli; i gruppi successivi documentano l'estensione dell'audit.

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
0030 resta una verifica in sola lettura degli output scientifici, che
salva solo il riepilogo unico. Dal18 novembre0026 esporta anche i due indicatori
di performance approvati, come descritto sotto. Dopo le decisioni del13 novembre,0034 esporta
il mapping SNS corretto e i domini BESSI con i pesi ufficiali, con la stessa
procedura compatta di 0120. Per tutti, `--check-only` non scrive nulla.
Gli script degli autori sono esaminati, non eseguiti.

| Matrice | N di riferimento | N per coppia | Esito |
|---|---:|---:|---|
| 0026a | 2992 | 2195–2992 | Domini BESSI-45 verificati; dal18 novembre31 correlazioni aggiunte per i due indicatori oggettivi. Vecchio blocco invariato. |
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

**Confronto successivo con la pubblicazione:** i risultati salvati dagli autori
coincidono entro l'arrotondamento con tutte le Tabelle 5–8 di Study 3. Per la
Tabella 8 è stata replicata la stessa analisi PIM (generatore originale, CFA con
MLR/FIML, sei item BESSI, tre parcelle cognitive, test entro il 6 febbraio 2021),
variando soltanto l'occasione BESSI. T0 riproduce 64/64 coefficienti entro
l'arrotondamento (tutti entro 0.0001 dai risultati salvati); T2 ne riproduce 12/64.
Tutti i 128 modelli convergono. I confronti sono nel riepilogo unico come
`published_table8_PIM_T0_replication` e `published_table8_PIM_T2_replication`.
Il codice IQ riassegna effettivamente l'input a T0, mentre il testo dichiara T2:
la discrepanza di occasione è distinta da quella fra PIM e Pearson. L'intenzione
degli autori va chiarita prima di considerare definitiva la scelta per queste
associazioni. Le matrici attive e il report non sono modificati da questo controllo.

**Replica estesa e struttura da discutere:** lo script dedicato
`0050_publication.R` riproduce le analisi delle Tabelle 5–9 senza esportare
matrici attive. Eseguire dalla radice:
`Rscript scripts/extraction/articles/0050_publication.R --check-only`.
Senza `--check-only` salva soltanto i riepiloghi nel registro unico. La funzione
PIM originale è letta dopo verifica del checksum; il resto dello script degli
autori non viene eseguito. Sono stati calcolati 704 modelli PIM convergenti e
35 coefficienti dal modello FIML dei domini: 732/739 sono compatibili con i
risultati salvati entro 0.001; due differenze minime attraversano il confine
dell'arrotondamento del PDF. I sette scarti sostanziali sono tutti nella Tabella 6.
La matrice FIML completa 12×12 dei domini coincide con quella salvata dagli autori
entro 0.0000002.

Il controllo della versione lavaan 0.6-10 indicata da `renv.lock`, installata in
una libreria temporanea isolata, riproduce entro l'arrotondamento sei dei sette
coefficienti problematici, ma tutti e sette i fit diagnostici risultano non
convergenti. I due controlli convergono. Questo è un indizio concreto di
coefficienti estratti da fit non convergenti, non una prova dello stato di tutti
i fit originali (che non è salvato). Non si promuovono coefficienti non
convergenti soltanto perché assomigliano a quelli del PDF. Per ripetere la
diagnostica, impostare `LIVING_SEB_REPLICATION_LIBRARY` alla libreria isolata
contenente lavaan 0.6-10 e aggiungere `--legacy-diagnostics`.

I blocchi pubblicati hanno origini diverse:
Tabelle 5–6 = faccette T2 × BFI T0/T2 (PIM);
Tabella 7 = domini T2 × BFI T0/T2 (FIML);
Tabella 8 = faccette T0 × intelligenza T1 (PIM);
Tabella 9 = domini T2 × intelligenza T1 (FIML).
I domini riproducono i punteggi `_prm` effettivamente usati dagli autori,
senza sostituirli silenziosamente con quelli rivisti o con pesi armonizzati.
**Decisione corrente del 14 novembre 2026:** mantenere i due campioni
dell'impostazione manuale: `0050a` T4, N=940; `0050b` Study 3, N=767 e 768 righe
originali. La successiva prova di unire panel e T4 con la prima rilevazione BESSI
disponibile (1.857 persone, N analitico 1.855) è stata annullata su richiesta
dell'utente. Le due matrici ripristinano esattamente i valori già approvati prima
di quella prova, mantenendo la chiave T2 rivista e i pesi/regole di mancanza dei
domini. L'esperimento e il ripristino restano nel ledger e nell'audit, non negli
output attivi. Non si crea una terza matrice e non si importano i blocchi PIM/FIML
come fossero correlazioni osservate della stessa occasione. Si accettano e si
documentano sia i 238 partecipanti condivisi sia gli N per correlazione diversi.

La codifica generale, metadata, review e registro delle ricerche sono aggiornati
tramite `workflow.R`. Le revisioni numeriche esatte sono nel ledger esistente;
il primo confronto approvato resta come `approved_integration_2026_11_13`.
Il dataset attivo ha 30 articoli quantitativi e 38 matrici, con 51 articoli nella
review. Dei quantitativi attivi sono stati esaminati tutti i 30 articoli e le
38 matrici. Esaminato non significa sempre ricostruito dagli item: alcuni lavori
restano verificabili soltanto dalle matrici pubblicate o fornite dagli autori.
Restano limitazioni esplicite, incluso il contrasto sul genere di 0128, che
l'utente mantiene con nota. Anche 0013 è stato esaminato, ma non è più
conteggiato fra i quantitativi.

## Quinto gruppo storico: 0053, 0068 e 0070

Verifica del 14 novembre 2026. I tre script riproducono complessivamente le
6.925 correlazioni delle sette matrici precedenti a questa verifica. Le cinque
matrici 0053 e le 28 correlazioni già presenti in 0070 restano invariate.
`0053.R` aggiorna soltanto l'audit; `0068.R` e `0070.R` esportano le correzioni
approvate tramite il consueto workflow. `--check-only` non scrive nulla.

| Matrice | N di riferimento | N per coppia | Verifica |
|---|---:|---:|---|
| 0053c | 400 | 400 | Tutti i punteggi BESSI e i pesi ufficiali riprodotti. |
| 0053d | 600 | 600 | Come sopra; resta il riferimento di sovrapposizione già usato per 0150. |
| 0053e | 322 | 247–322 | Anche tutte le 20 scale BFI-2 ricostruite dagli item. |
| 0053f | 499 | 62–499 | Disponibilità diversa degli esiti; media storica GPA inverno/primavera conservata. |
| 0053g | 488 | 488 | BESSI other-report; sesso della persona valutata. |
| 0068a | 356 | 350–356 | Tutti mantenuti; voti ricalcolati e standardizzati per anno scolastico. |
| 0070a | 702 | 702 | Recuperato il comportamento proambientale; 15 riferimenti pubblicati riprodotti. |

**0053:** tutte le 160 medie delle faccette (32 × 5 campioni) e i 25 domini
ponderati riproducono i punteggi forniti. I dati erano già sottoposti alla
procedura di imputazione dichiarata nella fonte; non vengono imputati di nuovo.
Gli identificativi `Case` del campione college non sono univoci (165 etichette
ripetute), ma nessun record completo o profilo BESSI è duplicato: non si eliminano
persone in base a queste etichette. Gli esiti non-BESSI/non-BFI rimangono compositi
forniti, senza affermare una ricostruzione completa dei loro item.

I campioni A/B sono file preliminari con sottoinsiemi di item somministrati per
dominio; non si inventano matrici complete per risolvere i segnaposto storici.
Per il campione high-school, i GPA di inverno/primavera sono successivi al BESSI:
la media disponibile storica è mantenuta e il tempo prospettico è segnalato.
Per il campione other-report, la matrice usa correttamente il sesso del target,
ma i descrittivi storici erano degli informatori (49,26 anni; 51,6% donne).
Per decisione dell'utente del 14 novembre, i descrittivi correnti si riferiscono
ai target: media 47,52 anni e 46,72% donne; quelli degli informatori restano nelle
note. I nomi storici senza suffisso
rimangono distinti dalla successiva armonizzazione delle modalità di misura.

**0068 — correzione approvata:** la matrice storica usava già 356 persone, mentre
il campo N=350 si riferiva alla selezione degli autori che escludeva sei risposte
di genere diverse da maschio/femmina. Per decisione esplicita si mantengono tutti
e si porta N a 356; solo il sesso binario rimane mancante nei sei casi. La quota
femminile è ora 207/356=58,15%, età media 16,65. Si ricostruiscono i cinque voti
medi, correggendo due aggregati forniti (11 diventa 10; 9,2 diventa 9), poi si
standardizza il rendimento separatamente entro quarto e quinto anno usando tutti
i partecipanti mantenuti. Nei dati individuali restano media grezza, anno ed età.
Cambiano otto correlazioni, tutte relative al rendimento. La replica separata
del campione e dei punteggi pubblicati riproduce 55/55 valori della Tabella 2.

**0070:** BESSI-45 ricostruito; CNS-12 e SPACS-12 seguono il codice originale.
Gli item CNS erano già invertiti nella fonte e non vengono invertiti di nuovo;
CNS12/14 restano esclusi. Le 13 associazioni pubblicate già presenti coincidono
entro arrotondamento. `PebSingle` misura il comportamento proambientale ed è
omesso dallo script storico, pur esistendo `proenvironmentalbehavior` nel
codebook. Dopo il chiarimento sui costrutti, l'utente approva il recupero:
SPACS resta `sustainabilitycompetence`, mentre PebSingle diventa
`proenvironmentalbehavior`. Si mantiene la scala originale 1–7 dell'item di
frequenza. La matrice contiene ora nove variabili e 36 correlazioni: otto nuove,
le 28 precedenti invariate, N=702 per tutte. Le due associazioni pubblicate del
nuovo item con CNS e SPACS coincidono entro arrotondamento (15 riferimenti
verificati in totale). La precedente sospensione resta nello storico dell'audit,
superata dalla correzione approvata. Non si trattano competenza e comportamento
come due misure equivalenti.

Originali e codice degli autori sono registrati in `coding/sources.csv`; le date
di acquisizione sconosciute restano tali. Gli accessi automatici OSF non sono
riusciti, senza modificare fonti locali o eleggibilità. Non vengono contattati
autori e non vengono sostituiti file originali.

## Sesto gruppo storico: 0071, 0080 e 0087

Verifica del 14 novembre 2026: nessun coefficiente, N o partecipante modificato.
I tre script aggiornano soltanto `historical_audit.csv`; `--check-only` non scrive.
Le note sono integrate tramite codifica generale e `workflow.R`.

| Matrice | N di riferimento | N per coppia | Verifica |
|---|---:|---:|---|
| 0071a | 5.075 | 4.564–5.075 | 105 correlazioni osservate riprodotte dagli item. |
| 0080a | 319 | 319 | 45 correlazioni storiche e 55 valori pubblicati riprodotti. |
| 0087a | 373 | Non ricostruibile | 63 coefficienti pubblicati e 15 celle mancanti verificati. |

**0071:** medie BESSI-45 e medie delle altre scale con le regole di mancanza
storiche. Le esclusioni BESSI e le mascherature degli altri questionari erano già
applicate nella fonte. `carSl` riguarda l'accettazione dei pari: non si eliminano
le risposte SWLS valide dei 136 casi segnalati. Gli item AMOS sono già orientati
per l'analisi secondo il codice degli autori; la chiave originale e i testi non
sono disponibili per una verifica indipendente delle inversioni. La Tabella 2
usa punteggi fattoriali da CFA ordinale: non è un riferimento numerico diretto
per le nostre medie osservate. Non si sostituiscono i punteggi storici.

**0080:** tutte le cinque medie BESSI-20, le sette inversioni SRL e le medie
SRL/ASE/AS coincidono con i totali forniti. Anche le 55 correlazioni pubblicate,
incluse età e anno di corso solo per verifica, coincidono entro arrotondamento.
La conversione dei voti tra paesi rimane quella fornita. Due punteggi convertiti
sono zero, benché il testo descriva 1–5; il supplemento di conversione non è stato
recuperato automaticamente. Non si inventano correzioni o esclusioni: la regola
di conversione non è dichiarata indipendentemente verificata. La presenza del
file locale non modifica automaticamente il campo di disponibilità pubblica.

**0087 — decisione dell'utente:** mantenere incluso con nota e N storico 373.
La trascrizione è corretta, ma la Tabella 2 contiene correlazioni di un modello:
PSEC bifactor ESEM, predittori/esiti rappresentati da medie corrette per errore
di misura (carico 1, varianza residua `varianza * (1 - omega)`), MLR/FIML.
Non sono normali correlazioni Pearson tra punteggi osservati. A T1 ci sono 373
persone, a T2 221, cinque settimane dopo. Non si attribuiscono N per coppia
inesistenti e non si trasformano in zeri le 15 celle non riportate tra fattori
PSEC ortogonali per vincolo del modello. L'approvazione etica impedisce la
condivisione dei dati individuali. Una sensibilità senza questo studio resta
un'analisi futura; non è stata eseguita né è stata cambiata l'inclusione.

## Settimo gruppo storico: 0095, 0100 e 0107

Verifica del 14 novembre 2026. Articoli:

- **0095 — Sewell et al. (2026):** *Experiental features of volunteering predict
  changes in college students' social, emotional, and behavioral skills*.
- **0100 — Pellegrino et al. (2026):** *University Students With Specific Learning
  Disabilities: Do Soft Skills and Study-Related Factors Make a Difference to
  Their Academic Outcomes?*
- **0107 — Fallucchi, Marietta Leina e Quercia (2026):** *Validating a survey
  measure of conditional cooperation*.

| Matrice | N di riferimento | N per coppia corrente | Esito |
|---|---:|---:|---|
| 0095a | 455 | 418–446 | Medie disponibili conservate per decisione dell'utente. |
| 0100a | 752 | 331–752 | Voti standardizzati per laurea e intelligenza fluida recuperata. |
| 0107a | 259 | 255–259 | Unione e sesso corretti; scoring BESSI verificato nel supplemento. |

**0095:** riprodotte tutte le 21 correlazioni storiche a T1. La media dei nove
item disponibili rimane quella scelta dall'utente; i nove casi senza alcuna
risposta BESSI non vengono eliminati dal dataset. La variante degli autori
richiede tutti i nove item del dominio e riproduce i 21 riferimenti pubblicati,
ma non viene adottata. Con la regola conservata, dieci riferimenti differiscono
oltre arrotondamento (massimo 0,02424). Il codice degli autori usa correlazioni
pairwise sui punteggi completi, benché la nota della tabella dica listwise.
Nessuna matrice T2, ore di volontariato successive o riflessioni selezionate viene
aggiunta. CSV e XLSX originali contengono gli stessi valori. Lo script salva
soltanto l'audit; nessuna matrice o dato individuale viene riscritto.

**0100:** tutte le 78 correlazioni precedenti ricostruite. Per decisione
dell'utente, il rendimento viene standardizzato separatamente per bachelor,
master e ciclo unico, come nel codice degli autori. Tutti i 91 coefficienti della
Tabella 4, incluse età e intelligenza per confronto, coincidono entro
arrotondamento. Si recupera `intelligencefluid` dal totale Cattell fornito:
13 associazioni nuove; cambiano le 12 correlazioni precedenti con il rendimento.
Rimangono 752 persone e 331 voti, senza imputazioni. Tutti i questionari sono
ricalcolati dagli item, comprese le inversioni QAS e la media equiponderata delle
sei sottoscale di engagement. Mancano invece le risposte ai singoli quesiti
Cattell: il loro scoring non viene dichiarato indipendentemente ricostruito.
Nei dati privati restano età, voto grezzo e tipo di laurea.

**0107:** riprodotte tutte le 190 correlazioni della vecchia matrice. L'unione
per il solo identificativo, ripetuto due volte in entrambi gli originali,
generava 261 righe da 259 persone. L'abbinamento univoco usa identificativo più
le 29 risposte comuni, tutte identiche, senza eliminare nessuno. Per decisione
dell'utente, i quattro generi `Altro` restano nel dataset e diventano mancanti
solo nel contrasto binario sesso (prima valevano arbitrariamente 0,5).

Su richiesta dell'utente è stato approfondito lo scoring: la Tabella 3 rimanda
alla chiave Soto; il questionario in Appendice D (supplemento, pp.17–18 e 24)
somministra **sei** item etici, Q40/45/50/55/60/61. Il totale fornito ne usa cinque
ed è dimezzato. La faccetta viene ricostruita come media dei sei item su 1–5;
il dominio applica i pesi ufficiali normalizzati (quattro faccette con peso 1,
etica con peso 0,5; denominatore 4,5). Il codice pubblico di replica consuma
i totali forniti senza mostrarne la costruzione: non smentisce l'errore
aritmetico riscontrato. Q35 conferma il teamwork per il campo `item_24`, che è
un'etichetta errata dell'item ufficiale 23; il dato originale non viene alterato.
Quattro coppie TIPI sono ricostruibili; per l'estroversione si conserva il totale
fornito perché manca il secondo item grezzo.

Cambiano tutte le 190 correlazioni per effetto dell'unione corretta; sul campione
correttamente abbinato, il nuovo scoring del dominio modifica 19 coefficienti.
N rimane 259. La proposta di aggiungere `conditionalcooperation` **non è stata
adottata dall'utente**. L'attuale misura è un sottoinsieme di 30 item della sola
cooperazione: l'etichetta storica `bessi45` è esplicitamente segnalata nelle note
e resta da armonizzare con i codici degli strumenti, separatamente.

Gli script 0100/0107 esportano con la procedura compatta; `--check-only` non
scrive. I confronti iniziali rimangono nell'audit e le revisioni esatte nel ledger
di compatibilità. Matrici CSV/XLSX nella cartella comune, N per coppia nella loro
cartella, dati individuali CSV soltanto locali. Il supplemento, README e codice
finale di 0107 e il modulo ufficiale BESSI sono stati scaricati il 14 novembre;
il checksum del `Dataset.csv` pubblico coincide con `d0107a.csv`, senza creare
una seconda copia. Le date degli originali storici restano sconosciute.

## Ottavo gruppo storico: 0108, 0118 e 0126

Verifica del 14 novembre 2026:

- **0108 — Soto et al. (2026):** *Practice makes perfect: A behavioral challenge
  intervention to develop social, emotional, and behavioral skills*.
- **0118 — Cravet e Usai (2026):** *Using board games as a protective tool:
  Effects on cognitive, social, emotional, behavioural outcomes, and learning*.
- **0126 — Allemand et al. (2026):** *Motivation to change personality across
  adulthood: Preferences for human and digital assistance in representative Swiss
  samples*.

| Matrice | N corrente | N per coppia | Esito |
|---|---:|---:|---|
| 0108a | 470 | 401–470 | Prima misurazione conservata; applicato il contrasto binario del sesso. |
| 0118a | 150 | 144–150 | Corretti quattro totali CPM e sesso secondo la decisione dell'utente. |
| 0126a | 1.917 | 1.773–1.906 | Tutta la seconda parte; N corretto, coefficienti invariati. |

**0108:** tutte le 253 correlazioni storiche riprodotte da punteggi forniti,
senza pretendere la ricostruzione di item non condivisi. Il PDF principale,
inizialmente assente, è stato aggiunto dall'utente durante l'audit e verificato:
conferma la rotazione delle batterie di esiti tra settimane dispari e pari.
Restano BESSI, tratti ed esiti disponibili a T1, con cinque esiti osservati per
la prima volta a T2 collegati per identificativo univoco. Non si esportano le
occasioni successive o le variabili storiche `*_t2` ignorate dal codebook.

La Tabella S2 riporta 106 delle 136 associazioni confrontabili fra 17 variabili:
76 coincidono direttamente; 30 coincidono esattamente usando BESSI e
soddisfazione di vita a T2 con gli esiti T2. Altre 30 celle tra esiti raccolti in
settimane diverse sono vuote nel supplemento, mentre la nostra matrice le calcola
collegando le persone. Questa differenza di occasione rimane esplicita: non si
sostituiscono i punteggi iniziali per forzare la tabella pubblicata.

Il paper usa un contrasto di genere codificato −0,5/0/0,5. Per la convenzione
binaria del progetto, gli 11 codici intermedi non diventano 0,5 ma mancanti;
restano mancanti anche i due codici già assenti. Le 470 persone rimangono nel
dataset, con 457 osservazioni del sesso binario. Cambiano solo le sue 22
correlazioni. Il volontariato resta il composito standardizzato fornito di tre
item, non viene interpretato come numero grezzo di ore. Il checksum del file
pubblico OSF coincide con l'originale locale; non se ne scarica una seconda copia.

**0118:** tutte le 120 correlazioni precedenti riprodotte. Le cinque medie BESSI
ricalcolate dagli item coincidono con i punteggi forniti arrotondati a tre decimali.
I mapping TeleFE e la media delle due condizioni di memoria di lavoro sono
confermati dal paper e dalla replica di 272 coefficienti pubblicati nelle tabelle
separate IG/CG. Quella replica usa i totali CPM originali per confrontare gli
stessi valori degli autori, senza presentarli come punteggi già corretti.

Quattro totali CPM differiscono di un punto dalla somma delle 36 risposte 0/1,
tutte complete; per gli altri 146 bambini la somma coincide esattamente.
L'utente approva il ricalcolo dei quattro totali e il sesso coerente con il paper:
il codice 1 conta esattamente le 46 femmine IG e le 27 CG, pur contraddicendo il
nome `gender_0F1M`. Si mantiene il 48,7% femminile riportato e si registra la
discordanza nell'etichetta. Cambiano 29 associazioni, tutte relative a sesso o CPM;
nessuno viene escluso. Il BRIEF resta orientato a maggiori difficoltà: è verificata
l'aritmetica degli indici dai punteggi di sottoscala forniti, non la chiave
proprietaria degli item o le risposte ai singoli trial TeleFE. Innovazione resta
inclusa benché gli autori la escludano dai modelli per affidabilità bassa. Il paper
descrive ancore BESSI di frequenza: mancando i moduli somministrati, la discrepanza
con le usuali ancore di abilità resta nelle note per la successiva armonizzazione.

**0126:** l'originale contiene 3.030 righe della prima parte, di cui 1.917 con
dati della seconda parte. Le 1.113 persone senza alcuna variabile della seconda
parte non contribuivano alle correlazioni: selezionare i 1.917 conserva tutti i
15 coefficienti. N=1.898 si riferiva invece alle esclusioni degli autori, non
applicate storicamente. L'utente sceglie tutti i 1.917 e l'aggiornamento dell'N.
Manca l'item sulla preferenza di assistenza necessario a distinguere quattro
casi al confine del filtro pubblicato: non si inventa una selezione per ottenere
1.898. Il nuovo CSV individuale conserva anche le tre età mancanti, prima escluse
dalla sola copia XLSX storica. L'età è 2024 meno anno di nascita. Restano le medie
disponibili dei sei item di adattabilità e le inversioni esplicite dei singoli
item; le categorie ordinali non vengono trasformate in nuove misure continue.
I descrittivi correnti seguono i rispondenti mantenuti: 970/1.917 donne (50,60%)
ed età media 52,72 anni su 1.914 anni di nascita disponibili. Età 52,67 e quota
femminile 50,9% pubblicate per il campione ristretto restano nelle note, senza
mescolarle con il nuovo N. Il primo campione non misura BESSI e non riceve una
matrice.

Tutti e tre gli script esportano con il consueto flusso compatto. Gli originali,
i vecchi codici e le vecchie copie individuali XLSX rimangono intatti; i nuovi
individuali canonici sono CSV locali. Prima/dopo nell'audit unico, transizioni
esatte nel ledger, note in codifica/metadata/review. La data dei supplementi
scaricati è il 14 novembre; per il PDF 0108 è registrata la ricezione dall'utente,
mentre il download originale e la data bibliografica non vengono dedotti.

## Ultimo gruppo storico: 0128, 0134, 0138 e 0141

Verifica del 15 novembre 2026. Tutti i quantitativi attivi sono stati esaminati,
con i limiti sotto indicati. Un solo coefficiente viene corretto in questo
gruppo, su approvazione esplicita; nessun N cambia. Gli script storici e gli
originali sono conservati. Il consueto `last_update.xlsx` contiene il prima/dopo.

| Articolo | Fonte e risultato | Limite o decisione |
|---|---|---|
| 0128 — Springstein, Bankston & English, *To Regulate or Not to Regulate? Situational and Individual Differences in Emotion Regulation Initiation in Daily Life* | Tabella 2: 105 coefficienti riprodotti, inclusa l'inversione storica del contrasto cis-man. N=215. | Le 91 associazioni senza sesso coincidono direttamente. `sex` significa non-uomo-cisgender contro uomo-cisgender, non donna contro uomo. L'utente mantiene tutte le correlazioni con nota esplicita il 15 novembre 2026; matrice invariata. |
| 0134 — Pellegrino et al., *Social, emotional, and behavioral skills as resources for student engagement: A one-year longitudinal study in high school students* | Tutte le 45 correlazioni T1 ricostruite dagli item; N per tutte le coppie=1330. | Campione dipendente da 0068; entrambe le matrici sono conservate senza nuove esclusioni. |
| 0138 — Kramer et al., *Self-Efficacy Predicts Differential Neuroticism Change in a Smartphone-Based Intervention* | Figura S3, pannello T1: nove coefficienti corretti; autoefficacia × growth mindset corretto da .10 a .16. N=446 invariato. | Dati individuali subordinati a un contratto SWISSUbase; nessun N per coppia viene inventato. |
| 0141 — Sharabi, *Understanding Academic Adjustment in Undergraduates With SLD or ADHD: An Integrative Model of Social-Emotional-Behavioral Skills, Academic Self-Efficacy, and Self-Regulated Learning* | Tutte le 45 correlazioni riprodotte dalle due tabelle totali degli autori, N per coppia=525. | Sesso confermato dall'utente rileggendo le email degli autori. Mancano i descrittivi verificabili del sottocampione ridotto. |

**0128:** vengono usate solo le associazioni fra persone, sotto la diagonale:
quelle sopra sono entro persona e non vengono confuse con le prime. Il codice
pubblico chiama `psych::statsBy` e richiede due dataset non depositati. Non si
creano dati individuali né N per coppia. Il campione deriva da 216 reclutati meno
una persona senza alcuna risposta ESM; non è applicata una soglia di adesione.
Il paper riporta 75,35% donne cisgender, 21,86% uomini cisgender, 0,93% non-binari
e 0,47% transgender: quest'ultima categoria non identifica necessariamente un
genere non binario e le percentuali non coprono il 100%.

**0134:** il rilascio contiene 1.555 persone selezionate dagli autori per il
longitudinale; si conservano i 1.330 rispondenti T1, senza recuperare altri tempi.
Tutti i domini BESSI45 e le quattro scale di engagement sono verificati dagli
item, comprese le quattro inversioni. `0134.R` esporta nel flusso compatto la
matrice comune, gli N per coppia e il CSV individuale locale con età. Non si
ricostruiscono le esclusioni a monte di persone assenti dal rilascio.
Il paper dichiara che T3 riusa dati già pubblicati in 0068. Profili completi e
univoci su tutti i 45 item identificano 273 corrispondenze con 0068; 238 sono
presenti anche nel T1 usato qui, con sesso concordante. Gli identificativi
anonimi non vengono usati come chiave d'identità; i campi di età discordano e
non vengono riconciliati per supposizione. La dipendenza è annotata in entrambi
gli articoli: non si presume che i casi senza corrispondenza siano indipendenti.

**0138:** `0138.R` esporta la matrice pubblicata corretta direttamente nella
cartella comune, senza fingere un'estrazione individuale o un file di N per
coppia. Il prima/dopo resta nell'audit e la transizione esatta è nel ledger.
Si conserva il BFI-2 come misura self-report del nevroticismo e l'IAT separato;
la seconda misura B5T dello stesso costrutto non viene aggiunta. Gli HTML degli
autori confermano Pearson pairwise a T1; il codice scaricato non è eseguito.

**0141:** il file degli autori contiene il totale ridotto N=525, mentre il paper
usa N=651. L'esclusione di 126 persone sovrapposte a 0080 è la decisione storica;
non essendoci individui, non la si riverifica da identificativi. Il paper riporta
475 donne e 176 uomini (73%) sul campione completo e non offre confronti per
genere che permettano di ricostruire la chiave. La direzione è mantenuta sulla
conferma esplicita dell'utente del 15 novembre dopo controllo delle email; le
email non sono state acquisite né lette dall'agente. Età e percentuale femminile
storiche non sono certificate per il sottocampione ridotto. Alcuni fogli separati
per diagnosi differiscono fra loro: non sostituiscono i due totali concordanti.

`0128.R` e `0141.R` aggiornano solo l'audit, senza riscrivere le matrici. Tutti
e quattro gli script rispettano `--check-only` senza scrivere file. I nuovi
originali pubblici OSF sono locali e ignorati da Git, con SHA256 e download del
15 novembre in `coding/sources.csv`; le date originali ignote restano ignote.

## SECA facet recovery in 0053e (17 November 2026)

The pilot exposed five SECA facets reported in Study 4 but missing from the
previous matrix. The original college file contains all five scale means:
SECA_SA_StrengthsWeaknesses (Self-Concept), SECA_SA_EmotionKnowledge,
SECA_SM_EmotionRegulation, SECA_SM_GoalManagement and SECA_SM_SchoolWork.
They map to secaSelfconcept, secaEmotionknowledge, secaEmotionregulation,
secaGoalmanagement and secaSchoolwork. Original 1-4 means are supplied by the
authors; no independent item-key reconstruction is claimed.

All five share 249 observed participants among the 322 retained records.
The matrix has 87 variables and 3741 pairs: 420 additions, no changed old
coefficient or N. New pairs have Ns 247-249. The old common block is saved
exactly after Pearson reconstruction agreement is checked. No automatic
positive-definiteness correction or additional participant selection is applied.

0053.R exports only 0053e through the compact workflow: private individual CSV
with age, common matrix CSV/XLSX, pair Ns and summary. The other four matrices
remain read-only checks. --check-only writes nothing. The existing exact
revision ledger and last_update.xlsx document the addition.

The approved review pilot is now integrated in records.csv, coding.xlsx and
review_data. Topics reflect scientific focus; analyzed constructs are indexed
once per article, including empirical review-only papers. All 51 current articles
have received the expanded coding. The remaining fields stay explicitly blank.

Second review group, 2026-11-18: 0071 Feraco et al. (SEB/SRL integration),
0080 Sharabi et al. (university achievement and satisfaction), and 0113 Feraco
et al. (SLD replication and synthesis). The first two are cross-sectional;
0113 includes cross-sectional reanalysis plus a mini-meta-analysis. The 0113
cohort is the same as 0071 and remains review-only/noNewData. Its final Figure 1
and note 3 identify two high-school studies; no university context is inferred
from residual discussion of an earlier three-study version. Indexed constructs:
14, 9 and 14 respectively, with no duplicate scores or extra matrices. All three
old positiveschooloutcomes assignments have been replaced after reading the
papers; the code is retained as retired for historical compatibility. No
coefficient, reference N, matrix file or historical manual script was changed.

Third review group, 2026-11-18: 0012 Vega-Dienstmaier et al. (Peruvian
emotional-resilience validation), 0014 Walton et al. (raters and item formats),
and 0015 Lechner and Urban (social-category inequalities). All remain review-only
with their historical sample fields. Indexed constructs: one, 22 and 37.
The five ability-framed BFI domains in 0014 have their own non-BESSI family;
Likert/forced-choice and self/caregiver variants do not duplicate the construct
list. External criteria are mapped explicitly in coding evidence, including
two tolerance indicators under one broad construct; no composite is computed.
0015 uses two independent cohorts, not longitudinal repeated measurements,
and analyzes all 32 BESSI facets plus five domains. Its social-group dimensions
are indexed by focus topics; age is a sample descriptor, not a focal age effect.
Four source-defined topics were registered (response formats, migration
background, educational tracking and intersectionality); no older unreviewed
paper was automatically recoded. No quantitative data or matrix changed.

Fourth review group, 2026-11-18: 0018 Feraco et al. (SLD profiles and outcomes),
0023 Beatty et al. (purpose during university), and 0026 Chen et al. (workplace).
Indexed constructs: 12, 38 and 15. In0018, only the five innovation facets are
separately analyzed in addition to the domains; other historically available
matrix facets are not automatically added to the analyzed-construct list.
In0023, the32 facets are actual predictors; domain labels identify blocks of
predictors rather than five aggregate scores. Two purpose scales index one
construct. Beatty remains review-only/missingData; Walton0014 remains
review-only/noValidatedMeasure with no matrix.

The user explicitly approved recovering both objective performance indicators
in0026a. Released showing_center and deal_center are already monthly and
centered within city. Keep them unchanged as jobperformanceactivity and
jobperformancesales:15 to17 variables,105 to136 associations,31 additions,
no old coefficient changed. Retain all2992 people; observed indicator Ns are
2723/2196, with new pair Ns2195-2723. Eighteen of20 published performance
correlations reproduce within rounding. The two printed stability/performance
signs disagree with the reversal of the released neuroticism associations.
Preserve the actual-data coefficients and document the discrepancy.

0026.R now exports the common CSV/XLSX matrix, pair Ns, extraction summary and
private individual CSV with age. --check-only remains read-only. Source URLs,
unknown original retrieval dates, hashes and exact historical transition are
recorded in the existing provenance/evidence/ledger tables.

Fifth review group, 2026-11-19: 0027 Sewell et al. (BESSI short forms),
0030 Feraco et al. (Italian BESSI), and 0034 Postigo et al. (Spanish BESSI).
Indexed constructs:78,46 and68 respectively. In0027, the43 criterion rows in
Table4 represent41 unique constructs after merging repeated GPA and life
satisfaction entries; the five BESSI domains and32 facets are indexed once
across forms and informants. New entries represent personal income and personal
burnout; neither implies a new matrix. Finer SECA subscales recovered in0053
are not separate analyzed criteria in0027. The paper remains review-only/noNewData.

Focus topics are assessment/short forms/language adaptation for0027,
assessment/language adaptation/nomological network for0030, and those three
plus incremental validity for0034. Individual validation criteria remain
discoverable through constructs. Generic selfreport tags are removed where
self-report is a method rather than a dedicated comparison. SEB measurement
still records ability, informant and frequency/typical-behavior methods as
actually used. SEC-Q in0034 retains its historical broad construct mappings;
this does not imply administration of the SECA instrument.

All three retain cross: ancillary reliability retests do not add a longitudinal
tag, and0030 recontacting supplies remaining questionnaires rather than modeled
skill change. Existing quantitative routes, sample IDs, Ns, overlap decisions,
matrix/pair-N files, source extraction scripts and historical manual files are
unchanged. Coding evidence retains source pages and PDF hashes.

Sixth review group, 2026-11-19 (five articles):0035 Yoon et al. (ACT prediction),
0042 Feraco and Meneghetti (adolescent age/gender differences),0043 Soto et al.
(matched skill/trait items),0045 Sewell et al. (prospective volunteering),
and0048 Soto et al. (adolescent outcomes beyond traits). Indexed constructs:
11,37,19,9 and40, using only existing variable/topic codes.

0035 includes five representative BESSI facets,not five complete domains; ACT
is indexed as standardized achievement. 0042 includes all32 facets explicitly
analyzed in AppendixA as well as five domains; cross-sectional age patterns
are not within-person change. Its historical review-only route is retained.
0043 has ability/frequency versions of the same20 BESSI items plus five Mosaic
scores; frequency-framed traits retain their existing broad trait mapping and
are not mislabeled BFI-2. Forced-choice self-report is not a performance task.
The focal response-format comparison is now an explicit topic. Two school-grade
indicators index one construct; no new aggregate is computed.

0045 includes all eight analyzed facets,including capacity for consistency,
and volunteering once regardless of hour/category operationalization. Initial
skills predict hours over ten weeks,so long is retained; program participation
does not establish an evaluated SEB training intervention or repeated skill
change. 0048 indexes five BESSI domains,five Big Five traits,five SECA dimensions
and25 outcomes. Its topics describe nomological network,incremental validity
and skill-trait differences; school outcomes are searchable through constructs.
The approved representative status over0013 and all historical numerical values
are unchanged. No matrix,pair-N file,codebook or extraction script was modified.

Seventh review group,2026-11-19:0049 Breil et al. (self-concepts and role-play
performance),0050 Lechner et al. (German BESSI),0057 Napolitano et al.
(conceptual skill model),0061 Feraco et al. (change goals/beliefs),and0065
Napolitano et al. (longitudinal correlated change). Indexed constructs:
24,59,not applicable,4 and29. Coverage is30/51 articles and775 construct links.

Three new personal-change constructs distinguish perceived feasibility,
motivation and anticipated benefit from desired change. In0061 the skill/trait
names are referents for these judgments,not measured ability/personality levels;
SEB measurement is Not applicable. Its within-person framing experiment remains
exp and is not a skill-training intervention. Purely conceptual0057 has theory
design and Not applicable for actual samples,administered measures and analyzed
constructs; its conceptual content is indexed by focused theory,trait-skill,
assessment and developmental-process topics.

New temporalstability distinguishes substantive rank-order/true-score stability
from normative mean change and ancillary reliability retests. It is assigned to
0050 and0065. The German paper explicitly studies temporal stability as a
central aim; its pilot/report and Study2 design now include cross;long. The
Study3 matrix remains cross with the previously accepted staggered timing.
The two selected matrices,all scores,occasions,Ns and overlap decisions remain
unchanged. 0065 is long and remains review-only; emotional/behavioral engagement
are separate constructs,while outcomes from the wider baseline dataset are not
automatically added. All three waves index each base construct only once.

0049 includes expressed skills once across self/observer appraisals. The user
approved the separately prepared recovery on2026-11-19. The registered self
suffix identifies task_socialengagementself,task_cooperationself and
task_emotionalresilienceself; existing observer task codes are unchanged.
The common matrix now has28 variables and378 correlations (78 additions),
with new pair Ns130-137 and all137 participants retained. All300 old coefficients
are preserved exactly. Nine selected published observed-score references match
rounding. The script exports the common CSV/XLSX,pair Ns,private individual CSV
and aggregate checks; the exact approved transition and evidence are recorded.
Source self-appraisals retain their1-6 metric,distinct from global BESSI scores.

Eighth review group,2026-11-19:0066 De Fruyt/Scheirlinckx (assessment review),
0073 Ichsan et al. (VR review),0087 Collie/Ryan (autonomy/PSEC),0095 Sewell et al.
(volunteering experiences) and0100 Pellegrino et al. (university SLD).
Indexed constructs: not applicable,not applicable,13,9 and13. Coverage is35/51
articles and810 links. Only experientiallearning required a new base construct;
positive reflection valence and coded social interaction reuse positiveaffect
and socialsituation with explicit operational evidence.

0066 is a narrative methodological review; measurement modalities are focal
topics rather than falsely administered measures. 0073 is a descriptive systematic
review: its SEB categories classify technologies/studies and do not establish
quantitatively synthesized BESSI scores or pooled effects,despite a Meta-Analysis
section heading. Both have review design and no original sampled context or
analyzed-score index. 0087 is long for T1 support predicting T2 competence/skills;
its retained model correlations,N373,FIML,221 follow-up participants and15 blanks
are unchanged. 0095 is long;exp for the reported nonrandomized quasi-experimental
program comparison,not random allocation. Later experience features enter only
review coverage; its first-occasion matrix/available-item scoring are unchanged.
0100 indexes13 actually analyzed scores; scoring intermediates are not invented
as separate reported subscale analyses. School factors are now an explicit focus
topic. No quantitative value outside the approved0049 addition was changed.

## 2026-11-18: CAAS and SPACS resources

0068 Pellegrino et al., "Ready for What's Next? The Associations Between Social,
Emotional, and Behavioral Skills and Career Adaptability in High School Students":
four CAAS resources (concern, control, curiosity, confidence), each a six-item
mean independently checked against the supplied score. Retain the overall
adaptability score and all 356 people. Matrix 0068a expands from 9 to 13 variables,
36 to 78 associations, preserving its old common block exactly. The published
350-person reference still reproduces all 55 Table 2 coefficients; it does not
override the approved retained sample or corrected within-year grades.

0070 Feraco et al., "The role of social, emotional, and behavioral skills for
nature connectedness, action competence, and proenvironmental behaviors in
adolescence": add SPACS Knowledge of Action Possibilities (items 1-4), Confidence
in Own Influence (5-8), and Willingness to Act (9-12), as four-item means using
the authors' key. All 30 published associations involving these resources
reproduce within rounding. Matrix 0070a expands from 9 to 12 variables, 36 to
66 associations, retaining all 702 people, the total score and PebSingle.

Both additions were explicitly approved by the user. Common matrix CSV/XLSX,
private individual CSV, pair Ns, extraction summary, exact revision ledger and
coding evidence are updated. No old coefficient or reference N changes.


Ninth review group,2026-11-19:0104 Sun et al. (longitudinal mental-health
networks),0107 Fallucchi et al. (conditional-cooperation validation),0108 Soto
et al. (behavioral challenges),0114 Jalynskij et al. (adult age differences),
and0118 Cravet/Usai (board games). Indexed constructs:9,26,19,59 and15;
coverage is40/51 articles and938 article/construct links. Nine base constructs
and the boardgames focus topic were registered before workbook import.
Existing variable groupings and compact,unwrapped Excel formatting are preserved.

0104 stays review-only/dataQuality,with nine network nodes; wellbeing component
scales are not duplicated as separate analyzed outcomes. 0107 stays meta-only;
its construct index includes individual-item regression and supplementary
behavioral/environmental analyses,but does not add the five BESSI facets merely
because they are available for scoring. Distinct conditions/items are indexed
under shared constructs without calculating new totals. The user's earlier
choice not to recover conditional-cooperation matrix correlations remains in
force. Design exp identifies experimental behavioral elicitation,not skill
training or random intervention allocation.

0108 has five skill domains,twelve outcomes,change goals and skill practice;
Big Five scores available in the release/historical matrix are not automatically
indexed as analyzed paper constructs. Its design is long;exp for a repeated
single-arm intervention. 0114 remains cross and review-only/noNewData: skill
and trait age patterns come from separate adult samples,and BFI halves across
occasions do not make this an analysis of within-person developmental change.
It indexes all32 BESSI and15 Big Five facets,plus domains and life-quality
criteria. 0118 is long;exp for a teacher-selected,nonrandomized classroom trial;
its BESSI method follows the paper's stated frequency anchors,with unavailable
administered-form confirmation explicit in evidence. Teacher BRIEF and cognitive
tasks are external outcomes,not extra SEB methods. Innovation is indexed because
it is described/analyzed at baseline despite omission from later models.

No correlation,sample N,eligibility,download date,matrix,pair-N file or extraction
script changed in this group. CSV/XLSX and editable-workbook round-trips,local
app filters and the relevant codebook/workflow/review/reconciliation tests passed.
The results report was rendered again; all six models converged with finite
estimates. Historical manual scripts and user-edited QMD prose remain unchanged.


Tenth review group,2026-11-19:0119 Jayawickreme/Fleeson (dynamic personality
models),0120 Fernandes et al. (Brazilian BESSI/self-informant agreement),0123
Rakhimbekova et al. (Kazakhstani teacher adaptation/profiles),0124 Hopwood
(personality-functioning conceptualizations),and0126 Allemand et al. (change
stages and human/digital support). Construct counts: not applicable,59,21,
not applicable,and6. Coverage is45/51 articles and1024 article/construct links.
No new variable codes were needed; skillprofiles and digitalassistance were
registered as reusable focus topics before coding/import.

0119 and0124 are theory papers: cited findings,instrument names and conceptual
figures do not constitute newly analyzed or quantitatively synthesized scores.
Their empirical construct index and participant/measurement fields are Not
applicable; their substantive arguments remain searchable through focus topics.
0120 indexes all37 BESSI domains/facets,20 Big Five domains/facets,life
satisfaction and self-esteem once across studies,informants and occasions.
Its brief14-day test-retest subset concerns reliability; both components are
classified cross under the substantive-design convention rather than inheriting
long from the repeated administration alone. The previously approved scoring,
invalid-item handling,N1103/N123 and unresolved publication discrepancies remain
unchanged. No author contact is performed.

0123 remains review-only/noValidatedMeasure. Its modified45-item version uses
reverse-phrased adaptations and lacks confirmed equivalence to original facet
scores. The initial21-construct index at the tenth-group checkpoint was
superseded by the user's clarification on2026-11-19: the45-item form covers all
29 non-compound facets,excluding self-reflection,adaptability and capacity for
independence. The current index has five domains and29 facets,with the adapted
format and continued quantitative exclusion explicit in notes/evidence. Earlier
coverage-limit evidence is retained as superseded rather than left unresolved.
No original six-item facet-score equivalence or new matrix is asserted.
Teachers are classified as Workplace; AI/digital-era framing does not establish
measured AI practices or an intervention.

0126 indexes six constructs,including categorical human/digital support
preference analyzed in the paper but absent from the local quantitative subset.
The three-stage change item is indexed once under the existing broad changegoals
code,not duplicated as a second independently measured motivation scale.
BESSI coverage is adaptability only. All1917 approved Part2 participants and
all existing matrix values are retained; Sample1 receives no BESSI matrix.

All correlations,Ns,eligibility,download dates,matrices,pair-N files,extraction
scripts and historical manual inputs are unchanged. Editable workbook/CSV
round-trips,source/hash checks,local app construct/topic/design filtering and
codebook/human-workflow/review-coding tests passed. Results HTML was rebuilt;
all six models converged with finite estimates,and user QMD prose is unchanged.


Eleventh/final expanded-review group,2026-11-19: user-confirmed0123 correction
plus0132 Pellegrino et al. (dual rating),0141 Sharabi (SLD/ADHD academic
adjustment),0144 Feraco et al. (change beliefs/workshop framing),0145 Lubczyk
et al. (cognition/personality/sustainability),0148 Njoten/Feyer (4S framework),
and0150 Napolitano et al. (life-span age gradients). The seven current construct
counts are34,18,9,3,32,not applicable,and37. All51 current articles now have
the five expanded fields,substantive designs and focus topics; the general
workbook contains1136 unique article/construct links. Two base codes were added:
interventioninterest and overall complexproblemsolving,without new matrix
columns or scores. The skillforeducation focus topic captures substantive
educational applications that cannot be represented by achievement outcomes.

0132 indexes domains,external outcomes and similarity beliefs once across
instruments/informants; dual frequency ratings are trait comparisons,not a second
SEB ability method. Existing anxiety/social-status/parent-aggregation corrections
and N598 are retained. 0141 has nine constructs and remains cross; ADHD justifies
the general disabilities topic alongside SLD. Supplied N525 and user-confirmed
sex direction are unchanged. 0144 measures change goals,feasibility beliefs and
hypothetical-workshop interest,not current skill levels. Its design cross;exp
includes randomized vignette framing; no program is delivered and no skill-change
outcome or quantitative matrix is invented.

0145 indexes both studies,including Study1 Big Five predictors and Study2 overall
CPS with its separately analyzed knowledge/performance indicators. BESSI/HEXACO
facets computed for domain scoring and SECS subscale scores available in the
project matrix are not automatically treated as separately analyzed source
outcomes. All existing matrix columns are retained. Different reasoning tests
are indexed under broad fluid intelligence without pooling or computing new
scores. Supplementary source code was inspected statically for the analyzed
attitude subscales and cognitive indicators. 0148 is review;theory: bibliometric
keyword counts and proposed competencies do not constitute a pooled synthesis
of participant skill scores; the analyzed-score index is Not applicable.
0150 indexes all37 BESSI domains/facets,retains cross,and uses normativechange
instead of generic skilldevelopment for its typical age-pattern question.
The approved596 overlap removals and analytic N30719 are unchanged.

All correlations,Ns,eligibility,download dates,matrices,pair-N files,extraction
scripts and historical manual inputs are unchanged. CSV/XLSX/import round-trips,
complete-field/registered-code checks and local app filters passed. Relevant
codebook,historical-revision,human-workflow and review-coding tests passed after
replacing a test assumption that an uncoded production article must still exist
with explicit missing/blank/pending fixtures. Not applicable and Not reported
remain independently tested. Results HTML was rebuilt; all six models converged
with finite estimates and user-edited QMD prose is unchanged.
