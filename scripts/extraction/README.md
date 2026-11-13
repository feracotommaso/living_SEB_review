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
N di riferimento 840 è mantenuto e la nota approvata è nella codifica generale,
nei metadata e nella review. Chiedere agli autori come individuare il campione
analitico. Nessuna selezione arbitraria dei record o modifica delle correlazioni.

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

Verifiche in sola lettura degli output scientifici: quattro matrici e tutte le
3629 correlazioni storiche riprodotte. Una normale esecuzione aggiorna soltanto
il riepilogo unico; `--check-only` non scrive nulla. Nessuna proposta di questo
gruppo è integrata in matrici, codifica o metadata.

| Matrice | N di riferimento | N per coppia | Punto da discutere |
|---|---:|---:|---|
| 0048a | 897 | 428–840 | Stesso campione di 0013, con una matrice più ampia. |
| 0049a | 137 | 128–136 | Omissione di `bf_emotionalvolatility` per un nome non registrato. |
| 0050a | 940 | 631–791 | Domini costruiti solo sulle faccette principali; dipendenza con Study 3 dichiarata dal paper. |
| 0050b | 767 | 469–755 | Scoring di otto faccette da chiarire; campione e misure distribuiti su più occasioni. |

**0013/0048:** i 897 identificativi Case coincidono, come tutti i 20 punteggi
con nomi identici e la loro mancanza di dati. Dopo aver uniformato la direzione
di neuroticismo, anche tutte le 210 correlazioni comuni coincidono. Questi
coefficienti non costituiscono evidenze indipendenti. Anche i metodi di 0013
(p.3) dichiarano che i dati erano già stati analizzati da Soto e colleghi.
Proposta: usare la matrice
più ampia di 0048 per le analisi, conservando 0013 come storico e mantenendo
entrambi gli articoli nella review. L'utente ha chiesto di discutere prima:
**nessun cambiamento dell'inclusione è applicato**. Resta da chiarire l'N di
riferimento (840 contro 897), che non coincide con l'N di ogni correlazione.
I dati 0048 hanno 504 donne, 387 uomini e sei mancanti: le percentuali del paper
sono diverse. La versione e la codifica demografica originarie richiedono verifica;
non sono ricodificati dati sulla sola base delle percentuali pubblicate.

**0049:** ricostruite sei faccette BESSI, tre compositi a due faccette, le scale
BFI focali e le medie dei giudici. Il vecchio nome `emotionalvolatility` viene
scartato dal codebook; il nome registrato è `bf_emotionalvolatility`. Recuperarlo
aggiungerebbe 24 correlazioni senza modificare le 276 attuali. Nove coefficienti
di riferimento osservati, letti nelle Tabelle 3–5, coincidono entro l'arrotondamento,
inclusi quelli della faccetta omessa. L'età è rimossa dal file per anonimizzazione.
Gli altri item BFI disponibili e le autovalutazioni della performance non sono
aggiunti: appartengono all'ampliamento della codifica da progettare separatamente.

**0050:** tutti i 32 punteggi di faccetta T4 sono ricostruibili dagli item
rilasciati. Per Study 3, 24 faccette coincidono e otto no: dettaglio,
organizzazione, responsabilità, pensiero astratto, creatività, espressività,
persuasione e autoriflessione. Sono 2954 valori diversi, con scarto massimo 1.5;
la corrispondenza fra item rilasciati, traduzioni riviste e punteggi forniti va
chiarita. I punteggi forniti restano conservati. Le 20 scale BFI coincidono con
le medie dei valori già ricodificati; i totali cognitivi sono verificati nei
607 casi ammessi dagli autori. Non si reintegrano i 120 casi con diverso
protocollo temporale. Lo scoring delle risposte originarie BFI non è ricostruito.

I vecchi domini 0050 usano nove/quattro faccette principali con regola completa.
La sensibilità con pesi ufficiali 1:0.5 e tutte le faccette costituenti richieste
cambia 175 e 290 correlazioni (scarti massimi circa 0.121 e 0.105). In 0050a,
gli N minimi scendono da 631 a 482: il disegno con mancanza pianificata richiede
una scelta esplicita sulla copertura prima di esportare nuovi domini.
0050b ha 768 righe contro N=767 dichiarato; solo 766 righe hanno almeno due
punteggi selezionati. Nessuna selezione arbitraria è applicata.

Il paper (p.9) dichiara 238 adulti ricontattati, 203 con dati validi a T2 e T4.
L'Rdata contiene 203 record appaiati, ma non una corrispondenza diretta degli
identificativi con il campione T4 esportato. Non si presume indipendenza sulla
base dei diversi identificativi e non si escludono record senza un collegamento
verificato. Study 3 combina BESSI T2, cognizione T1 e item BFI suddivisi fra T0/T2:
questa separazione temporale è distinta dall'inclusione di retest come nuove matrici.

Conteggio aggiornato: 13 articoli storici e 17 matrici passati in verifica;
includendo 0145 e 0150, 15 articoli su 31 e 19 matrici su 39. Restano 16 articoli
e 20 matrici. Sono conteggi di articoli/matrici esaminati, non di campioni
indipendenti né di tutte le questioni scientifiche già risolte.
