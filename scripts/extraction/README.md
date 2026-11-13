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
