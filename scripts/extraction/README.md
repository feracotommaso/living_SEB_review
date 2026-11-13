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

`articles/0013.R`, `0018.R` e `0120.R` sono verifiche in sola lettura dei dati
scientifici: non esportano matrici o dati individuali e non aggiornano la codifica.
Un'esecuzione normale salva soltanto il riepilogo aggregato unico
`data/3.meta_data/extraction/historical_audit.csv`. Con `--check-only` non salva
neppure quello. Esempio dalla radice:

```sh
Rscript scripts/extraction/articles/0018.R
Rscript scripts/extraction/articles/0120.R --check-only
```

Le matrici candidate e gli N per coppia restano in memoria. Gli originali sono
controllati tramite checksum; la loro data di acquisizione non viene inventata.
`keys/bessi192.R` contiene soltanto la chiave ufficiale e i pesi del BESSI-192.
In 0120 il riepilogo distingue riproduzione dei punteggi forniti dagli autori e
analisi di sensibilità non approvate: pesi ufficiali e risposta fuori scala
trattata come mancante. Nessuna di queste alternative va promossa prima della
revisione scientifica. Le tre verifiche non costituiscono ancora una ricodifica
completa di tutte le misure degli articoli.
