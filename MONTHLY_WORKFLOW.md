# Living SEB — guida operativa unica

Aggiornata l'8 novembre 2026. Aprire il progetto RStudio nella radice della repository.
Il punto di ripristino precedente alla semplificazione è il commit `0e1daaa`.

## I file che usi normalmente

| File | Funzione | Modifica manuale |
|---|---|---|
| `workflow.R` | Comandi dei diversi passaggi | Esegui le funzioni dalla console RStudio |
| `data/1.abstracting/<lotto>/<lotto>_screening.xlsx` | Duplicati, abstract, PDF, full text e decisioni umane | Celle gialle nei fogli operativi |
| `data/3.meta_data/coding/coding.xlsx` | Codifica cumulativa di tutti gli articoli, campioni, fonti ed evidenze | Celle gialle; poi importa le modifiche |
| `scripts/extraction/articles/<paper_id>.R` | Selezione del campione e scoring leggibile | Qui correggi item, medie, esclusioni e ricodifiche |
| `data/3.meta_data/last_update.xlsx` | Anteprima delle variazioni e problemi da valutare | Consultazione |

CSV è il formato canonico. Gli Excel sono interfacce: salva e chiudi il file, poi esegui l'importazione R. Nessun aggiornamento deve sovrascrivere modifiche Excel non ancora importate. Identificativi e decisioni derivate sono controllati da R.

Gli output finali restano:

- `data/3.meta_data/meta_data/metadata.csv` e `metadata.xlsx`: dati per le meta-analisi;
- `data/3.meta_data/review_data/review_data.csv` e `review_data.xlsx`: dati della review.

## 1. Caricare un nuovo BibTeX

Per Scopus/WoS usa `data/0.bib_download/updates/YYYY-MM/`. Mantieni nei nomi `scopus`, `wos`, `web of science` o `savedrecs`. Il mese identifica il lotto di pubblicazione. Aggiorna `history_through` e `search_run_date` in `config/dedup_config.R`. Gli originali non vengono modificati; la normalizzazione Scopus usa copie derivate.

Per segnalazioni esterne usa `data/0.bib_download/external/YYYY-MM-external-NN/`, dove il mese è quello di ricezione. Esempio:

`data/0.bib_download/external/2026-11-external-01/2026-11-05_author_suggestion_01.bib`

Registra la provenienza in `config/external/<lotto>.csv`: percorso, tipo di fonte, data di ricezione, data di download se nota, dettaglio della segnalazione, URL, nome originale e SHA256. I tipi ammessi sono in `config/external_source_types.csv`: autori, esperti, citazioni precedenti/successive, ricerca manuale e altre fonti. Gli ingressi esterni passano dalla stessa deduplicazione; una successiva comparsa in Scopus/WoS non deve creare un nuovo paper.

Non dedurre la data di download dal mese di pubblicazione. Tieni distinte le date di bibliografia, PDF e dati individuali; una data ignota resta vuota e dichiarata sconosciuta.

## 2. Deduplicazione e screening abstract

```r
source("workflow.R")
lotto <- "2026-11"  # oppure "2026-11-external-02"
prepara_lotto(lotto)
```

Apri l'Excel di screening. Se compare il foglio `duplicati`, compila `result` con `match` o `no_match`. Le altre colonne identificano le coppie e non vanno cambiate. Poi:

```r
conferma_duplicati(lotto)
promuovi_registro(lotto)
```

La promozione usa lo script protetto esistente e richiede di digitare `PROMOTE <lotto>` in RStudio. Non aggiorna il registro con coppie irrisolte o un confronto Rayyan discordante. Rayyan resta facoltativo, salvo configurazione esplicita. Non ripetere la preparazione della deduplicazione per un lotto già promosso: riprendi dallo screening.

L'input per la skill titolo/abstract è:

`data/1.abstracting/<lotto>/title_abstract/<lotto>_title_abstract_input.csv`

La skill produce `..._title_abstract_ai_decisions.csv`; `raccogli_abstract(lotto)` valida le decisioni e aggiorna l'Excel. Le policy scientifiche restano in `config/screening/title_abstract.yml`.

Puoi intervenire direttamente nel foglio `abstract` compilando `human_decision`, `human_exclusion_reason`, `human_rationale` e `human_evidence`. I valori sono `include`, `exclude`, `uncertain`; l'incertezza è mantenuta automaticamente. Un'esclusione richiede uno dei motivi ammessi e un'evidenza esplicita. Le proposte IA restano conservate nelle loro colonne.

```r
importa_screening(lotto)
```

I risultati canonici sono `..._title_abstract_decisions.csv`; il CSV `..._included.csv` è l'ingresso della fase successiva. Un lotto senza articoli mantenuti si conclude qui.

## 3. PDF e full text

```r
prepara_full_text(lotto)
```

R assegna o riusa i paper ID e mostra nel foglio `pdf` quali file mancano. Salva il PDF principale in `data/2.full_abstracting/pdfs/<paper_id>.pdf`, e gli eventuali supplementi con lo stesso prefisso. R rieseguito aggiorna la disponibilità senza cambiare gli ID.

La skill full text legge i PDF completi, applica le policy e salva le valutazioni strutturate in `data/2.full_abstracting/ai_decisions/<lotto>/`. Questi JSON conservano la valutazione originale e le evidenze; non sono la codifica dei campioni.

```r
raccogli_full_text(lotto)
```

Nel medesimo Excel trovi decisioni ed evidenze nel foglio `full_text` e gli input umani in `decisioni_full_text`. Per i casi da decidere compila i campi `human_*`, la motivazione, chi ha deciso, la data e `adjudication_status = RESOLVED`; quindi esegui `importa_screening(lotto)`.

Per correggere una decisione già risolta:

```r
rivedi_full_text(lotto, "0150", "Motivo della rivalutazione")
```

R riapre il caso e precompila i valori precedenti. Completa la correzione nell'Excel e importa. Non usare questo comando per cambiare un punteggio o una data: quelle modifiche appartengono alla codifica o allo scoring.

PDF mancanti impediscono la valutazione, mentre dati individuali non accessibili non costituiscono una nuova ragione di esclusione. Review e meta-eleggibilità restano valutate separatamente.

## 4. Codifica generale

```r
prepara_codifica(lotto, data_download = "2026-12-02") # esempio: usare la data realmente nota
```

Ometti `data_download` se sconosciuta. La funzione aggiunge gli articoli mantenuti alla codifica generale, precompilando i dati bibliografici. Nel file `coding.xlsx`:

- `articoli`: titolo, autori, date, topic e informazioni comuni;
- `campioni`: campioni quantitativi, n, descrittive, misure e note scientifiche;
- `fonti`: originali, URL, date, checksum e riferimenti storici;
- `evidenze`: pagine, campi, motivazioni e limiti della codifica;
- `dizionario`: definizione dei 31 campi della compatibilità analitica.

Le note dei review-only si modificano in `articoli`; quelle quantitative in `campioni`. I campioni dei nuovi review-only non vengono codificati. Le informazioni review-only già presenti nello storico restano nel CSV per mantenere la compatibilità, senza nuove estrazioni.

Per un nuovo campione quantitativo:

```r
aggiungi_campione("0150", componente = "Main study", campione = "Non-overlapping participants")
```

Questo è solo un esempio: 0150 è già assegnato e non va aggiunto di nuovo. R genera gli ID e conserva quelli esistenti. Più campioni ricevono lettere diverse. Le raccolte successive restano fuori dalla matrice corrente; per ora `occasion_role` è `first_eligible`.

Puoi compilare direttamente l'Excel. L'IA può invece fornire un JSON conforme allo schema di codifica, importabile con `importa_articolo(file_json, lotto)`. Dopo l'importazione, le informazioni accettate sono nei CSV cumulativi; non occorrono cartelle JSON mensili da mantenere allineate.

Salva, chiudi e importa:

```r
importa_codifica()
```

Per correzioni scrivi una breve motivazione in `change_note`. I CSV cumulativi sono `coding/records.csv`, `coding/sources.csv` e `coding/evidence.csv`. Non devi aggiornarli a mano insieme all'Excel. I riferimenti a vecchi file derivati indicano il checkpoint Git che ne conserva il contenuto.

## 5. Dati originali, codebook e scoring

Cerca sempre i dati individuali originali. Salvali immutati in `data/3.meta_data/open_data/raw/<paper_id>/` e registrali nel foglio `fonti`. Le matrici pubblicate sono confronti o fallback motivati, dopo aver verificato disponibilità e usabilità dei dati individuali.

Lo script `scripts/extraction/articles/<paper_id>.R` mostra selezione, ricodifiche, item, pesi, medie, gestione dei mancanti e correlazioni. Le chiavi lunghe possono stare in `scripts/extraction/keys/`. Il modello comune è in `scripts/extraction/templates/article_extraction.R`.

Le regole scientifiche restano in `policies/extraction_conventions.md`: medie degli item, SEB/BESSI sulla metrica 1–5, conversione documentata del dual rating, età nei dati individuali e descrittivi ma non nelle matrici, sesso/genere opportunamente codificato nelle matrici. Nessuna correzione automatica delle correlazioni osservate.

Per nuove variabili o topic, modifica gli Excel `data/matrix_codebook.xlsx` e `data/topics_codebook.xlsx`, mantenendo i codici già usati e documentando le aggiunte, poi esegui `importa_codebook()`. I suffissi ammessi restano in `config/coding/variable_suffixes.csv`. Riusa i topic esistenti quando adeguati; aggiungine per differenze sostanziali.

```r
estrai_articolo("0150", verifica_soltanto = TRUE)
estrai_articolo("0150")
```

Il primo comando verifica senza esportare; il secondo salva:

| Output | Percorso |
|---|---|
| Dati individuali codificati, privati | `data/3.meta_data/open_data/individual_data/<matrix_id>.csv` |
| Matrice CSV e XLSX | `data/3.meta_data/matrices/<matrix_id>.*` |
| Numerosità delle correlazioni | `data/3.meta_data/pair_n/<matrix_id>.csv` |
| Controlli aggregati di tutti i nuovi campioni | `data/3.meta_data/extraction/summary.csv` |

Il file individuale non contiene identificativi personali. `open_data/` resta esclusa da Git. La matrice è già nella cartella comune; i dataset finali si aggiornano nel passaggio seguente. Per una matrice estratta da una tabella pubblicata conserva la trascrizione e la fonte e usa uno script articolo dedicato alla lettura e validazione; non inventare dati individuali né N per coppia non riportati.

## 6. Anteprima e aggiornamento finale

```r
anteprima_risultati()
```

Apri `data/3.meta_data/last_update.xlsx`. Mostra conteggi, variazioni prima/dopo e anomalie: matrici mancanti, N diversi per correlazione, autovalori negativi e nomi storici da verificare. Una segnalazione non modifica i dati automaticamente.

```r
aggiorna_risultati()
```

Se cambiano N o correlazioni già presenti, verifica l'anteprima e indica il motivo:

```r
aggiorna_risultati(motivo = "Corretto lo scoring secondo la fonte verificata")
```

I nuovi N derivati dai dati individuali vengono letti dai controlli dell'estrazione. R salva insieme CSV e XLSX dei risultati. Un file bloccato da Excel impedisce il salvataggio: chiudilo e ripeti. Non serve compilare manifest o firme tecniche.

## Correggere uno studio precedente

| Problema | Intervento | Passaggio successivo |
|---|---|---|
| Topic, assessment, paese, note o data | Modifica `coding.xlsx`, aggiungi `change_note` | `importa_codifica()`, anteprima, aggiornamento |
| Item invertito, media, esclusione o ricodifica | Correggi lo script dell'articolo | Riesegui lo scoring, anteprima, aggiornamento con motivo |
| Correlazione trascritta male | Correggi la trascrizione collegata alla fonte e riesegui lo script | Anteprima, aggiornamento con motivo |
| Decisione di eleggibilità | Riapri il caso nel lotto full text | Importa screening, poi `prepara_codifica(lotto)` e aggiornamento |

Gli script storici, incluso `data/3.1opendata_to_cor.R`, restano conservati. Per quei paper continua a usare il relativo blocco per ricalcolare la matrice. La codifica corrente si corregge nell'Excel generale. Il normale aggiornamento finale ora passa da `workflow.R`; il vecchio `data/3.2cor_to_metadata.R` resta disponibile per compatibilità e conserva i suoi controlli storici.

## Verifiche, Git, paper e app

La migrazione è stata confrontata con il checkpoint: 39 campioni e 51 articoli con gli stessi valori. La prova 0150 ha riprodotto gli stessi CSV individuali e della matrice: N=30719, 38 variabili, N per coppia 29244–30719, dopo l'esclusione dei 596 partecipanti sovrapposti. La matrice conserva il piccolo autovalore negativo già documentato.

Restano visibili le lacune storiche 0050, 0053a e 0053b; 0104a resta esclusa secondo la decisione già registrata. Questi casi non sono stati ricodificati durante la semplificazione.

Dopo modifiche al workflow:

```r
system2(file.path(R.home("bin"), "Rscript.exe"), "scripts/screening/99_run_tests.R")
system2(file.path(R.home("bin"), "Rscript.exe"), "scripts/screening/03_validate_2026_10_regression.R")
```

Il secondo comando confronta i 14 esiti del pilot di ottobre con quelli validati; non è una regressione statistica. Prima di commit/push controlla le differenze e conserva localmente gli originali privati, che Git non include.

Per aggiornare il report dei risultati dopo i dataset, dalla radice della repository:

```sh
quarto render paper/livingSEBresults.qmd --to html --execute
```

Apri `paper/livingSEBresults.html` e controlla provenienza dei dati, conteggi, diagramma e risultati. Il report legge gli XLSX locali; `R/report_inputs.R` combina gli export storici e i CSV canonici senza ricontare ottobre. Le analisi vengono rieseguite e i modelli OSMASEM devono superare i controlli numerici. Il divario storico tra registro e screening documentato (20 record al 9 novembre 2026) resta esplicito, senza attribuirgli esclusioni non dimostrate. L'aggiornamento del report non modifica i dataset.

L'app, anche avviata localmente, legge gli XLSX e i codebook da GitHub `main`: un push sulla branch di lavoro non aggiorna l'app. Bibliografia, rendering del paper e pubblicazione dell'app restano passaggi espliciti separati dall'aggiornamento dei dati.
