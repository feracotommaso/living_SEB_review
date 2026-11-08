# Deduplicazione bibliografica mensile

La procedura corrente è nella [guida unica](../../MONTHLY_WORKFLOW.md).
Da `workflow.R` usa `prepara_lotto()`, `conferma_duplicati()` e `promuovi_registro()`.
Le coppie si revisionano nel foglio duplicati dell’Excel unico; le scelte sono salvate
in `human_duplicates.csv`. Le istruzioni sotto documentano anche gli script di compatibilità;
gli Excel separati e le copie intermedie dei registri non vengono generati nel percorso compatto.

## Principio

- Gli export originali Scopus e Web of Science restano immutati.
- Le copie Scopus normalizzate vengono create automaticamente sotto
  `data/0.bib_normalized/`.
- Il registro attivo è `data/0.registry/records_master_full.csv`.
- Il registro viene aggiornato solo con `06_promote_registry.R`.
- CSV è il formato canonico; XLSX è usato per eventuali revisioni umane.

Per segnalazioni degli autori, citation chasing, ricerche manuali o altre fonti
esterne, seguire [la regola comune](../../policies/external_intake.md). Questi
ingressi hanno un manifest con provenienza e checksum e un lotto separato
`YYYY-MM-external-NN`; non vanno rinominati come export della ricerca Scopus/WoS.

## Prima configurazione o ricostruzione completa

Usare questi passaggi una sola volta, oppure dopo un reset completo intenzionale:

```r
source("scripts/dedup/00_setup.R")
source("scripts/dedup/01_history_prepare.R")
# Se presente, compilare history_candidate_pairs_review.xlsx
source("scripts/dedup/02_history_finalize.R")
```

Il file risultante deve esistere:

```text
data/0.registry/records_master_full.csv
```

## Aggiornamento mensile ordinario

### 1. Aggiornare la configurazione

Modificare `config/dedup_config.R`:

```r
batch_id <- "YYYY-MM"          # mese di pubblicazione rappresentato
history_through <- "YYYY-MM"   # mese precedente già nel registro
search_run_date <- "YYYY-MM-DD"
```

Non cambiare retroattivamente le copie versionate del registro.

### 2. Inserire gli export del nuovo mese

Copiare Scopus e Web of Science in:

```text
data/0.bib_download/updates/<batch_id>/
```

I nomi devono contenere `scopus`, oppure `wos`, `web of science` o
`savedrecs`.

### 3. Preparare la deduplicazione

```r
source("scripts/dedup/00_setup.R")
source("scripts/dedup/03_update_prepare.R")
```

Se viene creato:

```text
data/0.deduplication/<batch_id>/update_candidate_pairs_review.xlsx
```

aprire il foglio `review` e compilare `result` con `match` o `no_match`.
Nei casi dubbi usare `no_match`.

### 4. Finalizzare il batch

```r
source("scripts/dedup/04_update_finalize.R")
```

Output principali:

```text
data/1.abstracting/<batch_id>/<batch_id>_new_records.csv
data/1.abstracting/<batch_id>/<batch_id>_new_records.xlsx
data/0.deduplication/<batch_id>/records_master_full_proposed_<batch_id>.csv
```

### 5. Confronto Rayyan, se richiesto

Il pilot 2026-10 ha mostrato concordanza esatta 14/14. Il confronto non è
necessario ogni mese, salvo audit periodici o se
`require_rayyan_comparison <- TRUE`.

Per eseguirlo, inserire una sola esportazione Rayyan in:

```text
data/0.deduplication/<batch_id>/rayyan/
```

poi:

```r
source("scripts/dedup/05_compare_rayyan.R")
```

### 6. Promuovere il registro

Dopo aver controllato conteggi e output:

```r
source("scripts/dedup/06_promote_registry.R")
```

Digitare:

```text
PROMOTE <batch_id>
```

Il registro attivo viene aggiornato e viene creata una copia versionata:

```text
data/0.registry/records_master_full_through_<batch_id>.csv
```

## Reset

Per eliminare solo gli output del batch corrente, preservando lo storico:

```r
source("scripts/dedup/00_reset_current_batch_outputs.R")
```

Il reset completo elimina anche il registro e richiede una ricostruzione dello
storico. Usarlo solo deliberatamente:

```r
source("scripts/dedup/00_reset_full_rebuild.R")
```

## Controlli prima del passaggio allo screening

Verificare che:

- `genuinely_new_records` sia plausibile;
- `proposed_master_records = historical_unique_records + genuinely_new_records`;
- non esistano coppie manuali non risolte;
- il registro proposto sia stato promosso solo dopo i controlli;
- il file `<batch_id>_new_records.csv` esista.

## Errori comuni

### `Historical registry not found`

Il registro non esiste o è stato cancellato. Ricostruirlo con `01` e `02`; non
eseguire `03` senza `records_master_full.csv`.

### Errore del parser BibTeX Scopus

Il workflow rimuove automaticamente il preambolo Scopus dalle copie derivate.
Non modificare gli export originali.

### Excel mostra il CSV in una sola colonna

Aprire il file con **Dati → Da testo/CSV**, delimitatore virgola e codifica
UTF-8, oppure usare direttamente il corrispondente XLSX.
