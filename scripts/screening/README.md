# Screening title/abstract assistito da AI

## Obiettivo

Questo passaggio massimizza la sensibilità. È preferibile mantenere un record in
più per lo step successivo piuttosto che escludere una pubblicazione
potenzialmente pertinente.

La policy versionata è:

```text
config/screening/title_abstract.yml
```

Le istruzioni AI sono:

```text
prompts/title_abstract_screening.md
```

La skill Codex/ChatGPT versionata nel repository è:

```text
.agents/skills/living-seb-title-abstract/
```

## Regole scientifiche

Sono mantenuti:

- articoli empirici potenzialmente pertinenti;
- review;
- meta-analisi;
- editorial pubblicati in rivista;
- casi incerti.

Sono esclusi automaticamente solo record con evidenza chiara e alta confidenza:

- `BOOK`;
- `OUT_OF_SCOPE`;
- `NON_SEB_FRAMEWORK`;
- `NOT_JOURNAL_PEER_REVIEWED`.

L'assenza della parola `SEB` non è un criterio di esclusione.

## Procedura mensile

### 1. Preparare l'input

Dopo `scripts/dedup/04_update_finalize.R`:

```r
source("scripts/screening/00_setup.R")
source("scripts/screening/01_prepare_title_abstract.R")
```

Vengono creati, sotto
`data/1.abstracting/<batch_id>/title_abstract/`:

```text
<batch_id>_title_abstract_input.csv
<batch_id>_title_abstract_input.xlsx
<batch_id>_title_abstract_ai_request.jsonl
<batch_id>_title_abstract_codex_task.md
```

### 2. Eseguire la skill

In Codex, dalla root del repository, usare la skill:

```text
$living-seb-title-abstract
```

Richiesta consigliata:

```text
Esegui lo screening title/abstract del batch corrente seguendo il task generato
in data/1.abstracting/<batch_id>/title_abstract/. Salva il CSV delle decisioni
nel percorso richiesto e valida la copertura degli ID.
```

La skill deve creare:

```text
<batch_id>_title_abstract_ai_decisions.csv
```

Non è richiesto intervento umano per l'incertezza scientifica: `uncertain` viene
mantenuto automaticamente.

### 3. Finalizzare

```r
source("scripts/screening/02_finalize_title_abstract.R")
```

Output principali:

```text
<batch_id>_title_abstract_decisions.csv
<batch_id>_title_abstract_screening.xlsx
<batch_id>_title_abstract_included.csv
<batch_id>_title_abstract_included.xlsx
<batch_id>_title_abstract_excluded.csv
<batch_id>_title_abstract_excluded.xlsx
<batch_id>_title_abstract_summary.csv
```

`*_included.csv` e `*_included.xlsx` sono l'input dello step successivo.

## Quando il processo si ferma

Il workflow si arresta solo per problemi tecnici/auditabili, per esempio:

- ID mancanti, duplicati o aggiuntivi;
- decisione non ammessa;
- motivo di esclusione non ammesso;
- esclusione senza evidenza;
- versioni di policy/prompt non coerenti.

In questi casi correggere o rigenerare il file delle decisioni. Non è necessario
fare screening umano dell'intero batch.

## Pilot validato 2026-10

Il batch pilota ha prodotto:

```text
14 record in input
6 mantenuti
8 esclusi
0 casi incerti
```

Le 14 decisioni sono state confermate dal ricercatore. La fixture di regressione
è in:

```text
tests/fixtures/title_abstract/2026-10_expected_decisions.csv
```

Per confrontare una nuova esecuzione sul pilot:

```r
source("scripts/screening/03_validate_2026_10_regression.R")
```

## Test unitari

```r
source("scripts/screening/99_run_tests.R")
```
