# Living SEB: workflow mensile

## Sequenza ordinaria

```text
1. Aggiornare config/dedup_config.R
2. Inserire export Scopus/WoS del nuovo mese
3. Deduplicare e produrre i nuovi record
4. Promuovere il registro bibliografico
5. Preparare lo screening title/abstract
6. Eseguire la skill AI
7. Finalizzare lo screening
8. Usare il file included per lo step successivo
9. Controllare e versionare gli output
```

## Comandi R

```r
source("scripts/dedup/00_setup.R")
source("scripts/dedup/03_update_prepare.R")
# revisionare update_candidate_pairs_review.xlsx solo se esiste
source("scripts/dedup/04_update_finalize.R")
# Rayyan è opzionale salvo require_rayyan_comparison = TRUE
source("scripts/dedup/06_promote_registry.R")

source("scripts/screening/00_setup.R")
source("scripts/screening/01_prepare_title_abstract.R")
# eseguire $living-seb-title-abstract in Codex
source("scripts/screening/02_finalize_title_abstract.R")
```

## Output che alimenta lo step successivo

```text
data/1.abstracting/<batch_id>/title_abstract/
<batch_id>_title_abstract_included.xlsx
```

Il CSV con lo stesso basename è la versione canonica machine-readable.

## Documentazione dettagliata

- Deduplicazione: `scripts/dedup/README.md`
- Screening title/abstract: `scripts/screening/README.md`
