# Installazione dell'overlay 2026-10.3

Questo archivio aggiorna il workflow di deduplicazione e aggiunge lo screening
title/abstract assistito da AI.

## Copia nel repository

Copia nella root di `living_SEB_review` le seguenti cartelle/file, mantenendo la
struttura:

```text
.agents/
config/
prompts/
schemas/
R/
scripts/
tests/
AGENTS.md
MONTHLY_WORKFLOW.md
```

Sovrascrivi i file omonimi del precedente pacchetto di deduplicazione.

Se il repository contiene già un `AGENTS.md`, non sovrascriverlo alla cieca:
integra le sezioni Living SEB del file fornito.

## Non copiare dati grezzi

L'archivio non contiene export Scopus/WoS né registri reali. I dati esistenti
sotto `data/` restano in posizione.

## Verifica iniziale

```r
source("scripts/dedup/00_setup.R")
source("scripts/screening/00_setup.R")
source("scripts/screening/99_run_tests.R")
```

## Skill locale

La skill repository-scoped è già collocata in:

```text
.agents/skills/living-seb-title-abstract/
```

Codex la rileva quando viene avviato dalla root del repository. È disponibile
anche come archivio separato `skill.zip` per installazione/distribuzione.
