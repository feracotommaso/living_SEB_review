# Codifica generale

La procedura operativa è nella [guida unica](../../MONTHLY_WORKFLOW.md).
Carica `source("workflow.R")` dalla radice del progetto.

- `prepara_codifica(lotto, data_download)` aggiunge i paper mantenuti.
- `aggiungi_campione()` assegna gli identificativi quantitativi.
- `importa_codifica()` importa le modifiche di `data/3.meta_data/coding/coding.xlsx`.
- `importa_articolo(file_json, lotto)` importa una prima codifica IA nelle tabelle cumulative.
- `anteprima_risultati()` e `aggiorna_risultati()` ricostruiscono i dataset finali.

CSV canonici: `records.csv`, `sources.csv`, `evidence.csv` nella cartella coding.
Gli script 01 e 03 restano strumenti specifici per compatibilità storica e manutenzione dei codebook.
Le esportazioni mensili e i registri di integrazione sono stati consolidati; non vanno ricreati.
