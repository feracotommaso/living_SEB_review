# Living SEB — guida operativa unica

Aggiornata al **5 novembre 2026**. Descrive il percorso attualmente eseguito,
gli input, gli output e i punti di intervento manuale. Le policy scientifiche
e gli schemi indicati restano i contratti usati dai programmi.

Aprire `living_SEB_review.Rproj` in RStudio. Tutti i percorsi della guida sono
relativi alla cartella del progetto:

```text
C:\Users\tfera\OneDrive - Università degli Studi di Padova\BESSI\living_SEB_review\
```

## La mappa: cosa succede e dove guardare

| Fase | Dati usati | Risultato principale |
|---|---|---|
| 1. Ingresso | BibTeX/RIS e provenienza | lotto identificato e originali conservati |
| 2. Deduplicazione | originali + registro bibliografico | nuovi riferimenti e registro proposto |
| 3. Titolo/abstract | nuovi riferimenti | elenco dei mantenuti |
| 4. Full text | mantenuti + PDF | paper ID, decisioni ed eventuali adjudicazioni |
| 5. Codifica | paper eleggibili e fonti | JSON scientifico, tabelle di codifica, sample/matrix ID |
| 6. Acquisizione/scoring | dati originali e regole della misura | individuali codificati, matrice e verifiche |
| 7. Integrazione | codifica e matrici validate | **metadata e review_data locali aggiornati** |
| 8. Distribuzione | file locali verificati | commit/push; poi eventuali merge, render e aggiornamento app |

**R** importa, assegna ID, valida, unisce ed esporta. **Codex/ricercatore**
leggono le fonti e formulano le decisioni scientifiche. Per ogni nuovo articolo
lo scoring richiede codice R adattato e verificato: non è una conversione
universale automatica di qualsiasi file di dati.

I due file finali da consultare sono:

- **`data/3.meta_data/meta_data/metadata.xlsx`** e relativo `metadata.csv`:
  una riga per matrice quantitativa utilizzabile;
- **`data/3.meta_data/review_data/review_data.xlsx`** e relativo `review_data.csv`:
  una riga per paper.

Una matrice generata, uno screening finito o un Excel di codifica salvato **non
significano ancora che questi due file siano stati aggiornati**: serve la fase 7.

## 1. Caricare il BibTeX e selezionare il lotto

### Ricerca mensile Scopus/WoS

Inserire gli originali in `data/0.bib_download/updates/<lotto>/`. I nomi devono
contenere `scopus` oppure `wos`, `web of science` o `savedrecs`.

In `config/dedup_config.R` modificare `batch_id` (mese di pubblicazione coperto),
`history_through` (ultimo mese DB consolidato) e `search_run_date` (data effettiva
della ricerca/esportazione). Poi, nella **Console di RStudio**:

```r
Sys.unsetenv("LIVING_SEB_BATCH_ID")
source("config/dedup_config.R")
lotto <- batch_id
```

### Segnalazioni esterne, di qualsiasi origine

Usare `data/0.bib_download/external/<lotto>/`, con lotto
`YYYY-MM-external-NN`: mese di ingresso nel progetto e progressivo mensile da
`01`. La pubblicazione mantiene la propria data nei metadati.

Nome del file: `YYYY-MM-DD_<fonte>_NN.bib`, con data di ingresso, progressivo
per data/fonte e una sola estensione; è ammesso anche `.ris`.

| Origine | Codice della fonte |
|---|---|
| Autori | `author_suggestion` |
| Colleghi o altre persone | `expert_suggestion` |
| Bibliografia di un paper | `backward_citation` |
| Articoli che citano un paper | `forward_citation` |
| Ricerca manuale, sito, rivista, alert | `manual_search` |
| Altro, da descrivere | `other` |

**Intervento manuale:** registrare una riga per file in
`config/external/<lotto>.csv`, con colonne `path`, `source_type`,
`received_date`, `download_date`, `source_detail`, `source_url`,
`original_filename`, `sha256`. La data di download ignota resta vuota.
Specificare anche la fonte dell'export, se diversa dalla modalità di scoperta;
per citation chasing indicare il paper di partenza. Il checksum si calcola con
`digest::digest(percorso_del_file, algo = "sha256", file = TRUE)`.

Dopo aver predisposto file e manifest:

```r
lotto <- "2026-11-external-02" # esempio nuovo: il lotto 01 è già stato lavorato
Sys.setenv(LIVING_SEB_BATCH_ID = lotto)
source("config/dedup_config.R")
```

`R/external_intake.R` valida fonte, date, percorso e checksum. Il selettore è
locale alla sessione/processo: cancellarlo con `Sys.unsetenv("LIVING_SEB_BATCH_ID")`
prima della successiva ricerca DB. Non aggiungere nuovi riferimenti a un lotto
già finalizzato: aprirne un altro. Se si dispone soltanto di DOI/link/PDF,
acquisire prima metadati verificabili e documentarne la fonte.

**Per entrambi gli ingressi:** originali immutati e Git-ignored; manifest,
registri e codice versionati. Data di ingresso, download bibliografico,
pubblicazione e download dei dati individuali sono informazioni distinte.

### Come eseguire gli altri comandi

I blocchi `r` seguenti vanno nella **Console di RStudio**, dalla root.
Preparare una volta l'eseguibile per gli script che richiedono argomenti:

```r
rscript <- file.path(R.home("bin"),
  if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
stopifnot(file.exists(rscript))
```

`source()` usa la sessione corrente; `system2(rscript, ...)` lancia un processo
R con gli argomenti indicati. Fermarsi se un comando termina con errore/codice
diverso da zero.

## 2. Deduplicare e promuovere il registro

**Input:** file del lotto e `data/0.registry/records_master_full.csv`.

```r
source("scripts/dedup/00_setup.R")
source("scripts/dedup/03_update_prepare.R")
```

Si creano manifest, risultati e controlli sotto
`data/0.deduplication/<lotto>/`. La normalizzazione Scopus riguarda solo le
copie derivate in `data/0.bib_normalized/`.

**Intervento manuale:** se esiste `update_candidate_pairs_review.xlsx`,
compilare `result` nel foglio `review` con `match` o `no_match`; nei dubbi
`no_match`. Salvare/chiudere Excel e applicare le risposte con:

```r
source("scripts/dedup/04_update_finalize.R")
```

**Output:** `data/1.abstracting/<lotto>/<lotto>_new_records.csv/.xlsx` e
`records_master_full_proposed_<lotto>.csv` nella cartella di deduplicazione.
Controllare i conteggi in `update_finalize_summary.csv`. Un duplicato conserva
le provenienze aggiuntive, senza diventare un nuovo paper.

Rayyan è facoltativo salvo configurazione: export nella sottocartella `rayyan/`,
poi `source("scripts/dedup/05_compare_rayyan.R")`. Eventuali discrepanze presenti
bloccano la promozione.

**Solo dopo i controlli, aggiornare il registro attivo:**

```r
source("scripts/dedup/06_promote_registry.R")
```

Digitare `PROMOTE <lotto>` quando richiesto. Lo script protetto aggiorna
`records_master_full.csv` e crea `records_master_full_through_<lotto>.csv`
più il log. Il registro bibliografico conserva anche i riferimenti poi esclusi.

Se `genuinely_new_records = 0`, non serve altro screening. Dopo la promozione,
**riprendere dai file già prodotti, non ricominciare la deduplicazione**:
quei riferimenti risultano ormai presenti nel registro.

## 3. Screening titolo/abstract

**Input:** `<lotto>_new_records.csv`. Policy:
`config/screening/title_abstract.yml`; istruzioni:
`prompts/title_abstract_screening.md`.

```r
source("scripts/screening/00_setup.R")
source("scripts/screening/01_prepare_title_abstract.R")
```

In `data/1.abstracting/<lotto>/title_abstract/` vengono creati input CSV/XLSX,
richiesta JSONL e task Codex. Chiedere a Codex di usare
`$living-seb-title-abstract` per il lotto. L'IA produce
`<lotto>_title_abstract_ai_decisions.csv`, una riga per ID.

**Intervento manuale:** correggere questo CSV mantenendo colonne e
`screening_record_id`. Decisioni: `include`, `exclude`, `uncertain`; le
esclusioni richiedono motivo/evidenza/confidenza. Le incertezze sono mantenute;
review, meta-analisi ed editorial di rivista sono trattenuti.
Se correggi una valutazione manualmente, conserva la precedente e annota
autore/motivo della correzione, distinguendola dalla valutazione IA originaria.

```r
source("scripts/screening/02_finalize_title_abstract.R")
```

R valida e produce decisioni, riepilogo, workbook e
`*_included.csv/.xlsx` / `*_excluded.csv/.xlsx`. L'input del full text è
**`*_title_abstract_included.csv`**. Una modifica a un Excel derivato senza
finalizzazione non cambia quel CSV. Se nessun riferimento rimane, fermarsi qui.

## 4. Paper ID, PDF e screening full text

```r
system2(rscript, c("scripts/full_text/01_assign_paper_ids.R", lotto))
```

Usa il CSV incluso, assegna/riutilizza gli ID nel registro
`data/2.full_abstracting/paper_id_registry.csv` e genera
`<lotto>_full_text_manifest.csv/.xlsx`. `paper_id_aliases.csv` conserva gli alias.

**Intervento manuale:** acquisire i PDF mancanti come
`data/2.full_abstracting/pdfs/<paper_id>.pdf`; supplementi con lo stesso
prefisso, per esempio `0150_supplement.pdf`. Verificare DOI/titolo e rieseguire
l'assegnazione per aggiornare la disponibilità. Il manifest serve
all'acquisizione, non alla decisione. Un PDF mancante/illeggibile blocca la lettura.

La skill `$living-seb-full-text` legge articolo completo e supplementi,
applicando `policies/full_text_eligibility_v2.md`, le configurazioni in
`config/screening/` e `prompts/full_text_eligibility.md`. Produce
`data/2.full_abstracting/ai_decisions/<lotto>/<paper_id>.json` con decisioni,
evidenze/pagine e componenti quantitativi eleggibili. Non assegna ancora campioni.

```r
system2(rscript, c("scripts/full_text/02_build_full_text_decisions.R", lotto))
```

**Output:** `<lotto>_full_text_decisions.csv/.xlsx` e
`<lotto>_full_text_human_review.csv`. Il workbook contiene `summary`,
`decisions`, `human_review`, `codebook`.

**Intervento manuale:** solo nel foglio `human_review` modificare i campi
`human_*` e `adjudication_note`, `adjudicated_by`, `adjudication_date`,
`adjudication_status`. Inserire decisioni pertinenti, motivazione e stato
`RESOLVED`; non cambiare `decisions`, ID/evidenze IA né eliminare casi risolti.
Salvare/chiudere Excel, poi:

```r
system2(rscript, c("scripts/full_text/03_finalize_full_text_decisions.R", lotto))
```

Questo sincronizza la decisione umana nei CSV. Eseguirlo prima di ricostruire
il workbook se ci sono modifiche manuali non ancora finalizzate.
Questa modalità riguarda le righe già presenti nella coda: non aggiungerne
arbitrariamente. Per riesaminare un caso già risolto senza riga in coda serve
una rivalutazione esplicita, conservando JSON/evidenze e decisioni precedenti.

| Review | Stato meta | Destinazione |
|---|---|---|
| sì | eligible | `both` |
| sì | ineligible | `review` |
| no | eligible | `meta` |
| no | ineligible | `exclude` |

Review e meta sono indipendenti. Dati da recuperare danno `pending`, non
esclusione; dubbi scientifici richiedono giudizio. `pending`/`uncertain` o
adjudicazioni aperte non hanno destinazione finale. Quando tutto è risolto:

```r
system2(rscript, c("scripts/full_text/04_export_legacy_routing.R", lotto))
```

Si crea `<lotto>_legacy_routing.csv`. **È eleggibilità, non codifica completa:
non aggiorna i dataset finali.**

## 5. Codificare paper, campioni e date

**Fonte dei nuovi record:**
`data/3.meta_data/coding/artifacts/<lotto>/<paper_id>.json`.

Codex/ricercatore compila `article` (bibliografia, topics, date, note…),
`samples` (solo campioni quantitativi eleggibili) ed `evidence` (fonti,
checksum, pagine/motivazioni). Il dizionario dei 31 campi è
`config/coding/legacy_31_fields.csv`, anche in `data/coding_dictionary.xlsx`.
`additional` resta vuoto per i nuovi record.

**Intervento manuale:** correggere questo JSON e le evidenze, non i finali.
Conservare la versione precedente prima di cambiamenti scientifici sostanziali.
Il JSON di `0150` nel lotto esterno 01 è un esempio di struttura, non una fonte
di valori/ID da copiare. Una data sconosciuta rimane mancante. Per una data
bibliografica unica e confermata nel lotto, dopo aver creato i JSON:

```r
system2(rscript, c("scripts/coding/05_set_download_date.R", lotto, "2026-11-05"))
# La data sopra è un esempio: sostituirla con quella effettiva.
```

Questo registra la provenienza in `coding/provenance/`, aggiorna i JSON e
conserva le versioni precedenti. Se le date differiscono tra paper, mantenerle
individualmente: non applicare una data comune. Quindi:

```r
system2(rscript, c("scripts/coding/02_build_article_coding.R", lotto))
```

**Output** sotto `data/3.meta_data/coding/<lotto>/`: `papers.csv`, `samples.csv`,
`evidence.csv`, `legacy_export.csv` e `<lotto>_article_coding.xlsx`.
Il builder valida/esporta; non estrae autonomamente i contenuti del PDF.
Assegna i campioni in `coding/sample_id_registry.csv`.

**Le fasi 5–7 possono richiedere due passate.** Se n/composizione dipendono dai
dati, acquisire e ispezionare prima gli originali della fase 6. Il primo build
assegna gli ID necessari allo script; dopo lo scoring si aggiorna il JSON con
i risultati verificati e si ricostruisce il coding prima dell'integrazione.

| ID | Esempio | Assegnazione |
|---|---|---|
| Lotto | `2026-11-external-01` | ingresso |
| Screening | `seb_d694f460184d4b18` | preparazione titolo/abstract; poi preservato |
| Paper | `0150` | prima del full text; persistente anche se escluso |
| Sample | `a` | codifica del campione quantitativo eleggibile |
| Matrix | `0150a` | paper + sample, nel registro |

Non rinumerare manualmente. I review-only non hanno campioni quantitativi;
il loro `matrix_id = paper_id` nell'export compatibile è un segnaposto,
non una matrice. Se è eleggibile solo Study 2, non creare un campione vuoto
per Study 1.

## 6. Dati originali, codebook e script dell'articolo

Per i contributi quantitativi, cercare prima i dati individuali, anche se il paper contiene una matrice.
Conservare dati/codice/README originali in
`data/3.meta_data/open_data/raw/<paper_id>/`; una nuova versione non sostituisce
silenziosamente quella precedente. Ispezionare il codice degli autori prima di
adattarlo. Provenienza, date note/ignote e hash sono nel manifest del paper,
normalmente `data/3.meta_data/extraction/<paper_id>/source_manifest.csv`.

**Intervento manuale:** fornire file mancanti e chiarire scoring, missing,
esclusioni e sovrapposizioni. Senza originali utilizzabili, documentare i tentativi
e predisporre un fallback da matrice pubblicata con R e verifiche dedicate:
non c'è un importatore universale per ogni formato.

I codebook canonici sono `data/matrix_codebook.csv` e
`data/topics_codebook.csv`, con XLSX di consultazione. I suffissi registrati
sono in `config/coding/variable_suffixes.csv`, per esempio `other`/`iat`.

**Intervento manuale:** riusare codici adeguati; proporre nuove variabili/topics
quando servono differenze sostanziali. Le proposte hanno le colonne del codebook
più `paper_id,change_date,reason,source` e una definizione (`notes` per variabili,
`Explanation` per topics). Non cambiare soltanto l'XLSX.

```r
system2(rscript, c("scripts/coding/03_update_codebooks.R")) # verifica
# Solo se servono aggiunte; usare le opzioni pertinenti:
system2(rscript, c("scripts/coding/03_update_codebooks.R",
  "--add-variables", "percorso/variable_additions.csv",
  "--add-topics", "percorso/topic_additions.csv"))
```

Il comando sincronizza CSV/XLSX e registra `codebook_changes.csv`.
Non ridefinisce codici né ricodifica automaticamente gli articoli precedenti.
Aggiornare anche il JSON del paper se cambiano i suoi topics.

## 7. Calcolare i punteggi ed esportare la matrice provvisoria

Questa fase riguarda solo i campioni quantitativi eleggibili. Per i review-only,
completare codifica/topics e passare all'integrazione senza creare matrici.

Il codice specifico è `scripts/extraction/articles/<paper_id>.R`, basato su
`scripts/extraction/templates/article_extraction.R`. Chiavi lunghe:
`scripts/extraction/keys/<paper_id>.R`. Verifiche/esportazioni comuni:
`R/extraction_helpers.R`. Esempio, da sostituire con l'articolo da lavorare:

```r
system2(rscript, c("scripts/extraction/articles/0150.R", "--check-only"))
system2(rscript, c("scripts/extraction/articles/0150.R"))
```

Il primo verifica senza esportare; il secondo scrive. Entrambi controllano
fonti e ID, applicano le regole scientifiche e verificano i risultati.
**Intervento manuale:** correggere script/chiavi e motivazioni, non le celle
della matrice.

Regole attuali: medie degli item dopo i reverse; SEB/BESSI su 1–5, dual rating
1–9 convertito con `(score + 1) / 2`; pesi/missing espliciti; Pearson zero-order
con coppie disponibili. Età solo individuale. Genere/sesso con codifica
documentata, senza forzare altre categorie nel contrasto binario o eliminarle
dalle altre correlazioni. Per longitudinali si usa la prima occasione eleggibile.

**Output sotto `data/3.meta_data/`:**

- `open_data/individual_data/<matrix_id>.csv`: individuali codificati, locali
  e Git-ignored; eventuale XLSX aggiuntivo;
- `extraction/<paper_id>/<matrix_id>/matrices/`: matrici CSV/XLSX in
  **staging**, cioè pronte per la verifica ma non ancora integrate;
- nella cartella del campione: `pair_correlations.csv` (r e n per coppia),
  `checks.csv`, `score_summary.csv`, `variable_map.csv`,
  `scientific_decisions.csv` ed `extraction_manifest.csv` (hash).

Il pilot 0145 conserva l'audit direttamente in `extraction/0145/`.
Documentare eventuale non positività della matrice senza ripararla
automaticamente. Aggiornare il JSON con n, composizione, esclusioni e note
effettive; rifare il build coding della fase 5.

**Limite attuale:** metaSEM riceve un n di riferimento per matrice. Gli n per
coppia restano negli audit e le differenze nelle note; le note non correggono
automaticamente i pesi statistici.

## 8. Integrare: qui si aggiornano i file finali

**Input:** coding aggiornato, registro campioni, matrici e audit validati.

```r
system2(rscript, c("scripts/coding/04_integrate_batch.R", lotto))
```

Il comando riconfronta il coding con i JSON, verifica hash/n e matrici CSV/XLSX,
copia le matrici pronte in **`data/3.meta_data/matrices/`** e registra
il lotto in `data/3.meta_data/integration/batches.csv` e `matrices.csv`.
Esegue poi `data/3.2cor_to_metadata.R`, che ricostruisce metadata e review_data.

**Intervento manuale:** chiudere Excel prima della scrittura e controllare nei
finali ID, n, note e conteggi. Non correggerli a mano: il rebuild sovrascrive
le modifiche. Un'integrazione coerente rieseguita non duplica le righe.

Altri output sotto `data/3.meta_data/`: `current_basic_info.csv` (storico +
lotti registrati), `legacy_basic_info.csv`, `coding_provenance.csv` e
`matrix_inventory.csv`. Quest'ultimo indica matrici incluse, mancanti o inattive.

`source("data/3.2cor_to_metadata.R")` ricostruisce dai dati **già registrati**:
da solo non integra un nuovo lotto/staging. Un campione senza matrice pronta
non aggiunge una riga quantitativa; una codifica review completata può avanzare.
Questa indipendenza riguarda l'estrazione ancora in attesa, non autorizza a
considerare risolta un'eleggibilità full text ancora incerta o pending.
Per i nuovi lotti la review segue l'eleggibilità review; nello storico resta
la selezione precedente della prima riga per paper, incluso il meta-only storico.

## 9. Riprendere o correggere senza perdere il controllo

| Cosa modificare | Fonte corretta | Cosa rieseguire |
|---|---|---|
| Coppia di duplicati | workbook `update_candidate_pairs_review.xlsx` | finalizzazione dedup, controlli, promozione |
| Decisione titolo/abstract | `*_title_abstract_ai_decisions.csv` | finalizzazione TA; verificare gli step successivi già prodotti |
| PDF | cartella `pdfs/` con paper ID corretto | aggiornamento manifest e screening |
| Eleggibilità full text | `human_review` nel workbook | finalizzazione 03 e handoff 04 quando risolto |
| Topic, n, età, nota di un nuovo paper | JSON coding ed evidenze coerenti | build coding 02 e integrazione 04; scoring se cambia il campione |
| Item/reverse/esclusioni | script dell'articolo e chiavi | check-only, estrazione, aggiornamento JSON, build e integrazione |
| Nuovo codice | proposta codebook | sincronizzazione, poi derivati pertinenti |

Se il paper è già integrato, cambiarne eleggibilità, campione o appartenenza
al lotto richiede una riconciliazione esplicita degli output successivi.
Non basta cancellare una riga finale o rinominare un campione.

**Storico e percorso precedente.** Prima: Excel `*_full_decisions.xlsx` →
`data/2.full_exclude.R` → `data/3.meta_data/basic_info/*.xlsx`; scoring nei
blocchi di `data/3.1opendata_to_cor.R` → matrici → `data/3.2cor_to_metadata.R`.
Questi input alimentano ora lo storico, affiancato dai nuovi JSON/CSV registrati.
Non duplicare i nuovi paper anche nei vecchi basic_info; per nuovi articoli usare
script dedicati, senza lanciare tutto lo scoring storico.

Per correzioni storiche deliberate, correggere la fonte/codice e registrare il
confronto prima/dopo in `config/coding/historical_revisions.csv`: n, firme dei
valori, motivazione e note. Esempio: 0132. Il riferimento numerico originale in
`tests/fixtures/coding/pre_maintenance_numeric_contract.csv` resta fermo.

| Errore/sintomo | Intervento |
|---|---|
| `Historical correlations or sample Ns changed` | identificare la differenza; correggere l'errore o registrare la revisione verificata, senza disattivare il controllo |
| Coding `stale` / tabella modificata a mano | correggere il JSON, rifare build 02 e integrazione |
| Checksum cambiato | verificare versione/contenuto e rigenerare derivati/evidenze; non cambiare solo la firma |
| Matrice attiva modificata/non registrata | confrontare staging/registro e ripristinare o riconciliare |
| File Excel bloccato | salvare le vere modifiche manuali, chiudere e ripetere il passaggio fallito |
| Nuovo paper assente dai finali | verificare integrazione, matrice pronta, inventario e percorsi effettivi |

Il reset `scripts/dedup/00_reset_current_batch_outputs.R` elimina anche lo
screening TA del lotto: usarlo solo per ricostruzione intenzionale, preservando
il lavoro umano. Non è un comando di normale ripresa. Il reset storico completo
non fa parte dell'aggiornamento ordinario.

## 10. Verificare, salvare su GitHub e aggiornare paper/app

```r
system2(rscript, c("scripts/screening/99_run_tests.R")) # suite completa
system2(rscript, c("scripts/screening/03_validate_2026_10_regression.R"))
```

La regressione confronta i 14 record del pilot ottobre; non ripete il giudizio
scientifico. `scripts/full_text/99_run_tests.R` è il sottoinsieme full text
della suite completa. Controllare anche i diff prima di commit/push.

| Operazione | Effetto |
|---|---|
| Script R | aggiorna il filesystem locale |
| Add/commit/push | aggiorna la branch di lavoro su GitHub |
| Merge su `main` | aggiorna i file letti dagli URL che puntano a `main` |
| Render Quarto | rigenera il manoscritto/documento |
| Avvio/aggiornamento servizio app | carica i dati usati dall'app |

**Verificato nel codice:** `paper/livingSEBpaper.qmd` e
`paper/livingSEBresults.qmd` leggono i finali locali. **`app/global.R` legge
metadata, review_data e codebook da GitHub, ramo `main`, all'avvio**: vale anche
per l'app avviata localmente con quel codice. Un push sulla branch di lavoro
non basta ad aggiornare quei dati.

L'app usa anche `data/0.bib_download/bibAll.bib` per la bibliografia e
`R/cite.bib`/`R/apa.txt` per la citazione del progetto. L'integrazione dei
dataset non rigenera questi file, non renderizza il paper e non pubblica l'app:
sono controlli/azioni separati della pubblicazione.

## Esempio verificato: paper 0150

`2026-11-external-01`: BibTeX degli autori → nuovo riferimento → TA incluso →
PDF 0150 → full text `both` → JSON coding → sample `a` → `0150.R` →
staging `extraction/0150/0150a/` → matrice comune `0150a` → finali.

Da 31.315 righe sono escluse 596 identità già in 0053d: n nuovo **30.719**.
Matrice di 38 variabili; età solo individuale; n per coppia 29.244–30.719.
Download paper/BibTeX: 2026-11-05; download originali dati/codice ignoto,
ricezione locale 2026-11-05. Gli originali e le matrici precedenti sono preservati.

**Fotografia al 5 novembre 2026:** 39 campioni quantitativi e 51 paper nella
review. Restano nell'inventario tre matrici storiche mancanti (0050, 0053a,
0053b) e l'artefatto inattivo 0104a. Sono conteggi descrittivi, non obiettivi da
imporre al prossimo lotto.

**Fine della lavorazione locale:** decisioni risolte, coding aggiornato, matrici
pronte e validate, integrazione riuscita e nuovi ID verificati nei due finali.
GitHub, manoscritto e app richiedono le azioni distinte descritte sopra.
