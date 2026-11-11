# Segnalazioni esterne a Scopus e Web of Science

Current operating commands are in `MONTHLY_WORKFLOW.md` and `workflow.R`.
Naming and provenance rules below remain in force. Screening uses one workbook
per batch; descriptive coding uses the cumulative coding workbook. Older pilot
output paths below describe the checkpoint preceding consolidation.


## Regola unica

Ogni riferimento individuato fuori dalla ricerca periodica entra come fonte
esterna, anche quando il file bibliografico è esportato da Scopus o WoS.
Il metodo di individuazione e il servizio che produce il file sono informazioni
distinte. Nessuna fonte determina automaticamente l'inclusione scientifica.

Conservare il contenuto originale di BibTeX/RIS. Usare:

```text
data/0.bib_download/external/<batch_id>/
YYYY-MM-DD_<source_type>_<NN>.bib
```

Gli originali restano locali e Git-ignored, come gli export DB. Il manifest di
provenienza, i checksum, il registro e gli script sono versionati.

- `batch_id`: `YYYY-MM-external-NN`, dove anno/mese indicano l'acquisizione nel
  progetto e `NN` è il progressivo dei lotti esterni di quel mese, da `01`.
- `YYYY-MM-DD` nel nome: data di inserimento nel progetto.
- Il progressivo del file parte da `01` per data e tipo di fonte. Usare una sola
  estensione `.bib` o `.ris`; i file possono contenere uno o più riferimenti.
- Più segnalazioni possono essere lavorate insieme in un lotto ancora aperto.
  Dopo la finalizzazione usare un nuovo lotto, anche nello stesso mese. Non
  aggiungere nuovi input a un lotto già lavorato o riaprire un aggiornamento DB.
- Per un articolo vecchio acquisito oggi, conservare anno/data di pubblicazione
  originali nei metadati. Non usare la pubblicazione per retrodatare l'ingresso.

I tipi ammessi sono versionati in `config/external_source_types.csv`:

| Codice | Quando usarlo | Dettaglio da conservare |
|---|---|---|
| `author_suggestion` | Segnalazione degli autori | Origine della segnalazione; nominativo solo se disponibile e necessario |
| `expert_suggestion` | Colleghi o altre persone | Origine e contesto |
| `backward_citation` | Bibliografia di un altro lavoro | DOI/titolo del lavoro di partenza |
| `forward_citation` | Lavori che citano un articolo | DOI/titolo di partenza e servizio utilizzato |
| `manual_search` | Sito, motore di ricerca, rivista, alert | Servizio, ricerca o modalità di ritrovamento |
| `other` | Qualsiasi altra modalità | Descrizione specifica obbligatoria |

Per una segnalazione che arriva soltanto come DOI, URL, email o PDF, identificare
il riferimento e acquisire il BibTeX/RIS da una fonte bibliografica affidabile;
documentare sia la segnalazione sia la fonte dell'export. Se non è disponibile un
export, creare una citazione derivata da metadati verificati, distinguendola da un
download originale. Informazioni mancanti non vanno inventate.

## Provenienza e date

Prima dell'importazione, compilare `config/external/<batch_id>.csv`, una riga per
file, con il contratto validato da `R/external_intake.R`:

`path`, `source_type`, `received_date`, `download_date`, `source_detail`,
`source_url`, `original_filename`, `sha256`.

`received_date` è l'ingresso nel progetto; `download_date` è la data effettiva,
confermata, di download dell'export e può restare vuota finché non è nota.
La data di pubblicazione rimane nei metadati dell'articolo. Per più file con
download diversi non sostituire queste date con una data unica del lotto.
Usare `source_detail` per documentare anche il servizio di esportazione, se
diverso dal metodo di individuazione. SHA-256 protegge i byte dell'originale.

Un articolo ricevuto più volte mantiene tutte le provenienze, ma un solo record
bibliografico e gli identificativi scientifici già assegnati. Non rieseguire
screening o estrazione di un duplicato come se fosse un nuovo paper. Un nuovo
full text/versione sostanzialmente diversa richiede invece una verifica esplicita.
In coding, collegare la data di download di ogni paper al file di provenienza;
se i file del lotto hanno date diverse, non applicare una data unica a tutti gli
articoli. Una nuova segnalazione duplicata non riscrive la prima acquisizione.

## Esecuzione

Non cambiare la configurazione dell'ultimo aggiornamento mensile. In RStudio:

```r
Sys.setenv(LIVING_SEB_BATCH_ID = "2026-11-external-01")
source("scripts/dedup/00_setup.R")
source("scripts/dedup/03_update_prepare.R")
# Risolvere eventuali coppie candidate nel workbook, come per Scopus/WoS.
source("scripts/dedup/04_update_finalize.R")
# Dopo i controlli, promuovere solo tramite lo script protetto.
source("scripts/dedup/06_promote_registry.R")
# Proseguire soltanto se genuinely_new_records > 0; se tutti i riferimenti sono
# duplicati, conservare la provenienza nel registro e non ripetere lo screening.
source("scripts/screening/01_prepare_title_abstract.R")
# Skill title/abstract; validazione e finalizzazione con lo stesso selettore.
source("scripts/screening/02_finalize_title_abstract.R")
```

Per i comandi full-text/coding passare esplicitamente lo stesso `batch_id`.
Il suffisso esterno è accettato fino all'integrazione analitica. Proseguire con
gli stessi criteri, skill, controlli e identificativi permanenti del workflow
mensile. Ricercare i dati individuali e usare lo stesso stile di scoring R.
Non promuovere registri con coppie irrisolte o confronti Rayyan discordanti.

Al termine, e prima di ogni nuova ricerca Scopus/WoS:

```r
Sys.unsetenv("LIVING_SEB_BATCH_ID")
```

Il selettore è locale alla sessione/processo. La configurazione ordinaria mantiene
il mese di pubblicazione del batch DB. Nei log di un lotto esterno il campo
tecnico `search_run_date` rappresenta la data dell'ultimo ingresso del lotto;
non certifica una ricerca DB né la data di download dell'export.

## Primo caso

`2026-11-external-01`: Napolitano et al., *How Perceived Social, Emotional, and
Behavioral Skills Differ Across the Life Span*, DOI `10.1177/27000710261487580`.
Segnalazione degli autori; ingresso e download confermato: 2026-11-05.
Il file iniziale con doppia estensione è stato spostato/rinominato senza cambiare
contenuto; nome precedente e checksum sono conservati nel manifest.
Deduplicazione: un nuovo record, nessuna coppia dubbia; registro promosso a 387
record dallo script protetto. Titolo/abstract: incluso. Paper ID: `0150`.
Il primo download automatico del full text ha restituito HTTP 403; il PDF è stato
poi fornito dall'utente, verificato e letto integralmente. L'utente ha fornito
anche dati SAV e codice R, con data di download originale sconosciuta e ricezione
locale 2026-11-05. Full-text eligibility: `both`. La verifica degli ID collega 596
righe al campione storico 0053d: sono escluse solo dal nuovo contributo, lasciando
30.719 partecipanti. La matrice 0150a e la codifica sono integrate con provenienza
`external_coding`; gli originali e i valori delle altre matrici restano invariati.
