# Living SEB — guida operativa unica

Aggiornata il 18 novembre 2026. Aprire il progetto RStudio nella radice della repository.
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

Gli Excel correnti del workflow usano testo senza ritorni a capo automatici e colonne larghe al massimo 40 caratteri Excel. Per leggere integralmente una cella lunga, selezionala e usa la barra della formula. Le esportazioni successive mantengono questa impostazione.

## 4. Codifica generale

```r
prepara_codifica(lotto, data_download = "2026-12-02") # esempio: usare la data realmente nota
```

Ometti `data_download` se sconosciuta. La funzione aggiunge gli articoli mantenuti alla codifica generale, precompilando i dati bibliografici. Nel file `coding.xlsx`:

- `articoli`: titolo, autori, date, topic e informazioni comuni;
- `campioni`: campioni quantitativi, n, descrittive, misure e note scientifiche;
- `costrutti`: vista di consultazione con una riga per costrutto/articolo e le etichette del codebook;
- `fonti`: originali, URL, date, checksum e riferimenti storici;
- `evidenze`: pagine, campi, motivazioni e limiti della codifica;
- `dizionario`: i 31 campi della compatibilità analitica e i cinque nuovi campi della review.

In `articoli` si compilano `main_objective` (una frase sull'obiettivo),
`sample_context`, `seb_measurement` (solo il modo di misurare le skill),
`study_contribution` e `constructs_analyzed`. Categorie e definizioni sono nel
dizionario. Nei campi multipli separa i valori con `;`. Per i costrutti usa i
codici di `matrix_codebook`, una sola volta per articolo: strumenti diversi,
informatori e tempi non duplicano il medesimo costrutto. Domini e sottoscale
distinte effettivamente analizzate possono essere entrambi presenti.
Il foglio `costrutti` si rigenera automaticamente: modifica l'elenco in `articoli`.

`study_design` accetta più codici (`cross; long`, ad esempio). Per articoli
quantitativi modifica il disegno di ciascuna componente in `campioni`:
`articoli` e la review ne mostrano l'insieme. Per i review-only lo modifichi in
`articoli`. Classifica le analisi presentate, non ogni raccolta del progetto;
un piccolo retest di affidabilità da solo non richiede `long`.

I topic descrivono il focus scientifico; gli esiti usati solo per validazione
si trovano attraverso i costrutti. Le sintesi quantitative hanno costrutti
analizzati anche senza un nuovo campione. Lascia vuoto ciò che non è ancora
esaminato; `Not reported` e `Not applicable` hanno significati distinti.
La revisione procede articolo per articolo: i campi vuoti non significano che
un costrutto sia assente. I nuovi filtri della review nell'app riportano la
copertura della codifica. La codifica ampliata dei 51 articoli attuali è completa. Per i nuovi inserimenti si applicano gli stessi campi e criteri.

`Development/change` comprende anche desideri, motivazioni e credenze sul cambiamento: non implica di per sé un cambiamento osservato nel tempo. La distinzione resta in `study_design`. In `open_data` usa un link oppure `Available on request`, `Not publicly available`, `Not reported`, `Not applicable`, secondo la fonte. Gli indicatori a singolo item sono qualificati nelle note/evidenze.

I cinque campi sono salvati nel medesimo `records.csv` ed esportati in
`review_data` (36 colonne). `metadata` conserva i 31 campi descrittivi seguiti
dalle correlazioni. Non occorre mantenere una nuova tabella manuale separata.

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
Il dizionario dei topic comprende 50 topic attivi e il codice storico
`positiveschooloutcomes`, ritirato e conservato per compatibilita. Le sue ultime
tre assegnazioni (0071, 0080, 0113) sono state rilette e ricodificate sul focus:
nessun articolo corrente lo utilizza. Non assegnarlo a nuovi articoli.
Per 0014 sono stati registrati i punteggi di abilita derivati dal BFI in un
gruppo distinto dai domini BESSI. Le forme forced-choice restano self-report;
non sono prove di prestazione. I nuovi topic `responseformats`,
`migrationbackground`, `educationaltracking` e `intersectionality` descrivono
focus sostanziali verificati nei paper; non ricodificano automaticamente gli
articoli ancora da esaminare.
Il topic `purposeinlife` rende individuabile il focus di Beatty et al. (0023),
che rimane review-only. Anche Walton et al. (0014) rimane review-only e non
ha matrici quantitative: registrare un costrutto nel codebook non ne determina
l'inclusione nella meta-analisi.
Il codebook delle variabili comprende `definition` obbligatoria e `synonyms`
facoltativi per la ricerca. Le etichette delle 32 faccette BESSI sono complete.
L'importazione riordina le variabili per `Topic` e `broad`, anche se inserite
inizialmente in fondo, e mantiene l'Excel senza testo a capo. Le categorie con
una sola variabile sono ammesse. Sono registrati `other`, `iat` e `task`;
l'app distingue le varianti presenti nei dati e conserva gli ID nelle selezioni.
I sinonimi servono a trovare un costrutto, non ad assegnargli automaticamente
nuove misure. `conditionalcooperationtask` sostituisce il precedente codice
solo nella prova non integrata di 0107: non esiste una matrice attiva da rinominare.
Il 17 novembre sono state recuperate anche cinque sottoscale SECA di 0053:
`secaSelfconcept`, `secaEmotionknowledge`, `secaEmotionregulation`,
`secaGoalmanagement`, `secaSchoolwork`. Lo script `0053.R` esporta soltanto
l'espansione approvata di `0053e`: 87 variabili, 420 associazioni aggiuntive,
N di riferimento 322 invariato e 249 osservazioni per ciascuna nuova sottoscala.
Le altre quattro matrici vengono verificate ma non riscritte. I punteggi sono
le medie fornite dagli autori sulla scala originale 1–4, senza nuove esclusioni.

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

Lo stesso comando ricostruisce `data/search_results_history.csv` e il relativo Excel
con una riga per lotto di acquisizione, comprese le segnalazioni esterne. I record
importati provengono dai manifest; ogni articolo distinto è attribuito al primo
lotto registrato nelle sue provenienze. I conteggi di screening e inclusione sono
lo stato attuale degli articoli di quel lotto, non la data in cui furono valutati.
`not_in_current_datasets` indica full text assenti dai dataset inclusi attuali.
Le colonne `included` e `meta` contano articoli, non campioni. I totali coincidono
con il report; non modificare manualmente questo riepilogo derivato. Per rigenerare
il report usa il comando Quarto indicato nella sezione finale.

La strategia di ricerca è conservata in `paper/materials/search_code.docx`.
Le vecchie prove locali sono in `archive_local/analysis_trials/`, esclusa da Git.
Gli script storici dentro `data/` restano disponibili fino alla ricodifica degli studi.

Le funzioni condivise per analisi locali, paper e app si modificano solo in
`app/R/utils.R`. Il file `R/utils.R` carica quella stessa implementazione per
compatibilità con gli script esistenti. Sono incluse anche `getMETAdata()` e
`getSEMdata()`, prima contenute in `correlation_pairs.R`. Pubblicando la cartella
`app/`, il codice necessario viene incluso senza dipendere da cartelle esterne.

## Manuale dell'app e formati dei documenti

Solo `paper/livingSEBpaper.qmd` usa APAquarto, conservato in `paper/_extensions/`.
Il report dei risultati usa HTML standard. Il manuale usa HTML con indice laterale
e PDF standard; il percorso del PDF esistente è conservato. Per rigenerare entrambi:

```sh
quarto render appManual/livingSEBapp_manual.qmd --to all
```

Il contenuto è in `appManual/livingSEBapp_manual.qmd`; lo stile HTML è in
`appManual/manual.css`. Le pagine e il codice nel PDF vanno controllati dopo modifiche
alla struttura. La verifica dell'estensione del paper può essere svolta su una copia
temporanea senza eseguire le analisi; non usare questi output di prova come paper finale.

## Correggere uno studio precedente

| Problema | Intervento | Passaggio successivo |
|---|---|---|
| Topic, assessment, paese, note o data | Modifica `coding.xlsx`, aggiungi `change_note` | `importa_codifica()`, anteprima, aggiornamento |
| Item invertito, media, esclusione o ricodifica | Correggi lo script dell'articolo | Riesegui lo scoring, anteprima, aggiornamento con motivo |
| Correlazione trascritta male | Correggi la trascrizione collegata alla fonte e riesegui lo script | Anteprima, aggiornamento con motivo |
| Decisione di eleggibilità | Riapri il caso nel lotto full text | Importa screening, poi `prepara_codifica(lotto)` e aggiornamento |

Gli script storici, incluso `data/3.1opendata_to_cor.R`, restano conservati. Per gli studi non ancora migrati continua a usare il relativo blocco per ricalcolare la matrice. **0120 è ora migrato:** usa `estrai_articolo("0120")`, che applica i pesi ufficiali BESSI e tratta la risposta fuori scala come mancante; il vecchio blocco conserva lo scoring precedente. `estrai_articolo("0013", verifica_soltanto = TRUE)` e l'equivalente per 0018 eseguono invece solo le verifiche storiche. Per 0013 resta N=840, con la nota sul file di 897 record e gli N per coppia 573–840. La codifica corrente si corregge nell'Excel generale. Il normale aggiornamento finale passa da `workflow.R`; il vecchio `data/3.2cor_to_metadata.R` resta disponibile per compatibilità e conserva i suoi controlli storici.

## Verifiche, Git, paper e app

`estrai_articolo("0026")` include ora i due indicatori oggettivi approvati:
`jobperformanceactivity` (immobili mostrati) e `jobperformancesales` (vendite).
Le colonne originali sono gia rapportate ai mesi lavorati e centrate per citta;
non si ripete la trasformazione. La matrice contiene 17 variabili e 136
correlazioni: 31 aggiunte, nessun coefficiente precedente cambiato. Restano
2.992 partecipanti; i due indicatori sono osservati per 2.723 e 2.196 persone.
I nuovi N per coppia sono 2.195-2.723 e sono salvati in `pair_n/0026a.csv`.
Nelle note e nelle evidenze sono documentati due segni della Tabella 2
discordanti dai dati originali per il contrasto neuroticismo/stabilita emotiva.
La review indicizza una sola volta il costrutto generale `jobperformance`;
le due colonne quantitative restano distinte.

`estrai_articolo("0030")` resta una verifica
storica: aggiorna solo il riepilogo dell'audit.
`estrai_articolo("0048")`, `estrai_articolo("0049")` e
`estrai_articolo("0050")` esportano ora le estrazioni approvate. 0048 rappresenta
il campione condiviso con 0013: quest'ultimo resta solo nella review e la sua
matrice è conservata come storico inattivo. 0049 recupera `bf_emotionalvolatility`.
0050 ricostruisce le medie riviste dagli item originali e usa domini ponderati
sulle faccette disponibili, conservando la copertura delle faccette principali.
Tutte le persone sono mantenute; in 0050b l'N dichiarato 767 è distinto dalle
768 righe del file. Il codice esplicita la regola e gli N per coppia.
**0050 — decisione corrente del 14 novembre 2026:** si mantengono i due campioni
dell'impostazione manuale, T4 in `0050a` e Study 3 in `0050b`. La prova del campione
unico con N=1.855 è stata annullata. Restano le correzioni di scoring già
approvate, senza unire tutte le persone T0/T4 né introdurre una terza matrice.
Le 238 persone condivise e gli N diversi per correlazione restano documentati.
Il confronto autonomo `scripts/extraction/articles/0050_publication.R` conserva
la replica delle tabelle pubblicate senza sostituire le matrici Pearson.
`estrai_articolo("0035")`, `estrai_articolo("0043")` e
`estrai_articolo("0045")` esportano le correzioni approvate: sesso, istruzione
dei genitori, faccetta di consistenza e ore continue di volontariato. Conservano
N e partecipanti. Il confronto storico resta in
`data/3.meta_data/extraction/historical_audit.csv`.
Per tutti, con `verifica_soltanto = TRUE` non si scrive nulla.
`estrai_articolo("0053")` verifica le cinque matrici ed esporta soltanto
l'espansione SECA di 0053e descritta sopra. `estrai_articolo("0070")` esporta la matrice con SPACS codificata come
`sustainabilitycompetence` e PebSingle come `proenvironmentalbehavior`: otto
correlazioni aggiunte, nessun coefficiente precedente modificato, N=702.
Dal 18 novembre include anche le tre sottoscale SPACS (quattro item ciascuna):
`sustainabilityknowledge`, `sustainabilityinfluence`,
`sustainabilitywillingnesstoact`. Sono 12 variabili e 66 correlazioni: 30 nuove
rispetto alla matrice con PebSingle, tutte verificate con la Tabella 3.
`estrai_articolo("0068")` esporta invece
la correzione approvata il 14 novembre: tutte le 356 persone, due medie dei voti
ricalcolate e rendimento standardizzato separatamente per anno scolastico.
Nei dati privati restano anche media grezza, anno scolastico ed età.
Dal 18 novembre sono presenti anche le quattro risorse CAAS (sei item ciascuna):
`careerconcern`, `careercontrol`, `careercuriosity`, `careerconfidence`,
conservando il totale. Sono 13 variabili e 78 correlazioni: 42 nuove,
nessun coefficiente precedente o N modificato.
Le proposte sui descrittivi other-report di 0053g sono state approvate: si usano età/sesso
delle persone valutate, conservando i dati degli informatori nelle note.
Il recupero di PebSingle in 0070 è approvato dopo il chiarimento sulla sua
distinzione dalla competenza percepita SPACS, documentata nell'audit.
`estrai_articolo("0071")`, `estrai_articolo("0080")` e
`estrai_articolo("0087")` aggiornano soltanto l'audit, senza riscrivere matrici
o dati individuali. I primi due ricostruiscono le correlazioni osservate;
0087 verifica la trascrizione della tabella pubblicata. Per decisione esplicita,
0087 resta incluso con N=373 e con nota su correlazioni da modello corrette
per errore di misura, FIML e 221 rispondenti a T2. Non sono disponibili N per
coppia osservati. Per ripetere i controlli senza salvare:
`estrai_articolo("0087", verifica_soltanto = TRUE)`.
Per il gruppo successivo, `estrai_articolo("0095")` (Sewell et al., volontariato)
aggiorna solo l'audit: rimangono medie degli item disponibili e T1.
`estrai_articolo("0100")` (Pellegrino et al., studenti con SLD) esporta voti
standardizzati per tipo di laurea e intelligenza fluida Cattell.
`estrai_articolo("0107")` (Fallucchi et al., cooperazione condizionale) esporta
259 persone abbinate senza duplicazioni, genere binario mancante per i quattro
casi `Altro` e scoring BESSI verificato nel questionario supplementare: sei item
etici e pesi normalizzati del dominio. La misura del compito di cooperazione
condizionale non viene aggiunta. Tutti gli N di riferimento restano invariati.
`estrai_articolo("0108")` (Soto et al., intervento comportamentale) conserva
la prima misurazione disponibile, inclusi cinque esiti raccolti inizialmente a T2,
e usa il contrasto binario del sesso senza escludere persone.
`estrai_articolo("0118")` (Cravet e Usai, giochi da tavolo) esporta i quattro
totali CPM corretti e il sesso secondo il paper, come approvato dall'utente.
`estrai_articolo("0126")` (Allemand et al., cambiamento di personalità) seleziona
tutti i 1.917 rispondenti della seconda parte, corregge l'N da 1.898 e conserva
esattamente le correlazioni. Le tre età mancanti restano nei dati individuali.
`estrai_articolo("0134")` (Pellegrino et al., engagement longitudinale) ricalcola
gli item e conserva soltanto T1: tutte le 45 correlazioni e N=1330 sono confermati.
La sovrapposizione con 0068 è annotata in entrambe le codifiche, senza esclusioni.
`estrai_articolo("0138")` (Kramer et al., intervento sul nevroticismo) riscrive
soltanto la matrice pubblicata con autoefficacia × growth mindset=.16, come
approvato dall'utente e riportato nella Figura S3 T1. Non genera individui o N
per coppia fittizi. `0128.R` e `0141.R` verificano le matrici pubblicate/fornite
dagli autori e aggiornano solo l'audit: non ci sono dati individuali utilizzabili.
Per 0141 il sesso storico è confermato dall'utente tramite email degli autori;
per 0128 l'utente mantiene il contrasto cis-man con nota esplicita: la matrice
è invariata e `sex` non va interpretato come confronto soltanto donne-uomini.
L'audit ha esaminato tutti i 30 articoli quantitativi attivi e le 38 matrici;
i limiti di fonte e le dipendenze documentate non sono considerati risolti per
il solo fatto di aver completato l'esame.
**0034 è ora migrato:** `estrai_articolo("0034")` esporta i due mapping SNS
corretti e i domini BESSI con i pesi ufficiali, mantenendo tutti i 303
partecipanti, inclusi gli otto minorenni. Il vecchio blocco conserva lo scoring
precedente. Per 0030 la decisione è mantenere entrambi i campioni e gli N
invariati, annotando la sovrapposizione. Il dettaglio è in
`scripts/extraction/README.md`.

La migrazione iniziale è stata confrontata con il checkpoint di 39 campioni e 51 articoli, con gli stessi valori. Dopo la decisione del 13 novembre di rappresentare una sola volta il campione 0013/0048, gli output correnti hanno 38 matrici, 30 articoli quantitativi e 51 articoli nella review. La prova 0150 ha riprodotto gli stessi CSV individuali e della matrice: N=30719, 38 variabili, N per coppia 29244–30719, dopo l'esclusione dei 596 partecipanti sovrapposti. La matrice conserva il piccolo autovalore negativo già documentato.

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

Apri `paper/livingSEBresults.html` e controlla provenienza dei dati, conteggi, diagramma e risultati. Il report legge gli XLSX locali; `R/report_inputs.R` combina gli export storici (anche `.xlsm`) e i CSV canonici. Ogni articolo viene contato una volta tramite il registro bibliografico, anche se compare in più export. Le analisi vengono rieseguite e i modelli OSMASEM devono superare i controlli numerici. Dopo la riconciliazione del 10 novembre: 614 record importati, 229 duplicati, 385 articoli sottoposti a screening e 149 full text. Il precedente divario di 20 record era un errore di conteggio, non screening mancanti. L'aggiornamento del report non modifica i dataset.

L'app, anche avviata localmente, legge gli XLSX e i codebook da GitHub `main`: un push sulla branch di lavoro non aggiorna l'app. Bibliografia, rendering del paper e pubblicazione dell'app restano passaggi espliciti separati dall'aggiornamento dei dati.

## Copia manuale locale durante gli aggiornamenti automatizzati

La copia operativa resta in `archive_local/manual_workflow_2026-11-23/`, esclusa
da Git. `storico_originale/` resta un archivio: non e il progetto da aggiornare.
Il codice pubblico in `R/manual_backup.R` e i template tecnici non contengono
nessun dato individuale o contenuto della copia privata.

Durante l'estrazione di ogni nuovo articolo quantitativo, l'assistente prepara
anche `data/codici_articoli/<paper_id>.R` nella copia locale: item, inversioni,
medie, soglie e pesi espliciti, nello stile semplice concordato. Un file con
`TODO_SCORING` va completato e verificato prima di considerare concluso il mese.
Gli articoli review-only non richiedono scoring o matrici inutili.

`aggiorna_risultati()` ora aggiorna automaticamente anche questa copia: crea i
nuovi full_decisions mensili, importa la codifica ampliata e i topics, aggiorna
codebook e originali locali, estende l'indice degli scoring e rigenera i risultati
manuali. Controlla quindi equivalenza di matrici, N per coppia disponibili,
metadata e review_data. `batch_id`, conservato negli input manuali come contesto,
distingue le decisioni moderne meta-only dalle inclusioni storiche nella review;
non modifica i 31 campi analitici o la data di download.

Per ripetere soltanto la sincronizzazione locale:

```r
source('workflow.R')
aggiorna_backup_manuale()
```

Il controllo locale e in `verifica/ultimo_sync.md`; un piccolo registro in
`provenienza/manual_sync.csv` protegge le modifiche autonome ai full_decisions e
ai codebook. Se trova modifiche manuali, si ferma conservandole: vanno confrontate
con la codifica principale, non sovrascritte. Se lo scoring non riproduce gli
output accettati, la copia e segnalata come incompleta. L'assenza della copia su
un altro computer non pubblica ne ricostruisce dati privati: l'assistente deve
predisporla localmente prima di chiudere il relativo aggiornamento.

Per ogni mese, anche senza nuovi inclusi, concludere con `aggiorna_risultati()`
per includere nel backup le nuove fonti e gli esiti dello screening. Il report
Quarto e il merge/push restano operazioni di pubblicazione separate.
