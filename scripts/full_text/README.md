# Screening full text

La procedura operativa corrente è nella [guida unica](../../MONTHLY_WORKFLOW.md).
Carica `source("workflow.R")` dalla radice del progetto.

1. `prepara_full_text(lotto)` assegna/riusa i paper ID e aggiorna il manifest dei PDF.
2. La skill full text legge gli originali e salva JSON conformi allo schema in `data/2.full_abstracting/ai_decisions/<lotto>/`.
3. `raccogli_full_text(lotto)` valida e raccoglie le decisioni nell'Excel unico di screening.
4. Compila le celle gialle di `decisioni_full_text`, poi `importa_screening(lotto)`.
5. Per riaprire un caso risolto: `rivedi_full_text(lotto, paper_id, motivo)`.

La policy resta in `config/screening/full_text_eligibility.yml` e nei riferimenti collegati.
Review e meta-eleggibilità sono indipendenti; indisponibilità dei dati e incertezza sostanziale restano distinte.
I fogli `full_text`, `pdf` e `criteri_full_text` sono di consultazione.
Il foglio `decisioni_full_text` conserva le valutazioni originali e consente di modificare i soli input umani.
Per risolvere un caso sono necessari decisioni coerenti, nota, autore della decisione, data e stato RESOLVED.
Il foglio criteri mostra i codici e le definizioni. I campi non applicabili restano vuoti.

I CSV `<lotto>_full_text_decisions.csv` e `<lotto>_full_text_human_review.csv` sono canonici.
I JSON conservano l'evidenza originale; le correzioni umane restano nel CSV di adjudication.
Non ricreare Excel separati di manifest e decisioni o un export legacy routing.
Gli strumenti di importazione storica e le funzioni di compatibilità sono conservati per i vecchi file.
