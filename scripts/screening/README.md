# Screening titolo e abstract

Procedura operativa: [guida unica](../../MONTHLY_WORKFLOW.md).
La policy scientifica resta `config/screening/title_abstract.yml`.

La preparazione dopo la deduplicazione produce il CSV `..._title_abstract_input.csv`.
La skill produce `..._title_abstract_ai_decisions.csv` con gli ID esatti.
Da `workflow.R`, `raccogli_abstract(lotto)` valida e aggiorna l'Excel unico del lotto.
La revisione manuale usa i campi `human_*` del foglio abstract e `importa_screening(lotto)`.
L'incertezza viene mantenuta; un'esclusione richiede un criterio ammesso e un'evidenza esplicita.
La proposta IA resta distinta dalla decisione operativa finale.

Output canonico: `..._title_abstract_decisions.csv`; ingresso del full text: `..._title_abstract_included.csv`.
La normale procedura non produce workbook separati input/included/excluded né manifest e riepiloghi duplicati.
`99_run_tests.R` verifica la suite; `03_validate_2026_10_regression.R` confronta i 14 esiti del pilot di ottobre con quelli validati.
