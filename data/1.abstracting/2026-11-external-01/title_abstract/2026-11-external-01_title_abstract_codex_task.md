# Codex task: Living SEB title/abstract screening for 2026-11-external-01

Use the repository skill `$living-seb-title-abstract`.

Input: `C:/Users/tfera/OneDrive - Università degli Studi di Padova/BESSI/living_SEB_review/data/1.abstracting/2026-11-external-01/title_abstract/2026-11-external-01_title_abstract_input.csv`
Policy: `C:/Users/tfera/OneDrive - Università degli Studi di Padova/BESSI/living_SEB_review/config/screening/title_abstract.yml`
Prompt: `C:/Users/tfera/OneDrive - Università degli Studi di Padova/BESSI/living_SEB_review/prompts/title_abstract_screening.md`
Required output: `C:/Users/tfera/OneDrive - Università degli Studi di Padova/BESSI/living_SEB_review/data/1.abstracting/2026-11-external-01/title_abstract/2026-11-external-01_title_abstract_ai_decisions.csv`

After writing and validating the decisions file, run:

```r
source("scripts/screening/02_finalize_title_abstract.R")
```

Do not ask for scientific human review when the model is uncertain: use `uncertain`; the deterministic policy retains it.
