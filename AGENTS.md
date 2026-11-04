# Living SEB repository instructions

## General

- Treat the repository as a versioned scientific workflow.
- Preserve raw bibliographic exports; never edit files under
  `data/0.bib_download/` in place.
- Prefer deterministic R code for import, validation, merging, and output
  generation. Use AI only for structured scientific judgment.
- Keep CSV as the canonical machine-readable format and XLSX as a derived human
  interface.
- Do not overwrite a promoted registry outside the guarded promotion script.

## Deduplication

- Read `scripts/dedup/README.md` before changing or running the workflow.
- Normalise Scopus only through the existing derived-copy functions.
- Use `00_reset_current_batch_outputs.R` for a batch reset.
- Use `00_reset_full_rebuild.R` only when intentionally rebuilding history.
- Do not promote a registry when manual duplicate pairs remain unresolved or a
  Rayyan comparison exists with discrepancies.

## Title/abstract screening

- Use the repository skill `$living-seb-title-abstract`.
- The source of truth is `config/screening/title_abstract.yml`.
- Reviews, meta-analyses, and journal editorials are retained.
- Exclude only when an allowed exclusion criterion is explicit and clear.
- Absence of SEB terminology is not sufficient for exclusion.
- Encode ambiguity as `uncertain`; the deterministic policy retains uncertain
  records automatically.
- Preserve every `screening_record_id` exactly and provide one decision per ID.
- Never invent or omit records to balance the batch.

## Validation

- After changing title/abstract screening code, run:
  `Rscript scripts/screening/99_run_tests.R`.
- For the 2026-10 pilot, run:
  `Rscript scripts/screening/03_validate_2026_10_regression.R`.
- Report any test that could not be executed in the current environment.
