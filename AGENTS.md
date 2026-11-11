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

## Full-text screening

- Use the repository skill `$living-seb-full-text` for monthly full-text screening.
- Read `config/screening/full_text_eligibility.yml`, its linked policy/prompt and
  the decision schema; use the existing R workflow for IDs, validation and exports.
- Preserve existing assessments and resolved adjudications when resuming a batch.
- Missing/unreadable main PDFs block acquisition; do not invent eligibility rows.
- Assess review focus and quantitative eligibility independently, including BESSI
  measurement source versus the construct actually measured.
- Collect substantive ambiguities into one evidence-based human-review queue.
- Do not assign sample/matrix IDs or perform detailed extraction at this stage.

## Quantitative extraction

- Follow `policies/extraction_conventions.md` and its common article R template.
- Keep explicit scientific steps in one preparation script per article; use shared
  helpers only for repeated technical operations. Preserve historical scripts.
- Include appropriately coded sex/gender in matrices when available; retain age
  in individual data and descriptive metadata, not in correlation matrices.
- Questionnaire totals are item means with documented missingness and weights;
  keep SEB/BESSI questionnaire scores on 1-5 and explicitly convert dual rating.
- Reuse or extend variable and review-topic codebooks with evidence. Topics should
  group articles and preserve useful/substantive differences; register additions
  before export. Keep historical harmonisation a separate, versioned operation.
- Use registered attached suffixes (`other`, `iat`) for measurement variants; see
  `config/coding/variable_suffixes.csv`. Do not invent script-specific exceptions.
- Seek original individual-level data even when a published matrix is sufficient.
- Preserve downloaded originals under `data/3.meta_data/open_data/`; use
  `raw/<paper_id>/` for new acquisitions. Save derived coded individual datasets
  under `data/3.meta_data/open_data/individual_data/<matrix_id>.csv` with an
  optional XLSX companion. Keep the entire directory Git-ignored.
- Version reproducible R scoring/extraction and non-individual source provenance,
  including URLs, retrieval dates, checksums, exclusions and missing-data rules.
- Published matrices are a comparison reference or a documented fallback after
  checking individual-data availability/usability. Access failures do not change
  eligibility. Do not invent scoring rules or silently replace downloaded versions.
- Use `workflow.R` for the current human workflow and final integration.
  Canonical coding is in `data/3.meta_data/coding/records.csv`; import edits from
  the general workbook. Preserve historical scientific values until an explicit
  source correction and readable before/after comparison have been reviewed.
- Long article keys may be in an explicitly sourced `scripts/extraction/keys/`
  companion; keep scientific operations visible in the main article script.
- Save new matrices directly in the common matrix directory, pair Ns by matrix
  in `data/3.meta_data/pair_n/`, and aggregate checks in `extraction/summary.csv`.
  Confirm bibliographic dates in the general coding workbook; do not infer them
  from publication month or PDF/individual-data retrieval. Keep original source
  provenance in `coding/sources.csv` and coding evidence in `coding/evidence.csv`.

## Validation

- After changing title/abstract screening code, run:
  `Rscript scripts/screening/99_run_tests.R`.
- For the 2026-10 pilot, run:
  `Rscript scripts/screening/03_validate_2026_10_regression.R`.
- Report any test that could not be executed in the current environment.
