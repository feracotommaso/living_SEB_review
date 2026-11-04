# Monthly full-text eligibility

Scientific policy: **2.0.0**. Deterministic workflow: **2.2.0**.
See [the consolidation audit](AUDIT.md) for findings and the compatibility boundary.

## Data contracts

```text
data/1.abstracting/<batch>/title_abstract/<batch>_title_abstract_included.csv
  -> persistent paper_id assignment / legacy identity reconciliation
data/2.full_abstracting/
  paper_id_registry.csv
  paper_id_aliases.csv
  <batch>_full_text_manifest.csv          # canonical retrieval manifest
  <batch>_full_text_manifest.xlsx         # DOI hyperlinks; retrieval interface
  pdfs/<paper_id>.pdf                    # canonical location; ignored by Git
  ai_decisions/<batch>/<paper_id>.json    # typed assessment + evidence
  <batch>_full_text_decisions.csv         # canonical finalized assessments
  <batch>_full_text_human_review.csv      # canonical open/resolved audit queue
  <batch>_full_text_decisions.xlsx        # summary, decisions, human_review, codebook
  <batch>_legacy_routing.csv              # guarded eligibility handoff
```

CSV is canonical for machine processing. XLSX is the derived human interface;
only adjudication inputs on `human_review` are edited. JSON artifacts preserve
input assessments. Their paths and SHA-256 checksums accompany outputs.
Semicolons delimit multiple tags, components and pages in flat exports.

## Monthly commands

Run from the repository root, using an explicit batch to avoid ambiguity:

In Codex, invoke the repository skill with a request such as:

```text
Use $living-seb-full-text to screen the 2026-11 full-text batch, resume any saved
assessments, build the monthly decisions workbook, and present open cases together.
```

The skill reads available full texts and creates structured assessments, then runs
the R commands below. It preserves saved adjudications and reports missing PDFs as
acquisition blockers. It does not perform detailed quantitative extraction.

```bash
Rscript scripts/full_text/01_assign_paper_ids.R 2026-10
# Retrieve PDFs, then rerun Step 01 to refresh availability.
# Screen every paper using prompts/full_text_eligibility.md and the schema.
Rscript scripts/full_text/02_build_full_text_decisions.R 2026-10
# Discuss open human-review packets together; edit adjudication inputs.
Rscript scripts/full_text/03_finalize_full_text_decisions.R 2026-10
Rscript scripts/full_text/04_export_legacy_routing.R 2026-10
```

Steps 01–03 default to `config/dedup_config.R` when no batch is supplied.
An explicit argument selects that batch's own upstream directory, even when
the configuration points at another month. Step 04 requires an explicit batch.
Step 03 is optional when there are no new adjudications.

### Identity and historical data

Existing upstream screening IDs are preserved verbatim. New paper IDs are assigned
in stable screening-ID order above the maximum canonical or reserved alias ID.
Excluded papers keep their IDs. IDs do not gain extra leading zeroes when the
sequence passes 9999. No sample or matrix IDs are assigned at eligibility.

Bootstrap reads historical `*_full_decisions.xlsx` / `*_decision_progress.xlsx`
only; monthly v2 workbooks are excluded from legacy discovery. Historical mixed
types are coerced in memory. DOI placeholders are missing identities.
The known `0086 -> 0083` alias for DOI `10.1007/s12310-025-09811-9` remains recorded
and reserved. Persisted aliases survive removal of source files. Conflicting
identities under one paper ID stop assignment and write an auditable conflict CSV.
Raw exports, historical workbooks and promoted bibliographic registries are not
rewritten by these commands.

### Assessment and validation

Read the complete available article and supplements. Assess review focus and
quantitative eligibility independently. Assess BESSI source and measured construct
separately: beliefs/goals about skills do not measure skill capacity. Record only
eligible publication components, e.g. `Study 2`. Missing retrievable correlations
are `pending`; material scientific uncertainty requires human judgment.
Language and clinical diagnosis alone are not exclusions.

The builder requires exact one-to-one manifest coverage and matching filenames.
It validates JSON types, required fields, enums, evidence, dates and conditional
constraints **before** flattening. `R/full_text_schema.R` implements the vocabulary
used by the two checked-in schemas without optional packages or remote resolution;
unknown assertions fail closed. This bounded validator does not implement every
JSON Schema feature.

XLSX export also removes unused relationships to absent default drawing/VML or
printer parts emitted by the local openxlsx writer, while preserving actual parts,
tables, hyperlinks and validation. Unexpected broken internal relationships fail.

Routing is derived. Pending/uncertain decisions have no final route. Review-only
uncertainty also blocks a final route through `requires_human_review`, retaining
the independent quantitative status. Every human-review packet includes an issue,
recommendation, options and recorded evidence/pages.

### Adjudication and safe reruns

Edit the `human_*` inputs, `adjudication_note`, `adjudicated_by`,
`adjudication_date` and `adjudication_status` on `human_review`. Mark a completed
adjudication `RESOLVED`; its meta status may be eligible, ineligible or pending.
A note is required. Do not edit `decisions`, identities, original evidence or AI
assessment fields; do not delete resolved rows. Step 03 validates these constraints
and synchronizes CSV/XLSX outputs. Human-review row order does not matter.

Step 02 replays saved adjudications by identity and retains their evidence packets.
It refuses to overwrite workbook edits not finalized with Step 03. If the XLSX is
lost, rebuilding from JSON and the audit CSV recreates it. Generalizable
adjudications should update the versioned policy/guidance and regression tests.

### Existing-workbook migration

For an existing operational workbook with no JSON/CSV inputs, run once:

```bash
Rscript scripts/full_text/00_import_existing_decisions.R 2026-10
```

This recovered the October pilot from its **existing operational workbook**,
never from test fixtures. Original dates, decision sources, evidence text, page
lists and three adjudications were retained. Each JSON states that it was migrated
and PDFs were not re-screened. Flattened historical evidence uses its preserved
page-list locator rather than inventing per-item page numbers. The importer
refuses to overwrite an existing artifact directory or CSV. Normal monthly
screening writes fresh typed JSON directly and does not use Step 00.

## Compatibility and extraction boundary

Step 04 exports resolved routes as legacy `final_decision` values (both, meta,
review, exclude), retaining v2 evidence, tags and eligible components. It fails
if any paper remains pending or requires human review. It does not generate
extraction data or append eligibility rows to `basic_info`.

Complete detailed extraction before the historical `basic_info + matrices` stage.
For `0145`, only Study 2 proceeds; its first quantitative sample may be `a` and
matrix `0145a`. Existing paper/sample/matrix IDs are unchanged. The paper, app,
`metadata.xlsx` and `review_data.xlsx` continue using their current contracts.
Positional selections in both paper sources remain documented technical debt;
changing analytical behavior requires a separately tested migration.

## Verification

```bash
Rscript scripts/full_text/99_run_tests.R
Rscript scripts/screening/99_run_tests.R
Rscript scripts/screening/03_validate_2026_10_regression.R
```

The screening runner includes all `tests/testthat/` tests, including full text.
October fixtures under `tests/full_text/` are independent regression expectations:
review-only 2, both 1, meta-only 0, exclude 3, unresolved 0. Production builders
never read them. Do not silently edit adjudicated expectations.

On Windows, if inherited `C.UTF-8` locale settings corrupt accented workspace paths,
set process-local variables before R:

```powershell
$env:LC_ALL = 'English_United States.utf8'
$env:LANG = 'English_United States.utf8'
```
