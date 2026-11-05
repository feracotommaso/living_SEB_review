# Descriptive coding and legacy reconciliation

The shared monthly/article convention is in
[`policies/extraction_conventions.md`](../../policies/extraction_conventions.md).
Both variable and review-topic codebooks evolve: reuse adequate entries, adding
new ones when needed to describe substantive/useful differences with source
evidence. Register additions before exporting; unknown codes are not silently
accepted. A new topic need not already occur in multiple papers. The later
historical harmonisation pass is separate from incremental codebook maintenance.

## Individual-data priority

For each eligible quantitative sample, first seek the original individual data,
even if the paper provides a sufficient correlation matrix. Preserve downloaded
files unchanged in `data/3.meta_data/open_data/raw/<paper_id>/`, score and code
them with reproducible R, and save the derived individual dataset in
`data/3.meta_data/open_data/individual_data/<matrix_id>.csv` (optional XLSX
companion). Existing historical files remain in place. The entire `open_data/`
directory is Git-ignored; keep extraction code and non-individual download/source
checksums in versioned files. Document analytic exclusions, scoring, variable
mapping, missingness and correlation calculation. Compare with published results
where comparable. A paper matrix is a documented fallback after checking access
and usability of individual data, not the default shortcut. Access problems keep
extraction pending and do not imply scientific exclusion.

The October integration pilot is implemented. Preserve historical eligibility
until explicit reassessment; completed review coding advances independently of
quantitative samples awaiting validated matrices.

Policy/dictionary version: **1.0.0**. Read `policies/coding_v1.md` and the canonical
31-field dictionary `config/coding/legacy_31_fields.csv`. The derived readable
dictionary is `data/coding_dictionary.xlsx`. Keep scientific metadata distinct
from technical evidence, missingness and version information.

## Commands

Run from the repository root on the current feature branch:

```bash
Rscript scripts/coding/01_update_legacy_compatibility.R
Rscript scripts/coding/03_update_codebooks.R
Rscript scripts/coding/05_set_download_date.R 2026-10 2026-11-02
Rscript scripts/coding/02_build_article_coding.R 2026-10
# Prepare/verify extraction, then rebuild coding if its source notes changed.
Rscript scripts/coding/04_integrate_batch.R 2026-10
Rscript scripts/screening/99_run_tests.R
Rscript scripts/screening/03_validate_2026_10_regression.R
```

Step 01 checks CSV/XLSX codebook consistency and derives the dictionary workbook.
Historical spelling corrections have already been applied and remain audited.
The analytical builder reads
historical coding without modifying it, normalises topic aliases, inventories
matrix matches, and writes compatible CSV/XLSX outputs and audit queues.

## Record the monthly bibliographic download date

Confirm the actual database export date, then run
`05_set_download_date.R <batch> <YYYY-MM-DD>` after creating the article coding
artifacts and before building/integrating the batch. The command writes a stable
per-batch provenance CSV, updates source JSON/evidence, preserves previous source
versions and rejects conflicting existing dates. It never edits historical raw
exports or final workbooks directly. Rebuild step 02 and rerun step 04 afterwards.
For October the user confirmed 2026-11-02 for both Scopus and Web of Science.

New months use the same parameterised commands. The first coding build assigns
sample/matrix IDs before an article extraction script validates its scope. New
article scoring still needs scientific adaptation, source evidence and the shared
template; it is not a universal automatic conversion of arbitrary datasets.
Regression fixtures additionally cover all-review batches, no retained records
and multiple sample exports. No operational November records have been created.

## Add codebook entries

`03_update_codebooks.R` without arguments verifies both codebooks and bootstraps
the variable CSV from the existing XLSX if absent, preserving historical cells.
Use CSV proposals for additions:

```bash
Rscript scripts/coding/03_update_codebooks.R --add-variables path/to/variable_additions.csv --add-topics path/to/topic_additions.csv
```

Each proposal has the corresponding canonical codebook columns plus `paper_id`,
`change_date` (YYYY-MM-DD), `reason` and `source`. Topic definitions go in
`Explanation`; variable definitions go in `notes`. The command stages and checks
CSV/XLSX before replacement, records additions in `codebook_changes.csv`, and
leaves bytes unchanged on an identical rerun. Existing keys cannot be silently
redefined; CSV/XLSX conflicts must be reconciled explicitly. Merely adding a topic
does not recode older articles. `codebook_resolve_variables()` validates base names
and attached registered suffixes and supplies labels for measurement variants.

This command performs codebook maintenance only; it does not publish an app,
change eligibility or promote coding/matrices into analytical datasets.

## Integrate a validated batch

`04_integrate_batch.R <batch>` rebuilds coding into a temporary directory and
compares canonical tables, validates extraction manifests/checksums and sample N,
and copies ready matrices to the established `data/3.meta_data/matrices/` folder.
It records batches/matrices in `data/3.meta_data/integration/`, refuses manual
active-file conflicts and duplicate papers, and then runs the analytical builder.
Rerunning unchanged inputs does not duplicate records. Samples awaiting extraction
remain eligible and pending while their completed review coding can advance.

`data/3.2cor_to_metadata.R` reads registered batches plus historical basic_info.
It preserves the 31-field interface and checks every historical populated effect
and analytic N before atomically replacing CSV/XLSX output pairs. Historical review
selection is preserved; new review rows follow resolved v2 review eligibility.
`legacy_basic_info.csv` remains historical; `current_basic_info.csv` combines both
sources, and `coding_provenance.csv` records origin and reassessment status.

October adds 0144/0145/0148 to the review and 0145a to quantitative metadata:
50 review papers and 38 quantitative samples. The three pre-existing missing
historical matrices remain visible for reconciliation, with no eligibility change.
Local integration does not publish/merge the branch or refresh the deployed app.

The descriptive builder reads complete per-paper JSON artifacts under
`data/3.meta_data/coding/artifacts/<batch>/`, checks them against resolved full-text
decisions, and writes under `data/3.meta_data/coding/<batch>/`:

- `papers.csv`: report-level fields, current eligibility and framework tags;
- `samples.csv`: eligible quantitative samples, selected occasion and stable IDs;
- `evidence.csv`: field-level source, PDF page, evidence text and source checksum;
- `legacy_export.csv`: the 31-column compatibility view;
- `<batch>_article_coding.xlsx`: derived consultation sheets for those four tables.

`sample_id_registry.csv` persists sample assignments. Only an explicit historical
crosswalk may attach new extraction labels to existing historical sample IDs.
Raw JSON is the scientific extraction input. Correct it by reference to the source
and retain a versioned audit before substantive reassessment; do not edit derived
XLSX values as an alternative input. Coding v1 enables only `first_eligible`
occasions. The descriptive builder alone does not promote outputs: the separate
guarded integration command connects them to analytical datasets after extraction.
Source checksums are captured in the input artifacts and checked on rebuild. A
changed PDF or reference source therefore requires evidence verification. Changed
sample labels in a previously coded paper require explicit registry reconciliation,
so an editorial label correction cannot silently create a new quantitative sample.

## Reconciliation decisions

- `nomoligicalnet` is an exact alias of `nomologicalnet`, not a new topic.
- `assessment -> skillassessment` for paper 0066 is a user-confirmed legacy
  correction; `theorydevelopment` is retained and the resolved case leaves the
  topic-review queue. Original historical inputs and before/after values remain.
- `0104a.xlsx` stays in place as an inactive prepared matrix: report 0104 has an
  existing review-only/dataQuality decision. It must not gain quantitative
  eligibility through filename reconciliation. Its 18 empty-only effect columns
  are removed from analytical exports, while all scientific values are preserved.
- Missing same-named matrices for 0050, 0053a and 0053b remain visible in the
  inventory for source-level reconciliation; no new exclusion is inferred.

The pre-maintenance fixture records analytic Ns, populated correlation counts and
SHA-256 signatures of sorted nonmissing effects rounded to 12 significant digits.
It stays frozen. `config/coding/historical_revisions.csv` records explicit
scientific corrections with previous and revised signatures, Ns, dates, reasons
and notes. `coding_validate_historical()` applies those exact transitions to the
baseline and checks every historical sample. A further change to a revised
matrix still fails; a revision is not an exemption from validation.

The 2026-11-05 revision of 0132a applies the documented exclusions (598 students),
both social-status items, five anxiety items without an additional reversal, and
parent scale/education means per student (321 questionnaires for 315 students).
The corrected historical script also writes canonical matrix/individual CSV
companions. `data/3.meta_data/extraction/0132/0132a/historical_revision.csv` records
all 276 previous/current correlations and their actual pair Ns; the accompanying
manifest records source/output checksums without individual observations.
The ledger's notes are applied when reading historical basic_info and therefore
reach metadata and review outputs without editing original monthly workbooks.
They explicitly retain the current single-N metaSEM limitation. The ordinary
`data/3.2cor_to_metadata.R` command now accepts this exact correction; future
scientific revisions require their own reviewed ledger transition and evidence.

For Windows locale problems with accented paths, use the process-local settings
documented in `scripts/full_text/README.md`. On installations where the R launcher
fails, invoke the installed `bin/x64/Rscript.exe` directly.
