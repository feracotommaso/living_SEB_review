# Descriptive coding and legacy reconciliation

Policy/dictionary version: **1.0.0**. Read `policies/coding_v1.md` and the canonical
31-field dictionary `config/coding/legacy_31_fields.csv`. The derived readable
dictionary is `data/coding_dictionary.xlsx`. Keep scientific metadata distinct
from technical evidence, missingness and version information.

## Commands

Run from the repository root on the current feature branch:

```bash
Rscript scripts/coding/01_update_legacy_compatibility.R
Rscript data/3.2cor_to_metadata.R
Rscript scripts/coding/02_build_article_coding.R 2026-10
Rscript scripts/screening/99_run_tests.R
Rscript scripts/screening/03_validate_2026_10_regression.R
```

Step 01 applies exact topic spelling aliases to the active codebook, writes its
CSV companion, and derives the dictionary workbook. The analytical builder reads
historical coding without modifying it, normalises topic aliases, inventories
matrix matches, and writes compatible CSV/XLSX outputs and audit queues.

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
occasions. New batch outputs are not automatically promoted to historical
basic_info or analytical datasets. Correlation extraction remains a later stage.
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
It checks scientific preservation while allowing removal of all-missing columns.

For Windows locale problems with accented paths, use the process-local settings
documented in `scripts/full_text/README.md`. On installations where the R launcher
fails, invoke the installed `bin/x64/Rscript.exe` directly.
