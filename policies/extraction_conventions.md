# Article extraction conventions

Agreed on 2026-11-04. Apply with `policies/coding_v1.md`. This is the common
convention for new articles and monthly batches; do not rewrite historical scripts
or scientific decisions merely to adopt the style.

## Files and readability

- Use one R preparation script per article:
  `scripts/extraction/articles/<paper_id>.R`. Keep explicit sections for separate
  samples/matrices within the article; split exceptionally complex preparation
  only when that improves readability and retain one clear article entry point.
- Start from `scripts/extraction/templates/article_extraction.R`. Acquisition can
  remain a separate command, such as `0145_download_sources.R`; preparation runs
  from verified local sources rather than implicitly downloading on every run.
- Run from the repository root. No personal absolute paths, `setwd()`,
  `rm(list = ls())`, or package installation inside an article script.
- Use direct, readable R assignments. Keep participant selection, reversals,
  item lists, weights, scoring, missing-data decisions and variable mapping in
  the article script, with comments identifying the source and reason.
- Long named answer/item keys may use an explicitly sourced companion under
  `scripts/extraction/keys/<paper_id>.R`; it contains constants, not processing
  logic. The main script retains the scoring operations and visible weights.
- Share only genuinely repeated technical operations, such as source checksums,
  name validation and export. Do not hide scientific scoring in a generic engine.
  Add shared helpers when the pilot establishes the actual repeated operations.

## Stable sections and object names

Every article script follows these sections in order:

1. Article, eligible components, assigned IDs and source references.
2. Import of verified originals (`raw_*`, distinct objects for separate files).
3. Linkage, documented exclusions and selected occasion (`data_analytic`).
4. Recoding, missing-data handling and scoring (`data_scored`).
5. Explicit mapping to canonical variable names.
6. Scientific checks against the source, including analytic N and score ranges.
7. Variable selection (`data_matrix`), correlations (`cor_matrix`) and pair Ns.
8. Export of coded individual records, matrices and non-individual provenance.

These names describe in-memory stages, not mandatory intermediate files. Use
sample-specific object names when necessary to keep samples distinct. Never
force a reported N by dropping arbitrary rows; discrepancies require explanation.

## Scoring and demographics

- Questionnaire scale totals are item means after documented reversals. Preserve
  the documented item/facet weights when calculating domains. Specify which
  missing responses are allowed; `na.rm = TRUE` is not an implicit universal rule.
  An all-missing score is missing, not zero or a valid score.
- SEB/BESSI questionnaire scores in coded individual data use the common 1-5
  metric. For the BESSI dual-rating 1-9 version use `(score_9 + 1) / 2`, as in the
  historical extraction. Keep the transformation explicit and verify the source
  response range. Other methods such as IAT retain their method-specific scoring;
  do not automatically rescale them as questionnaire responses.
- Retain age in coded individual data and mean age in descriptive metadata, but
  exclude individual age from exported correlation matrices. Missing age alone
  must not remove participants from the correlation sample or individual export.
- Include sex/gender in correlation matrices when available and appropriately
  coded. Preserve the source meaning and document the coding. Historical binary
  coding is 0 male, 1 female; do not force other categories into that contrast or
  infer sex/gender. Keep this distinct from the sample-level female percentage.
- Explicitly state Pearson and `use = "pairwise.complete.obs"` as the usual
  correlation convention, documenting any scientifically necessary exception.
  Retain reference analytic N in the 31-field export and pair-specific Ns in a
  separate technical output; document relevant differences in notes.

## Variable codebook and measurement suffixes

`data/matrix_codebook.csv` is the canonical variable codebook;
`data/matrix_codebook.xlsx` remains the derived interface used by legacy readers.
`scripts/coding/03_update_codebooks.R` bootstraps a missing CSV from its current
XLSX without changing the workbook, and applies documented additive proposals to
both formats. It refuses conflicting CSV/XLSX values and silent redefinitions.

Reuse an existing broad construct name when the construct is sufficiently
similar, including when a different questionnaire measures it. If no entry fits,
add the new construct before export, with its name, label, grouping, definition
and an evidence-based reason. Do not silently discard unregistered variables.
Retain original variable/instrument-to-construct mapping in the article script.
If two measures map to the same construct within one sample, document how they
are handled; never overwrite one silently or average distinct measures by default.

`config/coding/variable_suffixes.csv` registers measurement suffixes. Append them
directly without a separator: `selfmanagement`, `selfmanagementother`,
`selfmanagementiat`. The unsuffixed SEB/BESSI name denotes the ordinary self-report
measure. `other` denotes another informant and `iat` an IAT measure. A different
questionnaire alone does not require a suffix. Historical `...other` names remain
unchanged. A suffixed variable requires both a registered base name and a
registered, scientifically appropriate suffix. Preserve the distinction in
analysis rather than pooling methods implicitly. Future suffixes must first be
defined in the shared registry; do not introduce ad hoc exceptions per script.

`codebook_resolve_variables()` in `R/codebook_workflow.R` validates exact names and
registered variants and supplies base construct, suffix and readable labels.
Exact registered names take precedence, including historical names containing
underscores. An unknown or ambiguous variant fails explicitly. Downstream app
label/selection support remains separate work before publishing new variants.

## Review topics: commonality and useful distinctions

Topics must both group related articles and describe substantive or useful
differences. Reuse existing topics when they describe the article adequately;
create a new one when reuse alone would conceal an important distinction or fail
to describe a meaningful aspect of the article. A topic may originate in one
article; no minimum article count is required. Neither incidental mentions nor
minor wording differences alone justify a new topic.

For each article:

1. Identify its central aims, framework, applications and contributions, using
   the actual article and retaining page/source evidence.
2. Compare them with existing topic definitions and assign the relevant topics.
   Topics are not mutually exclusive; there is no fixed quota per article.
3. When needed, add a concise new code, readable label, definition distinguishing
   it from neighbouring topics, and a broad group (reuse a broad group if suitable).
   Record the motivating article/evidence and the reason for adding the topic.
4. Update the codebook before validating/exporting the article. The current
   builder checks membership in `data/topics_codebook.csv`; it must continue to
   reject an unregistered code rather than treating a typo as a new category.
5. Keep CSV and the derived XLSX interface in sync through
   `scripts/coding/03_update_codebooks.R`. Proposals contain the existing columns
   plus `paper_id`, `change_date`, `reason` and `source`. New topics require an
   `Explanation`; new variables require a definition in `notes`. Updates are
   additive and idempotent and recorded in `data/3.meta_data/codebook_changes.csv`.
   The compatibility command also checks consistency rather than overwriting CSV
   from a manually edited XLSX. Substantive revisions/merges require a separately
   designed migration, not the addition command.

Do not force an article into a misleading old topic merely to avoid extending
the codebook, or create a synonym for an adequate existing category. Reuse plus
useful specificity is the aim, not a permanently frozen or maximally fragmented
taxonomy. The 31-field descriptive dictionary is separate from these evolving
variable and topic codebooks.

An incremental topic addition does not automatically recode historical articles.
Record potentially affected earlier papers for the later harmonisation pass.
Retain previous values/evidence and version any substantive recoding or merging.

## Outputs and completion

Preserve raw downloads under `open_data/raw/<paper_id>/`. Save coded individual
data under `open_data/individual_data/<matrix_id>.csv` (optional XLSX companion).
The whole `open_data/` directory stays Git-ignored.

Stage each sample under `data/3.meta_data/extraction/<paper_id>/<matrix_id>/`,
including its own checks, pair Ns, summaries and extraction manifest. The labelled
CSV and square XLSX are in that sample's `matrices/` subfolder. Acquisition/source
manifests are shared at paper level. This prevents samples from overwriting each
other's audits. The single-sample October pilot retains its existing paper-level
audit layout for compatibility; integration recognises both layouts.
The later analytical
integration step will promote these with their validated coding rows to
`data/3.meta_data/matrices/`; do not create an unmatched active matrix prematurely.
In the CSV include an explicit row-label column; in the legacy XLSX keep the
existing square-column layout so the current builder can reconstruct row names
from column names. Version pair Ns, source
checksums, mappings and aggregate checks under `data/3.meta_data/extraction/`.
Do not include participant identifiers or observations in those versioned audits.

A matrix is ready only after source, eligibility/ID, scoring, naming, sample N,
symmetry, range and missingness checks. Diagnose non-positive-semidefinite results
possible with pairwise missingness; do not silently repair observed correlations.
Prepare the article outputs first. `scripts/coding/04_integrate_batch.R <batch>`
then validates coding against a fresh temporary rebuild, checks extraction
input/output hashes, copies ready matrices into the common matrix directory,
registers the batch and rebuilds final CSV/XLSX outputs. Review coding can advance
while a sample awaits extraction; availability never changes eligibility.

## October quantitative pilot

`scripts/extraction/articles/0145.R` implements the common structure for Study 2.
It uses the released author-selected original-item dataset (N=973), checks BESSI
and HEXACO correspondence to the public session data, and preserves missing
blocks. Selection based on private comments is not independently reproducible.
The user confirmed distinct persuasion/perspective scoring on 2026-11-04; the
source object's name collision is documented in `scientific_decisions.csv`.
Core/cross-loading weights retain their 1:0.5 ratio and are normalised by available
weight to keep BESSI means within 1-5. The source's 80% response threshold remains
explicit. Response code -9 is missing, not a score.

The staged output has 61 variables: 32 BESSI facets, five BESSI domains, six HEXACO
domains, observed cognitive/political/environmental measures and sex/gender.
Age is the 62nd column in the local individual export. The released data contain
23 wholly missing HEXACO blocks; pair N ranges 790-973. The small negative minimum
eigenvalue (approximately -4.13e-5) is recorded without repairing correlations.
Observed components remain separate from the article's latent factor composites.
Fifteen evidenced variable-codebook additions are registered; the existing two
review topics remain adequate. Input/output SHA-256 values are retained in the
extraction manifest. October is now integrated: the 0145a matrix is in the common
matrix directory, three new review papers are included, and the final datasets
contain 38 quantitative samples and 50 review papers. Historical values and
eligibility remain unchanged. Integration registries and per-row provenance keep
monthly coding distinct from historical data that have not been reassessed.

## Bibliographic dates and monthly readiness

Record the actual export/download date separately from the publication-month
batch. `scripts/coding/05_set_download_date.R <batch> <YYYY-MM-DD>` records a
human-confirmed date in a stable per-batch provenance CSV, updates article JSON
sources/evidence and preserves prior source versions. Rebuild coding and rerun
guarded integration afterwards. Do not substitute the PDF or dataset retrieval
date, the current date or the end of the publication month. Dates that remain
unknown stay explicitly missing; conflicting existing dates require reconciliation.

The monthly R stages accept explicit batch IDs. New-month regression fixtures
cover review-only retention, no retained articles, independent review readiness,
multiple sample exports, source/date history and repeatable integration. These
fixtures are isolated tests, not invented operational bibliographic records.
Actual future papers still require source acquisition, structured scientific
coding and article-specific R scoring using the common convention; unavailable
data and scientific ambiguities remain explicit. The November operational batch
has not been run before real inputs are supplied.
