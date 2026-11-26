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
The variable schema is `Topic, broad, column_name, label, notes, definition,
synonyms`. Definitions are concise operational descriptions for literature
mapping, not instrument scoring keys. Synonyms are optional search terms and
never trigger automatic construct recoding. BESSI facet labels follow the full
names at https://www.sebskills.com/about-seb-skills.html (verified 2026-11-17).
Both additive maintenance and `importa_codebook()` group rows by Topic/broad,
retain within-group order, and export compact Excel rows with wrap disabled.

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
measure. `other` denotes another informant, `iat` an IAT measure, and `task` a
behavioral/performance task. A different
questionnaire alone does not require a suffix. Historical `...other` names remain
unchanged. A suffixed variable requires both a registered base name and a
registered, scientifically appropriate suffix. Preserve the distinction in
analysis rather than pooling methods implicitly. Future suffixes must first be
defined in the shared registry; do not introduce ad hoc exceptions per script.

Approved 2026-11-19: `self` denotes an appraisal of one's own performance in a
specific task when the historical unsuffixed task code denotes observer ratings,
for example `task_socialengagementself`. Keep the observer task codes unchanged
and make their method explicit in the base-code notes. This is distinct from
ordinary global BESSI self-report,which continues to use unsuffixed BESSI names.
Task performance keeps its source metric; the 0049 self-appraisals are on1-6.

`codebook_resolve_variables()` in `R/codebook_workflow.R` validates exact names and
registered variants and supplies base construct, suffix and readable labels.
Exact registered names take precedence, including historical names containing
underscores. An unknown or ambiguous variant fails explicitly. The standalone
app shares the resolver in `app/R/variable_lookup.R`, adds registered variants
actually present in effect-column names to its menus, and displays their method
labels. Aliases aid searching while selections submit canonical IDs. Historical
`task_*` names are retained; the approved `conditionalcooperationtask` code is
explicitly registered and remains distinct from the BESSI cooperation domain.

## Review topics: commonality and useful distinctions

User clarification 2026-11-17: topics identify the article's main scientific
focus. Do not turn outcomes used to validate a measure, or illustrative outcomes
in a resource paper, into a long topic list. Those remain discoverable through
the separately coded construct list. Index each base construct once per article,
not once per instrument, informant or occasion. Domains and distinct subscales
may both be listed when actually analyzed. Quantitative syntheses can have
analyzed constructs despite collecting no new individual data; purely theoretical
discussion is represented by topics. Classify the substantive study design;
ancillary test-retest reliability alone does not require a longitudinal tag.
The approved pilot is integrated in the cumulative coding table. Historical
harmonization proceeds article by article; blank expanded fields mean not yet
assessed. Five article-level fields (main_objective, sample_context,
seb_measurement, study_contribution, constructs_analyzed) extend review exports
without changing the legacy 31 analytical fields. The shared specification is
`app/R/review_coding.R`; quantitative component designs are edited in campioni,
and their union is displayed at article level. The LEGACY_ONLY topic
positiveschooloutcomes is retired: all its article assignments were individually
reviewed by 2026-11-18. The code remains for historical compatibility; new
assignments are rejected rather than automatically mapped to an outcome.

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

Use `source("workflow.R")` and `estrai_articolo(paper_id)` for current extraction.
Original provenance is consolidated in `data/3.meta_data/coding/sources.csv`.
The helper checks original bytes; changing a comment or a codebook label does not
require rebuilding a chain of derived-file signatures.

Export coded individuals under the Git-ignored `open_data/individual_data/`,
matrices directly into `data/3.meta_data/matrices/`, pair-specific Ns into
`data/3.meta_data/pair_n/`, and aggregate checks into `extraction/summary.csv`.
CSV matrices include row labels; XLSX matrices preserve the square-column layout
consumed by historical analysis. Keep participant identifiers out of these exports.

If the documented reference N differs from released rows, pass `reference_n`
explicitly to `wf_compact_extraction()` (as in 0050b). The technical summary keeps
`input_rows` separately; pair Ns must not exceed reference N. Do not delete
records to force agreement with the paper's declared N.

Run `anteprima_risultati()` to compare all current results with the saved datasets.
`aggiorna_risultati()` writes the final CSV/XLSX pairs together. Corrections to
existing correlations or Ns require a stated scientific reason after the readable
comparison; no hand-maintained numeric hash ledger is needed in this route.
Missing matrices remain pending, and non-positive-semidefinite matrices are
reported without silently repairing correlations.

The original 0145 and 0150 source comparisons and agreed scientific decisions
remain in their article scripts and `config/extraction/*_decisions.csv`.
Bibliographic download dates are edited in the general coding workbook. They remain
separate from PDF and individual-data acquisition dates; unknown dates stay missing.
Historical scripts are retained. The sole operating guide is `MONTHLY_WORKFLOW.md`.

## Local manual counterpart during monthly updates

The user-approved manual copy remains entirely under Git-ignored
`archive_local/manual_workflow_*/`. Each automated monthly/external extraction
also creates or updates its local `data/codici_articoli/<paper_id>.R`: one visible
assignment per score, with explicit items, reversals, means, weights and source
missingness. Reuse the local technical export helpers. Do not copy the audit or
registry framework into these scripts and do not invent scoring from a final
correlation matrix. Published-matrix fallbacks remain explicitly distinguished.

Prepare and verify this counterpart while extracting each new quantitative
paper, before `aggiorna_risultati()`. The accepted final update automatically
refreshes monthly full_decisions, generated basic_info, codebooks, source copies,
the scoring index and final manual outputs, then compares matrices, available
pair Ns, metadata and review against the accepted main outputs. This includes
the expanded coding and harmonized topics. A pending local scoring scaffold is
an incomplete task, never a completed backup. Finish it within the automated
batch rather than asking the user to implement it.

Preserve standalone human edits: reconcile a divergence instead of silently
overwriting it. The original historical archive is not rewritten. Only the
generic updater and technical templates are versioned; local article scripts,
data, workbooks, original downloads and provenance snapshots stay ignored.
