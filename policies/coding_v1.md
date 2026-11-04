# Descriptive coding dictionary v1.0.0

The canonical dictionary is `config/coding/legacy_31_fields.csv`. Its 31 rows
preserve the exact historical analytical column order. The derived human interface
is `data/coding_dictionary.xlsx`. `rule_status` distinguishes agreed methodological
rules, implementation conventions and unresolved legacy mappings/definitions.

## Scope and units

Review-only papers receive bibliographic information, review topics, framework
relation tags and coding provenance. Sample demographics and measure details are
not extracted merely to fill an old worksheet. Empirical review-only fields are
`not_extracted_for_review_only`; theoretical inapplicable fields are
`not_applicable` in the future structured extraction, even if both export blank.
Historical sample descriptions are retained unchanged until explicitly reviewed.

Quantitative coding follows the eligible publication components established at
full text. A publication label such as Study 2 is retained separately from the
quantitative sample letter. Existing IDs are permanent. No placeholder sample is
created for an ineligible component.

## Analytic N

`n` is the reference analytic sample size after relevant exclusions. One reference
N is sufficient for the current matrix export even when individual correlations
have somewhat different N. Record known differences, missing-data handling,
reported N range and page/table source in `notes`. Do not invent a range or replace
analytic N with the recruited sample size. Per-correlation N is a future extension.

## Occasions and longitudinal extension

Current analyses use contemporaneous correlations at the first eligible occasion,
preferably before an intervention. A longitudinal study keeps `study_design=long`.
Record the selected occasion's original publication label in notes/provenance.
Later occasions are not currently included. They remain the same sample if added
later, with explicit occasion metadata and possible matrix suffixes such as `_t2`;
the existing matrix ID remains unchanged. Within-occasion and between-occasion
correlations must be distinguished. Their future use requires analytical handling
of repeated observations, rather than treating waves as independent samples.

## Broad age categories

Use the described population and age range, not the mean alone. Categories are
deliberately broad: `children` for preschool populations (roughly under 6),
`school_age` for school-age children/adolescents, `young_adult` for predominantly
young adults (roughly 18–29), `adult` for general or heterogeneous adult samples,
and `older_adult` for populations specifically selected as older adults (roughly
65+). These are operational guides rather than automatic numeric thresholds.
Mixed adult age groups remain `adult`. Explicitly note samples that also include
children/adolescents. Do not automatically reclassify historical records.

## Topics and historical corrections

Keep the existing topic taxonomy for this release. The exact spelling alias
`nomoligicalnet -> nomologicalnet` is versioned in
`config/coding/topic_aliases.csv`. Historical full-text and basic_info workbooks
retain original values; the compatibility builder normalises their exports and
records before/after values in `data/3.meta_data/topic_corrections.csv`.
The active topic codebook is corrected in place by a targeted shared-string patch
that preserves its other workbook parts, with a canonical CSV counterpart.

The legacy `assessment` code for paper 0066 was confirmed by the user as an error
on 2026-11-04 after inspecting the paper. It maps to `skillassessment`, retaining
`theorydevelopment`; the alias and before/after audit preserve the correction.
Other unrecognised codes enter `data/3.meta_data/topic_review_queue.csv` and are
not guessed or automatically merged. Future AI recoding must first version the revised taxonomy, preserve old
topics and evidence, and then apply the same rules across all review-eligible papers.

## Matrix 0104a

The matrix file corresponds to the correct paper, DOI
`10.1007/s11482-026-10590-x`. The historical decision and basic_info row classify
paper `0104` as review-only with quantitative exclusion `dataQuality`; the stored
report-level placeholder matrix ID is `0104`, not quantitative sample `0104a`.
The old raw-data recipe prepared a baseline matrix anyway. This is an inactive
prepared artifact, not proof of quantitative eligibility and not an ID typo.

`config/coding/matrix_exclusions.csv` records this reconciliation. The builder
preserves the physical matrix and recorded scientific decision and explicitly
excludes it from analytical input. It fails if that exclusion no longer matches
the report's recorded route/reason. The underlying data-quality judgment is not
re-adjudicated by this maintenance change.

The old builder contributed 18 entirely missing effect columns from that
unmatched matrix even though it contributed no analytical row. Excluding the
inactive matrix removes only those empty columns: 37 quantitative rows remain,
and all populated correlation values and analytic Ns are unchanged. Removed
column names are in `data/3.meta_data/removed_empty_effect_columns.csv`; the
pre-maintenance numeric contract is frozen under `tests/fixtures/coding/`.

## Compatibility boundary and checks

`data/3.2cor_to_metadata.R` retains the established 31-column prefix, historical
sample identifiers and correlation values. It writes CSV companions and a matrix
inventory. Existing quantitative rows lacking a same-named matrix are reported as
`missing_matrix_file` without an eligibility change. Any new unrecognised matrix
fails until reconciled explicitly.

The current legacy review workbook continues its established first-row-per-paper
selection, including the historical meta-only record. Replacing that behaviour
with an explicit review-eligible view and an all-included-paper view belongs to the
new extraction export, and must not be hidden inside a spelling correction.

Run `Rscript scripts/coding/01_update_legacy_compatibility.R`, then
`Rscript data/3.2cor_to_metadata.R`. Run `Rscript scripts/screening/99_run_tests.R`
for the combined suite and the October regression command specified in AGENTS.md.

## First descriptive batch

`R/article_coding.R` and `scripts/coding/02_build_article_coding.R` implement the
first descriptive-coding stage. Evidence-based per-paper JSON inputs live under
`data/3.meta_data/coding/artifacts/<batch>/`. The builder requires complete retained
paper coverage, matching full-text identities/DOIs, known topics, typed analytic
values, field-level source evidence and eligible publication components. It
rejects sample coding for review-only papers and later-occasion entries.

The first eligible occasion is marked explicitly. A permanent sample registry
assigns quantitative letters and matrix IDs once; historical papers require an
explicit crosswalk before assignment. Review-only report placeholders in the
31-column compatibility sheet do not create quantitative matrix assignments.

Outputs are separate papers, samples and evidence CSVs, the 31-column compatibility
CSV, and a derived workbook. They stay in the coding batch directory and do not
append to historical basic_info or create correlation matrices. New compatibility
exclusion fields carry resolved v2 reason codes; original historical codes remain
preserved. They are not inputs to a new screening decision.

The October pilot codes papers 0144, 0145 and 0148. Only Study 2 of 0145 receives
sample a / matrix identifier 0145a. Its analytic N is 973 (PDF p.10), mean age
26.05 and female percentage 51.3 (p.11), BESSI-96 and HEXACO (pp.11–12).
The observed 18–49 range is coded adult under the mixed-adult convention. The
BESSI-96 long/short label follows prior legacy usage documented in
`config/coding/measure_forms.csv`, not a new validation judgment. The actual
bibliographic download date stays unverified and blank. OSF access has not
been checked, and no effect-size extraction is claimed.
