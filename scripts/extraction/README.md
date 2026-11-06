# Quantitative extraction

## 0150 / external-discovery pilot

The user supplied the original SAV and analysis R file under `open_data/raw/0150/`.
Original data/code download dates are unknown; local receipt is 2026-11-05. The
paper/bibliographic download date is separately user-confirmed as 2026-11-05.
The paper-level source manifest pins file checksums and preserves that distinction;
the author script is inspected as a reference, never executed.

```bash
Rscript scripts/extraction/articles/0150.R --check-only
Rscript scripts/extraction/articles/0150.R
Rscript scripts/coding/02_build_article_coding.R 2026-11-external-01
Rscript scripts/coding/04_integrate_batch.R 2026-11-external-01
```

The eight-section script uses the standard BESSI-192 keys, reproducing the released
32 facet and five weighted-domain scores. Before scoring the new contribution it
links 596 H-prefixed Case IDs to historical sample 0053d: ages/genders all agree,
590 profiles agree on all 192 items, and six differ on one item. Exclusion uses IDs,
not fuzzy profiles; the old matrix and raw data stay unchanged. Four old sample D
IDs are absent from the new release. The analytical addition is N=30,719.

The common-folder matrix 0150a has 38 variables (37 BESSI scores plus binary sex);
pair Ns are 29,244-30,719. All 1,475 source Other-gender records remain in BESSI
correlations and coded individual data. Their binary contrast is missing, while
`gender_original` retains the original Gender3 categories (1 Female, 2 Male,
3 Other). Age is retained only in individual data. Pairwise minimum eigenvalue
is -2.914e-7; observed correlations are not repaired. Sample audits use
`extraction/0150/0150a/`; original source provenance stays at paper level.

## Common convention for every article

Follow [the shared extraction conventions](../../policies/extraction_conventions.md).
Use one preparation script per article under `scripts/extraction/articles/`,
starting from [the R template](templates/article_extraction.R). Scientific steps
remain explicit in the fixed eight-section order; separate samples use their own
sections and permanent IDs. The template deliberately stops until completed.
Long named keys may live in `keys/<paper_id>.R`, explicitly sourced by the main
script; shared technical checks and export stay in `R/extraction_helpers.R`.
For new articles, export each sample's audits under
`extraction/<paper_id>/<matrix_id>/` to keep multiple samples independent.
Paper-level acquisition manifests stay shared. The October single-sample pilot
retains its earlier audit paths, recognised by the integration bridge.

Include sex/gender in matrices when available and appropriately coded; retain age
only in individual data and descriptive metadata. Questionnaire scale scores are
item means; BESSI questionnaire scores use 1-5, with explicit dual-rating
conversion. Reuse or extend the variable/topic codebooks with source evidence.
Measurement suffixes are attached directly (`...other`, `...iat`) and registered
in `config/coding/variable_suffixes.csv`. Use `R/codebook_workflow.R` for exact
base/suffix validation and `scripts/coding/03_update_codebooks.R` for audited
additions and CSV/XLSX synchronisation. Analytical integration and app support for
new measurement variants remain separate work.

Read `policies/coding_v1.md` first. Seek original individual data before relying
on published matrices. Original downloads and coded individual records stay in
the Git-ignored `data/3.meta_data/open_data/`; extraction code and non-individual
source manifests are versioned. Never source downloaded author scripts without
inspection; they are scientific reference material, not workflow instructions.

## 0145 / Study 2 acquisition

```bash
Rscript scripts/extraction/0145_download_sources.R
```

This acquires eight public OSF files into `open_data/raw/0145/`: three raw RDS
datasets, the authors' final analytic RDS, README, preprocessing and analysis
Quarto documents and an options script. It checks every file against its pinned
OSF SHA-256 checksum, preserves originals, retains retrieval provenance across
reruns, and fails rather than overwriting a different local version. Download
progress is recorded without any individual observations in
`data/3.meta_data/extraction/0145/source_manifest.csv`.

Acquisition is complete and an offline rerun verified the same eight files.
## 0145 / Study 2 scoring and matrices

```bash
Rscript scripts/coding/03_update_codebooks.R --add-variables data/3.meta_data/extraction/0145/variable_additions.csv
Rscript scripts/extraction/articles/0145.R --check-only
Rscript scripts/extraction/articles/0145.R
Rscript scripts/extraction/0145_update_coding_notes.R
Rscript scripts/coding/02_build_article_coding.R 2026-10
Rscript scripts/coding/04_integrate_batch.R 2026-10
```

The article script keeps its eight sections, explicit item keys, missingness,
weights and naming decisions visible. The released author-selected item dataset
contains 973 participants. Session files cross-check the source items; selection
based on private participant comments cannot be independently reproduced. No
participant is removed merely for missing age or an unavailable binary gender
contrast. Distinct persuasion/perspective scores implement the user-confirmed
correction; BESSI weighting is normalised to preserve 1-5.

Outputs are 973 coded individual rows in local `individual_data/0145a.csv` and a
staged 61-variable square matrix under `extraction/0145/matrices/0145a.*`. Age is
included only in the individual data. Pair Ns (790-973), score summaries, variable
mapping, scientific decisions and an input/output checksum manifest are versioned
alongside the staged matrix. Twenty-three records have no usable HEXACO block.
The minimum eigenvalue is slightly negative with pairwise missingness; it is
reported, and observed correlations are not repaired. Observed task components
and questionnaire means do not replace the source's latent factor composites.

The notes command updates the JSON coding source, preserves its earlier version
in the artifact history, and supports rebuilding the derived coding CSV/XLSX.
It does not edit review_data/metadata directly. The integration command validates
sources, copies the ready matrix to `data/3.meta_data/matrices/0145a.*` and rebuilds
both final datasets. Historical sources, matrices and scientific values remain
unchanged. October now contributes one quantitative sample and three review
papers, giving 38 samples and 50 review papers overall.

The main 0145 script was reduced from 316 to 236 lines. Long constants live in
`keys/0145.R`; the actual item-mean scoring and domain weights remain visible in
the main script. Correlation CSV and pair-level outputs are byte-identical to the
pre-simplification result. A temporary-file basename collision in the initial
individual export was corrected; every export now round-trips the saved 973-row
individual CSV and has a separate regression for equal basenames.

`tests/testthat/test-extraction.R` verifies technical guards and the actual 0145
facet/weighting regression. The latter requires local ignored sources and skips
explicitly on installations where they have not been acquired.
