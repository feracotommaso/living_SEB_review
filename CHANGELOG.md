## 2026-11-12 — Format the app manual and retain one APAquarto extension

- Replaced APAquarto manual output with a self-contained HTML guide with left navigation and a standard six-page A4 PDF. Preserved the existing PDF URL, manual prose and citation includes. Improved heading/list hierarchy, mobile wrapping and long code lines; removed an empty References section.
- Added HTML manual links to the site source and README. Only the scientific paper now declares APAquarto; retained its 49 extension files unchanged and removed the unused root and manual copies.
- Validation: manual HTML checked at desktop/mobile widths with no horizontal overflow; PDF pages visually checked. The paper's four output formats compiled in an isolated layout-only fixture with analytical chunks omitted; warnings for omitted cross-references, Word SVG conversion and Typst's Times font remain environment/test limitations. Published paper files and scientific data were not regenerated.

## 2026-11-11 — Share analysis helpers between app and local workflows

- Consolidated the ten analysis, review and bibliography helpers in `app/R/utils.R`, including the former correlation-pair module. `R/utils.R` is now a compatibility loader; app startup uses the same Excel download helper.
- Retained existing function behavior and used explicit `dplyr::select()` in review filtering as well as matrix preparation. Removed duplicate implementations and obsolete commented model-building functions.
- Added standalone-app loading and masked-function checks, alongside the existing correlation-pair tests.
- Validation: full R suite and October regression (14/14) passed. A standalone app bundle loaded the public data and passed server checks for data selection, review and five meta-analysis models. The deployment file list includes the shared helpers. The living results report reran with all six OSMASEM status codes zero and all four analytical tables unchanged.

## 2026-11-11 — Organize materials and reconstruct update history

- Moved the search strategy to `paper/materials/search_code.docx` and replaced the manual search history with `data/search_results_history.csv` and its derived Excel interface. The original workbook remains in Git history.
- Reconstructed acquisition-batch counts from the existing import manifests, live registry and documented screening, using the current included datasets. Every distinct publication belongs to its first recorded acquisition batch. Counts describe its current status, not when screening occurred.
- Integrated history generation into `aggiorna_risultati()` and the same guarded save as analytical outputs. Historical publisher DOI resolver URLs are recognized without changing source coding.
- Archived the nine local analysis trial files under Git-ignored `archive_local/analysis_trials/`, verifying their hashes. Removed the unused empty matrix, three unreferenced default TIFF plots, generated LaTeX float files, and the standalone Quarto example and its sample image.
- Retained utility implementations, historical extraction scripts, Quarto extension copies and published document paths for the separate decisions discussed with the user.
- Validation: full R suite and October regression (14/14) passed, including a simulated future batch; exported Excel data and totals match the CSV reconstruction. Automatic workbook preview was unavailable because the bundled rendering runtime crashed during import.

## 2026-11-11 — Reconcile bibliographic counts before merging

- Included the historical May `.xlsm` screening export and counted distinct publications across manual exports and canonical batches. All 370 historical publications have documented screening; the previously reported 20-record gap was a counting error.
- Reconciled two user-approved registry duplicate pairs through guarded promotion on November 10, retaining source IDs and provenance. Historical snapshots and original exports remain unchanged; the live registry now contains 385 publications.
- Corrected report counts: 614 imported records, 229 duplicates, 385 screened publications and 149 full texts (one repeated historical full-text row removed from the count). The 51 review papers, 31 quantitative papers and 39 samples are unchanged.
- Preserved the user's current report prose; removed only the obsolete discrepancy node from its diagram.
- Validation: complete R test suite and October regression (14/14) passed; Quarto HTML regenerated with all six OSMASEM optimizer status codes equal to zero.

## 2026-11-09 — Refresh the living results report

- Rendered `paper/livingSEBresults.qmd` with the current local datasets (51 review papers; 31 quantitative papers, 39 samples), preserving the statistical model specifications and input data.
- Replaced remote-release metadata and flat-folder counts with local data provenance, import manifests and canonical screening CSVs alongside preserved manual exports. Counted October once and included external intake.
- Recorded 614 imported records, 387 registered unique records, 367 documented abstract screenings and 150 full texts. The historical 20-record screening discrepancy is explicitly unresolved, not an invented exclusion.
- Checked finite meta-analytic estimates and all six OSMASEM optimizer codes (all zero). Corrected threshold wording in the narrative and refreshed the figures. Report-input tests passed; browser file preview was unavailable, so figure images and HTML content were inspected directly.

## 2026-11-09 — Second cleanup audit

- Rechecked tracked additions, local automation folders, empty directories and references. Removed the obsolete combined reset script; the history-finalization message now names the explicit full-rebuild reset.
- No reset was executed. Retained all scientific data, bibliography, registries, test expectations, compatibility helpers and local RStudio session folders; no pre-automation manual file changed.
- The empty local `author_suggestions` directory was also removed; `external` remains the sole current intake directory for external discoveries.

## 2026-11-09 — Remove superseded AI workflow files

- Audited additions against pre-automation commit `0244692`; no pre-existing manual file or scientific output changed.
- Removed the completed migration script, obsolete monthly JSON date helper and legacy routing exporter. Download-date editing is covered by the cumulative workbook test.
- Removed the old full-text consolidation report, duplicate pilot summaries and separate dictionary XLSX; the current guide, original comparison CSV, test fixtures and cumulative dictionary sheet remain.
- Removed seven empty workflow directories. Preserved raw inputs, private data/PDFs, bibliographic registries, provenance, manual adjudications and the locally rendered guide HTML.
- Removed files remain recoverable from commit `d6dee47`.

## 2026-11-08 — Human workflow consolidation

- One screening workbook per batch and one cumulative coding workbook, with explicit imports, stable IDs and protection for unsaved edits.
- Added `workflow.R` for preparation, correction, extraction, before/after review and final updates. Scientific scoring remains visible in per-article scripts.
- Consolidated coding, evidence and source provenance; new extraction exports matrices to the common folder, pair Ns by matrix and one aggregate summary.
- Retired redundant monthly coding artifacts, staging matrices and separate screening workbooks; retained originals and historical scientific scripts.
- Compared all final values with checkpoint 0e1daaa and reproduced the 0150 individual and matrix CSVs exactly.
- Validation: full R suite passed, October screening 14/14, 39 quantitative samples and 51 review articles unchanged; workbook round trips and rendered previews checked. Original BibTeX and historical scientific scripts remain unchanged.
- Skill metadata checked with R's YAML parser. The optional Python `quick_validate.py` could not run because PyYAML is absent in the bundled runtime.

# Changelog

## External discovery intake — 2026-11-05

- Added one provenance/naming rule for author/expert suggestions, backward/forward
  citations, manual searches and other external sources. Keep original BibTeX/RIS
  bytes, receipt/download dates and discovery details in a checksum-pinned manifest.
- Isolate external acquisition batches with `YYYY-MM-external-NN` and an explicit
  process-local selector, retaining the ordinary monthly configuration. Recognise
  these batch IDs through full-text/coding/integration entry points.
- Parse external BibTeX with its abstract and publication type intact. Keep
  discovery source in CiteSource provenance, and test later database duplicates
  without double-counting papers or losing either source.
- First real case: author-referred Napolitano et al. (DOI
  `10.1177/27000710261487580`), one new record, no duplicate candidates. Promoted
  the 387-record bibliography only through the guarded script; title/abstract
  retained and persistent paper ID 0150 assigned. The publisher's automatic PDF
  download returned HTTP 403; the user subsequently provided the published PDF,
  original SAV and author R code. Data/code download dates remain unknown;
  paper/bibliographic date is confirmed as 2026-11-05.
- Read the complete full text and resolved sample overlap by linking 596
  H-prefixed IDs to 0053d, with age/gender agreement for all links (590 full item
  matches, six one-item differences). Exclude these from the new contribution,
  retaining historical data unchanged. All 32 facet and five weighted-domain
  scores reproduce the released scores; source Table 1 agrees within rounding.
- Integrated 0150a: N=30,719, 38 matrix variables, actual pair N=29,244-30,719;
  retain Other-gender participants and source categories in individual data, age
  outside matrices, and the unrepaired minimum eigenvalue (-2.914e-7) in notes.
  Outputs now contain 39 quantitative samples and 51 review papers, with external
  origin distinguished from monthly coding. Reused existing age/gender/development
  topics and variable names; no new construct codes or historical recoding.

## Stable checkout checksums — 2026-11-05

- Pin the recorded line endings of hashed coding, extraction and revision inputs
  in `.gitattributes`, so Git checkout does not invalidate scientific manifests
  through platform-dependent text conversion. Raw bibliographic exports are
  outside these rules; working-tree data and numerical values are unchanged.

## Historical 0132 extraction correction — 2026-11-05

- Corrected the existing 0132 block in `data/3.1opendata_to_cor.R`: apply the
  documented careless-response exclusions (598 students), average both social
  status items and all five test-anxiety items without an additional reversal.
- Average parent scale scores and coded education levels per student before
  linkage (321 reports, 315 students), avoiding duplicated self-report rows.
  Document the single-N metaSEM limitation; keep age out of the matrix and do
  not exclude individual records solely for missing age.
- Validated the corrected block in memory with exports disabled; the user then
  regenerated the historical matrix and individual XLSX with that block.
- Reconciled the analytical builder through an exact before/after revision
  ledger, preserving the original numeric fixture and guards on all other
  samples and on subsequent changes to 0132a. Added regression coverage for
  unauthorised matrix/N changes and invalid revision transitions.
- Verified the user's regenerated files against the corrected script, added
  canonical CSV companions and a 276-pair before/after audit with actual Ns and
  source/output checksums. Rebuilt metadata/review outputs with the N caveat
  sourced from the revision ledger; original downloaded and basic_info files stay
  unchanged. All other 36 historical numerical contracts remain unchanged.

## Bibliographic date and reuse checks

- Recorded the user-confirmed 2026-11-02 export date in all October coding sources,
  with per-batch provenance/history and a reusable date-setting command.
- Added valid empty-table contracts for review-only/no-retention batches and
  isolated audit/manifest directories for multiple samples in a new paper.
- Added regression fixtures for future-month coding shapes, date history and
  repeated date application; operational future bibliographic records are untouched.

## Script simplification and October integration

- Reduced the main 0145 script from 316 to 236 lines, separating named long keys
  and common technical checks while retaining explicit scientific operations.
  Correlation and pair-level CSVs are byte-identical to the preceding extraction.
- Fixed a temporary-file basename collision that had copied the matrix CSV into
  the individual-data destination; the actual individual export now has 973 rows
  and 62 columns, with full saved-value checks and a regression for this failure.
- Added guarded batch integration with fresh coding validation, extraction hashes,
  duplicate/conflict guards, common-folder matrix copies and separate readiness.
- Integrated October: 38 quantitative samples and 50 review papers, retaining every
  historical populated correlation and analytic N and the 31-field interface.
- Shared exact full-variable pair readers between paper/app, supporting names
  containing underscores and distinct attached measurement suffixes.

## 0145 quantitative extraction — 2026-11-04

- Added the first operational article script in the agreed eight-section style,
  producing 973 local individual records and a staged 61-variable correlation
  matrix with sex/gender; age remains in individual data only.
- Implemented the user-confirmed persuasion/perspective distinction, explicit item
  means, response-code handling, normalised BESSI domain weights and source checks.
- Added 15 defined variable-codebook entries with provenance; existing topics and
  historical codebook values are preserved. Added readable formatting for new rows.
- Recorded pair N=790-973, 23 missing HEXACO blocks, author-selected cohort limits
  and an unrepaired small negative eigenvalue, with input/output checksums.
- Updated source JSON notes and rebuilt descriptive coding, preserving its prior
  version. No matrix or coding was promoted into historical analytical outputs.

## Codebook maintenance — 2026-11-04

- Added CSV-led, staged additive maintenance for variable and review-topic
  codebooks, including definitions, source provenance, change ledger, conflict
  detection and idempotent reruns. Existing codebook values remain unchanged.
- Added the canonical variable CSV and exact base/suffix resolution for attached
  `other`/`iat` variants, including base names that contain underscores.
- Changed the compatibility maintenance command to verify CSV/XLSX consistency
  instead of overwriting canonical CSV from a manually edited workbook.

## Agreed next-stage extraction rules — 2026-11-04

- Added shared article-extraction conventions, a guarded eight-section R template
  and attached measurement-suffix registry (`other`, `iat`). Scientific scripts
  retain explicit scoring; age stays outside matrices and within individual data.
- Clarified evolving review topics: reuse common categories and add new ones for
  meaningful distinctions, preserving evidence and separating historical recoding.
  Suffix-aware readers and canonical codebook-maintenance commands remain pending.
- Prioritise original individual data over published matrices, preserve immutable
  local downloads and coded individual datasets in the Git-ignored `open_data/`
  directory, and version reproducible R scoring and source/checksum provenance.
- Record the agreed October-first integration sequence, separate review and
  quantitative readiness, and preserve historical eligibility until reassessment.
  The integration/export stage remains to be implemented.
- Acquired eight public Study 2 sources for 0145 with pinned OSF SHA-256 checks;
  added a repeatable acquisition script and non-individual source manifest. Raw
  data stay Git-ignored. Scoring and correlation extraction remain pending.

## Descriptive coding 1.0.0 — 2026-11-04

- Applied the user-confirmed `assessment -> skillassessment` correction for 0066,
  retaining `theorydevelopment` and recording the original coding in the audit.

- Added the versioned 31-field CSV dictionary, derived Excel dictionary and agreed
  coding rules for review-only scope, analytic N, broad ages and selected occasions.
- Added a validated descriptive-coding builder and October pilot: three papers,
  only Study 2 of 0145 as quantitative sample a (analytic N=973), with page evidence.
- Corrected the exact nomoligicalnet spelling alias in the active codebook and
  compatibility exports while preserving original historical coding files.
- Reconciled 0104a as an inactive matrix under the recorded review/dataQuality
  decision; removed its 18 entirely missing effect columns without changing any
  populated correlation, analytic N or quantitative sample.
- Added CSV companions, explicit matrix/topic audit queues and scientific numeric
  regression fixtures. No new correlation matrices or coding data were promoted.

## Full-text workflow 2.2.0 — 2026-11-04

- Added the repository-scoped `living-seb-full-text` skill to orchestrate PDF
  assessment, validated JSON output, monthly workbook generation and batched
  adjudication using the existing versioned workflow.

- Consolidated policy 2.0.0 on the current feature branch with typed JSON Schema
  validation, exact coverage checks, evidence and checksum provenance.
- Added canonical decision/audit CSVs and rebuild-safe operational XLSX outputs.
- Recovered October artifacts from its existing workbook, preserving six decisions
  and three adjudications with explicit migration provenance.
- Hardened IDs, alias reservations, batch selection and legacy discovery.
- Added guarded resolved-routing export without assigning samples/matrices or
  changing historical analytical workbooks.
- Added full-text tests and English local-workflow documentation. Removed stale
  package checksums and the un-applied ignore snippet; .gitignore is authoritative.

## 2026-10.3

- Added a versioned title/abstract screening policy, prompt, JSON schema, R helpers, scripts, tests, and Codex/ChatGPT skill.
- Retained reviews, meta-analyses, and journal editorials by policy.
- Changed uncertainty handling to automatic conservative retention (`KEEP_UNCERTAIN`), avoiding routine human screening.
- Added stable DOI/title-based `screening_record_id` values and exact coverage validation.
- Added canonical CSV and human-readable XLSX outputs for decisions, included records, and excluded records.
- Added the independently validated 2026-10 pilot as a regression fixture.
- Added module-specific README files and a monthly workflow guide.
- Replaced the destructive reset with separate current-batch and full-rebuild scripts.
- Made Rayyan comparison optional after the exact 14/14 pilot validation, while still blocking promotion when an existing comparison contains discrepancies.
- Added repository-level `AGENTS.md` instructions and a repo-scoped skill under `.agents/skills/`.

## 2026-10.2

- Added Excel workbooks for all human-review steps while retaining CSV as the machine-readable audit format.
- Historical manual review now uses `history_candidate_pairs_review.xlsx`.
- Current-update manual review now uses `update_candidate_pairs_review.xlsx`.
- Added dropdown validation (`match` / `no_match`), instructions, filters, frozen panes and review-oriented formatting.
- Finalization reads only the decisions from Excel and applies them to the canonical candidate pairs held in the R deduplication result.
- Review rows may be sorted in Excel; decisions are matched back using the pair identifiers.
- Added backward compatibility with legacy `*_candidate_pairs_reviewed.csv` files.
- Added an XLSX mirror of the final new-record screening file.
- Added a multi-sheet Excel workbook for the Rayyan comparison while retaining all CSV comparison outputs.
- Added `openxlsx` as a workflow dependency.

## 2026-10.1

- Corrected the active update to publication month `2026-10`.
- Corrected the historical cutoff to `2026-09`.
- Integrated automatic Scopus BibTeX preamble removal in both historical and current imports.
- Preserved all original Scopus files and wrote normalised copies under `data/0.bib_normalized/`.
- Added per-file import counts, MD5 hashes, and import manifests.
- Improved database detection by inspecting the full path.
- Improved month detection for filenames such as `2026_04_30_scopus.bib` and `25_01_31_scopus.bib`.
- Added deterministic bootstrap `record_id` values to avoid row-number warnings.
- Added a guarded clean-reset script.
- Added a guarded registry-promotion script.
