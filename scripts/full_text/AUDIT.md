# Full-text consolidation audit — 2026-11-04

Baseline: `workflow/dedup-2026-11`, HEAD `7eeb82e` (`full text`).
The working tree was clean and one commit ahead of its remote. No branches or
history were changed during consolidation.

## Reconstructed workflow

Raw exports → guarded deduplication/promotion → title/abstract input → conservative
screening → retained CSV → full-text registry/manifest → eligibility decisions
→ detailed extraction → historical basic_info/matrices → `data/3.2cor_to_metadata.R`
→ `metadata.xlsx` and `review_data.xlsx` → paper, tutorials and Shiny app.

The legacy handoff `data/2.full_exclude.R` processes monthly manually extracted
workbooks. Eligibility cannot replace detailed extraction. The consolidated
handoff exports resolved routing separately and does not write to
`basic_info`, analytical outputs or historical coding files.

## Baseline inconsistencies and resolution

| Finding | Consolidation |
|---|---|
| October workbook was correct, but JSON inputs were absent | Explicit operational-workbook migration with origin notes; three adjudications retained |
| JSON schema existed but the reader did not execute it | Typed local validation before flattening; same contract after adjudication |
| Workbook described as canonical despite repository CSV convention | Canonical decision/audit CSVs and derived four-sheet XLSX |
| Manifest CSV had bare DOIs and six stale missing-PDF statuses | Step 01 refreshes CSV/XLSX together; all six local PDFs present |
| Rebuilding discarded the adjudication audit | Replay by exact identity; retain packets; refuse unfinalized edits and deletions |
| Alias report could be deleted if no aliases were rediscovered | Merge persistent aliases; reserve IDs even without legacy sources |
| IDs could be repadded after 9999; fractional IDs were truncated | Minimum padding per ID; strict positive-integer validation |
| Upstream IDs were recomputed/rejected after metadata changes | Preserve upstream IDs and reconcile against both ID and bibliographic key |
| Explicit batch could still read configured month's input | Derive upstream directory from selected batch |
| No executable full-text tests | Identity, schema, routing, migration, audit, handoff and frozen October tests |
| Workbook lacked versions and a complete codebook; resolved rows looked open | Version/checksum fields, complete codebook, wrapped layout, highlight open rows only |
| Generated XLSX contained unused relationships to missing default drawing/VML parts | Remove only missing default parts and references during R export; preserve all real parts and hyperlink targets |
| Setup/monthly guides described package copying and ended at abstracts | English local setup and complete monthly guide; obsolete package-only files removed |

The CSVs in `tests/full_text/` have distinct roles: input identities,
original AI/human comparison, and final independent expectations. They are not
duplicate operational databases. Legacy monthly workbooks remain scientific
history. No alternative full-text PDF directory is introduced.

## Retained technical debt

`paper/livingSEBpaper.qmd` and `paper/livingSEBresults.qmd` use positional ranges
such as `download_date:additional` to separate metadata from correlations.
`R/utils.R::data_combine` joins extracted basic information by `matrix_id`.
This consolidation does not add fields to these analytical contracts.
A future change should introduce explicit metadata/effect column contracts and
compare output columns, identifiers, effect counts and numerical values before
touching paper/app code. Full-text eligibility does not automate detailed
sample/measure/correlation extraction or rewrite historical coding.

## Validation record

R 4.5.1 is available. The inherited locale initially broke accented Windows paths;
process-local UTF-8 settings fixed execution without system changes.
Baseline title/abstract tests and the 14/14 October regression passed. The
consolidation adds full-text tests and executes the October operational workflow
with six reused IDs, no new IDs, three retained adjudications and unchanged routes.
Scientific policy remains `2.0.0`; workflow/schema enforcement is `2.2.0`.

Final validation: 130 test assertions passed, zero failures/warnings/skips;
title/abstract regression and the standalone AI CSV validator both passed 14/14.
The October operational steps 01–04 completed with all six local PDFs available.
Independent saved-XLSX checks verified CSV agreement, text IDs, artifact checksums,
DOI hyperlinks, tables and frozen panes. A baseline comparison confirmed 240
protected files unchanged, including historical coding, registries, fixtures and
paper/app analytical contracts. Git whitespace checks passed.

All four decision sheets and the retrieval manifest were rendered. The bundled
renderer can visually coerce numeric-looking text IDs and empty shared strings;
native saved values were therefore also verified independently with readxl and
read-only openpyxl inspection, with dimension-based layout previews as a cross-check.
No native Excel application session was used. No automated tests were skipped.
