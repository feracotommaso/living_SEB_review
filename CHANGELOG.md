# Changelog

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
