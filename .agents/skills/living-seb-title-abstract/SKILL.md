---
name: living-seb-title-abstract
description: Perform conservative title/abstract screening for the Living SEB review from CSV or XLSX bibliographic records. Use when asked to screen a new Living SEB monthly batch, classify records as include/exclude/uncertain, generate the structured AI decisions file expected by the R workflow, or validate that every screening_record_id received one policy-compliant decision. Retain reviews, meta-analyses, journal editorials, and uncertain records; exclude only clear books, out-of-scope records, explicit non-SEB frameworks, or clearly non-peer-reviewed non-journal publications.
---

# Living SEB title/abstract screening

## Source of truth

When operating inside the Living SEB repository, read these project files first:

- `config/screening/title_abstract.yml`
- `prompts/title_abstract_screening.md`
- `data/1.abstracting/<batch>/title_abstract/<batch>_title_abstract_input.csv`

If the project files are unavailable, use the bundled references:

- `references/policy.md`
- `references/output_schema.md`

Do not silently change policy or output columns.

## Workflow

1. Identify the canonical input CSV. Prefer the prepared
   `<batch>_title_abstract_input.csv`; otherwise accept the supplied CSV/XLSX.
2. Read every record and preserve `screening_record_id` exactly. If IDs are
   absent, ask the project R preparation script to create them rather than
   inventing incompatible IDs.
3. Classify each record independently using only its supplied title, abstract,
   document/publication type, journal, and bibliographic metadata.
4. Apply the conservative scientific rules below.
5. Write one decision row per input ID to the exact output path requested by the
   workflow output path. Use the exact column order in `references/output_schema.md`.
6. Run the bundled validator:

   ```bash
   python scripts/validate_decisions.py \
     --input <prepared-input.csv> \
     --decisions <ai-decisions.csv> \
     --policy references/policy.json
   ```

7. If validation passes and R is available in the repository, run:

   ```bash
   Rscript -e 'source("workflow.R"); raccogli_abstract("YYYY-MM")'
   ```

   Replace YYYY-MM with the actual monthly or external batch. The finalizer updates the single batch screening workbook. Read `MONTHLY_WORKFLOW.md` for human edits; use `human_*` columns and `importa_screening(batch)` so original AI proposals remain recorded. Do not generate separate input/included/excluded workbooks.

8. Report the counts of `include`, `exclude`, and `uncertain`, plus the path of
   the included file for the next stage. Do not require human scientific review
   solely because a record is uncertain; the R policy retains uncertainty.

## Scientific decision rules

Retain:

- potentially relevant empirical journal articles;
- reviews;
- meta-analyses;
- editorials published in journals;
- records with insufficient or ambiguous evidence.

Exclude only when clear:

- `BOOK`: book or book chapter;
- `OUT_OF_SCOPE`: clearly unrelated to the Living SEB topic;
- `NON_SEB_FRAMEWORK`: explicitly centered on a clearly different conceptual
  framework despite superficial topical similarity;
- `NOT_JOURNAL_PEER_REVIEWED`: clearly a thesis, dissertation, conference
  proceeding/abstract, report, working paper, or unpublished preprint rather
  than a peer-reviewed journal publication.

Never infer `NON_SEB_FRAMEWORK` merely because the record omits the term `SEB`
or the name of a specific SEB framework. Relevant constructs may use different
terminology.

## Decision behavior

- Use `include` when relevance is plausible or an included publication type is
  explicit.
- Use `exclude` only when one allowed criterion is explicit and confidence is
  supportably high.
- Use `uncertain` when evidence is incomplete or conflicting. Do not force a
  binary decision.
- For `include` and `uncertain`, leave `ai_exclusion_reason` blank.
- For `exclude`, provide one allowed code and concise decisive evidence.
- Do not copy long abstract passages. Paraphrase or quote only a short decisive
  phrase.
- Do not calibrate decisions to an expected proportion of included records.
- Do not omit, duplicate, merge, or add records.

## Output quality

Before finishing, verify:

- exact one-to-one ID coverage;
- no duplicate IDs;
- allowed decision, confidence, and reason values only;
- policy and prompt versions are `1.0.0`;
- model and UTC timestamp fields are populated;
- exclusions have explicit evidence;
- reviews, meta-analyses, and journal editorials have not been excluded because
  of publication type.
