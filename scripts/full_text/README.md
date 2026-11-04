# Full-text eligibility workflow

This directory contains the operational full-text routing workflow for Living SEB policy `2.0.0` and workflow implementation `2.1.0`.

## Canonical layout

```text
data/2.full_abstracting/
├── pdfs/
│   ├── 0001.pdf
│   └── ...
├── paper_id_registry.csv
├── paper_id_aliases.csv                 # only when legacy aliases exist
├── <batch_id>_full_text_manifest.xlsx
├── ai_decisions/
│   └── <batch_id>/
│       └── <paper_id>.json
├── <batch_id>_full_text_decisions.xlsx
├── *_full_decisions.xlsx                # legacy historical files, unchanged
└── *_decision_progress.xlsx             # legacy historical files, unchanged
```

The monthly manifest is an Excel workbook with clickable DOI links. The registry remains CSV because it is a technical, diff-friendly identity file.

## Step 1 - assign paper IDs

Run from the repository root:

```bash
Rscript scripts/full_text/01_assign_paper_ids.R
```

The script locates the current title/abstract-included file, reconciles the historical paper-ID registry, reuses existing IDs, assigns new IDs only to new publications, generates `<batch_id>_full_text_manifest.xlsx`, and reports missing PDFs.

`paper_id` is persistent and is never recycled, including historical alias IDs.

## Step 2 - add PDFs

Place main full texts at:

```text
data/2.full_abstracting/pdfs/<paper_id>.pdf
```

Re-run Step 1 to refresh PDF availability. Existing IDs must be reused unchanged.

## Step 3 - full-text eligibility

For every available PDF:

1. assess `review_eligible`;
2. record `framework_relation_tags` when review-eligible;
3. assess original quantitative data;
4. assess BESSI/SEB source measurement;
5. separately assess `bessi_target_construct`;
6. identify eligible internal study/sample components using their publication labels;
7. assess data availability/retrievability, overlap, clinical scope, and language reliability;
8. assign `meta_status`;
9. validate output against `schemas/full_text_eligibility_decision.schema.json`;
10. derive `final_route` only for resolved records.

### Important: target construct vs item provenance

BESSI-derived items are not sufficient for meta eligibility. The quantitative score must measure SEB skill level/capacity. BESSI items repurposed to measure change goals, feasibility/changeability beliefs, preferences, or similar beliefs about skills are quantitatively ineligible with `NO_ELIGIBLE_SEB_SKILL_CONSTRUCT`.

### Important: multi-study publications

A publication is meta-eligible when at least one internal component is eligible. Record eligible labels in `eligible_quantitative_components` (e.g. `Study 2`). Only those components proceed to detailed sample/matrix coding. Do not create placeholder sample IDs for ineligible components.

### Important: framework extension/modification

For review-eligible conceptual papers, preserve whether the paper extends/modifies SEB or compares it with other frameworks using `framework_relation_tags`. These flags are intentionally carried forward until the richer review topic/keyword taxonomy is implemented.

## Human review

Human review is required only when unresolved substantive judgment can change routing. A non-standard BESSI form does not automatically require human review if another criterion already determines ineligibility.

## Step 4 - build the canonical decision workbook

After the AI/skill has produced one validated JSON decision for **every** paper in the manifest, run:

```bash
Rscript scripts/full_text/02_build_full_text_decisions.R
```

AI decision files live at:

```text
data/2.full_abstracting/ai_decisions/<batch_id>/<paper_id>.json
```

The script refuses to create a partially screened workbook: every manifest record must have one AI decision. It writes:

```text
data/2.full_abstracting/<batch_id>_full_text_decisions.xlsx
```

with four sheets: `summary`, `decisions`, `human_review`, and `codebook`.

`pending` cases remain in `decisions` as operational retrieval tasks. `uncertain` cases are copied into `human_review`.

## Step 5 - consolidated human adjudication

Do not adjudicate open cases one-by-one by default. The AI/skill should present all cases in `human_review` together, each with:

- the precise methodological issue;
- page-level evidence;
- an AI recommendation;
- a small set of defensible options.

The user may resolve multiple cases in one response without reading the complete PDFs when the evidence packet is sufficient. The AI/skill can then write the adjudicated fields into the `human_review` sheet.

Run:

```bash
Rscript scripts/full_text/03_finalize_full_text_decisions.R
```

Resolved rows remain in `human_review` for auditability, while the `decisions` sheet is updated to the adjudicated values. If an adjudication establishes a reusable methodological rule, update the policy/guidance so future analogous cases can be resolved automatically.

## Missing correlations

Missing correlations are not an immediate exclusion. Use `pending` while reasonable retrieval remains possible.

## Legacy compatibility

Resolved v2 decisions map to legacy `final_decision` as follows:

```text
review=true  + meta=eligible   -> both
review=false + meta=eligible   -> meta
review=true  + meta=ineligible -> review
review=false + meta=ineligible -> exclude
```

`pending` and `uncertain` are never exported as final legacy decisions.

The downstream `metadata.xlsx`, `review_data.xlsx`, paper, tutorials, and Shiny app remain unchanged until the compatibility/export layer is implemented.

## Operational data vs regression tests

The canonical monthly result is `data/2.full_abstracting/<batch_id>_full_text_decisions.xlsx`. Files under `tests/full_text/` are regression fixtures only and are not the operational database.

## Pilot regression fixture

The 2026-10 pilot is complete and adjudicated. See:

- `tests/full_text/2026-10_expected_records.csv`
- `tests/full_text/2026-10_adjudicated_decisions.csv`
- `tests/full_text/2026-10_expected_full_text_decisions.csv`
- `tests/full_text/PILOT_2026-10.md`
