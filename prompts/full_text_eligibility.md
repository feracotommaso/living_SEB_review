# Living SEB full-text eligibility screening instructions

Use this prompt together with:

- `policies/full_text_eligibility_v2.md`
- `config/screening/full_text_eligibility.yml`
- `config/screening/bessi_measure_eligibility.yml`
- `schemas/full_text_eligibility_decision.schema.json`

## Task

Read the complete available full text and supplied supplementary material. Assess **full-text eligibility and routing only**. Do not perform detailed sample, measure, or correlation extraction unless a detail is necessary to resolve eligibility.

Return one JSON object per input record and conform exactly to the JSON schema. Save each record as `data/2.full_abstracting/ai_decisions/<batch_id>/<paper_id>.json` when operating in the repository.

Use eligibility policy `2.0.0` and workflow `2.2.0`. The R builder validates the
typed object against the local schema before flattening it. Do not omit required
nulls or arrays. Record actual PDF evidence pages; never invent page numbers.

## Record identity and PDF convention

`paper_id` is assigned automatically before screening and must be preserved exactly. Do not create, increment, recycle, or modify paper IDs during eligibility assessment.

The canonical main PDF path is `data/2.full_abstracting/pdfs/<paper_id>.pdf`. `sample_id` and `matrix_id` are not assigned during this step.

## Decision order

1. Verify `screening_record_id`, `paper_id`, and bibliographic identity.
2. Assess **review eligibility** independently from quantitative eligibility.
3. For review-eligible papers, assign one or more `framework_relation_tags` describing how the paper relates to SEB/BESSI.
4. Assess original quantitative data.
5. Assess BESSI/SEB **source measurement** using the source-measure hierarchy.
6. Separately assess the **target construct actually measured**. Determine whether the measure represents SEB skill level/capacity or instead beliefs/goals/judgments about SEB skills.
7. If the paper contains multiple studies/components, determine which components, if any, satisfy quantitative eligibility. Record their paper labels in `eligible_quantitative_components`; do not create sample IDs.
8. Assess whether quantitative information is usable now, retrievable, or definitively unavailable/non-reducible.
9. Check duplicate/overlap issues only to the extent needed for eligibility.
10. Consider clinical-population eligibility only if relevant.
11. Assess whether language interpretation is sufficiently reliable for an AI-only decision.
12. Determine `meta_status` and derive routing. Never assign a resolved `final_route` to `pending` or `uncertain` records.

## Review eligibility

Set `review_eligible = true` when SEB/BESSI is a substantive focus. A citation or incidental mention is insufficient. Reviews, theoretical papers, and journal editorials may be review-eligible when substantively relevant.

For review-eligible papers, use controlled `framework_relation_tags`. In particular, preserve whether the paper extends/modifies SEB or integrates it into an alternative framework; this will later feed the richer review topic/keyword taxonomy.

## BESSI source measurement

Use `clearly_eligible` only for unambiguous recognized BESSI/SEB measurement. Use `clearly_ineligible` only when the study clearly lacks eligible BESSI/SEB measurement. Use `human_judgment` for partial, modified, selected-item, altered-scoring, or validation-unclear BESSI use **when that ambiguity is material to routing**.

Do not require human review for source-measure ambiguity if another independent criterion already determines ineligibility.

## Target construct actually measured

This is separate from item provenance.

Use `seb_skill_level_or_capacity` when the quantitative score represents participants' SEB skill level/capacity.

Use `belief_goal_or_judgment_about_seb_skills` when BESSI/SEB-derived content has been repurposed to assess change goals, feasibility/changeability beliefs, desirability, preferences, importance, intentions, or other beliefs about SEB skills. These contributions are quantitatively ineligible with `NO_ELIGIBLE_SEB_SKILL_CONSTRUCT`.

If the target construct is genuinely mixed or ambiguous and the distinction affects eligibility, use `mixed_or_ambiguous`, `meta_status = uncertain`, and `AMBIGUOUS_SEB_TARGET_CONSTRUCT`.

## Multi-study publications

Publication-level `meta_status = eligible` when at least one internal study/sample component is eligible. List only those component labels in `eligible_quantitative_components` (for example, `Study 2`). Ineligible components do not later receive placeholder sample IDs or matrix IDs.

## Missing correlations or data

Do not exclude because correlations are absent from the main article. If otherwise potentially eligible information may be obtained from supplements, open data, raw data, or authors, use `pending` and the appropriate pending reason.

## Language

Language is not an exclusion criterion. If you can reliably interpret the relevant full text, proceed without human review. If language interpretation is material and uncertain, do not exclude; require human review.

## Clinical populations

Do not exclude merely because a sample is clinical. Specific learning disorders/learning disabilities are not excluded solely because of diagnosis. Severe out-of-scope populations may be excluded only when severity/comparability is clear; otherwise use human review.

## Evidence

For each review and meta decision, provide concise paraphrased evidence plus a locator (PDF page and section/table/supplement when available). Evidence must establish the criterion, not merely repeat the label.

## Human review

Human review is for substantive uncertainty that can change routing. Typical triggers are ambiguous BESSI source measurement, ambiguous target construct, unclear clinical severity/comparability, material language ambiguity, unclear framework adoption, complex sample overlap, or conflicting full-text information.

If review eligibility is materially uncertain, record the provisional review
assessment and keep the independent quantitative status. Set
`requires_human_review = true`, `final_route = null`, and
`current_route = human_review_required` (unless meta is also uncertain, in which
case use its uncertainty route). Explain the review issue in the evidence packet.

When `requires_human_review = true`, the JSON must also contain a concise **human-review packet**:

- `human_review_summary`: the exact methodological issue that needs a decision;
- `ai_recommendation`: the recommended resolution and why;
- `decision_options`: the small set of defensible choices;
- evidence and page/locator sufficient for the user to adjudicate without reading the entire PDF whenever possible.

Do not ask the user to adjudicate papers one at a time by default. After all PDFs in the batch have been screened, present all open human-review cases together. The user may decide several cases in one response. If an adjudication establishes a general rule, preserve that rule in policy/guidance so analogous future cases can be resolved automatically.

## Prohibited shortcuts

- Do not infer review eligibility from keywords alone.
- Do not infer BESSI eligibility merely from the phrase `social-emotional skills`.
- Do not infer quantitative eligibility from BESSI item provenance alone.
- Do not treat goals, beliefs, preferences, or perceived changeability about SEB skills as measurements of SEB skill level/capacity.
- Do not automatically exclude partial BESSI measurement when source validity is the only uncertainty.
- Do not exclude because correlations are absent from the main article.
- Do not exclude because an article is non-English.
- Do not infer severe clinical status from a diagnostic label alone.
- Do not create sample IDs for quantitatively ineligible components of a multi-study paper.
- Do not manually choose a resolved route when `meta_status` is pending or uncertain.
