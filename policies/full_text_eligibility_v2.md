# Living SEB Full-text Eligibility Policy v2

**Policy version:** `2.0.0`  
**Status:** stable after the 2026-10 pilot  
**Scope:** full-text eligibility and routing only  
**Project:** Living SEB Skills Project

## 1. Purpose

Full-text eligibility screening determines whether a publication retained after title/abstract screening should be excluded from the Living SEB Project, retained for the living review, retained for the quantitative/meta-analytic database, or retained for both components.

Eligibility screening precedes detailed extraction of paper-, sample-, measure-, and matrix-level information. The primary unit at this stage is the **publication**, but quantitative eligibility may apply to only a subset of the studies or samples reported within that publication.

## 2. General principles

1. **Sensitivity is prioritized over specificity.** Do not exclude when a material eligibility criterion remains genuinely uncertain.
2. **Review eligibility and quantitative eligibility are independent dimensions.** A publication may be review-only, meta-only, both, or excluded.
3. **The final route is derived, not manually coded.**
4. **Scientific eligibility is distinct from operational data availability.** Missing correlations or data that may be retrievable produce a `pending` status rather than immediate exclusion.
5. **The origin of measurement items and the construct actually measured are separate criteria.** BESSI-derived items do not make a quantitative contribution eligible if they are used to measure beliefs, goals, preferences, perceived changeability, or another construct rather than SEB skill level/capacity.
6. **Publication-level meta eligibility may be component-specific.** A publication is meta-eligible when at least one internal study/sample component is eligible. Only eligible components proceed to sample/matrix coding.
7. **Human review is reserved for substantive judgment that can change routing.** It is not required when another independent criterion already determines the decision.
8. **Every automated decision must be auditable.** Rationale and evidence location must be recorded.

## 2.1 Record identity, paper IDs, and full-text files

Each publication entering full-text screening receives a persistent `paper_id` **before** the eligibility decision. `paper_id` is assigned automatically and must never be manually recycled or reassigned, including when a publication is later excluded.

The canonical registry is:

`data/2.full_abstracting/paper_id_registry.csv`

The canonical full-text PDF directory remains:

`data/2.full_abstracting/pdfs/`

The main article PDF is named `<paper_id>.pdf`. Optional supplements retain the same `paper_id` prefix.

The monthly operational manifest is generated as:

`data/2.full_abstracting/<batch_id>_full_text_manifest.xlsx`

The manifest contains clickable DOI URLs in the form `https://doi.org/...`.

`sample_id` and `matrix_id` are **not** assigned at this stage. They are created only for quantitative components that proceed to detailed coding.

## 3. Review eligibility

Set `review_eligible = true` when the SEB framework, SEB skills, or their measurement represent a substantive focus of the publication.

Original quantitative data are not required. Eligible publication types can therefore include empirical studies, validation or adaptation studies, longitudinal and experimental studies, theoretical articles, narrative or systematic reviews, meta-analyses, journal editorials, commentaries, and perspectives.

A publication is review-eligible when at least one of the following applies:

- **SEB framework focus:** it studies, discusses, develops, critiques, extends, modifies, compares, or applies the Social, Emotional, and Behavioral skills framework.
- **BESSI/SEB measurement focus:** it concerns development, validation, translation, adaptation, psychometric properties, or interpretation of a BESSI/SEB measure.
- **SEB skills as substantive variables:** SEB skills are among the main constructs investigated.
- **Evidence synthesis:** a review or meta-analysis substantively synthesizes SEB skills, the SEB framework, or BESSI-based measures.
- **Conceptual contribution:** a theoretical article, editorial, commentary, or perspective provides a substantive contribution to SEB conceptualization, measurement, application, critique, extension, or modification.

Set `review_eligible = false` when the publication only cites foundational SEB papers, mentions SEB skills incidentally, focuses on another social-emotional framework without substantively adopting the SEB framework, or uses BESSI only as an ancillary variable while its substantive focus lies elsewhere.

The last case may still be eligible for the quantitative database and therefore routed as `meta`.

### 3.1 Framework-relation tags

Full-text screening records a small set of **framework-relation tags** for every review-eligible publication. These are not a replacement for the future review-topic/keyword taxonomy. They preserve how the publication relates to the SEB framework so this information is not lost before topic coding.

Controlled tags are:

- `APPLIES_SEB_FRAMEWORK`
- `TESTS_SEB_FRAMEWORK`
- `MEASURES_SEB_SKILLS_SUBSTANTIVELY`
- `VALIDATES_OR_ADAPTS_BESSI`
- `EXTENDS_OR_MODIFIES_SEB_FRAMEWORK`
- `COMPARES_SEB_WITH_OTHER_FRAMEWORKS`
- `CRITIQUES_SEB_FRAMEWORK`
- `SYNTHESIZES_SEB_EVIDENCE`
- `OTHER_SUBSTANTIVE_SEB_RELATION`

A publication that proposes a new framework by integrating, extending, or modifying SEB/BESSI should therefore remain review-eligible when substantively relevant and should be tagged accordingly. The later review-topic coding should explicitly preserve this distinction.

### Controlled review exclusion reasons

- `NOT_SEB_FOCUS`
- `SEB_MENTION_ONLY`
- `NON_SEB_FRAMEWORK`
- `INELIGIBLE_PUBLICATION_TYPE`
- `OTHER_REVIEW_EXCLUSION`

A review exclusion reason is recorded only when `review_eligible = false`.

## 4. Quantitative/meta-analytic eligibility

Quantitative eligibility is evaluated independently from review eligibility. A publication can contribute to the quantitative database even when it is not eligible for the living review.

A publication is `meta_status = eligible` when **at least one quantitative component** (e.g., Study 2 of a multi-study publication) satisfies all applicable criteria below.

### 4.1 Original quantitative data

The component must provide original quantitative data. Reviews, meta-analyses, editorials, commentaries, and exclusively theoretical articles are therefore not quantitatively eligible.

### 4.2 Eligible BESSI/SEB measurement source

The component must include measurement sufficiently grounded in the BESSI/SEB measurement framework. Source-measure eligibility is classified using the hierarchy in `config/screening/bessi_measure_eligibility.yml`.

#### Clearly eligible

Examples include a recognized validated BESSI form used as intended or a validated translation/adaptation used according to its validated scoring structure.

#### Clearly ineligible

Examples include a measure explicitly belonging to another framework with no BESSI-derived measurement, a generic social-emotional measure merely described as similar to SEB skills, or personality-trait measurement without a BESSI/SEB skill measure.

#### Human judgment required

Selected BESSI facets/subscales, selected items, modified forms, ad hoc short forms, altered scoring, or validation-unclear adaptations may require human judgment **when source-measure validity is material to the decision**.

However, source-measure ambiguity does not force human review when another independent criterion already determines ineligibility. For example, if modified BESSI items clearly measure change goals or feasibility beliefs rather than SEB skill level/capacity, the contribution is quantitatively ineligible regardless of whether the modified item set might otherwise be considered BESSI-derived.

### 4.3 Eligible target construct: actual SEB skill level/capacity

For the quantitative database, BESSI/SEB measurement must represent the participant's **SEB skill level or capacity** (or a direct scoring representation of that level/capacity).

A BESSI-derived item set is **not** quantitatively eligible when it has been repurposed to measure a different target construct, including:

- change goals;
- feasibility or changeability beliefs;
- preferences or desirability judgments;
- perceived importance;
- intentions to change;
- attitudes toward SEB skills;
- intervention preferences;
- other beliefs or judgments *about* SEB skills rather than skill level/capacity itself.

When this distinction is clear, use:

`meta_status = ineligible`  
`meta_exclusion_reason = NO_ELIGIBLE_SEB_SKILL_CONSTRUCT`

When the target construct is genuinely mixed or unclear and this affects eligibility, use `meta_status = uncertain` and human review.

### 4.4 Quantitative information usable by the Living SEB database

Eligible quantitative information may come from correlations reported in the article or supplements, openly available raw data, raw data or correlations provided by authors, or other statistics from which the required correlations can be derived validly.

For longitudinal and experimental studies, the current database primarily uses baseline or the first eligible measurement occasion unless a future protocol explicitly extends the effect-size structure.

### 4.5 Non-duplicated contribution

The same effect from the same sample, measurement occasion, and variables must not be entered more than once. A publication is not automatically excluded because its sample overlaps with an existing study. If overlap is suspected and cannot be resolved immediately, use `meta_status = pending` with `SAMPLE_OVERLAP_TO_RESOLVE`.

A publication remains quantitatively eligible when at least one valid non-duplicated contribution can enter the database.

## 5. Component-specific quantitative scope

A multi-study publication may contain both eligible and ineligible components.

At full-text eligibility, identify eligible internal components using the labels used in the publication (for example `Study 2`). Do **not** assign `sample_id` or `matrix_id` yet.

If at least one component is eligible:

- set publication-level `meta_status = eligible`;
- list the eligible component label(s) in `eligible_quantitative_components`;
- only those components proceed to detailed sample/matrix coding;
- ineligible components do not receive `sample_id` or `matrix_id` merely to preserve the publication's internal numbering.

Therefore, if only `Study 2` of a two-study publication is eligible, the first coded quantitative sample may still become sample `a` and matrix `<paper_id>a`; there is no requirement to create an unused sample for Study 1.

## 6. Quantitative status

### `eligible`

At least one quantitative component clearly satisfies the applicable eligibility criteria.

### `ineligible`

There is sufficient evidence that no quantitative component in the publication can satisfy the applicable criteria.

### `pending`

The substantive criteria appear potentially satisfied, but operational retrieval or verification is still required. `pending` is temporary and is not a scientific exclusion.

### `uncertain`

A material eligibility criterion requires substantive human judgment. `uncertain` is also temporary and must not be silently converted to exclusion.

## 7. Missing correlations and data retrieval

Absence of immediately available correlations is not itself a scientific exclusion criterion.

If the publication is otherwise potentially eligible and the required quantitative information may be obtainable, set `meta_status = pending` and record an appropriate pending reason. Examples include author contact, supplement retrieval, open-data retrieval, or verification of raw-data access.

Only after reasonable retrieval attempts have failed should the study be changed to `meta_status = ineligible` with `DATA_UNAVAILABLE_AFTER_RETRIEVAL_ATTEMPT` or, when appropriate, `DATA_NOT_REDUCIBLE_TO_CORRELATIONS`.

### Controlled pending reasons

- `AUTHOR_CONTACT_NEEDED`
- `SUPPLEMENT_NEEDED`
- `OPEN_DATA_TO_RETRIEVE`
- `RAW_DATA_ACCESS_TO_VERIFY`
- `SAMPLE_OVERLAP_TO_RESOLVE`
- `OTHER_PENDING`

## 8. Language

Language is not a substantive exclusion criterion when the AI can reliably evaluate the relevant full text without human interpretation.

Articles in languages other than those spoken by the review team may be screened and included when the complete relevant text is available to the AI, interpretation is sufficiently reliable, and no material language ambiguity remains.

If language interpretation is material and uncertain, do not exclude. Use `meta_status = uncertain`, `requires_human_review = true`, and `LANGUAGE_INTERPRETATION_REQUIRED` when quantitative routing depends on that uncertainty.

## 9. Clinical populations

Clinical status is not itself an exclusion criterion. Specific learning disorders/learning disabilities are explicitly not excluded merely because of diagnosis.

Quantitative data may be excluded when they derive exclusively from a clearly severe clinical population for which SEB-skill interpretation is outside the target scope of the project. Do not infer severity from a diagnostic label alone.

If severity or comparability requires substantive judgment, use `meta_status = uncertain` and `CLINICAL_SEVERITY_UNCLEAR`.

## 10. Publication type

Publication type is usually screened earlier but should be verified at full text when needed. Books, book chapters, theses/dissertations, conference abstracts or proceedings that are not peer-reviewed journal publications, and other clearly non-journal non-peer-reviewed outputs remain excluded according to project policy.

Journal editorials are retained for the living review when substantively relevant. Systematic reviews and meta-analyses are likewise retained for the living review when substantively relevant.

## 11. Controlled quantitative exclusion reasons

- `NO_ORIGINAL_QUANTITATIVE_DATA`
- `NO_ELIGIBLE_BESSI_MEASURE`
- `NO_ELIGIBLE_SEB_SKILL_CONSTRUCT`
- `FULLY_DUPLICATED_DATA`
- `NO_ELIGIBLE_BASELINE_DATA`
- `DATA_NOT_REDUCIBLE_TO_CORRELATIONS`
- `DATA_UNAVAILABLE_AFTER_RETRIEVAL_ATTEMPT`
- `SEVERE_CLINICAL_POPULATION`
- `OTHER_META_EXCLUSION`

`NO_ELIGIBLE_BESSI_MEASURE` is used when the measurement source/framework is ineligible. `NO_ELIGIBLE_SEB_SKILL_CONSTRUCT` is used when BESSI/SEB-derived content is present but the actual measured target is not SEB skill level/capacity.

## 12. Human-review triggers

Human review is required only when substantive judgment can change routing. Controlled reasons are:

- `AMBIGUOUS_BESSI_MEASURE`
- `AMBIGUOUS_SEB_TARGET_CONSTRUCT`
- `CLINICAL_SEVERITY_UNCLEAR`
- `LANGUAGE_INTERPRETATION_REQUIRED`
- `FRAMEWORK_ADOPTION_UNCLEAR`
- `SAMPLE_OVERLAP_COMPLEX`
- `CONFLICTING_FULLTEXT_INFORMATION`
- `OTHER_SUBSTANTIVE_UNCERTAINTY`

Human review is not required merely because the article is non-English, because an author must be contacted, because a supplement must be retrieved, or because the BESSI form is non-standard when another decisive criterion already resolves eligibility.

## 13. Deterministic routing

The final scientific route is derived as follows:

| review_eligible | meta_status | final_route |
|---|---|---|
| true | eligible | `both` |
| false | eligible | `meta` |
| true | ineligible | `review` |
| false | ineligible | `exclude` |

When `meta_status` is `pending` or `uncertain`, `final_route` must remain null until the issue is resolved.

## 14. Audit trail

Each assessment must retain at least:

- `screening_record_id` and persistent `paper_id`;
- review decision, rationale, exclusion reason when applicable, and evidence location;
- framework-relation tag(s) for review-eligible publications;
- quantitative status, rationale, exclusion or pending reason when applicable, and evidence location;
- BESSI source-measure classification;
- BESSI target-construct classification;
- eligible quantitative component label(s), when any;
- language and whether AI-only language assessment was sufficient;
- clinical-population assessment when relevant;
- human-review flag and reason(s);
- decision confidence;
- policy version;
- decision source;
- screening date.

Evidence should be concise and attributable to a specific full-text location. Long quotations are not required.

## 15. Separation from detailed data extraction

Full-text eligibility is not the detailed coding stage. Country, age, gender composition, detailed sample sizes, exact measure implementation, trait and outcome measures, review-topic taxonomy, and correlation matrices should normally be coded after eligibility has been established.

Two limited classifications are deliberately retained at full text because they affect routing or future interpretation:

1. `eligible_quantitative_components`, which determines what proceeds to sample/matrix coding;
2. `framework_relation_tags`, which records how a review-eligible publication relates to SEB and must later be preserved in the richer topic/keyword taxonomy.

## 16. Legacy compatibility

The current legacy `final_decision` can be derived from the resolved v2 fields:

- `review_eligible = true` + `meta_status = eligible` -> `both`
- `review_eligible = false` + `meta_status = eligible` -> `meta`
- `review_eligible = true` + `meta_status = ineligible` -> `review`
- `review_eligible = false` + `meta_status = ineligible` -> `exclude`

`pending` and `uncertain` must never be exported as final legacy decisions.

The new eligibility fields can be introduced upstream while continuing to generate the current `metadata.xlsx` and `review_data.xlsx` outputs for the paper, tutorials, and Shiny application.

## 17. Pilot validation and policy promotion

Policy `2.0.0` incorporates the completed 2026-10 pilot on paper IDs `0144`-`0149`.

The pilot established three policy refinements:

1. **Target construct matters independently of BESSI item provenance.** Paper `0144` was review-eligible but quantitatively ineligible because modified BESSI-derived items measured change goals and feasibility beliefs rather than SEB skill levels/capacities.
2. **Quantitative eligibility may be component-specific.** Paper `0145` was quantitatively eligible only for Study 2; Study 1 does not proceed to sample/matrix coding.
3. **Framework extension/modification must be preserved for later review coding.** Paper `0148` was review-eligible and should be explicitly identifiable as extending/modifying and comparing the SEB/BESSI framework when the richer review topic taxonomy is developed.

The frozen pilot decisions are stored under `tests/full_text/` and should be used as regression expectations for future policy or screening changes.


## 18. Operational decision workbook and adjudication

The canonical monthly full-text eligibility result is not stored under `tests/`. Regression fixtures under `tests/full_text/` are software/methodological checks only.

For each monthly batch, the operational result is:

`data/2.full_abstracting/<batch_id>_full_text_decisions.xlsx`

The workbook contains four sheets:

1. `summary`: batch-level counts and workflow state;
2. `decisions`: one row per publication, including all resolved and unresolved eligibility fields;
3. `human_review`: only cases that require substantive adjudication, plus the AI evidence packet and human decision fields;
4. `codebook`: definitions of the operational fields.

The decision workbook must not be created from incomplete AI screening. Every record in the monthly full-text manifest must have an AI decision object first. `pending` is allowed because it represents a resolved scientific assessment with an operational data-retrieval task; `uncertain` enters the human-review queue.

Human adjudication should be **batched by default**. The AI/skill should screen all full texts first, then present all unresolved cases together with the methodological issue, recommendation, decision options, and page-level evidence. The reviewer should not need to read the entire PDF when this evidence packet is sufficient.

Resolved adjudications remain in the `human_review` sheet as an audit trail. A user decision that establishes a generalizable methodological rule should be incorporated into the policy or operational guidance so that equivalent future cases do not repeatedly require human review.

The workbook is the canonical operational screening record. The frozen adjudicated cases in `tests/full_text/` are regression expectations and must not be treated as the monthly database.
