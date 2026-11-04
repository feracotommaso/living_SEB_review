# Living SEB title/abstract screening prompt

Policy version: **1.0.0**  
Prompt version: **1.0.0**

## Goal

Perform a high-sensitivity title/abstract screening of bibliographic records for
Living SEB. This is not final eligibility assessment. Retain any record that
could plausibly be relevant.

## Governing rule

Exclude only when the title, abstract, document type, journal metadata, or other
provided bibliographic information makes an exclusion criterion clear. Missing
or ambiguous information is not evidence for exclusion.

## Records that must be retained

Retain:

- potentially relevant empirical articles;
- reviews;
- meta-analyses;
- editorials published in journals;
- records for which relevance or publication type remains uncertain.

## Allowed exclusion reasons

Use exactly one of these codes only when clearly supported:

- `BOOK`: clearly a book or book chapter;
- `OUT_OF_SCOPE`: clearly unrelated to the Living SEB topic;
- `NON_SEB_FRAMEWORK`: superficially related, but explicitly and clearly based
  on a non-SEB conceptual framework;
- `NOT_JOURNAL_PEER_REVIEWED`: clearly not a peer-reviewed journal publication,
  such as a thesis, dissertation, conference proceeding, report, working paper,
  or unpublished preprint.

Do not exclude a record simply because it does not use the term `SEB`, the full
phrase `social, emotional, and behavioral skills`, or the name of a particular
SEB framework. Constructs may be relevant under different terminology.

## Decision values

For every input record, return exactly one:

- `include`: plausibly relevant or explicitly eligible for retention;
- `exclude`: one allowed exclusion criterion is clearly met;
- `uncertain`: the evidence does not support confident inclusion or exclusion.

The downstream deterministic policy retains `uncertain` records automatically.
Do not force an exclusion merely to avoid uncertainty.

## Confidence

Use one of:

- `high`: the decision is directly supported by explicit metadata or clear
  title/abstract content;
- `medium`: the decision requires a material inference;
- `low`: information is sparse or conflicting.

Only a high-confidence exclusion with a valid reason and explicit evidence is
automatically excluded. Other exclusions are retained as uncertain by the R
workflow.

## Required output

Write one CSV row per input record. Preserve `screening_record_id` exactly.
Use these columns in this exact order:

```text
screening_record_id
ai_decision
ai_exclusion_reason
ai_confidence
ai_rationale
ai_evidence
policy_version
prompt_version
model
screened_at_utc
```

Rules:

- `ai_exclusion_reason` must be blank for `include` and `uncertain`;
- `ai_exclusion_reason` must be one allowed code for `exclude`;
- `ai_rationale` must be concise and record-specific;
- `ai_evidence` must identify the decisive title/abstract/metadata evidence;
- do not copy long abstract passages;
- use `1.0.0` for both policy and prompt versions;
- use an ISO 8601 UTC timestamp for `screened_at_utc`;
- produce no additional records and omit none.
