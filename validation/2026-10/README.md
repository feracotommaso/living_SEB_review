# Pilot validation: 2026-10

## Deduplication

The R workflow and the prior Rayyan workflow produced the same 14 records:

```text
R records: 14
Rayyan records: 14
Exact matches: 14
Only in R: 0
Only in Rayyan: 0
Fuzzy candidates: 0
```

## Title/abstract screening

The 14 AI-assisted decisions were reviewed by the researcher:

```text
KEEP: 6
EXCLUDE: 8
UNCERTAIN/REVIEW: 0
```

All decisions were accepted. The minimal expected-decision fixture is stored in
`tests/fixtures/title_abstract/2026-10_expected_decisions.csv`.
