# Required decisions CSV

Write UTF-8 CSV with one row per input record and exactly these columns in order:

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

Allowed values:

- `ai_decision`: `include`, `exclude`, `uncertain`
- `ai_confidence`: `high`, `medium`, `low`
- `ai_exclusion_reason`: blank unless excluded; otherwise one of
  `BOOK`, `OUT_OF_SCOPE`, `NON_SEB_FRAMEWORK`,
  `NOT_JOURNAL_PEER_REVIEWED`
- `policy_version`: `1.0.0`
- `prompt_version`: `1.0.0`
- `screened_at_utc`: ISO 8601 UTC, for example `2026-11-03T10:30:00Z`

Every exclusion needs a concise evidence statement. Preserve IDs exactly.
