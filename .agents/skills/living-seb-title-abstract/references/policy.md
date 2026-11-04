# Living SEB title/abstract policy 1.0.0

## Aim

Maximize sensitivity. Retain an extra record rather than risk excluding a
potentially eligible publication.

## Retain

- empirical articles that could be relevant;
- reviews;
- meta-analyses;
- journal editorials;
- uncertain records.

## Exclude only when clear

| Code | Meaning |
|---|---|
| `BOOK` | Clearly a book or book chapter |
| `OUT_OF_SCOPE` | Clearly unrelated to social, emotional, or behavioral skills and the Living SEB topic |
| `NON_SEB_FRAMEWORK` | Clearly and explicitly based on a different, non-SEB conceptual framework |
| `NOT_JOURNAL_PEER_REVIEWED` | Clearly not a peer-reviewed journal publication |

Absence of explicit SEB terminology is not evidence for exclusion.

## Operational mapping

| AI decision | Confidence/evidence | R action |
|---|---|---|
| `include` | any valid | `KEEP` |
| `uncertain` | any valid | `KEEP_UNCERTAIN` |
| `exclude` | high + valid reason + evidence | `EXCLUDE` |
| `exclude` | medium/low or incomplete | `KEEP_UNCERTAIN` |

Scientific uncertainty does not trigger mandatory human screening at this stage.
