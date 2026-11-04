#!/usr/bin/env python3
"""Validate Living SEB title/abstract AI decisions against prepared input."""

from __future__ import annotations

import argparse
import csv
import json
from pathlib import Path
import sys


def read_csv(path: Path) -> list[dict[str, str]]:
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def clean(value: object) -> str:
    if value is None:
        return ""
    return str(value).strip()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", required=True, type=Path)
    parser.add_argument("--decisions", required=True, type=Path)
    parser.add_argument("--policy", required=True, type=Path)
    args = parser.parse_args()

    for path in (args.input, args.decisions, args.policy):
        if not path.exists():
            print(f"ERROR: file not found: {path}", file=sys.stderr)
            return 2

    policy = json.loads(args.policy.read_text(encoding="utf-8"))
    input_rows = read_csv(args.input)
    decision_rows = read_csv(args.decisions)
    errors: list[str] = []

    input_ids = [clean(row.get("screening_record_id")) for row in input_rows]
    decision_ids = [clean(row.get("screening_record_id")) for row in decision_rows]

    required = policy["required_columns"]
    if decision_rows:
        missing_columns = [c for c in required if c not in decision_rows[0]]
    else:
        missing_columns = required
    if missing_columns:
        errors.append("missing columns: " + ", ".join(missing_columns))

    if any(not value for value in input_ids):
        errors.append("input contains blank screening_record_id")
    if len(set(input_ids)) != len(input_ids):
        errors.append("input contains duplicate screening_record_id")
    if any(not value for value in decision_ids):
        errors.append("decisions contain blank screening_record_id")
    if len(set(decision_ids)) != len(decision_ids):
        errors.append("decisions contain duplicate screening_record_id")

    missing_ids = sorted(set(input_ids) - set(decision_ids))
    extra_ids = sorted(set(decision_ids) - set(input_ids))
    if missing_ids:
        errors.append("missing decision IDs: " + ", ".join(missing_ids))
    if extra_ids:
        errors.append("extra decision IDs: " + ", ".join(extra_ids))

    allowed_decisions = set(policy["allowed_decisions"])
    allowed_confidence = set(policy["allowed_confidence"])
    allowed_reasons = set(policy["allowed_exclusion_reasons"])

    for index, row in enumerate(decision_rows, start=2):
        decision = clean(row.get("ai_decision")).lower()
        confidence = clean(row.get("ai_confidence")).lower()
        reason = clean(row.get("ai_exclusion_reason")).upper()
        rationale = clean(row.get("ai_rationale"))
        evidence = clean(row.get("ai_evidence"))
        policy_version = clean(row.get("policy_version"))
        prompt_version = clean(row.get("prompt_version"))
        model = clean(row.get("model"))
        screened_at = clean(row.get("screened_at_utc"))

        if decision not in allowed_decisions:
            errors.append(f"row {index}: invalid ai_decision {decision!r}")
        if confidence not in allowed_confidence:
            errors.append(f"row {index}: invalid ai_confidence {confidence!r}")
        if decision == "exclude":
            if reason not in allowed_reasons:
                errors.append(f"row {index}: invalid/missing exclusion reason")
            if not evidence:
                errors.append(f"row {index}: exclusion evidence is required")
        elif reason:
            errors.append(f"row {index}: exclusion reason must be blank")
        if not rationale:
            errors.append(f"row {index}: rationale is required")
        if policy_version != policy["policy_version"]:
            errors.append(f"row {index}: policy_version mismatch")
        if prompt_version != policy["prompt_version"]:
            errors.append(f"row {index}: prompt_version mismatch")
        if not model:
            errors.append(f"row {index}: model is required")
        if not screened_at:
            errors.append(f"row {index}: screened_at_utc is required")

    if errors:
        print("Living SEB decision validation FAILED", file=sys.stderr)
        for error in errors:
            print(f"- {error}", file=sys.stderr)
        return 1

    counts = {key: 0 for key in sorted(allowed_decisions)}
    for row in decision_rows:
        counts[clean(row.get("ai_decision")).lower()] += 1

    print("Living SEB decision validation passed")
    print(f"input_records={len(input_rows)}")
    print(f"decision_records={len(decision_rows)}")
    for key, value in counts.items():
        print(f"{key}={value}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
