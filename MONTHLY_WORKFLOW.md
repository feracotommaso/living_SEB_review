# Monthly Living SEB workflow

Run from the repository root on the current feature branch. Update
`config/dedup_config.R` for the intended publication month.

1. Preserve raw Scopus/WoS exports under `data/0.bib_download/`.
2. Follow [deduplication](scripts/dedup/README.md), resolving duplicate pairs before
   guarded registry promotion. Existing Rayyan discrepancies block promotion.
3. Follow [title/abstract screening](scripts/screening/README.md): prepare input,
   invoke `$living-seb-title-abstract`, validate and finalize. Uncertain records are
   retained. The included CSV is the canonical next-stage input.
4. Invoke `$living-seb-full-text` and follow [full-text eligibility](scripts/full_text/README.md): assign persistent
   paper IDs; retrieve `data/2.full_abstracting/pdfs/<paper_id>.pdf`; screen all
   articles into typed JSON; build monthly CSV/XLSX decisions and the review queue.
5. Discuss substantive ambiguities as a batch, record adjudications in
   `human_review`, then finalize. Keep resolved adjudications. Data retrieval
   remains pending rather than becoming a scientific exclusion.
6. Export resolved legacy routing. Extract eligible quantitative components,
   assigning sample/matrix IDs at extraction. Preserve framework-relation tags
   for later review coding. Eligibility outputs are not complete `basic_info`.
   The [descriptive coding stage](scripts/coding/README.md) defines the 31-field
   contract, evidence-based inputs and isolated monthly coding outputs.
7. After extraction, use the established `basic_info + matrices` pipeline to
   generate analytical workbooks. Preserve paper/app column contracts.
8. Run checks and inspect Git diffs before committing on the feature branch.
   Merge into main only through the normal explicit review process.

```bash
Rscript scripts/screening/99_run_tests.R
Rscript scripts/screening/03_validate_2026_10_regression.R
Rscript scripts/full_text/99_run_tests.R
```

The October full-text pilot is operational under `data/2.full_abstracting/`;
independent regression expectations remain under `tests/full_text/`.
