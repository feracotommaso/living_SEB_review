# Monthly Living SEB workflow

Run from the repository root on the current feature branch. Update
`config/dedup_config.R` for the intended publication month.

1. Preserve raw Scopus/WoS exports under `data/0.bib_download/`. Confirm their
   actual export/download date separately from the publication-month batch ID;
   update `search_run_date` in configuration for the new search.
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
   Prepare structured article coding for every retained paper, update the variable
   and topic codebooks where needed, record the confirmed bibliographic export
   date with `scripts/coding/05_set_download_date.R <batch> <date>`, then run
   `scripts/coding/02_build_article_coding.R <batch>`. This assigns stable sample
   IDs before running the article-specific quantitative preparation scripts.
   For quantitative extraction, prioritise downloading original individual data
   even when a paper matrix is available. Keep immutable downloads under
   `data/3.meta_data/open_data/raw/<paper_id>/` and save coded individual data in
   `data/3.meta_data/open_data/individual_data/<matrix_id>.csv`; both stay local
   under the Git-ignored `open_data/` directory. Version reproducible R scoring
   and source/checksum provenance. Published matrices are a comparison reference
   or a documented fallback when individual data are unavailable or unusable.
   Follow the common R template for each new article. Keep sample-specific audits
   separate and rebuild descriptive coding after updating source extraction notes.
7. After coding/extraction, run
   `Rscript scripts/coding/04_integrate_batch.R <batch_id>`. This validates the
   canonical coding and extraction checksums, registers the batch, copies ready
   matrices into the common matrix folder and rebuilds metadata/review CSV/XLSX.
   Review rows can advance while quantitative extraction is pending. Preserve
   historical eligibility and the established paper/app column contracts.
8. Run checks and inspect Git diffs before committing on the feature branch.
   Merge into main only through the normal explicit review process.

```bash
Rscript scripts/screening/99_run_tests.R
Rscript scripts/screening/03_validate_2026_10_regression.R
Rscript scripts/full_text/99_run_tests.R
```

The October full-text pilot is operational under `data/2.full_abstracting/`;
independent regression expectations remain under `tests/full_text/`.

The pipeline is reusable with explicit new batch IDs. Scientific extraction remains
assisted work: verify each instrument's items/scoring, acquisition and codebook
mapping, and discuss substantive ambiguities. Tests of synthetic future-month
cases do not substitute for running a real batch with its actual source records.
