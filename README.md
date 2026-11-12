# A Living Database for Review and Meta-Analysis of SEB Skills Studies

This repository contains all the open materials of the **Living SEB Skills Project**, a living meta-analysis on Social, Emotional, and Behavioral (SEB) skills updated regularly, alongside an interactive Shiny application for exploring the results.

[![Zenodo DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20398350.svg)](https://doi.org/10.5281/zenodo.20398350)

[![Latest release](https://img.shields.io/github/v/release/feracotommaso/living_SEB_review)](https://github.com/feracotommaso/living_SEB_review/releases)


## Main Links

- 🔗 **Shiny app:** [https://feracotommaso.github.io/living_SEB_review/shiny-app.html](https://feracotommaso.github.io/living_SEB_review/shiny-app.html)
- **App manual:** [HTML](https://feracotommaso.github.io/living_SEB_review/appManual/livingSEBapp_manual.html) · [PDF](https://feracotommaso.github.io/living_SEB_review/appManual/livingSEBapp_manual.pdf)
- 📄 **Postprint:** [https://doi.org/10.31234/osf.io/ewvuf_v2](https://doi.org/10.31234/osf.io/ewvuf_v2)
- 📝 **Published paper:** [https://doi.org/10.1177/08902070261467477](https://doi.org/10.1177/08902070261467477)
- 📊 **Updated living results:** [feracotommaso.github.io/living_SEB_results](https://feracotommaso.github.io/living_SEB_review/paper/livingSEBresults.html)


## Repository Structure

For the reproducible monthly pipeline, see [Monthly workflow](MONTHLY_WORKFLOW.md)
and [Full-text eligibility](scripts/full_text/README.md). Setup instructions are in
[INSTALLATION.md](INSTALLATION.md).

```text
├── workflow.R                  # Entry point for monthly updates and corrections
├── MONTHLY_WORKFLOW.md         # Step-by-step guide, files and manual interventions
├── INSTALLATION.md             # Dependencies and document rendering setup
├── app/                        # Shiny application and downloadable-report templates
│   └── R/utils.R               # Single implementation of shared analysis helpers
├── R/                          # Workflow functions; utils.R loads the shared helpers
├── scripts/                    # Runnable steps for deduplication, screening and coding
│   └── extraction/articles/    # One explicit scoring/extraction script per article
├── data/
│   ├── 0.bib_download/         # Original bibliographic exports
│   ├── 0.bib_normalized/       # Derived copies for bibliographic import
│   ├── 0.deduplication/        # Import manifests and duplicate-review decisions
│   ├── 0.registry/            # Active bibliography and versioned snapshots
│   ├── 1.abstracting/         # Historical exports and current screening workbooks
│   ├── 2.full_abstracting/    # Full-text decisions, paper IDs and local PDFs
│   ├── 3.meta_data/
│   │   ├── basic_info/        # Historical manually coded sample/article information
│   │   ├── coding/           # Canonical coding CSVs and general coding workbook
│   │   ├── matrices/         # Common correlation-matrix directory
│   │   ├── pair_n/           # Sample sizes for individual correlations
│   │   ├── extraction/       # Aggregated extraction checks
│   │   ├── meta_data/        # Derived quantitative datasets (CSV/XLSX)
│   │   ├── review_data/      # Derived review datasets (CSV/XLSX)
│   │   ├── last_update.xlsx  # Latest integration preview, changes and issues
│   │   └── open_data/        # Local originals and coded individual data; Git-ignored
│   ├── matrix_codebook.*     # Variable codebook (CSV/XLSX)
│   ├── topics_codebook.*     # Review-topic codebook (CSV/XLSX)
│   └── search_results_history.* # Reconstructed acquisition-batch counts (CSV/XLSX)
├── config/                    # Workflow settings and coding conventions
├── policies/                  # Scientific eligibility and extraction rules
├── prompts/                   # Structured AI judgment instructions
├── schemas/                   # Decision-output schemas
├── tests/                     # Workflow checks and validated reference cases
├── .agents/skills/             # Repository screening skills
├── paper/
│   ├── livingSEBpaper.qmd      # Scientific paper: APAquarto formats
│   ├── livingSEBresults.qmd    # Updated results: standard HTML
│   ├── _extensions/           # Only APAquarto extension copy
│   ├── materials/             # Search strategy document
│   └── PRISMA_2020_flow_diagram_new_SRs_v1.docx # Historical Word template
├── appManual/                 # Standard HTML/PDF manual, Quarto source and CSS
├── preregistration/           # Preregistration source and PDF
├── slides/                    # Quarto Reveal.js presentations
├── tutorialAnalysis/          # Public example analysis scripts
├── index.Qmd / index.html     # Public project homepage
├── shiny-app.html             # Redirect to the deployed Shiny application
├── CONTRIBUTING.md            # Contribution guidelines
├── CHANGELOG.md               # Versioned workflow changes
├── LICENSE-*                  # Software, data, output and documentation licenses
└── living_SEB_review.Rproj     # RStudio project
```

CSV is canonical; XLSX files are the human interface or derived outputs. The
historical R scripts directly under `data/` remain available until the articles
have been recoded with the current method. Local analysis trials are kept in
`archive_local/analysis_trials/`, excluded from Git alongside private individual
data and downloaded PDFs.

Rendered HTML/PDF files accompany the document sources. GitHub Pages publishes
the files on its configured publishing branch; updating the deployed Shiny code
is a separate operation from updating this repository.


## Licensing

| Component             | Directory                                     | License                        |
| --------------------- | --------------------------------------------- | ------------------------------ |
| Shiny app & R scripts | app/, tutorialAnalysis/                       | AGPL-3.0                       |
| Curated dataset       | data/                                         | CC-BY 4.0                      |
| App outputs           | —                                             | CC-BY 4.0                      |
| Paper, docs, slides   | paper/, slides/, preregistration/, appManual/ | CC-BY 4.0                      |
| Raw data              | —                                             | Not redistributed (no license) |


## Contributing

Please read [`CONTRIBUTING.md`](CONTRIBUTING.md) for details. If you find this project useful, consider ⭐ starring the repository, sharing it with colleagues, or citing it in your work.


## Citation

Feraco, T., Calderan, M. & Pellegrino, G. (2026). The Living SEB Skills Project: A Living Resource for Review and Meta-Analysis of Social, Emotional, and Behavioral Skills. *European Journal of Personality*. [https://doi.org/10.1177/08902070261467477](https://doi.org/10.1177/08902070261467477)

```bibtex
@Article{livingSEB,
  author    = {Tommaso Feraco and Margherita Calderan and Gerardo Pellegrino},
  doi       = {https://doi.org/10.1177/08902070261467477},
  title     = {The Living SEB Skills Project: A Living Resource for Review
               and Meta-Analysis of Social, Emotional, and Behavioral Skills},
  year      = {2026},
  publisher = {European Journal of Personality}
}
```


### Data

[![Latest release](https://img.shields.io/github/v/release/feracotommaso/living_SEB_review)](https://github.com/feracotommaso/living_SEB_review/releases)

The dataset is updated approximately once a month. 
This project is linked to **Zenodo**, and each dataset update will trigger a new release. If no new articles are available, a no-data release will be published anyway.
To ensure reproducibility, please cite a specific release using its version tag.
