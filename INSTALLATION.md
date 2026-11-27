# Local workflow setup

Use this repository's checked-out branch directly. No package overlays or manual
file copying are required. Preserve existing data and configuration.

Full-text commands require R packages `readr`, `readxl`, `dplyr`, `digest`,
`openxlsx`, `jsonlite`, `xml2`, `zip`, and `here`. Tests also require `testthat`, `withr`,
and `yaml` (title/abstract policy checks). The local full-text schema validator
needs no optional JSON Schema runtime or network access.

For deduplication dependencies, read
[`scripts/dedup/README.md`](scripts/dedup/README.md) before running those scripts.
The title/abstract skill is in `.agents/skills/living-seb-title-abstract/`.
The full-text skill is in `.agents/skills/living-seb-full-text/`; invoke it as
`$living-seb-full-text` with the requested batch ID.

```bash
Rscript scripts/screening/99_run_tests.R
Rscript scripts/screening/03_validate_2026_10_regression.R
```

See [MONTHLY_WORKFLOW.md](MONTHLY_WORKFLOW.md) for the sequence and
[full-text guidance](scripts/full_text/README.md) for commands, CSV/XLSX contracts,
one-time existing-workbook import and Windows locale settings.
PDFs remain in the ignored `data/2.full_abstracting/pdfs/` directory.

## MetaSEM app and analyses

The revised app requires **metaSEM >= 2.0** and **lavaan >= 0.7-3** (R >= 4.2).
The tested release bundle uses R 4.5.1 and OpenMx 2.22.7; this OpenMx version
requires R >= 4.5.0. Use R 4.5 or newer for this package combination. Restart R after
installing or updating either package. The app checks these minimum versions
at startup and records the versions in its MetaSEM reports and data exports.

```r
install.packages(c("lavaan", "metaSEM"), repos = "https://cran.r-project.org")
packageVersion("metaSEM")
packageVersion("lavaan")
```

The one-stage model remains `RE.type="Zero"`, `acov="weighted"`, with Wald-z
intervals. Fixed directed coefficients and variables without directed paths are
supported; feedback loops remain unsupported by this correlation-model path.
The exploratory `tssem1()` pooling remains a separate calculation with its
existing defaults. Local version-comparison results and previous package copies
are retained under Git-ignored `archive_local/metasem_version_check_2026_11_26/`.

## App release

The published [Living SEB App 0.2.0 release](https://github.com/feracotommaso/living_SEB_review/releases/tag/app-v0.2.0)
records deployed commit `bbfe33d` (27 November 2026), with the updated Pages
manual and online checks completed. Its Zenodo archive is
[10.5281/zenodo.24592687](https://doi.org/10.5281/zenodo.24592687).
Use the [stable app link](https://feracotommaso.github.io/living_SEB_review/shiny-app.html)
when sharing the app.

Render `appManual/livingSEBapp_manual.qmd` to HTML and PDF, then version the
reviewed app, manual, citation files and tutorials. Merge/push the approved
changes to `main` and wait for GitHub Pages to publish the updated manual before
deploying `app/`: standalone deployments link to the published Pages manual and
load the citation/data files from `main`.

Deploy the complete `app/` directory, including its `R/` helpers, both report
templates and `report.css`. `rsconnect::listDeploymentFiles("app")` lists the
payload; `rsconnect::appDependencies("app")` resolves package versions. Keep the
local manual backup, raw/individual data and validation artifacts outside this
payload. After deployment, check startup and one current analysis/report online.

Create the GitHub release/tag on the commit actually deployed, then verify the
matching version, GitHub tag link and downloadable archive on Zenodo. Record the
version DOI; the [concept DOI](https://doi.org/10.5281/zenodo.19101246) groups all
versions. App and monthly data releases have separate tags. The live app loads
data from `main` at process startup, so a software tag alone does not identify
the data used by a later session: retain the exported analysis data and report.

## Paper formats

APAquarto is stored only in `paper/_extensions/`. The paper selects
`Times New Roman` explicitly for Typst; the font must be installed and visible
in `quarto typst fonts` on the rendering computer.

For Word, the local `frontmatter.lua` uses the bundled PNG of the ORCID icon;
HTML and Typst retain the original SVG. This small local adaptation avoids an
external SVG-converter requirement for Word. Preserve it when updating APAquarto.
The PNG is a rasterization of the original 256-by-256 SVG, generated with:

```r
rsvg::rsvg_png(
  "paper/_extensions/wjschne/apaquarto/ORCID-iD_icon-vector.svg",
  file = "paper/_extensions/wjschne/apaquarto/ORCID-iD_icon.png",
  width = 256, height = 256
)
```
