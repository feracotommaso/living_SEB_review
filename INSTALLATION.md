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
