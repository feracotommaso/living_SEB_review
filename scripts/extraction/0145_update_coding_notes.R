# Reconcile descriptive source notes after the verified quantitative extraction.
# Run after articles/0145.R, then rebuild October coding via the existing builder.
source("R/codebook_workflow.R")
path <- "data/3.meta_data/coding/artifacts/2026-10/0145.json"
history <- "data/3.meta_data/coding/artifacts/2026-10/history/0145.before_extraction_2026-11-04.json"
object <- jsonlite::fromJSON(path, simplifyVector = FALSE)
checks <- codebook_csv("data/3.meta_data/extraction/0145/checks.csv")
value <- function(key) as.numeric(checks$value[match(key, checks$check)])
stopifnot(object$paper_id == "0145", length(object$samples) == 1L,
  object$samples[[1]]$component_label == "Study 2", value("n") == object$samples[[1]]$n)
if (!file.exists(history)) {
  dir.create(dirname(history), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(path, history)) stop("Could not preserve pre-extraction coding.")
}
object$samples[[1]]$notes <- paste0(
  "Reference analytic N=973 in the author-selected Study 2 dataset (PDF pp.10-11; public original-item data verified against the source manifest). ",
  "Private-comment exclusions cannot be independently reconstructed. ", value("hexaco_missing_block"),
  " released records have the entire HEXACO block missing. Pearson correlations use available pairs; pair N ranges ",
  value("minimum_pair_n"), "-", value("maximum_pair_n"), ". Reference N remains 973; no participants are removed for missing age or the binary sex/gender contrast. ",
  "Male=0 and female=1; other categories/nonresponse are missing only for that contrast. ",
  "Age range 18-49 is coded adult under the mixed-adult convention. The two sessions measured different constructs rather than repeated SEB waves. ",
  "BESSI-96 form type retains the historical long classification. ",
  "Distinct persuasion/perspective scoring was confirmed by the user on 2026-11-04; domain weights are normalised to retain the 1-5 metric. ",
  "Observed task components and questionnaire means are retained separately from latent factors. ",
  "The pairwise matrix has a small negative minimum eigenvalue (", format(value("minimum_eigenvalue"), scientific = TRUE, digits = 5),
  "); observed correlations were not repaired. Extraction audits are under extraction/0145; current analytical integration status is recorded separately in integration/batches.csv and integration/matrices.csv.")
for (i in seq_along(object$evidence)) if (object$evidence[[i]]$field == "open_data")
  object$evidence[[i]]$evidence <- "The paper identifies OSF an64w as its source for Study 2 data/code; actual retrieval and checksums are recorded separately in the source manifest."
sources <- list(
  list(unit = "paper", field = "open_data", source = "data/3.meta_data/extraction/0145/source_manifest.csv",
    evidence = "Eight Study 2 sources acquired and verified against pinned OSF SHA-256 values; individual records remain local."),
  list(unit = "Study 2|Analytic sample", field = "notes", source = "data/3.meta_data/extraction/0145/checks.csv",
    evidence = "Extraction checks establish the reference N, missing HEXACO blocks, available-pair N range and unrepaired minimum eigenvalue."),
  list(unit = "Study 2|Analytic sample", field = "notes", source = "data/3.meta_data/extraction/0145/scientific_decisions.csv",
    evidence = "Records the user-confirmed facet correction, normalised domain weighting and author-selected cohort provenance."))
for (entry in sources) {
  object$evidence <- Filter(function(ev) !(identical(ev$unit, entry$unit) && identical(ev$field, entry$field) && identical(ev$source, entry$source)), object$evidence)
  object$evidence[[length(object$evidence) + 1L]] <- list(unit = entry$unit, field = entry$field,
    status = "derived", source = entry$source, page = NULL, evidence = entry$evidence,
    source_sha256 = digest::digest(entry$source, algo = "sha256", file = TRUE))
}
jsonlite::write_json(object, path, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
cat("0145 coding source notes updated; pre-extraction artifact retained in history.\n")
