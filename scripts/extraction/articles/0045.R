# 0045: approved historical correction; the original comparison stays visible.
# A normal run exports the reviewed extraction and updates the cumulative audit.
# --check-only writes nothing. Run from the repository root.
source("R/historical_audit.R")
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% "--check-only"))
source_hashes <- c(
  "data/3.meta_data/open_data/d0045.xlsx"="43a659399c641d8bfdd4ed0f202887cb1974c3f80453c7eaa4c66e8f4f2e19ac",
  "data/2.full_abstracting/pdfs/0045.pdf"="0e996e225e8958c667a44410902abf24176165d9d07eb0b6e62acc035099159a",
  "data/3.meta_data/open_data/raw/shared/bessi_scoring_v1.pdf"="9d842f5196f021131be8b6561cc70b17dd0deebdaca2cbdc2f3579dce8e0b2a9")
audit_verify_sources(source_hashes)

source("scripts/extraction/keys/bessi192.R",local=TRUE)
# 1-3. PDF pp.4-7: N=248 students, eight facets (48 items), two volunteering
# program sessions. Keep every record; no retest or additional exclusions.
raw <- as.data.frame(readxl::read_excel(names(source_hashes)[1]))
stopifnot(nrow(raw)==248,!anyDuplicated(raw$user.id))
measured_facets <- c("taskmanagement","leadershipskill","creativeskill",
  "abstractthinkingskill","culturalcompetence","perspectivetakingskill",
  "stressregulation","capacityforconsistency")

# 4-5. Independently rebuild all eight measured facets. Preserve the historical
# available-item mean rule: all-missing is missing; observed facets have 4-6
# responses here. The article models latent factors/FIML; it does not define
# these observed-score correlations as its published Table 3 coefficients.
data_scored <- data.frame(row.names=seq_len(nrow(raw)))
for(facet in measured_facets) {
  columns <- paste0("bessi_",seq(bessi192_first_item[[facet]],192,32))
  items <- as.matrix(raw[,columns])
  stopifnot(all(is.na(items) | (items>=1 & items<=5)))
  value <- rowMeans(items,na.rm=TRUE);value[is.nan(value)] <- NA_real_
  data_scored[[facet]] <- value
}
stopifnot(all(is.na(raw$gender) | raw$gender %in% c(0,1,2)))
# Female=0 and male=1 in source; other=2 and missing stay missing for this contrast.
# Counts agree with the participant table (187 females, 55 males).
data_scored$sex <- ifelse(raw$gender==2,NA_real_,1-raw$gender)
data_scored$age <- raw$age
data_scored$volunteering <- raw$hour_group # Historical ordinal 1/2/3 outcome.
legacy_facets <- setdiff(measured_facets,"capacityforconsistency")
data_matrix <- data_scored[,c("sex","volunteering",legacy_facets)]
note <- "Released N=248 matches article. Reconstructed the seven historical facets from original items; capacity for consistency is also measured but omitted historically. Missing items retain the existing available-item mean rule, all-missing facets remain missing. Age retained separately; sex other/missing not forced into the binary contrast. Historical volunteering is hour_group 1=zero, 2=some, 3=benchmark hours. Table 3 reports latent correlations with volunteering hours and is not an observed-score rounding reference."

# 6-8. Reproduction and separate proposals; nothing promoted.
audit_0045 <- list()
audit_0045$source <- audit_compare_matrix("0045","0045a",data_matrix,248,
  note=note,sources=names(source_hashes),evidence="0045 PDF pp.4-7 and item appendix p.14; official BESSI facet key",
  status="HISTORICAL_SCORING_COMPARISON")
facet_candidate <- data_matrix
facet_candidate$capacityforconsistency <- data_scored$capacityforconsistency
audit_0045$facet <- audit_compare_matrix("0045","0045a",facet_candidate,248,
  note=paste(note,"Isolated facet comparison: include capacityforconsistency but preserve historical ordinal volunteering. The approved export combines this facet with continuous total hours."),
  sources=names(source_hashes),evidence="PDF Table 3 p.7 and Appendix p.14; six source items bessi_9:169 in steps of 32",
  status="ISOLATED_FACET_COMPARISON",comparison="consistency_facet_augmentation")
hours_candidate <- data_matrix
hours_candidate$volunteering <- raw$hours.total
audit_0045$hours <- audit_compare_matrix("0045","0045a",hours_candidate,248,
  note=paste(note,"Isolated metric comparison: replace the ordinal group outcome with observed total volunteer hours; preserve the seven historical facets to isolate this choice. The approved combined export also includes capacityforconsistency."),
  sources=names(source_hashes),evidence="Source hour_group and hours.total; PDF volunteering measure p.6",
  status="ISOLATED_METRIC_COMPARISON",comparison="continuous_hours_sensitivity")
summary_0045 <- dplyr::bind_rows(lapply(audit_0045,`[[`,"summary"))
print(summary_0045[,c("matrix_id","comparison","changed_pairs","max_abs_delta","added_variables","pair_n_min","pair_n_max")])
combined_data <- facet_candidate
combined_data$volunteering <- raw$hours.total
audit_0045$combined <- audit_compare_matrix("0045","0045a",combined_data,248,
  note=paste(note,"User approval 2026-11-13: include capacityforconsistency and use continuous hours.total for volunteering, preserving every participant and the available-item mean rule."),
  sources=names(source_hashes),evidence="PDF Table 3 p.7, item appendix p.14 and volunteering measure p.6; original hours.total",
  status="APPROVED_EXTRACTION",comparison="approved_extraction")

# 8. Approved 2026-11-13. Export only the reviewed extraction; keep the original
# mapping above visible for comparison. --check-only never changes any files.
data_matrix <- combined_data
approved <- audit_0045$combined
approved$summary$status <- "APPROVED_EXTRACTION"
approved$summary$comparison <- "approved_extraction"
approved$summary$note <- "Include all eight measured BESSI facets, including capacityforconsistency, and use observed continuous hours.total for volunteering. Historical outcome was the ordinal hour_group (session benchmarks 40 summer/20 fall). Preserve available-item means, N=248 and pair Ns=236-242; age kept only in individuals. Approved by user 2026-11-13. A normal run exports this extraction; --check-only never writes."
summary_0045 <- dplyr::bind_rows(summary_0045[summary_0045$comparison!="approved_extraction",],approved$summary)
if(!"--check-only" %in% args) {
  source("R/codebook_workflow.R",local=TRUE)
  source("R/extraction_helpers.R",local=TRUE)
  source("R/workflow_io.R",local=TRUE)
  source("R/workflow_results.R",local=TRUE)
  path <- "data/3.meta_data/extraction/historical_audit.csv"
  previous <- readr::read_csv(path,show_col_types=FALSE)
  first <- approved$summary
  first$comparison <- "approved_integration_2026_11_13"
  first$status <- "REVIEWED_BEFORE_AFTER"
  first <- first[!first$matrix_id %in% previous$matrix_id[previous$comparison==first$comparison],]
  audit_save_summary(dplyr::bind_rows(summary_0045,first))
  individual_data <- data_matrix
  individual_data$age <- as.numeric(raw$age)
  checks <- extraction_check_matrix(data_matrix,approved$candidate,approved$pair_n)
  wf_compact_extraction("0045","0045a",individual_data,approved$candidate,approved$pair_n,checks)
}
