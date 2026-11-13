# 0043: approved historical correction; the original comparison stays visible.
# A normal run exports the reviewed extraction and updates the cumulative audit.
# --check-only writes nothing. Run from the repository root.
source("R/historical_audit.R")
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% "--check-only"))
source_hashes <- c(
  "data/3.meta_data/open_data/d0043.sav"="6ea049a8aa2fc048bba4b6c387a458eb207be7c9dbd6abc24c3d43e62952e286",
  "data/3.meta_data/open_data/opencode0043.sps"="69af0f5894bf3c49b55d32ab222f06831a7c1ce521b2454467964591f66f47b4",
  "data/2.full_abstracting/pdfs/0043.pdf"="a5ad1335e21bda0cf9bd9e831eb8e77ba57a0ed0156a9bffb93a1e39cd21f143")
audit_verify_sources(source_hashes)

# 1-3. Prepared N=975, PDF p.3: union of usable skill, trait and Mosaic data
# after preregistered exclusions. Item responses/exclusion flags are not released
# here, so do not claim independent item scoring or reapply exclusions.
raw <- as.data.frame(haven::read_sav(names(source_hashes)[1]))
stopifnot(nrow(raw)==975,!anyDuplicated(raw$Case))

# 4-5. BESSI-20 skill and frequency/trait versions are distinct measures.
# Trait Emotional Resilience is negated only to match neuroticism direction.
# Fix the source spelling explicitly, but omit parentEducation in the baseline
# to reproduce the historical omission: old code used unregistered ParentEducation.
variable_map <- c(
  "ParentsEducation"="parentEducation",
  "Skill_SelfManagement"="selfmanagement",
  "Skill_SocialEngagement"="socialengagement",
  "Skill_Cooperation"="cooperation",
  "Skill_EmotionalResilience"="emotionalresilience",
  "Skill_Innovation"="innovation",
  "Trait_SelfManagement"="conscientiousness",
  "Trait_Innovation"="openness",
  "Trait_Cooperation"="agreeableness",
  "Trait_SocialEngagement"="extraversion",
  "Trait_EmotionalResilience"="neuroticism",
  "Mosaic_SustainingEffort"="mosaicSustainingEffort",
  "Mosaic_SocialConnection"="mosaicSocialConnection",
  "Mosaic_GettingAlongWithOthers"="mosaicGettingAlong",
  "Mosaic_MaintainingComposure"="mosaicComposure",
  "Mosaic_KeepingAnOpenMind"="mosaicOpenMind",
  "ActTestScore"="standardizedacademicachievement",
  "OverallGPA"="academicachievement",
  "SchoolAttendance"="schoolattendance",
  "EducationalAspirations"="educationalaspiration")
data_scored <- raw
names(data_scored)[match(names(variable_map),names(data_scored))] <- unname(variable_map)
data_scored$neuroticism <- -data_scored$neuroticism
stopifnot(all(is.na(raw$Gender) | raw$Gender %in% c(-1,1)))
data_scored$sex <- ifelse(raw$Gender==-1,0,1) # SPSS labels Male=-1, Female=1.
legacy_variables <- c(setdiff(unname(variable_map),"parentEducation"),"sex")
data_matrix <- data_scored[,legacy_variables]
note <- "Released N=975 matches the article. Supplied BESSI-20 skill/trait and Mosaic scores reproduced as in the historical matrix; raw items and pre-imputation versions unavailable. The five trait measures are frequency-rated BESSI items, not BFI-2. Age absent (grade level is not age). No cohort or missingness changes."

# 6-8. Keep the current matrix; quantify the missing canonical parent variable.
audit_0043 <- list()
audit_0043$source <- audit_compare_matrix("0043","0043a",data_matrix,975,
  note=note,sources=names(source_hashes),evidence="0043 PDF pp.3-4; original processed data and SPSS analysis syntax",
  status="HISTORICAL_SCORING_COMPARISON")
parent_candidate <- data_matrix
parent_candidate$parentEducation <- as.numeric(raw$ParentsEducation)
audit_0043$parent <- audit_compare_matrix("0043","0043a",parent_candidate,975,
  note=paste(note,"Approved recovery of ParentsEducation as registered parentEducation. Historical spelling ParentEducation caused its omission during codebook selection. 835 observed source scores, 140 missing. Originally added twenty correlations without changing old coefficients."),
  sources=names(source_hashes),evidence="Original ParentsEducation; historical rename; matrix_codebook.csv exact canonical name",
  status="APPROVED_COMPARISON",comparison="parent_education_augmentation")
summary_0043 <- dplyr::bind_rows(lapply(audit_0043,`[[`,"summary"))
print(summary_0043[,c("matrix_id","comparison","changed_pairs","added_variables","pair_n_min","pair_n_max")])
# 8. Approved 2026-11-13. Export only the reviewed extraction; keep the original
# mapping above visible for comparison. --check-only never changes any files.
data_matrix <- parent_candidate
approved <- audit_0043$parent
approved$summary$status <- "APPROVED_EXTRACTION"
approved$summary$comparison <- "approved_extraction"
approved$summary$note <- "Recover ParentsEducation as registered parentEducation (835 observed, 140 missing); the historical uppercase-P spelling caused omission. Supplied processed BESSI-20, trait and Mosaic scores retained; N=975 and pair Ns=627-910. Raw item scoring unavailable; no new exclusions. Approved by user 2026-11-13. A normal run exports this extraction; --check-only never writes."
summary_0043 <- dplyr::bind_rows(summary_0043[summary_0043$comparison!="approved_extraction",],approved$summary)
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
  audit_save_summary(dplyr::bind_rows(summary_0043,first))
  individual_data <- data_matrix
  # No age in the supplied data; grade level is not age.
  checks <- extraction_check_matrix(data_matrix,approved$candidate,approved$pair_n)
  wf_compact_extraction("0043","0043a",individual_data,approved$candidate,approved$pair_n,checks)
}
