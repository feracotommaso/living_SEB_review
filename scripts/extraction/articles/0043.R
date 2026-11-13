# 0043: historical verification only. No scientific outputs are promoted.
# A normal run updates only extraction/historical_audit.csv.
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
  status="SCORE_LEVEL_MATRIX_REPRODUCED")
parent_candidate <- data_matrix
parent_candidate$parentEducation <- as.numeric(raw$ParentsEducation)
audit_0043$parent <- audit_compare_matrix("0043","0043a",parent_candidate,975,
  note=paste(note,"Proposal only: recover ParentsEducation as registered parentEducation. Historical spelling ParentEducation caused its omission during codebook selection. 835 observed source scores, 140 missing. Adds twenty correlations; no original coefficients changed. Not exported."),
  sources=names(source_hashes),evidence="Original ParentsEducation; historical rename; matrix_codebook.csv exact canonical name",
  status="VARIABLE_ADDITION_REVIEW_REQUIRED",comparison="parent_education_augmentation")
summary_0043 <- dplyr::bind_rows(lapply(audit_0043,`[[`,"summary"))
print(summary_0043[,c("matrix_id","comparison","changed_pairs","added_variables","pair_n_min","pair_n_max")])
if(!"--check-only" %in% args)audit_save_summary(summary_0043)
