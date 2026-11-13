# 0013: read-only historical verification. No scientific output is promoted.
# Run: Rscript scripts/extraction/articles/0013.R [--check-only]
# A normal run updates only extraction/historical_audit.csv.
source("R/historical_audit.R")
args <- commandArgs(trailingOnly = TRUE)
stopifnot(all(args %in% "--check-only"))
source_hashes <- c(
  "data/3.meta_data/open_data/d0013.csv" = "70a14af47173200fb9b7964dfb53357791cb5d9e61bab43af1ca10c72922dbe7",
  "data/2.full_abstracting/pdfs/0013.pdf" = "b068cf2df69844b1c2f435d23492b632f7d8edfc4300c5eb3d7efe44e2187f58")
audit_verify_sources(source_hashes)

# 1-3. The article reports N=840 (PDF pp. 3-5). The supplied score-level
# dataset has 897 records and no exclusion flags or demographic columns.
# Do not invent a participant filter to force the reported reference N.
raw_scores <- as.data.frame(readr::read_csv(names(source_hashes)[1], show_col_types=FALSE))
stopifnot(nrow(raw_scores)==897, !anyDuplicated(raw_scores$Case))
data_scored <- raw_scores

# 4. The article keyed Neuroticism as Emotional Stability (p. 4).
# Negation restores the direction of the correlation; this is not an item-level
# reversal or a reconstruction of the original questionnaire scale.
data_scored$BFI_EmotionalStability <- -data_scored$BFI_EmotionalStability

# 5. Explicit source-to-codebook mapping, preserving the historical constructs.
variable_map <- c(
  "BFI_OpenMindedness" = "openness",
  "BFI_Conscientiousness" = "conscientiousness",
  "BFI_Extraversion" = "extraversion",
  "BFI_Agreeableness" = "agreeableness",
  "BFI_EmotionalStability" = "neuroticism",
  "BESSI_SelfManagement" = "selfmanagement",
  "BESSI_Cooperation" = "cooperation",
  "BESSI_SocialEngagement" = "socialengagement",
  "BESSI_EmotionalResilience" = "emotionalresilience",
  "BESSI_Innovation" = "innovation",
  "AcademicEngagement" = "academicengagement",
  "SchoolGrades" = "academicachievement",
  "MotherRelationshipQuality" = "motherrelationship",
  "FatherRelationshipQuality" = "fatherrelationship",
  "FriendshipQuality" = "friendshipquality",
  "PeerAcceptance" = "peeracceptance",
  "SocialResponsibilityValues" = "socialresponsibility",
  "CivicSkills" = "civicskills",
  "Anxiety" = "anxiety",
  "Depression" = "depression",
  "LifeSatisfaction" = "satisfactionwithlife")
names(data_scored)[match(names(variable_map),names(data_scored))] <- unname(variable_map)
data_matrix <- data_scored[, unname(variable_map), drop=FALSE]

# 6-7. Pairwise correlations, retaining all released observations as before.
# Scoring cannot be independently validated without original items. The record
# count and the article's analytic cohort remain an explicit review question.
audit_0013 <- audit_compare_matrix("0013","0013a",data_matrix,840,
  note="Supplied score-level data: 897 rows, article reference N=840. All 897 have at least one score; 896 have at least two; 475 are complete across the 21 variables. Big Five scores available for 840 people, BESSI domains for 838; pair Ns 573-840. Existing pairwise correlations already use all available released observations and reproduce the stored matrix. Keep reference N=840 pending cohort clarification; 897 is a file-row count, not an N achieved by any correlation. No source eligibility flags; do not force N by filtering. Original selection and item scoring not independently reconstructed. Age and sex absent. Author clarification needed to identify the intended analytic cohort.",
  sources=names(source_hashes), evidence="0013 PDF pp.3-5; downloaded author analysis opencode0013.R loads scores without an explicit selection step",
  status="REVIEW_ONLY_COHORT_REPRESENTED_BY_0048")
audit_0013$summary$note <- paste(audit_0013$summary$note,
  "User decision 2026-11-13: 0013 remains review-only; 0048 is the richer quantitative representative. This matrix is preserved as an inactive historical artifact. Its historical N does not enter current meta-analytic weighting.")
print(audit_0013$summary[,c("matrix_id","input_rows","changed_pairs","max_abs_delta","pair_n_min","pair_n_max")])
# 8. Only the aggregate audit is versioned. No matrices/individuals are exported.
if (!"--check-only" %in% args) audit_save_summary(audit_0013$summary)
