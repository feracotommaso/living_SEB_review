# 0048: approved representative of the cohort also reported in 0013.
# --check-only writes nothing. Original item scoring is unavailable locally.
# Paper data/code: https://osf.io/bcgq7/. Historical retrieval date unknown.
source("R/historical_audit.R")
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% "--check-only"))
source_hashes <- c(
  "data/3.meta_data/open_data/d0048.sav"="f7a5f43a6cd650a956bd4ed169df4ab4d37d692a95898b163c9a7cd2ead33673",
  "data/2.full_abstracting/pdfs/0048.pdf"="8f7edeaa64675f36465bb4e591728097b903204f685b9a462ee94e800f27a63f",
  "data/3.meta_data/open_data/d0013.csv"="70a14af47173200fb9b7964dfb53357791cb5d9e61bab43af1ca10c72922dbe7",
  "data/2.full_abstracting/pdfs/0013.pdf"="b068cf2df69844b1c2f435d23492b632f7d8edfc4300c5eb3d7efe44e2187f58")
audit_verify_sources(source_hashes)

# 1-3. Paper p.3: 897 students after exclusions, with block/measure-specific
# eligibility. The released file is already processed and has no raw items.
raw <- as.data.frame(haven::read_sav(names(source_hashes)[1]))
stopifnot(nrow(raw)==897,!anyDuplicated(raw$Case))
# 4-5. Retain supplied scores, canonical mapping and historical binary coding.
variable_map <- c(
  "Age"="age",
  "BESSI_SelfManagement"="selfmanagement",
  "BESSI_SocialEngagement"="socialengagement",
  "BESSI_Cooperation"="cooperation",
  "BESSI_EmotionalResilience"="emotionalresilience",
  "BESSI_Innovation"="innovation",
  "BFI_Conscientiousness"="conscientiousness",
  "BFI_OpenMindedness"="openness",
  "BFI_Agreeableness"="agreeableness",
  "BFI_Extraversion"="extraversion",
  "BFI_NegativeEmotionality"="neuroticism",
  "SECA_SelfManagement"="secaSelfmanagement",
  "SECA_RelationshipSkills"="secaRelationships",
  "SECA_SocialAwareness"="secaSocialawareness",
  "SECA_SelfAwareness"="secaSelfawareness",
  "SECA_ResponsibleDecisionMaking"="secaDecisionmaking",
  "AcademicEngagement"="academicengagement",
  "SchoolGrades"="academicachievement",
  "SchoolAttendance"="schoolattendance",
  "SchoolDisciplineReferral"="schooldiscipline",
  "RealisticInterests"="realisticinterests",
  "InvestigativeInterests"="investigativeinterests",
  "ArtisticInterests"="artisticinterests",
  "SocialInterests"="socialinterests",
  "EnterprisingInterests"="enterprisinginterests",
  "ConventionalInterests"="conventionalinterests",
  "PeerAcceptance"="peeracceptance",
  "FriendshipQuality"="friendshipquality",
  "MotherRelationshipQuality"="motherrelationship",
  "FatherRelationshipQuality"="fatherrelationship",
  "SocialResponsibilityValues"="socialresponsibility",
  "CivicSkills"="civicskills",
  "VotingIntention"="votingintention",
  "CivicOrganizationInvolvement"="civicinvolvement",
  "Activism"="activism",
  "InformalHelping"="informalhelping",
  "Volunteerism"="volunteering",
  "PhysicalExercise"="physicalexercise",
  "Anxiety"="anxiety",
  "Depression"="depression",
  "LifeSatisfaction"="satisfactionwithlife")
data_scored <- raw
names(data_scored)[match(names(variable_map),names(data_scored))] <- unname(variable_map)
stopifnot(all(is.na(raw$Gender) | raw$Gender %in% c(-1,1)))
data_scored$sex <- ifelse(raw$Gender==-1,0,1)
data_matrix <- data_scored[,c(setdiff(unname(variable_map),"age"),"sex")]

# 6. Check the complete overlap with 0013, without changing either inclusion.
other <- as.data.frame(readr::read_csv(names(source_hashes)[3],show_col_types=FALSE))
link <- match(other$Case,raw$Case)
shared <- setdiff(intersect(names(other),names(raw)),"Case")
stopifnot(nrow(other)==897,!anyNA(link),!anyDuplicated(other$Case),length(shared)==20)
for(variable in shared)stopifnot(isTRUE(all.equal(other[[variable]],as.numeric(raw[[variable]][link]),
  check.attributes=FALSE,tolerance=1e-10)))
saved_0013 <- as.matrix(readxl::read_excel("data/3.meta_data/matrices/0013a.xlsx"))
rownames(saved_0013) <- colnames(saved_0013)
candidate <- cor(data_matrix,method="pearson",use="pairwise.complete.obs")
common <- colnames(saved_0013)
overlap_delta <- abs(saved_0013-candidate[common,common])
stopifnot(all(overlap_delta[upper.tri(overlap_delta)]<1e-10))

# 7-8. Reproduction is distinct from independence. Keep scientific outputs as-is
# while the user discusses whether to select the richer 0048 matrix for analyses.
audit_0048 <- audit_compare_matrix("0048","0048a",data_matrix,897,
  note=paste("Released 897 rows; all 820 saved coefficients reproduced. Pair Ns 428-840, not 897 for every correlation. Supplied scores only; source selection/item scoring cannot be reconstructed.",
    "Complete overlap with 0013: all 897 Case IDs link, all 20 identically named scores and their missingness agree, and all 210 common correlations agree after neuroticism direction harmonization.",
    "0013 Methods p.3 explicitly states that the same data were previously analyzed by Soto et al. User decision 2026-11-13: use 0048 as the quantitative representative; 0013 remains review-only and its matrix is preserved as an inactive historical artifact. Reference N=897 retained by explicit decision, with pair Ns documented.",
    "Released sex has 504 female, 387 male and six missing, whereas published demographic percentages differ; retain source values and flag provenance/version clarification. Age excluded from correlations."),
  sources=names(source_hashes),evidence="0048 PDF pp.3-5; 0013 Methods p.3; original Case-linked score files and stored matrices",
  status="APPROVED_COHORT_REPRESENTATIVE")
audit_0048$summary$overlap_paper_id <- "0013"
audit_0048$summary$overlap_linked_records <- length(link)
audit_0048$summary$overlap_identical_correlations <- sum(upper.tri(overlap_delta))
print(audit_0048$summary[,c("matrix_id","changed_pairs","pair_n_min","pair_n_max","overlap_identical_correlations")])
if(!"--check-only" %in% args) {
  source("R/codebook_workflow.R",local=TRUE)
  source("R/extraction_helpers.R",local=TRUE)
  source("R/workflow_io.R",local=TRUE)
  source("R/workflow_results.R",local=TRUE)
  audit_save_summary(audit_0048$summary)
  individual_data <- data_matrix
  individual_data$age <- as.numeric(raw$Age)
  checks <- extraction_check_matrix(data_matrix,audit_0048$candidate,audit_0048$pair_n)
  wf_compact_extraction("0048","0048a",individual_data,audit_0048$candidate,audit_0048$pair_n,checks)
}
