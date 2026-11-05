# 1. Article and sources ----------------------------------------------------
# Lubczyk et al. (2026), doi:10.1177/08902070261486523. Study 2 only.
# Source: OSF an64w, Supplement_S2.1.qmd (selection), Supplement_S2.2.qmd (scoring),
# and 01_Options.R (80% response threshold). See source_manifest.csv checksums.
# Run from repository root. --check-only scores/checks without writing outputs.
paper_id <- "0145"
matrix_id <- "0145a"
component_label <- "Study 2"
occasion_label <- "Single assessment over two sessions"
expected_n <- 973L
source("R/codebook_workflow.R")
source("R/extraction_helpers.R")
source("scripts/extraction/keys/0145.R") # Item/answer keys and output variable lists.
arguments <- extraction_arguments(commandArgs(trailingOnly = TRUE))

# 2. Import verified originals ---------------------------------------------
raw_dir <- "data/3.meta_data/open_data/raw/0145"
audit_dir <- "data/3.meta_data/extraction/0145"
source_manifest <- extraction_verify_sources(file.path(audit_dir, "source_manifest.csv"), paper_id)
raw_day1 <- readRDS(file.path(raw_dir, "thin_data_day1_sosci.rds"))
raw_day2 <- readRDS(file.path(raw_dir, "thin_data_day2_sosci.rds"))
raw_tailor <- readRDS(file.path(raw_dir, "thin_data_day2_tailor.rds"))
raw_analysis <- readRDS(file.path(raw_dir, "data_study2_analysis.rds"))

extraction_validate_scope("2026-10", paper_id, matrix_id, component_label)

# 3. Analytic sample --------------------------------------------------------
# The publicly supplied analysis dataset is the author-selected cohort, containing
# original items rather than final scale scores. Some selection used private
# participant comments; do not claim an independent reconstruction of those rules.
# Preserve author-applied missing blocks and do not restore excluded raw answers.
data_analytic <- as.data.frame(raw_analysis)
stopifnot(nrow(data_analytic) == expected_n, !anyNA(data_analytic$case_id),
          !anyDuplicated(data_analytic$case_id))
selection_filters <- c("filter_com", "filter_day", "filter_long_inst",
  "filter_hexaco_missing", "filter_hexaco_careless", "filter_bessi_missing", "filter_bessi_careless")
stopifnot(all(vapply(data_analytic[selection_filters], function(x) all(!is.na(x) & x == 0), logical(1))),
          all(!is.na(data_analytic$filter_break) & data_analytic$filter_break != 2),
          all(data_analytic$filter_time_tailor))

# Check source item correspondence for each link available in the released data.
# CASE.day1.sosci is missing in 23 records, whose HEXACO block is missing as well.
bessi_items <- c(paste0("PE02_", sprintf("%02d", 1:48)), paste0("PE03_", sprintf("%02d", 1:48)))
hexaco_items <- c(paste0("PH02_", sprintf("%02d", 1:50)), paste0("PH03_", sprintf("%02d", 1:50)))
source_links <- c(
  day1 = extraction_check_item_links(data_analytic, raw_day1, "CASE.day1.sosci", hexaco_items),
  day2 = extraction_check_item_links(data_analytic, raw_day2, "CASE.day2.sosci", bessi_items),
  tailor = extraction_check_item_links(data_analytic, raw_tailor, "CASE.day2.tailor"))

# 4. Recoding and scoring ---------------------------------------------------
data_scored <- data_analytic
# The source response code -9 denotes no answer. It must not enter a score.
questionnaire_items <- grep("^(PH0[23]|PE0[23]|SA02|MV01|CN02|SD02|RW02|CV02|SB02|MC01)_", names(data_scored), value = TRUE)
missing_code_counts <- vapply(data_scored[questionnaire_items], function(x) sum(x == -9, na.rm = TRUE), integer(1))
for (name in questionnaire_items) {
  data_scored[[name]] <- as.numeric(data_scored[[name]])
  data_scored[[name]][data_scored[[name]] == -9] <- NA_real_
}

# Item means with the source's 80% response rule; weights are shown at each domain.
score_mean <- function(items, min_response = 0.8, weights = rep(1, length(items)))
  extraction_item_mean(data_scored[, items, drop = FALSE], min_response, weights)

# BESSI-96: three positive-keyed items per facet (Supplement 2.2, SEB Skills).
data_scored$timemanagement <- score_mean(c("PE02_03", "PE02_35", "PE03_19"))
data_scored$organizationalskill <- score_mean(c("PE02_06", "PE02_38", "PE03_22"))
data_scored$capacityforconsistency <- score_mean(c("PE02_09", "PE02_41", "PE03_25"))
data_scored$taskmanagement <- score_mean(c("PE02_12", "PE02_44", "PE03_28"))
data_scored$detailmanagement <- score_mean(c("PE02_15", "PE02_47", "PE03_31"))
data_scored$rulefollowingskill <- score_mean(c("PE02_18", "PE03_02", "PE03_34"))
data_scored$responsibilitymanagement <- score_mean(c("PE02_21", "PE03_05", "PE03_37"))
data_scored$goalregulation <- score_mean(c("PE02_24", "PE03_08", "PE03_40"))
data_scored$decisionmakingskill <- score_mean(c("PE02_27", "PE03_11", "PE03_43"))
data_scored$energyregulation <- score_mean(c("PE02_07", "PE02_39", "PE03_23"))
data_scored$ethicalcompetence <- score_mean(c("PE02_29", "PE03_13", "PE03_45"))
data_scored$impulseregulation <- score_mean(c("PE02_30", "PE03_14", "PE03_46"))
data_scored$informationprocessingskill <- score_mean(c("PE02_22", "PE03_06", "PE03_38"))
data_scored$leadershipskill <- score_mean(c("PE02_01", "PE02_33", "PE03_17"))
data_scored$persuasiveskill <- score_mean(c("PE02_13", "PE02_45", "PE03_29"))
data_scored$expressiveskill <- score_mean(c("PE02_17", "PE03_01", "PE03_33"))
data_scored$conversationalskill <- score_mean(c("PE02_25", "PE03_09", "PE03_41"))
# User-confirmed correction: never reuse the persuasion object for perspective.
data_scored$perspectivetakingskill <- score_mean(c("PE02_02", "PE02_34", "PE03_18"))
data_scored$capacityfortrust <- score_mean(c("PE02_08", "PE02_40", "PE03_24"))
data_scored$capacityforsocialwarmth <- score_mean(c("PE02_14", "PE02_46", "PE03_30"))
data_scored$teamworkskill <- score_mean(c("PE02_23", "PE03_07", "PE03_39"))
data_scored$stressregulation <- score_mean(c("PE02_05", "PE02_37", "PE03_21"))
data_scored$capacityforoptimism <- score_mean(c("PE02_11", "PE02_43", "PE03_27"))
data_scored$angermanagement <- score_mean(c("PE02_20", "PE03_04", "PE03_36"))
data_scored$confidenceregulation <- score_mean(c("PE02_26", "PE03_10", "PE03_42"))
data_scored$abstractthinkingskill <- score_mean(c("PE02_04", "PE02_36", "PE03_20"))
data_scored$creativeskill <- score_mean(c("PE02_16", "PE02_48", "PE03_32"))
data_scored$artisticskill <- score_mean(c("PE02_28", "PE03_12", "PE03_44"))
data_scored$culturalcompetence <- score_mean(c("PE02_32", "PE03_16", "PE03_48"))
data_scored$selfreflectionskill <- score_mean(c("PE02_10", "PE02_42", "PE03_26"))
data_scored$adaptability <- score_mean(c("PE02_19", "PE03_03", "PE03_35"))
data_scored$capacityforindependence <- score_mean(c("PE02_31", "PE03_15", "PE03_47"))

# Domain weights: core facets 1, cross-loading facets 0.5. This is equivalent to
# the historical 2:1 weighting, with the denominator normalised to preserve 1-5.
data_scored$selfmanagement <- score_mean(c("timemanagement", "organizationalskill", "capacityforconsistency",
  "taskmanagement", "detailmanagement", "rulefollowingskill", "responsibilitymanagement", "goalregulation",
  "decisionmakingskill", "energyregulation", "ethicalcompetence", "impulseregulation", "informationprocessingskill"),
  weights = c(rep(1, 9), rep(0.5, 4)))
data_scored$socialengagement <- score_mean(c("leadershipskill", "persuasiveskill", "expressiveskill",
  "conversationalskill", "energyregulation"), weights = c(1, 1, 1, 1, 0.5))
data_scored$cooperation <- score_mean(c("perspectivetakingskill", "capacityfortrust", "capacityforsocialwarmth",
  "teamworkskill", "ethicalcompetence"), weights = c(1, 1, 1, 1, 0.5))
data_scored$emotionalresilience <- score_mean(c("stressregulation", "capacityforoptimism", "angermanagement",
  "confidenceregulation", "impulseregulation"), weights = c(1, 1, 1, 1, 0.5))
data_scored$innovation <- score_mean(c("abstractthinkingskill", "creativeskill", "artisticskill",
  "culturalcompetence", "informationprocessingskill"), weights = c(1, 1, 1, 1, 0.5))

# HEXACO: reverse keys and 24 facets, followed by the six reported domain means.
# Each four-item facet needs 80% (all four); each domain needs all four facets.
for (name in hexaco_reversed) data_scored[[paste0(name, "_inv")]] <- 6 - data_scored[[name]]
for (facet in names(hexaco_facets)) data_scored[[paste0("hx_", facet)]] <- score_mean(hexaco_facets[[facet]])
data_scored$hexaco_honestyhumility <- score_mean(paste0("hx_", c("sincerity", "fairness", "greedavoidance", "modesty")))
data_scored$hexaco_emotionality <- score_mean(paste0("hx_", c("fearfulness", "anxiety", "dependence", "sentimentality")))
data_scored$hexaco_extraversion <- score_mean(paste0("hx_", c("socialselfesteem", "socialboldness", "sociability", "liveliness")))
data_scored$hexaco_agreeableness <- score_mean(paste0("hx_", c("forgiveness", "gentleness", "flexibility", "patience")))
data_scored$hexaco_conscientiousness <- score_mean(paste0("hx_", c("organisation", "diligence", "perfectionism", "prudence")))
data_scored$hexaco_openness <- score_mean(paste0("hx_", c("aestheticappreciation", "inquisitiveness", "creativity", "unconventionality")))

# Political measures. Keep distinct constructs and reported observed subscales;
# do not replace the authors' latent political factor with an invented average.
for (name in paste0("SD02_", sprintf("%02d", c(5:8, 13:16)))) data_scored[[paste0(name, "_inv")]] <- 8 - data_scored[[name]]
data_scored$socialdominanceorientation <- score_mean(c(paste0("SD02_", sprintf("%02d", c(1:4, 9:12))),
  paste0("SD02_", sprintf("%02d", c(5:8, 13:16)), "_inv")))
rwa_positive <- paste0("RW02_", sprintf("%02d", c(3,5,7,10,12,14,16,17,19,22)))
rwa_reversed <- paste0("RW02_", sprintf("%02d", c(4,6,8,9,11,13,15,18,20,21)))
for (name in rwa_reversed) data_scored[[paste0(name, "_inv")]] <- 10 - data_scored[[name]]
data_scored$rightwingauthoritarianism <- score_mean(c(rwa_positive, paste0(rwa_reversed, "_inv")))
data_scored$CV02_01_inv <- 100 - data_scored$CV02_01
data_scored$CV02_05_inv <- 100 - data_scored$CV02_05
data_scored$conservatism <- score_mean(c("CV02_01_inv", "CV02_02", "CV02_03", "CV02_04", "CV02_05_inv", "CV02_06", "CV02_07", "CV02_08", "CV02_09", "CV02_10", "CV02_11", "CV02_12"))
data_scored$economicconservatism <- score_mean(c("CV02_02", "CV02_05_inv", "CV02_06", "CV02_09", "CV02_10"))
data_scored$socialconservatism <- score_mean(c("CV02_01_inv", "CV02_03", "CV02_04", "CV02_07", "CV02_08", "CV02_11", "CV02_12"))
political_key <- c("Very Liberal" = 1, "Liberal" = 2, "Moderate" = 3, "Conservative" = 4, "Very Conservative" = 5)
data_scored$politicalorientation <- unname(political_key[as.character(data_scored$PO03)])

# Fluid reasoning: published answer key, nine subtest means, then their mean.
# Retain the source study-part filters and no-missing rule for subtest/total means.
valid_reasoning <- with(data_analytic, filter_time == 0 & filter_break == 0 & filter_repeat_inst == 0)
valid_reasoning[is.na(valid_reasoning)] <- FALSE
for (name in names(reasoning_key)) {
  response <- as.character(data_analytic[[name]])
  response[response == "[NA] nicht beantwortet"] <- NA_character_
  correct <- as.numeric(response == reasoning_key[[name]])
  if (startsWith(name, "IN")) correct[is.na(correct)] <- 0 # Missing numeric response is incorrect in source key.
  correct[!valid_reasoning] <- NA_real_
  data_scored[[paste0(name, "_score")]] <- correct
}
data_scored$IN14_score <- as.numeric(rowSums(data_scored[, c("IN14_01_score", "IN14_02_score")]) == 2)
for (name in names(reasoning_subtests)) data_scored[[paste0("reasoning_", name)]] <-
  score_mean(paste0(reasoning_subtests[[name]], "_score"), min_response = 1)
data_scored$intelligencefluid <- score_mean(paste0("reasoning_", names(reasoning_subtests)), min_response = 1)

# Complex problem solving: observed knowledge and performance components remain
# distinct. No factor-score composite is substituted by a new observed average.
for (name in names(tailor_key)) {
  items <- grep(paste0("^", name, "_[0-9]{2}$"), names(data_scored), value = TRUE)
  x <- as.matrix(data_scored[, items, drop = FALSE])
  stopifnot(ncol(x) == length(tailor_key[[name]]))
  data_scored[[paste0(name, "_score")]] <- rowMeans(sweep(x, 2, tailor_key[[name]], "=="), na.rm = FALSE)
}
data_scored$complexproblemsolvingknowledge <- score_mean(paste0(names(tailor_key), "_score"), min_response = 1)
company_value <- vapply(as.character(data_analytic$TS01_02), function(log) {
  if (is.na(log)) return(NA_real_)
  value <- regmatches(log, regexec("Turn:12.*?CompanyValue:(-?[0-9]+)", log, perl = TRUE))[[1]]
  if (length(value) != 2L) NA_real_ else as.numeric(value[[2]])
}, numeric(1), USE.NAMES = FALSE)
company_value[company_value < -300000] <- NA_real_ # Published performance trim.
data_scored$complexproblemsolvingperformance <- company_value

# Environment: questionnaire means and the authors' explicitly weighted behavior
# index. Other questionnaire metrics remain unchanged; only SEB/BESSI uses 1-5.
data_scored$proenvironmentalattitudes <- score_mean(paste0("SA02_", sprintf("%02d", 1:7)))
data_scored$environmentalmotivation <- score_mean(c("MV01_04", "MV01_05", "MV01_06"))
for (name in c("CN02_04", "CN02_12", "CN02_14")) data_scored[[paste0(name, "_inv")]] <- 6 - data_scored[[name]]
data_scored$natureconnectedness <- score_mean(c("CN02_01", "CN02_02", "CN02_03", "CN02_04_inv", "CN02_05", "CN02_06", "CN02_07", "CN02_08", "CN02_09", "CN02_10", "CN02_11", "CN02_12_inv", "CN02_13", "CN02_14_inv"))
data_scored$proenvironmentalbehaviorgoods <- score_mean(paste0("SB02_", sprintf("%02d", 1:12)), min_response = 0)
data_scored$proenvironmentalbehaviortransport <- score_mean(paste0("SB02_", sprintf("%02d", 13:17)), min_response = 0)
data_scored$proenvironmentalbehaviorengagement <- score_mean(paste0("SB02_", sprintf("%02d", 18:28)), min_response = 0)
food_behaviors <- score_mean(c("SB02_29", "SB02_30"), min_response = 0)
animal_items <- paste0("MC01_", sprintf("%02d", 1:6))
for (name in animal_items[1:5]) data_scored[[paste0(name, "_environment")]] <- 8 - data_scored[[name]]
data_scored$MC01_06_environment <- data_scored$MC01_06
animal_consumption <- score_mean(paste0(animal_items, "_environment"), min_response = 0)
data_scored$proenvironmentalbehaviorfood <- rowMeans(cbind((food_behaviors - 1) / 4, (animal_consumption - 1) / 6), na.rm = TRUE) * 4 + 1
data_scored$proenvironmentalbehaviorfood[is.nan(data_scored$proenvironmentalbehaviorfood)] <- NA_real_
data_scored$proenvironmentalbehavior <- score_mean(c("proenvironmentalbehaviorgoods", "proenvironmentalbehaviortransport", "proenvironmentalbehaviorengagement", "proenvironmentalbehaviorfood"), min_response = 0)

# Demographics: preserve gender categories as missing for the historical binary
# contrast; never drop these participants from other correlations or exports.
data_scored$sex <- unname(c("Male"=0, "Female"=1)[as.character(data_analytic$DG04)])
age_text <- as.character(data_analytic$DG02)
age_text[grepl("[NA]", age_text, fixed = TRUE)] <- NA_character_
age_text[!is.na(age_text) & age_text == "More than 99 years"] <- "100"
data_scored$age <- as.numeric(sub(" years$", "", age_text))

# 5. Canonical variable names ----------------------------------------------
# All scientific output names were assigned explicitly above. This selection is
# the 32 BESSI facets + five domains, six HEXACO domains, observed cognitive,
# political and environmental measures, plus sex. Latent factor scores, task
# subtest intermediates and auxiliary survey demographics are not new constructs.
variable_map <- codebook_resolve_variables(matrix_variables)
variable_map$source <- "OSF an64w; Supplement_S2.2.qmd; explicit scoring in scripts/extraction/articles/0145.R"

# 6. Scientific checks ------------------------------------------------------
stopifnot(length(bessi_variables) == 37L, nrow(data_scored) == expected_n)
stopifnot(all(vapply(data_scored[bessi_variables], function(x) all(is.na(x) | (x >= 1 & x <= 5)), logical(1))))
stopifnot(all(is.na(data_scored$sex) | data_scored$sex %in% c(0,1)))
stopifnot(abs(mean(data_scored$age, na.rm = TRUE) - 26.05) < 0.01,
  round(100 * sum(data_scored$sex == 1, na.rm = TRUE) / expected_n, 1) == 51.3)

# 7. Correlations -----------------------------------------------------------
data_matrix <- data_scored[, matrix_variables, drop = FALSE]
cor_matrix <- cor(data_matrix, method = "pearson", use = "pairwise.complete.obs")
pair_n <- crossprod(!is.na(as.matrix(data_matrix)))
checks <- extraction_check_matrix(data_matrix, cor_matrix, pair_n)
checks$hexaco_missing_block <- sum(rowSums(!is.na(data_scored[hexaco_items])) == 0)
checks$raw_day1_links <- unname(source_links["day1"])
checks$raw_day2_links <- unname(source_links["day2"])
checks$raw_tailor_links <- unname(source_links["tailor"])
checks$missing_response_codes_replaced <- sum(missing_code_counts)

# 8. Export ----------------------------------------------------------------
# Staging does not alter metadata/review_data or register an unmatched active
# matrix. Integration will promote the validated matrix with its coding row.
individual_data <- data_scored[, c(matrix_variables, "age"), drop = FALSE]
decisions <- codebook_csv("config/extraction/0145_decisions.csv")
if (!"--check-only" %in% arguments) extraction_export(paper_id, matrix_id, individual_data,
  cor_matrix, pair_n, variable_map, checks, decisions, audit_dir = audit_dir)
cat("0145a:", nrow(individual_data), "participants;", ncol(data_matrix), "matrix variables; pair N",
  checks$minimum_pair_n, "to", checks$maximum_pair_n, ". Age retained only in individual data.\n")
