# 0120: read-only historical verification. No scientific output is promoted.
# Run: Rscript scripts/extraction/articles/0120.R [--check-only]
# A normal run updates only extraction/historical_audit.csv.
source("R/historical_audit.R")
args <- commandArgs(trailingOnly = TRUE)
stopifnot(all(args %in% "--check-only"))
source_hashes <- c(
  "data/3.meta_data/open_data/d0120a.sav" = "6eaace8fbf7aaa8ca4ebebf8e7d966f0af058abbc195ad4a63538dded559775f",
  "data/3.meta_data/open_data/d0120b.sav" = "2f14b0f31717987725a2eed4567a6175df2d083acc91654031529c57ccc5baa2",
  "data/3.meta_data/open_data/d0120a_informant.sav" = "4d71807222cb6b8c52b05c8889bb64078d3ff69bc4c1a6efd8c55519afd50446",
  "data/2.full_abstracting/pdfs/0120.pdf" = "9ca2a8b5cddc2eee0fc1819be81184b8616097c7bb629daca4bd6c2f0a3cffd1",
  "data/3.meta_data/open_data/raw/shared/bessi_scoring_v1.pdf" = "9d842f5196f021131be8b6561cc70b17dd0deebdaca2cbdc2f3579dce8e0b2a9")
audit_verify_sources(source_hashes)

source("scripts/extraction/keys/bessi192.R")
# 1-3. Study 1 self-report N=1103 (PDF p.7, Table 5); Study 2 N=123
# (pp.19-20). Study 2 has a two-week retest; only first-occasion columns
# enter the existing matrices. Do not create a new overlapping informant sample.
raw_a <- as.data.frame(haven::read_sav(names(source_hashes)[1]))
raw_b <- as.data.frame(haven::read_sav(names(source_hashes)[2]))
raw_informant <- haven::read_sav(names(source_hashes)[3])
stopifnot(nrow(raw_a)==1103,nrow(raw_b)==123,nrow(raw_informant)==184,
  !anyDuplicated(raw_a$Case),!anyDuplicated(raw_b$Participant))

# 4-5. Source-supplied composites and covariates: explicit historical mapping.
# Non-BESSI item keys are not reconstructed in this first pass.
map_a <- c(
  "Self_Management_Skills" = "selfmanagement",
  "Cooperation_Skills" = "cooperation",
  "Social_Engagement_Skills" = "socialengagement",
  "Emotional_Resilience_Skills" = "emotionalresilience",
  "Innovation_Skills" = "innovation",
  "Time_Management" = "timemanagement",
  "Organizational_Skill" = "organizationalskill",
  "Capacity_for_Consistency" = "capacityforconsistency",
  "Task_Management" = "taskmanagement",
  "Detail_Management" = "detailmanagement",
  "Rule_Following_Skill" = "rulefollowingskill",
  "Responsibility_Management" = "responsibilitymanagement",
  "Goal_Regulation" = "goalregulation",
  "Decision_Making_Skill" = "decisionmakingskill",
  "Leadership_Skill" = "leadershipskill",
  "Persuasive_Skill" = "persuasiveskill",
  "Expressive_Skill" = "expressiveskill",
  "Conversational_Skill" = "conversationalskill",
  "Energy_Regulation" = "energyregulation",
  "Perspective_Taking_Skill" = "perspectivetakingskill",
  "Capacity_for_Trust" = "capacityfortrust",
  "Capacity_for_Social_Warmth" = "capacityforsocialwarmth",
  "Teamwork_Skill" = "teamworkskill",
  "Ethical_Competence" = "ethicalcompetence",
  "Stress_Regulation" = "stressregulation",
  "Capacity_for_Optimism" = "capacityforoptimism",
  "Anger_Management" = "angermanagement",
  "Confidence_Regulation" = "confidenceregulation",
  "Impulse_Regulation" = "impulseregulation",
  "Abstract_Thinking_Skill" = "abstractthinkingskill",
  "Creative_Skill" = "creativeskill",
  "Artistic_Skill" = "artisticskill",
  "Cultural_Competence" = "culturalcompetence",
  "Information_Processing_Skill" = "informationprocessingskill",
  "Self_Reflection_Skill" = "selfreflectionskill",
  "Adaptability" = "adaptability",
  "Independence" = "capacityforindependence")
map_b <- c(
  "SMS" = "selfmanagement",
  "COOP" = "cooperation",
  "SES" = "socialengagement",
  "ERS" = "emotionalresilience",
  "INO" = "innovation",
  "TM" = "timemanagement",
  "OS" = "organizationalskill",
  "CC" = "capacityforconsistency",
  "TaM" = "taskmanagement",
  "DM" = "detailmanagement",
  "RFS" = "rulefollowingskill",
  "RM" = "responsibilitymanagement",
  "GR" = "goalregulation",
  "DMS" = "decisionmakingskill",
  "LEA" = "leadershipskill",
  "PER" = "persuasiveskill",
  "EXPR" = "expressiveskill",
  "CON" = "conversationalskill",
  "ER" = "energyregulation",
  "PTS" = "perspectivetakingskill",
  "CFT" = "capacityfortrust",
  "CSW" = "capacityforsocialwarmth",
  "TWS" = "teamworkskill",
  "EC" = "ethicalcompetence",
  "SR" = "stressregulation",
  "CFO" = "capacityforoptimism",
  "AM" = "angermanagement",
  "CR" = "confidenceregulation",
  "IR" = "impulseregulation",
  "ATS" = "abstractthinkingskill",
  "CREA" = "creativeskill",
  "ART" = "artisticskill",
  "CUL" = "culturalcompetence",
  "IPS" = "informationprocessingskill",
  "SRS" = "selfreflectionskill",
  "ADA" = "adaptability",
  "CFI" = "capacityforindependence",
  "BFI2_O" = "openness",
  "BFI2_C" = "conscientiousness",
  "BFI2_E" = "extraversion",
  "BFI2_A" = "agreeableness",
  "BFI2_N" = "neuroticism",
  "BFI2_E_Sociability" = "bf_sociability",
  "BFI2_E_Assertiveness" = "bf_assertiveness",
  "BFI2_E_EnergyLevel" = "bf_energy",
  "BFI2_A_Compassion" = "bf_compassion",
  "BFI2_A_Respectfulness" = "bf_respectfulness",
  "BFI2_A_Trust" = "bf_trust",
  "BFI2_N_Anxiety" = "bf_anxiety",
  "BFI2_N_Depression" = "bf_depression",
  "BFI2_N_EmotionalVolatility" = "bf_emotionalvolatility",
  "BFI2_C_Organization" = "bf_organization",
  "BFI2_C_Productiveness" = "bf_productiveness",
  "BFI2_C_Responsibility" = "bf_responsibility",
  "BFI2_O_IntellectualCuriosity" = "bf_curiosity",
  "BFI2_O_AestheticSensitivity" = "bf_aesthetic",
  "BFI2_O_CreativeImagination" = "bf_creativity",
  "SWLS" = "satisfactionwithlife",
  "EAR" = "selfesteem")
raw_samples <- list(a=raw_a,b=raw_b)
source_maps <- list(a=map_a,b=map_b)
audit_0120 <- list()
score_checks <- list()
for (sample in c("a","b")) {
  raw <- raw_samples[[sample]]; variable_map <- source_maps[[sample]]
  data_scored <- raw
  names(data_scored)[match(names(variable_map),names(data_scored))] <- unname(variable_map)
  gender <- as.character(haven::zap_labels(raw$Gender))
  data_scored$sex <- ifelse(gender=="1",0,ifelse(gender=="2",1,NA_real_))
  # Retain age separately; the historical preselection discarded it before export.
  data_scored$age <- as.numeric(raw$Age)
  data_matrix <- data_scored[,c(unname(variable_map),"sex"),drop=FALSE]
  stopifnot(!any(grepl("_t2$",names(data_matrix))))

  # 6. Rebuild all 32 BESSI facets from original first-wave items. PDF p.7
  # specifies at least three of six responses; supplied items are all observed.
  rebuilt_facets <- list()
  for (facet in names(bessi192_first_item)) {
    indices <- seq(bessi192_first_item[[facet]],192,32)
    columns <- if(sample=="a")sprintf("BESSI%03d",indices) else paste0("BESSI",indices)
    items <- as.matrix(raw[,columns,drop=FALSE])
    # Known source anomaly: one Study 2 BESSI171 response is 32 on a 1-5
    # instrument. Retain it ONLY to reproduce the source scores; do not infer
    # whether the intended response was 2, 3 or another value.
    outside <- !is.na(items) & (items<1 | items>5)
    if (any(outside)) stopifnot(sample=="b",facet=="capacityforoptimism",
      sum(outside)==1L,all(items[outside]==32),sum(raw$BESSI171==32)==1L)
    value <- rowMeans(items,na.rm=TRUE)
    value[rowSums(!is.na(items))<3] <- NA_real_
    rebuilt_facets[[facet]] <- value
    stopifnot(isTRUE(all.equal(value,as.numeric(data_scored[[facet]]),tolerance=1e-12,check.attributes=FALSE)))
  }
  rebuilt_facets <- as.data.frame(rebuilt_facets)
  # Source domains equal UNWEIGHTED means, not the official half-weight key.
  # Verify this exact fact rather than silently replacing the author scores.
  for (domain in names(bessi192_domain_weights)) {
    columns <- names(bessi192_domain_weights[[domain]])
    source_value <- rowMeans(rebuilt_facets[,columns,drop=FALSE],na.rm=TRUE)
    stopifnot(isTRUE(all.equal(source_value,as.numeric(data_scored[[domain]]),tolerance=1e-12,check.attributes=FALSE)))
  }
  matrix_id <- paste0("0120",sample); reference_n <- if(sample=="a")1103 else 123
  common_note <- "All 32 BESSI facets reproduced from original items. Source domains use equal facet weights. Other supplied composite scoring not independently reconstructed. First occasion only; informant file not added as a separate sample."
  if(sample=="b") common_note <- paste(common_note,
    "One BESSI171 response is 32 outside the 1-5 range and is included in the supplied optimism score. Its correction requires review.")
  audit_0120[[paste0(sample,"_source")]] <- audit_compare_matrix("0120",matrix_id,
    data_matrix,reference_n,note=common_note,sources=names(source_hashes),
    evidence="0120 PDF p.7 and pp.19-22; official BESSI v1.0 key pp.1-2",
    status=if(sample=="a")"SOURCE_DOMAIN_WEIGHTING_REVIEW" else "SOURCE_ITEM_AND_WEIGHTING_REVIEW")

  # 7. Sensitivity candidate with the official half weights; keep it in memory.
  official_matrix_data <- data_matrix
  for (domain in names(bessi192_domain_weights)) {
    weights <- bessi192_domain_weights[[domain]]
    facets <- as.matrix(rebuilt_facets[,names(weights),drop=FALSE])
    official_matrix_data[[domain]] <- rowSums(sweep(facets,2,weights,"*"),na.rm=TRUE) /
      rowSums(sweep(!is.na(facets),2,weights,"*"))
  }
  audit_0120[[paste0(sample,"_official")]] <- audit_compare_matrix("0120",matrix_id,
    official_matrix_data,reference_n,note=paste(common_note,
      "Sensitivity analysis only: five domain scores replaced with official half-weighted composites. Requires scientific review, not promoted."),
    sources=names(source_hashes),evidence="Official BESSI v1.0 scoring key p.2 versus supplied composite values",
    status="UNAPPROVED_WEIGHTING_SENSITIVITY",comparison="official_weight_sensitivity")
  if(sample=="b") {
    # Separate sensitivity: treat only the uninterpretable value as missing,
    # retain the authors' equal facet weights, and leave every other score intact.
    # This is a candidate for discussion, not an inferred correction.
    corrected <- data_matrix
    columns <- paste0("BESSI",seq(11,192,32))
    items <- as.matrix(raw[,columns,drop=FALSE])
    items[items<1 | items>5] <- NA_real_
    corrected$capacityforoptimism <- rowMeans(items,na.rm=TRUE)
    for(domain in names(bessi192_domain_weights)) {
      corrected[[domain]] <- rowMeans(corrected[,names(bessi192_domain_weights[[domain]]),drop=FALSE])
    }
    audit_0120[["b_invalid_as_missing"]] <- audit_compare_matrix("0120",matrix_id,
      corrected,reference_n,note=paste(common_note,
        "Sensitivity only: invalid response treated as NA; optimism averaged over the other five items; domain weights remain source-equal."),
      sources=names(source_hashes),evidence="0120 Study 2 raw BESSI171; instrument response range 1-5",
      status="UNAPPROVED_ITEM_SENSITIVITY",comparison="invalid_item_as_missing_sensitivity")
  }
}
# 8. Only aggregate evidence is saved. No candidate matrices or individuals promoted.
summary_0120 <- dplyr::bind_rows(lapply(audit_0120, `[[`, "summary"))
# Independent publication comparison: Table 8, PDF p.22, seven correlates.
# Transcribed from the rendered table, not the stored metadata or author scores.
published_table8 <- rbind(
  innovation=c(.78,.19,.31,.18,-.25,.18,.19),
  selfmanagement=c(.21,.85,.37,.24,-.51,.51,.52),
  socialengagement=c(.36,.42,.86,.33,-.49,.46,.49),
  cooperation=c(.23,.37,.50,.78,-.51,.42,.42),
  emotionalresilience=c(.18,.46,.33,.37,-.89,.54,.61))
colnames(published_table8) <- c("openness","conscientiousness","extraversion",
  "agreeableness","neuroticism","satisfactionwithlife","selfesteem")
summary_0120$published_pairs_compared <- NA_integer_
summary_0120$published_pairs_outside_rounding <- NA_integer_
summary_0120$published_max_abs_delta <- NA_real_
for (key in c("b_source","b_official","b_invalid_as_missing")) {
  entry <- audit_0120[[key]]
  compared <- entry$candidate[rownames(published_table8),colnames(published_table8)]
  difference <- abs(compared-published_table8)
  row <- summary_0120$matrix_id=="0120b" & summary_0120$comparison==entry$summary$comparison
  summary_0120$published_pairs_compared[row] <- length(difference)
  summary_0120$published_pairs_outside_rounding[row] <- sum(difference > .005 + 1e-12)
  summary_0120$published_max_abs_delta[row] <- max(difference)
  summary_0120$note[row] <- paste(summary_0120$note[row],
    "Also compared 35 coefficients with published Table 8 at two-decimal rounding tolerance; confirm released data/analysis version before promotion.")
}
print(summary_0120[,c("matrix_id","comparison","changed_pairs","max_abs_delta","pair_n_min","pair_n_max")])
if (!"--check-only" %in% args) audit_save_summary(summary_0120)
