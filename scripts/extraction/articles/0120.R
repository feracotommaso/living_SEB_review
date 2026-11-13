# 0120: approved BESSI correction and historical/publication comparisons.
# Run: Rscript scripts/extraction/articles/0120.R [--check-only]
# A normal run exports the approved matrices, private coded individuals, pair
# Ns and aggregate checks. --check-only never writes. Final analytical outputs
# are updated separately through workflow.R after reviewing the changes.
source("R/historical_audit.R")
args <- commandArgs(trailingOnly = TRUE)
stopifnot(all(args %in% "--check-only"))
source_hashes <- c(
  "data/3.meta_data/open_data/d0120a.sav" = "6eaace8fbf7aaa8ca4ebebf8e7d966f0af058abbc195ad4a63538dded559775f",
  "data/3.meta_data/open_data/d0120b.sav" = "2f14b0f31717987725a2eed4567a6175df2d083acc91654031529c57ccc5baa2",
  "data/3.meta_data/open_data/d0120a_informant.sav" = "4d71807222cb6b8c52b05c8889bb64078d3ff69bc4c1a6efd8c55519afd50446",
  "data/2.full_abstracting/pdfs/0120.pdf" = "9ca2a8b5cddc2eee0fc1819be81184b8616097c7bb629daca4bd6c2f0a3cffd1",
  "data/3.meta_data/open_data/raw/shared/bessi_scoring_v1.pdf" = "9d842f5196f021131be8b6561cc70b17dd0deebdaca2cbdc2f3579dce8e0b2a9",
  "data/3.meta_data/open_data/raw/0120/osf_7efmp/7efmp-osfstorage-archive.zip" = "16fae8f002d65a5056a38a6f0036cb43ad5d7aff4693ef0d209c76501eebc42b")
audit_verify_sources(source_hashes)

# Full OSF archive supplied by the user on 2026-11-13; original download date
# unknown. Originals and extracted files are local and Git-ignored. Confirm that
# the three shared SAV files are byte-identical to the historical inputs.
archive_files <- "data/3.meta_data/open_data/raw/0120/osf_7efmp/files"
archive_data <- file.path(archive_files,c("Study 1 Self Report (N = 1103).sav",
  "Study 2 (N = 123).sav","Study 1 Informant report (N= 184).sav"))
audit_verify_sources(stats::setNames(unname(source_hashes[1:3]),archive_data))
# Static review only, without executing author code: the two 'R Syntax' DOCX
# files define two-item parcels and CFA models. The hierarchical model includes
# the three compound facets in all five domains; this is a latent model, not an
# observed-score averaging rule. Neither document supplies the Table 8 analysis
# or explains the equal-weight composites in the SAV files. The separate
# 'Cohen\'s D - Data Analysis.R' uses hard-coded descriptive statistics, not raw
# item scoring or correlations. No alternative data version or item correction
# was found in the eight-file archive.

source("scripts/extraction/keys/bessi192.R",local=TRUE)
source("scripts/extraction/keys/0120.R",local=TRUE)
# 1-3. Study 1 self-report N=1103 (PDF p.7, Table 5); Study 2 N=123
# (pp.19-20). Study 2 has a two-week retest; only first-occasion columns
# enter the existing matrices. Do not create a new overlapping informant sample.
raw_a <- as.data.frame(haven::read_sav(names(source_hashes)[1]))
raw_b <- as.data.frame(haven::read_sav(names(source_hashes)[2]))
raw_informant <- haven::read_sav(names(source_hashes)[3])
stopifnot(nrow(raw_a)==1103,nrow(raw_b)==123,nrow(raw_informant)==184,
  !anyDuplicated(raw_a$Case),!anyDuplicated(raw_b$Participant))

# 4-5. Source-supplied composites and covariates: explicit historical mapping.
# BFI-2 is independently reconstructed below; SWLS/RSES are still source-supplied.
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
  common_note <- "All 32 BESSI facets reproduced from original items. Source domains use equal facet weights. SWLS/RSES scoring not independently reconstructed. First occasion only; informant file not added as a separate sample."
  common_note <- paste(common_note,
    "User-supplied OSF 7efmp archive received 2026-11-13: all three SAV files byte-identical to historical inputs. Shared code covers parcel/CFA models and descriptive Cohen's d, not Table 8 correlations or observed-domain scoring; discrepancies remain unresolved.")
  if(sample=="b") common_note <- paste(common_note,
    "One BESSI171 response is 32 outside the 1-5 range and is included in the supplied optimism score. The approved extraction below treats it as missing.",
    "User decision 2026-11-13: defer the publication discrepancy. TODO: contact the authors to report that correlations recalculated from the shared data do not reproduce published Tables 8 and 9, and request the exact analysis code/data version. No contact sent or resolution claimed. User approved BESSI171=32 as missing, official domain weights, and final integration. BFI-2 replacement remains unapproved.")
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
      "Isolated weighting comparison: five domain scores replaced with official half-weighted composites. The combined correction is the extraction used for export."),
    sources=names(source_hashes),evidence="Official BESSI v1.0 scoring key p.2 versus supplied composite values",
    status="ISOLATED_WEIGHTING_COMPARISON",comparison="official_weight_sensitivity")
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
        "Isolated item comparison: invalid response treated as NA; optimism averaged over the other five items; domain weights remain source-equal. The combined correction is the extraction used for export."),
      sources=names(source_hashes),evidence="0120 Study 2 raw BESSI171; instrument response range 1-5",
      status="ISOLATED_ITEM_COMPARISON",comparison="invalid_item_as_missing_sensitivity")
  }
}

# User-approved source corrections (2026-11-13), combined explicitly:
# 0120a: official domain weights. 0120b: BESSI171=32 becomes missing; optimism
# is the mean of the other five responses, then domains use official weights.
# Other supplied scores, including BFI-2/SWLS/RSES, stay unchanged.
for(sample in c("a","b")) {
  entry <- audit_0120[[paste0(sample,"_source")]]
  approved_data <- entry$data_matrix
  if(sample=="b") approved_data$capacityforoptimism <-
    audit_0120$b_invalid_as_missing$data_matrix$capacityforoptimism
  for(domain in names(bessi192_domain_weights)) {
    weights <- bessi192_domain_weights[[domain]]
    facets <- as.matrix(approved_data[,names(weights),drop=FALSE])
    approved_data[[domain]] <- rowSums(sweep(facets,2,weights,"*"),na.rm=TRUE) /
      rowSums(sweep(!is.na(facets),2,weights,"*"))
  }
  stopifnot(all(as.matrix(approved_data[,unname(map_a),drop=FALSE])>=1 &
    as.matrix(approved_data[,unname(map_a),drop=FALSE])<=5))
  audit_0120[[paste0(sample,"_approved")]] <- audit_compare_matrix("0120",
    paste0("0120",sample),approved_data,entry$summary$reference_n,
    note=paste(entry$summary$note,
      "Combined user-approved extraction: official BESSI domain weights; invalid Study 2 BESSI171 response treated as missing and optimism averaged over five observed items. Original sources preserved. Other instruments unchanged. Exported by a normal run; --check-only does not write."),
    sources=names(source_hashes),
    evidence="User decisions 2026-11-13; official BESSI v1.0 key pp.1-2; Study 2 BESSI171 outside 1-5",
    status="APPROVED_EXTRACTION",comparison="approved_source_correction")
}

# 6-7. Independent BFI-2 scoring for Study 2: official 60-item key, 15 facets
# and five domains. All released values are observed and within 1-5, including
# 16 fractional cells compatible with prior imputation. Preserve them here;
# we do not have the pre-imputation file or invent which responses were missing.
bfi_items <- as.matrix(raw_b[,paste0("BFI2_",1:60)])
stopifnot(!anyNA(bfi_items),all(bfi_items>=1 & bfi_items<=5))
bfi_rebuilt <- data.frame(row.names=seq_len(nrow(raw_b)))
for(facet in names(bfi2_facet_items)) {
  key <- bfi2_facet_items[[facet]]
  items <- bfi_items[,abs(key),drop=FALSE]
  items[,key<0] <- 6-items[,key<0]
  bfi_rebuilt[[facet]] <- rowMeans(items)
}
for(domain in names(bfi2_domain_facets)) {
  bfi_rebuilt[[domain]] <- rowMeans(bfi_rebuilt[,bfi2_domain_facets[[domain]],drop=FALSE])
}
supplied <- audit_0120$b_source$data_matrix
bfi_score_comparison <- dplyr::bind_rows(lapply(names(bfi_rebuilt),function(scale) {
  delta <- abs(bfi_rebuilt[[scale]]-supplied[[scale]])
  data.frame(scale,changed_values=sum(delta>1e-10),max_abs_delta=max(delta))
}))
# Nine facet-score discrepancies across six facets. In each affected cell,
# the supplied score equals the mean of the three non-fractional items.
# This is compatible with scoring before item imputation; chronology is not
# documented. Check this numerical explanation without treating every
# fractional response as missing or changing the supplied scores.
for(facet in names(bfi2_facet_items)) {
  changed <- abs(bfi_rebuilt[[facet]]-supplied[[facet]])>1e-10
  if(!any(changed)) next
  key <- bfi2_facet_items[[facet]]
  items <- bfi_items[,abs(key),drop=FALSE]
  fractional <- abs(items-round(items))>1e-10
  stopifnot(all(rowSums(fractional)[changed]==1L))
  items[,key<0] <- 6-items[,key<0]
  items[fractional] <- NA_real_
  stopifnot(max(abs(rowMeans(items[changed,,drop=FALSE],na.rm=TRUE)-
    supplied[[facet]][changed]))<1e-10)
}
rebuilt_data <- supplied
# The previous item reconstruction exactly reproduces all 32 BESSI facets.
# Replace both instruments explicitly; retain source BESSI domain weights and
# the invalid BESSI171 value to isolate the effect of BFI-2 item reconstruction.
rebuilt_data[,names(rebuilt_facets)] <- rebuilt_facets
rebuilt_data[,names(bfi_rebuilt)] <- bfi_rebuilt
audit_0120$b_bfi_rebuilt <- audit_compare_matrix("0120","0120b",rebuilt_data,123,
  note=paste(audit_0120$b_source$summary$note,
    "Independent BFI-2 key applied to released item values: 9 changed facet scores across 6 facets, affecting 9 participants; supplied values equal available-three-item means in these cells. Compatible with a difference in imputation/scoring order, not a demonstrated reversal-key error. Candidate only; no score replacement approved."),
  sources=names(source_hashes),
  evidence="Official BFI-2 scoring key p.2 (Colby Personality Lab; checked 2026-11-13); 0120 PDF Tables 8-9 pp.22-23",
  status="UNAPPROVED_BFI2_RECONSTRUCTION",comparison="bfi2_item_reconstruction")

# 8. Publication comparisons and export of the approved extraction only.
summary_0120 <- dplyr::bind_rows(lapply(audit_0120, `[[`, "summary"))
# Published reference values are in keys/0120.R. Existing published_* columns
# refer to the 35 domain correlations from Table 8; table9_* covers 480 facets.
summary_0120$published_pairs_compared <- NA_integer_
summary_0120$published_pairs_outside_rounding <- NA_integer_
summary_0120$published_max_abs_delta <- NA_real_
summary_0120$table9_pairs_compared <- NA_integer_
summary_0120$table9_pairs_outside_rounding <- NA_integer_
summary_0120$table9_max_abs_delta <- NA_real_
for (key in c("b_source","b_official","b_invalid_as_missing","b_bfi_rebuilt","b_approved")) {
  entry <- audit_0120[[key]]
  compared <- entry$candidate[rownames(published_table8),colnames(published_table8)]
  difference <- abs(compared-published_table8)
  row <- summary_0120$matrix_id=="0120b" & summary_0120$comparison==entry$summary$comparison
  summary_0120$published_pairs_compared[row] <- length(difference)
  summary_0120$published_pairs_outside_rounding[row] <- sum(difference > .005 + 1e-12)
  summary_0120$published_max_abs_delta[row] <- max(difference)
  compared9 <- entry$candidate[rownames(published_table9),colnames(published_table9)]
  difference9 <- abs(compared9-published_table9)
  summary_0120$table9_pairs_compared[row] <- length(difference9)
  summary_0120$table9_pairs_outside_rounding[row] <- sum(difference9>.005+1e-12)
  summary_0120$table9_max_abs_delta[row] <- max(difference9)
  summary_0120$note[row] <- paste(summary_0120$note[row],
    "Compared 35 Table 8 domain coefficients and all 480 Table 9 facet coefficients at two-decimal rounding tolerance. Publication discrepancy remains deferred pending author clarification; it is separate from the approved BESSI corrections.")
}
print(summary_0120[,c("matrix_id","comparison","changed_pairs","max_abs_delta","pair_n_min","pair_n_max")])
if (!"--check-only" %in% args) {
  source("R/codebook_workflow.R",local=TRUE)
  source("R/extraction_helpers.R",local=TRUE)
  source("R/workflow_io.R",local=TRUE)
  source("R/workflow_results.R",local=TRUE)
  # Keep the first reviewed before/after comparison in the existing aggregate
  # report. Subsequent runs compare with the current saved matrices, without
  # replacing this dated integration record or creating another audit file.
  audit_path <- "data/3.meta_data/extraction/historical_audit.csv"
  previous <- if(file.exists(audit_path)) readr::read_csv(audit_path,show_col_types=FALSE) else summary_0120[FALSE,]
  first_integration <- summary_0120[summary_0120$comparison=="approved_source_correction",]
  first_integration$comparison <- "approved_integration_2026_11_13"
  first_integration$status <- "REVIEWED_BEFORE_AFTER"
  first_integration <- first_integration[!first_integration$matrix_id %in%
    previous$matrix_id[previous$comparison=="approved_integration_2026_11_13"],]
  audit_save_summary(dplyr::bind_rows(summary_0120,first_integration))
  for(sample in c("a","b")) {
    entry <- audit_0120[[paste0(sample,"_approved")]]
    individual_data <- entry$data_matrix
    individual_data$age <- as.numeric(raw_samples[[sample]]$Age)
    checks <- extraction_check_matrix(entry$data_matrix,entry$candidate,entry$pair_n)
    checks$invalid_bessi_items_as_missing <- if(sample=="b")1L else 0L
    wf_compact_extraction("0120",paste0("0120",sample),individual_data,
      entry$candidate,entry$pair_n,checks)
  }
}
