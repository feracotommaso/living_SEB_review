# 0050: read-only historical verification of two matrices from the released Rdata.
# Normal run: update only historical_audit.csv. --check-only never writes.
# Paper data/code: https://osf.io/9pvmj/. Historical retrieval date unknown.
source("R/historical_audit.R")
source("scripts/extraction/keys/bessi192.R",local=TRUE)
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% "--check-only"))
source_hashes <- c(
  "data/3.meta_data/open_data/d0050.Rdata"="229c2fee55a3c3e578c8216eaa3c31b8b6f850dbc006ddb41a087e051cfc1cbd",
  "data/2.full_abstracting/pdfs/0050.pdf"="c479f900ed63c77541814ec7074de1ecca0d98d3a109bf115aa938ace5ac1dea",
  "data/3.meta_data/open_data/raw/shared/bessi_scoring_v1.pdf"="9d842f5196f021131be8b6561cc70b17dd0deebdaca2cbdc2f3579dce8e0b2a9")
audit_verify_sources(source_hashes)

# 1-3. Load into a private environment, not the user's workspace.
raw_objects <- new.env(parent=emptyenv())
load(names(source_hashes)[1],envir=raw_objects)
# a: main Study 2/T4 cohort. b: Study 3 combines T0/T1/T2 sources, not repeated
# BESSI waves as separate samples. Paper pp.9/19-20 explains the time separation.
raw_a <- raw_objects$adults_t4
raw_b <- raw_objects$adults_teens_t0_t1_t2
stopifnot(nrow(raw_a)==940,nrow(raw_b)==768,
  !anyDuplicated(raw_a$id),!anyDuplicated(raw_b$id))
# Paper p.9 reports 238 T4 adults had participated at T2 (203 valid at both).
# The Rdata includes 203 paired records, but their IDs do not map to T4 IDs.
# Do not infer independence from the anonymized ID namespaces, or exclude people
# by an unverified score match. Keep this new dependency question for discussion.
stopifnot(nrow(raw_objects$adults_teens_t2_t4)==203)

# 4-5. Explicit mappings. Questionnaire score columns are supplied means on 1-5.
facet_map <- c(
  "goal_prm"="goalregulation",
  "tafo_prm"="taskmanagement",
  "deci_prm"="decisionmakingskill",
  "deta_prm"="detailmanagement",
  "cons_prm"="capacityforconsistency",
  "orga_prm"="organizationalskill",
  "time_prm"="timemanagement",
  "resp_prm"="responsibilitymanagement",
  "rule_prm"="rulefollowingskill",
  "curi_prm"="abstractthinkingskill",
  "crea_prm"="creativeskill",
  "info_prm"="informationprocessingskill",
  "cult_prm"="culturalcompetence",
  "arti_prm"="artisticskill",
  "empa_prm"="perspectivetakingskill",
  "soha_prm"="capacityforsocialwarmth",
  "team_prm"="teamworkskill",
  "inte_prm"="ethicalcompetence",
  "trus_prm"="capacityfortrust",
  "lead_prm"="leadershipskill",
  "expr_prm"="expressiveskill",
  "conv_prm"="conversationalskill",
  "pers_prm"="persuasiveskill",
  "ener_prm"="energyregulation",
  "stre_prm"="stressregulation",
  "opti_prm"="capacityforoptimism",
  "scon_prm"="confidenceregulation",
  "impu_prm"="impulseregulation",
  "angm_prm"="angermanagement",
  "refl_prm"="selfreflectionskill",
  "inde_prm"="capacityforindependence",
  "adap_prm"="adaptability")
trait_map <- c(
  "b5_cons_prm"="conscientiousness",
  "b5_open_prm"="openness",
  "b5_agre_prm"="agreeableness",
  "b5_extr_prm"="extraversion",
  "b5_emos_prm"="neuroticism",
  "b5_open_int_prm"="bf_curiosity",
  "b5_open_aes_prm"="bf_aesthetic",
  "b5_open_cre_prm"="bf_creativity",
  "b5_cons_org_prm"="bf_organization",
  "b5_cons_pro_prm"="bf_productiveness",
  "b5_cons_res_prm"="bf_responsibility",
  "b5_extr_soc_prm"="bf_sociability",
  "b5_extr_ass_prm"="bf_assertiveness",
  "b5_extr_ene_prm"="bf_energy",
  "b5_agre_com_prm"="bf_compassion",
  "b5_agre_res_prm"="bf_respectfulness",
  "b5_agre_tru_prm"="bf_trust",
  "b5_emos_anx_prm"="bf_anxiety",
  "b5_emos_dep_prm"="bf_depression",
  "b5_emos_vol_prm"="bf_emotionalvolatility")
# Item keys extracted from the source Rdata b5_lookup; these released BFI values
# are already keyed toward Emotional Stability. Do not reverse them again.
bfi_key <- list(
  "b5_open"=c("bfiop05","bfiop10","bfiop15","bfiop20","bfiop25","bfiop30"),
  "b5_cons"=c("bfico03","bfico08","bfico13","bfico18","bfico23","bfico28"),
  "b5_extr"=c("bfiex01","bfiex06","bfiex11","bfiex16","bfiex21","bfiex26"),
  "b5_agre"=c("bfiag02","bfiag07","bfiag12","bfiag17","bfiag22","bfiag27"),
  "b5_emos"=c("bfine04","bfine09","bfine14","bfine19","bfine24","bfine29"),
  "b5_extr_soc"=c("bfiex16","bfiex01"),
  "b5_extr_ass"=c("bfiex21","bfiex06"),
  "b5_extr_ene"=c("bfiex26","bfiex11"),
  "b5_agre_com"=c("bfiag17","bfiag02"),
  "b5_agre_res"=c("bfiag22","bfiag07"),
  "b5_agre_tru"=c("bfiag27","bfiag12"),
  "b5_cons_org"=c("bfico18","bfico03"),
  "b5_cons_pro"=c("bfico23","bfico08"),
  "b5_cons_res"=c("bfico28","bfico13"),
  "b5_emos_anx"=c("bfine19","bfine04"),
  "b5_emos_dep"=c("bfine24","bfine09"),
  "b5_emos_vol"=c("bfine29","bfine14"),
  "b5_open_aes"=c("bfiop20","bfiop05"),
  "b5_open_int"=c("bfiop25","bfiop10"),
  "b5_open_cre"=c("bfiop30","bfiop15"))
# Historical domains used primary facets only (nine/four), complete-constituent
# means. Keep this baseline visible; official interstitial weights are separate.
legacy_domains <- list(
  selfmanagement=c("taskmanagement","timemanagement","detailmanagement","organizationalskill",
    "responsibilitymanagement","capacityforconsistency","goalregulation","rulefollowingskill","decisionmakingskill"),
  socialengagement=c("leadershipskill","persuasiveskill","conversationalskill","expressiveskill"),
  cooperation=c("teamworkskill","capacityfortrust","perspectivetakingskill","capacityforsocialwarmth"),
  emotionalresilience=c("stressregulation","capacityforoptimism","angermanagement","confidenceregulation"),
  innovation=c("creativeskill","abstractthinkingskill","artisticskill","culturalcompetence"))
audit_0050 <- list();score_checks <- list()
for(sample in c("a","b")) {
  raw <- if(sample=="a")raw_a else raw_b
  matrix_id <- paste0("0050",sample)
  data_scored <- as.data.frame(raw[,names(facet_map)])
  names(data_scored) <- unname(facet_map)
  stopifnot(all(is.na(as.matrix(data_scored)) | (as.matrix(data_scored)>=1 & as.matrix(data_scored)<=5)))

  # Reconstruct means from released six-item columns. At T4 all 32 agree. At
  # T2, eight revised facets cannot be reproduced from the retained initial-name
  # item columns; keep supplied scores, flag the mismatch, and do not silently
  # substitute older translations. Their exact scoring source needs clarification.
  checks <- list()
  for(source in names(facet_map)) {
    prefix <- sub("_prm$","",source)
    columns <- paste0(prefix,1:6)
    items <- as.matrix(raw[,columns])
    stopifnot(all(is.na(items) | (items>=1 & items<=5)))
    rebuilt <- rowMeans(items,na.rm=TRUE);rebuilt[is.nan(rebuilt)] <- NA_real_
    supplied <- as.numeric(raw[[source]])
    checks[[source]] <- data.frame(facet=facet_map[[source]],
      changed_values=sum(abs(rebuilt-supplied)>1e-10,na.rm=TRUE),
      missingness_differences=sum(xor(is.na(rebuilt),is.na(supplied))),
      max_abs_delta=max(abs(rebuilt-supplied),na.rm=TRUE))
  }
  score_checks[[sample]] <- do.call(rbind,checks)
  if(sample=="a")stopifnot(all(score_checks[[sample]]$changed_values==0),
    all(score_checks[[sample]]$missingness_differences==0))
  for(domain in names(legacy_domains))
    data_scored[[domain]] <- rowMeans(data_scored[,legacy_domains[[domain]]])

  if(sample=="a") {
    stopifnot(all(is.na(raw$sex) | raw$sex %in% c(1,2)))
    data_scored$sex <- ifelse(raw$sex==1,0,ifelse(raw$sex==2,1,NA_real_))
  } else {
    # Preserve historical reconciliation, without treating discordant sex values
    # as an invented intermediate category or removing the participant.
    sex_source <- rowMeans(raw[,c("sex.x","sex.y")],na.rm=TRUE)
    data_scored$sex <- ifelse(sex_source==1,0,ifelse(sex_source==2,1,NA_real_))
    for(source in names(trait_map)) {
      item_columns <- bfi_key[[sub("_prm$","",source)]]
      rebuilt <- rowMeans(raw[,item_columns],na.rm=TRUE);rebuilt[is.nan(rebuilt)] <- NA_real_
      stopifnot(isTRUE(all.equal(rebuilt,as.numeric(raw[[source]]),check.attributes=FALSE,tolerance=1e-12)))
      data_scored[[trait_map[[source]]]] <- as.numeric(raw[[source]])
    }
    for(variable in c("neuroticism","bf_anxiety","bf_depression","bf_emotionalvolatility"))
      data_scored[[variable]] <- -data_scored[[variable]]
    # Released ICAR/BEFKI items are already recoded 0/1. The source deliberately
    # retains only 607 eligible test administrations, excluding 120 with another
    # timing protocol (PDF p.20). Verify sums for those 607; never undo this mask.
    intelligence_items <- list(IQGF=c("iqvr04_R","iqvr16_R","iqvr17_R","iqvr19_R","iqln07_R","iqln33_R","iqln34_R","iqln58_R","iqmr45_R","iqmr46_R","iqmr47_R","iqmr55_R"),IQGC=c("iqbe01_R","iqbe02_R","iqbe03_R","iqbe04_R","iqbe05_R","iqbe06_R","iqbe07_R","iqbe08_R","iqbe09_R","iqbe10_R","iqbe11_R","iqbe12_R"))
    for(source in names(intelligence_items)) {
      eligible <- !is.na(raw[[source]])
      stopifnot(sum(eligible)==607,
        all(raw[[source]][eligible]==rowSums(raw[eligible,intelligence_items[[source]],drop=FALSE])))
    }
    data_scored$intelligencecrystallized <- as.numeric(scale(raw$IQGC))
    data_scored$intelligencefluid <- as.numeric(scale(raw$IQGF))
    value <- rowMeans(cbind(scale(data_scored$intelligencecrystallized),
      scale(data_scored$intelligencefluid)),na.rm=TRUE)
    value[is.nan(value)] <- NA_real_;data_scored$intelligence <- value
  }
  data_matrix <- data_scored
  note <- paste("Reference N",if(sample=="a")940 else 767,
    "; released rows",nrow(raw),". Every source record retained. No age in correlations.",
    "Source domains use nine/four primary facets with complete-constituent means, omitting official interstitial contributions.",
    "Study 2 and Study 3 are not proven independent: paper reports 238 recontacted adults (203 valid at both waves). Anonymized T4 IDs do not directly link; no new exclusions.",
    if(sample=="a")"All 32 supplied facet means independently reconstructed from released items."else
      "Study 3 uses BESSI at T2, cognition at T1, and BFI items split between T0/T2. Twenty BFI scores and eligible IQ sums reproduced; 24 BESSI facets reproduce, eight revised-facet scores require source clarification. Supplied scores retained. File has 768 rows versus paper N=767; only 766 rows contribute at least two selected scores. Reference N retained pending reconciliation.")
  audit_0050[[paste0(sample,"_source")]] <- audit_compare_matrix("0050",matrix_id,data_matrix,
    if(sample=="a")940 else 767,note,names(source_hashes),
    "0050 PDF pp.9 and 19-20; original Rdata objects, released score columns and item keys",
    status="MATRIX_REPRODUCED_SCORING_AND_DEPENDENCE_REVIEW_REQUIRED")
  item_summary <- audit_0050[[paste0(sample,"_source")]]$summary
  item_summary$comparison <- "released_item_score_check"
  item_summary$score_mismatch_facets <- sum(score_checks[[sample]]$changed_values>0 | score_checks[[sample]]$missingness_differences>0)
  item_summary$score_mismatch_values <- sum(score_checks[[sample]]$changed_values)
  item_summary$score_max_abs_delta <- max(score_checks[[sample]]$max_abs_delta)
  item_summary$unreconciled_facets <- paste(score_checks[[sample]]$facet[
    score_checks[[sample]]$changed_values>0 | score_checks[[sample]]$missingness_differences>0],collapse=";")
  item_summary$status <- if(sample=="a")"32_FACET_SCORES_REPRODUCED"else"8_REVISED_FACET_SCORES_UNRECONCILED"
  audit_0050[[paste0(sample,"_items")]] <- list(summary=item_summary)

  # Separate unapproved sensitivity: official normalized core/interstitial
  # weights, retaining the complete-constituent rule. Added constituents can
  # alter pair Ns under the planned-missingness design; this is not exported.
  official <- data_matrix
  for(domain in names(bessi192_domain_weights)) {
    weights <- bessi192_domain_weights[[domain]]
    official[[domain]] <- rowSums(sweep(as.matrix(data_matrix[,names(weights)]),2,weights,"*"))/sum(weights)
  }
  audit_0050[[paste0(sample,"_weights")]] <- audit_compare_matrix("0050",matrix_id,official,
    if(sample=="a")940 else 767,
    paste(note,"Unapproved sensitivity: official domain weights 1:0.5, normalized, with every constituent required. More constituents can reduce score coverage; missing-data policy must be decided before export."),
    names(source_hashes),"Official BESSI v1.0 scoring key pp.1-2; source planned missingness",
    status="SCORING_AND_MISSINGNESS_REVIEW_REQUIRED",comparison="official_weight_sensitivity")
}
summary_0050 <- dplyr::bind_rows(lapply(audit_0050,`[[`,"summary"))
print(summary_0050[,c("matrix_id","comparison","changed_pairs","max_abs_delta","pair_n_min","pair_n_max","score_mismatch_facets")])
if(!"--check-only" %in% args)audit_save_summary(summary_0050)
