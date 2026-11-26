# 0034: user-approved SNS mapping and official BESSI domain scoring.
# A normal run exports the approved matrix, private coded individuals, pair Ns
# and aggregate checks. --check-only never writes. Update final results through
# workflow.R. User explicitly retained all 303 people, including eight aged 17.
source("R/historical_audit.R")
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% "--check-only"))
source_hashes <- c(
  "data/3.meta_data/open_data/d0034t.sav"="dcf4bd927536d637cc240a2f335ffe16598bf4447931dcbbdf501cd8803e5a90",
  "data/3.meta_data/open_data/d0034.sav"="7c136869230e1ba64f8fba4710ac0436afa511bf3305a2f43c6c18ded13e16a4",
  "data/2.full_abstracting/pdfs/0034.pdf"="b826ba7f5d438ccd05f0af68508feb3f0a2994d1615cb2ef03327bfa9e6c81a6",
  "data/3.meta_data/open_data/raw/shared/bessi_scoring_v1.pdf"="9d842f5196f021131be8b6561cc70b17dd0deebdaca2cbdc2f3579dce8e0b2a9")
audit_verify_sources(source_hashes)

source("scripts/extraction/keys/bessi192.R",local=TRUE)
# Reuse only BFI-2 item allocation constants already checked against the official
# key; this companion has constants, not the 0120 article's processing code.
source("scripts/extraction/keys/0120.R",local=TRUE)

# 1-3. Prepared first-occasion N=303, with retest columns for 42 people. The
# earlier release has 322 rows; all 303 prepared IDs link uniquely into it.
# The paper reports 313 minus 10 exclusions (p.4). Do not invent a new filter.
raw <- as.data.frame(haven::read_sav(names(source_hashes)[1]))
raw_earlier <- as.data.frame(haven::read_sav(names(source_hashes)[2]))
links <- match(raw$ID,raw_earlier$ID)
stopifnot(nrow(raw)==303,nrow(raw_earlier)==322,
  !anyDuplicated(raw$ID),!anyDuplicated(raw_earlier$ID),!anyNA(links))

# 4-5. Explicit mapping, with the two SNS constructs corrected as approved.
variable_map <- c(
  "estudios"="educationlevel",
  "DOMINIO_SelfManagement"="selfmanagement",
  "DOMINIO_SocialEngagement"="socialengagement",
  "DOMINIO_Cooperation"="cooperation",
  "DOMINIO_EmotionalResilience"="emotionalresilience",
  "DOMINIO_Innovation"="innovation",
  "GR"="goalregulation",
  "TaskM"="taskmanagement",
  "DecM"="decisionmakingskill",
  "DM"="detailmanagement",
  "CC"="capacityforconsistency",
  "ORR"="organizationalskill",
  "TM"="timemanagement",
  "RM"="responsibilitymanagement",
  "RF"="rulefollowingskill",
  "ABS"="abstractthinkingskill",
  "CREA"="creativeskill",
  "IP"="informationprocessingskill",
  "CU"="culturalcompetence",
  "ART"="artisticskill",
  "PT"="perspectivetakingskill",
  "CSW"="capacityforsocialwarmth",
  "TW"="teamworkskill",
  "EC"="ethicalcompetence",
  "CT"="capacityfortrust",
  "LS"="leadershipskill",
  "Ex"="expressiveskill",
  "CV"="conversationalskill",
  "PS"="persuasiveskill",
  "ER"="energyregulation",
  "SR"="stressregulation",
  "COP"="capacityforoptimism",
  "CR"="confidenceregulation",
  "IR"="impulseregulation",
  "AM"="angermanagement",
  "REF"="selfreflectionskill",
  "IND"="capacityforindependence",
  "ADA"="adaptability",
  "Conscientiousness"="conscientiousness",
  "OpenMindedness"="openness",
  "Agreeableness"="agreeableness",
  "Extraversion"="extraversion",
  "Negative_Emotionality"="neuroticism",
  "Curiosity"="bf_curiosity",
  "AestheticSensitivity"="bf_aesthetic",
  "Creativity"="bf_creativity",
  "Organization"="bf_organization",
  "Productiveness"="bf_productiveness",
  "Responsability"="bf_responsibility",
  "Sociabilidad"="bf_sociability",
  "Assertiviness"="bf_assertiveness",
  "Energy"="bf_energy",
  "Compassion"="bf_compassion",
  "Respectfulness"="bf_respectfulness",
  "Trust"="bf_trust",
  "Anxiety"="bf_anxiety",
  "Depression"="bf_depression",
  "Volatility"="bf_emotionalvolatility",
  "SelfmanagementMotivation"="secaSelfmanagement",
  "SocialawarenessProsocialbehavior"="secaSocialawareness",
  "Selfawareness"="secaSelfawareness",
  "DecisionMaking"="secaDecisionmaking",
  "MindsetGrowth"="growthmindset",
  "MindsetFixed"="fixedmindset",
  "Grit"="grit",
  "Curiosity_Test"="epistemiccuriosity",
  "PersonalidadEmprendedora"="enterprisingpersonality",
  "ComparaciónSocial_Redes"="socialnetworknegativesocialcomparison",
  "ConsecuenciasAdictivas_Redes"="socialnetworkaddictiveconsequences")
data_scored <- raw
names(data_scored)[match(names(variable_map),names(data_scored))] <- unname(variable_map)
stopifnot(all(raw$sexo %in% c(1,2)))
# Source labels: 1 Mujer, 2 Hombre. Historical contrast is 0 male, 1 female.
data_scored$sex <- ifelse(raw$sexo==2,0,1)
data_scored$age <- raw$edad

# Independently rebuild the 32 BESSI facet sums from the six selected Spanish
# items, including the explicitly named Alternative columns. Exclude all _TR
# retest values and parcel columns. The supplied sums require all six items:
# one missing persuasive item and one missing goal-regulation item propagate NA.
facet_means <- list()
for(facet in names(bessi192_first_item)) {
  source <- names(variable_map)[match(facet,variable_map)]
  prefix <- if(source=="ORR")"OR" else source
  columns <- grep(paste0("^",prefix,"_(0[1-6]|Alternative_.*)$"),names(raw),value=TRUE)
  columns <- columns[!endsWith(columns,"_TR")]
  stopifnot(length(columns)==6L)
  items <- as.matrix(raw[,columns])
  stopifnot(all(is.na(items) | (items>=1 & items<=5)))
  value <- rowMeans(items) # Complete-six rule reproduces the supplied sums.
  stopifnot(isTRUE(all.equal(value,as.numeric(raw[[source]])/6,
    check.attributes=FALSE,tolerance=1e-12)))
  facet_means[[facet]] <- value
  data_scored[[facet]] <- as.numeric(raw[[source]])/6
}
facet_means <- as.data.frame(facet_means)

# Source domains: nine primary facets for self-management; all five assigned
# facets equally weighted for each other domain. Any missing constituent facet
# leaves its domain missing, matching the supplied totals.
for(domain in names(bessi192_domain_weights)) {
  weights <- bessi192_domain_weights[[domain]]
  used <- if(domain=="selfmanagement")names(weights)[weights==1] else names(weights)
  source_mean <- rowMeans(facet_means[,used])
  source <- names(variable_map)[match(domain,variable_map)]
  supplied_mean <- as.numeric(raw[[source]])/(6*length(used))
  stopifnot(isTRUE(all.equal(source_mean,supplied_mean,check.attributes=FALSE,tolerance=1e-12)))
  data_scored[[domain]] <- supplied_mean
}

# BFI-2 supplied facets equal sums of four released items; domains sum their
# three facets. Both SAV releases contain identical BFI item values after ID
# linkage. Oppositely worded sociability items correlate positively, consistent
# with prior reverse scoring. Its history is not documented: do not reverse
# these released values again or claim access to original unkeyed responses.
for(facet in names(bfi2_facet_items)) {
  indices <- abs(bfi2_facet_items[[facet]])
  columns <- vapply(indices,function(i)grep(sprintf("^BFI_%02d_",i),names(raw),value=TRUE),character(1))
  stopifnot(isTRUE(all.equal(as.matrix(raw[,columns]),as.matrix(raw_earlier[links,columns]),
    check.attributes=FALSE,tolerance=1e-12)))
  stopifnot(max(abs(rowSums(raw[,columns])-data_scored[[facet]]))<1e-12)
}
for(domain in names(bfi2_domain_facets))
  stopifnot(max(abs(rowSums(data_scored[,bfi2_domain_facets[[domain]]])-data_scored[[domain]]))<1e-12)

# SNS source sums and original Spanish item wording identify the constructs:
# UPS_1:10_NC: inferiority/loneliness while viewing others -> social comparison.
# UPS_11:18_AC: urges to connect/lost time/neglected tasks -> addictive effects.
# The paper p.5 gives the opposite item counts; preserve this source inconsistency.
social_comparison_items <- grep("^UPS_.*_NC_",names(raw),value=TRUE)
addictive_items <- grep("^UPS_.*_AC_",names(raw),value=TRUE)
social_comparison_items <- social_comparison_items[!endsWith(social_comparison_items,"_TR")]
addictive_items <- addictive_items[!endsWith(addictive_items,"_TR")]
stopifnot(length(social_comparison_items)==10L,length(addictive_items)==8L,
  all(raw[["ComparaciónSocial_Redes"]]==rowSums(raw[,social_comparison_items])),
  all(raw[["ConsecuenciasAdictivas_Redes"]]==rowSums(raw[,addictive_items])))
data_matrix <- data_scored[,c(unname(variable_map),"sex")]
stopifnot(!any(grepl("_TR$",names(data_matrix))))

# 6-8. Compare the historical choices and the approved combined extraction.
common_note <- paste("Prepared N=303; all 32 BESSI facets reproduced from selected first-wave items. Two missing items give missing facet sums; retest not included.",
  "Source domains differ from official weights. BFI sums reproduce released preprocessed item values; original reversal history unavailable. Other outcome scoring not independently reconstructed.",
  "Eight retained ages are 17 although the paper states 18-85; original 322 rows versus reported initial 313 also requires clarification. User decision 2026-11-13: retain all 303 people, explicitly including minors; no additional exclusions. SNS mapping correction and official domain weights approved together.")
audit_0034 <- list()
# Reproduce the OLD mapping only for a readable comparison; the variable map
# above and the exported combined data use the corrected source meanings.
historical_data <- data_matrix
historical_data$socialnetworkaddictiveconsequences <- raw[["ComparaciónSocial_Redes"]]
historical_data$socialnetworknegativesocialcomparison <- raw[["ConsecuenciasAdictivas_Redes"]]
audit_0034$source <- audit_compare_matrix("0034","0034a",historical_data,303,
  note=common_note,sources=names(source_hashes),
  evidence="0034 PDF pp.4-7; source item columns, sums and SPSS item labels",
  status="HISTORICAL_SCORING_COMPARISON")

label_corrected <- data_matrix
label_corrected$socialnetworknegativesocialcomparison <- raw[["ComparaciónSocial_Redes"]]
label_corrected$socialnetworkaddictiveconsequences <- raw[["ConsecuenciasAdictivas_Redes"]]
audit_0034$sns <- audit_compare_matrix("0034","0034a",label_corrected,303,
  note=paste(common_note,"Isolated SNS mapping comparison. The combined approved extraction is used for export; this isolated alternative is not exported."),
  sources=names(source_hashes),evidence="SPSS labels UPS_01_NC_01 through UPS_18_AC_8; paper p.5 item-count discrepancy",
  status="ISOLATED_MAPPING_COMPARISON",comparison="sns_mapping_sensitivity")

official_data <- historical_data
weighted_columns <- c(selfmanagement="MANAGEMENT_DOMAIN_WEIGHT",socialengagement="SOCIALENGAGEMENT_DOMAIN_WEIGHT",
  cooperation="COOPERATION_DOMAIN_WEIGHT",emotionalresilience="EMOTIONALRESILIENCE_DOMAIN_WEIGHT",innovation="INNOVATION_DOMAIN_WEIGHT")
complete <- complete.cases(facet_means)
for(domain in names(bessi192_domain_weights)) {
  weights <- bessi192_domain_weights[[domain]]
  facets <- as.matrix(facet_means[,names(weights)])
  official_data[[domain]] <- rowSums(sweep(facets,2,weights,"*"))/sum(weights)
  # Existing author WEIGHT columns verify this formula for complete records.
  # Keep the source complete-facet rule for the two incomplete records; do not
  # silently substitute the differently preprocessed WEIGHT-column values.
  reference <- as.numeric(raw[[weighted_columns[[domain]]]])/(6*2*sum(weights))
  stopifnot(max(abs(reference[complete]-official_data[[domain]][complete]))<1e-12)
}
audit_0034$weights <- audit_compare_matrix("0034","0034a",official_data,303,
  note=paste(common_note,"Isolated weighting comparison, preserving source facet NAs and the complete-constituent-facet rule. Author WEIGHT columns confirm complete-record scores. The combined approved extraction is used for export; this isolated alternative is not exported."),
  sources=names(source_hashes),evidence="Official BESSI v1.0 key pp.1-2; prepared SAV WEIGHT columns",
  status="ISOLATED_WEIGHTING_COMPARISON",comparison="official_weight_sensitivity")
combined_data <- official_data
combined_data$socialnetworknegativesocialcomparison <- label_corrected$socialnetworknegativesocialcomparison
combined_data$socialnetworkaddictiveconsequences <- label_corrected$socialnetworkaddictiveconsequences
audit_0034$combined <- audit_compare_matrix("0034","0034a",combined_data,303,
  note=paste(common_note,"Combined user-approved extraction: corrected SNS mapping and official normalized domain weights, preserving source complete-facet missingness, N=303 and all eight minors. Exported by a normal run; --check-only never writes."),
  sources=names(source_hashes),evidence="SPSS SNS item labels and sums; official BESSI scoring key",
  status="APPROVED_EXTRACTION",comparison="combined_correction_sensitivity")
stopifnot(identical(audit_0034$source$pair_n,audit_0034$combined$pair_n),
  nrow(combined_data)==303,sum(raw$edad<18)==8)
summary_0034 <- dplyr::bind_rows(lapply(audit_0034,`[[`,"summary"))
print(summary_0034[,c("matrix_id","comparison","changed_pairs","max_abs_delta","pair_n_min","pair_n_max")])
if(!"--check-only" %in% args) {
  source("R/codebook_workflow.R",local=TRUE)
  source("R/extraction_helpers.R",local=TRUE)
  source("R/workflow_io.R",local=TRUE)
  source("R/workflow_results.R",local=TRUE)
  # Preserve the first reviewed before/after in the existing aggregate report.
  path <- "data/3.meta_data/extraction/historical_audit.csv"
  previous <- if(file.exists(path))readr::read_csv(path,show_col_types=FALSE)else summary_0034[FALSE,]
  first <- summary_0034[summary_0034$comparison=="combined_correction_sensitivity",]
  first$comparison <- "approved_integration_2026_11_13"
  first$status <- "REVIEWED_BEFORE_AFTER"
  first <- first[!first$matrix_id %in% previous$matrix_id[previous$comparison==first$comparison],]
  audit_save_summary(dplyr::bind_rows(summary_0034,first))
  individual_data <- combined_data
  individual_data$age <- as.numeric(raw$edad)
  checks <- extraction_check_matrix(combined_data,audit_0034$combined$candidate,audit_0034$combined$pair_n)
  checks$retained_minors <- sum(individual_data$age<18)
  wf_compact_extraction("0034","0034a",individual_data,audit_0034$combined$candidate,
    audit_0034$combined$pair_n,checks)
}
