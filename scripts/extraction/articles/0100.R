# 0100 — Pellegrino et al. (2026), University Students With Specific Learning
# Disabilities: Do Soft Skills and Study-Related Factors Make a Difference to
# Their Academic Outcomes? Approved degree-standardized grades and Cattell
# recovery are exported on a normal run; --check-only writes nothing.
source('R/historical_audit.R')
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% '--check-only'))
source_hashes <- c(
 'data/3.meta_data/open_data/d0100.xlsx'='0d32568001c68c69b6839b678b6bdd6a9c243a886838ad2ccd3d5d26ab312ae0',
 'data/3.meta_data/open_data/opencode0100.R'='6aa47efcd5561a766b400903090e756f3d0b84867962f3e813c36f4007aec884',
 'data/2.full_abstracting/pdfs/0100.pdf'='381d621206f64a1a07b78891e61eff620a9f9238cd239666d02f97b3c1563c33')
audit_verify_sources(source_hashes)

# 1-3. All 752 released students, SLD and comparison, without matching/subsetting.
# Source already screened. Grades exist for 331; retain all other participants.
raw <- as.data.frame(readxl::read_excel(names(source_hashes)[1]))
stopifnot(nrow(raw)==752L,!anyDuplicated(raw$Code),all(raw$Gender %in% 1:2),
 sum(!is.na(raw$Achievement))==331L)
data_analytic <- raw

# 4-5. Available-item means reproduce the author code. All-missing means stay NA.
data_scored <- data.frame(academicachievement=raw$Achievement,
 sex=ifelse(raw$Gender=='2',1,0))
positions <- c(selfmanagement=1,innovation=5,cooperation=3,
 socialengagement=2,emotionalresilience=4)
for(domain in names(positions)) {
 values <- as.matrix(raw[,paste0('bessi_',seq(positions[[domain]],45,5))])
 stopifnot(all(is.na(values) | (values>=1 & values<=5)))
 value <- rowMeans(values,na.rm=TRUE);value[is.nan(value)] <- NA_real_
 data_scored[[domain]] <- value
}
# Engagement: reverse four items, then average six subscale means with equal
# subscale weights, as in the authors' code (not the mean of all 29 items).
engagement <- as.data.frame(raw[,paste0('eng_',1:29)])
engagement[,paste0('eng_',c(1,20,24,25))] <- 6-engagement[,paste0('eng_',c(1,20,24,25))]
engagement_keys <- list(persistence=c(1,20,24,25),university=c(2,13,14,18,19,23),
 course=c(9,10,11,12,26,27,29),network=c(15,16,17),peers=c(3,4,21,22,28),professors=5:8)
engagement_subscales <- as.data.frame(lapply(engagement_keys,
 function(key)rowMeans(engagement[,paste0('eng_',key)],na.rm=TRUE)))
data_scored$academicengagement <- rowMeans(engagement_subscales,na.rm=TRUE)
qas <- as.matrix(raw[,paste0('QAS_',1:20)])
qas_reverse <- c(2,5,8,9,10,12,17)
qas[,qas_reverse] <- 6-qas[,qas_reverse]
data_scored$srlstrategies <- rowMeans(qas,na.rm=TRUE)
scale_items <- list(academicselfefficacy=paste0('eff_',1:5),
 academicsatisfaction=paste0('uni_sat_',1:5),satisfactionwithlife=paste0('life_',1:5),
 academicburnout=paste0('burn_',1:9))
for(variable in names(scale_items))
 data_scored[[variable]] <- rowMeans(raw[,scale_items[[variable]]],na.rm=TRUE)
for(variable in names(data_scored))data_scored[[variable]][is.nan(data_scored[[variable]])] <- NA_real_
matrix_variables <- c('academicachievement','sex',names(positions),
 'academicengagement','academicselfefficacy','srlstrategies',
 'academicsatisfaction','satisfactionwithlife','academicburnout')
data_matrix <- data_scored[,matrix_variables]
individual_data <- cbind(data_scored,age=raw$Age)
note <- paste('All 78 historical correlations reproduced; N=752, pair Ns=331-752. All five BESSI-45 means, four engagement reversals, equally weighted six-subscale engagement, seven QAS reversals and other questionnaire means reconstructed from original items.',
 'Grade raw values retained in historical comparison. Author code standardizes grades within Degree (bachelor/master/single cycle); this is a separate candidate, without excluding any participant or imputing grades.',
 'Cattell fluid-intelligence total is available for all 752; raw task responses are not supplied, so item scoring cannot be independently reconstructed. Age excluded from matrices.',
 'User approval 2026-11-14: standardize grades within degree and recover intelligencefluid. Retain all 752 participants, 331 grades and all other coefficients; do not impute grades.')
audit_0100 <- list(historical=audit_compare_matrix('0100','0100a',data_matrix,752,
 note,names(source_hashes),'0100 Methods pp.5-7; Table 4 p.7; author scoring/correlation code',
 status='LEGACY_SCORING_RECONSTRUCTED'))

# 6. Reproduce Table 4 exactly: Pearson pairwise correlations, grades scaled
# within degree. Global scaling of the other variables does not affect r.
author_scores <- data_matrix
for(degree in unique(raw$Degree)) {
 selected <- raw$Degree==degree
 author_scores$academicachievement[selected] <- as.numeric(scale(raw$Achievement[selected]))
}
publication_variables <- c('academicengagement','academicselfefficacy','srlstrategies',
 'selfmanagement','innovation','socialengagement','cooperation','emotionalresilience',
 'academicsatisfaction','satisfactionwithlife','academicburnout','academicachievement')
published <- c(
 .46,.44,.53,.46,.52,.57,.22,.22,.31,.26,.38,.31,.35,.39,.44,
 .41,.22,.31,.43,.38,.57,.32,.24,.32,.37,.36,.40,.51,
 .55,.30,.30,.28,.23,.22,.28,.28,.47,.31,.26,.31,.14,.35,.26,.45,.37,
 -.48,-.38,-.30,-.28,-.18,-.22,-.17,-.39,-.46,-.41,
 .17,.47,.25,.19,-.01,.08,.12,.06,.07,.05,-.09,
 -.05,.07,.08,-.02,.07,-.03,-.07,-.02,.05,-.05,-.02,.10,
 .00,.02,.14,.07,.10,.12,.05,.11,.12,.01,-.05,.02,.08)
publication_data <- cbind(author_scores[,publication_variables],
 intelligencefluid=raw$Cattell_TOT,age=raw$Age)
publication_cor <- cor(publication_data,method='pearson',use='pairwise.complete.obs')
delta <- abs(publication_cor[upper.tri(publication_cor)]-published)
stopifnot(length(delta)==91L,all(delta<=.0050001),
 abs(mean(raw$Cattell_TOT)-11.80)<.0051,abs(sd(raw$Cattell_TOT)-4.15)<.0051)
audit_0100$degree <- audit_compare_matrix('0100','0100a',author_scores,752,note,
 names(source_hashes),'0100 author correlation code: group_by(Degree), scale(Achievement)',
 status='APPROVED_DEGREE_STANDARDIZATION',comparison='degree_standardized_achievement')
with_intelligence <- author_scores
with_intelligence$intelligencefluid <- raw$Cattell_TOT
audit_0100$recovery <- audit_compare_matrix('0100','0100a',with_intelligence,752,note,
 names(source_hashes),'0100 Table 4 fluid intelligence, Methods Cattell-3A',
 status='APPROVED_EXTRACTION',comparison='degree_and_fluid_intelligence_recovery')
audit_0100$degree$summary$published_pairs_checked <- length(delta)
audit_0100$degree$summary$published_pairs_outside_rounding <- sum(delta>.0050001)
audit_0100$recovery$summary$published_pairs_checked <- length(delta)
audit_0100$recovery$summary$published_pairs_outside_rounding <- sum(delta>.0050001)
# 7-8. Export only the approved combination; preserve the first before/after.
summary_0100 <- dplyr::bind_rows(lapply(audit_0100,`[[`,'summary'))
print(summary_0100[,c('matrix_id','comparison','changed_pairs','added_variables','pair_n_min','pair_n_max')])
if(!'--check-only' %in% args) {
 source('R/codebook_workflow.R',local=TRUE);source('R/extraction_helpers.R',local=TRUE)
 source('R/workflow_io.R',local=TRUE);source('R/workflow_results.R',local=TRUE)
 old <- readr::read_csv('data/3.meta_data/extraction/historical_audit.csv',show_col_types=FALSE)
 first <- audit_0100$recovery$summary
 first$comparison <- 'approved_integration_2026_11_14';first$status <- 'REVIEWED_BEFORE_AFTER'
 if(any(old$matrix_id=='0100a' & old$comparison==first$comparison))first <- first[FALSE,]
 audit_save_summary(dplyr::bind_rows(summary_0100,first))
 individual_data <- cbind(with_intelligence,age=raw$Age,
  academicachievement_raw=raw$Achievement,degree_original=raw$Degree)
 checks <- extraction_check_matrix(with_intelligence,audit_0100$recovery$candidate,audit_0100$recovery$pair_n)
 wf_compact_extraction('0100','0100a',individual_data,
  audit_0100$recovery$candidate,audit_0100$recovery$pair_n,checks)
}
