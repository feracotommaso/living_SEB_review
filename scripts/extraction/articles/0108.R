# 0108 — Soto et al. (2026), Practice makes perfect: A behavioral challenge
# intervention to develop social, emotional, and behavioral skills.
# Normal run exports the binary-sex correction; --check-only writes nothing.
# Main PDF supplied by the user during this audit; original download date
# unknown. Item-level responses remain unavailable in the public score release.
source('R/historical_audit.R')
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% '--check-only'))
source_hashes <- c(
 'data/3.meta_data/open_data/d0108.sav'='2126ac580390cae06fde8cd96f576087eea73b9c37aaec0d32811b6357f8f168',
 'data/3.meta_data/open_data/raw/0108/Supplemental tables.xlsx'='0c27025b7c899eee8a1b84132a3c390905aacc55b0a5f1161037e10ea1caf9d2',
 'data/2.full_abstracting/pdfs/0108.pdf'='27aeff86d4b1088d05618ac30a4dc3fb12f202296d5784a81c5813a9133622b6')
audit_verify_sources(source_hashes)

# 1-3. One baseline record per person; five outcomes first occur at wave 2.
# Do not turn 16 intervention waves into independent samples or import the
# historical *_t2/latest-wave fields that never entered the saved matrix.
raw <- as.data.frame(haven::read_sav(names(source_hashes)[1]))
stopifnot(!anyDuplicated(paste(raw$user,raw$wave)))
data_analytic <- raw[raw$wave==1,]
wave2 <- raw[raw$wave==2,]
stopifnot(nrow(data_analytic)==470L,!anyDuplicated(data_analytic$user),
 nrow(wave2)==416L,!anyDuplicated(wave2$user),all(wave2$user %in% data_analytic$user))

# 4-5. Supplied scale means/composites only: no item-level data are released.
variable_map <- c(BESSI_Self_Management='selfmanagement',BESSI_Social_Engagement='socialengagement',
 BESSI_Cooperation='cooperation',BESSI_Emotional_Resilience='emotionalresilience',
 BESSI_Innovation='innovation',Social_Status='socialstatus',Friendship_Quality='friendshipquality',
 Civic_Skills='civicskills',Activism='activism',Informal_Helping='informalhelping',
 Volunteering='volunteering',Life_Satisfaction='satisfactionwithlife',
 BFI_Conscientiousness='conscientiousness',BFI_Extraversion='extraversion',
 BFI_Agreeableness='agreeableness',BFI_Emotional_Stability='neuroticism',BFI_Open_Mindedness='openness')
data_scored <- data_analytic[,names(variable_map)];names(data_scored) <- unname(variable_map)
data_scored$neuroticism <- -data_scored$neuroticism
data_scored$sex <- ifelse(data_analytic$gender3==-.5,0,
 ifelse(data_analytic$gender3==.5,1,.5)) # Legacy diagnostic only.
wave2_map <- c(Academic_Engagement='academicengagement',Exercise='physicalexercise',
 Physical_Health='physicalhealth',Mental_Health='mentalhealth',Affective_Well_Being='affectivewellbeing')
link <- match(data_analytic$user,wave2$user)
for(variable in names(wave2_map)) {
 stopifnot(all(is.na(data_analytic[[variable]])))
 data_scored[[wave2_map[[variable]]]] <- wave2[[variable]][link]
}
stopifnot(all(as.matrix(data_scored[,unname(variable_map[1:5])])>=1 &
 as.matrix(data_scored[,unname(variable_map[1:5])])<=5))
data_matrix <- data_scored
data_matrix$sex <- ifelse(data_analytic$gender3==-.5,0,
 ifelse(data_analytic$gender3==.5,1,NA_real_))
stopifnot(sum(data_analytic$gender3==0,na.rm=TRUE)==11L,
 sum(is.na(data_matrix$sex))==13L)

# 6. Main PDF p.5 confirms alternating odd/even outcome batteries and life
# satisfaction at both. Gender3 is the authors' effect contrast (.5 female,
# -.5 male, 0 another identification), not a binary variable. Table S2 has
# 33 variables, followed by p and N blocks. Validate the 17
# variables represented here (106 reported pairs; 30 cross-wave cells blank).
# Its five wave-2 outcomes correlate concurrently
# with wave-2 BESSI/life satisfaction in 30 cells, not with their wave-1 scores.
# This timing is established by exact numerical replication, not by replacing
# our first-observation matrix with a mixture designed to match the table.
table <- suppressMessages(as.data.frame(readxl::read_excel(names(source_hashes)[2],
 sheet='Table S2',col_names=FALSE)))
publication_map <- c('Self-management skills'='selfmanagement','Social engagement skills'='socialengagement',
 'Cooperation skills'='cooperation','Emotional resilience skills'='emotionalresilience',
 'Innovation skills'='innovation','Social status'='socialstatus','Friendship quality'='friendshipquality',
 'Civic engagement'='civicskills','Activism'='activism','Informal helping'='informalhelping',
 'Volunteering'='volunteering','Academic engagement'='academicengagement','Exercise'='physicalexercise',
 'Physical health'='physicalhealth','Mental health'='mentalhealth','Affective well-being'='affectivewellbeing',
 'Life satisfaction'='satisfactionwithlife')
indices <- match(names(publication_map),as.character(table[5:37,1]))
stopifnot(!anyNA(indices))
published <- matrix(as.numeric(as.matrix(table[5:37,7:39])),33,33)[indices,indices]
dimnames(published) <- list(unname(publication_map),unname(publication_map));diag(published) <- 1
first_cor <- cor(data_matrix[,unname(publication_map)],method='pearson',use='pairwise.complete.obs')
same_wave <- first_cor
wave2_base <- c(names(variable_map)[1:5],'Life_Satisfaction')
wave2_base_names <- c(unname(variable_map[1:5]),'satisfactionwithlife')
wave2_cor <- cor(wave2[,c(wave2_base,names(wave2_map))],method='pearson',use='pairwise.complete.obs')
same_wave[wave2_base_names,unname(wave2_map)] <- wave2_cor[wave2_base,names(wave2_map)]
same_wave[unname(wave2_map),wave2_base_names] <- t(wave2_cor[wave2_base,names(wave2_map)])
reported <- upper.tri(published) & !is.na(published)
delta <- abs(first_cor[reported]-published[reported])
stopifnot(length(delta)==106L,sum(delta>1e-10)==30L,
 sum(is.na(published[upper.tri(published)]))==30L,
 max(abs(same_wave-published),na.rm=TRUE)<1e-10)

# 7-8. Apply the already agreed binary contrast: intermediate source gender
# codes are missing only for sex correlations. All 470 people remain included.
note <- paste('All 253 historical correlations reproduced. Retain 470 baseline participants; five outcomes first measured at wave2 are linked one-to-one, with no latest-wave matrix or participant exclusions.',
 'Binary coding convention: 11 intermediate gender3 codes become missing rather than .5; two pre-existing missing codes remain missing, so sex is observed for 457 people. Other variables unchanged. Pair Ns=401-470.',
 'Table S2: 76 reported comparisons match our first-observation timing; another 30 reproduce exactly using concurrent wave2 BESSI/life satisfaction. Thirty additional linked cross-wave correlations are not reported in that supplement. Preserve baseline BESSI/life satisfaction, documenting staggered outcome timing.',
 'Main PDF p.5 confirms alternating odd/even outcome batteries and the source effect-coded gender contrast; use the project binary-sex convention without changing other variables. Volunteering is the source standardized three-item composite, not raw hours.',
 'Supplied scores only: original item scoring not independently reconstructed. Public OSF data checksum equals the local original. Main PDF supplied by the user during the 2026-11-14 audit; original download date unknown.')
audit_0108 <- list(historical=audit_compare_matrix('0108','0108a',data_scored,470,note,
 names(source_hashes),'OSF x3zjw official Table S2, including its timing footnote; source gender variables',
 status='LEGACY_BINARY_CODING_RECONSTRUCTED'))
audit_0108$binary <- audit_compare_matrix('0108','0108a',data_matrix,470,note,
 names(source_hashes),'Agreed extraction conventions: binary sex contrast; Table S2',
 status='BINARY_CODING_CONVENTION_CORRECTION',comparison='binary_sex_correction')
for(which in names(audit_0108)) {
 audit_0108[[which]]$summary$published_pairs_checked <- length(delta)
 audit_0108[[which]]$summary$published_pairs_outside_rounding <- sum(delta>.0050001)
 audit_0108[[which]]$summary$numerically_compatible_pairs <- sum(delta<=1e-10)
}
summary_0108 <- dplyr::bind_rows(lapply(audit_0108,`[[`,'summary'))
print(summary_0108[,c('matrix_id','comparison','changed_pairs','pair_n_min','pair_n_max')])
if(!'--check-only' %in% args) {
 source('R/codebook_workflow.R',local=TRUE);source('R/extraction_helpers.R',local=TRUE)
 source('R/workflow_io.R',local=TRUE);source('R/workflow_results.R',local=TRUE)
 old <- readr::read_csv('data/3.meta_data/extraction/historical_audit.csv',show_col_types=FALSE)
 first <- audit_0108$binary$summary
 first$comparison <- 'approved_integration_2026_11_14';first$status <- 'REVIEWED_BEFORE_AFTER'
 if(any(old$matrix_id=='0108a' & old$comparison==first$comparison))first <- first[FALSE,]
 audit_save_summary(dplyr::bind_rows(summary_0108,first))
 individual_data <- cbind(data_matrix,age=data_analytic$birthday_age,
  gender3_original=as.numeric(data_analytic$gender3),data_analytic[,c('gender_Male','gender_Female','gender_Trans','gender_Non_Binary','gender_Other')])
 checks <- extraction_check_matrix(data_matrix,audit_0108$binary$candidate,audit_0108$binary$pair_n)
 wf_compact_extraction('0108','0108a',individual_data,audit_0108$binary$candidate,audit_0108$binary$pair_n,checks)
}
