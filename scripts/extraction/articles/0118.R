# 0118 — Cravet & Usai (2026), Using board games as a protective tool:
# Effects on cognitive, social, emotional, behavioural outcomes, and learning.
# User-approved CPM/sex corrections exported on a normal run;
# --check-only writes nothing.
source('R/historical_audit.R')
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% '--check-only'))
source_hashes <- c(
 'data/3.meta_data/open_data/d0118.xlsx'='d945592b82dc38604458ff917de283d467742306687eaf1acad23e9d76b72203',
 'data/2.full_abstracting/pdfs/0118.pdf'='8aba9747122bc3d7acf023186eb39f33be0de7bff87a0e0140afacfb8aff5feb',
 'data/3.meta_data/open_data/raw/0118/Supplementary Materials - Correlation Tables.xlsx'='9dd134e4f9ad9c8985bc1310ed42b5479c527b597b8842de080b2ab3b757c85e')
audit_verify_sources(source_hashes)

# 1-3. Baseline, both study groups; all 150 unique children retained. The source
# ID prefixes identify 83 IG and 67 CG records, matching Methods p.4.
raw <- as.data.frame(readxl::read_excel(names(source_hashes)[1]))
group <- substr(raw$ID,1,2)
stopifnot(nrow(raw)==150L,!anyDuplicated(raw$ID),sum(group=='IG')==83L,
 sum(group=='CG')==67L,all(raw$gender_0F1M %in% 0:1))
data_analytic <- raw

# 4-5. Five BESSI means independently reconstructed; released scores were
# rounded to THREE decimal places. Preserve that documented release precision.
positions <- c(selfmanagement=1,innovation=5,cooperation=3,socialengagement=2,emotionalresilience=4)
provided <- c('BESSI_SELFMANAGEMENT_pre','BESSI_INNOVATION_pre','BESSI_COOPERATION_pre',
 'BESSI_SOCIALENG_pre','BESSI_EMORES_pre')
bessi_means <- data.frame(row.names=seq_len(nrow(raw)))
for(i in seq_along(positions)) {
 values <- as.matrix(raw[,paste0('BESSI',seq(positions[[i]],45,5))])
 stopifnot(all(is.na(values) | values>=1 & values<=5))
 value <- rowMeans(values,na.rm=TRUE);value[is.nan(value)] <- NA_real_
 bessi_means[[names(positions)[i]]] <- value
 stopifnot(isTRUE(all.equal(round(value,3),raw[[provided[i]]],check.attributes=FALSE)))
}
variable_map <- c(CPM_sum_TOT='intelligencefluid',TELEFE_inhibition_pre='inhibition',
 TELEFE_cong_pre='flexibility',TELEFE_incong_pre='interferencecontrol',
 BRIEF_BRI_pre='brief_behavioralregulation',BRIEF_ERI_pre='brief_emotionregulation',
 BRIEF_CRI_pre='brief_cognitiveregulation',BRIEF_GEC_pre='brief_generalexecutivefunctioning',
 BESSI_SELFMANAGEMENT_pre='selfmanagement',BESSI_INNOVATION_pre='innovation',
 BESSI_COOPERATION_pre='cooperation',BESSI_SOCIALENG_pre='socialengagement',BESSI_EMORES_pre='emotionalresilience')
data_scored <- raw[,names(variable_map)];names(data_scored) <- unname(variable_map)
data_scored$sex <- 1-raw$gender_0F1M # Historical reading of the field name, diagnostic only.
data_scored$workingmemory <- rowMeans(raw[,c('TELEFE_lowload_pre','TELEFE_highload_pre')],na.rm=TRUE)
data_scored$academicachievement <- rowMeans(raw[,c('ita_pre','mat_pre')],na.rm=TRUE)
for(v in names(data_scored))data_scored[[v]][is.nan(data_scored[[v]])] <- NA_real_
data_matrix <- data_scored

# 6. BRIEF indices reproduce rounded sums of the supplied subscale means;
# their proprietary item key and TeleFE trial-level responses are unavailable.
# Retain source indices/directions: higher BRIEF means MORE difficulties.
brief_components <- list(BRIEF_BRI_pre=c('BRIEF_INHIB_pre','BRIEF_SELFTASKMONITOR_pre'),
 BRIEF_ERI_pre=c('BRIEF_SHIFT_pre','BRIEF_EMOCONTROL_pre'),
 BRIEF_CRI_pre=c('BRIEF_INITIATE_pre','BRIEF_WM_pre','BRIEF_PLAN_pre','BRIEF_TASKMONITOR_pre','BRIEF_ORG_pre'))
for(index in names(brief_components))stopifnot(isTRUE(all.equal(
 round(rowSums(raw[,brief_components[[index]]])),raw[[index]],check.attributes=FALSE)))
stopifnot(isTRUE(all.equal(rowSums(raw[,names(brief_components)]),raw$BRIEF_GEC_pre,check.attributes=FALSE)))
cpm_items <- as.matrix(raw[,paste0('CPM',1:36)])
stopifnot(!anyNA(cpm_items),all(cpm_items %in% 0:1))
cpm_sum <- rowSums(cpm_items)
cpm_mismatches <- sum(cpm_sum!=raw$CPM_sum_TOT)
stopifnot(cpm_mismatches==4L,max(abs(cpm_sum-raw$CPM_sum_TOT))==1)
# Methods p.4: F=46 IG and F=27 CG, EXACTLY the source value 1 in both
# groups. This contradicts the field's 0F1M label; user decision 2026-11-14:
# follow the paper (1 female), keeping the contradiction documented.
stopifnot(sum(raw$gender_0F1M[group=='IG']==1)==46L,
 sum(raw$gender_0F1M[group=='CG']==1)==27L)

# Published tables report groups separately. Reproduce 136 baseline references
# per group (17 variables, including separate grades and age for validation).
publication_map <- c('Age in Months'='age_months','Fluid Reasoning'='CPM_sum_TOT',
 'Mathematics grades T1'='mat_pre','Italian grades T1'='ita_pre','WM T1'='workingmemory',
 'Inhibition T1'='TELEFE_inhibition_pre','Flexibility T1'='TELEFE_cong_pre',
 'Interference Control T1'='TELEFE_incong_pre','BRI T1'='BRIEF_BRI_pre',
 'ERI T1'='BRIEF_ERI_pre','CRI T1'='BRIEF_CRI_pre','GEC T1'='BRIEF_GEC_pre',
 'Self-Management T1'='BESSI_SELFMANAGEMENT_pre','Innovation T1'='BESSI_INNOVATION_pre',
 'Cooperation T1'='BESSI_COOPERATION_pre','Emotional Resilience T1'='BESSI_EMORES_pre',
 'Social Engagement T1'='BESSI_SOCIALENG_pre')
publication_data <- raw;publication_data$workingmemory <- data_scored$workingmemory
published_checks <- list()
for(which in c('IG','CG')) {
 table <- suppressMessages(as.data.frame(readxl::read_excel(names(source_hashes)[3],
  sheet=paste0(which,'_Correlations'),col_names=FALSE)))
 indices <- match(names(publication_map),trimws(as.character(table[[2]][3:48])))
 stopifnot(!anyNA(indices))
 values <- gsub('[^0-9.eE+-]','',as.matrix(table[3:48,3:48]))
 reference <- matrix(suppressWarnings(as.numeric(values)),46,46)
 reference[upper.tri(reference)] <- t(reference)[upper.tri(reference)];diag(reference) <- 1
 reference <- reference[indices,indices]
 r <- cor(publication_data[group==which,unname(publication_map)],method='pearson',use='pairwise.complete.obs')
 delta <- abs(r[upper.tri(r)]-reference[upper.tri(reference)])
 stopifnot(length(delta)==136L,all(delta<=.0050001))
 published_checks[[which]] <- delta
}

# 7-8. Isolate both corrections and export only the approved combination.
note <- paste('All 120 historical correlations reproduced, N=150, pair Ns=144-150. All five BESSI-45 means reconstructed to released three-decimal precision; BRIEF supplied-index arithmetic verified, not proprietary item scoring. TeleFE task mappings follow Methods p.4; two-load working-memory mean reproduces the supplement.',
 'All 272 selected published baseline coefficients reproduce within rounding, separately for the explicit CG/IG source groups. Supplied CPM total differs from 36 complete binary item sums in four cases, by one point each.',
 'Gender field name says 0 female/1 male, but the paper\'s 46 IG/27 CG females correspond exactly to source value1 in both groups. User decision 2026-11-14: follow paper-supported 1 female/0 male; retain the reported 48.7% female and document the label conflict.',
 'User approval 2026-11-14: recalculate the four discrepant CPM totals from all 36 binary items. The other 146 provided totals equal the item sums exactly. Retain all 150 participants and all item responses. Publication replication uses original supplied CPM totals; corrected intelligence correlations need not equal those references.')
audit_0118 <- list(historical=audit_compare_matrix('0118','0118a',data_matrix,150,note,
 names(source_hashes),'0118 Methods p.4; official OSF IG/CG supplementary correlation tables',
 status='LEGACY_VALUES_RECONSTRUCTED'))
cpm_corrected <- data_matrix;cpm_corrected$intelligencefluid <- cpm_sum
sex_corrected <- data_matrix;sex_corrected$sex <- raw$gender_0F1M
combined <- cpm_corrected;combined$sex <- raw$gender_0F1M
audit_0118$cpm <- audit_compare_matrix('0118','0118a',cpm_corrected,150,note,names(source_hashes),
 'User-approved four supplied CPM aggregation corrections; no missing items',status='APPROVED_CPM_CORRECTION',comparison='cpm_item_sum_correction')
audit_0118$sex <- audit_compare_matrix('0118','0118a',sex_corrected,150,note,names(source_hashes),
 'User follows paper Methods p.4: 46 IG/27 CG females, matching raw code1',status='APPROVED_SEX_CORRECTION',comparison='paper_supported_sex_correction')
audit_0118$combined <- audit_compare_matrix('0118','0118a',combined,150,note,names(source_hashes),
 'User-approved CPM and paper-supported sex corrections',status='APPROVED_EXTRACTION',comparison='cpm_and_sex_correction')
audit_0118$historical$summary$published_pairs_checked <- sum(lengths(published_checks))
audit_0118$historical$summary$published_pairs_outside_rounding <- 0L
summary_0118 <- dplyr::bind_rows(lapply(audit_0118,`[[`,'summary'))
print(summary_0118[,c('matrix_id','comparison','changed_pairs','pair_n_min','pair_n_max')])
if(!'--check-only' %in% args) {
 source('R/codebook_workflow.R',local=TRUE);source('R/extraction_helpers.R',local=TRUE)
 source('R/workflow_io.R',local=TRUE);source('R/workflow_results.R',local=TRUE)
 old <- readr::read_csv('data/3.meta_data/extraction/historical_audit.csv',show_col_types=FALSE)
 first <- audit_0118$combined$summary
 first$comparison <- 'approved_integration_2026_11_14';first$status <- 'REVIEWED_BEFORE_AFTER'
 if(any(old$matrix_id=='0118a' & old$comparison==first$comparison))first <- first[FALSE,]
 audit_save_summary(dplyr::bind_rows(summary_0118,first))
 individual_data <- cbind(combined,age=raw$age_months/12,group_original=group,
  gender_code_original=raw$gender_0F1M,intelligencefluid_original=raw$CPM_sum_TOT)
 checks <- extraction_check_matrix(combined,audit_0118$combined$candidate,audit_0118$combined$pair_n)
 wf_compact_extraction('0118','0118a',individual_data,audit_0118$combined$candidate,audit_0118$combined$pair_n,checks)
}
