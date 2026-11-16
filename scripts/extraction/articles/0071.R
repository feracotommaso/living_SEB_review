# 0071: historical observed-score verification, not a new factor-score analysis.
# Normal run updates only extraction/historical_audit.csv; --check-only writes nothing.
source('R/historical_audit.R')
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% '--check-only'))
source_hashes <- c(
 'data/3.meta_data/open_data/d0071.xlsx'='f045ec84820161531d55b493d09b630deab1d7e275802b6e1050cc2a42342ca5',
 'data/3.meta_data/open_data/opencode0071.R'='7fcdffeada614f3433f6c3424ec6ee0a5b2d87836d405c7893d1d883a8696974',
 'data/2.full_abstracting/pdfs/0071.pdf'='e55a20f6303a76caaa95f77317291c21fda9b8eaffdeeec4f0342ca73c14651d')
audit_verify_sources(source_hashes)

# 1-3. The release already excludes 487 careless BESSI respondents (paper p.7).
# All 5,075 remaining unique students are retained. Missing/careless responses
# to other questionnaires are already masked at the questionnaire level.
raw <- as.data.frame(readxl::read_excel(names(source_hashes)[1]))
stopifnot(nrow(raw)==5075L, !anyDuplicated(raw$ID),
 all(raw$Gender %in% c('Male','Female')))
data_analytic <- raw

# 4-5. Positive-keyed BESSI-45: nine items per domain, available-item means.
# Other scale means require all their items, preserving the manual extraction.
# The AMOS/QAS/QC items are supplied already keyed for analysis: the inspected
# author code applies no additional reversal. Original item wording/key is not
# present in the release, so this is not an independent reverse-key validation.
data_scored <- data.frame(sex=ifelse(raw$Gender=='Female',1,0))
positions <- c(selfmanagement=1,innovation=5,cooperation=3,
 socialengagement=2,emotionalresilience=4)
for(domain in names(positions)) {
 items <- as.matrix(raw[,paste0('bessi_',seq(positions[[domain]],45,5))])
 stopifnot(all(is.na(items) | (items>=1 & items<=5)))
 value <- rowMeans(items,na.rm=TRUE); value[is.nan(value)] <- NA_real_
 data_scored[[domain]] <- value
}
scale_items <- list(
 masterygoals=paste0('qc_',5:8),
 academicselfefficacy=paste0('qc_',9:12),
 positiveachievementemotions=paste0('qe_',seq(2,14,2)),
 negativeachievementemotions=paste0('qe_',seq(1,13,2)),
 growthmindset=paste0('qc_',1:4),
 satisfactionwithlife=paste0('swls_',1:5),
 peeracceptance=paste0('acc_',1:12),
 srlstrategies=paste0('qas_',1:16))
for(variable in names(scale_items))
 data_scored[[variable]] <- rowMeans(raw[,scale_items[[variable]]])
data_scored$academicachievement <- raw$media
data_scored$age <- raw$Age

# 6. carSl identifies peer-acceptance carelessness, not life satisfaction:
# 136 flagged students have masked ACC but valid SWLS. Do not mask SWLS again.
stopifnot(sum(raw$carSl=='TRUE')==136L,
 all(is.na(data_scored$peeracceptance[raw$carSl=='TRUE'])),
 all(!is.na(data_scored$satisfactionwithlife[raw$carSl=='TRUE'])),
 sum(is.na(data_scored$peeracceptance))==366L,
 sum(is.na(data_scored$satisfactionwithlife))==230L)

# 7-8. Table 2 uses ordinal-CFA factor scores (Methods pp.7-8; lavPredict in
# author code), whereas our historical matrix uses observed item means.
# Numerical differences from that table are not evidence of a scoring error.
data_matrix <- data_scored[,setdiff(names(data_scored),'age')]
note <- paste('All 105 historical observed-score correlations reproduced; N=5075, pair Ns=4564-5075. All students retained; questionnaire-level missing/careless masks preserved.',
 'BESSI-45 available-item means and complete-item means for the other scales. Supplied AMOS items already keyed according to inspected author analysis; original item wording/reversal key unavailable.',
 'carSl concerns peer acceptance, not SWLS; 136 flagged students retain valid SWLS.',
 'Published Table 2 uses ordinal-CFA factor scores, not these observed item means; no factor-score substitution. Age excluded from correlations.')
audit_0071 <- audit_compare_matrix('0071','0071a',data_matrix,5075,note,
 names(source_hashes),'0071 Methods pp.7-12, Table 2; original author analysis code',
 status='HISTORICAL_VALUES_REPRODUCED')
cor_matrix <- audit_0071$candidate
pair_n <- audit_0071$pair_n
stopifnot(audit_0071$summary$changed_pairs==0L)
print(audit_0071$summary[,c('matrix_id','compared_pairs','changed_pairs','pair_n_min','pair_n_max')])
if(!'--check-only' %in% args) audit_save_summary(audit_0071$summary)
