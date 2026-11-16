# 0134 — Pellegrino, Feraco, Meneghetti & Carretti (2026), Social, emotional,
# and behavioral skills as resources for student engagement: A one-year
# longitudinal study in high school students. Retain historical T1 selection.
# Normal run updates the audit only; --check-only writes nothing.
source('R/historical_audit.R')
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% '--check-only'))
source_hashes <- c(
 'data/3.meta_data/open_data/d0134.xlsx'='dfb686cb37761427532d1a62b238f530329f415600d258c813a495aa6193456c',
 'data/3.meta_data/open_data/opencode0134.Rmd'='2be92b3bea02781a8206ab3489c0b69b0d5fe3ef4802425ecd768ffe7d08da19',
 'data/2.full_abstracting/pdfs/0134.pdf'='9608b790980b036eadb082a7370f8477ff4cb97df4789451e92a3e147e75f3d4')
audit_verify_sources(source_hashes)
raw <- as.data.frame(readxl::read_excel(names(source_hashes)[1]))
# Released longitudinal cohort: 1,555 unique participants with >=2 waves,
# after authors' upstream exclusions. Use its 1,330 T1 respondents only.
# The first-item rule happens to equal ANY T1 BESSI response in this release.
t1_items <- paste0('bessi_',1:45,'_1')
at_t1 <- rowSums(!is.na(raw[,t1_items]))>0
stopifnot(nrow(raw)==1555L,!anyDuplicated(raw$Code),sum(at_t1)==1330L,
 identical(at_t1,!is.na(raw$bessi_1_1)))
data_analytic <- raw[at_t1,]
stopifnot(all(data_analytic$Gender %in% 1:2))

# Nine positive BESSI-45 items per domain, interleaved SM, SE, CO, ER, IN.
# Questionnaire scores are available-item means on 1-5. Verify supplied
# scores independently; all selected responses and resulting scores complete.
data_scored <- data.frame(
 selfmanagement=rowMeans(data_analytic[,paste0('bessi_',seq(1,45,5),'_1')],na.rm=TRUE),
 innovation=rowMeans(data_analytic[,paste0('bessi_',seq(5,45,5),'_1')],na.rm=TRUE),
 cooperation=rowMeans(data_analytic[,paste0('bessi_',seq(3,45,5),'_1')],na.rm=TRUE),
 socialengagement=rowMeans(data_analytic[,paste0('bessi_',seq(2,45,5),'_1')],na.rm=TRUE),
 emotionalresilience=rowMeans(data_analytic[,paste0('bessi_',seq(4,45,5),'_1')],na.rm=TRUE))
domain_source <- c('SMD_1','IND_1','COD_1','SED_1','ERD_1')
stopifnot(max(abs(as.matrix(data_scored)-as.matrix(data_analytic[,domain_source])))<1e-12)

# Engagement: affective 9 items (reverse 9); behavioral 12 (reverse 2,4,9);
# cognitive 12; agentic 5. All source response ranges are 1-5. Reverse keys
# agree with the author code; 6 minus each item equals its supplied reverse.
for(item in c('aff_9','beha_2','beha_4','beha_9')) {
 original <- paste0(item,'_1'); reversed <- paste0('rev_',item,'_1')
 stopifnot(all(data_analytic[[reversed]]==6-data_analytic[[original]]))
 data_analytic[[reversed]] <- 6-data_analytic[[original]]
}
affective_items <- c(paste0('aff_',1:8,'_1'),'rev_aff_9_1')
behavioral_items <- c('beha_1_1','rev_beha_2_1','beha_3_1','rev_beha_4_1',
 paste0('beha_',5:8,'_1'),'rev_beha_9_1',paste0('beha_',10:12,'_1'))
data_scored$affectiveengagement <- rowMeans(data_analytic[,affective_items],na.rm=TRUE)
data_scored$behavioralengagement <- rowMeans(data_analytic[,behavioral_items],na.rm=TRUE)
data_scored$cognitiveengagement <- rowMeans(data_analytic[,paste0('cog_',1:12,'_1')],na.rm=TRUE)
data_scored$agenticengagement <- rowMeans(data_analytic[,paste0('agen_',1:5,'_1')],na.rm=TRUE)
stopifnot(max(abs(as.matrix(data_scored[,6:9])-as.matrix(data_analytic[,c('AFF_ENG_1','BEH_ENG_1','COG_ENG_1','AG_ENG_1')])))<1e-12)
data_scored$sex <- as.numeric(data_analytic$Gender)-1 # source 1=male, 2=female
data_matrix <- data_scored
stopifnot(!anyNA(data_matrix),all(unlist(data_matrix[,1:9])>=1),all(unlist(data_matrix[,1:9])<=5))

# Dependence diagnostic, NOT an exclusion or linkage by anonymized Code.
# The paper p.3 explicitly says T3 reused the 0068 cohort. Independent exact
# profiles across ALL 45 T3 items uniquely match 273 complete 0068 profiles;
# 238 also have T1 data in this matrix. Sex agrees in every matched profile.
# No person-level matching table is exported. Cohort age fields disagree and
# are not used to infer identity; published overlap + unique profiles support
# dependence, not an assertion that every unmatchable record is independent.
comparison_path <- 'data/3.meta_data/open_data/d0068.xlsx'
previous <- as.data.frame(readxl::read_excel(comparison_path))
profile <- function(z) apply(z,1,paste,collapse='|')
old_profile <- profile(previous[,paste0('bessi_',1:45)])
t3_profile <- profile(raw[,paste0('bessi_',1:45,'_3')])
old_frequency <- table(old_profile);new_frequency <- table(t3_profile)
unique_common <- intersect(names(old_frequency)[old_frequency==1],names(new_frequency)[new_frequency==1])
old_index <- match(unique_common,old_profile);new_index <- match(unique_common,t3_profile)
stopifnot(length(unique_common)==273L,sum(at_t1[new_index])==238L,
 all(previous$Gender[old_index]==raw$Gender[new_index]))
note <- paste('All 45 historical T1 correlations reproduced from raw items; all five nine-item BESSI domains and four engagement means match the supplied totals. Four reverse items independently verified.',
 'Released longitudinal cohort has 1555 participants; retain all 1330 with T1 responses, all pair Ns=1330, including 755 women and 575 men. No new exclusion or later-wave pooling; upstream author selection is not reconstructed.',
 'DEPENDENCE: PDF p.3 explicitly identifies reuse of T3 data published as 0068. Unique exact 45-item profiles identify 273 matches with 0068, of whom 238 are present at T1 here, with concordant sex. Anonymized Code equality is not an identity key. Age fields disagree and are not silently reconciled. These numbers are verified overlap evidence, not a full estimate of all shared participants. Both existing matrices retained pending discussion.')
audit_0134 <- audit_compare_matrix('0134','0134a',data_matrix,1330,note,
 c(names(source_hashes),comparison_path),'0134 PDF p.3 cohort overlap; measures and public code item lists',
 status='REPRODUCED_WITH_KNOWN_COHORT_DEPENDENCE')
stopifnot(audit_0134$summary$changed_pairs==0L,audit_0134$summary$pair_n_min==1330L)
summary_0134 <- audit_0134$summary
print(summary_0134[,c('matrix_id','input_rows','compared_pairs','changed_pairs','pair_n_min','status')])
if(!'--check-only' %in% args) {
 audit_save_summary(summary_0134)
 source('R/codebook_workflow.R',local=TRUE);source('R/extraction_helpers.R',local=TRUE)
 source('R/workflow_io.R',local=TRUE);source('R/workflow_results.R',local=TRUE)
 individual_data <- cbind(data_matrix,age=data_analytic$age)
 checks <- extraction_check_matrix(data_matrix,audit_0134$candidate,audit_0134$pair_n)
 wf_compact_extraction('0134','0134a',individual_data,audit_0134$candidate,audit_0134$pair_n,checks)
}
