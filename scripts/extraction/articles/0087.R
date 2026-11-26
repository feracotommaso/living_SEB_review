# 0087: independent transcription check, NOT raw-data reconstruction.
# User decision 2026-11-14: retain inclusion and N=373 with an explicit caveat.
# Normal run updates only extraction/historical_audit.csv; --check-only writes nothing.
source('R/historical_audit.R')
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% '--check-only'))
source_hashes <- c(
 'data/2.full_abstracting/pdfs/0087.pdf'='2504abb58f1c7469897c9db0947c424c5899e477a5b7240f0a7b0761c9e4cb60')
audit_verify_sources(source_hashes)

# 1-6. Data sharing is prohibited by the ethics approval (PDF p.23).
# Table 2 (p.15): row-by-row lower triangle, in published order. PSEC global
# and specific factors are orthogonal BY MODEL CONSTRAINT; their blank cells
# remain NA as historically transcribed. Never fill them with observed zeros.
variables <- c('teacherautonomysupport','parentautonomysupport','psec_total',
 'perceivedAssertiveness','perceivedTolerance','perceivedSocialregulation',
 'perceivedEmotionregulation','perceivedEmotionalawareness','leadershipskill',
 'culturalcompetence','teamworkskill','cognitivereappraisalskill','emotionalreflectionskill')
published <- c(
 .46,
 .31,.47,
 .25,.05,NA,
 .10,.13,NA,NA,
 .31,.13,NA,NA,NA,
 .14,-.07,NA,NA,NA,NA,
 .08,.12,NA,NA,NA,NA,NA,
 .22,.35,.59,.37,-.05,-.26,-.06,-.15,
 .22,.41,.50,.04,.51,-.11,-.10,-.08,.45,
 .36,.52,.72,.12,.13,.05,-.15,-.15,.73,.71,
 .32,.39,.63,.09,.01,.07,.30,.05,.49,.38,.51,
 .25,.42,.65,-.01,-.03,-.08,.10,.22,.67,.60,.65,.68)
reference <- diag(length(variables))
reference[upper.tri(reference)] <- published
reference[lower.tri(reference)] <- t(reference)[lower.tri(reference)]
dimnames(reference) <- list(variables,variables)
saved <- as.matrix(readxl::read_excel('data/3.meta_data/matrices/0087a.xlsx'))
rownames(saved) <- colnames(saved)
stopifnot(identical(colnames(saved),variables))
values <- saved[upper.tri(saved)]
stopifnot(identical(is.na(values),is.na(published)),
 sum(!is.na(values))==63L,sum(is.na(values))==15L)
changed <- sum(abs(values-published)>1e-10,na.rm=TRUE)
stopifnot(changed==0L)

# 7-8. No individual data, Pearson recalculation or pair N can be recovered.
# The model uses bifactor ESEM for PSEC and error-adjusted means for predictors
# and outcomes: loading=1, residual variance=variance*(1-omega), MLR/FIML.
# N=373 is baseline; 221 return at T2 five weeks later. Neither 373 nor 221 is
# an observed pair N for every model coefficient. Retain historical names for
# parent-rated BESSI facets; suffix harmonisation is a separate operation.
note <- paste('All 63 Table 2 coefficients and 15 blank cells independently verified; this is a published-model transcription check, not raw-score reconstruction.',
 'User decision 2026-11-14: retain quantitative inclusion and historical reference N=373 with this note.',
 'Coefficients are measurement-model correlations: bifactor ESEM PSEC plus error-adjusted means (loading=1, residual variance=variance*(1-omega)) for autonomy support and parent-rated skills; not ordinary observed-score Pearson correlations.',
 'MLR with FIML. Baseline T1 N=373; T2 N=221 five weeks later; no coefficient-specific observed Ns available. Do not assign fabricated pair Ns.',
 'PSEC factors are orthogonal by model constraint; 15 unreported cells remain NA. Ethics approval prohibits individual-data sharing. Model-based and observed-score associations are not assumed equivalent; sensitivity excluding this study remains a future analysis.')
records <- readr::read_csv('data/3.meta_data/coding/records.csv',col_types=readr::cols(.default='c'))
summary_0087 <- data.frame(paper_id='0087',matrix_id='0087a',
 comparison='published_model_transcription',reference_n=373,
 recorded_n=as.numeric(records$n[match('0087a',records$matrix_id)]),
 input_rows=NA_integer_,variables=13L,compared_pairs=63L,changed_pairs=changed,
 max_abs_delta=max(abs(values-published),na.rm=TRUE),
 pair_n_min=NA_integer_,pair_n_max=NA_integer_,added_variables='',removed_variables='',
 status='MODEL_CORRELATIONS_RETAINED_BY_DECISION',note=note,
 evidence='0087 PDF pp.9-10 sample/timing, pp.12-13 model, Table 2 p.15, data availability p.23',
 sources_sha256=paste(names(source_hashes),source_hashes,sep='='),
 published_pairs_checked=63L,published_pairs_outside_rounding=0L)
print(summary_0087[,c('matrix_id','compared_pairs','changed_pairs','status')])
if(!'--check-only' %in% args) audit_save_summary(summary_0087)
