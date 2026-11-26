# 0141 — Sharabi (2026), Understanding Academic Adjustment in Undergraduates
# With SLD or ADHD: An Integrative Model of Social-Emotional-Behavioral Skills,
# Academic Self-Efficacy, and Self-Regulated Learning.
# Author-supplied SPSS matrix check, NOT individual-data reconstruction.
# Normal run updates the audit only; --check-only writes nothing.
source('R/historical_audit.R')
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% '--check-only'))
source_hashes <- c(
 'data/3.meta_data/open_data/d0141a_Matrices.xlsx'='2858dc6bd67c3b23f2443c54a120db89e09a3f4c66929544bd0b4e0e57061935',
 'data/2.full_abstracting/pdfs/0141.pdf'='c0972d1a93c7fc1d3017be522fd8579c73b6b6a82e3864b5b08abd70f78e3cef')
audit_verify_sources(source_hashes)

# SEX_TOTAL_525: each variable has three SPSS rows: Pearson, p, N.
# The separate Total Sample sheet omits sex; its shared coefficients agree.
# The BESSI global is a duplicate of its domains and stays excluded.
raw <- as.data.frame(readxl::read_excel(names(source_hashes)[1],sheet='SEX_TOTAL_525',col_names=FALSE,.name_repair='minimal'))
stopifnot(identical(as.character(unlist(raw[2,3:13],use.names=FALSE)),
 c('3. Sex','SELFMANEG','RESILIENCE','INNOVATION','SOCIALENGAGEMENT','COOPERATION','BESSI','ASE','AS','SRL','grade_end')))
variables <- c('sex','selfmanagement','emotionalresilience','innovation','socialengagement',
 'cooperation','bessiglobal','academicselfefficacy','academicsatisfaction','srlstrategies','academicachievement')
reference <- matrix(as.numeric(as.matrix(raw[3+3*(0:10),3:13])),11,11,
 dimnames=list(variables,variables))
source_pair_n <- matrix(as.numeric(as.matrix(raw[5+3*(0:10),3:13])),11,11)
stopifnot(all(source_pair_n==525),max(abs(reference-t(reference)))<1e-12)
without_sex <- as.data.frame(readxl::read_excel(names(source_hashes)[1],sheet='Total Sample (N = 525)',col_names=FALSE,.name_repair='minimal'))
reference_without_sex <- matrix(as.numeric(as.matrix(without_sex[3+3*(0:9),3:12])),10,10)
stopifnot(max(abs(reference[-1,-1]-reference_without_sex))<1e-12)

# Reproduce historical extraction exactly, making the unexplained operation
# visible: it negated all nine sex correlations. User checked the original
# author emails and explicitly confirmed the saved direction on 2026-11-15.
# The email itself is not in the repository; do not claim to have read it.
reference['sex',-1] <- -reference['sex',-1]
reference[-1,'sex'] <- -reference[-1,'sex']
saved <- as.matrix(readxl::read_excel('data/3.meta_data/matrices/0141a.xlsx'))
rownames(saved) <- colnames(saved)
reference <- reference[colnames(saved),colnames(saved)]
delta <- abs(saved-reference)
stopifnot(ncol(saved)==10L,max(delta)<1e-12)
note <- paste('All 45 historical coefficients reproduced from author-supplied SEX_TOTAL_525, including the historical reversal of nine sex coefficients. All reported pair Ns in that sheet are 525; non-sex values agree with Total Sample (N=525).',
 'SEX DIRECTION CONFIRMED BY USER 2026-11-15 after checking original author emails: retain historical direction of all nine sex correlations. Email correspondence itself is not deposited or independently inspected. Workbook only labels 3. Sex. Main paper reports 475 female/176 male of 651 (73.0%) and uses sex as a covariate, but has no sex-stratified outcome means or sex coefficients to identify direction independently; no key inferred from expected group differences.',
 'The provided reduced matrix has 525 participants (318 SLD/ADHD and 207 comparison); paper has 651 (318/333). Historical note says 126 overlapping 0080 participants were removed; no raw IDs are available to verify their removal or derive reduced-cohort descriptives. Published full-cohort age/female descriptives are not independently validated for the reduced sample.',
 'Academic satisfaction is the Huebner school-satisfaction measure; SRL is the 20-item strategies score. BESSI20 means and SRL reversals cannot be reconstructed without individuals. Global BESSI and split-group matrices are not pooled or added. Some split-group tabs differ from each other; they do not replace the concordant total-sample sheets.')
records <- readr::read_csv('data/3.meta_data/coding/records.csv',col_types=readr::cols(.default='c'))
summary_0141 <- data.frame(paper_id='0141',matrix_id='0141a',comparison='author_reduced_matrix_transcription',
 reference_n=525,recorded_n=as.numeric(records$n[match('0141a',records$matrix_id)]),
 input_rows=NA_integer_,variables=10L,compared_pairs=45L,changed_pairs=0L,
 max_abs_delta=max(delta),pair_n_min=525L,pair_n_max=525L,
 added_variables='',removed_variables='',status='REPRODUCED_SEX_CONFIRMED_REDUCED_DESCRIPTIVES_UNVERIFIED',note=note,
 evidence='0141 author workbook total-sample sheets; PDF pp.3-4 participants and measures; original overlap note in coding',
 sources_sha256=paste(names(source_hashes),source_hashes,sep='=',collapse=';'),
 published_pairs_checked=NA_integer_,published_pairs_outside_rounding=NA_integer_)
print(summary_0141[,c('matrix_id','compared_pairs','changed_pairs','pair_n_min','status')])
if(!'--check-only' %in% args) audit_save_summary(summary_0141)
