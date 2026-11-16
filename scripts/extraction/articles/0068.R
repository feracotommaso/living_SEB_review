# 0068: retain all 356 people; rebuild mean grades and standardize by school
# year, as approved 2026-11-14. Preserve source/published comparisons separately.
# --check-only never writes. Original data: OSF 8thsg; retrieval date unknown.
source('R/historical_audit.R')
source('scripts/extraction/keys/0068.R',local=TRUE)
args<-commandArgs(trailingOnly=TRUE)
stopifnot(all(args%in%'--check-only'))
source_hashes<-c(
 'data/3.meta_data/open_data/d0068.xlsx'='12245c57db9ce7230189ad77ddbf529fd61067b2447f7b893b0ffb057c58fd0b',
 'data/3.meta_data/open_data/opencode0068.R'='d11f67f180c57eb7d7a0eaa2a32781bcac7251f378ba5e30ff2aaf0efe8fb653',
 'data/2.full_abstracting/pdfs/0068.pdf'='2bd8e04ab53f45b580ffd436ca404040b4706a8123f24b0489a1532bdd2df3ca')
audit_verify_sources(source_hashes)

# 1-3. The original paper/code removes six non-boy/girl respondents (N=350).
# User retains all 356. Code their sex as missing for the binary association;
# keep their questionnaire/grade information and update the reference N.
raw<-as.data.frame(readxl::read_excel(names(source_hashes)[1]))
stopifnot(nrow(raw)==356L,all(raw$Gender%in%1:4),all(raw$Grade%in%c(4,5)))
data_scored<-raw[,names(source_map)];names(data_scored)<-unname(source_map)
data_scored$sex<-ifelse(raw$Gender==1,0,ifelse(raw$Gender==2,1,NA_real_))
stopifnot(sum(is.na(data_scored$sex))==6L)

# 4. BESSI-45 and the other questionnaire totals independently reproduce.
positions<-c(selfmanagement=1,innovation=5,cooperation=3,socialengagement=2,emotionalresilience=4)
for(domain in names(positions)) {
 items<-as.matrix(raw[,paste0('bessi_',seq(positions[[domain]],45,5))])
 stopifnot(all(is.na(items) | (items>=1 & items<=5)))
 value<-rowMeans(items,na.rm=TRUE);value[is.nan(value)]<-NA_real_
 stopifnot(isTRUE(all.equal(value,data_scored[[domain]],check.attributes=FALSE,tolerance=1e-12)))
}
stopifnot(isTRUE(all.equal(rowMeans(raw[,paste0('eff_',1:5)]),raw$Efficacy,tolerance=1e-12)),
 isTRUE(all.equal(rowMeans(raw[,paste0('career_',1:24)]),raw$adapt,tolerance=1e-12)))
source_note<-paste('Historical matrix uses all 356 people although the recorded/paper N was 350; binary sex has six missing values.',
 'BESSI means, efficacy and adaptability reproduce. Source grade aggregate has two inconsistencies: 11 versus item mean 10, and 9.2 versus item mean 9.',
 'Author script standardizes achievement within Grade after selecting 350 people. Source comparison does not approve these errors or exclusions.')
audit_0068<-list(source=audit_compare_matrix('0068','0068a',data_scored,350,
 source_note,names(source_hashes),'0068 PDF pp.4-6 and author code',
 status='HISTORICAL_SCORING_COMPARISON'))

# Publication replication: source scores, original 350-person selection and
# within-year achievement standardization reproduce all 55 Table 2 values.
# This distinct reference must not override the approved 356-person correction.
published<-matrix(NA_real_,11,11,dimnames=list(table2_variables,table2_variables))
diag(published)<-1
for(i in 2:11){published[i,1:(i-1)]<-table2_lower[[i]];published[1:(i-1),i]<-table2_lower[[i]]}
paper_data<-raw[raw$Gender%in%c(1,2),]
paper_data$achievement<-ave(paper_data$achievement,paper_data$Grade,FUN=function(x)as.numeric(scale(x)))
paper_cor<-cor(paper_data[,table2_variables])
delta<-abs(paper_cor-published)[upper.tri(published)]
publication<-audit_0068$source$summary
publication$comparison<-'published_table2_replication'
publication$status<-'PUBLICATION_VALUES_REPRODUCED'
publication$published_pairs_checked<-length(delta)
publication$published_pairs_outside_rounding<-sum(delta>.0050001)
publication$max_abs_delta<-max(delta)
publication$input_rows<-nrow(paper_data)
publication$variables<-11L;publication$compared_pairs<-55L
publication$pair_n_min<-350L;publication$pair_n_max<-350L
publication$note<-'Publication-only replication: original 350-person selection, supplied grade aggregates and achievement standardized separately by school year. Not the corrected quantitative export.'
stopifnot(publication$published_pairs_outside_rounding==0L)

# 5-7. User-approved correction: compute from the five actual grades, then
# z-score within fourth/fifth year over all retained people. Preserve raw means
# and school year privately so this transformation remains directly inspectable.
grade_items<-as.matrix(raw[,c('ita','mate','engl','scie','hist')])
stopifnot(!anyNA(grade_items),all(grade_items>=1 & grade_items<=10))
mean_grade<-rowMeans(grade_items)
grade_mismatches<-sum(abs(mean_grade-raw$achievement)>1e-10)
stopifnot(grade_mismatches==2L)
data_scored$academicachievement<-ave(mean_grade,raw$Grade,FUN=function(x)as.numeric(scale(x)))
data_matrix<-data_scored
approved_note<-paste('User-approved 2026-11-14: retain every one of 356 participants and set reference N=356; six non-boy/girl codes remain missing only for binary sex.',
 'Recalculate mean achievement from the five subject grades, correcting two source aggregates (11 -> 10; 9.2 -> 9), then standardize it separately within fourth/fifth year over all 356 retained people, following the author within-year analysis convention.',
 'BESSI-45, efficacy and adaptability scoring independently verified; unchanged supplied questionnaire scores retained. Female share refers to all retained people (207/356 = 58.15%); mean age 16.65. Raw grade means, school year and age kept in private individual data. Source/published comparisons remain separate.')
audit_0068$approved<-audit_compare_matrix('0068','0068a',data_matrix,356,approved_note,
 names(source_hashes),'User decisions 2026-11-14; original items; author Grade standardization',
 status='APPROVED_EXTRACTION',comparison='approved_extraction')
summary_0068<-dplyr::bind_rows(audit_0068$source$summary,publication,audit_0068$approved$summary)
print(summary_0068[,c('matrix_id','comparison','reference_n','input_rows','changed_pairs','pair_n_min','pair_n_max')])

# 8. Compact export; final coding/metadata integration uses workflow.R.
if(!'--check-only'%in%args) {
 source('R/codebook_workflow.R',local=TRUE);source('R/extraction_helpers.R',local=TRUE)
 source('R/workflow_io.R',local=TRUE);source('R/workflow_results.R',local=TRUE)
 old<-readr::read_csv('data/3.meta_data/extraction/historical_audit.csv',show_col_types=FALSE)
 first<-audit_0068$approved$summary
 first$comparison<-'approved_integration_2026_11_14';first$status<-'REVIEWED_BEFORE_AFTER'
 if(any(old$matrix_id=='0068a' & old$comparison==first$comparison))first<-first[FALSE,]
 audit_save_summary(dplyr::bind_rows(summary_0068,first))
 individual_data<-cbind(data_matrix,age=raw$age,school_year=raw$Grade,academicachievement_raw=mean_grade)
 checks<-extraction_check_matrix(data_matrix,audit_0068$approved$candidate,audit_0068$approved$pair_n)
 wf_compact_extraction('0068','0068a',individual_data,audit_0068$approved$candidate,audit_0068$approved$pair_n,checks)
}
