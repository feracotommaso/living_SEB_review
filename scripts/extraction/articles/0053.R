# 0053: verify the five historical full-inventory samples, without promotion.
# Source: Soto et al., doi:10.1037/pspp0000401, OSF 4zgyr. Original dates unknown.
# A normal run updates only historical_audit.csv; --check-only never writes.
source('R/historical_audit.R')
source('scripts/extraction/keys/bessi192.R',local=TRUE)
source('scripts/extraction/keys/0053.R',local=TRUE)
args<-commandArgs(trailingOnly=TRUE)
stopifnot(all(args%in%'--check-only'))
source_hashes<-c(
 'data/3.meta_data/open_data/d0053c.xlsx'='1a978309f2e41ae8a17715910af117402c5d2cd273f05f86a9ce8bd44fafb2bd',
 'data/3.meta_data/open_data/d0053d.xlsx'='1b27b3f70825312187eeee3bfe9f6280da041ad173fa033bfc98fe7a9c8c6c95',
 'data/3.meta_data/open_data/d0053e.xlsx'='1424d3ccdbc6f1b8dd63bce66b4fb6c12e2525ff5c220b6b14383ad15391ff93',
 'data/3.meta_data/open_data/d0053f.xlsx'='c59e4bb7d16c2c4b32ea70c9fdc69595e87add88bc108ceb4ad9e301e1bad116',
 'data/3.meta_data/open_data/d0053g.xlsx'='133a0f5a73fd354307138bfa16f64ea653963588d3e45bce801d21e7d7dd46e9',
 'data/2.full_abstracting/pdfs/0053.pdf'='9b0249b97039c9b0555df1f087a339c8f768091b2e61a55f0e2f68026261de07',
 'data/3.meta_data/open_data/raw/shared/bessi_scoring_v1.pdf'='9d842f5196f021131be8b6561cc70b17dd0deebdaca2cbdc2f3579dce8e0b2a9')
audit_verify_sources(source_hashes)

# 1-3. PDF pp.7-11: Internet C/D, college, high-school and observer samples.
# These cleaned releases already use item-mean imputation, rounded to possible
# responses. Do not impute again, filter by age, add A/B subset-only matrices,
# or treat reports of another person as self-reports.
expected_n<-c(c=400L,d=600L,e=322L,f=499L,g=488L)
audit_0053<-list();score_checks<-list()
for(sample in names(expected_n)) {
 id<-paste0('0053',sample)
 path<-paste0('data/3.meta_data/open_data/d',id,'.xlsx')
 raw<-as.data.frame(readxl::read_excel(path))
 stopifnot(nrow(raw)==expected_n[[sample]],!anyDuplicated(raw))
 # College Case labels are reused (165 repeated labels), while the records
 # and all 192-item profiles differ. They are not a verified unique-person key;
 # do not delete respondents by Case or infer overlap from these labels.
 if(sample!='e')stopifnot(!anyDuplicated(raw$Case))
 data_scored<-as.data.frame(raw[,names(bessi_map)])
 names(data_scored)<-unname(bessi_map)

 # 4. Reconstruct all 32 six-item facet means and five official weighted
 # domains. The final 192 named columns reproduce even the earlier C version.
 checks<-list()
 for(facet in names(bessi192_first_item)) {
  items<-as.matrix(raw[,paste0('bessi_',seq(bessi192_first_item[[facet]],192,32))])
  stopifnot(all(is.na(items) | (items>=1 & items<=5)))
  value<-rowMeans(items,na.rm=TRUE);value[is.nan(value)]<-NA_real_
  stopifnot(isTRUE(all.equal(value,data_scored[[facet]],check.attributes=FALSE,tolerance=1e-12)))
  checks[[facet]]<-max(abs(value-data_scored[[facet]]),na.rm=TRUE)
 }
 for(domain in names(bessi192_domain_weights)) {
  w<-bessi192_domain_weights[[domain]]
  value<-rowSums(sweep(as.matrix(data_scored[,names(w)]),2,w,'*'))/sum(w)
  stopifnot(isTRUE(all.equal(value,data_scored[[domain]],check.attributes=FALSE,tolerance=1e-12)))
 }
 score_checks[[sample]]<-checks

 # 5. The college correlates retain the supplied scores. Independently check
 # every BFI-2 facet/domain against its items; other instruments remain supplied
 # composites, not an independently reconstructed item-level scoring claim.
 if(sample=='e') {
  for(source in names(college_map))data_scored[[college_map[[source]]]]<-as.numeric(raw[[source]])
  bfi_rebuilt<-data.frame(matrix(NA_real_,nrow(raw),length(bfi2_facet_items),
   dimnames=list(NULL,names(bfi2_facet_items))))
  for(facet in names(bfi2_facet_items)) {
   key<-bfi2_facet_items[[facet]];items<-as.matrix(raw[,paste0('bfi2_',abs(key))])
   items[,key<0]<-6-items[,key<0,drop=FALSE]
   value<-rowMeans(items)
   stopifnot(isTRUE(all.equal(value,data_scored[[facet]],check.attributes=FALSE,tolerance=1e-12)))
   bfi_rebuilt[[facet]]<-value
  }
  for(domain in names(bfi2_domain_facets))stopifnot(isTRUE(all.equal(
   rowMeans(bfi_rebuilt[,bfi2_domain_facets[[domain]]]),data_scored[[domain]],
   check.attributes=FALSE,tolerance=1e-12)))
 }
 if(sample=='f') {
  for(source in names(school_map))data_scored[[school_map[[source]]]]<-as.numeric(raw[[source]])
  # Retain the historical available-term mean. Winter/spring GPA are both
  # prospective to BESSI; do not call this a same-occasion outcome (PDF p.23).
  data_scored$academicachievement<-rowMeans(raw[,c('GPA_Winter','GPA_Spring')],na.rm=TRUE)
  data_scored$academicachievement[is.nan(data_scored$academicachievement)]<-NA_real_
 }
 if(sample%in%c('c','d')) {
  stopifnot(all(raw$Gender%in%c('Male','Female')))
  data_scored$sex<-ifelse(raw$Gender=='Female',1,0)
 }
 if(sample=='e')data_scored$sex<-ifelse(raw$Gender==1,0,ifelse(raw$Gender==2,1,NA_real_))
 if(sample=='f')data_scored$sex<-ifelse(raw$Gender==-1,0,ifelse(raw$Gender==1,1,NA_real_))
 if(sample=='g') {
  stopifnot(all(raw$TargetGender%in%c(1,2)))
  data_scored$sex<-ifelse(raw$TargetGender==2,0,1)
 }
 codebook<-readr::read_csv('data/matrix_codebook.csv',show_col_types=FALSE)
 data_matrix<-data_scored[,names(data_scored)%in%codebook$column_name,drop=FALSE]
 note<-paste('All released participants retained. All 32 BESSI item means and five official weighted domains reproduce; supplied data already contain source item-mean imputation.',
  if(sample=='e')'All 20 BFI-2 facet/domain scores independently reconstructed; remaining non-BESSI composites supplied. Study 4 eligibility differs by measure (313/249 in paper), so retain pair Ns rather than assuming all 322 per correlation. Case labels are not unique (165 repeated labels), but no full records or BESSI item profiles duplicate; retain the released sample without inventing exclusions.',
  if(sample=='f')'Retain the historical available-term winter/spring GPA mean, with prospective timing documented; do not change occasions or participants. Outcome-specific coverage gives pair Ns 62-499, especially for relationship measures; do not assume N=499 for every coefficient.',
  if(sample=='g')paste('Observer-report BESSI: historical unsuffixed variable names retained pending separate method harmonization. Sex is target sex, not rater sex.',
   'User decision 2026-11-14: current age/gender descriptives refer to rated targets (mean age 47.52; 46.72% female). Rater descriptives (mean age 49.26; 51.64% female) remain documented in notes.'),
  'Age excluded from matrices. Retrieval date unknown. Current OSF automatic access failed; local originals are verified, with no eligibility change.')
 result<-audit_compare_matrix('0053',id,data_matrix,expected_n[[sample]],note,names(source_hashes),
  '0053 PDF pp.7-11,15,23; official BESSI key; original item/score columns',status='HISTORICAL_VALUES_REPRODUCED')
 result$summary$score_mismatch_facets<-0L
 result$summary$score_mismatch_values<-0L
 result$summary$duplicate_source_case_ids<-sum(duplicated(raw$Case))
 audit_0053[[sample]]<-result
}
summary_0053<-dplyr::bind_rows(lapply(audit_0053,`[[`,'summary'))
print(summary_0053[,c('matrix_id','reference_n','variables','compared_pairs','changed_pairs','pair_n_min','pair_n_max')])
if(!'--check-only'%in%args)audit_save_summary(summary_0053)
