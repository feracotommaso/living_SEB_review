# 0070: approved recovery of single-item behavior, distinct from competence.
# Source: doi:10.1080/13504622.2025.2459323, OSF u3mny; original dates unknown.
# Normal run exports the complete matrix. --check-only never writes.
source('R/historical_audit.R')
args<-commandArgs(trailingOnly=TRUE)
stopifnot(all(args%in%'--check-only'))
source_hashes<-c(
 'data/3.meta_data/open_data/d0070.xlsx'='213b884e779d15293d35b1f39f6f9088ad44fb5302ff5ff800bc8f3eeb1e2e24',
 'data/3.meta_data/open_data/opencode0070.R'='afe698731f5642f514aed4ecad181c33eebb89c00860743081f0179459e45c7b',
 'data/2.full_abstracting/pdfs/0070.pdf'='dba3f2e543370e62d2e62b6af77f57a51ae208396fe78e458f47f64cef72159a')
audit_verify_sources(source_hashes)

# 1-3. Full N=702, not the two validation subsamples treated independently.
raw<-as.data.frame(readxl::read_excel(names(source_hashes)[1]))
stopifnot(nrow(raw)==702L,all(raw$sex%in%c(1,2)))
data_scored<-data.frame(sex=raw$sex-1)

# 4-5. BESSI-45: nine items per domain. The authors already reversed CNS
# items 4/12/14 before releasing these data (PDF p.8); do not reverse again.
# Their final CNS retains 12 items, excluding 12/14. All SPACS items retained.
positions<-c(selfmanagement=1,innovation=5,cooperation=3,socialengagement=2,emotionalresilience=4)
for(domain in names(positions)) {
 items<-as.matrix(raw[,paste0('bessi_',seq(positions[[domain]],45,5))])
 stopifnot(all(is.na(items) | (items>=1 & items<=5)))
 value<-rowMeans(items,na.rm=TRUE);value[is.nan(value)]<-NA_real_
 data_scored[[domain]]<-value
}
data_scored$natureconnectedness<-rowMeans(raw[,paste0('CNS_',c(1:11,13))])
data_scored$sustainabilitycompetence<-rowMeans(raw[,paste0('SPACS_',1:12)])
data_matrix<-data_scored
note<-paste('N=702 retained. All five BESSI-45 means and the final 12-item CNS/12-item SPACS totals reconstructed with original scoring/missingness rules.',
 'CNS4 already reversed; excluded CNS12/14 are not restored. Binary sex 0 male/1 female; age retained in raw sources, absent from correlations.',
 'User-approved 2026-11-14: keep SPACS as sustainabilitycompetence and add PebSingle as proenvironmentalbehavior. These are perceived competence versus reported behavioral frequency, not duplicate measures of one construct. The single frequency item retains its original 1-7 scale.',
 'OSF automated access failed; verified local originals retained, retrieval date unknown.')
audit_0070<-list(source=audit_compare_matrix('0070','0070a',data_matrix,702,note,
 names(source_hashes),'0070 PDF pp.6-10, Table 3; original author scoring script',status='HISTORICAL_VALUES_REPRODUCED'))

# 6. Table 3, PDF p.10: 13 observed correlations currently included. The .40
# following CNS is its association with PEB, not the CNS/SPACS cell (.64).
reference<-rbind(natureconnectedness=c(.24,.39,.25,.15,.16,.09),
 sustainabilitycompetence=c(.26,.33,.31,.13,.17,.18))
colnames(reference)<-c(names(positions),'sex')
candidate<-audit_0070$source$candidate
delta<-c(abs(candidate[rownames(reference),colnames(reference)]-reference),
 abs(candidate['natureconnectedness','sustainabilitycompetence']-.64))
audit_0070$source$summary$published_pairs_checked<-length(delta)
audit_0070$source$summary$published_pairs_outside_rounding<-sum(delta>.0050001)
stopifnot(sum(delta>.0050001)==0L,
 abs(mean(data_scored$natureconnectedness)-3.40)<.0051,
 abs(sd(data_scored$natureconnectedness)-.68)<.0051)

# 7. Approved recovery: source PebSingle is an observed frequency response,
# not another competence scale. Keep the existing eight variables unchanged.
stopifnot(!anyNA(raw$PebSingle),all(raw$PebSingle%in%1:7))
with_peb<-data_matrix;with_peb$proenvironmentalbehavior<-raw$PebSingle
audit_0070$peb<-audit_compare_matrix('0070','0070a',with_peb,702,
 paste(note,'Approved recovery: eight additional correlations; all previous coefficients and N=702 retained.'),
 names(source_hashes),'0070 Table 3 PEB column; raw PebSingle',
 status='APPROVED_EXTRACTION',comparison='proenvironmental_behavior_recovery')
# The two additional published PEB associations in Table 3 are .40 with CNS
# and .44 with SPACS. Validate these separately from the 13 old references.
peb_reference<-c(natureconnectedness=.40,sustainabilitycompetence=.44)
peb_delta<-abs(audit_0070$peb$candidate[names(peb_reference),'proenvironmentalbehavior']-peb_reference)
stopifnot(all(peb_delta<=.0050001))
audit_0070$peb$summary$published_pairs_checked<-length(delta)+length(peb_delta)
audit_0070$peb$summary$published_pairs_outside_rounding<-sum(c(delta,peb_delta)>.0050001)
# 2026-11-18: recover the three SPACS resources, retaining the total score.
# Author model: KAP items 1-4, COI 5-8, WTA 9-12; four-item means, no missing items.
spacs_facets<-list(sustainabilityknowledge=1:4,sustainabilityinfluence=5:8,
 sustainabilitywillingnesstoact=9:12)
with_subscales<-with_peb
for(f in names(spacs_facets)) {
 items<-raw[,paste0('SPACS_',spacs_facets[[f]])]
 stopifnot(!anyNA(items),all(as.matrix(items)>=1 & as.matrix(items)<=5))
 with_subscales[[f]]<-rowMeans(items)
}
facet_note<-paste(note,'User-approved 2026-11-18: add SPACS Knowledge of Action Possibilities, Confidence in Own Influence and Willingness to Act as four-item means from the author key. Retain SPACS total and PebSingle; expand 9 to 12 variables, adding 30 associations, preserving all old coefficients and N=702.')
audit_0070$subscales<-audit_compare_matrix('0070','0070a',with_subscales,702,facet_note,
 names(source_hashes),'0070 PDF pp.7-10, Table 3; original author SPACS model',
 status='APPROVED_EXTRACTION',comparison='spacs_subscale_recovery')
# Table 3 independently checks all 27 published cells involving these resources.
facet_reference<-rbind(sustainabilityknowledge=c(.28,.29,.34,.11,.19,.13,.52,.79,.40),
 sustainabilityinfluence=c(.22,.25,.23,.15,.14,.17,.50,.85,.33),
 sustainabilitywillingnesstoact=c(.17,.29,.22,.06,.10,.14,.58,.87,.39))
colnames(facet_reference)<-c(names(positions),'sex','natureconnectedness','sustainabilitycompetence','proenvironmentalbehavior')
facet_delta<-abs(audit_0070$subscales$candidate[rownames(facet_reference),colnames(facet_reference)]-facet_reference)
within_reference<-c(.49,.55,.60)
within_facets<-audit_0070$subscales$candidate[names(spacs_facets),names(spacs_facets)]
facet_delta<-c(facet_delta,abs(within_facets[upper.tri(within_facets)]-within_reference))
stopifnot(all(facet_delta<=.0050001))
audit_0070$subscales$summary$published_pairs_checked<-length(facet_delta)
audit_0070$subscales$summary$published_pairs_outside_rounding<-sum(facet_delta>.0050001)
saved<-as.matrix(readxl::read_excel('data/3.meta_data/matrices/0070a.xlsx'))
rownames(saved)<-colnames(saved);common<-intersect(colnames(saved),names(with_subscales))
stopifnot(isTRUE(all.equal(saved[common,common],audit_0070$subscales$candidate[common,common],tolerance=1e-12)))
audit_0070$subscales$candidate[common,common]<-saved[common,common]
summary_0070<-dplyr::bind_rows(lapply(audit_0070,`[[`,'summary'))
print(summary_0070[,c('matrix_id','comparison','compared_pairs','changed_pairs','added_variables','pair_n_min','pair_n_max')])
if(!'--check-only'%in%args) {
 source('R/codebook_workflow.R',local=TRUE);source('R/extraction_helpers.R',local=TRUE)
 source('R/workflow_io.R',local=TRUE);source('R/workflow_results.R',local=TRUE)
 old<-readr::read_csv('data/3.meta_data/extraction/historical_audit.csv',show_col_types=FALSE)
 first<-audit_0070$peb$summary
 first$comparison<-'approved_integration_2026_11_14';first$status<-'REVIEWED_BEFORE_AFTER'
 if(any(old$matrix_id=='0070a' & old$comparison==first$comparison))first<-first[FALSE,]
 audit_save_summary(dplyr::bind_rows(summary_0070,first))
 individual_data<-cbind(with_subscales,age=raw$age)
 checks<-extraction_check_matrix(with_subscales,audit_0070$subscales$candidate,audit_0070$subscales$pair_n)
 wf_compact_extraction('0070','0070a',individual_data,audit_0070$subscales$candidate,audit_0070$subscales$pair_n,checks)
}
