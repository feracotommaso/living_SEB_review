# 0095 — Sewell et al. (2026), Experiental features of volunteering predict
# changes in college students' social, emotional, and behavioral skills.
# Audit only: normal run updates historical_audit.csv; --check-only writes nothing.
source('R/historical_audit.R')
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% '--check-only'))
source_hashes <- c(
 'data/3.meta_data/open_data/d0095.xlsx'='31da055fdf658d374b4b7074c2f8f876d2922eacb59c8eacb9706f44afb2f13f',
 'data/3.meta_data/open_data/d0095.csv'='d6da545f2886e8dd39ad0bc0a40daa0fb396c788e9dea59b4bb70bc1575dec71',
 'data/3.meta_data/open_data/opencode0095.Rmd'='b377d7eec25e799d0ee82902df0e172edca6c8da44b8f1093d2fe4e9dd35ae0e',
 'data/2.full_abstracting/pdfs/0095.pdf'='03798e21c6e0fd08649f4b1a00047d5e05857a83552ecb60d8dad4e825e3f240')
audit_verify_sources(source_hashes)

# 1-3. Baseline T1 only: all 455 unique participants, volunteers and comparison.
# Do not treat T2 as a second independent sample or exclude anyone for age.
raw <- as.data.frame(readxl::read_excel(names(source_hashes)[1]))
raw_csv <- as.data.frame(readr::read_csv(names(source_hashes)[2],show_col_types=FALSE))
stopifnot(nrow(raw)==455L,!anyDuplicated(raw$user.id),
 isTRUE(all.equal(raw,raw_csv,check.attributes=FALSE)))
data_analytic <- raw

# 4-5. This BESSI-45 release uses the original 192-item IDs, not positions 1:45.
items <- list(
 selfmanagement=c(163,70,169,172,143,178,149,152,155),
 socialengagement=c(1,65,109,141,81,113,121,153,103),
 cooperation=c(2,162,136,72,78,110,23,183,61),
 emotionalresilience=c(133,165,107,139,52,116,90,186,126),
 innovation=c(4,68,144,176,28,60,32,64,86))
stopifnot(all(is.na(raw$gender2_t1) | raw$gender2_t1 %in% 0:1))
data_scored <- data.frame(volunteering=raw$group_t1.x,sex=1-raw$gender2_t1)
author_scores <- data_scored
for(domain in names(items)) {
 values <- as.matrix(raw[,paste0('bessi_',items[[domain]],'_t1')])
 stopifnot(all(is.na(values) | (values>=1 & values<=5)))
 value <- rowMeans(values,na.rm=TRUE);value[is.nan(value)] <- NA_real_
 data_scored[[domain]] <- value                    # Historical available items.
 author_scores[[domain]] <- rowMeans(values)      # Author code: all nine items.
}
individual_data <- cbind(data_scored,age=raw$age_t1)
data_matrix <- data_scored
note <- paste('N=455 baseline T1 participants retained; no T2 matrix. All 21 historical correlations reproduced using available-item means; pair Ns=418-446.',
 'Original CSV and XLSX have identical values. Binary sex is 0 male/1 female; 28 missing source sex codes remain missing.',
 'Author code instead requires all nine items per domain. Candidate complete-item scoring retains all 455 rows, pair Ns=403-443; incomplete domains alone become missing.',
 'Volunteering is binary group membership, not hours. Hours/reflection variables concern a later selected volunteer subset and are not added to this baseline matrix.',
 'User decision 2026-11-14: retain historical available-item means and reference N=455; do not replace them with the complete-item candidate. Ten of 21 published baseline references differ beyond rounding under the retained rule (maximum difference .02424).')
audit_0095 <- list(historical=audit_compare_matrix('0095','0095a',data_matrix,455,
 note,names(source_hashes),'0095 Methods pp.3-5; Table 2 p.6; author Rmd scoring',
 status='AVAILABLE_ITEM_SCORING_RETAINED_BY_DECISION'))
audit_0095$author <- audit_compare_matrix('0095','0095a',author_scores,455,note,
 names(source_hashes),'0095 author Rmd: explicit sums divided by nine',
 status='NOT_ADOPTED_BY_USER',comparison='complete_item_scoring')

# 6. Table 2 baseline subset, published gender 0 female/1 male. Reverse our
# binary contrast back only for publication validation; do not change the matrix.
publication_order <- c('sex',names(items),'volunteering')
published <- c(-.04,-.02,.32,-.14,.44,.50,.18,.52,.36,.48,
 -.08,.30,.40,.42,.35,.05,.26,.08,.18,.19,.12)
for(which in names(audit_0095)) {
 comparison <- audit_0095[[which]]$data_matrix[,publication_order]
 comparison$sex <- 1-comparison$sex
 r <- cor(comparison,method='pearson',use='pairwise.complete.obs')
 delta <- abs(r[upper.tri(r)]-published)
 audit_0095[[which]]$summary$published_pairs_checked <- length(delta)
 audit_0095[[which]]$summary$published_pairs_outside_rounding <- sum(delta>.0050001)
}
# Paper footnote says list-wise deletion, but inspected corr.test defaults and
# the 21 coefficients reproduce pairwise deletion on complete-item scale scores.
stopifnot(audit_0095$historical$summary$changed_pairs==0L,
 audit_0095$historical$summary$published_pairs_outside_rounding==10L,
 audit_0095$author$summary$published_pairs_outside_rounding==0L)

# 7-8. Retain the user's available-item rule; only the audit is updated.
summary_0095 <- dplyr::bind_rows(lapply(audit_0095,`[[`,'summary'))
print(summary_0095[,c('matrix_id','comparison','changed_pairs','pair_n_min','pair_n_max','published_pairs_outside_rounding')])
if(!'--check-only' %in% args) audit_save_summary(summary_0095)
