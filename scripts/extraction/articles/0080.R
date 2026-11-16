# 0080: reconstruct observed scale means and verify published Table 2.
# Normal run updates only extraction/historical_audit.csv; --check-only writes nothing.
source('R/historical_audit.R')
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% '--check-only'))
source_hashes <- c(
 'data/3.meta_data/open_data/d0080.xlsx'='5c9f61e02ac4d9b79a60ab27818275c4db4476c1ee7d401e3817c39e4e6dc977',
 'data/2.full_abstracting/pdfs/0080.pdf'='ecb5cfb7ae4db3e84b083409b481868b29f449b696f31021cdd357e581ece97c')
audit_verify_sources(source_hashes)

# 1-3. All 319 unique released participants: Israel 126, Italy 65, Spain 128.
raw <- as.data.frame(readxl::read_excel(names(source_hashes)[1]))
stopifnot(nrow(raw)==319L,!anyDuplicated(raw$Participant),all(raw$Sex %in% 0:1))
data_analytic <- raw

# 4-5. BESSI-20 has four positive items per domain, in repeating five-item blocks.
# Sex is 1 male/0 female in the source; use our 0 male/1 female contrast.
data_scored <- data.frame(sex=1-raw$Sex)
positions <- c(selfmanagement=1,socialengagement=2,cooperation=3,
 emotionalresilience=4,innovation=5)
provided <- c('BESSI_SM','BESSI_SE','BESSI_CO','BESSI_ER','BESSI_IN')
for(i in seq_along(positions)) {
 items <- as.matrix(raw[,paste0('BESSI',seq(positions[[i]],20,5))])
 stopifnot(all(items>=1 & items<=5))
 data_scored[[names(positions)[i]]] <- rowMeans(items)
 stopifnot(max(abs(data_scored[[names(positions)[i]]]-raw[[provided[i]]]))<1e-12)
}
# SRL raw negative items are present alongside independently supplied ...N
# columns. Recompute each reversal once, verify it, then average all 20 items.
srl_items <- as.matrix(raw[,paste0('SRL_',1:20)])
reverse_items <- c(2,5,8,9,10,12,17)
srl_items[,reverse_items] <- 6-srl_items[,reverse_items]
stopifnot(max(abs(srl_items[,reverse_items]-
 as.matrix(raw[,paste0('SRL_',reverse_items,'N')])))<1e-12)
data_scored$srlstrategies <- rowMeans(srl_items)
data_scored$academicselfefficacy <- rowMeans(raw[,paste0('ASE',1:5)])
data_scored$academicsatisfaction <- rowMeans(raw[,paste0('AS',1:5)])
stopifnot(max(abs(data_scored$srlstrategies-raw$SRL_TOT))<1e-12,
 max(abs(data_scored$academicselfefficacy-raw$ASE_TOT))<1e-12,
 max(abs(data_scored$academicsatisfaction-raw$AS_TOT))<1e-12)

# Keep the supplied cross-country grade conversion; no guessed conversion or
# deletion. Two supplied values are zero, despite the main text saying 1-5.
# The conversion supplement could not be retrieved automatically. These zeros
# are retained as in the published analysis, not declared independently valid.
data_scored$academicachievement <- raw$Standard_Score
data_scored$age <- raw$Age
stopifnot(sum(raw$Standard_Score==0)==2L,
 abs(mean(raw$Standard_Score)-3.34)<.0051,
 abs(sd(raw$Standard_Score)-.93)<.0051)

# 6. Published Table 2, PDF p.6, row-by-row lower triangle in this order:
# Age, year, SM, IN, CO, SE, ER, SRL, ASE, academic satisfaction, achievement.
published <- c(
 .37,
 .24,.07,
 .30,.15,.49,
 .23,.07,.41,.47,
 .22,.10,.50,.52,.61,
 .23,.08,.46,.55,.61,.47,
 -.02,.00,.43,.32,.22,.34,.23,
 .14,.05,.60,.40,.41,.51,.39,.47,
 .06,-.12,.32,.33,.29,.34,.27,.33,.42,
 .25,.15,.41,.24,.30,.33,.26,.28,.51,.14)
publication_data <- cbind(age=raw$Age,year=raw$Year_study,
 data_scored[,c('selfmanagement','innovation','cooperation','socialengagement',
 'emotionalresilience','srlstrategies','academicselfefficacy',
 'academicsatisfaction','academicachievement')])
publication_cor <- cor(publication_data,method='pearson',use='pairwise.complete.obs')
delta <- abs(publication_cor[upper.tri(publication_cor)]-published)
stopifnot(length(delta)==55L,all(delta<=.0050001))

# 7-8. Preserve the saved observed correlations; age remains outside the matrix.
data_matrix <- data_scored[,setdiff(names(data_scored),'age')]
note <- paste('All 45 historical correlations reproduced, N=319 for every pair. All five BESSI-20 means, seven SRL reversals and SRL/ASE/AS means independently reconstructed and identical to supplied totals.',
 'All 55 published Table 2 coefficients reproduce within rounding (including age/year for validation only). Supplied Standard_Score grade conversion retained.',
 'Two source grade scores are zero although the main text describes 1-5; the conversion supplement was not automatically retrievable. No recoding or exclusion inferred. Original raw grade values and supplied conversion remain unchanged.',
 'Historical local individual data are available despite open_data=NO; this is not a new claim of public availability. Original acquisition date unknown.')
audit_0080 <- audit_compare_matrix('0080','0080a',data_matrix,319,note,
 names(source_hashes),'0080 Methods pp.3-5, Table 2 p.6; released items and supplied totals',
 status='HISTORICAL_VALUES_REPRODUCED')
audit_0080$summary$published_pairs_checked <- length(delta)
audit_0080$summary$published_pairs_outside_rounding <- sum(delta>.0050001)
cor_matrix <- audit_0080$candidate
pair_n <- audit_0080$pair_n
stopifnot(audit_0080$summary$changed_pairs==0L)
print(audit_0080$summary[,c('matrix_id','compared_pairs','changed_pairs','pair_n_min','pair_n_max','published_pairs_checked')])
if(!'--check-only' %in% args) audit_save_summary(audit_0080$summary)
