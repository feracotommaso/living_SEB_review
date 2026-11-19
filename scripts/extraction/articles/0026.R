# 0026: verified historical scores plus user-approved objective performance.
# Run from the repository root. --check-only writes nothing; a normal run
# exports the expanded matrix, pair Ns and private coded individual CSV.
source("R/historical_audit.R")
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% "--check-only"))
source_hashes <- c(
  "data/3.meta_data/open_data/d0026.csv"="4216a7a0343e30ddda6cd39a09ffa92b7ee364dfa61e25f355bd0607ccca023d",
  "data/3.meta_data/open_data/opencode0026.R"="fa6360740363954330c0f22b980690c012df47b10a67822fe835acc4adb67454",
  "data/2.full_abstracting/pdfs/0026.pdf"="4a3bac11db6570675bcc247fff3b551f6ed1c77628989a9d85755ee7d40d132b")
audit_verify_sources(source_hashes)

# 1-3. Workplace sample, PDF pp.3-4: N=2992 after author exclusions.
# The release contains the selected cohort; upstream exclusions cannot be replayed.
raw_scores <- as.data.frame(readr::read_csv(names(source_hashes)[1],show_col_types=FALSE))
stopifnot(nrow(raw_scores)==2992)
data_scored <- raw_scores

# 4. The release numbers SES items by consecutive nine-item domain blocks.
# Each supplied domain equals the corresponding block mean to CSV precision.
# SES_ indices must not be confused with the interleaved numbering in the
# official BESSI-45 form; original item wording is absent from this CSV.
# Official reference checked online 2026-11-13:
# https://www.canpersonalitychange.com/uploads/1/3/1/0/131015187/bessi-45_scoring_1.pdf
release_item_blocks <- list(selfmanagement=1:9,socialengagement=10:18,
  cooperation=19:27,emotionalresilience=28:36,innovation=37:45)
domain_sources <- c(selfmanagement="SMang_M",socialengagement="SEnga_M",
  cooperation="Coop_M",emotionalresilience="ERes_M",innovation="Innov_M")
item_values <- as.matrix(raw_scores[,paste0("SES_",1:45)])
stopifnot(!anyNA(item_values),all(item_values>=1 & item_values<=5))
maximum_score_delta <- 0
for(domain in names(release_item_blocks)) {
  rebuilt <- rowMeans(raw_scores[,paste0("SES_",release_item_blocks[[domain]])])
  delta <- max(abs(rebuilt-raw_scores[[domain_sources[[domain]]]]))
  stopifnot(delta<5e-10)
  maximum_score_delta <- max(maximum_score_delta,delta)
}
# Preserve the supplied rounded domain values for the exact legacy comparison.

# 5. Explicit historical source mapping; sex is 0 female / 1 male in the release.
variable_map <- c(
  "Open_M"="openness",
  "Cons_M"="conscientiousness",
  "Extra_M"="extraversion",
  "Agree_M"="agreeableness",
  "Neu_M"="neuroticism",
  "SMang_M"="selfmanagement",
  "Coop_M"="cooperation",
  "SEnga_M"="socialengagement",
  "ERes_M"="emotionalresilience",
  "Innov_M"="innovation",
  "CWB_M"="counterproductiveworkbehavior",
  "FWG_M"="coworkerrelationship",
  "OCB_M"="organizationalcitizenshipbehavior",
  "JSB"="jobsatisfaction")
names(data_scored)[match(names(variable_map),names(data_scored))] <- unname(variable_map)
stopifnot(all(is.na(raw_scores$sex) | raw_scores$sex %in% c(0,1)))
data_scored$sex <- 1-raw_scores$sex
data_scored$age <- raw_scores$age
data_matrix <- data_scored[,c(unname(variable_map),"sex")]

# 6-8. Scores match to release precision; item-content/order and non-BESSI
# scoring cannot be independently checked without their original item data.
audit_0026 <- audit_compare_matrix("0026","0026a",data_matrix,2992,
  note=paste("Released N=2992 matches the paper. All five BESSI domains reproduced as means of nine consecutive release items; maximum score difference",
    signif(maximum_score_delta,4),
    "(rounding in supplied scores). Release item wording/order not independently verified; do not apply official form numbers to SES_ indices. Non-BESSI items unavailable. Sex recoded 0 male / 1 female; 14 missing sex values produce pair Ns 2978-2992. Author exclusions not reconstructed; age retained separately."),
  sources=names(source_hashes),evidence="0026 PDF pp.3-5; released SES_1:45 and domain scores; author code inspected without execution",
  status="BESSI45_ARITHMETIC_AND_MATRIX_REPRODUCED")
print(audit_0026$summary[,c("matrix_id","changed_pairs","max_abs_delta","pair_n_min","pair_n_max")])

# User-approved 2026-11-18: keep the two objective indicators distinct.
# PDF p.4: both are already divided by months employed and centered by city
# in the release. Do not apply these transformations a second time.
data_performance <- data_matrix
data_performance$jobperformanceactivity <- raw_scores$showing_center
data_performance$jobperformancesales <- raw_scores$deal_center
stopifnot(sum(!is.na(raw_scores$showing_center))==2723L,
  sum(!is.na(raw_scores$deal_center))==2196L)
performance_note <- paste('User-approved 2026-11-18: recover showing_center as jobperformanceactivity and deal_center as jobperformancesales.',
  'These company-recorded indicators are already expressed per month employed and centered within city; retain their original metric.',
  'Retain all2992 people and reference N=2992. Activity is observed for2723, sales for2196; new pair Ns=2195-2723, with2196 for the activity-sales association.',
  'Expand15 to17 variables and105 to136 associations:31 additions, all previously saved coefficients unchanged.',
  'Table2 p.8 reproduces18/20 performance correlations within rounding. Two printed Emotional Stability correlations (+.02,+.02) have the opposite sign to stability obtained by reversing the released neuroticism score (about-.018 for each).',
  'Keep the released neuroticism direction and independently computed performance correlations; do not flip values merely to match the table. No raw non-BESSI item scoring or participant exclusions are reconstructed.')
audit_0026_performance <- audit_compare_matrix('0026','0026a',data_performance,2992,
  performance_note,names(source_hashes),'0026 PDF p.4 andTable2 p.8; released performance columns; user decision2026-11-18',
  status='APPROVED_PERFORMANCE_RECOVERY',comparison='approved_performance_recovery')

# Published Table2, with Emotional Stability translated to the opposite-pole
# neuroticism code for comparison. Preserve and document its two discrepancies.
predictors <- c('selfmanagement','socialengagement','cooperation','emotionalresilience','innovation',
  'conscientiousness','extraversion','agreeableness','neuroticism','openness')
performance_reference <- cbind(jobperformanceactivity=c(.03,.02,.02,.01,.00,.04,.03,.02,-.02,.00),
  jobperformancesales=c(.02,.04,.01,-.01,-.02,.05,.03,.00,-.02,-.03))
performance_delta <- abs(audit_0026_performance$candidate[predictors,colnames(performance_reference)]-performance_reference)
stopifnot(sum(performance_delta>.0050001)==2L,
  all(which(performance_delta>.0050001,arr.ind=TRUE)[,1]==match('neuroticism',predictors)))
audit_0026_performance$summary$published_pairs_checked <- length(performance_delta)
audit_0026_performance$summary$published_pairs_outside_rounding <- sum(performance_delta>.0050001)

# Preserve the old common block exactly after independent Pearson agreement.
saved <- as.matrix(readxl::read_excel('data/3.meta_data/matrices/0026a.xlsx'))
rownames(saved) <- colnames(saved);common <- intersect(colnames(saved),names(data_performance))
stopifnot(isTRUE(all.equal(saved[common,common],audit_0026_performance$candidate[common,common],tolerance=1e-12)))
audit_0026_performance$candidate[common,common] <- saved[common,common]
print(audit_0026_performance$summary[,c('matrix_id','variables','compared_pairs','changed_pairs','pair_n_min','pair_n_max')])
if(!'--check-only'%in%args) {
  source('R/codebook_workflow.R',local=TRUE);source('R/extraction_helpers.R',local=TRUE)
  source('R/workflow_io.R',local=TRUE);source('R/workflow_results.R',local=TRUE)
  audit_save_summary(audit_0026_performance$summary)
  individual_data <- cbind(data_performance,age=raw_scores$age)
  checks <- extraction_check_matrix(data_performance,audit_0026_performance$candidate,audit_0026_performance$pair_n)
  wf_compact_extraction('0026','0026a',individual_data,audit_0026_performance$candidate,audit_0026_performance$pair_n,checks)
}
