# 0126 — Allemand et al. (2026), Motivation to change personality across
# adulthood: Preferences for human and digital assistance in representative
# Swiss samples. User retains all 1,917 Part-2 participants; N corrected from
# paper's restricted 1,898. Normal run exports; --check-only writes nothing.
source('R/historical_audit.R')
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% '--check-only'))
source_hashes <- c(
 'data/3.meta_data/open_data/d0126.xlsx'='de4ee5963e3667d9ac0c9b03794fc92c4a38da013e98e5f6b09ee3913ad5f669',
 'data/2.full_abstracting/pdfs/0126.pdf'='266bacb248ad916d11c3f5306cb1583c29dccb377b7813beda30acf15072e1d0')
audit_verify_sources(source_hashes)

# 1-3. 3,030 Part-1 rows; exactly 1,917 contain Part-2 demographics. All other
# Part-2 columns are empty outside this set. The paper excludes 19 respondents
# missing >=6/11 items; the local subset omits the assistance-preference item,
# preventing exact adjudication of four borderline cases. Do not pick one by
# matching aggregate statistics. User decision: retain ALL 1,917 instead.
raw <- as.data.frame(readxl::read_excel(names(source_hashes)[1]))
part2 <- !is.na(raw$DEMO1v2_wp2)
part2_variables <- grep('_wp2$',names(raw),value=TRUE)
stopifnot(nrow(raw)==3030L,sum(part2)==1917L,
 all(rowSums(!is.na(raw[!part2,part2_variables]))==0L),
 all(raw$DEMO1v2_wp2[part2] %in% 1:2))
data_analytic <- raw[part2,]

# 4-5. All scores explicitly oriented high=more of the construct; source
# questionnaire single items retain their ordinal 3/5/6/7-category metrics.
# Adaptability is the available-item mean of six positive BESSI items (1-5).
adaptability_items <- paste0('DIGS12',letters[1:6],'_wp2')
scored <- data.frame(sex=raw$DEMO1v2_wp2-1,
 adaptability=rowMeans(raw[,adaptability_items],na.rm=TRUE),
 satisfactionwithlife=8-raw$SOC15v2,changegoals=4-raw$DIGS10_wp2,
 willingnessfordigitalassistance=6-raw$DIGS8_wp2,
 useofdigitalassistance=7-raw$DIGS9_wp2)
scored$adaptability[is.nan(scored$adaptability)] <- NA_real_
data_scored <- scored[part2,]
data_matrix <- data_scored
age <- 2024-data_analytic$DEMO2_wp2 # Survey year, not the extraction year.
descriptive_age <- round(mean(age,na.rm=TRUE),2)
descriptive_female <- round(mean(data_matrix$sex)*100,2)
stopifnot(sum(is.na(age))==3L,descriptive_age==52.72,descriptive_female==50.60)

# 6-8. Removing the 1,113 Part-1-only rows changes NO correlations: they only
# supply life satisfaction and cannot contribute any bivariate association.
note <- paste('All 15 historical correlations reproduced. User decision 2026-11-14: retain all 1917 Part-2 participants and update reference N from 1898 to 1917; do not impose the paper\'s 19 missingness exclusions.',
 'Local file contains 3030 Part-1 rows; 1113 have no Part-2 variables and contribute no bivariate correlations. Removing only those rows preserves every coefficient. Pair Ns=1773-1906.',
 'The assistance-preference item needed for the exact 11-item paper filter is absent; four cases cannot be classified uniquely, so the restricted 1898 sample is not invented.',
 'Six-item available-response BESSI adaptability mean; life satisfaction, change stage, willingness and use reversed explicitly. Ordinal questionnaire metrics retained. Three missing ages remain in private data; no age-based exclusion. Age is 2024 minus birth year.',
 'Sample 1 has no BESSI and is not assigned a matrix. Categorical human/digital preference remains excluded as historically decided. Published text explicitly confirms life-satisfaction direction; no independent full questionnaire-codebook reconstruction claimed.',
 'Descriptives aligned to retained Part-2 rows: female 970/1917=50.60%; mean age from available birth years is 52.72 (1914 observed, 3 missing). Published restricted-cohort descriptives were 52.67 years and 50.9% women (historically rounded to 51%); these are retained as reference, not mixed with the new N.')
audit_0126 <- list(historical=audit_compare_matrix('0126','0126a',scored,1898,note,
 names(source_hashes),'0126 Sample 2 p.3; additional measures pp.3-4',status='HISTORICAL_N_MISMATCH'))
audit_0126$part2 <- audit_compare_matrix('0126','0126a',data_matrix,1917,note,
 names(source_hashes),'User retains all Part-2 respondents; paper p.3 reports N=1917 before exclusions',
 status='APPROVED_FULL_PART2_SAMPLE',comparison='full_part2_reference_n')
stopifnot(audit_0126$historical$summary$changed_pairs==0L,audit_0126$part2$summary$changed_pairs==0L,
 identical(unname(audit_0126$historical$candidate),unname(audit_0126$part2$candidate)))
summary_0126 <- dplyr::bind_rows(lapply(audit_0126,`[[`,'summary'))
print(summary_0126[,c('matrix_id','comparison','reference_n','input_rows','changed_pairs','pair_n_min','pair_n_max')])
if(!'--check-only' %in% args) {
 source('R/codebook_workflow.R',local=TRUE);source('R/extraction_helpers.R',local=TRUE)
 source('R/workflow_io.R',local=TRUE);source('R/workflow_results.R',local=TRUE)
 old <- readr::read_csv('data/3.meta_data/extraction/historical_audit.csv',show_col_types=FALSE)
 first <- audit_0126$part2$summary
 first$comparison <- 'approved_integration_2026_11_14';first$status <- 'REVIEWED_BEFORE_AFTER'
 if(any(old$matrix_id=='0126a' & old$comparison==first$comparison))first <- first[FALSE,]
 audit_save_summary(dplyr::bind_rows(summary_0126,first))
 individual_data <- cbind(data_matrix,age=age)
 checks <- extraction_check_matrix(data_matrix,audit_0126$part2$candidate,audit_0126$part2$pair_n)
 wf_compact_extraction('0126','0126a',individual_data,audit_0126$part2$candidate,audit_0126$part2$pair_n,checks)
}
