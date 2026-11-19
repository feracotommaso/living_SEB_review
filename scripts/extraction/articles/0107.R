# 0107 — Fallucchi, Marietta Leina & Quercia (2026), Validating a survey measure
# of conditional cooperation. Verified linkage, binary sex and source-based
# BESSI scoring correction; normal run exports, --check-only writes nothing.
source('R/historical_audit.R')
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% '--check-only'))
source_hashes <- c(
 'data/3.meta_data/open_data/d0107a.csv'='cdb68c6ad062e1576ba2850261e87d463a3c17163b4b9453481483f12bf16fd3',
 'data/3.meta_data/open_data/d0107b.csv'='380d287db9c5ea61047d0c5775cd586122287f053a8e2241863b33163a2bfaeb',
 'data/3.meta_data/open_data/opencode0107.R'='ed5a8479d97600983623bfd78613c2b58a43c7008a7bc7fee15cd70e7ae1820d',
 'data/2.full_abstracting/pdfs/0107.pdf'='31dcaae2cbc204f5de3f0fc8f2b57636293933b8a9b299cfc9f2dd0630a13c4e',
 'data/3.meta_data/open_data/raw/0107/supplement_mmc1.pdf'='12adef5cefb4ba5aa36ba9950651f3d73cb27da87a378d04e5dabe5aa2efaca0',
 'data/3.meta_data/open_data/raw/0107/Replication_Package_Final.R'='cfa8645967b260b8ebdf09a77ddc3531aee2e31911a38e9c1decfd2ef0000a2a',
 'data/3.meta_data/open_data/raw/0107/README.txt'='b185b40f8e5146bfe65c0d7f5203ccf347ddc56607773c7acdb9eb5e0b618eee',
 'data/3.meta_data/open_data/raw/shared/bessi_scoring_v1.pdf'='9d842f5196f021131be8b6561cc70b17dd0deebdaca2cbdc2f3579dce8e0b2a9',
 'data/3.meta_data/open_data/raw/shared/bessi_form_self_v1.pdf'='8901359bbd178be7f176906f601567801b1efd756d0b220b7fc8f9b67219fd00')
audit_verify_sources(source_hashes)

# 1-3. Both sources contain the same 259 records. A has scores/demographics but
# omits item 29; B has all 30 items. One identifier occurs twice in EACH file.
# Recreate the old many-to-many merge only to diagnose the stored 261-row matrix.
raw_a <- read.csv(names(source_hashes)[1])
raw_b <- read.csv(names(source_hashes)[2])
stopifnot(nrow(raw_a)==259L,nrow(raw_b)==259L,
 sum(duplicated(raw_a$player.identifier_code))==1L,
 sum(duplicated(raw_b$player.identifier_code))==1L)
legacy_join <- merge(raw_b,raw_a[,c('player.identifier_code',
 setdiff(names(raw_a),names(raw_b)))],by='player.identifier_code',all.x=TRUE)
stopifnot(nrow(legacy_join)==261L)

# Verify identity using the identifier AND all 29 common item responses. This
# key is unique in each file and matches every row; no respondent is deleted.
common_items <- intersect(grep('^questionnaire.1.player.item_',names(raw_a),value=TRUE),names(raw_b))
stopifnot(length(common_items)==29L)
key_a <- apply(raw_a[,c('player.identifier_code',common_items)],1,paste,collapse='|')
key_b <- apply(raw_b[,c('player.identifier_code',common_items)],1,paste,collapse='|')
link <- match(key_a,key_b)
stopifnot(!anyDuplicated(key_a),!anyDuplicated(key_b),!anyNA(link),
 identical(as.numeric(as.matrix(raw_a[,common_items])),
  as.numeric(as.matrix(raw_b[link,common_items]))))
data_analytic <- raw_a
data_analytic$questionnaire.1.player.item_29 <- raw_b$questionnaire.1.player.item_29[link]

# 4-5. Table 3 p.6 explicitly refers to Soto et al. scoring for Q32:Q61.
# Supplement Appendix D, PDF pp.17-18/24, supplies the actual 30 questions.
# Q35 is the teamwork activity corresponding to official item23, NOT goal
# setting (item24). Thus item_24 is a source label error; retain the original
# field/response and use it as the intended first teamwork item.
facet_items <- list(perspectivetakingskill=c(2,34,66,98,130,162),
 capacityfortrust=c(8,40,72,104,136,168),
 capacityforsocialwarmth=c(14,46,78,110,142,174),
 teamworkskill=c(24,55,87,119,151,183),
 ethicalcompetence=c(29,61,93,125,157,189))
variable_map <- c('questionnaire.1.player.seguire_regole'='rulefollowing',
 risk='risktaking',altruism='altruism',positive_reciprocity='positivereciprocity',
 negative_reciprocity='negativereciprocity',trust='trust',teamwork='teamwork',
 time_discounting='timediscounting',Cooperation_Skills='cooperation',
 extraversion='extraversion',agreeableness='agreeableness',conscientiousness='conscientiousness',
 emo_stability='neuroticism',openness='openness')
matrix_variables <- c(unname(variable_map[1:8]),'sex',unname(variable_map[9:14]),names(facet_items))
matrices <- list()
for(which in c('historical','linked')) {
 d <- if(which=='historical')legacy_join else data_analytic
 scored <- d[,names(variable_map)];names(scored) <- unname(variable_map)
 scored$neuroticism <- -scored$neuroticism
 scored$sex <- ifelse(d$survey.1.player.gender=='Uomo',0,
  ifelse(d$survey.1.player.gender=='Donna',1,.5)) # Historical coding, diagnostic only.
 for(facet in names(facet_items)) {
  values <- as.matrix(d[,paste0('questionnaire.1.player.item_',facet_items[[facet]])])
  stopifnot(!anyNA(values),all(values>=1 & values<=5))
  scored[[facet]] <- rowMeans(values)
 }
 scored$ethicalcompetence <- .5*scored$ethicalcompetence # Historical scale, diagnostic only.
 matrices[[which]] <- scored[,matrix_variables]
}
matrices$gender <- matrices$linked
stopifnot(sum(data_analytic$survey.1.player.gender=='Altro')==4L)
matrices$gender$sex[data_analytic$survey.1.player.gender=='Altro'] <- NA_real_
data_scored <- matrices$gender
data_scored$ethicalcompetence <- 2*data_scored$ethicalcompetence
# Supplement ethical items: Q40/Q45/Q50/Q55/Q60/Q61, i.e. source fields
# 61/93/125/157/189/29. All SIX were administered; the supplied five-item
# score omits Q61. The public replication script consumes supplied scores and
# does not define their construction. Restore the declared instrument scoring:
# full 1-5 facet means; only the domain applies normalized official weights.
weights <- c(perspectivetakingskill=1,capacityfortrust=1,
 capacityforsocialwarmth=1,teamworkskill=1,ethicalcompetence=.5)
data_scored$cooperation <- as.numeric(as.matrix(data_scored[,names(weights)]) %*% weights/sum(weights))
matrices$scoring <- data_scored
matrices$recovery <- data_scored
matrices$recovery$conditionalcooperationtask <- data_analytic$CClab # CC1 vs all other lab types.

# 6. Four available TIPI pairs reproduce supplied traits exactly. Extraversion's
# second raw item (6) is absent; preserve the supplied score, not a guessed item.
tipi_pairs <- list(agreeableness=c(7,2),conscientiousness=c(3,8),
 emo_stability=c(9,4),openness=c(5,10))
for(trait in names(tipi_pairs)) {
 pair <- tipi_pairs[[trait]]
 value <- (data_analytic[[paste0('questionnaire.1.player.i_tipi_',pair[1])]]+
  8-data_analytic[[paste0('questionnaire.1.player.i_tipi_',pair[2])]])/2
 stopifnot(max(abs(value-data_analytic[[trait]]))<1e-12)
}
# Confirm the exact source error: its ethical score used FIVE items at half
# scale and the domain averages four facets plus this half-scaled score / 5.
ethical5 <- rowMeans(data_analytic[,paste0('questionnaire.1.player.item_',facet_items$ethicalcompetence[-1])])
provided_facets <- c('Perspective_Taking_Score','Trust_Score','Social_Warmth_Score',
 'Teamwork_Score','Ethical_Competence_Score')
stopifnot(max(abs(data_analytic$Ethical_Competence_Score-.5*ethical5))<1e-12,
 max(abs(data_analytic$Cooperation_Skills-rowMeans(data_analytic[,provided_facets])))<1e-12,
 identical(as.numeric(colSums(data_analytic[,c('CClab','FRlab','FClab','OTlab')])),c(202,48,6,3)),
 all(data_analytic$CClab %in% 0:1))

# 7-8. Export the verified correction, without the declined CC recovery.
# Metadata's legacy bessi45 label is inconsistent with the
# 30-item cooperation-only instrument in Methods p.3; retained pending coding
# harmonisation, with the actual scope documented explicitly here.
note <- paste('All 190 historical coefficients reproduced from an erroneous 261-row many-to-many merge, while both originals and paper contain 259 participants.',
 'One-to-one identifier-plus-29-response linkage resolves both repeated labels without deleting people. Proposed binary sex contrast leaves four Altro responses missing, rather than assigning .5.',
 'User approval 2026-11-14: correct linkage and leave four Altro codes missing only in binary sex; retain N=259. Do not add conditionalcooperation.',
 'After requested article/supplement verification: Table 3 refers to Soto scoring; Appendix D Q32-Q61 includes all six ethical items. Public replication code consumes the supplied totals without constructing them. Restore six-item ethical mean on 1-5 and normalized official cooperation weights; provided ethical total used only five items and half scale.',
 'Appendix D Q35 wording confirms teamwork, not goal setting: source item_24 is retained as the intended official item23, a label error. Four TIPI pairs reconstructed; extraversion second item6 unavailable, supplied score retained.',
 'Instrument is a 30-item cooperation subset, not full BESSI45 despite legacy metadata. Original acquisition dates unknown. Supplement, final public code and README retrieved 2026-11-14; public Dataset.csv checksum equals local d0107a.csv.')
comparisons <- c(historical='source_reconstruction',linked='one_to_one_linkage',
 gender='linkage_and_binary_sex',scoring='linkage_sex_and_official_weights',
 recovery='conditional_cooperation_recovery')
audit_0107 <- list()
for(which in names(matrices)) audit_0107[[which]] <- audit_compare_matrix('0107','0107a',
 matrices[[which]],259,note,names(source_hashes),
 '0107 Methods pp.2-4, Table 2 p.5; original source item/score arithmetic; official BESSI v1 weights',
 status=if(which=='historical')'LEGACY_ERROR_RECONSTRUCTED' else if(which=='recovery')'NOT_ADOPTED_BY_USER' else if(which=='scoring')'SOURCE_CORRECTED_AFTER_REQUESTED_VERIFICATION' else 'APPROVED_LINKAGE_AND_SEX',
 comparison=comparisons[[which]])
summary_0107 <- dplyr::bind_rows(lapply(audit_0107,`[[`,'summary'))
print(summary_0107[,c('matrix_id','comparison','input_rows','changed_pairs','added_variables','pair_n_min','pair_n_max')])
if(!'--check-only' %in% args) {
 source('R/codebook_workflow.R',local=TRUE);source('R/extraction_helpers.R',local=TRUE)
 source('R/workflow_io.R',local=TRUE);source('R/workflow_results.R',local=TRUE)
 old <- readr::read_csv('data/3.meta_data/extraction/historical_audit.csv',show_col_types=FALSE)
 first <- audit_0107$scoring$summary
 first$comparison <- 'approved_integration_2026_11_14';first$status <- 'REVIEWED_BEFORE_AFTER'
 if(any(old$matrix_id=='0107a' & old$comparison==first$comparison))first <- first[FALSE,]
 audit_save_summary(dplyr::bind_rows(summary_0107,first))
 individual_data <- cbind(matrices$scoring,age=data_analytic$survey.1.player.age,
  gender_original=data_analytic$survey.1.player.gender)
 checks <- extraction_check_matrix(matrices$scoring,audit_0107$scoring$candidate,audit_0107$scoring$pair_n)
 wf_compact_extraction('0107','0107a',individual_data,
  audit_0107$scoring$candidate,audit_0107$scoring$pair_n,checks)
}
