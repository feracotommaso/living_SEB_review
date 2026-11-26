# 0128 — Springstein, Bankston & English (2026), To Regulate or Not to
# Regulate? Situational and Individual Differences in Emotion Regulation
# Initiation in Daily Life. Published-table check, NOT individual-data scoring.
# Normal run updates the audit only; --check-only writes nothing.
source('R/historical_audit.R')
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% '--check-only'))
source_hashes <- c(
 'data/2.full_abstracting/pdfs/0128.pdf'='475e49ee175049e52556e8d2da16cdf1e8908cdb8c1d1306d9387907bd54a453',
 'data/3.meta_data/open_data/raw/0128/NoRegOSF.Rmd'='002c5836cfec5ea79d98d914207003df961a7ae1eb2da1b727e9d1b9062ce509',
 'data/3.meta_data/open_data/raw/0128/noreg_osf_vars.csv'='5abbdcc8534eb6dcffd751f9a5754121ab0d42a733008eb4630989fe4d726d48')
audit_verify_sources(source_hashes)

# Table 2, PDF p.8: BELOW diagonal = between-person associations. The upper
# triangle contains within-person associations and must never be substituted.
# Author code uses psych::statsBy(..., group=PID); the underlying meel_esm.csv
# and meel_trait.csv are not in the public OSF deposit. No pair Ns are inferred.
variables <- c('emotionregulationinitiation','emotionregulationinitiationfrequency',
 'pleasantsituation','familiarsituation','controllablesituation','socialsituation',
 'sociallywarmsituation','sociallydominantsituation','positiveaffect','negativeaffect',
 'neuroticism','distressintolerance','emotioncontrollability','emotionalresilience','sex')
published <- c(
 .27,
 .02,.16,
 -.02,.22,.73,
 -.02,.23,.82,.81,
 -.06,-.07,.15,.10,.10,
 -.02,.06,.52,.61,.57,.25,
 .29,.16,-.07,-.03,-.05,-.21,-.28,
 .17,.20,.80,.55,.62,.15,.40,.03,
 .53,.29,-.15,-.09,-.13,-.23,-.21,.51,.03,
 .13,-.08,-.20,-.08,-.15,-.10,-.01,.00,-.19,.29,
 .22,-.03,-.15,-.15,-.11,-.07,-.07,.10,-.12,.24,.67,
 -.14,-.04,.08,.01,.04,.15,.01,-.21,-.01,-.19,-.23,-.28,
 -.06,.17,.26,.15,.22,.03,.05,.01,.25,-.18,-.73,-.54,.24,
 -.02,.09,.05,.04,-.01,.02,-.12,.16,.10,.02,-.25,-.16,.07,.19)
reference <- diag(length(variables))
reference[upper.tri(reference)] <- published
reference[lower.tri(reference)] <- t(reference)[lower.tri(reference)]
dimnames(reference) <- list(variables,variables)
# Historical sign reversal is reproducible, but changes the contrast to
# NOT-CIS-MAN versus CIS-MAN. It does NOT recover female versus male coding.
historical_reference <- reference
historical_reference['sex',-15] <- -reference['sex',-15]
historical_reference[-15,'sex'] <- -reference[-15,'sex']
saved <- as.matrix(readxl::read_excel('data/3.meta_data/matrices/0128a.xlsx'))
rownames(saved) <- colnames(saved)
stopifnot(identical(colnames(saved),variables))
delta <- abs(saved-historical_reference)
stopifnot(max(delta)<1e-10)
note <- paste('All 105 historical coefficients reproduce the between-person lower triangle of Table 2 after the historical cis-man sign reversal; the 91 non-sex coefficients match directly.',
 'User decision 2026-11-15: RETAIN HISTORICAL SEX CONTRAST WITH NOTE. Source is cis-man (0=no, 1=yes), including other gender identities in no; historical inversion is not a female-male contrast. Retain all 14 sex correlations and N=215 unchanged. No individual data are publicly deposited to recode binary sex.',
 'Published percentages imply 162 cisgender women and 47 cisgender men of 215; six remaining participants are not identified as cisgender women or men in the listed categories. These include about two non-binary and one transgender participant (gender unspecified); the listed percentages leave 1.39% unaccounted for. Do not describe all six as non-female or infer individual identities. The historical non-cis-man category therefore cannot be interpreted as women only.',
 'N=215 after one of 216 recruited participants completed no ESM prompts; no compliance-based exclusion was applied. Public code and variable list are available, but meel_esm.csv and meel_trait.csv are absent. No individual scores or pair Ns reconstructed. Within-person upper-triangle values are not pooled as between-person correlations.')
records <- readr::read_csv('data/3.meta_data/coding/records.csv',col_types=readr::cols(.default='c'))
summary_0128 <- data.frame(paper_id='0128',matrix_id='0128a',comparison='published_between_person_transcription',
 reference_n=215,recorded_n=as.numeric(records$n[match('0128a',records$matrix_id)]),
 input_rows=NA_integer_,variables=15L,compared_pairs=105L,changed_pairs=0L,
 max_abs_delta=max(delta),pair_n_min=NA_integer_,pair_n_max=NA_integer_,
 added_variables='',removed_variables='',status='SEX_CONTRAST_RETAINED_BY_USER_WITH_NOTE',note=note,
 evidence='0128 PDF pp.4-6 sample and cis-man coding; Table 2 p.8; OSF NoRegOSF.Rmd descriptive statsBy call',
 sources_sha256=paste(names(source_hashes),source_hashes,sep='=',collapse=';'),
 published_pairs_checked=105L,published_pairs_outside_rounding=0L)
print(summary_0128[,c('matrix_id','compared_pairs','changed_pairs','status')])
if(!'--check-only' %in% args) audit_save_summary(summary_0128)
