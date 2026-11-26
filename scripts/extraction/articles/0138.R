# 0138 — Kramer et al. (2026), Self-Efficacy Predicts Differential
# Neuroticism Change in a Smartphone-Based Intervention.
# Published observed-Pearson comparison; no individual-data reconstruction.
# User approves .10 -> .16 on 2026-11-15. Normal run exports the corrected
# published matrix and audit only; --check-only writes nothing. No fake raw
# dataset, pair-N file or individual-data extraction summary is generated.
source('R/historical_audit.R')
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% '--check-only'))
source_hashes <- c(
 'data/2.full_abstracting/pdfs/0138.pdf'='aa30f5b5b7dbfde471619ab6b65ef82c6b10da7e59ff9454d5e657d549a354a2',
 'data/3.meta_data/open_data/raw/0138/supplement_feasibility.pdf'='0859c9aacc93e07fe8d6cd74ff5c6195c72725a949831e97c110459ad7018dfc',
 'data/3.meta_data/open_data/raw/0138/02_analyses.html'='b17ed127b93146e1ee06cb8b5ffbe6c7db5df7e5a3dbb4e3db7466ee4dd5b849')
audit_verify_sources(source_hashes)

# Figure S3, supplement p.13, T1 PRE-intervention panel. Take BFI-2
# neuroticism, not the second B5T measure of the same construct. Retain IAT
# separately. Self-efficacy x growth mindset is .16, NOT historical .10.
# Public code explicitly selects wave=='T1' and uses Hmisc::rcorr Pearson
# pairwise complete pairs. No printed pair-N matrix is available.
variables <- c('neuroticism','neuroticismiat','emotionalresilience','selfefficacy','growthmindset')
published <- c(.27,-.74,-.24,-.41,-.21,.43,-.12,-.11,.14,.16)
reference <- diag(5)
reference[upper.tri(reference)] <- published
reference[lower.tri(reference)] <- t(reference)[lower.tri(reference)]
dimnames(reference) <- list(variables,variables)
saved <- as.matrix(readxl::read_excel('data/3.meta_data/matrices/0138a.xlsx'))
rownames(saved) <- colnames(saved)
stopifnot(identical(colnames(saved),variables))
delta <- abs(saved[upper.tri(saved)]-published)
stopifnot(all(delta[-10]<1e-10),saved['selfefficacy','growthmindset'] %in% c(.10,.16))
note <- paste('Figure S3 T1 independently checked: nine of ten historical coefficients match. User-approved 2026-11-15 correction: self-efficacy x growth mindset is .16, replacing the historical .10; all other coefficients and N=446 unchanged.',
 'All associations are observed between-person Pearson correlations before intervention, not latent change or later-wave associations. BFI-2 is the selected self-report neuroticism measure; IAT is retained separately; duplicate B5T measure is not added.',
 'Reference N=446; pair-specific Ns are not printed and are not fabricated. Individual data require a SWISSUbase data-use contract; public code/HTML and supplement were retrieved 2026-11-15, without executing downloaded code or accepting a data contract.')
records <- readr::read_csv('data/3.meta_data/coding/records.csv',col_types=readr::cols(.default='c'))
summary_0138 <- data.frame(paper_id='0138',matrix_id='0138a',comparison='published_baseline_transcription',
 reference_n=446,recorded_n=as.numeric(records$n[match('0138a',records$matrix_id)]),
 input_rows=NA_integer_,variables=5L,compared_pairs=10L,changed_pairs=sum(delta>1e-10),
 max_abs_delta=max(delta),pair_n_min=NA_integer_,pair_n_max=NA_integer_,
 added_variables='',removed_variables='',status='APPROVED_PUBLISHED_COEFFICIENT_CORRECTION',note=note,
 evidence='0138 supplement Figure S3 p.13 T1; public analyses section 3.1.1.2; main PDF data availability',
 sources_sha256=paste(names(source_hashes),source_hashes,sep='=',collapse=';'),
 published_pairs_checked=10L,published_pairs_outside_rounding=sum(delta>1e-10))
# Keep the original error visible on every rerun, independently of whether
# the corrected output is already present. Preserve a separate current check.
historical_summary_0138 <- summary_0138
historical_summary_0138$comparison <- 'historical_transcription_before_correction'
historical_summary_0138$changed_pairs <- 1L
historical_summary_0138$max_abs_delta <- .06
historical_summary_0138$published_pairs_outside_rounding <- 1L
summary_0138 <- dplyr::bind_rows(historical_summary_0138,summary_0138)
print(summary_0138[,c('matrix_id','comparison','compared_pairs','changed_pairs','max_abs_delta','status')])
if(!'--check-only' %in% args) {
 source('R/codebook_workflow.R',local=TRUE);source('R/workflow_io.R',local=TRUE)
 labelled <- data.frame(variable=variables,reference,check.names=FALSE)
 wf_commit(setNames(list(labelled),'data/3.meta_data/matrices/0138a.csv'),
  setNames(list(as.data.frame(reference)),'data/3.meta_data/matrices/0138a.xlsx'))
 summary_0138$changed_pairs[summary_0138$comparison=='published_baseline_transcription'] <- 0L
 summary_0138$max_abs_delta[summary_0138$comparison=='published_baseline_transcription'] <- 0
 summary_0138$published_pairs_outside_rounding[summary_0138$comparison=='published_baseline_transcription'] <- 0L
 audit_save_summary(summary_0138)
}
