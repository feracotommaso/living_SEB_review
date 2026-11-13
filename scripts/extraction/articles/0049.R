# 0049: read-only historical audit; no scientific outputs are promoted.
# A normal run saves only historical_audit.csv. --check-only writes nothing.
# Paper data/code: https://osf.io/74qnu/. Historical retrieval date unknown.
source("R/historical_audit.R")
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% "--check-only"))
source_hashes <- c(
  "data/3.meta_data/open_data/d0049.xlsx"="8e2464b5249a001f8a0156fc2f4055b9f82abb03b53009d80dbf1737b59de334",
  "data/3.meta_data/open_data/opencode0049.R"="a046de4743e2f3fe5ec297d056f5a8862e55500b62f0a75c7f48dd16b8841b7e",
  "data/2.full_abstracting/pdfs/0049.pdf"="68d878cd008c2138f3d0089a019b593b573c0c989d0e1037b2290d777fafbbc5")
audit_verify_sources(source_hashes)

# 1-3. N=137 consenting medical students. Age was removed for anonymization.
# Author code is inspected, never executed. No new participant exclusions.
raw <- as.data.frame(readxl::read_excel(names(source_hashes)[1],na="NA"))
stopifnot(nrow(raw)==137,all(raw$Gender %in% c(0,1)))
data_scored <- data.frame(sex=raw$Gender) # Source 0 male, 1 female; 88 females.

# 4. BFI-2-S: reverse exactly the author-labelled _R items on 1-5.
bfi_items <- vapply(1:30,function(i) {
  name <- sprintf("BFI%02d",i)
  if(name %in% names(raw))as.numeric(raw[[name]])else 6-as.numeric(raw[[paste0(name,"_R")]])
},numeric(nrow(raw)))
stopifnot(all(is.na(bfi_items) | (bfi_items>=1 & bfi_items<=5)))
# Complete-item means preserve the historical scoring; no new missing threshold.
bfi_key <- list(
  extraversion=c(1,6,11,16,21,26),agreeableness=c(2,7,12,17,22,27),
  neuroticism=c(4,9,14,19,24,29),
  bf_sociability=c(1,16),bf_assertiveness=c(6,21),bf_energy=c(11,26),
  bf_compassion=c(2,17),bf_respectfulness=c(7,22),bf_trust=c(12,27),
  bf_anxiety=c(4,19),bf_depression=c(9,24),bf_emotionalvolatility=c(14,29))
for(scale in names(bfi_key))data_scored[[scale]] <- rowMeans(bfi_items[,bfi_key[[scale]]])
# Six measured BESSI facets; Pers6a is the alternative translation used by authors.
facet_key <- list(
  leadershipskill=paste0("Lead",1:6),persuasiveskill=c(paste0("Pers",1:5),"Pers6a"),
  perspectivetakingskill=paste0("Empa",1:6),capacityforsocialwarmth=paste0("Soha",1:6),
  stressregulation=paste0("Stres",1:6),angermanagement=paste0("Angm",1:6))
for(facet in names(facet_key)) {
  items <- as.matrix(raw[,facet_key[[facet]]])
  stopifnot(all(is.na(items) | (items>=1 & items<=5)))
  data_scored[[facet]] <- rowMeans(items)
}
# These are the paper's two-facet composites, not full BESSI domain batteries.
data_scored$socialengagement <- rowMeans(data_scored[,c("leadershipskill","persuasiveskill")])
data_scored$cooperation <- rowMeans(data_scored[,c("perspectivetakingskill","capacityforsocialwarmth")])
data_scored$emotionalresilience <- rowMeans(data_scored[,c("stressregulation","angermanagement")])
# Observed exercise performance: mean available raters; all missing stays missing.
rater_key <- list(task_socialengagement=paste0("Agency_Observed_R",1:2),
  task_cooperation=paste0("Communion_Observed_R",1:2),
  task_emotionalresilience=paste0("Resilience_Observed_R",1:3))
for(variable in names(rater_key)) {
  value <- rowMeans(raw[,rater_key[[variable]]],na.rm=TRUE)
  value[is.nan(value)] <- NA_real_;data_scored[[variable]] <- value
}
# 5. Historical code used unregistered emotionalvolatility, which was dropped
# by exact codebook selection. Retain that omission only in the baseline.
legacy_variables <- setdiff(names(data_scored),"bf_emotionalvolatility")
data_matrix <- data_scored[,legacy_variables]
note <- paste("N=137 retained, pair Ns=128-136. BFI and six BESSI facet scores independently reconstructed using the inspected author scoring code.",
  "Paper composites use two BESSI facets each; observer ratings remain distinct task variables. Available-rater means and complete questionnaire means preserved.",
  "Historical emotionalvolatility spelling omitted the registered bf_emotionalvolatility. Raw BFI also contains items for the other two domains, outside the paper's focal analyses; their possible use belongs to a separate coding expansion.",
  "Age unavailable for anonymization; no new exclusions.")

# 6-8. Baseline and isolated spelling correction. No new matrices exported.
audit_0049 <- list()
audit_0049$source <- audit_compare_matrix("0049","0049a",data_matrix,137,note,
  names(source_hashes),"0049 PDF pp.5-8 and author scoring syntax",
  status="ITEM_SCORES_AND_MATRIX_REPRODUCED")
volatility_candidate <- data_matrix
volatility_candidate$bf_emotionalvolatility <- data_scored$bf_emotionalvolatility
audit_0049$volatility <- audit_compare_matrix("0049","0049a",volatility_candidate,137,
  paste(note,"Proposal only: recover bf_emotionalvolatility from reverse-keyed BFI14_R and BFI29; adds 24 correlations without changing old coefficients. Not exported."),
  names(source_hashes),"Author BFI key; PDF Table 5 p.7; exact variable codebook name",
  status="VARIABLE_ADDITION_REVIEW_REQUIRED",comparison="emotional_volatility_augmentation")
# Independently read Table 5: selected observed-score references, including the
# omitted facet. The latent/partial-correlation issue of other papers does not apply.
published <- data.frame(
  var1=c("bf_emotionalvolatility","bf_emotionalvolatility","bf_emotionalvolatility",
    "bf_emotionalvolatility","bf_emotionalvolatility","bf_emotionalvolatility",
    "task_emotionalresilience","task_cooperation","task_socialengagement"),
  var2=c("neuroticism","bf_anxiety","bf_depression","emotionalresilience","stressregulation","angermanagement",
    "emotionalresilience","cooperation","socialengagement"),
  r=c(.82,.61,.42,-.58,-.56,-.45,-.13,.10,.31))
calculated <- audit_0049$volatility$candidate[cbind(published$var1,published$var2)]
audit_0049$volatility$summary$published_pairs_compared <- nrow(published)
audit_0049$volatility$summary$published_pairs_outside_rounding <- sum(abs(calculated-published$r)>.00500001)
summary_0049 <- dplyr::bind_rows(lapply(audit_0049,`[[`,"summary"))
print(summary_0049[,c("matrix_id","comparison","changed_pairs","added_variables","pair_n_min","pair_n_max")])
if(!"--check-only" %in% args)audit_save_summary(summary_0049)
