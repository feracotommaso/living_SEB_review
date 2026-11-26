# 0050: reproduce published Study 3 results; never export analytical matrices.
# Run from the repository root. Normal run updates only historical_audit.csv.
# --check-only writes nothing. --legacy-diagnostics tests seven discrepant fits
# and two controls with lavaan 0.6-10, in an explicitly supplied isolated library.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% c("--check-only","--legacy-diagnostics")))
replication_library <- Sys.getenv("LIVING_SEB_REPLICATION_LIBRARY")
if(nzchar(replication_library)) .libPaths(c(replication_library,.libPaths()))
suppressPackageStartupMessages({
  library(dplyr);library(tidyr);library(purrr);library(stringr)
  library(glue);library(crayon);library(lavaan)
})
source("R/historical_audit.R")
source_hashes <- c(
  "data/3.meta_data/open_data/d0050.Rdata"="229c2fee55a3c3e578c8216eaa3c31b8b6f850dbc006ddb41a087e051cfc1cbd",
  "data/2.full_abstracting/pdfs/0050.pdf"="c479f900ed63c77541814ec7074de1ecca0d98d3a109bf115aa938ace5ac1dea",
  "data/3.meta_data/open_data/raw/0050/german/files/german/03_study3.R"="ba07bc3f5b31f7cad97e4c107a9f87f725e56e1224f7e772239bfda5c9b500de",
  "data/3.meta_data/open_data/raw/0050/german/files/german/02_results/study3_results.Rdata"="6ebf7708d89d4052e4037c4a46a48887e5bb6e00f1c56700f352da49098ec86c",
  "data/3.meta_data/open_data/raw/0050/german/german.zip"="ea50d51019f5ac154a5aa063f8d44b162213adaf487e35b242e24f4b9dd146f9")
audit_verify_sources(source_hashes)
raw <- new.env();load(names(source_hashes)[1],envir=raw)
reference <- new.env();load(names(source_hashes)[4],envir=reference)
reference_facets <- reference$study3_facet_cors

# Only the inspected function assignment is evaluated. Do not source the full
# author script (workspace reset, package setup, parallel workers and file writes).
definition <- Filter(function(x)is.call(x) && identical(x[[1]],as.name("<-")) &&
  identical(x[[2]],as.name("pim_generator")),as.list(parse(names(source_hashes)[3])))
stopifnot(length(definition)==1L)
eval(definition[[1]])
trait_data <- raw$adults_teens_t0_t1_t2
iq_data <- left_join(raw$adults_teens_t0,
  dplyr::select(raw$adults_teens_t1,id,datedays,starts_with("IQ")),by="id") |>
  filter(datedays<=as.Date("2021-02-06"))
outcome_keys <- c(raw$b5_lookup,
  list(gf=c("IQVR","IQLN","IQMR"),gc=paste0("IQGCp",1:3)))

# These are fixed diagnostic cases, not a rule for selecting published effects.
if("--legacy-diagnostics" %in% args) {
  stopifnot(as.character(packageVersion("lavaan"))=="0.6.10")
  pairs <- data.frame(facet=c("cons","cons","cons","resp","tafo","ener","stre","lead","curi"),
    outcome=c("b5_agre_res","b5_agre_tru","b5_emos_vol","b5_agre_res",
      "b5_cons_res","b5_cons_pro","b5_emos_vol","b5_extr","b5_open"))
} else {
  pairs <- expand.grid(outcome=names(outcome_keys),facet=raw$bessi_lookup$scale_name,
    stringsAsFactors=FALSE)[,c("facet","outcome")]
}
fits <- list()
for(i in seq_len(nrow(pairs))) {
  facet <- pairs$facet[i];outcome <- pairs$outcome[i]
  key <- raw$bessi_lookup[match(facet,raw$bessi_lookup$scale_name),]
  ref <- match(key$skill,reference_facets$bessi_facet);stopifnot(!is.na(ref))
  is_iq <- outcome %in% c("gf","gc")
  scores <- if(is_iq)list(iq=outcome_keys[[outcome]],bessi=key$items[[1]])else
    list(bfi=outcome_keys[[outcome]],bessi=key$items[[1]])
  model <- pim_generator(scores,score_function="mean")
  if(!is_iq)model <- paste0(model,"bessi ~~ start(0.6)*bessi; bfi ~~ start(0.6)*bfi;")
  warnings <- character()
  fit <- withCallingHandlers(
    lavaan::cfa(model,data=if(is_iq)iq_data else trait_data,estimator="MLR",missing="FIML"),
    warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")})
  value <- lavaan::lavInspect(fit,"cor.lv")[2,1]
  converged <- lavaan::lavInspect(fit,"converged")
  # A returned coefficient is NOT usable merely because it resembles the paper.
  fits[[i]] <- data.frame(facet,outcome,table=if(is_iq)8 else if(outcome %in%
    c("b5_open","b5_cons","b5_extr","b5_agre","b5_emos"))5 else 6,
    occasion=if(is_iq)"T0"else"T2",r=value,author=reference_facets[[outcome]][ref],
    converged,usable=converged && is.finite(value) && abs(value)<=1,
    model_n=as.numeric(lavInspect(fit,"nobs")),method="PIM / MLR / FIML",
    warnings=paste(unique(warnings),collapse=" | "))
  if(i%%44L==0L)cat("PIM fits:",i,"/",nrow(pairs),"\n")
}
publication_effects <- bind_rows(fits)

if(!"--legacy-diagnostics" %in% args) {
  # Exact author formula, including supplied T2 _prm means from the initial key.
  # Replication reproduces this source choice; it does not silently replace it
  # with revised means or harmonized interstitial weights.
  domain_key <- list(
    self_management=c("tafo","time","deta","orga","resp","cons","goal","rule","deci"),
    social_engagement=c("lead","pers","conv","expr"),cooperation=c("team","trus","empa","soha"),
    emotional_resilience=c("stre","opti","angm","scon"),innovation=c("crea","curi","arti","cult"))
  for(d in names(domain_key))trait_data[[d]] <- rowMeans(trait_data[,paste0(domain_key[[d]],"_prm")])
  columns <- c(names(domain_key),paste0(c("b5_cons","b5_extr","b5_agre","b5_emos","b5_open"),"_prm"),"IQGF","IQGC")
  domain_cor <- psych::corFiml(trait_data[,columns])
  domain_observed_rows <- sum(rowSums(!is.na(trait_data[,columns]))>0)
  for(d in names(domain_key))for(outcome in columns[6:12]) {
    publication_effects <- bind_rows(publication_effects,data.frame(
      facet=d,outcome,table=if(outcome %in% c("IQGF","IQGC"))9 else 7,
      occasion="T2",r=domain_cor[d,outcome],
      author=reference$study3_domain_cors_prm[d,outcome],
      converged=NA,usable=is.finite(domain_cor[d,outcome]) && abs(domain_cor[d,outcome])<=1,
      model_n=domain_observed_rows,method="FIML on source domain/trait scores",warnings=""))
  }
  # Direct printed Table 9 check: domain IQ uses T2, unlike Table 8 facet IQ.
  printed_table9 <- matrix(c(.05,.07,-.02,-.02,.01,-.07,-.04,-.01,.08,.01),5,byrow=TRUE)
  stopifnot(max(abs(domain_cor[names(domain_key),c("IQGF","IQGC")]-printed_table9))<=.00500001)
}
publication_effects$delta_author <- publication_effects$r-publication_effects$author
# Distinguish small cross-version optimizer differences from substantive
# discrepancies. Printed rounding is checked separately, including boundaries.
publication_effects$within_numeric_tolerance <- abs(publication_effects$delta_author)<=1e-3
publication_effects$within_printed_rounding <- abs(publication_effects$r-round(publication_effects$author,2))<=.00500001
publication_summary <- publication_effects |>
  group_by(table) |> summarise(pairs=n(),compatible=sum(within_numeric_tolerance),
    printed_rounding_matches=sum(within_printed_rounding),unconverged=sum(!converged,na.rm=TRUE),
    max_abs_delta=max(abs(delta_author)),.groups="drop")
print(publication_summary)
print(publication_effects[!publication_effects$within_numeric_tolerance |
  !publication_effects$usable,c("facet","outcome","r","author","converged","usable")],row.names=FALSE)

if(!"--check-only" %in% args) {
  source("R/workflow_io.R",local=TRUE)
  provenance <- paste(names(source_hashes),source_hashes,sep="=",collapse=";")
  version <- as.character(packageVersion("lavaan"))
  legacy <- "--legacy-diagnostics" %in% args
  rows <- lapply(seq_len(nrow(publication_summary)),function(i) {
    x <- publication_summary[i,]
    data.frame(paper_id="0050",matrix_id="0050b",
      comparison=paste0("publication_table",x$table,"_lavaan_",version,if(legacy)"_diagnostics"else""),
      status=if(x$unconverged>0)"NONCONVERGED_SOURCE_VERSION_FITS"else
        if(x$compatible<x$pairs)"PUBLICATION_VALUES_NOT_FULLY_REPRODUCED"else"PUBLICATION_VALUES_REPRODUCED",
      published_pairs_compared=x$pairs,
      published_pairs_outside_rounding=x$pairs-x$printed_rounding_matches,
      numerically_compatible_pairs=x$compatible,unconverged_models=x$unconverged,
      max_abs_delta_author_saved=x$max_abs_delta,
      note=paste("Publication-only replication; no analytical matrix exported. lavaan",version,
        "; cross-version numeric tolerance 0.001; printed rounding 0.005. PIM facet-BFI uses T2; PIM facet-IQ uses T0; domain FIML uses T2 source scores. Nonconvergent numbers are diagnostic, not approved effects."),
      evidence="0050 Tables 5-9 and original 03_study3.R; immutable saved results independently checked against PDF.",
      sources_sha256=provenance)
  })
  audit_save_summary(bind_rows(rows))
}
