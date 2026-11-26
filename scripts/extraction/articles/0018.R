# 0018: read-only historical verification. No scientific output is promoted.
# Run: Rscript scripts/extraction/articles/0018.R [--check-only]
# A normal run updates only extraction/historical_audit.csv.
source("R/historical_audit.R")
args <- commandArgs(trailingOnly = TRUE)
stopifnot(all(args %in% "--check-only"))
source_hashes <- c(
  "data/3.meta_data/open_data/d0018.csv" = "10bd0be71a0d81e70b0e9c13c43e8da3f2bba8d9afb26c454a73080d33f201a9",
  "data/2.full_abstracting/pdfs/0018.pdf" = "4b26d58b32a69341ba26f45b42928ec4aa14ab8908aa0df917bfd737dd060861",
  "data/3.meta_data/open_data/raw/shared/bessi_scoring_v1.pdf" = "9d842f5196f021131be8b6561cc70b17dd0deebdaca2cbdc2f3579dce8e0b2a9")
audit_verify_sources(source_hashes)

source("scripts/extraction/keys/bessi192.R")
# 1-3. Author-selected sample, PDF pp.3-4: N=2965, including 359 SLD students.
# Raw exclusions precede the supplied file and are not independently reproduced.
raw_items <- as.data.frame(readr::read_csv(names(source_hashes)[1],show_col_types=FALSE))
stopifnot(nrow(raw_items)==2965, sum(raw_items$dsa=="dsa")==359,
  sum(raw_items$dsa=="typical")==2606)
data_scored <- raw_items

# 4. BESSI-192: six positively keyed items per facet (official key p.1).
# All supplied BESSI items are observed, so no missing-item threshold is inferred.
item_columns <- paste0("bessi_",1:192)
stopifnot(!anyNA(raw_items[item_columns]), all(as.matrix(raw_items[item_columns])>=1),
  all(as.matrix(raw_items[item_columns])<=5))
for (facet in names(bessi192_first_item)) {
  columns <- paste0("bessi_",seq(bessi192_first_item[[facet]],192,32))
  data_scored[[facet]] <- rowMeans(raw_items[,columns,drop=FALSE])
}
# Domains: mean of facets, with half weight for interstitial facets (key p.2).
for (domain in names(bessi192_domain_weights)) {
  weights <- bessi192_domain_weights[[domain]]
  facets <- as.matrix(data_scored[,names(weights),drop=FALSE])
  data_scored[[domain]] <- as.vector(facets %*% weights / sum(weights))
}
# SWLS: mean of the five available responses, as in the historical extraction.
# PDF p.4 explains 513 entirely missing SWLS records caused by a technical issue.
swls_items <- paste0("SWLS_",1:5)
stopifnot(sum(rowSums(!is.na(raw_items[swls_items]))==0)==513)
data_scored$satisfactionwithlife <- rowMeans(raw_items[swls_items],na.rm=TRUE)
data_scored$satisfactionwithlife[is.nan(data_scored$satisfactionwithlife)] <- NA_real_
data_scored$academicachievement <- rowMeans(raw_items[,c("math","ita")])
data_scored$sex <- raw_items$sex - 1
stopifnot(all(data_scored$sex %in% c(0,1)))
data_scored$age <- raw_items$birth

# 5-7. Separate the two groups before calculating the matrices.
matrix_variables <- c(names(bessi192_first_item),names(bessi192_domain_weights),
  "satisfactionwithlife","academicachievement","sex")
audit_0018 <- list()
for (sample in c(a="dsa",b="typical")) {
  suffix <- if(sample=="dsa") "a" else "b"
  selected <- data_scored$dsa==sample
  data_matrix <- data_scored[selected,matrix_variables,drop=FALSE]
  audit_0018[[suffix]] <- audit_compare_matrix("0018",paste0("0018",suffix),
    data_matrix,if(suffix=="a")359 else 2606,
    note="Item-level BESSI scoring reconstructed with the official weighted key; all BESSI items observed. Two disjoint groups. SWLS missingness explains smaller pair Ns. Upstream author exclusions not reconstructed.",
    sources=names(source_hashes),evidence="0018 PDF pp.3-4; BESSI v1.0 scoring key pp.1-2",
    status="BESSI_AND_MATRIX_REPRODUCED")
}
# 8. Save only aggregate comparisons; retain candidate data in memory.
summary_0018 <- dplyr::bind_rows(lapply(audit_0018, `[[`, "summary"))
print(summary_0018[,c("matrix_id","input_rows","changed_pairs","max_abs_delta","pair_n_min","pair_n_max")])
if (!"--check-only" %in% args) audit_save_summary(summary_0018)
