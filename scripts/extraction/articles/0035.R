# 0035: historical verification only. No scientific outputs are promoted.
# A normal run updates only extraction/historical_audit.csv.
# --check-only writes nothing. Run from the repository root.
source("R/historical_audit.R")
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% "--check-only"))
source_hashes <- c(
  "data/3.meta_data/open_data/d0035.csv"="e4e915c8ab75980799b60bd383504c2cd81af12d218eb8a3d33a8c61dc5b034b",
  "data/3.meta_data/open_data/opencode0035.R"="b1794a489bd912093b64267b20e7155746db9061fc3c45ff934e90c32442278d",
  "data/2.full_abstracting/pdfs/0035.pdf"="43500fb8cbb803d6632b131e62edcb5f776875737895882970356f776c118e0e")
audit_verify_sources(source_hashes)

# 1-3. PDF p.4: N=642 ACT survey respondents. Only five representative BESSI
# facets were administered (six items each); these are not full domain scores.
raw <- as.data.frame(readr::read_csv(names(source_hashes)[1],show_col_types=FALSE))
stopifnot(nrow(raw)==642)

# 4. Independently reconstruct the five facet means from the supplied item
# blocks, following the explicit author rowMeans(..., na.rm=TRUE) rule.
# Retain rounded supplied scores for the exact historical matrix comparison.
facet_sources <- c(goalregulation="GP",leadershipskill="LD",teamworkskill="CL",
  abstractthinkingskill="IC",stressregulation="SR")
for(facet in names(facet_sources)) {
  prefix <- facet_sources[[facet]]
  items <- as.matrix(raw[,paste0(prefix,1:6)])
  stopifnot(all(is.na(items) | (items>=1 & items<=5)))
  value <- rowMeans(items,na.rm=TRUE);value[is.nan(value)] <- NA_real_
  supplied <- raw[[paste0("AV",prefix)]]
  stopifnot(identical(is.na(value),is.na(supplied)),max(abs(value-supplied),na.rm=TRUE)<5e-10)
}
# BFI-44: source response strings on 1-6, with the authors' explicit key.
# The ES score is Emotional Stability; neuroticism is negated below for r.
response_values <- c("Strongly disagree"=1,"Disagree"=2,"Somewhat disagree"=3,
  "Somewhat agree"=4,"Agree"=5,"Strongly agree"=6)
bfi_key <- list(
  bfi_E=c(1,-6,11,16,-21,26,-31,36),
  bfi_A=c(-2,7,-12,17,22,-27,32,-37,42),
  bfi_C=c(3,-8,13,-18,-23,28,33,38,-43),
  bfi_O=c(5,10,15,20,25,30,-35,40,-41,44),
  bfi_ES=c(-4,9,-14,-19,24,-29,34,-39))
for(scale in names(bfi_key)) {
  key <- bfi_key[[scale]]
  items <- vapply(abs(key),function(i) {
    response <- raw[[paste0("bfi",i)]]
    stopifnot(all(is.na(response) | response %in% names(response_values)))
    unname(response_values[response])
  },numeric(nrow(raw)))
  items[,key<0] <- 7-items[,key<0]
  value <- rowMeans(items,na.rm=TRUE);value[is.nan(value)] <- NA_real_
  stopifnot(identical(is.na(value),is.na(raw[[scale]])),
    max(abs(value-raw[[scale]]),na.rm=TRUE)<5e-10)
}

# 5. Existing scientific mapping; supplied score rounding is preserved.
variable_map <- c(
  "AVGP"="goalregulation",
  "AVLD"="leadershipskill",
  "AVCL"="teamworkskill",
  "AVIC"="abstractthinkingskill",
  "AVSR"="stressregulation",
  "bfi_C"="conscientiousness",
  "bfi_O"="openness",
  "bfi_A"="agreeableness",
  "bfi_E"="extraversion",
  "bfi_ES"="neuroticism",
  "RPTD_STDSCN_C"="standardizedacademicachievement")
data_scored <- raw
names(data_scored)[match(names(variable_map),names(data_scored))] <- unname(variable_map)
data_scored$neuroticism <- -data_scored$neuroticism # Direction, not a 1-6 item score.
data_matrix <- data_scored[,unname(variable_map)]
note <- "Released cohort N=642 matches paper p.4. Five six-item BESSI facets and all five BFI-44 scores independently reconstructed to supplied CSV rounding precision. BESSI scores available for 480-482 respondents; existing pair Ns 471-603 reflect incomplete blocks. Preserve reference N=642 and missingness; no complete-case exclusions. Original ACT score and supplied score precision retained."

# 6-8. Baseline plus a separate proposal to include available sex.
audit_0035 <- list()
audit_0035$source <- audit_compare_matrix("0035","0035a",data_matrix,642,
  note=note,sources=names(source_hashes),evidence="0035 PDF p.4 and Table 2 p.7; inspected original author scoring code",
  status="ITEM_SCORES_AND_MATRIX_REPRODUCED")
stopifnot(all(is.na(raw$GENDER) | raw$GENDER %in% c("male","female")))
sex_candidate <- data_matrix
sex_candidate$sex <- unname(c(male=0,female=1)[raw$GENDER])
audit_0035$sex <- audit_compare_matrix("0035","0035a",sex_candidate,642,
  note=paste(note,"Proposal only: include available GENDER as sex (0 male, 1 female); 468 females, 159 males, 15 missing. Adds eleven correlations, without changing existing coefficients. Not exported."),
  sources=names(source_hashes),evidence="Original GENDER strings; PDF p.4 female percentage",
  status="VARIABLE_ADDITION_REVIEW_REQUIRED",comparison="sex_augmentation")
summary_0035 <- dplyr::bind_rows(lapply(audit_0035,`[[`,"summary"))
print(summary_0035[,c("matrix_id","comparison","changed_pairs","added_variables","pair_n_min","pair_n_max")])
if(!"--check-only" %in% args)audit_save_summary(summary_0035)
