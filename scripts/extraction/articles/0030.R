# 0030: historical verification only; no scientific output is promoted.
# Run from the repository root. --check-only writes nothing; a normal run
# updates only the single extraction/historical_audit.csv.
source("R/historical_audit.R")
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% "--check-only"))
source_hashes <- c(
  "data/3.meta_data/open_data/d0030a.csv"="ebc474e72e7256de98f4c70c4f8d457a62d8527cd7ef5f0c385a246f45582d94",
  "data/3.meta_data/open_data/d0030b.csv"="3d0105ef82f2bc5d4d3c781ed440885e3f747954052565623582738aba9ae8b2",
  "data/3.meta_data/open_data/opencode0030a.R"="7f8d8a824cf46e5582ac3e50e2b02285fb8c85da12174272fe1249ee8b5aeeb3",
  "data/3.meta_data/open_data/opencode0030b.R"="5206fd865cfcff95fe059b7fbd13a37bbcd3de85fad5e93f53580cd98d0de985",
  "data/2.full_abstracting/pdfs/0030.pdf"="f8b8408506f178fc29715657020aa4f9bc5e5f31006eca0bd5a7b3ea5b4a14d7",
  "data/3.meta_data/open_data/raw/shared/bessi_scoring_v1.pdf"="9d842f5196f021131be8b6561cc70b17dd0deebdaca2cbdc2f3579dce8e0b2a9")
audit_verify_sources(source_hashes)

source("scripts/extraction/keys/bessi192.R",local=TRUE)

# 1-3. Study 1 N=990; Study 2 N=824 after exclusions (PDF pp.4,9-10).
# The supplied files already exclude careless/incomplete respondents.
raw_a <- as.data.frame(readr::read_csv2(names(source_hashes)[1],show_col_types=FALSE))
raw_b <- as.data.frame(readr::read_csv(names(source_hashes)[2],show_col_types=FALSE))
stopifnot(nrow(raw_a)==990,nrow(raw_b)==824)

# 4. Independently score Study 1 BESSI: six positive items per facet, official
# normalized domain weights. The selected Italian translations are already
# represented in bessi_1:192; the ten alternative items are not in this release.
data_a <- raw_a
for(facet in names(bessi192_first_item)) {
  columns <- paste0("bessi_",seq(bessi192_first_item[[facet]],192,32))
  items <- as.matrix(raw_a[,columns])
  stopifnot(all(is.na(items) | (items>=1 & items<=5)))
  value <- rowMeans(items,na.rm=TRUE) # Explicit rule in the author scoring code.
  value[rowSums(!is.na(items))==0] <- NA_real_
  data_a[[facet]] <- value
}
for(domain in names(bessi192_domain_weights)) {
  weights <- bessi192_domain_weights[[domain]]
  facets <- as.matrix(data_a[,names(weights)])
  data_a[[domain]] <- rowSums(sweep(facets,2,weights,"*"),na.rm=TRUE) /
    rowSums(sweep(!is.na(facets),2,weights,"*"))
}
stopifnot(all(raw_a$sex %in% c(1,2)),all(raw_b$sex %in% c(1,2)))
data_a$sex <- raw_a$sex-1
data_a$age <- 2022-raw_a$birth # Collection year used by the author code.
matrix_a <- data_a[,c(names(bessi192_first_item),names(bessi192_domain_weights),"sex")]

# 5. Study 2 supplies scores, not original items. Check its domain arithmetic
# independently against the 32 supplied facets; do not claim item reconstruction.
variable_map_b <- c(
  "SMD_GR"="goalregulation",
  "SMD_TaM"="taskmanagement",
  "SMD_DMS"="decisionmakingskill",
  "SMD_DM"="detailmanagement",
  "SMD_CC"="capacityforconsistency",
  "SMD_OS"="organizationalskill",
  "SMD_TiM"="timemanagement",
  "SMD_RM"="responsibilitymanagement",
  "SMD_RFS"="rulefollowingskill",
  "SMD"="selfmanagement",
  "IND_ATS"="abstractthinkingskill",
  "IND_CS"="creativeskill",
  "IND_IPS"="informationprocessingskill",
  "IND_CC"="culturalcompetence",
  "IND_AS"="artisticskill",
  "IND"="innovation",
  "COD_PTS"="perspectivetakingskill",
  "COD_CSW"="capacityforsocialwarmth",
  "COD_TS"="teamworkskill",
  "COD_EC"="ethicalcompetence",
  "COD_CT"="capacityfortrust",
  "COD"="cooperation",
  "SED_LS"="leadershipskill",
  "SED_ES"="expressiveskill",
  "SED_CS"="conversationalskill",
  "SED_PS"="persuasiveskill",
  "SED_ER"="energyregulation",
  "SED"="socialengagement",
  "ESD_SR"="stressregulation",
  "ESD_CO"="capacityforoptimism",
  "ESD_CR"="confidenceregulation",
  "ESD_IR"="impulseregulation",
  "ESD_AM"="angermanagement",
  "ESD"="emotionalresilience",
  "SRS"="selfreflectionskill",
  "CI"="capacityforindependence",
  "AD"="adaptability",
  "BF_C"="conscientiousness",
  "BF_O"="openness",
  "BF_A"="agreeableness",
  "BF_E"="extraversion",
  "BF_N"="neuroticism",
  "GSES"="selfefficacy",
  "GPS"="procrastination",
  "suppression"="suppressionstrat",
  "reappraisal"="reappraisalstrat")
data_b <- raw_b
names(data_b)[match(names(variable_map_b),names(data_b))] <- unname(variable_map_b)
data_b$sex <- raw_b$sex-1
data_b$age <- raw_b$age
for(domain in names(bessi192_domain_weights)) {
  weights <- bessi192_domain_weights[[domain]]
  value <- as.numeric(as.matrix(data_b[,names(weights)]) %*% weights/sum(weights))
  stopifnot(max(abs(value-data_b[[domain]]))<1e-12)
}
matrix_b <- data_b[,c(unname(variable_map_b),"sex")]

# 6. The paper explicitly says 83 adults from Study 1 were recontacted (p.10).
# No participant IDs survive in the score release. There are 68 unique identical
# 32-facet profiles, also agreeing on sex, all in the adult group. This is a
# diagnostic of possible retained overlap, not an authorized identity-based
# exclusion. User decision 2026-11-13: keep both samples and reference Ns
# unchanged despite the small overlap; retain the documented dependence caveat.
profile_key <- function(x) apply(x[,names(bessi192_first_item)],1,
  function(row)paste(sprintf("%.8f",row),collapse="|"))
keys_a <- profile_key(matrix_a)
links <- match(profile_key(matrix_b),keys_a)
matched <- !is.na(links)
stopifnot(!anyDuplicated(keys_a),sum(matched)==68L,
  all(raw_b$group[matched]=="adult"),all(matrix_b$sex[matched]==matrix_a$sex[links[matched]]))
overlap_note <- "Paper p.10 reports 83 Study 1 adults recontacted for Study 2. Released data have 68 identical unique 32-facet profiles agreeing on sex, all adult; no shared participant IDs are available. User decision 2026-11-13: retain both samples and reference Ns unchanged despite the small overlap. No participants removed or correlations changed; possible sample dependence remains documented."

# 7-8. Read-only numerical and source comparisons.
audit_0030a <- audit_compare_matrix("0030","0030a",matrix_a,990,
  note=paste("All 32 BESSI facets reconstructed from items and five domains from official normalized weights. Age kept outside the matrix. Selection of translated alternatives and upstream exclusions not reconstructed.",overlap_note),
  sources=names(source_hashes),evidence="0030 PDF pp.4-5,9-10; official BESSI key; inspected author scoring code",
  status="MATRIX_REPRODUCED_OVERLAP_RETAINED_BY_DECISION")
audit_0030b <- audit_compare_matrix("0030","0030b",matrix_b,824,
  note=paste("Supplied scores reproduce the matrix; all five domains equal official weighted means of supplied facets. Study 2 item scoring cannot be independently reconstructed. Pair Ns 787-824; missing outcome blocks retained.",overlap_note),
  sources=names(source_hashes),evidence="0030 PDF pp.9-10; supplied facet/domain scores; official BESSI key",
  status="MATRIX_REPRODUCED_OVERLAP_RETAINED_BY_DECISION")
summary_0030 <- dplyr::bind_rows(audit_0030a$summary,audit_0030b$summary)
print(summary_0030[,c("matrix_id","changed_pairs","max_abs_delta","pair_n_min","pair_n_max")])
if(!"--check-only" %in% args)audit_save_summary(summary_0030)
