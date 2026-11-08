# 1. Article and sources ----------------------------------------------------
# Napolitano et al. (2026), doi:10.1177/27000710261487580, Main study.
# PDF pp.5-6: BESSI-192 and zero-order correlations; p.5 discloses sample reuse.
# The H-prefixed IDs resolve reuse of Internet sample D, historical matrix 0053d.
# See coding/sources.csv for immutable originals and unknown dates.
source("R/codebook_workflow.R")
source("R/extraction_helpers.R")
source("scripts/extraction/keys/0150.R")
arguments <- extraction_arguments(commandArgs(trailingOnly = TRUE))
paper_id <- "0150"
batch_id <- "2026-11-external-01"
matrix_id <- "0150a"
component_label <- "Main study"
expected_n <- 30719L
audit_dir <- file.path("data/3.meta_data/extraction", paper_id, matrix_id)
extraction_validate_scope(batch_id, paper_id, matrix_id, component_label)

# 2. Import verified originals ---------------------------------------------
manifest <- extraction_originals(paper_id)
raw_items <- as.data.frame(haven::read_sav(
  "data/3.meta_data/open_data/raw/0150/0 Working data file - Copycmn2.sav"))
raw_old_d <- as.data.frame(readxl::read_excel("data/3.meta_data/open_data/d0053d.xlsx"))
items <- paste0("BESSI_", 1:192)
stopifnot(nrow(raw_items) == 31315L, nrow(raw_old_d) == 600L,
  !anyNA(raw_items$Case), !anyDuplicated(raw_items$Case), !anyDuplicated(raw_old_d$Case),
  !anyNA(raw_items[items]), all(as.matrix(raw_items[items]) %in% 1:5))

# 3. Analytic sample --------------------------------------------------------
# Identity links, not fuzzy response matching: H is the Hogwarts survey prefix.
old_d_ids <- paste0("H", as.integer(raw_old_d$Case))
links <- match(old_d_ids, raw_items$Case)
found <- !is.na(links)
stopifnot(sum(found) == 596L,
  all(raw_items$Test[links[found]] %in% c("bessir4w5_hogwarts", "bessir4we5_hogwarts")),
  identical(as.numeric(raw_old_d$Age[found]), as.numeric(raw_items$Age[links[found]])),
  identical(as.character(raw_old_d$Gender[found]), as.character(raw_items$Gender[links[found]])))
item_differences <- rowSums(abs(as.matrix(raw_old_d[found, paste0("bessi_", 1:192)]) -
  as.matrix(raw_items[links[found], items])) > 1e-12)
stopifnot(sum(item_differences == 0L) == 590L, sum(item_differences == 1L) == 6L)
keep <- !raw_items$Case %in% old_d_ids
data_analytic <- raw_items[keep, , drop = FALSE]
stopifnot(nrow(data_analytic) == expected_n, !any(data_analytic$Case %in% old_d_ids))
# No additional exclusions for age, ethnicity, gender category or equal profiles.

# 4. Recoding and scoring ---------------------------------------------------
# Score the complete released cohort for comparison with its stored scores and
# published Table 1, then select the non-overlapping participants for extraction.
full_scores <- as.data.frame(setNames(lapply(facet_start, function(start)
  rowMeans(raw_items[paste0("BESSI_", seq(start, 192, 32))])), names(facet_start)))
facet_difference <- max(abs(as.matrix(full_scores) - as.matrix(raw_items[source_facet_names])))
for (name in names(domain_weights)) {
  weights <- domain_weights[[name]]
  full_scores[[name]] <- as.numeric(as.matrix(full_scores[names(weights)]) %*% weights / sum(weights))
}
domain_difference <- max(abs(as.matrix(full_scores[names(source_domain_names)]) -
  as.matrix(raw_items[unname(source_domain_names)])))
data_scored <- full_scores[keep, , drop = FALSE]
data_scored$sex <- unname(c(Male=0, Female=1)[as.character(data_analytic$Gender)])
data_scored$age <- as.numeric(data_analytic$Age)
# Source category is retained for individual-data uses, not treated as a number
# in the matrix: 1=Female, 2=Male, 3=Other (the authors discuss non-binary gender).
data_scored$gender_original <- as.numeric(data_analytic$Gender3)
stopifnot(all(data_scored$gender_original == unname(c(Female=1, Male=2, Other=3)[data_analytic$Gender])))

# 5. Canonical variable names ----------------------------------------------
variable_map <- codebook_resolve_variables(matrix_variables)
variable_map$source <- "Standard BESSI-192; original SAV; PDF p.5; explicit keys/0150.R"
# Source auxiliary categorical demographics stay in the preserved original;
# they are not new numeric outcome constructs. See 0150_decisions.csv.

# 6. Scientific checks ------------------------------------------------------
stopifnot(length(facet_start) == 32L, length(matrix_variables) == 38L,
  facet_difference < 1e-12, domain_difference < 1e-12,
  all(as.matrix(full_scores) >= 1 & as.matrix(full_scores) <= 5),
  all(is.na(data_scored$sex) | data_scored$sex %in% 0:1))
# Zero-order values ABOVE the diagonal of PDF p.6 Table 1, not the partials below.
published <- data.frame(i=c(1,1,1,1,2,2,2,3,3,4), j=c(2,3,4,5,3,4,5,4,5,5),
  r=c(.41,.47,.51,.32,.48,.49,.43,.52,.42,.29))
reference_cor <- cor(full_scores[names(domain_weights)])
table_difference <- max(abs(reference_cor[as.matrix(published[c("i", "j")])] - published$r))
stopifnot(table_difference <= .005)

# 7. Correlations -----------------------------------------------------------
data_matrix <- data_scored[matrix_variables]
cor_matrix <- cor(data_matrix, method = "pearson", use = "pairwise.complete.obs")
pair_n <- crossprod(!is.na(as.matrix(data_matrix)))
checks <- extraction_check_matrix(data_matrix, cor_matrix, pair_n)
checks$publication_n <- nrow(raw_items)
checks$excluded_historical_overlap <- sum(!keep)
checks$overlap_all_items_equal <- sum(item_differences == 0L)
checks$overlap_one_item_different <- sum(item_differences == 1L)
checks$old_d_not_in_new_release <- sum(!found)
checks$maximum_facet_score_difference <- facet_difference
checks$maximum_domain_score_difference <- domain_difference
checks$maximum_published_table_rounding_difference <- table_difference
checks$gender_other_n <- sum(data_analytic$Gender == "Other")
checks$mean_age <- mean(data_scored$age)
checks$sd_age <- sd(data_scored$age)
checks$female_percentage <- 100 * sum(data_analytic$Gender == "Female") / expected_n
checks$minimum_age <- min(data_scored$age)
checks$maximum_age <- max(data_scored$age)

# 8. Export ----------------------------------------------------------------
# Only scored variables and documented demographics leave the raw-data layer.
# Individual records remain Git-ignored; source Case IDs are not exported.
individual_data <- data_scored[c(matrix_variables, "age", "gender_original")]
decisions <- codebook_csv("config/extraction/0150_decisions.csv")
if (!"--check-only" %in% arguments) extraction_export(paper_id, matrix_id, individual_data,
  cor_matrix, pair_n, variable_map, checks, decisions, audit_dir = audit_dir)
cat(matrix_id, ": ", expected_n, " participants; ", ncol(data_matrix),
  " matrix variables; pair N ", checks$minimum_pair_n, "-", checks$maximum_pair_n,
  "; 596 historical overlaps excluded.\n", sep="")
