# =======================================================================
# Title:     Tutorial meta-analysis of the Living SEB project data
# Author:    Tommaso Feraco
# Date:      2025
# Objective: Reproduce a metaSEM analysis using 'metaSEM' on 
#            SEB skills - traits - academic achievement with clear steps 
#            for data acquisition, preparation, modeling, and reporting.
# =======================================================================


# ================================ UTILITIES ============================= ####
# Purpose: Load helper functions and core packages used across the workflow.
# - 'R/utils.R' defines helper utilities (e.g., load_xlsx_url,
#   getSEMdata). Ensure it is available and paths are correct.
# - 'metaSEM' for metaSEM meta-analysis.
# - 'tidyverse' for data wrangling and plotting via ggplot2.
# - 'corrplot' for correlations plotting
# Tested with metaSEM 2.0, lavaan 0.7-3 and OpenMx 2.22.7.
# See INSTALLATION.md; restart R after package updates.
source(here::here("R/utils.R"))

library(metaSEM)   # Structural equation modeling in a meta-analytic framework
library(tidyverse) # Data handling
library(corrplot)  # Correlation matrix plotting

# ============================== DATA ACCESS ============================= ####
# Purpose: Obtain the combined dataset and the matrix codebook (admissible
#          column names) from either remote GitHub URLs or local files.
# Notes:
# - The two “Download from git” lines show how to fetch the latest files.
# - The two “Open it locally” lines show how to use the data you downloaded locally.
# - Local files are used below. The remote alternative is commented out.
# - To use published data instead, uncomment the remote import and comment
#   the matching local import; do not run both and silently replace one source.

# -------------------- Combined meta-analytic dataset -------------------- #
# Example pipeline: open combined data and analyze using meta-analytic SEM
## Download it from git
# data_url <- "https://raw.githubusercontent.com/feracotommaso/living_SEB_review/main/data/3.meta_data/meta_data/metadata.xlsx"
# combined_data <- load_xlsx_url(data_url)
# Open it locally
combined_data <- readxl::read_excel(here::here("data/3.meta_data/meta_data/metadata.xlsx"))

# -------------------- Matrix codebook / admissible columns -------------- #
# Codebook defines valid variable names and their human-readable labels
## Download it from git
# admcol_url <- "https://raw.githubusercontent.com/feracotommaso/living_SEB_review/main/data/matrix_codebook.xlsx"
# admcol <- load_xlsx_url(admcol_url)
## Open it locally
admcol <- readxl::read_excel(here::here("data/matrix_codebook.xlsx"))

# -------------------- Variables and human-readable labels --------------- #
# Keep rows whose first 4 columns are complete (Topic/BROAD/specific/label)
varlist <- admcol[complete.cases(admcol[,1:4]),] 

# Build a name → label lookup: names = column_name, values = label
labels_app <- setNames(varlist$label, varlist$column_name)

# ========================= STUDY SELECTION & SETUP ====================== ####
# Define the variables to include in the correlation matrices and models.
# Variables must be in the selected data; individual matrices may have missing
# correlations. Do not replace missing correlations or source Ns automatically.
target_vars <- c("academicachievement",
                 "selfmanagement", "socialengagement",
                 "conscientiousness", "extraversion")

# Study-level descriptors to carry through (for reporting/audits)
study_info <- c("matrix_id", "author_et_al", "year", "n", "age_class", "doi")

# Extract per-study correlation matrices and aligned metadata for SEM.
# The helper 'getSEMdata':
#  - retain matrices reporting at least one selected pair
#  - sort and name variables consistently, preserving missing correlations
#  - return a list with $cor_matrices (list of R-by-R matrices)
#    and $basic_info (data.frame aligned to that list)
metaD <- getSEMdata(target_vars,combined_data,study_info)

# ============================== DATA DESCRIPTION ======================== ####
# Inspect missingness patterns across matrices (by correlation entries).
# 'pattern.na' shows which correlations are available across studies (k).
pattern.na(metaD$cor_matrices, show.na = FALSE) # k cors

# Sums of coded reference Ns, not pair-specific counts or unique participants.
pattern.n(metaD$cor_matrices, metaD$basic_info$n) # n per cor

# Prepare a study inventory table (useful for PRISMA/reporting appendices).
StudyTab <- data.frame(
  # Dataset = metaD()$basic_info$download_date,
  StudyID = metaD$basic_info$matrix_id,
  Authors = metaD$basic_info$author_et_al,
  Year = as.character(metaD$basic_info$year),
  Age = as.character(metaD$basic_info$age_class),
  N = metaD$basic_info$n,
  doi = metaD$basic_info$doi)

# ============================== STEP 1 METASEM ======================== ####

# GOAL: Explore a pooled matrix. This is separate from the one-stage fit below;
# the one-stage model uses the original matrices directly, not SumTable.
# 'tssem1' performs a random-effects meta-analysis of each unique correlation,
# weighting by study n, to estimate the pooled (weighted) correlation matrix.

# Fit Stage 1: estimate pooled correlation structure with random effects
cfa1 <- metaSEM::tssem1(metaD$cor_matrices, metaD$basic_info$n)

# Summarize Stage 1 results: shows pooled correlations & heterogeneity info.
summary(cfa1)

# Convert the vectorized pooled parameters back into a symmetric correlation
# matrix; 'diag=FALSE' because diagonals in correlation matrices are 1 by def.
SumTable <- round(metaSEM::vec2symMat(coef(cfa1,"fixed"),diag=FALSE),3)

# Assign row/col names from the first study matrix (assumes consistent order).
dimnames(SumTable) <- list(rownames(metaD$cor_matrices[[1]]),
                           rownames(metaD$cor_matrices[[1]]))
SumTable

# Visualize the pooled correlation matrix with coefficients
corrplot::corrplot(SumTable, method = "ellipse", 
                   addCoef.col = "black",
                   tl.col = 'black', tl.srt = 45)

# ============================== STEP 2 OSMASEM ======================== ####

# GOAL: One-Stage MASEM (OSMASEM): estimate the SEM directly from all study-
# level correlation matrices, accounting for sampling error. In the current
# example, between-study heterogeneity is fixed at zero.

# ------------------------ 1) Specify the SEM in lavaan syntax ------------- #
# IMPORTANT:
# - The model must be identified and acyclic; coverage alone is not sufficient.
# - Exogenous variables (predictors with no regressions onto them) typically
#   have their variances fixed to 1 to set the scale in OSMASEM.
# - Pairwise covariances among exogenous variables are included (~~).
lavmodel <- "
# REGRESSION
academicachievement ~ selfmanagement + socialengagement + conscientiousness + extraversion

# COVARIANCES AMONG PREDICTORS (estimated)
conscientiousness ~~ extraversion
conscientiousness ~~ selfmanagement
conscientiousness ~~ socialengagement
extraversion ~~ selfmanagement
extraversion ~~ socialengagement
selfmanagement ~~ socialengagement

# FIXED EXOGENOUS VARIANCES (needed)
conscientiousness ~~ 1*conscientiousness
extraversion ~~ 1*extraversion
selfmanagement ~~ 1*selfmanagement
socialengagement ~~ 1*socialengagement
" 
# If you want to fix exogenous variables or saturate the model rapidly, 
# instead of writing all the variance and covariance use augment_lavaan_model

# Grab the variable names (ordering) as present in the matrices
varnames <- rownames(metaD$cor_matrices[[1]]) # Get the names of the variables

# Example (commented) augmentation helper, if using automated scaffolding:
# user_model <- lavmodel
# model_aug <- augment_lavaan_model(
#   user_model          = user_model,
#   varnames            = varnames,
#   fix_exo_var         = TRUE,
#   correlate_exogenous = TRUE,
#   saturate_endogenous = FALSE
#   )


# ------------------------ 2) Translate to RAM and prepare OSMASEM -------- #
# 'lavaan2RAM' converts lavaan syntax to RAM (Reticular Action Model) form:
#   A: asymmetric paths (regressions), S: symmetric (variances/covariances),
#   F: filter/selection matrix mapping latent/observed to observed variables.
nvar <- nrow(metaD$cor_matrices[[1]]) # Get their number

RAM1 <- metaSEM::lavaan2RAM(lavmodel, obs.variables=varnames) # Generate RAM syntax from lavaan

# Preserve the current model: RE.type = "Zero" fixes heterogeneity to zero.
# Transform/start values are retained from the original call but do not free
# any heterogeneity parameters in the Zero specification.
T0 <- metaSEM::create.Tau2(RAM=RAM1, RE.type="Zero", 
                           Transform="expLog", RE.startvalues=0.05)

# Convert list of correlation matrices + ns into a single long data frame:
# - 'acov = "weighted"' requests asymptotic covariance matrices (ACOV)
#   needed for correct weighting in OSMASEM.
my.df <- metaSEM::Cor2DataFrame(metaD$cor_matrices, metaD$basic_info$n, acov = "weighted")

# Build the model-implied vectorized covariance structure from RAM for OSMASEM:
# - 'create.vechsR' builds the implied correlation vector from A, S and F,
#   deriving dependent-variable residual variances for correlation analysis.
M0 <- create.vechsR(A0=RAM1$A, S0=RAM1$S, F0 = RAM1$F)

# ------------------------ 3) Fit the One-Stage MASEM model --------------- #
# 'osmasem' fits the original matrices directly with zero heterogeneity in T0.
# 'intervals.type = "z"' requests Wald-z intervals for the model parameters.
oss <- osmasem(model.name="One Stage MASEM", Mmatrix=M0, Tmatrix=T0, data=my.df, intervals.type = "z")

# Summarize parameter estimates and global fit indices
sumfit <- summary(oss, fitIndices = TRUE)
# See the results
sumfit

# ------------------------ 4) Report key fit indices ---------------------- #
# Assemble a compact fit table:
# - For this saturated example (df = 0), global fit indices are uninformative.
# - For non-saturated models, interpret fit together with coverage and precision.
data.frame(CFI = sumfit$CFI,
           TLI = sumfit$TLI,
           RMSEA = sumfit$RMSEA,
           SRMR = round(osmasemSRMR(oss),3))


# ------------------------ 5) Residual & variance components -------------- #
# Extract residual variances of observed variables from fitted S matrix:
# - Shows leftover variance after accounting for predictors (for endo vars)
#   or fixed variances for exogenous variables (fixed to 1 above).
Sres <- oss$mx.fit$Smatrix$result
Sres <- as.matrix(diag(Sres))
dimnames(Sres) <- list(colnames(oss$Mmatrix$S0$labels),'(residual) variance')
round(Sres,4)

# Extract correlation-level heterogeneity in vectorized order (all zero here):
# - These are not heterogeneity estimates for individual regression paths.
# - Taking diag() grabs variances; sqrt() converts to SDs (heterogeneity SD).
vcor <- round(diag(metaSEM::VarCorr(oss)),3)
names(vcor) <- my.df$ylabels
sqrt(vcor) # transform to SD
