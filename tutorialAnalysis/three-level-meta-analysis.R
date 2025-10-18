# =======================================================================
# Title:     Tutorial meta-analysis of the Living SEB project data
# Author:    Tommaso Feraco
# Date:      2025
# Objective: Reproduce a classic meta-analysis using 'metafor' on 
#            SEB skills -> academic achievement with clear steps for data
#            acquisition, preparation, modeling, and reporting.
# =======================================================================


# ================================ UTILITIES ============================= ####
# Purpose: Load helper functions and core packages used across the workflow.
# - 'R/utils.R' is expected to define helper utilities (e.g., load_xlsx_url,
#   getMETAdata). Ensure it is available and paths are correct.
# - 'metafor' for effect size calculation and multilevel meta-analysis.
# - 'tidyverse' for data wrangling and plotting via ggplot2.
# - 'ggplot2' for plotting.
source(here::here("R/utils.R"))

library(metafor)
library(tidyverse)
library(ggplot2)

# ============================== DATA ACCESS ============================= ####
# Purpose: Obtain the combined dataset and the matrix codebook (admissible
#          column names) from either remote GitHub URLs or local files.
# Notes:
# - The two “Download from git” lines show how to fetch the latest files.
# - The two “Open it locally” lines show how to use the data download locally.
# - Only one source is needed in practice; here both are demonstrated.

# -------------------- Combined meta-analytic dataset -------------------- #
# Here you can open the combined data and analyse them using metafor
## Download it from git
data_url <- "https://raw.githubusercontent.com/feracotommaso/living_SEB_review/main/data/3.meta_data/meta_data/metadata.xlsx"
combined_data <- load_xlsx_url(data_url)
# Open it locally
combined_data <- readxl::read_excel(here::here("data/3.meta_data/meta_data/25_01_31_metadata.xlsx"))

# -------------------- Matrix codebook / admissible columns -------------- #
# Open the matrix codebook with the admissible colnames
## Download it from git
admcol_url <- "https://raw.githubusercontent.com/feracotommaso/living_SEB_review/main/data/matrix_codebook.xlsx"
admcol <- load_xlsx_url(admcol_url)
## Open it locally
admcol <- readxl::read_excel(here::here("data/matrix_codebook.xlsx"))

# -------------------- Variables and human-readable labels --------------- #
# Purpose: Build a lookup between internal column names and reporting labels.
# Expectation: admcol has (at least) the first four columns populated for rows
# that define usable variables: [Topic, BROAD, specific, label, column_name].
varlist <- admcol[complete.cases(admcol[,1:4]),] 

# Create a named vector from the lookup table to rename variables
# names = internal column_name; values = human-friendly labels
labels_app <- setNames(varlist$label, varlist$column_name)

# ========================== METHODS: CLASSIC META ======================= ####
# Purpose: Specify study-level info, select predictors and outcome, reshape
#          data to long meta-analytic format, compute effect sizes/variances,
#          and fit multilevel random-effects models via 'metafor'.

# -------------------- Reporting: Study descriptors ---------------------- #
# Define the study info you want
# These columns are carried into outputs for transparency and audit trails.
study_info <- c("download_date","doi","year","age_class",
                "paper_id", "matrix_id", "author_et_al", "n", "title")

# -------------------- Model variables ---------------------------------- #
# Pred vars are used for selecting your predictor variables (seb skills)
pred_vars <- sort(c("selfmanagement", "cooperation", "socialengagement", 
                    "innovation", "emotionalresilience"))
# Outcome variable for the meta-analysis
out_var <- sort(c("academicachievement"))

# -------------------- Reshape to meta-analytic structure ---------------- #
# Use the combined data + the info, pred vars and outcome var to reshape 
# the data as desired
# Expected output 'dm' will contain columns:
#   pred (predictor), out (outcome), value (r), n, paper_id, matrix_id, etc.
dm <- getMETAdata(pred_vars, out_var, combined_data, study_info)

# -------------------- Effect size calculation --------------------------- #
# Use the metafor::escalc function to transform pearson r into Fisher's z (yi),
# and compute corresponding sampling variances (vi). Required for rma.mv().
dm <- escalc(measure = "ZCOR", ri = value, ni = n, data = dm)

# -------------------- Descriptive table for PRISMA-style reporting ------ #
# Build a study-level table (one row per matrix_id) for transparency.
uniqueD <- dm[!duplicated(dm$matrix_id), ]
metaStudyTab <- 
  data.frame(StudyID = uniqueD$matrix_id, 
             Authors = uniqueD$author_et_al,
             Year = as.character(uniqueD$year),
             Age = uniqueD$age_class,
             N = as.character(uniqueD$n),
             doi = uniqueD$doi
  )
rbind(metaStudyTab,
      c("Total","","","",sum(as.numeric(metaStudyTab$N)),""))

# -------------------- Multilevel random-effects models ------------------ #
# For each predictor:
#   - Fit rma.mv with random intercepts nested as paper_id/matrix_id
#   - Extract point estimate (Fisher's z and back-transformed r), CIs, SE, p
#   - Extract prediction interval (PI) on r scale and heterogeneity (tau^2)
#   - Record k (number of effects) and total N for included matrices.

# Preallocate containers
outList <- list()
metaCol <- c("b","cil","ciu","se","z","p",
             "r","rcil","rciu","rpil","rpiu",
             "tau2",
             "k","N",
             "q","qdf","qp")
metaRes <- matrix(nrow = length(pred_vars), ncol = length(metaCol))
colnames(metaRes) <- metaCol
rownames(metaRes) <- pred_vars

# Loop: fit models and summarize
for (i in 1:length(pred_vars)) {
  di <- dm[dm$pred == pred_vars[i] & is.na(dm$yi) == F,]
  outList[[i]] <- rma.mv(yi = yi, V = vi, random = ~ 1 | paper_id / matrix_id,
                         test = "t", data = di)
  mres <- summary(outList[[i]])
  pred <- predict(mres, transf=transf.ztor)
  tau2 <- sum(mres$sigma2)
  metaRes[i,] <- c(mres$beta,mres$ci.lb,mres$ci.ub,mres$se,mres$zval,mres$pval,
                   pred$pred,pred$ci.lb,pred$ci.ub,pred$pi.lb,pred$pi.ub,
                   tau2,
                   nrow(di),sum(di$n[di$matrix_id %in% unique(di$matrix_id)]),
                   mres$QE,mres$QEdf,mres$QEp
  )
}
names(outList) <- pred_vars

# Collect results
metaRes <- data.frame(metaRes)
metaRes$skill = pred_vars

metaAnalysis <- list(metaRes = metaRes, outList = outList)

# ============================== RESULTS TABLE =========================== ####
# Purpose: Produce a compact, reporting-ready table (per predictor) with:
#   - Sample size info (N, k)
#   - Pooled correlation r with 95% CI
#   - Standard error, p-value
#   - Heterogeneity (Q with df, significance flag; tau^2)

metaResTable <- data.frame(
  Skill = metaRes$skill,
  N = paste0(metaRes$N, " k = (", metaRes$k, ")"),
  r = format(round(metaRes$r, 3),nsmall=3),
  CI = paste0("[",format(round(metaRes$rcil,3),nsmall=3),"; ",
              format(round(metaRes$rciu,3),nsmall=3),"]"),
  se = format(round(metaRes$se,3),nsmall=3),
  p = format(round(metaRes$p,4),3),
  q = paste0(round(metaRes$q), "(",metaRes$qdf,")",ifelse(metaRes$qp < .01, "*", "")),
  tau2 = format(round(metaRes$tau2,3),nsmall=3))

# Replace factor levels
# Map internal variable names to human-friendly labels for reporting.
metaResTable$Skill <- labels_app[as.character(metaResTable$Skill)]
metaResTable


# ============================ VISUALIZATION ============================= ####
# Purpose: Create a summary plot of pooled r and 95% CI for each skill,
#          with in-plot annotations of estimates and CIs.

outcomemeta <- out_var
outcomelabel <- varlist$label[varlist$column_name == outcomemeta]
metaRes <- metaAnalysis$metaRes
metaRes$skill <- labels_app[as.character(metaRes$skill)]
metaRes$CI <- paste0("[",format(round(metaRes$rcil,3),nsmall=3),"; ",
                     format(round(metaRes$rciu,3),nsmall=3),"]")

ggplot(metaRes, aes(x = r, y = reorder(skill,length(pred_vars):1))) +
  geom_point(shape = 18,
             color = "black",
             size = 4) +
  geom_errorbar(aes(xmin = rcil, xmax = rciu), width = 0, linewidth = .8) +
  geom_vline(xintercept = c(.00), linetype = "dashed") +
  labs(x = paste0("Meta-analytical association with ", outcomelabel), y = "Skill") +
  theme_bw(base_size = 12) +
  theme(strip.text = element_text(size = 10)) +
  annotate("text", x = metaRes$r,
           y = length(pred_vars):1+.20,
           label = paste0(round(metaRes$r,3)," ",metaRes$CI), 
           hjust = "center", size = 3) +
  coord_cartesian(xlim = c(ifelse(min(metaRes$rcil) > -.05, -.15,
                                  min(metaRes$rcil)),
                           max(metaRes$rciu)))


# ============================= FOREST PLOTS ============================= ####
# Purpose: Inspect study-level effects for a specific predictor using the
#          corresponding fitted model in 'outList'.
# Notes:
# - 'outList' stores rma.mv models in the same order as 'pred_vars'.
# - Set x to any name in pred_vars (e.g., "cooperation").
# - 'slab' controls row labels (study identifier vs. author/year/matrix).

x = "cooperation"

forest(outList[[x]], 
       slab = outList[[x]]$data$author_et_al,
       ilab = cbind(outList[[x]]$data$year,
                    outList[[x]]$data$matrix_id),
       ilab.lab = c("Year","ID"),
       header = T,
       main = paste0(x))
