###   Load required packages
require(SGPdata)
require(data.table)

require(VIM)
require(mice)
require(miceadds)
require(ggplot2)
require(gghighlight)

require(lme4)
require(merTools)

require(SGP)


###   Work Environment Setup and Bookkeeping
set.seed(4224) # for reproducible missing data sim (hopefully)
run.vignette <- FALSE

missing.data.dir <- file.path("Multiple_Imputation_Analyses", "Missing_Data_Plots")
diagnostics.dir  <- file.path("Multiple_Imputation_Analyses", "Imputation_Plots", "Diagnostics")
if (!dir.exists(missing.data.dir)) dir.create(missing.data.dir, recursive = TRUE)
if (!dir.exists(diagnostics.dir)) dir.create(diagnostics.dir, recursive = TRUE)

##    Define the cohort of interest
cohort.grades <- c("4", "5", "7") # All `classes` must match - GRADE and YEAR are characters.
content.areas <- rep("ELA", 3)
cohort.years <- c("2018", "2019", "2021")
current.year <- tail(cohort.years, 1)

###   Data retrieval
Demonstration_COVID_Data <- SGPdata::sgpData_LONG_COVID
source("Helper_Scripts/Data_Retrieval_Initial.R")
##    Returns several data objects, but `wide_data` is the main one we'll use.

dim(wide_data)
names(wide_data)
summary(wide_data)

###   Assume complete data for this exercise. Remove any rows with missing scores.
wide_data <- wide_data[!(is.na(Z_SCORE.2018) | is.na(Z_SCORE.2019) | is.na(Z_SCORE.2021)),]
dim(wide_data)
summary(wide_data)

1 - 6341/9377 #  +32% overall missing proportion

#####
###   Missing Data Creation ("amputation")
#####

ampute.factors <- c("SCHOOL_NUMBER", "Z_SCORE")

###   Create Missing Data (Amputation)
amp.vars <- grep(paste(ampute.factors, collapse="|"), names(wide_data))

ampute_subset <- wide_data[, ..amp.vars]

##    Summarize Student's prior (2019) scores for schools in 2021
ampute_subset[, MEAN_PRIOR_Z := mean(Z_SCORE.2019), by = "SCHOOL_NUMBER"]
ampute_subset[, grep("SCHOOL_NUMBER", names(ampute_subset)) := NULL]

##    Run `mice::ampute` out-of-the-box and explore results
test.result <- mice::ampute(ampute_subset, prop = 0.325)
md.pattern(test.result$amp, rotate.names = TRUE)

test.result$prop
test.result$patterns

my.patterns <- data.frame(t(matrix(c(
               # Some we want from the default
                  c(0, 1, 1, 1),  #  only missing 2018
                  c(1, 0, 1, 1),  #  only missing 2019
                  c(1, 1, 0, 1),  #  only missing 2021
               # Some we want to add to default
                  c(0, 0, 1, 1),  #  missing 2018 & 2019 (only 2021 score)
                  c(0, 1, 0, 1),  #  missing 2018 & 2021
                  c(1, 0, 0, 1)   #  missing 2019 & 2021
                ), nrow = 4)))

test.result <- ampute(ampute_subset, prop = 0.325, patterns = my.patterns)
md.pattern(test.result$amp, rotate.names = TRUE)
test.result$prop
test.result$freq

my.freq <- c(0.1,   #  only missing 2018 - fairly common previously
             0.05,  #  only missing 2019 - less common
             0.50,  #  only missing 2021 - most likely
             0.1,   #  only 2021 scores  - students new to the system
             0.175, #  only 2019 scores  - fairly common
             0.075  #  only 2018 scores  - less common, but still occurs
            )   #   frequencies must sum to 1
test.result <- ampute(ampute_subset, prop = 0.3, patterns = my.patterns, freq = my.freq)
md.pattern(test.result$amp)


my.weights <- test.result$weights

my.weights[1,] <- c(0, 0, 0, 0)     #  MCAR - missing completly at random
my.weights[2,] <- c(0, 0, 0, 0)     #  MCAR - missing completly at random
my.weights[3,] <- c(1, 2, 0.25, 1)  #  MAR/MNAR - missing at random and weakly related to the observed score itself
my.weights[4,] <- c(0, 0, 1, -0.5)  #  MAR - students show up in system at best schools
my.weights[5,] <- c(0, 1, 0.25, 1)  #  MAR/MNAR
my.weights[6,] <- c(1, 0, 0.25, 1)  #  MAR/MNAR

##    reverse weight - lower scoring kids more likely to be missing
my.weights <- my.weights * -1L

test.result <- ampute(ampute_subset, prop = 0.325,
                      patterns = my.patterns,
                      freq = my.freq,
                      weights = my.weights)
md.pattern(test.result$amp, rotate.names = TRUE)

bwplot(test.result, which.pat = 3, descriptives = TRUE)
bwplot(test.result, which.pat = 5, descriptives = TRUE)

##    Save diagnostic plots
pdf(file=file.path(missing.data.dir, "Amputation_Summaries.pdf"), width = 7, height = 7, bg="transparent")
par(mfrow = c(1, 2))
md.pattern(test.result$amp, rotate.names = TRUE)
bwplot(test.result, descriptives = FALSE)
dev.off()

###   Merge amputed scores and reserve/rename the complete scores
amp_scores <- data.table(test.result$amp[, 1:3])
setnames(wide_data, names(amp_scores), gsub("Z_SCORE", "COMPLETE_Z", names(amp_scores)))
wide_data <- cbind(wide_data, amp_scores)

nrow(na.omit(wide_data))/nrow(wide_data); 6341/9377 #  +32% overall missing proportion

###   Missing data vizualizations with VIM
pdf(file=file.path(missing.data.dir, "Histograms-ELA_G7.pdf"), width = 7, height = 7, bg="transparent")
par(mfrow = c(2, 1))
# 2019
histMiss(as.data.frame(wide_data[, c("Z_SCORE.2018", "Z_SCORE.2019")]),
         main = "Missing 2019 to 2018", breaks=25, interactive=FALSE, only.miss=FALSE)
abline(v=0, col="green", lwd=2)

# 2021
histMiss(as.data.frame(wide_data[, c("Z_SCORE.2019", "Z_SCORE.2021")]),
         main = "Missing 2021 to 2019", breaks=25, interactive=FALSE, only.miss=FALSE)
abline(v=0, col="green", lwd=2)
dev.off()

pdf(file=file.path(missing.data.dir, "Mosaic_Ethn_SES.pdf"), width = 7, height = 7, bg="transparent")
mosaicMiss(as.data.frame(droplevels(wide_data[, c("ETHN", "FRL", "Z_SCORE.2021")])),
          highlight = 3, plotvars = 1:2, miss.labels = FALSE, only.miss=FALSE)
dev.off()

##
pdf(file=file.path(missing.data.dir, "Histogram-complete_v_missing.pdf"), width = 7, height = 7)
par(mfrow = c(1, 1))
# 2019
histMiss(as.data.frame(wide_data[, c("COMPLETE_Z.2021", "Z_SCORE.2021")]),
				 main = "2021 Missing by 2021 Complete", breaks=25, interactive=FALSE, only.miss=FALSE)
abline(v=0, col="green", lwd=2)

dev.off()

# Other factors too - demographics, school size (large urban schools more likely to have missing than small), etc.


#####
###   Multiple Imputation of Simulated Missing Data
#####

# impute.factors <- c("SCHOOL_NUMBER", "Z_SCORE", "FRL") # paste(c("ID", "COMPLETE_Z", impute.factors), collapse="|")

###   Select subset of `wide_data` that will be used for imputation and/or subsequent analyses
impute.vars <- grep("^ID$|COMPLETE_Z|Z_SCORE|SCHOOL_NUMBER|FRL", names(wide_data))
impute_subset <- wide_data[, ..impute.vars]
setnames(impute_subset, gsub(paste0(".", current.year), "", names(impute_subset)))

###   Create institutional level averages of achievement and economic disadvantage
impute_subset[, MEAN_INST_PRIOR_SCORE := mean(Z_SCORE.2019, na.rm = TRUE), by = "SCHOOL_NUMBER"] #
impute_subset[, PERCENT_INST_FRL := mean(FRL, na.rm = TRUE), by = "SCHOOL_NUMBER"]


###   Specify imputation parameters
M <- 20
MAXIT <- 5
impute.method <- "2l.pmm" # "2l.pan" # ?mice.impute.pmm # "pmm" #

my.methods <- mice::make.method(data=impute_subset)
my.methods[grep("^Z_SCORE", names(my.methods))] <- impute.method

my.predMtrx <- mice::make.predictorMatrix(data=impute_subset)
my.predMtrx[, "ID"] <- 0
my.predMtrx[, grep("COMPLETE_Z", rownames(my.predMtrx))] <- 0 # i.e. "Nothing used to predict it" -- redundant my.methods["COMPLETE_Z"] == ""
my.predMtrx[grep("COMPLETE_Z", rownames(my.predMtrx)), ] <- 0 # i.e. "It's used to predict nothing"

if (grepl("2l.", impute.method)) {
  inst.pred <- c(-2, 2)
  impute_subset[, SCHOOL_NUMBER := as.integer(SCHOOL_NUMBER)]
} else {
  inst.pred <- c(0, 1)
  impute_subset[, SCHOOL_NUMBER := as.character(SCHOOL_NUMBER)]
}
my.predMtrx[, "SCHOOL_NUMBER"] <- 0 # inst.pred[1]
my.predMtrx[grep("^Z_SCORE", names(my.methods)), "SCHOOL_NUMBER"] <- inst.pred[1]
my.predMtrx["SCHOOL_NUMBER", grep("^Z_SCORE", names(my.methods))] <- inst.pred[2]

# my.predMtrx["FRL",] <- 0
# my.predMtrx["PERCENT_INST_FRL",] <- 0
# my.predMtrx[, "FRL"] <- 0
# my.predMtrx[, "PERCENT_INST_FRL"] <- 0

###   Impute missing data with `mice`
imputed_results <- mice::mice(data = impute_subset,
                              method = my.methods,
                              predictorMatrix = my.predMtrx, # donors = 10L,
                              m = M, maxit = MAXIT, seed = 719589L, print = FALSE)

##    Save diagnostic plots (convergence and density)
pdf(file.path(diagnostics.dir, "Grade_7_ELA__converge.pdf"))
print(plot(imputed_results))
dev.off()

pdf(file.path(diagnostics.dir, "Grade_7_ELA__density_L2PMM.pdf"))
print(densityplot(imputed_results))
dev.off()

##    Scatter Plots for complete - missing - imputed comparisons
long_imputed <- as.data.table(complete(imputed_results, action="long", include=TRUE))
single_imputation <- long_imputed[.imp == M-1,] # .imp %in% c(13:17)

plot.name <- paste0("Data_Comparisons_ELA_7.pdf")

source("Helper_Scripts/MI_Data_Comp_Plots.R")

pdf(file = file.path(diagnostics.dir, plot.name), width=7, height=10)
gridExtra::grid.arrange(truth.p, truth.c, impute.p, impute.c, ampute.p, ampute.c, ncol=2)
dev.off()


#####
###   Analysis of Imputed Data
#####

###   Collect all imputation results in a list length = M
res <- miceadds::mids2datlist(imputed_results)

###   Convert data (complete, missing, and imputed) to long format
source("Helper_Scripts/Data_Retrieval_for_Analysis.R")
##    Returns objects `long_complete`, `long_amputed` and `res_long`

#######       Linear Mixed Effects       #######

###   Run LME models on various data sets
frmla <- "Z_SCORE ~ YEAR + COVID_IMPACT*FRL + (COVID_IMPACT | SCHOOL_NUMBER/ID)"
lmer.ctl <- lmerControl(check.conv.grad = .makeCC("warning", 0.02, NULL))

true_mod <- lmer(formula = frmla, data = long_complete, control = lmer.ctl)
miss_mod <- lmer(formula = frmla, data = long_amputed, control = lmer.ctl)
imp_mods <- lmerModList(formula=frmla, data = res_long, control = lmer.ctl)

###   Collate and print model (fixed effects) estimate comparison
source("Helper_Scripts/MI_LME_Model_Comp.R")
##    Returns an object `comp_table`
   #  comp_table_pmm <- copy(comp_table)
   #  print(comp_table_pmm, class = F)


#####     Baseline Student Growth Percentiles     #####

###   Run Baseline SGP analyses using complete, missing (observed), and imputed data

source("Helper_Scripts/SGP_Baseline_Analyses.R")
##    Returns several objects of interest including `Complete_SGP` and `Missing_SGP`,
##    which are typical SGP class objects from the associated data.  The 20 analyses
##    using imputation data are combined into a single data table - `Imputed_SGP_Data`.
##    Estimates for school level values (means, pooled errors, etc.) are in `SGP_Imputation_Summaries`.

source("Helper_Scripts/SGP_MI_Caterpillar_Plots.R")
##    Creates caterpillar plots for school-level mean achievement and growth.
##    These include bands of uncertainty associated with the imputations.
