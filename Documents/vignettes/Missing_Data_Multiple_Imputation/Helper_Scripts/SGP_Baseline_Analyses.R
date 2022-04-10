my.sgp.config <- list(
    ELA.2021.BASELINE = list(
    		sgp.content.areas = content.areas,
    		sgp.panel.years = cohort.years,
    		sgp.grade.sequences = list(cohort.grades)))

se <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}


if (!run.vignette) {
	load("Data/ELA_7_Baseline_Matrices.rda")
} else load("Missing_Data_Multiple_Imputation/Data/ELA_7_Baseline_Matrices.rda")

SGPstateData[["DEMO_COVID"]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]] <- ELA_7_Baseline_Matrices
SGPstateData[["DEMO_COVID"]][["Growth"]][["Levels"]] <- SGPstateData[["DEMO_COVID"]][["Growth"]][["Cutscores"]] <-
SGPstateData[["DEMO_COVID"]][["SGP_Configuration"]][["percentile.cuts"]] <- NULL

sgp.vars.to.get <- c("ID", "SCHOOL_NUMBER", "FRL", "SCALE_SCORE", "Z_SCORE", "SGP_BASELINE")

Complete_SGP <- abcSGP(sgp_object = long_complete,
                      state = "DEMO_COVID",
                    	steps = c("prepareSGP", "analyzeSGP", "combineSGP"),
                      sgp.config = my.sgp.config,
                      sgp.percentiles = FALSE,
                    	sgp.projections = FALSE,
                    	sgp.projections.lagged = FALSE,
                    	sgp.percentiles.baseline = TRUE,
                    	sgp.projections.baseline = FALSE,
                    	sgp.projections.lagged.baseline=FALSE,
                      simulate.sgps=FALSE,
											goodness.of.fit.print = ifelse(run.vignette, FALSE, TRUE))

# Complete_SGP@Data
if (!run.vignette) {
	file.rename("Goodness_of_Fit/ELA.2021.BASELINE/2021_ELA_7;2019_ELA_5;2018_ELA_4.pdf", "Goodness_of_Fit/ELA.2021.BASELINE/2021_ELA_7_Complete.pdf")
}

smry_comp <- Complete_SGP@Data[YEAR == "2021", ..sgp.vars.to.get][, .(
      Mean_SS_Complete = mean(Z_SCORE, na.rm = TRUE),
      SE_SS_Complete = se(Z_SCORE, na.rm = TRUE),
      Mean_SGPB_Complete = mean(SGP_BASELINE, na.rm = TRUE),
      SE_SGPB_Complete = se(SGP_BASELINE, na.rm = TRUE)),
  keyby = "SCHOOL_NUMBER"]

Missing_SGP <- abcSGP(sgp_object = long_amputed,
                      state = "DEMO_COVID",
                    	steps = c("prepareSGP", "analyzeSGP", "combineSGP"),
                      sgp.config = my.sgp.config,
                      sgp.percentiles = FALSE,
                    	sgp.projections = FALSE,
                    	sgp.projections.lagged = FALSE,
                    	sgp.percentiles.baseline = TRUE,
                    	sgp.projections.baseline = FALSE,
                    	sgp.projections.lagged.baseline=FALSE,
                      simulate.sgps=FALSE,
											goodness.of.fit.print = ifelse(run.vignette, FALSE, TRUE))

# Missing_SGP@Data
if (!run.vignette) {
	file.rename("Goodness_of_Fit/ELA.2021.BASELINE/2021_ELA_7;2019_ELA_5;2018_ELA_4.pdf", "Goodness_of_Fit/ELA.2021.BASELINE/2021_ELA_7_Missing.pdf")
}

smry_obs <- Missing_SGP@Data[YEAR == "2021", ..sgp.vars.to.get][, .(
      Mean_SS_Observed = mean(Z_SCORE, na.rm = TRUE),
      SE_SS_Observed = se(Z_SCORE, na.rm = TRUE),
      Mean_SGPB_Observed = mean(SGP_BASELINE, na.rm = TRUE),
      SE_SGPB_Observed = se(SGP_BASELINE, na.rm = TRUE),
      Percent_Missing = (sum(is.na(Z_SCORE))/.N)*100, N=.N),
  keyby = "SCHOOL_NUMBER"]


##    Run SGP analyses on each imputation (using `abcSGP`)
Imputed_SGP_Data <- data.table()

for (IMP in seq(M)) {
   TEMP_SGP <- abcSGP(sgp_object = res_long[[IMP]],
                      state = "DEMO_COVID",
                    	steps = c("prepareSGP", "analyzeSGP", "combineSGP"),
                      sgp.config = my.sgp.config,
                      sgp.percentiles = FALSE,
                    	sgp.projections = FALSE,
                    	sgp.projections.lagged = FALSE,
                    	sgp.percentiles.baseline = TRUE,
                    	sgp.projections.baseline = FALSE,
                    	sgp.projections.lagged.baseline = FALSE,
                      simulate.sgps = FALSE,
                      goodness.of.fit.print = ifelse(IMP == M && !run.vignette, TRUE, FALSE))

  Imputed_SGP_Data <- rbindlist(list(Imputed_SGP_Data, TEMP_SGP@Data[YEAR == "2021", ..sgp.vars.to.get][, IMP_N := IMP]))
}  #  END IMP

if (!run.vignette) {
	file.rename("Goodness_of_Fit/ELA.2021.BASELINE/2021_ELA_7;2019_ELA_5;2018_ELA_4.pdf", "Goodness_of_Fit/ELA.2021.BASELINE/2021_ELA_7_Imputation_20.pdf")

	gof.dir.base <- "Goodness_of_Fit/ELA.2021.BASELINE"
	gof.dir.new <- file.path("Multiple_Imputation_Analyses", gof.dir.base, gsub("[.]", "_", toupper(impute.method)))
	if (!dir.exists(gof.dir.new)) dir.create(gof.dir.new, recursive=TRUE)
	file.copy(list.files(gof.dir.base, pattern = ".pdf", full.names = T), gof.dir.new)
	unlink("Goodness_of_Fit", recursive = TRUE)
} else 	unlink("Logs", recursive = TRUE)

smry_imp <- Imputed_SGP_Data[, .(
    Mean_SS_Imputed = mean(Z_SCORE, na.rm = TRUE), # Q_l_SS
    SS_U_Imp = var(Z_SCORE, na.rm = TRUE), # Ubar_l_SS
    Mean_SGPB_Imputed = mean(SGP_BASELINE, na.rm = TRUE), # Q_l_SGP_Baseline
    SGPB_U_Imp = var(SGP_BASELINE, na.rm = TRUE)), # Ubar_l_SGP_Baseline
  keyby = c("SCHOOL_NUMBER", "IMP_N")]

smry_imp_pool <- smry_imp[, .(
    Mean_SS_Imputed = mean(Mean_SS_Imputed, na.rm = TRUE),
    SS_U_bar = mean(SS_U_Imp, na.rm = TRUE),
    SS_B = var(Mean_SS_Imputed),

    Mean_SGPB_Imputed = mean(Mean_SGPB_Imputed, na.rm = TRUE),
    SGPB_U_bar = mean(SGPB_U_Imp, na.rm = TRUE),
    SGPB_B = var(Mean_SGPB_Imputed)),
  keyby = "SCHOOL_NUMBER"]

smry_all <- smry_comp[smry_obs][smry_imp_pool]

##    T - Total Variance
smry_all[, SS_T := SS_U_bar + (1 + 1/M)*SS_B]
smry_all[, SGPB_T := SGPB_U_bar + (1 + 1/M)*SGPB_B]

###   Variance Ratios :: 2.3.5

##    Lambda -- proportion of variation attributed to the missing data. eq 2.24
smry_all[, SS_Lamda := round(((SS_B + SS_B/M)/SS_T), 5)]
smry_all[, SGPB_Lamda := round(((SGPB_B + SGPB_B/M)/SGPB_T), 5)]

##    RIV (r) -- relative increase in variance due to nonresponse. eq 2.25
smry_all[, SS_RIV := round((SS_Lamda/(1-SS_Lamda)), 5)]
smry_all[, SGPB_RIV := round((SGPB_Lamda/(1-SGPB_Lamda)), 5)]

##    Adjusted Degrees of Freedom -- eq 2.31
smry_all[, SS_ADF := (((N-1)+1)/((N-1)+3))*((N-1)*(1-SS_Lamda))]
smry_all[, SGPB_ADF := (((N-1)+1)/((N-1)+3))*((N-1)*(1-SGPB_Lamda))]

##    Gamma -- fraction of information about Q missing due to nonresponse
smry_all[, SS_Gamma := round((SS_RIV+(2/(SS_ADF+3)))/(1+SS_RIV), 5)]
smry_all[, SGPB_Gamma := round((SGPB_RIV+(2/(SGPB_ADF+3)))/(1+SGPB_RIV), 5)]

###   Statistical Inference -- Section 2.4.2
##    Confidence intervals
smry_all[!is.na(SS_ADF) & SS_ADF != 0, SS_tStat := qt(0.975, df=SS_ADF)*(sqrt(SS_T))]
smry_all[!is.na(SGPB_ADF) & SGPB_ADF != 0, SGPB_tStat := qt(0.975, df=SGPB_ADF)*(sqrt(SGPB_T))]

smry_all[!is.na(SS_tStat), SS_CI_low := Mean_SS_Imputed-SS_tStat]
smry_all[!is.na(SS_tStat), SS_CI_high := Mean_SS_Imputed+SS_tStat]

smry_all[!is.na(SGPB_tStat), SGPB_CI_low := Mean_SGPB_Imputed - SGPB_tStat]
smry_all[!is.na(SGPB_tStat), SGPB_CI_high := Mean_SGPB_Imputed + SGPB_tStat]

###   Simplified Confidence Invervals - when you have entire population, see Vink and Van Buuren, 2014
nu <- M-1
qtSimp <- qt(0.975, df=nu)

##    (1 + 1/M)*SS_B is the "simplified" T value (U_bar = 0)
smry_all[!is.na(SS_B) & SS_B != 0, SS_tSimp := qtSimp*(sqrt((1 + 1/M)*SS_B))]
smry_all[!is.na(SGPB_B) & SGPB_B != 0, SGPB_tSimp := qtSimp*(sqrt((1 + 1/M)*SGPB_B))]

smry_all[!is.na(SS_tSimp), SS_CI_low_simp := Mean_SS_Imputed-SS_tSimp]
smry_all[!is.na(SS_tSimp), SS_CI_high_simp := Mean_SS_Imputed+SS_tSimp]

smry_all[!is.na(SGPB_tSimp), SGPB_CI_low_simp := Mean_SGPB_Imputed - SGPB_tSimp]
smry_all[!is.na(SGPB_tSimp), SGPB_CI_high_simp := Mean_SGPB_Imputed + SGPB_tSimp]

##    F-test of NULL hypotheses (Observed vs Imputed)
smry_all[, SS_F_Stat := ((Mean_SS_Observed - Mean_SS_Imputed)^2)/SS_T]
smry_all[!is.na(SS_ADF) & SS_ADF != 0, SS_F_p := round(pf(SS_F_Stat, df1=1, df2=SS_ADF, lower.tail=FALSE), 5)]
smry_all[, SS_F_Simp := ((Mean_SS_Observed - Mean_SS_Imputed)^2)/((1 + 1/M)*SS_B)]
smry_all[, SS_F_p_simp := round(pf(SS_F_Simp, df1=1, df2=nu, lower.tail=FALSE), 5)]

smry_all[, SGPB_F_Stat := ((Mean_SGPB_Observed - Mean_SGPB_Imputed)^2)/SGPB_T]
smry_all[!is.na(SGPB_ADF) & SGPB_ADF != 0, SGPB_F_p := round(pf(SGPB_F_Stat, df1=1, df2=SGPB_ADF, lower.tail=FALSE), 5)]
smry_all[, SGPB_F_Simp := ((Mean_SGPB_Observed - Mean_SGPB_Imputed)^2)/((1 + 1/M)*SGPB_B)]
smry_all[, SGPB_F_p_simp := round(pf(SGPB_F_Simp, df1=1, df2=nu, lower.tail=FALSE), 5)]

##    Overall Summary/Comparison of Imputed vs Complete/Observed
overall_smry <- smry_all[, .(
    Mean_SS_Diff_IC = round(mean(Mean_SS_Complete - Mean_SS_Imputed, na.rm=TRUE), 3),
    Cor_SS_IC = round(cor(Mean_SS_Complete, Mean_SS_Imputed, use="na.or.complete"), 3),
    Mean_SGPB_Diff_IC = round(mean(Mean_SGPB_Complete - Mean_SGPB_Imputed, na.rm=TRUE), 3),
    Cor_SGPB_IC = round(cor(Mean_SGPB_Complete, Mean_SGPB_Imputed, use="na.or.complete"), 3),

    Mean_SS_Diff_IO = round(mean(Mean_SS_Observed - Mean_SS_Imputed, na.rm=TRUE), 3),
    Cor_SS_IO = round(cor(Mean_SS_Observed, Mean_SS_Imputed, use="na.or.complete", method = "spearman"), 3),
    Mean_SGPB_Diff_IO = round(mean(Mean_SGPB_Observed - Mean_SGPB_Imputed, na.rm=TRUE), 3),
    Cor_SGPB_IO = round(cor(Mean_SGPB_Observed, Mean_SGPB_Imputed, use="na.or.complete", method = "spearman"), 3),

    Mean_SS_Diff_CO = round(mean(Mean_SS_Complete - Mean_SS_Observed, na.rm=TRUE), 3),
    Cor_SS_CO = round(cor(Mean_SS_Complete, Mean_SS_Observed, use="na.or.complete", method = "spearman"), 3),
    Mean_SGPB_Diff_CO = round(mean(Mean_SGPB_Complete - Mean_SGPB_Observed, na.rm=TRUE), 3),
    Cor_SGPB_CO = round(cor(Mean_SGPB_Complete, Mean_SGPB_Observed, use="na.or.complete", method = "spearman"), 3))]

cols.to.keep <- c(institutions, "Percent_Missing", "N", grep("Mean_|SE_|Lambda|Gamma|RIV|_CI_|_F_p", names(smry_all), value=TRUE))
SGP_Imputation_Summaries <- list(Summary = smry_all[, ..cols.to.keep], Comparison = overall_smry)

# overall_smry[,grepl("SS_Diff", names(overall_smry)), with=F]
# overall_smry[,grepl("SGPB_Diff", names(overall_smry)), with=F]
# overall_smry[,grepl("Cor_SS", names(overall_smry)), with=F]
# overall_smry[,grepl("Cor_SGPB", names(overall_smry)), with=F]
