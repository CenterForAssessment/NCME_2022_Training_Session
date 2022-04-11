
meas.list.cm <- list(COMPLETE_Z = grep("COMPLETE_Z", names(impute_subset)),
                        Z_SCORE = grep("^Z_SCORE", names(impute_subset)))

long_complete <- melt(impute_subset, id = c("ID", "SCHOOL_NUMBER", "FRL"), variable.name = "GRADE", measure = meas.list.cm)
long_complete[, VALID_CASE := "VALID_CASE"]
long_complete[, CONTENT_AREA := tail(content.areas, 1)]
long_complete[, COVID_IMPACT := as.numeric(factor(GRADE, labels = c(0, 0, 1)))-1]
long_complete[, YEAR := as.numeric(as.character(factor(GRADE, labels = cohort.years)))]
long_complete[, GRADE := as.character(factor(GRADE, labels = cohort.grades))]

long_complete[, COMPLETE_SS := as.numeric(NA)]
for (G in cohort.grades) {
  y <- long_data[YEAR == tail(cohort.years, 2)[1], SCALE_SCORE]
  z <- long_complete[GRADE == G, COMPLETE_Z]
  long_complete[GRADE == G, COMPLETE_SS := z*sd(y, na.rm=TRUE) + mean(y, na.rm=TRUE)]

  tmp.loss.hoss <- SGP::SGPstateData[["DEMO_COVID"]][["Achievement"]][["Knots_Boundaries"]][[tail(content.areas, 1)]][[paste0("loss.hoss_", G)]]
  long_complete[GRADE == G & COMPLETE_SS < tmp.loss.hoss[1], COMPLETE_SS := tmp.loss.hoss[1]]
  long_complete[GRADE == G & COMPLETE_SS > tmp.loss.hoss[2], COMPLETE_SS := tmp.loss.hoss[2]]
  long_complete[, COMPLETE_SS := round(COMPLETE_SS, 0)]

  long_complete[, SCALE_SCORE := COMPLETE_SS]
  long_complete[is.na(Z_SCORE), SCALE_SCORE := as.numeric(NA)]
}
setcolorder(long_complete, c("VALID_CASE", "ID", "YEAR", "CONTENT_AREA", "GRADE", "SCHOOL_NUMBER", "FRL", "COVID_IMPACT", "SCALE_SCORE", "COMPLETE_SS"))
    # won't be the same since wide_data subsetted to remove NAs
    # long_data[, as.list(summary(SCALE_SCORE)), by = GRADE]
    # long_complete[, as.list(summary(COMPLETE_SS)), by = GRADE]

# long_complete <- getAnalysisData(impute_subset)
long_amputed  <- copy(long_complete)[, c("COMPLETE_SS", "COMPLETE_Z") := NULL]

###  The score variable of interest needs to be "SCALE_SCORE" in order to be used with the SGP package high-level functions
long_complete[, c("SCALE_SCORE", "Z_SCORE") := NULL]
setnames(long_complete, c("COMPLETE_SS", "COMPLETE_Z"), c("SCALE_SCORE", "Z_SCORE"))

res_long <- vector(mode = "list", length = M)
for (i in 1:M) {
  meas.list.i <- list(Z_SCORE = grep("^Z_SCORE", names(impute_subset)))
  res_long[[i]] <- melt(as.data.table(res[[i]]), id = c("ID", "SCHOOL_NUMBER", "FRL"), variable.name = "GRADE", measure = meas.list.i)
  if ("value" %in% names(res_long[[i]])) setnames(res_long[[i]], "value", "Z_SCORE")
  res_long[[i]][, VALID_CASE := "VALID_CASE"]
  res_long[[i]][, CONTENT_AREA := tail(content.areas, 1)]
  res_long[[i]][, COVID_IMPACT := as.numeric(factor(GRADE, labels = c(0, 0, 1)))-1]
  res_long[[i]][, YEAR := as.numeric(as.character(factor(GRADE, labels = cohort.years)))]
  res_long[[i]][, GRADE := as.character(factor(GRADE, labels = cohort.grades))]

  res_long[[i]][, SCALE_SCORE := as.numeric(NA)]
  for (G in cohort.grades) {
    y <- long_data[YEAR == tail(cohort.years, 2)[1], SCALE_SCORE]
    z <- res_long[[i]][GRADE == G, Z_SCORE]
    res_long[[i]][GRADE == G, SCALE_SCORE := z*sd(y, na.rm=TRUE) + mean(y, na.rm=TRUE)]

    tmp.loss.hoss <- SGP::SGPstateData[["DEMO_COVID"]][["Achievement"]][["Knots_Boundaries"]][[tail(content.areas, 1)]][[paste0("loss.hoss_", G)]]
    res_long[[i]][GRADE == G & SCALE_SCORE < tmp.loss.hoss[1], SCALE_SCORE := tmp.loss.hoss[1]]
    res_long[[i]][GRADE == G & SCALE_SCORE > tmp.loss.hoss[2], SCALE_SCORE := tmp.loss.hoss[2]]
    res_long[[i]][, SCALE_SCORE := round(SCALE_SCORE, 0)]
  }
  setcolorder(res_long[[i]], c("VALID_CASE", "ID", "YEAR", "CONTENT_AREA", "GRADE", "SCHOOL_NUMBER", "FRL", "COVID_IMPACT", "SCALE_SCORE"))
  # res_long[[i]] <- getAnalysisData(as.data.table(res[[i]]))[, c("COMPLETE_SS", "COMPLETE_Z") := NULL]
}
