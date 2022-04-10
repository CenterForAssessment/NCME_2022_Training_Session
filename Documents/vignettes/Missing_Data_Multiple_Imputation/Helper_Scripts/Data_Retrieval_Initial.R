###   Utility functions
`%w/o%` <- function(x, y) x[!x %in% y]

Z <- function(data_table, var.to.standardize, reference.year = NULL, rm.na = TRUE) {
  YEAR <- NULL
  x <- data_table[, get(var.to.standardize)]
  if (!is.null(reference.year)){
    y <- data_table[YEAR==reference.year, get(var.to.standardize)]
  } else y <- x
  (x - mean(y, na.rm = rm.na)) / sd(y, na.rm = rm.na)
}

###   Variables to be selected/created
default.vars <- c("VALID_CASE", "ID", "YEAR", "CONTENT_AREA", "GRADE", "SCALE_SCORE", "Z_SCORE")
demographics <- c("FREE_REDUCED_LUNCH_STATUS", "ETHNICITY", "ELL_STATUS", "IEP_STATUS", "GENDER")
demog.dummies<- c("ETHN", "FRL", "ELL", "SWD", "MALE")
institutions <- "SCHOOL_NUMBER"

###   Data retrieval
# Demonstration_COVID_Data <- SGPdata::sgpData_LONG_COVID # this line is included in the main script
Demonstration_COVID_Data[, Z_SCORE := Z(.SD, "SCALE_SCORE", reference.year = "2019"),
                by = list(CONTENT_AREA, GRADE), .SDcols = c("YEAR", "CONTENT_AREA", "GRADE", "SCALE_SCORE")]
# #   Quick summary of the Z score with 2019 used as reference year
# Demonstration_COVID_Data[YEAR %in% c("2018", "2019", "2021"), as.list(summary(round(Z_SCORE, 3))), keyby = c("YEAR", "CONTENT_AREA", "GRADE")]

###   First subset of the LONG data
variables.to.get <- c(default.vars, institutions, demographics)
Demonstration_COVID_Data <- Demonstration_COVID_Data[YEAR %in% c("2018", "2019", "2021"), ..variables.to.get]

###   Set up demographic factor and dummy variables
Demonstration_COVID_Data[, FRL := ifelse(FREE_REDUCED_LUNCH_STATUS == "Free Reduced Lunch: Yes", 1, 0)]
Demonstration_COVID_Data[, MALE := ifelse(GENDER == "Gender: Male", 1, 0)]
Demonstration_COVID_Data[, ELL := ifelse(ELL_STATUS == "ELL: Yes", 1, 0)]
Demonstration_COVID_Data[, SWD := ifelse(IEP_STATUS == "IEP: Yes", 1, 0)]
Demonstration_COVID_Data[, ETHN := ETHNICITY]
Demonstration_COVID_Data[, ETHN := factor(ETHN, levels = c("Other", "Asian", "African American", "Hispanic", "White"), 
																					      labels = c("O", "A", "B", "H", "W" ))]
Demonstration_COVID_Data <- droplevels(Demonstration_COVID_Data)

##    Extract relevant cohort data subset
cohort.to.get <- SJ("VALID_CASE", content.areas, cohort.years, cohort.grades)
vars.to.keep <- c(default.vars, institutions, demog.dummies)

setkeyv(Demonstration_COVID_Data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE")) # ensure lookup table is ordered by years.
long_data <- Demonstration_COVID_Data[cohort.to.get][, ..vars.to.keep]

###   Widen, subset and re-order the long data
long.to.wide.vars <- vars.to.keep %w/o% c("VALID_CASE", "ID", "YEAR", "CONTENT_AREA", "GRADE", "SCALE_SCORE", "ELL", "SWD", "MALE")
wide_data <- dcast(long_data, ID ~ YEAR, sep = ".", value.var = long.to.wide.vars)
wide_data[, c("ETHN.2018", "ETHN.2019", "FRL.2018", "FRL.2019", "SCHOOL_NUMBER.2018", "SCHOOL_NUMBER.2019") := NULL]
setnames(wide_data, c("SCHOOL_NUMBER.2021", "ETHN.2021", "FRL.2021"), c("SCHOOL_NUMBER", "ETHN", "FRL"))
setcolorder(wide_data, c("ID", "ETHN", "FRL", "SCHOOL_NUMBER"))

# rm(long_data); gc()
