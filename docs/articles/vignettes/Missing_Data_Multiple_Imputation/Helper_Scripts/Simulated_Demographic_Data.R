##########################################################
###   Simulated Sample of Students Based on Observed   ###
##########################################################

table(long_data[, YEAR, GRADE])
N <- 9000

###   Create a matrix of grades
# gen_grades <- matrix(cohort.grades, N, length(cohort.grades), byrow=TRUE)

###   Generate demographic variables
propTab <- function(v1, v2=NULL, digitz=3) {
  if (is.null(v2) & is.null(dim(v1))) return(round(prop.table(table(v1)), digitz))
  if (is.null(v2) & is.data.table(v1)) return(round(prop.table(table(v1), 1), digitz))
  return(round(prop.table(table(v1, v2), 1), digitz))
}

##    Create summary crosstabs to replicate observed proportions
ethn.tbl <- propTab(long_data[, ETHN])
ethn.sex.tbl <- propTab(long_data[, MALE, ETHN])
ethn.frl.tbl <- propTab(long_data[, FRL, ETHN])
ethn.ell <- propTab(long_data[, ELL, ETHN])
ethn.ell.frl0 <- propTab(long_data[FRL == 0, ELL, ETHN])
ethn.ell.frl1 <- propTab(long_data[FRL == 1, ELL, ETHN])
ethn.swd <- propTab(long_data[, SWD, ETHN])
ethn.swd.frl0 <- propTab(long_data[FRL == 0, SWD, ETHN])
ethn.swd.frl1 <- propTab(long_data[FRL == 1, SWD, ETHN])

##    Ethnicity dummy vars.
ethn <- sample(names(ethn.tbl), N, replace = TRUE, prob = ethn.tbl)
ethn.tbl; propTab(ethn)

# ETHNAsian <- ifelse(ethn == "Asian", 1, 0)
# ETHNBlack <- ifelse(ethn == "African American", 1, 0)
# ETHNHispanic <- ifelse(ethn == "Hispanic", 1, 0)
# ETHNWhite <- ifelse(ethn == "White", 1, 0)
# Other <- ifelse(ethn == "Other", 1, 0) # name so that first alphabetically
#
# ethn.dummies <- matrix(rbind(ETHNAsian, ETHNBlack, ETHNHispanic, ETHNWhite), N, 4, byrow=TRUE)

##    Male/Female
male <- ifelse(ethn == "Asian", rbinom(N, 1, ethn.sex.tbl["Asian", "1"]), 0)
male <- ifelse(ethn == "African American", rbinom(N, 1, ethn.sex.tbl["African American", "1"]), male)
male <- ifelse(ethn == "Hispanic", rbinom(N, 1, ethn.sex.tbl["Hispanic", "1"]), male)
male <- ifelse(ethn == "White", rbinom(N, 1, ethn.sex.tbl["White", "1"]), male)
male <- ifelse(ethn == "Other", rbinom(N, 1, ethn.sex.tbl["Other", "1"]), male)
ethn.sex.tbl; propTab(ethn, male)

##    Economic Disadvantaged
frl <- ifelse(ethn == "Asian", rbinom(N, 1, ethn.frl.tbl["Asian", "1"]), 0)
frl <- ifelse(ethn == "African American", rbinom(N, 1, ethn.frl.tbl["African American", "1"]), frl)
frl <- ifelse(ethn == "Hispanic", rbinom(N, 1, ethn.frl.tbl["Hispanic", "1"]), frl)
frl <- ifelse(ethn == "White", rbinom(N, 1, ethn.frl.tbl["White", "1"]), frl)
frl <- ifelse(ethn == "Other", rbinom(N, 1, ethn.frl.tbl["Other", "1"]), frl)
ethn.frl.tbl; propTab(ethn, frl)

##    English Learners
ell <- ifelse(ethn == "Asian" & frl == 0, rbinom(N, 1, ethn.ell.frl0["Asian", "1"]), 0)
ell <- ifelse(ethn == "Asian" & frl == 1, rbinom(N, 1, ethn.ell.frl1["Asian", "1"]), ell)
ell <- ifelse(ethn == "African American" & frl == 0, rbinom(N, 1, ethn.ell.frl0["African American", "1"]), ell)
ell <- ifelse(ethn == "African American" & frl == 1, rbinom(N, 1, ethn.ell.frl1["African American", "1"]), ell)
ell <- ifelse(ethn == "Hispanic" & frl == 0, rbinom(N, 1, ethn.ell.frl0["Hispanic", "1"]), ell)
ell <- ifelse(ethn == "Hispanic" & frl == 1, rbinom(N, 1, ethn.ell.frl1["Hispanic", "1"]), ell)
ell <- ifelse(ethn == "White" & frl == 0, rbinom(N, 1, ethn.ell.frl0["White", "1"]), ell)
ell <- ifelse(ethn == "White" & frl == 1, rbinom(N, 1, ethn.ell.frl1["White", "1"]), ell)
ell <- ifelse(ethn == "Other" & frl == 0, rbinom(N, 1, ethn.ell.frl0["Other", "1"]), ell)
ell <- ifelse(ethn == "Other" & frl == 1, rbinom(N, 1, ethn.ell.frl1["Other", "1"]), ell)
ethn.ell; propTab(ethn, ell)

##    Students with Disabilities
swd <- ifelse(ethn == "Asian" & frl == 0, rbinom(N, 1, ethn.swd.frl0["Asian", "1"]), 0)
swd <- ifelse(ethn == "Asian" & frl == 1, rbinom(N, 1, ethn.swd.frl1["Asian", "1"]), swd)
swd <- ifelse(ethn == "African American" & frl == 0, rbinom(N, 1, ethn.swd.frl0["African American", "1"]), swd)
swd <- ifelse(ethn == "African American" & frl == 1, rbinom(N, 1, ethn.swd.frl1["African American", "1"]), swd)
swd <- ifelse(ethn == "Hispanic" & frl == 0, rbinom(N, 1, ethn.swd.frl0["Hispanic", "1"]), swd)
swd <- ifelse(ethn == "Hispanic" & frl == 1, rbinom(N, 1, ethn.swd.frl1["Hispanic", "1"]), swd)
swd <- ifelse(ethn == "White" & frl == 0, rbinom(N, 1, ethn.swd.frl0["White", "1"]), swd)
swd <- ifelse(ethn == "White" & frl == 1, rbinom(N, 1, ethn.swd.frl1["White", "1"]), swd)
swd <- ifelse(ethn == "Other" & frl == 0, rbinom(N, 1, ethn.swd.frl0["Other", "1"]), swd)
swd <- ifelse(ethn == "Other" & frl == 1, rbinom(N, 1, ethn.swd.frl1["Other", "1"]), swd)
ethn.swd; propTab(ethn, swd)

###   Sample school numbers from larger schools
sch.n.count <- long_data[VALID_CASE == "VALID_CASE" & !is.na(SCALE_SCORE),
                          .(N = .N), keyby = c("SCHOOL_NUMBER", "YEAR")]
schools.to.keep <- unique(sch.n.count[YEAR== current.year & N > 19, SCHOOL_NUMBER])

sch.tbl <- propTab(long_data[SCHOOL_NUMBER %in% schools.to.keep, SCHOOL_NUMBER])
sch.num <- sample(names(sch.tbl), N, replace = TRUE, prob = sch.tbl)
head(sch.tbl); head(propTab(sch.num))


###   Combine all simulated demographic (and school number) vectors
sim_data <- data.table(ID = as.character(100001:(100000 + N)), SCHOOL_NUMBER = as.integer(sch.num), FRL = as.numeric(frl), SIM = TRUE)
# sim_data <- data.table(ID = as.character(100001:(100000 + N)), SCHOOL_NUMBER = sch.num, # as.integer(sch.num), #
#                        ETHN = ethn, FRL = frl, ELL = ell, SWD = swd, MALE = male, SIM = TRUE)
