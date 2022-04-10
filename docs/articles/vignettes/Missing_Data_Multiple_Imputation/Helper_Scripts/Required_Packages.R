#####
###   Update and install packages required for various analyses, plots, summaries, etc.
###   included in the "Missing Data and Multiple Imputation" vignette and R script.
###   Separate sections for CRAN and Github package versions.
#####

###   Update user's packages
# update.packages(ask = FALSE, checkBuilt = TRUE)


###   Install/update latest packages from CRAN

##    Data management & manipulation
if (!require(data.table)) {
	install.packages("data.table", dep=T)
}

##    Missing data simulation, imputation and visualization
if (!require(VIM)) {
	install.packages("VIM", dep=T)
}
if (!require(mice)) {
	install.packages("mice", dep=T)
}
if (!require(miceadds)) {
	install.packages("miceadds", dep=T)
}

##    Parametric imputed data analysis
if (!require(lme4)) {
	install.packages("lme4", dep=T)
}
if (!require(merTools)) {
	install.packages("merTools", dep=T)
}


##    Plotting
if (!require(ggplot2)) {
	install.packages("ggplot2", dep=T)
}

if (!require(gghighlight)) {
	install.packages("gghighlight", dep=T)
}

###   Install/update latest packages from GITHUB
if (!require(remotes)) {
	install.packages("remotes", dep=T)
}

remotes::install_github("centerforassessment/cfaTools")
remotes::install_github("centerforassessment/SGPdata"); require(SGPdata)
remotes::install_github("centerforassessment/SGP"); require(SGP)

