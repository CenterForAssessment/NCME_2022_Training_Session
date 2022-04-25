#####
###   Multiple Imputation Caterpillar Plots
#####

if (!exists("run.vignette")) run.vignette <- FALSE
if (!run.vignette) {
	plot.dir <- file.path("Multiple_Imputation_Analyses", "Imputation_Plots", "School_Catepillar")
	if (!dir.exists(plot.dir)) dir.create(plot.dir, recursive=TRUE)
}

##  Status
sch.cat.dat <- SGP_Imputation_Summaries[["Summary"]][N > 19,
    .(SCHOOL_NUMBER, Mean_SS_Complete, Mean_SS_Observed, Mean_SS_Imputed,
      SS_CI_low_simp, SS_CI_high_simp, SS_F_p_simp, Percent_Missing, N)]

sch.cat.dat[, Sig_F := fcase(
                SS_F_p_simp < 0.09, "< 0.09",
                SS_F_p_simp >= 0.09 & SS_F_p_simp < 0.19, "0.1 to 0.19",
                SS_F_p_simp >= 0.19 & SS_F_p_simp < 0.3, "0.2 to 0.3",
                SS_F_p_simp >= 0.03, "> 0.3")]
sch.cat.dat[, Sig_F := factor(Sig_F, levels = c("< 0.09", "0.1 to 0.19", "0.2 to 0.3", "> 0.3"))]
setkey(sch.cat.dat, Mean_SS_Imputed)
sch.cat.dat[, SCHOOL_RANK := seq(nrow(sch.cat.dat))]

mean.imputed <- mean(sch.cat.dat$Mean_SS_Imputed)
mean.complet <- mean(sch.cat.dat$Mean_SS_Complete)
mean.observd <- mean(sch.cat.dat$Mean_SS_Observed)

tmp.color.scale <- c("#cc4778", "#fa9b3d", "#f0f921", "#ffffff")[table(sch.cat.dat[, Sig_F]) != 0]

mss.cat <-
    ggplot(sch.cat.dat, aes(x=SCHOOL_RANK, y=Mean_SS_Observed, fill=Sig_F)) +
    geom_errorbar(aes(col=Sig_F, ymin=SS_CI_low_simp, ymax=SS_CI_high_simp), width=0, size=0.65) +
    gghighlight(Sig_F != "> 0.3", use_direct_label = FALSE, use_group_by = FALSE,
          unhighlighted_params = list(colour = NULL, alpha = 0.8, size=0.3)) +
    # geom_point(aes(col=Sig_F), size=1, show.legend=FALSE) +
    # geom_point(sch.cat.dat[Sig_F=="> 0.3"], mapping = aes(x=SCHOOL_RANK, y=Mean_SS_Observed, fill=NULL), col="white", size=1, show.legend=FALSE) +
		geom_line(sch.cat.dat, mapping = aes(x=SCHOOL_RANK, y=Mean_SS_Imputed, fill=NULL), col="green", size=0.5, show.legend=FALSE) +
		geom_point(sch.cat.dat, mapping = aes(x=SCHOOL_RANK, y=Mean_SS_Observed, fill=NULL), col="black", size=1, show.legend=FALSE) +
		geom_point(sch.cat.dat, mapping = aes(x=SCHOOL_RANK, y=Mean_SS_Complete, fill=NULL), col="red", size=1, show.legend=FALSE) +
    geom_hline(yintercept=mean.imputed, col="green") + geom_hline(yintercept=mean.observd, col="black") +
    geom_hline(yintercept=mean.complet, col="red", linetype=2) + scale_color_manual(values=tmp.color.scale) + # scale_colour_viridis_d(option = "C") +
    theme(panel.background = element_rect(colour="#747474", fill="#747474"),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour="#747474", fill="grey"),
          legend.title = element_text(size = 10, colour = "black"),
          legend.text = element_text(size = 9, colour = "black"),
          axis.title.x = element_text(size = 12),
          axis.text = element_text(size = 10, colour = "black"),
          axis.title.y = element_text(size = 12)) +
    ylab("Mean Scale Score Estimate") + xlab("School Rank (by Multiple Imputation Estimate)") + # Flipped!
    labs(color="F Significance") + coord_flip()

if (!run.vignette) {
	pdf(file.path(plot.dir, paste0("ELA_7_Sch_Achievment_Catepillar__", gsub("[.]", "_", toupper(impute.method)), ".pdf")), width=7, height = 5)
	print(mss.cat)
	dev.off()
}
##    Estimates by Percent Missing (and School Size)
mss.pctm <-
   ggplot(sch.cat.dat, aes(x=Percent_Missing, y=Mean_SS_Observed)) +
   geom_point(aes(col=Sig_F, size=N), alpha=0.35) +
   geom_errorbar(aes(col=Sig_F, ymin=SS_CI_low_simp, ymax=SS_CI_high_simp), width=0, size=0.5) +
   gghighlight(Sig_F != "> 0.3", use_direct_label = FALSE, use_group_by = FALSE,
         unhighlighted_params = list(colour = NULL, alpha = 0.8, size=0.3)) +
	 geom_point(sch.cat.dat[Sig_F == "> 0.3"], mapping = aes(x=Percent_Missing, y=Mean_SS_Observed, size=N), col="#FFFFFF", alpha=0.35, show.legend=FALSE) +
	 geom_smooth(sch.cat.dat, method = "gam", mapping = aes(x=Percent_Missing, y=Mean_SS_Imputed, fill=NULL), se=FALSE, show.legend=FALSE) +
	 geom_point(sch.cat.dat, mapping = aes(x=Percent_Missing, y=Mean_SS_Observed, fill=NULL), col="black", size=1, show.legend=FALSE) +
	 geom_point(sch.cat.dat, mapping = aes(x=Percent_Missing, y=Mean_SS_Imputed, fill=NULL), col="green", size=1, show.legend=FALSE) +
   geom_point(sch.cat.dat, mapping = aes(x=Percent_Missing, y=Mean_SS_Complete, fill=NULL), col="red", size=1, show.legend=FALSE) +
   geom_hline(yintercept=mean.imputed, col="green") + geom_hline(yintercept=mean.observd, col="black") +
   geom_hline(yintercept=mean.complet, col="red", linetype=2) + scale_color_manual(values=tmp.color.scale) +
   theme(panel.background = element_rect(colour="#747474", fill="#747474"),
         panel.border = element_blank(),
         panel.grid.minor = element_blank(),
         legend.key = element_rect(colour="#747474", fill="grey"),
         legend.title = element_text(size = 10, colour = "black"),
         legend.text = element_text(size = 9, colour = "black"),
         axis.title.x = element_text(size = 12),
         axis.text = element_text(size = 10, colour = "black"),
         axis.title.y = element_text(size = 12)) +
   ylab("Mean Scale Score") + xlab("Percent Missing") +
   labs(color="F Significance", size="School Size")

if (!run.vignette) {
	pdf(file.path(plot.dir, paste0("ELA_7_Sch_Achievment_Pct_Missing__", gsub("[.]", "_", toupper(impute.method)), ".pdf")), width=9, height = 5)
	print(mss.pctm)
	dev.off()
}

###   Growth
sch.cat.dat <- SGP_Imputation_Summaries[["Summary"]][N > 19,
    .(SCHOOL_NUMBER, Mean_SGPB_Complete, Mean_SGPB_Observed, Mean_SGPB_Imputed,
         SGPB_CI_low_simp, SGPB_CI_high_simp, SGPB_F_p_simp, Percent_Missing, N)]
sch.cat.dat[, Sig_F := fcase(
               SGPB_F_p_simp < 0.09, "< 0.09",
               SGPB_F_p_simp >= 0.09 & SGPB_F_p_simp < 0.19, "0.1 to 0.19",
               SGPB_F_p_simp >= 0.19 & SGPB_F_p_simp < 0.3, "0.2 to 0.3",
               SGPB_F_p_simp >= 0.03, "> 0.3")]
sch.cat.dat[, Sig_F := factor(Sig_F, levels = c("< 0.09", "0.1 to 0.19", "0.2 to 0.3", "> 0.3"))]

setkey(sch.cat.dat, Mean_SGPB_Imputed)
sch.cat.dat[, SCHOOL_RANK := seq(nrow(sch.cat.dat))]

mean.imputed <- mean(sch.cat.dat$Mean_SGPB_Imputed)
mean.complet <- mean(sch.cat.dat$Mean_SGPB_Complete)
mean.observd <- mean(sch.cat.dat$Mean_SGPB_Observed)

tmp.color.scale <- c("#cc4778", "#fa9b3d", "#f0f921", "#ffffff")[table(sch.cat.dat[, Sig_F]) != 0]

msgp.cat <-
    ggplot(sch.cat.dat, aes(x=SCHOOL_RANK, y=Mean_SGPB_Observed, fill=Sig_F)) +
    geom_errorbar(aes(col=Sig_F, ymin=SGPB_CI_low_simp, ymax=SGPB_CI_high_simp), width=0, size=0.65) +
    gghighlight(Sig_F != "> 0.3", use_direct_label = FALSE, use_group_by = FALSE,
          unhighlighted_params = list(colour = NULL, alpha = 0.8, size=0.3)) +
    # geom_point(aes(col=Sig_F), size=1, show.legend=FALSE) +
		# geom_point(sch.cat.dat[Sig_F=="> 0.3"], mapping = aes(x=SCHOOL_RANK, y=Mean_SGPB_Observed, fill=NULL), col="white", size=1, show.legend=FALSE) +
		geom_line(sch.cat.dat, mapping = aes(x=SCHOOL_RANK, y=Mean_SGPB_Imputed, fill=NULL), col="green", size=0.5, show.legend=FALSE) +
		geom_point(sch.cat.dat, mapping = aes(x=SCHOOL_RANK, y=Mean_SGPB_Observed, fill=NULL), col="black", size=1, show.legend=FALSE) +
    geom_point(sch.cat.dat, mapping = aes(x=SCHOOL_RANK, y=Mean_SGPB_Complete, fill=NULL), col="red", size=1, show.legend=FALSE) +
    geom_hline(yintercept=50, col="#FFFFFF") + geom_hline(yintercept=mean.observd, col="black") +
		geom_hline(yintercept=mean.imputed, col="green") + geom_hline(yintercept=mean.complet, col="red", linetype=2) +
    scale_color_manual(values=tmp.color.scale) + # scale_colour_viridis_d(option = "C") +
    theme(panel.background = element_rect(colour="#747474", fill="#747474"),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour="#747474", fill="grey"),
          legend.title = element_text(size = 10, colour = "black"),
          legend.text = element_text(size = 9, colour = "black"),
          axis.title.x = element_text(size = 12),
          axis.text = element_text(size = 10, colour = "black"),
          axis.title.y = element_text(size = 12)) +
    ylab("Mean Baseline SGP Estimate") + xlab("School Rank (by Multiple Imputation Estimate)") + # Flipped!
    labs(color="F Significance") + coord_flip()

if (!run.vignette) {
	pdf(file.path(plot.dir, paste0("ELA_7_Sch_Growth_Catepillar__", gsub("[.]", "_", toupper(impute.method)), ".pdf")), width=7, height = 5)
	print(msgp.cat)
	dev.off()
}

##    Estimates by Percent Missing (and School Size)
msgp.pctm <-
    ggplot(sch.cat.dat, aes(x=Percent_Missing, y=Mean_SGPB_Observed)) +
    geom_point(aes(col=Sig_F, size=N), alpha=0.35) +
    geom_errorbar(aes(col=Sig_F, ymin=SGPB_CI_low_simp, ymax=SGPB_CI_high_simp), width=0, size=0.5) +
    gghighlight(Sig_F != "> 0.3", use_direct_label = FALSE, use_group_by = FALSE,
          unhighlighted_params = list(colour = NULL, alpha = 0.8, size=0.3)) +
		geom_point(sch.cat.dat[Sig_F == "> 0.3"], mapping = aes(x=Percent_Missing, y=Mean_SGPB_Observed, size=N), col="#FFFFFF", alpha=0.35, show.legend=FALSE) +
		geom_smooth(sch.cat.dat, method = "gam", mapping = aes(x=Percent_Missing, y=Mean_SGPB_Imputed, fill=NULL), se=FALSE, show.legend=FALSE) +
		geom_point(sch.cat.dat, mapping = aes(x=Percent_Missing, y=Mean_SGPB_Observed, fill=NULL), col="black", size=1, show.legend=FALSE) +
		geom_point(sch.cat.dat, mapping = aes(x=Percent_Missing, y=Mean_SGPB_Imputed, fill=NULL), col="green", size=1, show.legend=FALSE) +
    geom_point(sch.cat.dat, mapping = aes(x=Percent_Missing, y=Mean_SGPB_Complete, fill=NULL), col="red", size=1, show.legend=FALSE) +
    geom_hline(yintercept=mean.imputed, col="green") + geom_hline(yintercept=mean.observd, col="black") +
    geom_hline(yintercept=mean.complet, col="red", linetype=2) + scale_color_manual(values=tmp.color.scale) +
    theme(panel.background = element_rect(colour="#747474", fill="#747474"),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour="#747474", fill="grey"),
          legend.title = element_text(size = 10, colour = "black"),
          legend.text = element_text(size = 9, colour = "black"),
          axis.title.x = element_text(size = 12),
          axis.text = element_text(size = 10, colour = "black"),
          axis.title.y = element_text(size = 12)) +
    ylab("Mean Baseline SGP") + xlab("Percent Missing") +
    labs(color="F Significance", size="School Size")

if (!run.vignette) {
	pdf(file.path(plot.dir, paste0("ELA_7_Sch_Growth_Pct_Missing__", gsub("[.]", "_", toupper(impute.method)), ".pdf")), width=9, height = 5)
	print(msgp.pctm)
	dev.off()
}
