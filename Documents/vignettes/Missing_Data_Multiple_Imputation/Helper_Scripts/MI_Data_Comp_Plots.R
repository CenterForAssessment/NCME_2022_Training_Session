
plot.range <- round(range(long_data$Z_SCORE, na.rm = TRUE), 1)

truth.p <- ggplot(wide_data, aes(y = COMPLETE_Z.2019, x = COMPLETE_Z.2018)) + theme_bw() +
              geom_hex(bins=50, show.legend = FALSE) + xlim(plot.range) + ylim(plot.range) +
              scale_fill_gradient(low = "grey", high = "black", na.value = NA) +
              geom_smooth(method = "gam", se = FALSE, color = "red") +
              ggtitle("2018 to 2019 Actual") + theme(plot.title = element_text(hjust = 0.5, size=14))
truth.c <- ggplot(wide_data, aes(y = COMPLETE_Z.2021, x = COMPLETE_Z.2019)) + theme_bw() +
              geom_hex(bins=50, show.legend = FALSE) + xlim(plot.range) + ylim(plot.range) +
              scale_fill_gradient(low = "grey", high = "black", na.value = NA) +
              geom_smooth(method = "gam", se = FALSE, color = "red") +
              ggtitle("2019 to 2021 Actual") + theme(plot.title = element_text(hjust = 0.5, size=14))

impute.p <- ggplot(single_imputation, aes(y = Z_SCORE.2019, x = Z_SCORE.2018)) + theme_bw() +
              geom_hex(bins=50, show.legend = FALSE) + xlim(plot.range) + ylim(plot.range) +
              scale_fill_gradient(low = "grey", high = "black", na.value = NA) +
              geom_smooth(method = "gam", se = FALSE, color = "red") +
              ggtitle("2018 to 2019 Imputed") + theme(plot.title = element_text(hjust = 0.5, size=14))
impute.c <- ggplot(single_imputation, aes(y = Z_SCORE, x = Z_SCORE.2019)) + theme_bw() +
              geom_hex(bins=50, show.legend = FALSE) + xlim(plot.range) + ylim(plot.range) +
              scale_fill_gradient(low = "grey", high = "black", na.value = NA) +
              geom_smooth(method = "gam", se = FALSE, color = "red") +
              ggtitle("2019 to 2021 Imputed") + theme(plot.title = element_text(hjust = 0.5, size=14))

ampute.p <- ggplot(wide_data, aes(y = Z_SCORE.2019, x = Z_SCORE.2018)) + theme_bw() +
              geom_hex(bins=50, show.legend = FALSE) + xlim(plot.range) + ylim(plot.range) +
              scale_fill_gradient(low = "grey", high = "black", na.value = NA) +
              geom_smooth(method = "gam", se = FALSE, color = "red") +
              ggtitle("2018 to 2019 Amputed") + theme(plot.title = element_text(hjust = 0.5, size=14))
ampute.c <- ggplot(wide_data, aes(y = Z_SCORE.2021, x = Z_SCORE.2019)) + theme_bw() +
              geom_hex(bins=50, show.legend = FALSE) + xlim(plot.range) + ylim(plot.range) +
              scale_fill_gradient(low = "grey", high = "black", na.value = NA) +
              geom_smooth(method = "gam", se = FALSE, color = "red") +
              ggtitle("2019 to 2021 Amputed") + theme(plot.title = element_text(hjust = 0.5, size=14))

if (!run.vignette) {
	pdf(file = file.path(diagnostics.dir, plot.name), width=7, height=10)
	gridExtra::grid.arrange(truth.p, truth.c, impute.p, impute.c, ampute.p, ampute.c, ncol=2)
	dev.off()
}
