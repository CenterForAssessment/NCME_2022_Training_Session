if (!dir.exists(file.path(diagnostics.dir, "Sim_Data_Comps"))) dir.create(file.path(diagnostics.dir, "Sim_Data_Comps"), recursive = TRUE)

tl <- ggplot(sim_imp_wide, aes(x = SCALE_SCORE.2019, y = SCALE_SCORE.2018)) + theme_bw() +
        geom_hex(bins=35, show.legend = FALSE) + xlim(plot.range) + ylim(plot.range) +
        scale_fill_gradient(low = "grey", high = "black", na.value = NA) +
        geom_smooth(method = "gam", se = FALSE, color = "red") +
        ggtitle("Generated Data") + theme(plot.title = element_text(hjust = 0.5, size=14))
tr <- ggplot(wide_data, aes(x = SCALE_SCORE.2019, y = SCALE_SCORE.2018)) + theme_bw() +
        geom_hex(bins=35, show.legend = FALSE) + xlim(plot.range) + ylim(plot.range) +
        scale_fill_gradient(low = "grey", high = "black", na.value = NA) +
        geom_smooth(method = "gam", se = FALSE, color = "red") +
        ggtitle("Observed Data") + theme(plot.title = element_text(hjust = 0.5, size=14))

ml <- ggplot(sim_imp_wide, aes(x = SCALE_SCORE.2021, y = SCALE_SCORE.2018)) + theme_bw() +
        geom_hex(bins=35, show.legend = FALSE) + xlim(plot.range) + ylim(plot.range) +
        scale_fill_gradient(low = "grey", high = "black", na.value = NA) +
        geom_smooth(method = "gam", se = FALSE, color = "red")
mr <- ggplot(wide_data, aes(x = SCALE_SCORE.2021, y = SCALE_SCORE.2018)) + theme_bw() +
        geom_hex(bins=35, show.legend = FALSE) + xlim(plot.range) + ylim(plot.range) +
        scale_fill_gradient(low = "grey", high = "black", na.value = NA) +
        geom_smooth(method = "gam", se = FALSE, color = "red")

bl <- ggplot(sim_imp_wide, aes(x = SCALE_SCORE.2021, y = SCALE_SCORE.2019)) + theme_bw() +
        geom_hex(bins=35, show.legend = FALSE) + xlim(plot.range) + ylim(plot.range) +
        scale_fill_gradient(low = "grey", high = "black", na.value = NA) +
        geom_smooth(method = "gam", se = FALSE, color = "red")
br <- ggplot(wide_data, aes(x = SCALE_SCORE.2021, y = SCALE_SCORE.2019)) + theme_bw() +
        geom_hex(bins=35, show.legend = FALSE) + xlim(plot.range) + ylim(plot.range) +
        scale_fill_gradient(low = "grey", high = "black", na.value = NA) +
        geom_smooth(method = "gam", se = FALSE, color = "red")

svglite::svglite(filename = file.path(diagnostics.dir, "Sim_Data_Comps", plot.name), pointsize = 12, width=7, height=10)
gridExtra::grid.arrange(tl, tr, ml, mr, bl, br, ncol=2)
dev.off()
