## calculates correlations between meteorological measurements and PM2.5 by season

# split by site
input_byseason <- split(input_restructured, list(input_restructured$site, input_restructured$season))

# calculate correlation coefficient, p-values, and ci
cor <- list()
df_meteo <- list()
for (i in seq_along(input_byseason)) {
  split_daynight_var <- c("radiation_mean", "inversion_ratio", "relative_humidity") # variables zo split into day and night
  input_byseason[[i]] <- split_daynight(input_byseason[[i]], split_daynight_var, "day_night")

  meteo_var_daynight <- c(
    meteo_var,
    paste0(split_daynight_var, "_day"),
    paste0(split_daynight_var, "_night")
  )
  df_meteo[[i]] <- input_byseason[[i]][, meteo_var_daynight]

  cor[[i]] <- as.data.frame(
    t(
      sapply(df_meteo[[i]], function(x) {
        correlate(input_byseason[[i]]$PM2.5, x)
      })
    )
  )
  cor[[i]]$var <- rownames(cor[[i]])
  cor[[i]]$season <- rep(first(input_byseason[[i]]$season), nrow(cor[[i]]))
  cor[[i]]$elevation <- rep(first(input_byseason[[i]]$elevation), nrow(cor[[i]]))
}
correlation_results <- as.data.frame(do.call(rbind, cor))
correlation_results$elevation <- factor(correlation_results$elevation, levels = elevation)
correlation_results$season <- factor(correlation_results$season, levels = season)

# Plot
pCor <- ggplot(correlation_results) +
  geom_hline(yintercept = 0, col = "grey") +
  geom_hline(yintercept = c(0.4, -0.4), col = "grey", linetype = "dashed") +
  geom_linerange(aes(
    x = var, y = V1, ymin = V3, ymax = V4,
    group = elevation, col = elevation
  ), position = position_dodge(width = .5), size = .3, alpha = 1) +
  geom_col(aes(
    x = var, y = V1,
    group = elevation, col = elevation, fill = elevation
  ), position = position_dodge(width = .5), width = 0.45, size = .3) +
  labs(col = "", fill = "") +
  theme_light() +
  scale_color_manual(values = main_palette) +
  scale_fill_manual(values = palette_light) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "Pearson's correlation coefficient") +
  facet_wrap(~season, nrow = 4)
pCor

cairo_pdf("correlation_pm_meteo.pdf", width = 8, height = 5, pointsize = 10)  
pCor
dev.off()
