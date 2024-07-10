## calculates correlations
## between meteorological measurements and PM2.5 by season

# split by site
input_byseason <- split(
  input_restructured,
  list(input_restructured$site, input_restructured$season)
)

#Initialize lists to store correlation results and meteorological data
cor <- list()
df_meteo <- list()

# calculate correlation coefficient, p-values, and ci
# Loop over each subset of the data (by site and season)
for (i in seq_along(input_byseason)) {
  # Variables to split into day and night
  split_daynight_var <- c(
    "radiation_mean",
    "inversion_ratio",
    "relative_humidity"
  )
  # Apply day/night split function to the subset
  input_byseason[[i]] <- split_daynight(
    input_byseason[[i]],
    split_daynight_var,
    "day_night"
  )
  # Create a list of meteorological variables including day and night splits
  meteo_var_daynight <- c(
    meteo_var,
    paste0(split_daynight_var, "_day"),
    paste0(split_daynight_var, "_night")
  )
  # Select the meteorological variables for correlation calculation
  df_meteo[[i]] <- input_byseason[[i]][, meteo_var_daynight]

  # Calculate correlation coefficients (V1), p-values (V2),
  # and confidence intervals (min: V3, max: V4)
  cor[[i]] <- as.data.frame(
    t(
      sapply(df_meteo[[i]], function(x) {
        correlate(input_byseason[[i]]$PM2.5, x)
      })
    )
  )
  # Add variable names, season, and elevation information to the results
  cor[[i]]$var <- rownames(cor[[i]])
  cor[[i]]$season <- rep(first(input_byseason[[i]]$season), nrow(cor[[i]]))
  cor[[i]]$elevation <- rep(
    first(input_byseason[[i]]$elevation),
    nrow(cor[[i]])
  )
}
# Combine all correlation results into a single df
correlation_results <- as.data.frame(do.call(rbind, cor))

# Convert elevation and season to factors with specific levels
correlation_results$elevation <- factor(correlation_results$elevation,
  levels = elevation
)
correlation_results$season <- factor(correlation_results$season,
  levels = season
)

# Plot output, if needed
view(correlation_results)

# Plot
p_cor <- ggplot(correlation_results) +
  geom_hline(yintercept = 0, col = "grey") + # Add horizontal line at y=0
  geom_hline(yintercept = c(0.4, -0.4), col = "grey", linetype = "dashed") +
  # Add dashed lines at y=0.4 and y=-0.4
  geom_linerange(aes(
    x = var, y = V1, ymin = V3, ymax = V4,
    group = elevation, col = elevation
  ), position = position_dodge(width = .5), size = .3, alpha = 1) +
  # Add linerange for confidence intervals
  geom_col(aes(
    x = var, y = V1,
    group = elevation, col = elevation, fill = elevation
  ), position = position_dodge(width = .5), width = 0.45, size = .3) +
  # Add bars for correlation coefficients
  labs(col = "", fill = "") + # Remove labels from legend
  theme_light() +
  scale_color_manual(values = main_palette) +
  scale_fill_manual(values = palette_light) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "Pearson's correlation coefficient") + # Axis labels
  facet_wrap(~season, nrow = 4) #Create panels for each season
p_cor

cairo_pdf("correlation_pm_meteo_season.pdf",
  width = 10,
  height = 14,
  pointsize = 10
)
p_cor
dev.off()
