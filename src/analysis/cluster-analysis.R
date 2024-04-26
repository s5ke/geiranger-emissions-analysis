## Cluster analysis
n_cluster <- 7
n_lag_days <- 100
min_length_cluster <- 2

# restructure input
# daily
input_cluster_daily_unscaled <- replace(
  input_daily[, meteo_var],
  is.na(input_daily[, meteo_var]) == TRUE, 0
)
input_cluster_daily_unscaled[, meteo_var_not_averraged] <- NULL

# scale and center
input_cluster_daily_scaled <- as.data.frame(scale(input_cluster_daily_unscaled))
cluster_var <- colnames(input_cluster_daily_scaled)

# add on/off-season from ships data (ships in port/no ships in port)
input_cluster_daily_scaled$season <- input_daily$season
input_cluster_daily_scaled$date <- input_daily$date
input_cluster_daily_scaled$day <- input_daily$date
input_cluster_daily_scaled$doy <- yday(input_daily$date)
input_cluster_daily_scaled$year <- input_daily$year
input_cluster_daily_scaled$site <- input_daily$site
input_cluster_daily_scaled$ships <- input_daily$all_ships
input_cluster_daily_scaled$model <- input_daily$model
input_cluster_daily_scaled$PM2.5 <- input_daily$PM2.5
input_cluster_daily_scaled$on_off_season_ships <- replace(
  input_cluster_daily_scaled$ships,
  input_cluster_daily_scaled$ships == 0, 0
)
input_cluster_daily_scaled$on_off_season_ships <-
  as.numeric(replace(
    input_cluster_daily_scaled$on_off_season_ships,
    input_cluster_daily_scaled$ships != 0, 1
  ))

input_cluster_daily_scaled$on_off_season <- replace(
  input_cluster_daily_scaled$season,
  input_cluster_daily_scaled$doy < 91 |
    input_cluster_daily_scaled$doy > 301, "off-season"
)
input_cluster_daily_scaled$on_off_season <- replace(
  input_cluster_daily_scaled$on_off_season,
  input_cluster_daily_scaled$on_off_season != "off-season", "on-season"
)

for (i in site) {
  input_cluster_daily_scaled[, i] <- rep(
    subset(
      input_cluster_daily_scaled,
      input_cluster_daily_scaled$site == i
    )$PM2.5,
    length(site)
  )
  input_cluster_daily_scaled[, paste0("model", i)] <- rep(
    subset(
      input_cluster_daily_scaled,
      input_cluster_daily_scaled$site == i
    )$model,
    length(site)
  )
}

input_cluster_daily_unscaled <- subset(
  input_cluster_daily_unscaled,
  input_cluster_daily_scaled$site == site[1]
)

input_cluster_daily_scaled <- subset(
  input_cluster_daily_scaled,
  input_cluster_daily_scaled$site == site[1]
)

input_cluster_daily_unscaled <- subset(
  input_cluster_daily_unscaled,
  input_cluster_daily_scaled$on_off_season == "on-season"
)

input_cluster_daily_scaled <- subset(
  input_cluster_daily_scaled,
  input_cluster_daily_scaled$on_off_season == "on-season"
)


# calculate kmeans
calculate_cluster <- function(df, df_unscaled, n, min_length_cluster, var) {
  input_kmeans <- df[, var]
  kmeans <- kmeans(input_kmeans,
    n,
    algorithm = "Hartigan-Wong"
  )
  kmeans[["betweenss"]] / kmeans[["totss"]] # sum of squares ratio
  kmeans[["totss"]]

  # Smoothing
  df$cluster <- as.numeric(unlist(kmeans$cluster))
  rle <- rle(df$cluster)
  df$length <- unlist(
    sapply(seq_along(rle[[2]]), function(x) {
      rep(
        rle[[1]][x],
        rle[[1]][x]
      )
    })
  )

  df$smooth <- ifelse(df$length < min_length_cluster,
    "exception",
    "cluster"
  )
  df <- as.data.frame(cbind(df, unscaled = df_unscaled))

  kmeans_reduced <- subset(df, df$smooth != "exception")
  n <- nrow(df) - nrow(kmeans_reduced)
  print(n)

  rle <- rle(kmeans_reduced$cluster)
  kmeans_reduced$length <- unlist(
    sapply(seq_along(rle[[2]]), function(x) {
      rep(
        rle[[1]][x],
        rle[[1]][x]
      )
    })
  )

  kmeans_reduced
}
kmeans_reduced <-
  calculate_cluster(
    input_cluster_daily_scaled,
    input_cluster_daily_unscaled,
    n_cluster,
    min_length_cluster,
    cluster_var
  )

# calculate percentage of each cluster
kmeans_byseason <- split(
  kmeans_reduced,
  list(
    kmeans_reduced$on_off_season
  )
)

perc <- list()
cluster_means <- list()
cluster_means_sd <- list()
all_clusters <- list()
all_clusters_sd <- list()
for (i in seq_along(kmeans_byseason)) {
  perc[[i]] <- split(
    kmeans_byseason[[i]],
    kmeans_byseason[[i]]$cluster
  )
  perc[[i]] <- lapply(
    perc[[i]],
    function(x) nrow(x) / nrow(kmeans_byseason[[i]])
  )
  perc[[i]] <- do.call(rbind, perc[[i]])

  cluster_means[[i]] <- aggregate(
    kmeans_byseason[[i]][
      ,
      c(cluster_var, "length", paste("unscaled", cluster_var, sep = "."))
    ],
    by = list(cluster = kmeans_byseason[[i]]$cluster), mean
  )
  cluster_means_sd[[i]] <- aggregate(
    kmeans_byseason[[i]][
      ,
      c(cluster_var, "length", paste("unscaled", cluster_var, sep = "."))
    ],
    by = list(cluster = kmeans_byseason[[i]]$cluster), sd
  )

  cluster_means[[i]]$perc <- perc[[i]][, 1]
  cluster_means[[i]]$season <- rep(
    first(kmeans_byseason[[i]]$on_off_season),
    nrow(cluster_means[[i]])
  )

  cluster_means[[i]]$cluster <-
    paste("Cluster", cluster_means[[i]]$cluster)

  all_clusters[[i]] <- c(
    cluster = "All clusters",
    colMeans(cluster_means[[i]][
      ,
      c(cluster_var, "length", paste("unscaled", cluster_var, sep = "."))
    ]),
    perc = NA,
    season = first(cluster_means[[i]]$season)
  )

  all_clusters_sd[[i]] <- c(
    cluster = "All clusters",
    sapply(cluster_means[[i]][
      ,
      c(cluster_var, "length", paste("unscaled", cluster_var, sep = "."))
    ], sd),
    perc = NA,
    season = first(cluster_means[[i]]$season)
  )

  cluster_means[[i]] <- as.data.frame(rbind(
    cluster_means[[i]],
    all_clusters[[i]]
  ))

  cluster_means_sd[[i]] <- as.data.frame(rbind(
    cluster_means_sd[[i]],
    all_clusters_sd[[i]]
  ))
}
cluster_means <- as.data.frame(do.call(rbind, cluster_means))
cluster_means_sd <- as.data.frame(do.call(rbind, cluster_means_sd))

pinput_cluster_means <- data.frame(
  value = unlist(cluster_means[, cluster_var]),
  value_unscaled = unlist(
    cluster_means[, paste("unscaled", cluster_var, sep = ".")]
  ),
  sd = unlist(cluster_means_sd[, cluster_var]),
  sd_unscaled = unlist(
    cluster_means_sd[, paste("unscaled", cluster_var, sep = ".")]
  ),
  length = rep(cluster_means$length, length(cluster_var)),
  season = rep(cluster_means$season, length(cluster_var)),
  cluster = rep(cluster_means$cluster, length(cluster_var)),
  var = rep(cluster_var, each = nrow(cluster_means))
)

length_labels <- aggregate(pinput_cluster_means,
  by = list(
    pinput_cluster_means$cluster,
    pinput_cluster_means$season
  ),
  first
)

x <- pinput_cluster_means$var
old <- cluster_var
new <- c(
  "air pressure [hPa]",
  "precipitation [mm]",
  "relative humidity [%]",
  "radiation [W/m²]",
  "temperature [°C]",
  "inversion ratio [°K]",
  "wind direction (east/west)",
  "wind direction (north/south)",
  "maximum wind speed [m/s]",
  "average wind speed [m/s]"
)
x[x %in% old] <- new[match(x, old, nomatch = 0)]
pinput_cluster_means$var <- x

# Plot clusters
p_cluster <- ggplot(subset(
  pinput_cluster_means,
  pinput_cluster_means$season == "on-season"
)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(
      x = var,
      y = as.numeric(value),
      ymin = as.numeric(value) - as.numeric(sd) / 2,
      ymax = as.numeric(value) + as.numeric(sd) / 2,
      group = paste(var, season),
      col = var
    ),
    size = .2
  ) +
  scale_color_manual(values = palette_cluster) +
  facet_wrap(~ cluster + season, nrow = 1) +
  geom_text(
    data = subset(length_labels, length_labels$season == "on-season"),
    aes(
      x = Inf,
      y = -Inf,
      hjust = 1.1,
      vjust = -0.2,
      label = round(as.numeric(length), 2)
    ),
    size = 3
  ) +
  theme_light() +
  labs(
    x = "",
    y = "measured variable (averaged, scaled and centered)",
    col = ""
  ) +
  theme(axis.text.x = element_text(angle = 90))
p_cluster

cairo_pdf("cluster_on_season.pdf", width = 14, height = 5, pointsize = 10)
p_cluster
dev.off()

p_cluster_transformed <- ggplot(subset(
  pinput_cluster_means,
  pinput_cluster_means$season == "on-season"
)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(
      x = var,
      y = as.numeric(value),
      ymin = as.numeric(value) - as.numeric(sd) / 2,
      ymax = as.numeric(value) + as.numeric(sd) / 2,
      group = paste(var, season),
      col = var
    ),
    size = .2
  ) +
  scale_color_manual(values = palette_cluster) +
  facet_wrap(~ cluster + season, ncol = 1) +
  geom_text(
    data = subset(length_labels, length_labels$season == "on-season"),
    aes(
      x = Inf,
      y = -Inf,
      hjust = 1.1,
      vjust = -0.2,
      label = round(as.numeric(length), 2)
    ),
    size = 3
  ) +
  theme_light() +
  labs(
    x = "",
    y = "measured variable (averaged, scaled and centered)",
    col = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "none"
  )
p_cluster_transformed

p_cluster_x <- ggplot(subset(
  pinput_cluster_means,
  pinput_cluster_means$season == "on-season"
)) +
  geom_pointrange(aes(
    x = cluster,
    y = as.numeric(value_unscaled),
    ymin = as.numeric(value_unscaled) - as.numeric(sd_unscaled) / 2,
    ymax = as.numeric(value_unscaled) + as.numeric(sd_unscaled) / 2,
    group = paste(var, season),
    col = var
  ), size = .2) +
  scale_color_manual(values = palette_cluster) +
  facet_wrap(~var, nrow = 1, scale = "free_y") +
  theme_light() +
  labs(x = "", y = "measured variable", col = "") +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "none"
  )
p_cluster_x

cairo_pdf("cluster_conditions.pdf", width = 18.5, height = 4, pointsize = 10)
p_cluster_x
dev.off()

pinput_boxplot <- data.frame(
  elevation = rep(elevation, nrow(kmeans_reduced)),
  PM2.5 = unlist(kmeans_reduced[, site]),
  cluster = paste("Cluster", rep(kmeans_reduced$cluster, length(site))),
  season = rep(kmeans_reduced$on_off_season, length(site))
)
pinput_boxplot_all <- pinput_boxplot
pinput_boxplot_all$cluster <- rep("All clusters", nrow(pinput_boxplot))
pinput_boxplot <- as.data.frame(rbind(pinput_boxplot, pinput_boxplot_all))

# boxplot
p_boxplot <- ggplot(subset(
  pinput_boxplot,
  pinput_boxplot$season == "on-season"
)) +
  geom_boxplot(aes(
    x = elevation,
    y = PM2.5,
    group = elevation,
    col = elevation
  ), outlier.shape = NA) +
  scale_color_manual(values = palette_elevation) +
  ylim(c(0, 20)) +
  theme_light() +
  labs(x = "", y = "PM2.5 [µg/m³]") +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "none"
  ) +
  facet_wrap(~ cluster + season, ncol = 1)
p_boxplot

cairo_pdf("PM_cluster_on_season.pdf", width = 4, height = 20, pointsize = 10)
p_boxplot
dev.off()

# lagged correlations
input_lagged <- do.call("rbind", replicate(
  length(site),
  kmeans_reduced,
  simplify = FALSE
))
input_lagged$PM2.5 <- unlist(kmeans_reduced[, site])
input_lagged$model <- unlist(kmeans_reduced[, paste0("model", site)])
input_lagged$site <- rep(elevation, each = nrow(kmeans_reduced))

input_lagged <- split(input_lagged, input_lagged$site)
lead_pm <- list()
lead_model <- list()
input_pm <- list()
input_model <- list()
for (i in seq_along(input_lagged)) {
  input_lagged[[i]]$PM2.5_scaled <- scale(input_lagged[[i]]$PM2.5)
  input_lagged[[i]]$model_scaled <- scale(input_lagged[[i]]$model)
  input_lagged[[i]]$ships_scaled <- scale(input_lagged[[i]]$ships)
  lead_pm[[i]] <- as.data.frame(sapply(
    c(1:n_lag_days),
    function(x) lead(input_lagged[[i]]$PM2.5_scaled, n = x)
  ))
  lead_model[[i]] <- as.data.frame(sapply(
    c(1:n_lag_days),
    function(x) lead(input_lagged[[i]]$model_scaled, n = x)
  ))
  input_pm[[i]] <- as.data.frame(cbind(
    input_lagged[[i]],
    lead_pm[[i]]
  ))
  input_model[[i]] <- as.data.frame(cbind(
    input_lagged[[i]],
    lead_model[[i]]
  ))
}
input_pm <- as.data.frame(do.call(rbind, input_pm))
input_model <- as.data.frame(do.call(rbind, input_model))

input_pm_all <- input_pm
input_pm_all$cluster <- rep("All clusters", nrow(input_pm_all))
input_pm <- as.data.frame(rbind(input_pm, input_pm_all))
input_pm <- split(input_pm, list(input_pm$site, input_pm$cluster))

input_model_all <- input_model
input_model_all$cluster <- rep("All clusters", nrow(input_model_all))
input_model <- as.data.frame(rbind(input_model, input_model_all))
input_model <- split(input_model, list(input_model$site, input_model$cluster))

cor_lead_pm <- list()
for (i in seq_along(input_pm)) {
  cor_lead_pm[[i]] <- as.data.frame(rbind(
    t(sapply(
      input_pm[[i]][, paste0("V", c(1:n_lag_days))],
      function(x) correlate(input_pm[[i]]$ships_scaled, x)
    )),
    t(sapply(
      input_model[[i]][, paste0("V", c(1:n_lag_days))],
      function(x) correlate(input_model[[i]]$ships_scaled, x)
    ))
  ))
  cor_lead_pm[[i]]$lead <- rep(c(1:n_lag_days), 2)
  cor_lead_pm[[i]]$method <- rep(c("measured", "modeled"), each = n_lag_days)
}
cor_lead_pm <- as.data.frame(do.call(rbind, cor_lead_pm))
colnames(cor_lead_pm) <- c("cor", "p", "ci_5", "ci_95", "lead", "method")
cor_lead_pm$id <- rep(names(input_pm), each = n_lag_days * 2)
cor_lead_pm$cluster <- paste(
  "Cluster",
  sapply(strsplit(cor_lead_pm$id, "[.]"), "[[", 5)
)
cor_lead_pm$cluster <- replace(
  cor_lead_pm$cluster,
  cor_lead_pm$cluster == "Cluster All clusters", "All clusters"
)
cor_lead_pm$site <- paste(sapply(strsplit(cor_lead_pm$id, "[.]"), "[[", 1),
  sapply(strsplit(cor_lead_pm$id, "[.]"), "[[", 2),
  sapply(strsplit(cor_lead_pm$id, "[.]"), "[[", 3),
  sep = "."
)
cor_lead_pm$sig <- replace(cor_lead_pm$cor, cor_lead_pm$p > 0.1, NA)
cor_lead_pm$site <- paste0(cor_lead_pm$site, ".")
cor_lead_pm$site <- replace(
  cor_lead_pm$site,
  cor_lead_pm$method == "modeled",
  "model"
)
unique(cor_lead_pm$site)
cor_lead_pm$site <- factor(cor_lead_pm$site,
  levels = c(elevation, "model")
)

# Plot
p_lagged <- ggplot(cor_lead_pm) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = c(0.5, -0.5), linetype = "dashed", col = "grey") +
  geom_point(aes(
    x = lead, y = cor,
    group = paste(site, method),
    col = site
  ), size = 1.5, alpha = .3) +
  geom_point(aes(
    x = lead, y = sig,
    group = paste(site, method),
    col = site,
  ), size = 1.5) +
  geom_line(aes(
    x = lead, y = cor,
    group = paste(site, method),
    col = site,
  ), size = .3, alpha = .3) +
  geom_line(aes(
    x = lead, y = sig,
    group = paste(site, method),
    col = site,
  ), size = .3) +
  facet_wrap(~cluster, ncol = 1) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 20)) +
  scale_color_manual(values = c(palette_elevation, "black")) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    y = "Correlation coefficient (Pearson)",
    x = "Time (lead) [days]",
    col = "elevation"
  )
p_lagged

cairo_pdf("cluster_analysis_on_season.pdf",
  width = 16,
  height = 15,
  pointsize = 10
)
ggarrange(
  plotlist = list(p_cluster_transformed, p_boxplot, p_lagged), widths = c(1, 1, 3),
  ncol = 3, align = "hv", common.legend = FALSE, labels = c("A", "B", "C")
)
dev.off()

p_lagged_all <- ggplot(subset(
  cor_lead_pm,
  cor_lead_pm$cluster == "All clusters"
)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(
    x = lead,
    y = cor,
    group = paste(site, method),
    col = site
  ), size = 1, alpha = .3) +
  geom_point(aes(
    x = lead,
    y = sig,
    group = paste(site, method),
    col = site
  ), size = 1) +
  geom_line(aes(
    x = lead,
    y = cor,
    group = paste(site, method),
    col = site
  ), size = .3, alpha = .3) +
  geom_line(aes(
    x = lead,
    y = sig,
    group = paste(site, method),
    col = site
  ), size = .3) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_color_manual(values = c(palette_elevation, "black")) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    y = "Correlation coefficient (Pearson)",
    x = "Time (lead) [days]",
    col = "elevation"
  )
p_lagged_all

cairo_pdf("correlation_on_season.pdf", width = 10, height = 4, pointsize = 10)
p_lagged_all
dev.off()
