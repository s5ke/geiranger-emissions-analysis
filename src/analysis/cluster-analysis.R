## Cluster analysis
n_cluster <- 7
n_lag_days <- 200
min_length_cluster <- 2

# restructure input
# daily
# Replace NA values from meteo_var with 0
input_cluster_daily_unscaled <- replace(
  input_daily[, meteo_var],
  is.na(input_daily[, meteo_var]) == TRUE, 0
)
# Here, we exclude those variables that will not be used for the cluster
# analysis
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
# I do not get the next part here. This is a copy of the "ships" column
# initiated before. The replacement does not really do anything!??
input_cluster_daily_scaled$on_off_season_ships <- replace(
  input_cluster_daily_scaled$ships,
  input_cluster_daily_scaled$ships == 0, 0
)
# Now, we replace all non-zeroes in the ships-column with 1 to get a numerical
# column indicating whether there was a ship at port (1), or not (0)
input_cluster_daily_scaled$on_off_season_ships <-
  as.numeric(replace(
    input_cluster_daily_scaled$on_off_season_ships,
    input_cluster_daily_scaled$ships != 0, 1
  ))
#Print output, if needed
view(input_cluster_daily_scaled)

# Re-arrange season-column
input_cluster_daily_scaled$on_off_season <- replace(
  input_cluster_daily_scaled$season,
  input_cluster_daily_scaled$doy < 91 |
    input_cluster_daily_scaled$doy > 301, "off-season"
)
input_cluster_daily_scaled$on_off_season <- replace(
  input_cluster_daily_scaled$on_off_season,
  input_cluster_daily_scaled$on_off_season != "off-season", "on-season"
)
#Print output, if needed
view(input_cluster_daily_scaled)

#Create new columns containing measured and modeled PM2.5
# for each site, repeated consecutively for each site
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
#Print output, if needed
view(input_cluster_daily_scaled)

# Subsetting both scaled and unscaled dataset, as they are still
# containing all values of each site after another. However, we
# only need one set of variables, that's why we subset accordingly
# In the last two block, we get only on-season data
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
  # Extracting the columns of interest from the dataframe for clustering
  input_kmeans <- df[, var]

  # Performing k-means clustering on the selected variables
  kmeans <- kmeans(input_kmeans,
    n, # Number of clusters
    algorithm = "Hartigan-Wong" # Algorithm used for clustering
  )
  kmeans[["betweenss"]] / kmeans[["totss"]] # Calculating the ratio of
  # between-cluster sum of squares to total sum of squares
  kmeans[["totss"]]

  # Total Sum of Squares ("totss") is a measure of the total variance
  # in the dataset. It represents the sum of the squared differences
  # between each data point and the overall mean of the dataset.

  # Between Cluster Sum of Squares ("betweenss") measures the variance between
  # the clusters. It represents the sum of the squared differences between
  # the cluster centroids and the overall mean of the dataset, weighted
  # by the number of points in each cluster.

  # The ratio of "betweenss" and "totss" indicates the proportion of the total
  # variance that is explained by the clustering. It helps assess the
  # effectiveness of the clustering process.
  # =>  A higher ratio indicates that a larger proportion of the total variance
  #     is accounted for by the differences between clusters, suggesting
  #     better-defined and more distinct clusters.
  # =>  A lower ratio suggests that the clusters are not as distinct, meaning
  #     more of the total variance is within the clusters rather than between
  #     them.

  # Adding the cluster assignments to the dataframe
  df$cluster <- as.numeric(unlist(kmeans$cluster))

  # Running length encoding to determine lengths of consecutive 
  # identical values in 'cluster'
  rle <- rle(df$cluster)

  # Creating a 'length' column which contains the lengths of these consecutive
  # identical values
  df$length <- unlist(
    sapply(seq_along(rle[[2]]), function(x) {
      rep(
        rle[[1]][x], # Value to be repeated (length of the run)
        rle[[1]][x]  # Number of repetitions (length of the run)
      )
    })
  )

  # Creating a 'smooth' column to identify clusters that are shorter than
  # the minimum length
  df$smooth <- ifelse(df$length < min_length_cluster,
    "exception",  # Mark as exception if the cluster length is less 
                  # than the minimum length
    "cluster"     # Otherwise, mark as cluster
  )

  # Combining the original unscaled data with the current dataframe
  df <- as.data.frame(cbind(df, unscaled = df_unscaled))

  # Subsetting the dataframe to remove exceptions (short clusters)
  kmeans_reduced <- subset(df, df$smooth != "exception")
  # Counting the number of rows removed as exceptions
  n <- nrow(df) - nrow(kmeans_reduced)
  print(n)
  
  # Running length encoding again on the reduced dataframe
  rle <- rle(kmeans_reduced$cluster)

  # Updating the 'length' column for the reduced dataframe
  kmeans_reduced$length <- unlist(
    sapply(seq_along(rle[[2]]), function(x) {
      rep(
        rle[[1]][x], # Value to be repeated (length of the run)
        rle[[1]][x]  # Number of repetitions (length of the run)
      )
    })
  )
  # Returning the reduced dataframe with clusters
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
# I do not get the following step. Why splitting again based on on_off_season,
# since this column only contains on-season values!?
kmeans_byseason <- split(
  kmeans_reduced,
  list(
    kmeans_reduced$on_off_season
  )
)

# Prepare some empty variables for statistics calculation
perc <- list()
cluster_means <- list()
cluster_means_sd <- list()
all_clusters <- list()
all_clusters_sd <- list()

# Loop to calculate statistics
for (i in seq_along(kmeans_byseason)) {
  # Split the data by clusters for the on-season (I do not get why, as
  # kmeans_byseason is already subsetted to on-season)
  perc[[i]] <- split(
    kmeans_byseason[[i]],
    kmeans_byseason[[i]]$cluster
  )
  # Calculate relative occurrence (proportion) of each cluster
  perc[[i]] <- lapply(
    perc[[i]],
    function(x) nrow(x) / nrow(kmeans_byseason[[i]]) # Calculate rel. frequency
  )
  # Combine the relative occurrences into a matrix
  perc[[i]] <- do.call(rbind, perc[[i]])

  # Calculate the average values (mean) for each cluster, including length
  # and unscaled variables
  cluster_means[[i]] <- aggregate(
    kmeans_byseason[[i]][
      ,
      c(cluster_var, "length", paste("unscaled", cluster_var, sep = "."))
    ],
    by = list(cluster = kmeans_byseason[[i]]$cluster), mean
  )

  # Calculate the standard deviation for each cluster, including length
  # and unscaled variables
  cluster_means_sd[[i]] <- aggregate(
    kmeans_byseason[[i]][
      ,
      c(cluster_var, "length", paste("unscaled", cluster_var, sep = "."))
    ],
    by = list(cluster = kmeans_byseason[[i]]$cluster), sd
  )
  # Add the relative occurrence (percentage) to the cluster means dataframe
  cluster_means[[i]]$perc <- perc[[i]][, 1]
  # Add the season information to the cluster means dataframe
  cluster_means[[i]]$season <- rep(
    first(kmeans_byseason[[i]]$on_off_season),
    nrow(cluster_means[[i]])
  )
  # Label the clusters with "Cluster" prefix
  cluster_means[[i]]$cluster <-
    paste("Cluster", cluster_means[[i]]$cluster)

  # Set the percentage for "All clusters" to NA
  all_clusters[[i]] <- c(
    cluster = "All clusters",
    colMeans(cluster_means[[i]][
      ,
      c(cluster_var, "length", paste("unscaled", cluster_var, sep = "."))
    ]),
    perc = NA,
    season = first(cluster_means[[i]]$season)
  )
  # Set the sd for "All clusters" to NA
  all_clusters_sd[[i]] <- c(
    cluster = "All clusters",
    sapply(cluster_means[[i]][
      ,
      c(cluster_var, "length", paste("unscaled", cluster_var, sep = "."))
    ], sd),
    perc = NA,
    season = first(cluster_means[[i]]$season)
  )
  # Combine the statistics of each cluster with those of "All clusters"
  cluster_means[[i]] <- as.data.frame(rbind(
    cluster_means[[i]],
    all_clusters[[i]]
  ))

  cluster_means_sd[[i]] <- as.data.frame(rbind(
    cluster_means_sd[[i]],
    all_clusters_sd[[i]]
  ))
}
# Saving calculated variables (means, sd) into dataframes
cluster_means <- as.data.frame(do.call(rbind, cluster_means))
cluster_means_sd <- as.data.frame(do.call(rbind, cluster_means_sd))
# Print output, if needed
view(cluster_means)
view(cluster_means_sd)

# Combine all variables into one df for plotting
# Here, we combine all calculated and unscaled values for each cluster.
# The df lists each meteorological variable associated to each cluster one after another
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
# Print output, if needed
view(pinput_cluster_means)
# I'm not entirely sure why we create the following variable!?
length_labels <- aggregate(pinput_cluster_means,
  by = list(
    pinput_cluster_means$cluster,
    pinput_cluster_means$season
  ),
  first
)
#View output, if needed
view(length_labels)

# Rename / add units of meteorological variables
x <- pinput_cluster_means$var
old <- cluster_var
new <- c(
  "air pressure [hPa]",
  "precipitation [mm]",
  "relative humidity [%]",
  "radiation [W/m²]",
  "temperature [°C]",
  "inversion ratio [K]",
  "wind direction (east/west)",
  "wind direction (north/south)",
  "maximum wind speed [m/s]",
  "average wind speed [m/s]"
)

# Replace actual names in pinput_cluster_means
x[x %in% old] <- new[match(x, old, nomatch = 0)]
pinput_cluster_means$var <- x

# Plot result, if needed
view(pinput_cluster_means)

# Plot clusters
# p_cluster is showing all desired meteorological variables per cluster,
# and the related retention time
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
graphics.off()
dev.off()

# p_cluster_transformed is basically the equivalent to the above one,
# but in consequtive rows
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

# Exclude "All clusters" for relative occurrence
clusters_only <- pinput_cluster_means[pinput_cluster_means$cluster != "All clusters", ]
# Plot output, if needed
view(clusters_only)

# Add the relative occurrence of clusters as annotations
relative_occurrences <- as.data.frame(matrix(unlist(perc),
 nrow=n_cluster, byrow=TRUE))
colnames(relative_occurrences) <- c("Relative_Occurrence")
relative_occurrences$percentage <- relative_occurrences$Relative_Occurrence*100
# Plot result, if needed
view(relative_occurrences)

# Ensure the relative_occurrences data frame has relevant cluster information
relative_occurrences$cluster <- unique(clusters_only$cluster)

p_cluster_transformed <- p_cluster_transformed +
  geom_text(data = relative_occurrences, 
            aes(x = -1, y = -1,  # Adjust these to place the text where you want
                label = sprintf("%.2f%%", percentage)), 
            hjust = -0.1, 
            vjust = 1.5, 
            size = 3, 
            color = "black")

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

# Prepare boxplots
pinput_boxplot <- data.frame(
  # Get elevation levels
  elevation = rep(elevation, nrow(kmeans_reduced)),
  # Get PM2.5 values per site (site refers to constant)
  PM2.5 = unlist(kmeans_reduced[, site]),
  # Paste the string "cluster" to the cluster number
  cluster = paste("Cluster", rep(kmeans_reduced$cluster, length(site))),
  # Paste down the on-season string
  season = rep(kmeans_reduced$on_off_season, length(site))
)

# "pinput_boxplot_all" contains everything needed again for all clusters taken
# together
pinput_boxplot_all <- pinput_boxplot
pinput_boxplot_all$cluster <- rep("All clusters", nrow(pinput_boxplot))
pinput_boxplot <- as.data.frame(rbind(pinput_boxplot, pinput_boxplot_all))
# Plot output if needed
view(pinput_boxplot_all)

# Code for the actual boxplot
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
# Prepare basic dataframe containing both scaled and unscaled meteorological
# variables and other constant variables
input_lagged <- do.call("rbind", replicate(
  length(site),     # Replicate the kmeans_reduced dataframe for each site
  kmeans_reduced,   # Data to replicate
  simplify = FALSE
))
# Adjust PM2.5, model and site
# Set PM2.5 column using site data from kmeans_reduced
input_lagged$PM2.5 <- unlist(kmeans_reduced[, site]) 
# Set model column using site-specific model data
input_lagged$model <- unlist(kmeans_reduced[, paste0("model", site)]) 
# Set site column using elevation, repeated for each row
input_lagged$site <- rep(elevation, each = nrow(kmeans_reduced))
# Print output, if needed
view(input_lagged)
# input_lagged contains scaled and unscaled meteorological variables and other
# constants, which are used as an input to the following lead/lag correlation
# The df still contains the data for each site

# Split the dataframe into a list of dataframes, one for each site
input_lagged <- split(input_lagged, input_lagged$site)

# Create empty lists for the following lead calculations
lead_pm <- list()
lead_model <- list()
input_pm <- list()
input_model <- list()

# Loop over each site's data in input_lagged
for (i in seq_along(input_lagged)) {
  # Scale PM2.5, model and ships for the current site
  input_lagged[[i]]$PM2.5_scaled <- scale(input_lagged[[i]]$PM2.5)
  input_lagged[[i]]$model_scaled <- scale(input_lagged[[i]]$model)
  input_lagged[[i]]$ships_scaled <- scale(input_lagged[[i]]$ships)

  # Create dataframes of lead values for PM2.5
  lead_pm[[i]] <- as.data.frame(sapply(
    c(1:n_lag_days),
    function(x) lead(input_lagged[[i]]$PM2.5_scaled, n = x) # Lead function
    # shifting PM2.5 data
  ))
  # Create dataframe of lead values for model data
  lead_model[[i]] <- as.data.frame(sapply(
    c(1:n_lag_days), # Range of lead days
    function(x) lead(input_lagged[[i]]$model_scaled, n = x) # Lead function
    # shifting PM2.5 data
  ))
  # Combine the original and lead data for PM2.5 and model
  input_pm[[i]] <- as.data.frame(cbind(
    input_lagged[[i]], # Original data
    lead_pm[[i]]       # Lead PM2.5 data
  ))
  input_model[[i]] <- as.data.frame(cbind(
    input_lagged[[i]], # Original data
    lead_model[[i]]    # Lead model data
  ))
}

#Print output, if needed
view(input_lagged)

# Explanations
# lead_pm contains lead values for scaled PM2.5 data, shifted by the
# corresponding lag_days consequtively (columns). The values are
# shifted in both row and column (e.g. V2 corresponds to row 2, V3 to row 3...)
# The different sites are listed column-wise, i.e, V1-V100 refers to 2 m a.s.l.,
# V1.1-100.1 to 420 m a.s.l., V.1.2-100.2 to 90 m a.s.l.
view(lead_pm)

# lead_model contains lead values for scaled modeled PM2.5 data, shifted in
# the same way as lead_pm
# Print output, if needed
view(lead_model)

# input_pm contains the scaled and unscaled datasets combined with the lead_pm
# values for each site (column-wise)
view(input_pm)

# input_model contains the scaled and unscaled datasets combined with the
# lead_model values for each site (column-wise)
view (input_model)

# Combine the list of dataframes into single dataframes
# I do not fully understand how this works, but here we basically restructure
# the input_pm and input_model dataframes, i.e., sites are listed in
# consequtive rows (instead of columns as before)
input_pm <- as.data.frame(do.call(rbind, input_pm))
input_model <- as.data.frame(do.call(rbind, input_model))

# Add a cluster label "All clusters" to the combined PM2.5 data
input_pm_all <- input_pm
input_pm_all$cluster <- rep("All clusters", nrow(input_pm_all))

# Combine the PM2.5 data with the "All clusters" label
# Here, we create a dataframe that contains scaled and unscaled data for each
# site after another (row-wise) referring to the associated cluster. This
# results in 2100 rows. After that, the blocks are repeated, but this time for
# the label "All_clusters"
input_pm <- as.data.frame(rbind(input_pm, input_pm_all))
# Print output, if needed
view(input_pm)

# Split the PM2.5 data by site and cluster
input_pm <- split(input_pm, list(input_pm$site, input_pm$cluster))

# The following procedure is simultaneous to the previous steps, but
# now for input_model instead
# Add a cluster label "All clusters" to the combined model data
input_model_all <- input_model
input_model_all$cluster <- rep("All clusters", nrow(input_model_all))

# Combine the model data with the "All clusters" label
input_model <- as.data.frame(rbind(input_model, input_model_all))

# Split the model data by site and cluster
input_model <- split(input_model, list(input_model$site, input_model$cluster))

# Create an empty list for storing correlation results
cor_lead_pm <- list()

# Loop over each site's data in input_pm
# Here, we actually run the correlation estimation between (scaled) hours
# ships spent at port, and shifted PM2.5 values per lead
for (i in seq_along(input_pm)) {
  # Calculate correlations between ships_scaled and lead PM2.5
  # for the current site
  cor_lead_pm[[i]] <- as.data.frame(rbind(
    # Correlations for measured PM2.5
    t(sapply(
      input_pm[[i]][, paste0("V", c(1:n_lag_days))], # columns representing
      # lead values
      function(x) correlate(input_pm[[i]]$ships_scaled, x) # correlate with
      # ships_scaled
    )),
    # Correlations for modeled PM2.5 in similar way as above
    t(sapply(
      input_model[[i]][, paste0("V", c(1:n_lag_days))],
      function(x) correlate(input_model[[i]]$ships_scaled, x)
    ))
  ))

  # Add lead days information to dataframe
  cor_lead_pm[[i]]$lead <- rep(c(1:n_lag_days), 2)

  # Add method information (measured, modeled) to the dataframe
  cor_lead_pm[[i]]$method <- rep(c("measured", "modeled"), each = n_lag_days)
}

# Combine all site-specific correlation dataframes into a single df
cor_lead_pm <- as.data.frame(do.call(rbind, cor_lead_pm))
# Print output, if needed
view(cor_lead_pm)

# Set column names for the combined df
colnames(cor_lead_pm) <- c("cor", "p", "ci_5", "ci_95", "lead", "method")

# Create an ID for each row based on the site and cluster information
# I do not fully understand the logic here, but it is clear what happens.
# We want to add the site information to the cor_lead_pm df. Apparently,
# this is possible by retrieving the names of input_pm. Each element in
# input_pm corresponds to one site and contains a df with the original
# data and the lead data for PM2.5 and modeled values, respectively.
# It seems that the visualization here in VS Code does not show this type
# of nested dataframes (?)

cor_lead_pm$id <- rep(names(input_pm), each = n_lag_days * 2)

# Extract cluster information from the ID and create a new column
cor_lead_pm$cluster <- paste(
  "Cluster",
  sapply(strsplit(cor_lead_pm$id, "[.]"), "[[", 5) # This removes the odd
  # original strings which were retrieved from the names-function
)

# Replace "Cluster All clusters" with "All clusters" in the cluster column
cor_lead_pm$cluster <- replace(
  cor_lead_pm$cluster,
  cor_lead_pm$cluster == "Cluster All clusters", "All clusters"
)

# Extract site information from the ID and create a new column
cor_lead_pm$site <- paste(sapply(strsplit(cor_lead_pm$id, "[.]"), "[[", 1),
  sapply(strsplit(cor_lead_pm$id, "[.]"), "[[", 2),
  sapply(strsplit(cor_lead_pm$id, "[.]"), "[[", 3),
  sep = "."
)

# Mark correlations with p-values greater than 0.1 as not significant (NA)
cor_lead_pm$sig <- replace(cor_lead_pm$cor, cor_lead_pm$p > 0.1, NA)

# Append a period to each site name in the site column
cor_lead_pm$site <- paste0(cor_lead_pm$site, ".")

# Replace "model" in the site column for rows where the method is "modeled"
cor_lead_pm$site <- replace(
  cor_lead_pm$site,
  cor_lead_pm$method == "modeled",
  "model"
)
# Print unique site names, if needed
unique(cor_lead_pm$site)

# Convert the site column to a factor with specified levels
# site is NA, if method = measured
cor_lead_pm$site <- factor(cor_lead_pm$site,
  levels = c(elevation, "model")
)
# Print output, if needed
view(cor_lead_pm)

# Remove model results, if wanted
cor_lead_pm <- cor_lead_pm %>% filter(method == "measured")

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

ggarrange(plotlist = list(
    p_cluster_transformed,
    p_boxplot,
    p_lagged
  ), widths = c(2.5, 2.5, 4),
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
  scale_x_continuous(expand = c(0, 0), limits = c(0, 200)) +
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

cairo_pdf("correlation_on_season_200_days.pdf", width = 10, height = 4, pointsize = 10)
p_lagged_all
dev.off()
