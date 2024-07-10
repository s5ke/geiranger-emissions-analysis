# Lagged correlations on hourly basis

n_lag_hours <- 2880

# Import input_hourly, if not present already
input_hourly <- read.csv("input_hourly.csv")

# Prepare hourly_input data
input_hourly_split <- split(input_hourly, input_hourly$site)
input_hourly_lag <- bind_rows(input_hourly_split)
input_hourly_lag <- data.frame(
    datetime = (input_hourly_lag$datetime),
    date = as.POSIXct(input_hourly_lag$datetime),
    year = as.integer(input_hourly_lag$year),
    month = as.character(input_hourly_lag$month),
    week = strftime(input_hourly_lag$datetime, format = "%V"),
    doy = as.integer(yday(as.POSIXct(input_hourly_lag$datetime))),
    hour = as.integer(input_hourly_lag$hour),
    site = as.factor(input_hourly_lag$site),
    elevation = as.character(input_hourly_lag$elevation),
    PM2.5 = as.numeric(input_hourly_lag$PM2.5),
    ships = as.numeric(input_hourly_lag$all_ships)
    )
view(input_hourly_lag)

# Subset on-season
input_hourly_lag_on <- subset(input_hourly_lag, week >=on_season_start &
week <= on_season_end)
view(input_hourly_lag_on)

# Split df into list of dfs, one for each site
input_hourly_lag_on <- split(input_hourly_lag_on,
input_hourly_lag_on$elevation)

# Create empty lists for the following lead calculation
lead_pm <-list()
input_pm <- list()

# Loop over each site's data
for(i in seq_along(input_hourly_lag_on)) {
  input_hourly_lag_on[[i]]$PM2.5_scaled <- scale(input_hourly_lag_on[[i]]$PM2.5)
  input_hourly_lag_on[[i]]$ships_scaled <- scale(input_hourly_lag_on[[i]]$ships)

    # Create dataframes of lead values for PM2.5
  lead_pm[[i]] <- as.data.frame(sapply(
    c(1:n_lag_hours),
    function(x) lead(input_hourly_lag_on[[i]]$PM2.5_scaled, n = x) # Lead function
    # shifting PM2.5 data
  ))
    # Combine the original and lead data for PM2.5 and model
  input_pm[[i]] <- as.data.frame(cbind(
    input_hourly_lag_on[[i]], # Original data
    setNames(lead_pm[[i]], paste0("V", 1:n_lag_hours)) # Lead PM2.5 data
  ))
  }
# Print output, if needed
view(input_pm)
view(lead_pm)

# Combine the lists of dfs into single dfs
input_pm <- as.data.frame(do.call(rbind, input_pm))
input_pm <- split(input_pm, input_pm$elevation)

# Create an empty list for storing correlation results
cor_lead_pm <- list()

# Loop over each site's data in input_pm and calculate correlations
for (i in seq_along(input_pm)) {
  # Calculate correlations between ships_scaled and lead PM2.5
  # for the current site
  cor_lead_pm[[i]] <- as.data.frame(rbind(
    # Correlations for measured PM2.5
    t(sapply(
      input_pm[[i]][, paste0("V", c(1:n_lag_hours))], # columns representing
      # lead values
      function(x) correlate(input_pm[[i]]$ships_scaled, x) # correlate with
      # ships_scaled
    ))
  ))
  
  cor_lead_pm[[i]]$lead <- 1:n_lag_hours
}

# Combine all site-specific correlation dataframes into a single df
cor_lead_pm <- as.data.frame(do.call(rbind, cor_lead_pm))
# Print output, if needed
view(cor_lead_pm)

# Set column names for the combined df
colnames(cor_lead_pm) <- c("cor", "p", "ci_5", "ci_95", "lead")

cor_lead_pm$id <- rep(names(input_pm), each = n_lag_hours)
view(cor_lead_pm)

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

# Print unique site names, if needed
unique(cor_lead_pm$site)

cor_lead_pm$site <- factor(cor_lead_pm$site,
  levels = c(elevation, "model")
)
# Print output, if needed
view(cor_lead_pm)

#Plot
p_lagged_all <- ggplot(cor_lead_pm) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(
    x = lead,
    y = cor,
    group = paste(site),
    col = site
  ), size = 1, alpha = .3) +
  geom_point(aes(
    x = lead,
    y = sig,
    group = paste(site),
    col = site
  ), size = 1) +
  geom_line(aes(
    x = lead,
    y = cor,
    group = paste(site),
    col = site
  ), size = .3, alpha = .3) +
  geom_line(aes(
    x = lead,
    y = sig,
    group = paste(site),
    col = site
  ), size = .3) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2880)) +
  scale_color_manual(values = c(palette_elevation, "black")) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    y = "Correlation coefficient (Pearson)",
    x = "Time (lead) [hours]",
    col = "elevation"
  )
p_lagged_all

cairo_pdf("correlation_on_season_168_hours.pdf", width = 10, height = 4, pointsize = 10)
p_lagged_all
dev.off()