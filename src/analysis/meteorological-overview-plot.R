## overview plot for meteorological data
# monthly
input_bysite <- split(input_restructured, input_restructured$site)

# restructure input
df_meteo <- list()
for (i in seq_along(input_bysite)) {
  df_meteo[[i]] <- input_bysite[[i]][, meteo_var]
  df_meteo[[i]] <- data.frame(
    value = unlist(df_meteo[[i]]),
    var = rep(colnames(df_meteo[[i]]),
      each = nrow(input_bysite[[i]])
    ),
    elev = rep(meteo_elevation,
      each = nrow(input_bysite[[i]])
    ),
    site = rep(
      first(input_bysite[[i]]$site),
      (nrow(input_bysite[[i]]) * length(meteo_var))
    ),
    day = rep(input_bysite[[i]]$day, length(meteo_var)),
    date = rep(input_bysite[[i]]$date, length(meteo_var)),
    daynight = rep(input_bysite[[i]]$day_night, length(meteo_var)),
    month = rep(input_bysite[[i]]$month, length(meteo_var)),
    year = rep(input_bysite[[i]]$year, length(meteo_var)),
    hour = rep(input_bysite[[i]]$hour, length(meteo_var))
  )
}
df_meteo <- as.data.frame(do.call(rbind, df_meteo))
df_meteo$group <- paste(df_meteo$var, df_meteo$elev, df_meteo$month)

# aggregate data (by month)
var <- aggregate(df_meteo,
  by = list(month = df_meteo$month, group = df_meteo$group),
  function(x) first(x)
)$var
elev <- aggregate(df_meteo,
  by = list(month = df_meteo$month, group = df_meteo$group),
  function(x) first(x)
)$elev
se <- aggregate(df_meteo,
  by = list(month = df_meteo$month, group = df_meteo$group),
  function(x) sd(x, na.rm = TRUE) / sqrt(length(x))
)$value

df_meteo_monthly <- aggregate(df_meteo,
  by = list(month = df_meteo$month, group = df_meteo$group),
  function(x) mean(x, na.rm = TRUE)
)
df_meteo_monthly <- df_meteo_monthly[, 1:3]
df_meteo_monthlyt$se <- se
df_meteo_monthly$var <- var
df_meteo_monthly$elev <- elev

df_meteo_monthly$month <- factor(df_meteo_monthly$month, levels = months)
df_meteo_monthly$elev <- factor(df_meteo_monthly$elev, levels = levels_elev)

p_meteo_monthly <- ggplot(df_meteo_monthly) +
  geom_pointrange(aes(
    x = month, y = value, ymin = value - se, ymax = value + se,
    group = paste(month, var), col = elev
  ), size = .2) +
  theme_light() +
  scale_color_manual(values = palette_meteo) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "Environmental variable", col = "elevation") +
  facet_wrap(~var, scale = "free_y", ncol = 1)
p_meteo_monthly

cairo_pdf("meteorological_variables_monthly.pdf",
  width = 6,
  height = 15,
  pointsize = 10
)
p_meteo_monthly
dev.off()

# daily
# restructure input
df_meteo$doy <- yday(df_meteo$date)
df_meteo$group <- paste(
  df_meteo$var,
  df_meteo$elev,
  df_meteo$doy,
  df_meteo$daynight
)

# aggregate data (by DOY)
var <- aggregate(df_meteo,
  by = list(
    doy = df_meteo$doy,
    daynight = df_meteo$daynight,
    group = df_meteo$group
  ),
  function(x) first(na.omit(x))
)$var
elev <- aggregate(df_meteo,
  by = list(
    doy = df_meteo$doy,
    daynight = df_meteo$daynight,
    group = df_meteo$group
  ),
  function(x) first(x)
)$elev
se <- aggregate(df_meteo,
  by = list(
    doy = df_meteo$doy,
    daynight = df_meteo$daynight,
    group = df_meteo$group
  ),
  function(x) sd(x, na.rm = TRUE) / sqrt(length(x))
)$value

df_meteo_daily <- aggregate(df_meteo,
  by = list(
    doy = df_meteo$doy,
    daynight = df_meteo$daynight,
    group = df_meteo$group
  ),
  function(x) mean(x, na.rm = TRUE)
)
df_meteo_daily <- df_meteo_daily[, 1:4]
df_meteo_daily$se <- se
df_meteo_daily$var <- var
df_meteo_daily$elev <- elev
df_meteo_daily$elev <- factor(df_meteo_daily$elev, levels = levels_elev)

# relabel
x <- df_meteo_daily$var
old <- meteo_var
new <- meteo_labels
x[x %in% old] <- new[match(x, old, nomatch = 0)]
df_meteo_daily$var <- x

p_meteo_daily <- ggplot(df_meteo_daily) +
  geom_ribbon(aes(
    x = doy, y = value, ymin = value - se, ymax = value + se,
    group = paste(doy, var, elev, daynight), fill = elev
  ), alpha = .2) +
  geom_line(
    aes(
      x = doy,
      y = value,
      group = paste(var, elev, daynight),
      linetype = daynight,
      col = elev
    ),
    size = .3
  ) +
  theme_light() +
  scale_color_manual(values = palette_meteo) +
  scale_fill_manual(values = palette_meteo) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(
    linetype = "",
    x = "Day of year",
    y = "Environmental variable",
    col = "elevation",
    fill = "elevation"
  ) +
  facet_wrap(~var, scale = "free_y", ncol = 1)
p_meteo_daily

cairo_pdf("meteorological_variables_daily.pdf",
  width = 7,
  height = 13,
  pointsize = 10
)
p_meteo_daily
dev.off()
