## moving window correlation
max_width <- 120  # set maximum window width
sig_niveau <- 0.1

# restructure input
input_movingwindow <- data.frame(
  PM2.5 = input_daily$PM2.5,
  port = input_daily$all_ships,
  var = rep(c("All"), each = nrow(input_daily)),
  site = input_daily$site,
  date = input_daily$date,
  doy = yday(input_daily$date),
  week = strftime(input_daily$date, format = "%V"),
  month = input_daily$month,
  year = input_daily$year
)

# get weekly sums
input_week <- split(
  input_movingwindow,
  list(
    input_movingwindow$var,
    input_movingwindow$site,
    input_movingwindow$week
  )
)
input_week <- lapply(input_week, function(x) {
  data.frame(
    PM2.5 = mean(x$PM2.5, na.rm = TRUE),
    # TODO: check if means are correct here (before: sum)
    port = sum(x$port, na.rm = TRUE),
    site = first(x$site),
    var = first(x$var),
    week = first(x$week)
  )
})
input_week <- as.data.frame(do.call(rbind, input_week))

# get on-season only
input_movingwindow_on <- subset(
  input_movingwindow,
  input_movingwindow$week > on_season_start &
    input_movingwindow$week < on_season_end
)

input_year_on <- split(
  input_movingwindow_on,
  list(
    input_movingwindow_on$site,
    input_movingwindow_on$var,
    input_movingwindow_on$year
  )
)
input_year <- split(
  input_movingwindow,
  list(
    input_movingwindow$site,
    input_movingwindow$var,
    input_movingwindow$year
  )
)

for (i in seq_along(input_year)) {
  input_year[[i]] <- input_year[[i]][order(input_year[[i]]$date), ]

  input_year[[i]] <- get_window_sums(
    input_year[[i]],
    input_year[[i]]$port,
    max_width,
    "right"
  )
}
input_year <- as.data.frame(do.call(rbind, input_year))
colSums(input_year[, c(1:max_width)], na.rm = TRUE)

input_week_sums <- split(
  input_year,
  list(
    input_year$var,
    input_year$site,
    input_year$week
  )
)
input_week_sums <- lapply(input_week_sums, function(x) {
  result <- list()
  for (i in c(1:max_width)) {
    result[[i]] <- data.frame(
      values = sum(x[, paste0("V", i)], na.rm = TRUE)
    )
    colnames(result[[i]])[1] <- paste0("values", i)
  }
  result <- do.call(cbind, result)
  result$port <- sum(x$port, na.rm = TRUE)
  result$site <- first(x$site)
  result$var <- first(x$var)
  result$week <- first(x$week)
  result
})
input_week_sums <- as.data.frame(do.call(rbind, input_week_sums))

input_year_on <- subset(
  input_year,
  input_year$week > on_season_start &
    input_year$week < on_season_end
)
input_year_on <- split(
  input_year_on,
  list(
    input_year_on$site,
    input_year_on$var,
    input_year_on$week
  )
)

# calculate correlations
corr <- list()
input_corr <- list()
dup <- list()
for (i in seq_along(input_year_on)) {
  dup[[i]] <- duplicated(colnames(input_year_on[[i]]))
  input_corr[[i]] <- input_year_on[[i]][c(1:max_width)]
  corr[[i]] <- sapply(
    input_corr[[i]],
    function(y) correlate(y, input_year_on[[i]]$PM2.5)
  )
}
length(corr)
length(input_year_on)

corr_result <- data.frame(
  cor = unlist(lapply(corr, function(x) x[1, ])),
  p = unlist(lapply(corr, function(x) x[2, ])),
  ci_5 = unlist(lapply(corr, function(x) x[3, ])),
  ci_95 = unlist(lapply(corr, function(x) x[4, ])),
  id = rep(names(input_year_on), each = max_width),
  width = rep(c(1:max_width), length(corr))
)
corr_result$site <- sapply(strsplit(corr_result$id, "[.]"), "[[", 1)
corr_result$var <- sapply(strsplit(corr_result$id, "[.]"), "[[", 2)
corr_result$week <- as.numeric(sapply(strsplit(corr_result$id, "[.]"), "[[", 3))

input_week <- subset(
  input_week,
  input_week$week > on_season_start &
    input_week$week < on_season_end
)

input_week_sums <- subset(
  input_week_sums,
  input_week_sums$week > on_season_start &
    input_week_sums$week < on_season_end
)

pinput_week <- data.frame(
  y = c(
    input_week_sums$values120 / 1000,
    input_week_sums$values100 / 1000,
    input_week_sums$values50 / 1000,
    input_week_sums$values10 / 1000,
    input_week$PM2.5 * 10,
    input_week$port / 10
  ),
  week = c(
    rep(input_week_sums$site, 4),
    rep(input_week$week, 2)
  ),
  site = c(
    rep(input_week_sums$site, 4),
    rep(input_week$site, 2)
  ),
  var = c(input_week_sums$var, rep(input_week$var, 2)),
  var_points = rep(c(
    "Time ships spent in port (sum over past 120 days) [100 hours]",
    "Time ships spent in port (sum over past 100 days) [100 hours]",
    "Time ships spent in port (sum over past 50 days) [100 hours]",
    "Time ships spent in port (sum over past 10 days) [100 hours]",
    "PM2.5 concentration",
    "Time ships spent in port [hours]"
  ), each = nrow(input_week))
)

corr_result$sig <- replace(
  corr_result$cor,
  corr_result$p > sig_niveau,
  NA
)
corr_result <- subset(
  corr_result,
  corr_result$var == "All"
)
pinput_week <- subset(
  pinput_week,
  pinput_week$var == "All"
)
include <- c(
  "PM2.5 concentration",
  "Time ships spent in port [hours]"
)
pinput_week <- pinput_week[pinput_week$var_points %in% include, ]

rename <- corr_result$site
rename[rename %in% site] <- elevation[match(rename, site, nomatch = 0)]
corr_result$elevation <- rename

rename <- pinput_week$site
rename[rename %in% site] <- elevation[match(rename, site, nomatch = 0)]
pinput_week$elevation <- rename

rename <- input_week_sums$site
rename[rename %in% site] <- elevation[match(rename, site, nomatch = 0)]
input_week_sums$elevation <- rename

# Plot
max_width_plot <- 100

breaks_on_season <- c(
  2.21,
  6.43,
  10.64,
  15,
  19.36,
  23.71,
  28.07,
  32.5,
  36.86,
  41.21,
  45.57,
  49.93
)

corr_result$elevation <- factor(corr_result$elevation, levels = elevation)

p_moving_window <- ggplot(
  subset(
    corr_result,
    corr_result$width <= max_width_plot
  )
) +
  geom_tile(aes(x = as.numeric(week), y = width, fill = cor), alpha = 0.4) +
  geom_tile(aes(x = as.numeric(week), y = width, fill = sig)) +
  geom_point(
    data = pinput_week,
    aes(x = as.numeric(week), y = y / 1.5, group = var_points, col = var_points)
  ) +
  facet_wrap(~elevation, ncol = 1) +
  theme_light() +
  scale_y_continuous(
    expand = c(0, 0),
    sec.axis = sec_axis(
      trans = ~ (.x * 10) * 1.5,
      name = expression(
        "Measured PM"[2.5] *
          " concentration [µg/100m³]/Sum of time spent at port [hours]"
      )
    )
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = breaks_on_season,
    labels = months
  ) +
  labs(
    x = "",
    color = "",
    fill = "Pearson's correlation coefficient",
    y = "Window width [days]"
  ) +
  scale_fill_gradient2(
    midpoint = 0,
    low = palette_moving_window[1],
    mid = palette_moving_window[2],
    high = palette_moving_window[3],
    na.value = "transparent"
  ) +
  scale_color_manual(
    values = c("black", "red"),
    labels = c(
      expression("PM"[2.5] * " concentration"),
      expression("Time ships spent at port")
    )
  ) +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    legend.justification = "bottom"
  )
p_moving_window

cairo_pdf("moving_window_correlation_new.pdf",
  width = 7,
  height = 10,
  pointsize = 12
)
p_moving_window
dev.off()
