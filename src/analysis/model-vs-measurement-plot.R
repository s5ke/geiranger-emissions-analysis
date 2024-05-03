# plot comparison: model vs measurements
# restructure input
scale_factor <- 300
input_model <- data.frame(
  value = c(
    input_daily$PM2.5,
    (subset(
      input_daily,
      input_daily$site == site[1]
    )$model * scale_factor)
  ),
  date = c(
    input_daily$date,
    subset(
      input_daily,
      input_daily$site == site[1]
    )$date
  ),
  var = c(
    input_daily$site,
    rep("model", nrow(subset(
      input_daily,
      input_daily$site == site[1]
    )))
  )
)

p <- ggplot(input_model) +
  geom_vline(xintercept = brks, col = "grey", size = .7) +
  geom_point(
    aes(
      x = date,
      y = value,
      group = var,
      col = as.factor(var)
    ),
    alpha = .7,
    shape = 1
  ) +
  scale_x_continuous(expand = c(0, 0), breaks = x_breaks, labels = x_labels) +
  scale_y_continuous(expand = c(0, 0), sec.axis = sec_axis(
    trans = ~ .x / scale_factor,
    name = "Modelled PM2.5 concentration [µg/m³]"
  )) +
  labs(y = "Measured PM2.5 concentration [µg/m³]", x = "", col = "") +
  scale_color_manual(values = c(palette_elevation, "black")) +
  theme_light()
p

cairo_pdf("model_vs_data_point.pdf", width = 12, height = 4, pointsize = 10)
p
dev.off()


p <- ggplot(input_model) +
  geom_vline(xintercept = brks, col = "grey", size = .7) +
  geom_line(
    aes(
      x = date,
      y = value,
      group = var,
      col = as.factor(var)
    ),
    alpha = .7
  ) +
  scale_color_manual(values = c(palette_elevation, "black")) +
  scale_x_continuous(expand = c(0, 0), breaks = x_breaks, labels = x_labels) +
  scale_y_continuous(expand = c(0, 0), sec.axis = sec_axis(
    trans = ~ .x / 300,
    name = "Modelled PM2.5 concentration [µg/m³]"
  )) +
  labs(y = "Measured PM2.5 concentration [µg/m³]", x = "", col = "") +
  theme_light()
p

cairo_pdf("model_vs_data_line.pdf", width = 12, height = 4, pointsize = 10)
p
dev.off()
