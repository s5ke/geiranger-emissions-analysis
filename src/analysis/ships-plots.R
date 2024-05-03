## plot time spent in port by ship
# restructure data
var <- c("Ferry", "Hurtigruten_dummy", "all_ships", "cruise_ships")
pinput <- data.frame(
  port = unlist(input_daily[, var]),
  date = rep(input_daily$date, length(var)),
  group = rep(var, each = nrow(input_daily))
)
pinput$port <- replace(pinput$port, pinput$port == 0, NA)

# rename labels
group <- pinput$group
old <- var
new <- ships_labels
group[group %in% old] <- new[match(group, old, nomatch = 0)]
pinput$group <- group

# Plots
# Barplot
p_ships_bar <- ggplot(
  subset(
    pinput,
    pinput$group != "All" & pinput$group != "Hurtigruten dummy"
  )
) +
  geom_col(
    aes(
      x = date, y = port,
      group = group, fill = group, col = group
    ),
    position = position_stack()
  ) +
  theme_light() +
   scale_x_continuous(expand = c(0, 0), breaks = x_breaks, labels = x_labels) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = palette_ships) +
  scale_fill_manual(values = palette_ships) +
  labs(x = "", y = "Hours in port (sum)", fill = "", col = "") +
  theme(
    panel.grid.major.x = element_line(color = "grey", linewidth = 1)
  )
p_ships_bar

cairo_pdf("ships_overview.pdf", width = 20, height = 4, pointsize = 10)
p_ships_bar
dev.off()

# Points
p_ships_point <- ggplot(
  subset(
    pinput,
    pinput$group != "All" &
      pinput$group != "Hurtigruten dummy"
  )
) +
  geom_point(aes(
    x = date, y = port,
    group = group, fill = group, col = group
  ), shape = 1) +
  theme_light() +
  scale_x_date(expand = c(0, 0), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = palette_ships) +
  scale_fill_manual(values = palette_ships) +
  labs(x = "", y = "Hours in port (sum)", fill = "", col = "") +
  theme(
    axis.text.x = element_text(hjust = -6),
    panel.grid.major.x = element_line(color = "grey", linewidth = 1)
  )
p_ships_point

cairo_pdf("ships_overview2.pdf", width = 20, height = 4, pointsize = 10)
p_ships_point
dev.off()
