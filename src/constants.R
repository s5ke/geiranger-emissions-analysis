# constants needed in all further analysis

# Color palettes
main_palette <- c("#00204d", "#7c7b78", "#ffea46", "darkred")
palette_light <- rgb(col2rgb(main_palette)[1, ], col2rgb(main_palette)[2, ], col2rgb(main_palette)[3, ], max = 255, alpha = 150)

palette_elevation <- c("#00204d", "#7c7b78", "#ffea46")

palette2 <- c("black", "#00204d", "#52648d", "#7c7b78", "#cbba69", "#ffea46")



# Day and Night hours for Norway (Geiranger)
# 0 = night, 1 = twilight, 2 = day
day_night <- list(
    jan = c(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0), # 10-16, twilight: 7-19
    feb = c(0, 0, 0, 0, 0, 0, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 0, 0, 0, 0, 0), # 8-17, twilight: 6-20
    mar = c(0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 0, 0, 0, 0), # 7-18, twilight: 4-21
    apr = c(0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1), # 6-21, twilight: 2-1
    may = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1), # 5-22, twilight: -
    jun = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1), # 4-23, twilight: -
    jul = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1), # 4-23, twilight: -
    aug = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1), # 6-21, twilight: -
    sep = c(0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 0, 0), # 7-20, twilight: 4-23
    oct = c(0, 0, 0, 0, 0, 0, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 0, 0, 0, 0), # 8-18, twilight: 6-21
    nov = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0), # 9-16, twilight: 6-18
    dec = c(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)  # 10-15, twilight: 7-18
)
