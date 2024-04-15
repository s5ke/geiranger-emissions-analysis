# constants needed in all further analysis

# Color palettes
main_palette <- c("#00204d", "#7c7b78", "#ffea46", "darkred")
palette_light <- rgb(col2rgb(main_palette)[1, ], col2rgb(main_palette)[2, ], col2rgb(main_palette)[3, ], max = 255, alpha = 150)

palette_elevation <- c("#00204d", "#7c7b78", "#ffea46")

palette_meteo <- c("black", "darkred", "#00204d", "#52648d", "#7c7b78", "#cbba69", "#ffea46")

# meterological variables to be included
meteo_var <- c(
    "air_pressure",
    "precipitation",
    "relative_humidity",
    "radiation00",
    "radiation933",
    "radiation1450",
    "radiation_mean",
    "temp00",
    "temp770",
    "temp933",
    "temp1280",
    "temp1450",
    "temp_mean",
    "inversion_ratio",
    "UWD00",
    "UWD14",
    "VWD00",
    "VWD14",
    "mxWS00",
    "mnWS00",
    "avWS00",
    "mxWS14",
    "mnWS14",
    "avWS14"
)

meteo_elevation <- c(
    "0 m a.s.l.",
    "0 m a.s.l.",
    "0 m a.s.l.",
    "0 m a.s.l.",
    "933 m a.s.l.",
    "1450 m a.s.l.",
    "average",
    "0 m a.s.l.",
    "770 m a.s.l.",
    "933 m a.s.l.",
    "1280 m a.s.l.",
    "1450 m a.s.l.",
    "average",
    "none",
    "0 m a.s.l.",
    "1450 m a.s.l.",
    "0 m a.s.l.",
    "1450 m a.s.l.",
    "0 m a.s.l.",
    "0 m a.s.l.",
    "0 m a.s.l.",
    "1450 m a.s.l.",
    "1450 m a.s.l.",
    "1450 m a.s.l."
)

temp_var <- c(
    "T_00",
    "T_07",
    "T_09",
    "T_12",
    "T_14"
)

radiation_var <- c(
    "R_00",
    "R_09",
    "R_14"
)

meteo_labels <- c(
    "air pressure [hPa]",
    "precipitation [mm]",
    "relative humidity [%]",
    "radiation [W/m²]",
    "radiation [W/m²]",
    "radiation [W/m²]",
    "radiation [W/m²]",
    "temperature [°C]",
    "temperature [°C]",
    "temperature [°C]",
    "temperature [°C]",
    "temperature [°C]",
    "temperature [°C]",
    "inversion ratio [°K]",
    "wind direction (east/west)",
    "wind direction (east/west)",
    "wind direction (north/south)",
    "wind direction (north/south)",
    "average wind speed [m/s]",
    "maximum wind speed [m/s]",
    "minimum wind speed [m/s]",
    "average wind speed [m/s]",
    "maximum wind speed [m/s]",
    "minimum wind speed [m/s]"
)

# Levels
site <- c("Geiranger", "Fjordcentre", "Dalen")
elevation <- c("2 m a.s.l.", "90 m a.s.l.", "420 m a.s.l.")
season <- c("spring", "summer", "autumn", "winter")
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
levels_elev <- c(
    "none",
    "average",
    "0 m a.s.l.",
    "770 m a.s.l.",
    "933 m a.s.l.",
    "1280 m a.s.l.",
    "1450 m a.s.l."
)

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
    dec = c(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0) # 10-15, twilight: 7-18
)
