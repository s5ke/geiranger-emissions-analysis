## Cluster analysis
n_cluster <- 7
min_length_cluster <- 3

# restructure input
input_cluster_unscaled <- replace(
    input_ships_data[, meteo_var],
    is.na(input_ships_data[, meteo_var]) == TRUE, 0
)
input_cluster_unscaled[, meteo_var_not_averraged] <- NULL

# scale and center
input_cluster_scaled <- as.data.frame(scale(input_cluster_unscaled))
input_cluster <- as.data.frame(cbind(
    input_cluster_scaled,
     input_cluster_unscaled))

# add on/off-season from ships data (ships in port/no ships in port)
input_cluster$season <- input_ships_data$season
input_cluster$date <- input_ships_data $date
input_cluster$day <- input_ships_data$day
input_cluster$doy <- yday(as.Date(input_ships_data$day))
input_cluster$year <- input_ships_data$year
input_cluster$site <- input_ships_data$site
input_cluster$elevation <- input_ships_data$elevation
input_cluster$ships <- input_ships_data$all_ships
input_cluster$model <- input_ships_data$model
input_cluster$on_off_season_ships <- replace(
    input_cluster$ships,
 input_cluster$ships == 0, 0)
input_cluster$on_off_season_ships <- as.numeric(replace(input_cluster$on_off_season_ships,
 input_cluster$ships != 0, 1))

input_cluster$on_off_season <- replace(input_cluster$season, 
input_cluster$doy < 91 | input_cluster$doy > 301, "off-season")
input_cluster$on_off_season <- replace(input_cluster$on_off_season, input_cluster$on_off_season != "off-season", "on-season")

# daily
input_cluster_daily_unscaled <- replace(
    input_daily[, meteo_var],
    is.na(input_daily[, meteo_var]) == TRUE, 0
)
input_cluster_daily_unscaled[, meteo_var_not_averraged] <- NULL

# scale and center
input_cluster_daily_scaled <- as.data.frame(scale(input_cluster_daily_unscaled))
input_cluster_daily <- as.data.frame(cbind(
    input_cluster_daily_scaled,
     input_cluster_daily_unscaled))

# add on/off-season from ships data (ships in port/no ships in port)
input_cluster_daily$season <- input_daily$season
input_cluster_daily$date <- input_daily $date
input_cluster_daily$day <- input_daily$date
input_cluster_daily$doy <- yday(input_daily$date)
input_cluster_daily$year <- input_daily$year
input_cluster_daily$site <- input_daily$site
input_cluster_daily$elevation <- input_daily$elevation
input_cluster_daily$ships <- input_daily$all_ships
input_cluster_daily$model <- input_daily$model
input_cluster_daily$on_off_season_ships <- replace(
    input_cluster_daily$ships,
 input_cluster_daily$ships == 0, 0)
input_cluster_daily$on_off_season_ships <- as.numeric(replace(input_cluster_daily$on_off_season_ships,
 input_cluster_daily$ships != 0, 1))

input_cluster_daily$on_off_season <- replace(input_cluster_daily$season, 
input_cluster_daily$doy < 91 | input_cluster_daily$doy > 301, "off-season")
input_cluster_daily$on_off_season <- replace(input_cluster_daily$on_off_season, input_cluster_daily$on_off_season != "off-season", "on-season")

# calculate kmeans
calculate_cluster <- function(input_cluster_function, season, n, min_length_cluster) {
onseason <- which(season == "on-season")
input_kmeans <- input_cluster_function[onseason,]

kmeans <- kmeans(input_kmeans,
n, 
algorithm = "Hartigan-Wong") # Cluster berechnen Auskommentieren und Objekt "kmeans" laden (unten), damit Cluster unverändert bleiben

kmeans[["betweenss"]] / kmeans[["totss"]] # sum of squares ratio
kmeans[["totss"]]

# Smoothing
input_kmeans$cluster <- as.numeric(unlist(kmeans$cluster))
rle <- rle(input_kmeans$cluster)
input_kmeans$length <- unlist(
    sapply(c(1:length(rle[[2]])), function(x) 
    rep(rle[[1]][x], 
    rle[[1]][x])))

input_kmeans$smooth <- ifelse(input_kmeans$length < min_length_cluster,
 "exception",
  "cluster")

subset(input_kmeans, input_kmeans$smooth != "exception")
n <- nrow(input_kmeans) - nrow(input_kmeans_reduced)

}
input_cluster_daily_scaled <- subset(input_cluster_daily_scaled, 
input_cluster_daily$site == site[1])


input_kmeans_reduced <- 
calculate_cluster(input_cluster_daily_scaled, 
input_cluster_daily$on_off_season,
n_cluster,
min_length_cluster)


rle <- rle(input_kmeans$smooth)
split_clusters <- split(input_kmeans, rep(seq_along(rle$lengths), rle$lengths))

for (i in seq_along(split_clusters)) {
split_clusters[[i]]$cluster_smooth_lag <- ifelse(i>1, split_clusters[[i-1]]$cluster, NA)
split_clusters[[i]]$cluster_smooth_lead <- ifelse(i<length(split_clusters), split_clusters[[i+1]]$cluster, NA)
split_clusters[[i]]$cluster_smooth <- mean(split_clusters[[i]]$cluster)

if(nrow(split_clusters[[i]]) < min_length_cluster) {
split_clusters[[i]]$cluster_smooth <- split_clusters[[i]]$cluster_smooth_lag    
}
}
test <- split(input_kmeans, input_kmeans$smooth)
View(split_clusters[[61]])
View(input_kmeans)

# calculate percentage of each cluster
perc <- split(input_onseason, input_onseason$cluster_smooth)
perc <- lapply(perc, function(x) nrow(x) / nrow(input_onseason))


input_offseason <- input[["off-season"]]



input_onseason2 <- input_onseason
input_onseason2$cluster <- rep(n + 1, nrow(input_onseason))
input_onseason2$length <- rep(NA, nrow(input_onseason))
input_onseason2$cluster_smooth <- rep(n + 1, nrow(input_onseason))
input_onseason2$length2 <- rep(NA, nrow(input_onseason))

input_onseason <- as.data.frame(rbind(input_onseason, input_onseason2))

input <- as.data.frame(rbind(input_offseason, input_onseason))


cluster_means <- aggregate(input_onseason[, 2:(ncol(input_onseason) - 5)], by = list(cluster = input_onseason$cluster_smooth), mean)
cluster_means$season <- rep("on-season", nrow(cluster_means))
cluster_means_sd <- aggregate(input_onseason[, 2:(ncol(input_onseason) - 5)], by = list(cluster = input_onseason$cluster_smooth), sd)

# cluster_means <- as.data.frame(do.call(rbind, cluster_means))
# cluster_means_sd <- as.data.frame(do.call(rbind, cluster_means_sd))
# input <- as.data.frame(do.call(rbind, input))

pinput2 <- data.frame(
    value = c(
        cluster_means[, 2],
        cluster_means[, 3],
        cluster_means[, 4],
        cluster_means[, 5],
        cluster_means[, 8],
        cluster_means[, 9],
        cluster_means[, 10],
        cluster_means[, 11],
        cluster_means[, 12],
        cluster_means[, 13]
    ),
    value2 = c(
        cluster_means[, 14],
        cluster_means[, 15],
        cluster_means[, 16],
        cluster_means[, 17],
        cluster_means[, 20],
        cluster_means[, 21],
        cluster_means[, 22],
        cluster_means[, 23],
        cluster_means[, 24],
        cluster_means[, 25]
    ),
    sd = c(
        cluster_means_sd[, 2],
        cluster_means_sd[, 3],
        cluster_means_sd[, 4],
        cluster_means_sd[, 5],
        cluster_means_sd[, 8],
        cluster_means_sd[, 9],
        cluster_means_sd[, 10],
        cluster_means_sd[, 11],
        cluster_means_sd[, 12],
        cluster_means_sd[, 13]
    ),
    sd2 = c(
        cluster_means_sd[, 14],
        cluster_means_sd[, 15],
        cluster_means_sd[, 16],
        cluster_means_sd[, 17],
        cluster_means_sd[, 20],
        cluster_means_sd[, 21],
        cluster_means_sd[, 22],
        cluster_means_sd[, 23],
        cluster_means_sd[, 24],
        cluster_means_sd[, 25]
    ),
    x = rep(colnames(cluster_means)[c(2:5, 8:13)], each = nrow(cluster_means)),
    cluster = rep(cluster_means$cluster, 10),
    season = rep(cluster_means$season, 10)
)


cluster <- paste(pinput2$cluster, pinput2$season)
old <- unique(cluster)
new <- c(paste("Cluster", 1:n), "All clusters")
cluster[cluster %in% old] <- new[match(cluster, old, nomatch = 0)]
pinput2$cluster <- factor(cluster, levels = new)


cluster <- paste(input$cluster_smooth, input$season)
old <- unique(cluster)
new <- c(
    "Cluster 7 off-season",
    "Cluster 5 off-season",
    "Cluster 3 off-season",
    "Cluster 6 off-season",
    "Cluster 1 off-season",
    "Cluster 4 off-season",
    "Cluster 2 off-season",
    NA,
    "Cluster 7 on-season",
    "Cluster 2 on-season",
    "Cluster 6 on-season",
    "Cluster 1 on-season",
    "Cluster 4 on-season",
    "Cluster 3 on-season",
    "Cluster 5 on-season",
    "All clusters"
)
cluster[cluster %in% old] <- new[match(cluster, old, nomatch = 0)]
input$cluster2 <- factor(cluster, levels = new)
input$cluster3 <- sapply(strsplit(as.character(input$cluster2), "[ ]"), "[", 2)


# get cluster length
length <- aggregate(input$length2, by = list(cluster = input$cluster2), mean)
length <- length[order(as.numeric(length$cluster)), ]
length$season <- c(sapply(strsplit(as.character(length$cluster[1:14]), "[ ]"), "[", 3), "on-season")

length2 <- subset(length, length$season == "on-season")
length2$cluster <- paste(
    sapply(strsplit(as.character(length2$cluster), "[ ]"), "[", 1),
    sapply(strsplit(as.character(length2$cluster), "[ ]"), "[", 2)
)

p_cluster <- ggplot(subset(pinput2, pinput2$season == "on-season")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange(aes(x = x, y = value, ymin = value - sd / 2, ymax = value + sd / 2, group = paste(x, season), col = x), size = .2) +
    scale_color_viridis(option = "E", discrete = TRUE) +
    facet_wrap(~ cluster + season, nrow = 4) +
    geom_text(
        data = length2,
        aes(x = Inf, y = -Inf, hjust = 1.1, vjust = -0.2, label = round(x, 2)), size = 3
    ) +
    theme_light() +
    labs(x = "", y = "measured variable (averaged, scaled and centered)", col = "") +
    theme(axis.text.x = element_text(angle = 90))
p_cluster

cairo_pdf("Cluster_on_season.pdf", width = 6, height = 12, pointsize = 10)
p_cluster
dev.off()


df$day <- substr(df$date, 1, 10)
df$day <- as.Date(df$day, format = "%d.%m.%Y")
cluster <- as.character(df$day)
old <- as.character(input$day)
new <- as.character(input$cluster2)
cluster[cluster %in% old] <- new[match(cluster, old, nomatch = 0)]
df$cluster <- factor(cluster, level = unique(new))
ships$day <- substr(ships$date, 1, 10)
ships$day <- as.Date(ships$day, format = "%d.%m.%Y")
ships$year <- year(ships$day)
input_ships <- ships[ships$year != 2020, ]
df$ships <- rep(input_ships$all_ships, 3)
df$ferry <- rep(input_ships$Ferry, 3)
df$cruise <- rep(input_ships$Cruise_ships, 3)
df2 <- subset(df, df$season2 == "on-season")
df2$cluster <- rep("All clusters", nrow(df2))
df2 <- as.data.frame(rbind(df, df2))
input <- split(df2, df2$cluster)


pcluster <- ggplot(pinput2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange(aes(x = x, y = value, ymin = value - sd / 2, ymax = value + sd / 2, group = paste(x, season), col = x), size = .2) +
    scale_color_viridis(option = "E", discrete = TRUE) +
    facet_wrap(~ cluster + season, nrow = 1) +
    geom_text(
        data = length2,
        aes(x = Inf, y = -Inf, hjust = 1.1, vjust = -0.2, label = round(x, 2)), size = 3
    ) +
    theme_light() +
    labs(x = "", y = "measured variable (averaged, scaled and centered)", col = "") +
    theme(axis.text.x = element_text(angle = 90))
pcluster

pcluster2 <- ggplot(subset(pinput2, pinput2$season == "on-season" & pinput2$cluster != "All clusters")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange(aes(x = x, y = value, ymin = value - sd / 2, ymax = value + sd / 2, group = paste(x, season), col = x), size = .2) +
    scale_color_viridis(option = "E", discrete = TRUE) +
    facet_wrap(~ cluster + season, ncol = 1) +
    geom_text(
        data = subset(length2, length2$season == "on-season" & length2$cluster != "All clusters"),
        aes(x = Inf, y = -Inf, hjust = 1.1, vjust = -0.2, label = round(x, 2)), size = 3
    ) +
    theme_light() +
    labs(x = "", y = "measured variable (averaged, scaled and centered)", col = "") +
    theme(
        axis.text.x = element_text(angle = 90),
        legend.position = "none"
    )
pcluster2

x <- pinput2$x
old <- unique(pinput2$x)
new <- c(
    "air pressure [hPa]", "precipitation [mm]", "relative humidity [%]",
    "inversion ratio [°K]", "temperature [°C]", "radiation [W/m²]", "wind direction (east/west)", "wind direction (north/south)",
    "average wind speed [m/s]", "maximum wind speed [m/s]"
)
x[x %in% old] <- new[match(x, old, nomatch = 0)]
pinput2$x <- x

pinput2 <- subset(pinput2, pinput2$season == "on-season")
pcluster3 <- ggplot(pinput2) +
    # geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange(aes(x = cluster, y = value2, ymin = value2 - sd2 / 2, ymax = value2 + sd2 / 2, group = paste(x, season), col = x), size = .2) +
    scale_color_viridis(option = "E", discrete = TRUE) +
    facet_wrap(~x, nrow = 1, scale = "free_y") +
    theme_light() +
    labs(x = "", y = "measured variable", col = "") +
    theme(
        axis.text.x = element_text(angle = 90),
        legend.position = "none"
    )
pcluster3

cairo_pdf("Cluster_conditions.pdf", width = 18.5, height = 4, pointsize = 10)
pcluster3
dev.off()

# boxplot
pinput <- subset(df2, is.na(df2$cluster) == FALSE)

p1 <- ggplot(pinput) +
    geom_boxplot(aes(x = site, y = PM2.5, group = paste(site), col = site), outlier.shape = NA) +
    ylim(c(0, 30)) +
    scale_color_viridis(option = "E", discrete = TRUE) +
    theme_light() +
    labs(x = "", y = "PM2.5 [µg/m³]") +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(~cluster, nrow = 1)
p1

pinput$season2 <- sapply(strsplit(as.character(pinput$cluster), "[ ]"), "[", 3)
pinput$cluster <- as.character(pinput$cluster)
p1_2 <- ggplot(subset(pinput, pinput$season2 == "on-season" & pinput$cluster != "All clusters")) +
    geom_boxplot(aes(x = site, y = PM2.5, group = paste(site), col = site), outlier.shape = NA) +
    ylim(c(0, 30)) +
    scale_color_viridis(option = "E", discrete = TRUE) +
    theme_light() +
    labs(x = "", y = "PM2.5 [µg/m³]") +
    theme(
        axis.text.x = element_text(angle = 90),
        legend.position = "none"
    ) +
    facet_wrap(~ cluster + season2, ncol = 1)
p1_2

cairo_pdf("PM_cluster_on_season.pdf", width = 20, height = 4, pointsize = 10)
p1
dev.off()


p2 <- ggplot(subset(pinput, is.na(pinput$season2) == FALSE)) +
    geom_boxplot(aes(x = cluster, y = PM2.5, group = paste(site, cluster), col = site), outlier.shape = NA) +
    ylim(c(0, 30)) +
    scale_color_viridis(option = "E", discrete = TRUE) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "") +
    facet_wrap(~season2, scale = "free_x", nrow = 1)
p2

cairo_pdf("PM_cluster2_by_on_and_off_season.pdf", width = 8, height = 5, pointsize = 10)
p2
dev.off()

cairo_pdf("cluster_analysis3_by_on_and_off_season.pdf", width = 22, height = 10, pointsize = 10)
ggarrange(
    plotlist = list(pcluster, p1), widths = c(1, 1),
    nrow = 2, align = "hv", common.legend = FALSE, legend = "right", labels = c("A", "B")
)


dev.off()

library("writexl")
write_xlsx(pinput, "~/Promotion/Feinstaub/output_cluster.xlsx")


# lagged correlations
n <- 100

b <- function(a, b) {
    b <- replace(b, b == -Inf, NA)
    cor <- ifelse(length(na.omit(a)) > 3, cor.test(a, b, method = "pearson", na.action = na.exclude)$estimate, NA)
    p <- ifelse(length(na.omit(a)) > 3, cor.test(a, b, method = "pearson", na.action = na.exclude)$p.value, NA)
    ci_5 <- ifelse(length(na.omit(a)) > 3, cor.test(a, b, method = "pearson", na.action = na.exclude)$conf.int[1], NA)
    ci_95 <- ifelse(length(na.omit(a)) > 3, cor.test(a, b, method = "pearson", na.action = na.exclude)$conf.int[2], NA)
    c(cor, p, ci_5, ci_95)
}

df$season2 <- sapply(strsplit(as.character(df$cluster), "[ ]"), "[", 3)
input <- split(df, list(df$site))

lead <- list()
input_pm <- list()
for (i in c(1:length(input))) {
    input[[i]]$PM2.5 <- scale(input[[i]]$PM2.5)
    input[[i]]$model <- scale(input[[i]]$model)
    input[[i]]$Ships <- scale(input[[i]]$ships)
    input[[i]]$Ferry <- scale(input[[i]]$ferry)
    input[[i]]$Cruise <- scale(input[[i]]$cruise)
    lead[[i]] <- as.data.frame(sapply(c(1:n), function(x) lead(input[[i]]$PM2.5, n = x)))
    input_pm[[i]] <- as.data.frame(cbind(input[[i]], lead[[i]]))
}


input_pm <- as.data.frame(do.call(rbind, input_pm))
histogram(input_pm$V1)
histogram(input_pm$V2)
plot(input_pm$V1, type = "l")
lines(input_pm$V2, col = "red")

input_pm$group <- paste(input_pm$site, input_pm$day)


input2 <- aggregate(input_pm[, c(1, 28, 39:142)], by = list(input_pm$group), function(x) mean(x, na.rm = TRUE))
input3 <- aggregate(input_pm[, c(2, 27, 29, 34, 35, 38)], by = list(input_pm$group), first)
input_pm <- as.data.frame(cbind(input3, input2))




input_pm2 <- subset(input_pm, input_pm$season2 == "on-season")
colnames(input_pm)[8] <- "group"
colnames(input_pm2)[8] <- "group"
input_pm2$cluster <- rep("All clusters", nrow(input_pm2))
input_pm <- as.data.frame(rbind(input_pm, input_pm2))
input_pm <- split(input_pm, list(input_pm$site, input_pm$cluster))
cor_lead_pm <- list()
cor_lead_model <- list()

for (i in c(1:length(input_pm))) {
    cor_lead_pm[[i]] <- as.data.frame(rbind(
        t(sapply(input_pm[[i]][, 14:(n + 13)], function(x) b(input_pm[[i]]$Ships, x))),
        t(sapply(input_pm[[i]][, 14:(n + 13)], function(x) b(input_pm[[i]]$Ferry, x))),
        t(sapply(input_pm[[i]][, 14:(n + 13)], function(x) b(input_pm[[i]]$Cruise, x)))
    ))
    cor_lead_pm[[i]]$lead <- rep(c(1:n), 3)
    cor_lead_pm[[i]]$ships <- rep(c("All", "Ferry", "Cruise ships"), each = n)
    cor_lead_model[[i]] <- as.data.frame(rbind(t(sapply(input_pm[[i]][, 14:(n + 13)], function(x) b(input_pm[[i]]$model, x)))))
    cor_lead_model[[i]]$lead <- c(1:n)
    cor_lead_model[[i]]$ships <- rep("All", n)
}
cor_lead_pm <- as.data.frame(do.call(rbind, cor_lead_pm))
colnames(cor_lead_pm) <- c("cor", "p", "ci_5", "ci_95", "lead", "ships")
cor_lead_pm$id <- rep(names(input_pm), each = n * 3)
cor_lead_pm$cluster <- sapply(strsplit(cor_lead_pm$id, "[.]"), "[[", 2)
cor_lead_pm$site <- sapply(strsplit(cor_lead_pm$id, "[.]"), "[[", 1)
cor_lead_pm$sig <- replace(cor_lead_pm$cor, cor_lead_pm$p > 0.05, NA)
# cor_lead_pm$days <- rep(1:100)

cor_lead_model <- as.data.frame(do.call(rbind, cor_lead_model))
colnames(cor_lead_model) <- c("cor", "p", "ci_5", "ci_95", "lead", "ships")
cor_lead_model$id <- rep(names(input_pm), each = n)
cor_lead_model$cluster <- sapply(strsplit(cor_lead_model$id, "[.]"), "[[", 2)
cor_lead_model$site <- sapply(strsplit(cor_lead_model$id, "[.]"), "[[", 1)
cor_lead_model$sig <- replace(cor_lead_model$cor, cor_lead_model$p > 0.05, NA)
# cor_lead_model$days <- rep(1:30, each = 24)

# save(cor_lead_pm, file = "~/Promotion/Feinstaub/cor_lead_pm.gz")
# save(cor_lead_model, file = "~/Promotion/Feinstaub/cor_lead_model.gz")

# load("~/Promotion/Feinstaub/cor_lead_pm.gz")
# load("~/Promotion/Feinstaub/cor_lead_model.gz")
# Plot
palette <- c("#00204d", "#7c7b78", "#ffea46", "darkred")
pinput <- as.data.frame(rbind(cor_lead_pm, cor_lead_model))
pinput$method <- c(rep("raw", nrow(cor_lead_pm)), rep("modelled", nrow(cor_lead_model)))


# pinput$cluster2 <- as.numeric(substr(pinput$cluster, 9, nchar(pinput$cluster)))
# pinput$cluster2 <-ifelse(is.na(pinput$cluster2) == TRUE, 15, pinput$cluster2)
pinput2 <- subset(pinput, pinput$cluster == "All clusters")
pinput2$season2 <- rep("on-season", nrow(pinput2))
pinput <- subset(pinput, pinput$cluster != "All clusters")
pinput$season2 <- sapply(strsplit(pinput$cluster, "[ ]"), "[[", 3)
pinput <- as.data.frame(rbind(pinput, pinput2))

pinput <- subset(pinput, pinput$season2 == "on-season")
pinput <- subset(pinput, pinput$ships == "All")
# pinput$cluster <- factor(pinput$cluster, paste("Cluster", 1:(15)))
# pinput$cluster <- factor(pinput$cluster, level = c(paste("Cluster", c(1:7)), "All clusters") )

p3 <- ggplot(subset(pinput, pinput$cluster != "All clusters")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = c(0.5, -0.5), linetype = "dashed", col = "grey") +
    geom_point(aes(x = lead, y = cor, group = paste(site, method), col = site, shape = method), size = 1.5, alpha = .3) +
    geom_point(aes(x = lead, y = sig, group = paste(site, method), col = site, shape = method), size = 1.5) +
    geom_line(aes(x = lead, y = cor, group = paste(site, method), col = site, linetype = method), size = .3, alpha = .3) +
    geom_line(aes(x = lead, y = sig, group = paste(site, method), col = site, linetype = method), size = .3) +
    facet_wrap(~cluster, ncol = 1) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 20)) +
    scale_color_manual(values = palette) +
    theme_light() +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
    ) +
    labs(y = "Correlation coefficient (Pearson)", x = "Time (lead) [days]")
p3


cairo_pdf("cluster_analysis_on_season.pdf", width = 12, height = 15, pointsize = 10)
ggarrange(
    plotlist = list(pcluster2, p1_2, p3), widths = c(1, 1, 3),
    ncol = 3, align = "hv", common.legend = FALSE, labels = c("A", "B", "C")
)


dev.off()


p4 <- ggplot(subset(pinput, pinput$cluster == "All clusters")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = c(0.5), linetype = "dashed", col = "grey") +
    # geom_ribbon(aes(x = lead, y = cor, ymin = ci_5, ymax = ci_95, group = paste(site, method), fill = site, shape = method), size = 1, alpha = .3) + #Optional! Konfidenzintervall hinzufügen
    geom_point(aes(x = lead, y = cor, group = paste(site, method), col = site, shape = method), size = 1, alpha = .3) +
    geom_point(aes(x = lead, y = sig, group = paste(site, method), col = site, shape = method), size = 1) +
    geom_line(aes(x = lead, y = cor, group = paste(site, method), col = site, linetype = method), size = .3, alpha = .3) +
    geom_line(aes(x = lead, y = sig, group = paste(site, method), col = site, linetype = method), size = .3) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
    scale_color_manual(values = palette) +
    scale_fill_manual(values = palette_light) +
    theme_light() +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
    ) +
    labs(y = "Correlation coefficient (Pearson)", x = "Time (lead) [days]")
p4
cairo_pdf("correlation_on_season.pdf", width = 10, height = 4, pointsize = 10)
p4
dev.off()

input_models <- input
