## Calculate daily means from hourly data
input_ships_hourly <- aggregate(input_ships,
  by = list(input_restructured$site, input_restructured$date),
  function(x) sum(na.omit(x))
)

input_data_hourly <- aggregate(input_restructured[, data_var],
  by = list(input_restructured$site, input_restructured$date),
  function(x) mean(x, na.rm = TRUE)
)

input_meta_hourly <- aggregate(input_restructured[, metadata],
  by = list(input_restructured$site, input_restructured$date),
  first
)

input_hourly <- as.data.frame(
  cbind(
    input_ships_hourly[, -1],
    input_data_hourly[, -c(1:2)],
    input_meta_hourly[, -c(1:2)]
  )
)
colnames(input_hourly)[1] <- "datetime"

# Save file
write.csv(input_hourly, file = "input_hourly.csv")
