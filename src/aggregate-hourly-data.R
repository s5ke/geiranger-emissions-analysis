## Calculate daily means from hourly data
input_ships_daily <- aggregate(input_ships,
  by = list(input_restructured$site, input_restructured$day),
<<<<<<< Updated upstream
  function(x) sum(x, na.rm = TRUE)
=======
  function(x) sum(na.omit(x))
>>>>>>> Stashed changes
)

test <- split(input_ships, list(input_restructured$site, input_restructured$day))

input_data_daily <- aggregate(input_restructured[, data_var],
  by = list(input_restructured$site, input_restructured$day),
  function(x) mean(x, na.rm = TRUE)
)

input_meta_daily <- aggregate(input_restructured[, metadata],
  by = list(input_restructured$site, input_restructured$day),
  first
)

input_daily <- as.data.frame(
  cbind(
    input_ships_daily[, -1],
    input_data_daily[, -c(1:2)],
    input_meta_daily[, -c(1:2)]
  )
)
colnames(input_daily)[1] <- "date"
