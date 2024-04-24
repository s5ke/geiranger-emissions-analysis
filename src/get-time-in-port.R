# Fetch ships that arrived at the port

# get ship names
ships <- unlist(input_restructured$ships)
n <- rep(0, nrow(input_restructured))
ship_names <- get_ship_names(ships)
remove <- c()
ship_names <- ship_names[, -remove]

# get number of hours in port
input_ships <- list()
for (i in ship_names) {
  input_ships[[i]] <- data.frame(value = replace(n, grep(i, ships), 1))
  colnames(input_ships[[i]])[1] <- i
}
input_ships <- as.data.frame(do.call(cbind, input_ships))

# calculate total amount
input_ships$all_ships <- rowSums(input_ships)

# calculate for Hurtigruten
hurtigruten <- c(
  "Hurtigruten_dummy",
  "Midnatsol",
  "Fridtjof Nansen",
  "Kong Harald",
  "Nordkapp",
  "Polarlys",
  "Nordlys",
  "Nordnorge",
  "Richard With",
  "Roald Amundsen",
  "Trollfjord",
  "Vesteralen",
  "Lofoten",
  "Finnmarken"
)
input_ships$hurtigruten <- rowSums(input_ships[, hurtigruten])

# calculate for cruise ships
cruise_ships <- ship_names[!ship_names %in% c(hurtigruten, "Ferry")]
input_ships$cruise_ships <- rowSums(input_ships[, cruise_ships])

# Add metadata to dataframe
input_metadata <- input_restructured[!input_restructured %in% meteo_var]
input_ships_meta <- as.data.frame(cbind(input_metadata, input_ships))
input_ships_data <- as.data.frame(cbind(input_restructured, input_ships))
