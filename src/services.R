## correlation function
# a,b: vectors to correlate
correlate <- function(a, b) {
  b <- replace(b, b == -Inf, NA)
  b <- replace(b, b == Inf, NA)
  a <- replace(a, a == -Inf, NA)
  a <- replace(a, a == Inf, NA)
  a <- as.numeric(a)
  b <- as.numeric(b)
  if (length(na.omit(a)) > 5 && length(na.omit(b)) > 5) {
    cor <- cor.test(a, b,
      method = "pearson",
      na.action = na.exclude
    )$estimate
    p <- cor.test(a, b,
      method = "pearson",
      na.action = na.exclude
    )$p.value
    ci_5 <- cor.test(a, b,
      method = "pearson",
      na.action = na.exclude
    )$conf.int[1]
    ci_95 <- cor.test(a, b,
      method = "pearson",
      na.action = na.exclude
    )$conf.int[2]
    c(cor, p, ci_5, ci_95)
  } else {
    c(NA, NA, NA, NA)
  }
}

## function to split columns into day and night while retaining length
# df: input dataframe
# colnames: colnames to split
# splitby: column to split_by
split_daynight <- function(df, colnames, splitby) {
  groups <- unique(df[, splitby])
  for (i in colnames) {
    first <- paste(i, groups[1], sep = "_")
    second <- paste(i, groups[2], sep = "_")
    df[, first] <- replace(df[, i], df[, splitby] == groups[2], NA)
    df[, second] <- replace(df[, i], df[, splitby] == groups[1], NA)
  }
  df
}

## function to get season from date
get_season <- function(date) {
  season <- lubridate::month(date)
  old <- c(1:12)
  new <- c(
    "winter", "winter",
    "spring", "spring", "spring",
    "summer", "summer", "summer",
    "autumn", "autumn", "autumn",
    "winter"
  )
  season[season %in% old] <- new[match(season, old, nomatch = 0)]
  season
}

# get names of ships from input vector of form
# ("[name0], [name1]", "[name2]", ...)
get_ship_names <- function(ships) {
  result <- gsub("\\,|\\[|\\]", "", ships)
  result <- result[!result %in% c("")]
  result <- paste0(result, collapse = " ")
  result <- stringr::str_split_1(result, "' '")
  result <- unique(gsub("\\'", "", result))
  result
}

# get sums for varying window widths
get_window_sums <- function(df, input_vector, max_width, align) {
  result <- as.data.frame(
    sapply(
      c(1:max_width), function(x) {
        zoo::rollapply(input_vector,
          width = x,
          function(y) sum(y, na.rm = TRUE),
          fill = NA,
          align = align
        )
      }
    )
  )
  result <- as.data.frame(cbind(result, df))
  result
}

# functions to get lead or lag value based on 'n'
lead_by_n <- function(x, n) {
  result <- rep(NA, length(x))
  for (i in seq_along(n)) {
    next_index <- min(i + n[i], length(x))
    result[i] <- x[next_index]
  }
  return(result)
}

lag_by_n <- function(x, n) {
  result <- rep(NA, length(x))
  for (i in seq_along(n)) {
    prev_index <- max(i - n[i], 1)
    result[i] <- x[prev_index]
  }
  return(result)
}
