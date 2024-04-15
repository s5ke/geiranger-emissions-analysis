## correlation function
# a,b: vectors to correlate
correlate <- function(a, b) {
    b <- replace(b, b == -Inf, NA)
    a <- as.numeric(a)
    b <- as.numeric(b)
    cor <- ifelse(length(na.omit(a)) > 3, cor.test(a, b, method = "pearson", na.action = na.exclude)$estimate, NA)
    p <- ifelse(length(na.omit(a)) > 3, cor.test(a, b, method = "pearson", na.action = na.exclude)$p.value, NA)
    ci_5 <- ifelse(length(na.omit(a)) > 3, cor.test(a, b, method = "pearson", na.action = na.exclude)$conf.int[1], NA)
    ci_95 <- ifelse(length(na.omit(a)) > 3, cor.test(a, b, method = "pearson", na.action = na.exclude)$conf.int[2], NA)
    c(cor, p, ci_5, ci_95)
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
    season <- month(date)
    old <- c(1:12)
    new <- c("winter", "winter", "spring", "spring", "spring", "summer", "summer", "summer", "autumn", "autumn", "autumn", "winter")
    season[season %in% old] <- new[match(season, old, nomatch = 0)]
    season
}

# get names of ships from input vector of form ("[name0], [name1]", "[name2]", ...)
get_ship_names <- function(ships) {
    result <- gsub("\\,|\\[|\\]", "", ships)
    result <- result[!result %in% c("")]
    result <- paste0(result, collapse = " ")
    result <- str_split_1(result, "' '")
    result <- unique(gsub("\\'", "", result))
    result
}
