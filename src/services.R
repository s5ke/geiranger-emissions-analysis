# correlation function
correlate <- function(a,b) {b <- replace(b, b == -Inf, NA)
a <- as.numeric(a)
b <- as.numeric(b)
cor <- ifelse(length(na.omit(a)) > 3, cor.test(a, b, method = "pearson", na.action = na.exclude)$estimate, NA )
p <- ifelse(length(na.omit(a)) > 3, cor.test(a, b, method = "pearson", na.action = na.exclude)$p.value, NA ) 
ci_5 <- ifelse(length(na.omit(a)) > 3, cor.test(a, b, method = "pearson", na.action = na.exclude)$conf.int[1], NA ) 
ci_95 <- ifelse(length(na.omit(a)) > 3, cor.test(a, b, method = "pearson", na.action = na.exclude)$conf.int[2], NA ) 
c(cor, p, ci_5, ci_95)
} 
