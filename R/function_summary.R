#function to do summary statistics by year , scenario and species
my.summary <- function(x, na.rm=TRUE){result <- list(c(n = as.integer(length(x)),
                        Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE),
                        Median = median(x),   Min = min(x), Max = max(x)))}


stat <- d %>%
   filter(species == "Cephalopholis fulva") %>%
   group_by(scenario,year, species) %>%
summarise_all(funs( mean,median,sd, max, min))

