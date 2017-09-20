
# Performance tests -------------------------------------------------------

library(tidyverse)
library(eulerrPaper)
library(eulerr)
library(venneuler)
library(Vennerable)

set.seed(1)

Software <- character(0)
Sets <- integer(0)
Time <- double(0)

n_out <- 100
n_set <- 8
n_rep <- 1

for (i in 3:n_set) {
  cat("i=", i, "\n", sep = "")
  ids <- eulerr:::bit_indexr(i)
  for (j in 1:n_out) {
    if (j %% 10 == 0) cat(" j=", j, "\n", sep = "")
    combinations <- double(2^i - 1)

    for (k in 1:NROW(ids)) {
      names(combinations)[k] <- paste0(LETTERS[1:i][ids[k, ]], collapse = "&")
    }

    for (k in 1:i) {
      has_letter <- unlist(lapply(strsplit(names(combinations), split = "&"),
                                  function(x) any(x %in% LETTERS[1:i][k])))
      combinations[sample(which(has_letter), 1)] <- runif(1)
    }

    intersections <- (i + 1):(NROW(ids))
    how_many <- sample(1:length(intersections), 1)

    combinations[sample(intersections, how_many)] <- runif(how_many)

    if (i == 3) {
      test <- NULL
      # Fit the areas using vennerable if 3 sets
      tryCatch({test <- microbenchmark(
        Vennerable = compute.Venn(Venn(SetNames = LETTERS[1:i],
                                       Weight = c(0, as.numeric(combinations))),
                                  doWeights = TRUE,
                                  doEuler = TRUE,
                                  type = "circles"),
        eulerr_circles = euler(combinations),
        eulerr_ellipses = euler(combinations, shape = "ellipse"),
        venneuler = venneuler(combinations),
        times = n_rep
      )},
      error = function(e) {})

      if (!is.null(test)) {
        Software <- c(Software, as.character(test$expr))
        Sets <- c(Sets, rep(i, 4*n_rep))
        Time <- c(Time, test$time)
      }

    } else {
      test <- microbenchmark(
        eulerr_circles = euler(combinations),
        eulerr_ellipses = euler(combinations, shape = "ellipse"),
        venneuler = venneuler(combinations),
        times = n_rep
      )

      Sets <- c(Sets, rep(i, 3*n_rep))
      Software <- c(Software, as.character(test$expr))
      Time <- c(Time, test$time)
    }
  }
}

Software <- as.factor(Software)
levels(Software)[levels(Software) %in% "eulerr_circles"] <- "eulerr (circles)"
levels(Software)[levels(Software) %in% "eulerr_ellipses"] <- "eulerr (ellipses)"

if (i == 8 && j == 100) {
  data_performance <- tibble(Sets, Software, Time) %>%
    mutate(Software = as.factor(Software))

  usethis::use_data(data_performance, overwrite = TRUE)
}


