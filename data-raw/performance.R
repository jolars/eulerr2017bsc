
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

for (i in 3:8) {
  ids <- eulerr:::bit_indexr(i)
  for (j in 1:100) {
    if (j %% 10 == 0) cat("i=", i, "; j=", j, "\n", sep = "")
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

    Software <- c(Software, "eulerr (circles)", "eulerr (ellipses)", "venneuler")
    Sets <- c(Sets, rep(i, 3))

    if (i == 3) {
      # Fit the areas using vennerable if 3 sets
      tryCatch({
        time <- microbenchmark(
          Vennerable = compute.Venn(Venn(SetNames = LETTERS[1:i],
                            Weight = c(0, as.numeric(combinations))),
                       doWeights = TRUE,
                       doEuler = TRUE,
                       type = "circles"),
          eulerr_circles = euler(combinations),
          eulerr_ellipses = euler(combinations, shape = "ellipse"),
          venneuler = venneuler(combinations),
          times = 1L
        )$time
      }, error = function(e) {})

      Software <- c(Software, "Vennerable")
      Sets <- c(Sets, i)
      Stress <- c(Stress, vennerable_gof$stress)
      diagError <- c(diagError, vennerable_gof$diag_error)
    } else {
      time <- microbenchmark(
        eulerr_circles = euler(combinations),
        eulerr_ellipses = euler(combinations, shape = "ellipse"),
        venneuler = venneuler(combinations),
        times = 1L
      )$time
    }

    Time <- c(Time, time)
  }
}

if (i == 8 && j == 100) {
  data_performance <- tibble(Sets, Software, time) %>%
    mutate(software = as.factor(Software))

  usethis::use_data(data_performance, overwrite = TRUE)
}


