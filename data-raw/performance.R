
# Performance tests -------------------------------------------------------

library(tidyverse)
library(eulerr)
library(venneuler)
library(Vennerable)
library(microbenchmark)

set.seed(1)

out <- data.frame(it = integer(),
                  software = character(),
                  sets = integer(),
                  time = double())

n_set <- 8

for (i in 3:n_set) {
  cat("i=", i, "\n", sep = "")
  ids <- eulerr:::bit_indexr(i)
  satisfied <- FALSE
  j <- 1

  while (!satisfied) {
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

    test <- NULL

    if (i == 3) {
      # Fit the areas using vennerable if 3 sets
      tryCatch({test <- microbenchmark::microbenchmark(
        Vennerable = compute.Venn(Venn(SetNames = LETTERS[1:i],
                                       Weight = c(0, as.numeric(combinations))),
                                  doWeights = TRUE,
                                  doEuler = TRUE,
                                  type = "circles"),
        eulerr_circles = euler(combinations),
        eulerr_ellipses = euler(combinations, shape = "ellipse"),
        venneuler = venneuler(combinations),
        times = 1L
      )},
      error = function(e) {})

    } else {
      test <- microbenchmark::microbenchmark(
        eulerr_circles = euler(combinations),
        eulerr_ellipses = euler(combinations, shape = "ellipse"),
        venneuler = venneuler(combinations),
        times = 1L
      )
    }

    if (!is.null(test)) {
      out <- rbind(out, data.frame(it = j,
                                   software = as.character(test$expr),
                                   sets = i,
                                   time = test$time))
    }

    if (j >= 100) {
      # dd <- filter(out, sets == i)
      #
      # ptt <- TukeyHSD(aov(time ~ software, data = dd))
      #
      # if (all(ptt$software[, 4] < 0.05))
      #   satisfied <- TRUE

      if (j >= 100) { # Run at least 100 iterations
        dd <- filter(out, sets == i) %>%
          group_by(software) %>%
          summarise(ci = qnorm(0.975)*sd(Time/1e6, na.rm = TRUE)/sqrt(n()))

        # Stop when the 95% CI for each estimate is smaller than 1 millisecond
        if (all(dd$ci*2 < 1))
          satisfied <- TRUE
      }

    }

    j <- j + 1
  }
}

if (i == 8 && j >= 100) {
  data_performance <-
    out %>%
    mutate(software = factor(software,
                             levels = c("eulerr_circles",
                                        "eulerr_ellipses",
                                        "venneuler",
                                        "Vennerable"),
                             labels = c("eulerr (circles)",
                                        "eulerr (ellipses)",
                                        "venneuler",
                                        "Vennerable"))) %>%
    rename(Sets = sets, Time = time)

  usethis::use_data(data_performance, overwrite = TRUE)
}


