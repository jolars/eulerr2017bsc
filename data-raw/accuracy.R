library(tidyr)
library(dplyr)
library(tibble)
library(eulerrPaper)
library(eulerr)
library(venneuler)
library(Vennerable)

set.seed(1)

Software <- character(0)
Stress <- double(0)
diagError <- double(0)
Sets <- integer(0)

for (i in 3:8) {
  ids <- eulerr:::bit_indexr(i)
  for (j in 1:100) {
    if (j %% 100 == 0) cat("i=", i, "; j=", j, "\n", sep = "")
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

    eulerr_circles <- euler(combinations)
    eulerr_ellipses <- euler(combinations, shape = "ellipse")

    venneuler_fit <- venneuler(combinations)
    venneuler_gof <- gof(venneuler_fit, combinations)

    Software <- c(Software, "eulerr (circles)", "eulerr (ellipses)", "venneuler")
    Sets <- c(Sets, rep(i, 3))
    Stress <- c(Stress,
                eulerr_circles$stress,
                eulerr_ellipses$stress,
                venneuler_gof$stress)
    diagError <- c(diagError,
                   eulerr_circles$diag_error,
                   eulerr_ellipses$diag_error,
                   venneuler_gof$diag_error)

    if (i == 3) {
      # Fit the areas using vennerable if 3 sets
      vest <- Vennerable::Venn(SetNames = LETTERS[1:i],
                               Weight = c(0, as.numeric(combinations)))

      vennerable_gof <- list(stress = NA, diag_error = NA)
      vennerable_fit <- NA

      tryCatch({
        vennerable_fit <- Vennerable::compute.Venn(vest,
                                                   doWeights = TRUE,
                                                   doEuler = TRUE,
                                                   type = "circles")
        if (isTRUE(class(vennerable_fit) == "VennDrawing")) {
          vennerable_gof <- gof(vennerable_fit, combinations)
        }
      }, error = function(e) {})

      Software <- c(Software, "Vennerable")
      Sets <- c(Sets, i)
      Stress <- c(Stress, vennerable_gof$stress)
      diagError <- c(diagError, vennerable_gof$diag_error)
    }
  }
}

data_accuracy <- tibble(Sets, Software, Stress, diagError) %>%
  mutate(software = as.factor(Software)) %>%
  gather("Metric", "Loss", Stress, diagError)

usethis::use_data(data_accuracy, overwrite = TRUE)



