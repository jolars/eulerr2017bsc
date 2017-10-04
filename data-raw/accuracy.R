library(tidyverse)
library(eulerr)
library(venneuler)
library(Vennerable)

set.seed(1)

Software <- character(0)
Stress <- double(0)
diagError <- double(0)
Sets <- integer(0)

n_out <- 100
n_set <- 8

oldwd <- getwd() # temporaily set the working directory

setwd(file.path(getwd(), "data-raw"))

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

    how_many <- sample(sum(combinations == 0), 1)

    combinations[combinations == 0][sample(how_many)] <- runif(how_many)

    eulerr_circles <- euler(combinations)
    eulerr_ellipses <- euler(combinations, shape = "ellipse")

    venneuler_fit <- venneuler(combinations)
    venneuler_gof <- gof(venneuler_fit, combinations)

    Software <- c(Software, "eulerr (circles)", "eulerr (ellipses)",
                  "venneuler")
    Sets <- c(Sets, rep(i, 3))
    Stress <- c(Stress,
                eulerr_circles$stress,
                eulerr_ellipses$stress,
                venneuler_gof$stress)
    diagError <- c(diagError,
                   eulerr_circles$diagError,
                   eulerr_ellipses$diagError,
                   venneuler_gof$diagError)

    if (i == 3) {
      # eulerAPE
      if (all(combinations > 0)) {
        input <- paste(combinations, collapse = " | ")

        write.table(input, file = "diagram.els", quote = FALSE,
                    col.names = FALSE, row.names = FALSE)

        APEgof <- list("circles" = list(), "ellipses" = list())

        for (shape in c("circles", "ellipses")) {
          system2("java", c("-jar eulerAPE_3.0.0.jar",
                            "-i", shQuote("diagram.els"),
                            "--silent",
                            "--curves", shape))
          output <- structure(readLines("diagram.eld"), class = "eulerAPE")
          APEgof[[shape]] <- gof(output, combinations)
        }

        Software <- c(Software, "eulerAPE (circles)", "eulerAPE (ellipses)")
        Sets <- c(Sets, rep(i, 2))
        Stress <- c(Stress, APEgof$circles$stress, APEgof$ellipses$stress)
        diagError <- c(diagError, APEgof$circles$diagError, APEgof$ellipses$diagError)
      }

      # Vennerable
      vest <- Vennerable::Venn(SetNames = LETTERS[1:i],
                               Weight = c(0, as.numeric(combinations)))

      vennerable_gof <- list(stress = NA, diagError = NA)
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
      diagError <- c(diagError, vennerable_gof$diagError)
    }
  }
}

setwd(oldwd)

if (i == n_set && j == n_out) {
data_accuracy <- tibble(Sets, Software, Stress, diagError) %>%
  mutate(Software = as.factor(Software)) %>%
  gather("Metric", "Loss", Stress, diagError)

usethis::use_data(data_accuracy, overwrite = TRUE)
}
