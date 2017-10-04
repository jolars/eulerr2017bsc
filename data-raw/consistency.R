library(eulerr)
library(venneuler)
library(Vennerable)
library(tidyverse)

set.seed(1)

# Look at consistency for circles first

out <- data.frame(it = integer(),
                  shape = character(),
                  sets = integer(),
                  software = character(),
                  stress = double(),
                  diagError = double())

oldwd <- getwd() # temporaily set the working directory

setwd(file.path(getwd(), "data-raw"))

n_out <- 100
n_set <- 8

# Place 3 to 10 circles
for (i in 3:8) {
  cat("i=", i, "\n", sep = "")
  ids <- eulerr:::bit_indexr(i)
  for (j in 1:n_out) {
    if (j %% 10 == 0) cat(" j=", j, "\n", sep = "")
    # Sample some random circles
    r <- runif(i, 0.3, 0.6)
    x <- runif(i, 0, 1)
    y <- runif(i, 0, 1)

    pars <- as.vector(matrix(c(x, y, r), nrow = 3, byrow = TRUE))

    config <- as.vector(eulerr:::intersect_ellipses(pars, circles = TRUE))

    # Avoid extremely small areas
    config[abs(config) < sqrt(.Machine$double.eps)] <- 0

    for (k in 1:nrow(ids)) {
      names(config)[k] <- paste0(LETTERS[1:i][ids[k, ]], collapse = "&")
    }

    # Fit the areas using venneuler
    venneuler_fit <- venneuler(config)
    venneuler_gof <- gof(venneuler_fit, config)
    out <- rbind(out, cbind(it = j,
                            shape = "Circles",
                            sets = i,
                            software = "venneuler",
                            stress = venneuler_gof$stress,
                            diagError = venneuler_gof$diagError))

    # Fit the areas using eulerr
    eulerr_fit <- euler(config)

    out <- rbind(out, cbind(it = j,
                            shape = "Circles",
                            sets = i,
                            software = "eulerr",
                            stress = eulerr_fit$stress,
                            diagError = eulerr_fit$diagError))
    if (i == 3) {
      # eulerAPE
      if (all(config > 0)) {
        input <- paste(config, collapse = " | ")
        APEgof <- list(stress = NA, diagError = NA)

        write.table(input, file = "diagram.els", quote = FALSE,
                    col.names = FALSE, row.names = FALSE)

        system2("java", c("-jar eulerAPE_3.0.0.jar",
                          "-i", shQuote("diagram.els"),
                          "--silent",
                          "--curves", "circles"))
        output <- structure(readLines("diagram.eld"), class = "eulerAPE")
        APEgof <- gof(output, config)

        out <- rbind(out, cbind(it = j,
                                shape = "Circles",
                                sets = i,
                                software = "eulerAPE (circles)",
                                stress = APEgof$stress,
                                diagError = APEgof$diagError))
      }

      # Fit the areas using vennerable if 3 sets
      vest <- Venn(SetNames = LETTERS[1:i], Weight = c(0, as.numeric(config)))

      vennerable_gof <- list(stress = NA, diagError = NA)

      tryCatch({
        vennerable_fit <- compute.Venn(vest,
                                       doWeights = TRUE,
                                       doEuler = TRUE,
                                       type = "circles")
        vennerable_gof <- gof(vennerable_fit, config)
      }, error = function(e) {})

      out <- rbind(out, cbind(it = j,
                              shape = "Circles",
                              sets = i,
                              software = "Vennerable",
                              stress = vennerable_gof$stress,
                              diagError = vennerable_gof$diagError))
    }
  }
}


# Place 3 to 10 ellipses
for (i in 3:8) {
  cat("i=", i, "\n", sep = "")
  ids <- eulerr:::bit_indexr(i)
  for (j in 1:n_out) {
    if (j %% 10 == 0) cat(" j=", j, "\n", sep = "")
    # Sample some random ellipses
    a <- runif(i, 0.2, 0.8)
    b <- runif(i, 0.2, 0.8)
    x <- runif(i, 0, 1)
    y <- runif(i, 0, 1)
    phi <- runif(i, 0, 2*pi)

    pars <- as.vector(matrix(c(x, y, a, b, phi), nrow = 5, byrow = TRUE))

    config <- as.vector(eulerr:::intersect_ellipses(pars, circles = FALSE))

    config[abs(config) < sqrt(.Machine$double.eps)] <- 0

    for (k in 1:nrow(ids)) {
      names(config)[k] <- paste0(LETTERS[1:i][ids[k, ]], collapse = "&")
    }

    # Fit the areas using eulerr
    eulerr_fit <- euler(config, shape = "ellipse")
    out <- rbind(out, cbind(it = j,
                            shape = "Ellipses",
                            sets = i,
                            software = "eulerr",
                            stress = eulerr_fit$stress,
                            diagError = eulerr_fit$diagError))

    if (i == 3) {
      # eulerAPE
      if (all(config > 0)) {
        input <- paste(config, collapse = " | ")

        write.table(input, file = "diagram.els", quote = FALSE,
                    col.names = FALSE, row.names = FALSE)

        system2("java", c("-jar eulerAPE_3.0.0.jar",
                          "-i", shQuote("diagram.els"),
                          "--silent",
                          "--curves", "ellipses"))
        output <- structure(readLines("diagram.eld"), class = "eulerAPE")
        APEgof <- gof(output, config)

        out <- rbind(out, cbind(it = j,
                                shape = "Ellipses",
                                sets = i,
                                software = "eulerAPE (ellipses)",
                                stress = APEgof$stress,
                                diagError = APEgof$diagError))
      }
    }
  }
}

setwd(oldwd)

if (i == n_set && j == n_out) {
  data_consistency <-
    mutate(out,
           it = as.integer(it),
           software = as.factor(software),
           stress = as.numeric(as.character(stress)),
           diagError = as.numeric(as.character(diagError)),
           shape = as.factor(shape)) %>%
    gather(metric, loss, diagError, stress) %>%
    mutate(metric = as.factor(metric))

  names(data_consistency$it) <- NULL
  levels(data_consistency$metric) <- c("diagError", "Stress")

  usethis::use_data(data_consistency, overwrite = TRUE)
}


