library(eulerr)
library(venneuler)
library(Vennerable)
library(tidyverse)
library(V8)

set.seed(1)

# Look at consistency for circles first

out <- data.frame(it = integer(),
                  shape = character(),
                  sets = integer(),
                  software = character(),
                  stress = double(),
                  diagError = double())

# JS context for vennjs
context <- v8()
context$source(system.file(file.path("js", "venn.js"), package = "eulerrPaper"))

if (is.null(oldwd)) {
  oldwd <- getwd()
}

setwd(file.path(oldwd, "data-raw"))

n_set <- 8

# Place 3 to 10 circles
for (i in 3:n_set) {
  ids <- eulerr:::bit_indexr(i)

  satisfied <- FALSE
  j <- 1

  while (!satisfied) {
    if (j %% 10 == 0) cat("i=", i,", j=", j, "\n", sep = "")
    # Sample some random circles
    r <- runif(i, 0.3, 0.6)
    x <- runif(i, 0, 1)
    y <- runif(i, 0, 1)

    pars <- as.vector(matrix(c(x, y, r), nrow = 3, byrow = TRUE))

    combinations <- as.vector(eulerr:::intersect_ellipses(pars, circles = TRUE))

    # Avoid extremely small areas
    combinations[abs(combinations) < sqrt(.Machine$double.eps)] <- 0

    for (k in 1:nrow(ids)) {
      names(combinations)[k] <- paste0(LETTERS[1:i][ids[k, ]], collapse = "&")
    }

    # venneuler
    venneuler_fit <- venneuler(combinations)
    venneuler_gof <- gof(venneuler_fit, combinations)
    out <- rbind(out, data.frame(it = j,
                                 shape = "Circles",
                                 sets = i,
                                 software = "venneuler",
                                 stress = venneuler_gof$stress,
                                 diagError = venneuler_gof$diagError))

    # eulerr
    eulerr_fit <- euler(combinations)

    out <- rbind(out, data.frame(it = j,
                                 shape = "Circles",
                                 sets = i,
                                 software = "eulerr",
                                 stress = eulerr_fit$stress,
                                 diagError = eulerr_fit$diagError))

    # vennjs
    input <- setup_vennjs(combinations)
    context$eval(input)
    context$eval("circles = venn.venn(sets)")
    vennjs_fit <- structure(context$get("circles"), class = "vennjs")
    vennjs_gof <- gof(vennjs_fit, combinations)

    out <- rbind(out,
                 data.frame(it = j,
                            shape = "Circles",
                            sets = i,
                            software = "venn.js",
                            stress = vennjs_gof$stress,
                            diagError = vennjs_gof$diagError))

    if (i == 3) {
      # eulerAPE
      if (all(combinations > 0)) {
        input <- paste(combinations, collapse = " | ")
        APEgof <- list(stress = NA, diagError = NA)

        write.table(input, file = "diagram.els", quote = FALSE,
                    col.names = FALSE, row.names = FALSE)

        system2("java", c("-jar eulerAPE_3.0.0.jar",
                          "-i", shQuote("diagram.els"),
                          "--silent",
                          "--curves", "circles"))
        output <- structure(readLines("diagram.eld"), class = "eulerAPE")
        APEgof <- gof(output, combinations)

        out <- rbind(out, data.frame(it = j,
                                     shape = "Circles",
                                     sets = i,
                                     software = "eulerAPE",
                                     stress = APEgof$stress,
                                     diagError = APEgof$diagError))
      }

      # Fit the areas using vennerable if 3 sets
      vest <- Venn(SetNames = LETTERS[1:i], Weight = c(0, as.numeric(combinations)))

      vennerable_gof <- list(stress = NA, diagError = NA)

      tryCatch({
        vennerable_fit <- compute.Venn(vest,
                                       doWeights = TRUE,
                                       doEuler = TRUE,
                                       type = "circles")
        vennerable_gof <- gof(vennerable_fit, combinations)
      }, error = function(e) {})

      out <- rbind(out, data.frame(it = j,
                                   shape = "Circles",
                                   sets = i,
                                   software = "Vennerable",
                                   stress = vennerable_gof$stress,
                                   diagError = vennerable_gof$diagError))
    }

    if (j >= 100) {
      dd <-
        out %>%
        filter(sets == i, shape == "Circles") %>%
        mutate(success = diagError < 0.01) %>%
        select(success, software) %>%
        group_by(software) %>%
        summarise(p = sum(success, na.rm = TRUE)/n(), n = n()) %>%
        mutate(ci = qnorm(0.975)*sqrt(p*(1 - p)/n))

      # Stop when the 95% CI for proportion is smaller than 0.01
      if (all(2*dd$ci < 0.02))
        satisfied <- TRUE
    }
    j <- j + 1
  }
}


# Place 3 to 10 ellipses
for (i in 3:n_set) {
  ids <- eulerr:::bit_indexr(i)

  satisfied <- FALSE
  j <- 1

  while (!satisfied) {
    if (j %% 10 == 0) cat("i=", i,", j=", j, "\n", sep = "")
    # Sample some random ellipses
    a <- runif(i, 0.2, 0.8)
    b <- runif(i, 0.2, 0.8)
    x <- runif(i, 0, 1)
    y <- runif(i, 0, 1)
    phi <- runif(i, 0, 2*pi)

    pars <- as.vector(matrix(c(x, y, a, b, phi), nrow = 5, byrow = TRUE))

    combinations <- as.vector(eulerr:::intersect_ellipses(pars, circles = FALSE))

    combinations[abs(combinations) < sqrt(.Machine$double.eps)] <- 0

    for (k in 1:nrow(ids)) {
      names(combinations)[k] <- paste0(LETTERS[1:i][ids[k, ]], collapse = "&")
    }

    # Fit the areas using eulerr
    eulerr_fit <- euler(combinations, shape = "ellipse")
    out <- rbind(out, data.frame(it = j,
                                 shape = "Ellipses",
                                 sets = i,
                                 software = "eulerr",
                                 stress = eulerr_fit$stress,
                                 diagError = eulerr_fit$diagError))

    if (i == 3) {
      # eulerAPE
      if (all(combinations > 0)) {
        input <- paste(combinations, collapse = " | ")

        write.table(input, file = "diagram.els", quote = FALSE,
                    col.names = FALSE, row.names = FALSE)

        system2("java", c("-jar eulerAPE_3.0.0.jar",
                          "-i", shQuote("diagram.els"),
                          "--silent",
                          "--curves", "ellipses"))
        output <- structure(readLines("diagram.eld"), class = "eulerAPE")
        APEgof <- gof(output, combinations)

        out <- rbind(out, data.frame(it = j,
                                     shape = "Ellipses",
                                     sets = i,
                                     software = "eulerAPE",
                                     stress = APEgof$stress,
                                     diagError = APEgof$diagError))
      }
    }
    if (j >= 100) {
      dd <-
        out %>%
        filter(sets == i, shape == "Ellipses") %>%
        mutate(success = diagError < 0.01) %>%
        select(success, software) %>%
        group_by(software) %>%
        summarise(p = sum(success, na.rm = TRUE)/n(), n = n()) %>%
        mutate(ci = qnorm(0.975)*sqrt(p*(1 - p)/n))

      # Stop when the 95% CI for proportion is smaller than 0.02
      if (all(2*dd$ci < 0.02))
        satisfied <- TRUE
    }

    j <- j + 1
  }
}

setwd(oldwd)

data_consistency <-
  mutate(out,
         it = as.integer(it),
         software = as.factor(software),
         shape = as.factor(shape)) %>%
  gather(metric, loss, diagError, stress, factor_key = TRUE)

if (i == 8 && j >= 100) {
  usethis::use_data(data_consistency, overwrite = TRUE)
}


