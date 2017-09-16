library(eulerr)
library(venneuler)
library(VennDiagram)
library(Vennerable)
library(magrittr)

set.seed(1)

# Look at consistency for circles first

out <- data.frame(it = integer(),
                  shape = character(),
                  sets = integer(),
                  software = character(),
                  stress = double(),
                  diag_error = double())

# Place 3 to 10 circles
for (i in 3:8) {
  ids <- eulerr:::bit_indexr(i)
  for (j in 1:100) {
    if (j %% 10 == 0) cat("i=", i, "; j=", j, "\n", sep = "")
    # Sample some random circles
    r <- runif(i, 0.3, 0.6)
    x <- runif(i, 0, 1)
    y <- runif(i, 0, 1)

    pars <- as.vector(matrix(c(x, y, r), nrow = 3, byrow = TRUE))

    config <- as.vector(eulerr:::intersect_ellipses(pars, circles = TRUE))

    for (k in 1:nrow(ids)) {
      names(config)[k] <- paste0(LETTERS[1:i][ids[k, ]], collapse = "&")
    }

    # Fit the areas using venneuler
    venneuler_fit <- venneuler::venneuler(config)
    venneuler_gof <- gof(venneuler_fit, config)
    out <- rbind(out, cbind(it = j,
                            shape = "circle",
                            sets = i,
                            software = "venneuler",
                            stress = venneuler_gof$stress,
                            diag_error = venneuler_gof$diag_error))

    # Fit the areas using eulerr
    eulerr_fit <- eulerr::euler(config)
    out <- rbind(out, cbind(it = j,
                            shape = "circle",
                            sets = i,
                            software = "eulerr",
                            stress = eulerr_fit$stress,
                            diag_error = eulerr_fit$diag_error))
    if (i == 3) {
      # Fit the areas using vennerable if 3 sets
      vest <- Vennerable::Venn(SetNames = LETTERS[1:i],
                               Weight = c(0, as.numeric(config)))

      vennerable_gof <- list(stress = NA, diag_error = NA)

      tryCatch({
        vennerable_fit <- Vennerable::compute.Venn(vest,
                                                   doWeights = TRUE,
                                                   doEuler = TRUE,
                                                   type = "circles")
        vennerable_gof <- gof(vennerable_fit, config)
      }, error = function(e) {})

      out <- rbind(out, cbind(it = j,
                              shape = "circle",
                              sets = i,
                              software = "Vennerable",
                              stress = vennerable_gof$stress,
                              diag_error = vennerable_gof$diag_error))
    }
  }
}


# Place 3 to 10 ellipses
for (i in 3:8) {
  ids <- eulerr:::bit_indexr(i)
  for (j in 1:100) {
    if (j %% 10 == 0) cat("i = ", i, "; j = ", j, "\n")
    # Sample some random ellipses
    a <- runif(i, 0.2, 0.8)
    b <- runif(i, 0.2, 0.8)
    x <- runif(i, 0, 1)
    y <- runif(i, 0, 1)
    phi <- runif(i, 0, 2*pi)

    pars <- as.vector(matrix(c(x, y, a, b, phi), nrow = 5, byrow = TRUE))

    config <- as.vector(eulerr:::intersect_ellipses(pars, circles = FALSE))

    for (k in 1:nrow(ids)) {
      names(config)[k] <- paste0(LETTERS[1:i][ids[k, ]], collapse = "&")
    }

    # Fit the areas using eulerr
    eulerr_fit <- eulerr::euler(config, shape = "ellipse")
    out <- rbind(out, cbind(it = j,
                            shape = "ellipse",
                            sets = i,
                            software = "eulerr",
                            stress = eulerr_fit$stress,
                            diag_error = eulerr_fit$diag_error))
  }
}

data_consistency <-
  dplyr::mutate(out,
                it = as.integer(it),
                software = as.factor(software),
                stress = as.numeric(as.character(stress)),
                diag_error = as.numeric(as.character(diag_error)),
                shape = as.factor(shape)) %>%
  tidyr::gather(metric, loss, diag_error, stress) %>%
  dplyr::mutate(metric = as.factor(metric))

names(data_consistency$it) <- NULL
levels(data_consistency$metric) <- c("diagError", "Stress")

usethis::use_data(data_consistency, overwrite = TRUE)
