library(eulerr)
library(venneuler)
library(VennDiagram)
library(Vennerable)

set.seed(1)

# Look at consistency for circles first

out <- data.frame(it = integer(),
                  sets = integer(),
                  software = character(),
                  stress = double(),
                  diag_error = double())

# Place 3 to 10 circles
for (i in 3:10) {
  for (j in 1:100) {
    # Sample some random circles
    r <- runif(i, 0.3, 0.6)
    x <- runif(i, 0, 1)
    y <- runif(i, 0, 1)

    pars <- as.vector(matrix(c(x, y, r), nrow = 3, byrow = TRUE))

    config <- as.vector(eulerr:::intersect_ellipses(pars, circles = TRUE))
    ids <- eulerr:::bit_indexr(i)

    for (k in 1:nrow(ids)) {
      names(config)[k] <- paste0(LETTERS[1:i][ids[k, ]], collapse = "&")
    }

    out <- rbind(out, cbind(it = j,
                            sets = i,
                            software = "venneuler",
                            stress = venneuler::venneuler(config)$stress,
                            diag_error = 0))
    out <- rbind(out, cbind(it = j,
                            sets = i,
                            software = "eulerr",
                            stress = eulerr::euler(config)$stress,
                            diag_error = 0))
    if (i <= 3) {

    }
  }
}

data_consistency <-
  dplyr::mutate(out, software = as.factor(software), sets = as.factor(sets))

usethis::use_data(data_consistency, overwrite = TRUE)
