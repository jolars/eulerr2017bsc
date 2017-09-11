library(eulerr)
library(venneuler)
# library(VennDiagram)

set.seed(1)


# Look at consistency for circles first

out <- data.frame(venneuler = double(),
                  eulerr = double(),
                  sets = integer(),
                  it = integer())

# Place 2 to 10 sets
for (i in 2:10) {
  for (j in 1:100) {
    r <- runif(i, 0.3, 0.6)
    x <- runif(i, 0, 1)
    y <- runif(i, 0, 1)

    pars <- as.vector(matrix(c(x, y, r), nrow = 3, byrow = TRUE))

    config <- as.vector(eulerr:::intersect_ellipses(pars, circles = TRUE))
    ids <- eulerr:::bit_indexr(i)

    for (k in 1:nrow(ids)) {
      names(config)[k] <- paste0(LETTERS[1:i][ids[k, ]], collapse = "&")
    }

    s1 <- venneuler::venneuler(config)$stress
    s2 <- eulerr::euler(config)$stress

    out <- rbind(out, cbind(venneuler = s1, eulerr = s2, sets = i, it = j))
  }
}

data_consistency <-
  tidyr::gather(out, key = "software", "stress", -sets, -it) %>%
  dplyr::mutate(software = as.factor(software),
                sets = as.factor(sets))
levels(data_consistency$sets) <- paste(levels(data_consistency$sets), "sets")

usethis::use_data(data_consistency, overwrite = TRUE)
