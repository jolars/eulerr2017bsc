#' Setup input for vennjs fitting
#'
#' @param combinations Disjoint set relationships
#'
#' @return A character vector of set relationships that venn.js can work with.
#' @export
setup_vennjs <- function(combinations) {
  combo_names <- strsplit(names(combinations), split = "&", fixed = TRUE)
  setnames <- unique(unlist(combo_names, use.names = FALSE))

  n <- length(setnames)
  id <- eulerr:::bit_indexr(n)
  N <- NROW(id)

  areas <- double(N)
  for (i in 1L:N) {
    s <- setnames[id[i, ]]
    for (j in seq_along(combo_names)) {
      if (setequal(s, combo_names[[j]])) {
        areas[i] <- combinations[j]
      }
    }
  }

  areas_disjoint <- areas
  areas[] <- 0
  for (i in rev(seq_along(areas))) {
    prev_areas <- rowSums(id[, id[i, ], drop = FALSE]) == sum(id[i, ])
    areas[i] <- sum(areas_disjoint[prev_areas])
  }

  cnames <- apply(id, 1L, function(x) (setnames[x]))

  input <- character(0)
  for (i in 1:N) {
    cmbs <- paste0("'", cnames[[i]], "'", collapse = ",")

    input <- paste0(input,
                    (paste0("{sets: [",
                            cmbs,
                            "], ",
                            "size: ",
                            areas[[i]],
                            "}",
                            if (i == N) "" else ", ")))
  }

  paste0("var sets = [ ", input, " ];")
}


#' Hypothesis test of two-sample proportions
#'
#' @param p0 proportion of sample 1
#' @param p1 proportion of sample 2
#' @param n0 sample size of sample 1
#' @param n1 sample size of sample 2
#'
#' @return a p-value
#' @export
prop_test <- function(p0, p1, n0, n1) {
  if (p0 > 1 | p1 > 1)
    stop("proportions may not exceed 1")

  z <- (p0 - p1)/sqrt(p0*(1 - p0)/n0 + p1*(1 - p1)/n1)
  p <- 2*stats::pnorm(-abs(z))
  p
}
