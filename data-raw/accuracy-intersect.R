# Accuracy tests for 3 sets that all intersect ----------------------------

library(tidyverse)
library(eulerr)
library(eulerr2017bsc)
library(venneuler)
library(Vennerable)
library(V8)

out <- data.frame(it = integer(),
                  software = character(),
                  stress = double(),
                  diagError = double())

# JS context for vennjs
context <- v8()
context$source(system.file(file.path("js", "venn.js"), package = "eulerr2017bsc"))

oldwd <- getwd() # temporaily set the working directory
setwd(file.path(oldwd, "data-raw"))

ids <- eulerr:::bit_indexr(3)

satisfied <- FALSE
j <- 1L
n_it <- 1000L

while (!satisfied) {
  set.seed(j)
  if (j %% 10 == 0) cat("j=", j, "\n", sep = "")

  combinations <- runif(7, sqrt(.Machine$double.eps), 1)

  for (k in 1:NROW(ids)) {
    names(combinations)[k] <- paste0(LETTERS[1:3][ids[k, ]], collapse = "&")
  }

  # eulerr
  eulerr_circles <- euler(combinations)
  eulerr_ellipses <- euler(combinations, shape = "ellipse")
  out <- rbind(
    out,
    data.frame(
      it = j,
      software = rep(c("eulerr (circles)", "eulerr (ellipses)"), each = 2),
      Loss = c(eulerr_circles$stress, eulerr_circles$diagError,
               eulerr_ellipses$stress, eulerr_ellipses$diagError),
      Metric = c("stress", "diagError")
    )
  )

  # venneuler
  venneuler_fit <- venneuler(combinations)
  venneuler_gof <- gof(venneuler_fit, combinations)
  out <- rbind(out,
               data.frame(it = j,
                          software = "venneuler",
                          Loss = c(venneuler_gof$stress,
                                   venneuler_gof$diagError),
                          Metric = c("stress", "diagError")))

  # vennjs
  input <- setup_vennjs(combinations)
  context$eval(input)
  context$eval("circles = venn.venn(sets)")
  vennjs_fit <- structure(context$get("circles"), class = "vennjs")
  vennjs_gof <- gof(vennjs_fit, combinations)

  out <- rbind(out, data.frame(it = j,
                               software = "venn.js",
                               Loss = c(vennjs_gof$stress,
                                        vennjs_gof$diagError),
                               Metric = c("stress", "diagError")))
  # eulerAPE
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

  out <- rbind(
    out,
    data.frame(
      it = j,
      software = rep(c("eulerAPE (circles)", "eulerAPE (ellipses)"), each = 2),
      Loss = c(APEgof$circles$stress, APEgof$circles$diagError,
               APEgof$ellipses$stress, APEgof$ellipses$diagError),
      Metric = c("stress", "diagError")
    )
  )

  # Vennerable
  vest <- Vennerable::Venn(SetNames = LETTERS[seq_len(3)],
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

  out <- rbind(out, data.frame(it = j,
                               software = "Vennerable",
                               Loss = c(vennerable_gof$stress,
                                        vennerable_gof$diagError),
                               Metric = c("stress", "diagError")))

  if (j >= 100) {
    dd <- out %>%
      filter(Metric == "diagError") %>%
      group_by(software) %>%
      summarise(ci = qt(0.975, df = n() - 1)*sd(Loss,
                                                na.rm = TRUE)/sqrt(n()))
    if (j %% 100 == 0)
      print(dd)

    # Stop when the 95% CI for each estimate is smaller than 1% in diagError
    if (all(dd$ci*2 < 0.01) && j >= n_it)
      satisfied <- TRUE
  }
  j <- j + 1L
}

setwd(oldwd)

data_accuracy_int <- out %>%
  mutate(it = as.integer(it))

devtools::use_data(data_accuracy_int, overwrite = TRUE)
