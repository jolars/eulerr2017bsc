
# Accuracy tests for 3 sets that all intersect ----------------------------

library(tidyverse)
library(eulerr)
library(eulerrPaper)
library(venneuler)
library(Vennerable)
library(V8)

out <- data.frame(it = integer(),
                  software = character(),
                  stress = double(),
                  diagError = double())

# JS context for vennjs
context <- v8()
context$source(system.file(file.path("js", "venn.js"), package = "eulerrPaper"))

if (.Platform$OS.type == "windows") {
  if (!exists("oldwd")) {
    oldwd <- getwd() # temporaily set the working directory
    setwd(file.path(oldwd, "data-raw"))
  }
}

i <- 3

ids <- eulerr:::bit_indexr(i)

satisfied <- FALSE
j <- 1

while (!satisfied) {
  set.seed(j)
  if (j %% 10 == 0) cat("i=", i,", j=", j, "\n", sep = "")

  combinations <- runif(2^i - 1, sqrt(.Machine$double.eps), 1)

  for (k in 1:NROW(ids)) {
    names(combinations)[k] <- paste0(LETTERS[1:i][ids[k, ]], collapse = "&")
  }

  # eulerr
  eulerr_circles <- euler(combinations)
  eulerr_ellipses <- euler(combinations, shape = "ellipse")
  out <- rbind(
    out,
    data.frame(
      it = j,
      software = c("eulerr (circles)", "eulerr (ellipses)"),
      stress = c(eulerr_circles$stress, eulerr_ellipses$stress),
      diagError = c(eulerr_circles$diagError, eulerr_ellipses$diagError)
    )
  )

  # venneuler
  venneuler_fit <- venneuler(combinations)
  venneuler_gof <- gof(venneuler_fit, combinations)
  out <- rbind(out,
               data.frame(it = j,
                          software = "venneuler",
                          stress = venneuler_gof$stress,
                          diagError = venneuler_gof$diagError))

  # vennjs
  input <- setup_vennjs(combinations)
  context$eval(input)
  context$eval("circles = venn.venn(sets)")
  vennjs_fit <- structure(context$get("circles"), class = "vennjs")
  vennjs_gof <- gof(vennjs_fit, combinations)

  out <- rbind(out, data.frame(it = j,
                               software = "venn.js",
                               stress = vennjs_gof$stress,
                               diagError = vennjs_gof$diagError))
  # eulerAPE
  if (.Platform$OS.type == "windows") {
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
        software = c("eulerAPE (circles)", "eulerAPE (ellipses)"),
        stress = c(APEgof$circles$stress, APEgof$ellipses$stress),
        diagError = c(APEgof$circles$diagError, APEgof$ellipses$diagError)
      )
    )
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

  out <- rbind(out, data.frame(it = j,
                               software = "Vennerable",
                               stress = vennerable_gof$stress,
                               diagError = vennerable_gof$diagError))

  if (j >= 100) {
    dd <- out %>%
      group_by(software) %>%
      summarise(ci = qt(0.975, df = n() - 1)*sd(diagError,
                                                na.rm = TRUE)/sqrt(n()))
    if (j %% 100 == 0)
      print(dd)

    # Stop when the 95% CI for each estimate is smaller than 1% in diagError
    if (all(dd$ci*2 < 0.01) && j >= 500)
      satisfied <- TRUE
  }
  j <- j + 1
}


if (.Platform$OS.type == "windows" && exists("oldwd"))
  setwd(oldwd)

data_accuracy_int <- out %>%
  mutate(it = as.integer(it)) %>%
  gather("Metric", "Loss", stress, diagError, factor_key = TRUE)

# devtools::use_data(data_accuracy_int, overwrite = TRUE)
