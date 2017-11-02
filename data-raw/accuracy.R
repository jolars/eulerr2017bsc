library(tidyverse)
library(eulerr)
library(eulerrPaper)
library(venneuler)
library(Vennerable)
library(V8)

out <- data.frame(it = integer(),
                  sets = integer(),
                  software = character(),
                  stress = double(),
                  diagError = double())

n_set <- 8

# JS context for vennjs
context <- v8()
context$source(system.file(file.path("js", "venn.js"), package = "eulerrPaper"))

if (.Platform$OS.type == "windows") {
  if (is.null(oldwd)) {
    oldwd <- getwd() # temporaily set the working directory
  }
  setwd(file.path(oldwd, "data-raw"))
}

for (i in 3:n_set) {
  cat("i=", i, "\n", sep = "")
  ids <- eulerr:::bit_indexr(i)

  satisfied <- FALSE
  j <- 1

  while (!satisfied) {
    set.seed(i*j)

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

    # eulerr
    eulerr_circles <- euler(combinations)
    eulerr_ellipses <- euler(combinations, shape = "ellipse")
    out <- rbind(
      out,
      data.frame(
        it = j,
        sets = i,
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
                            sets = i,
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
                                 sets = i,
                                 software = "venn.js",
                                 stress = vennjs_gof$stress,
                                 diagError = vennjs_gof$diagError))

    if (i == 3) {
      # eulerAPE
      if (all(combinations > 0) &&  .Platform$OS.type == "windows") {
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
            sets = i,
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
                                   sets = i,
                                   software = "Vennerable",
                                   stress = vennerable_gof$stress,
                                   diagError = vennerable_gof$diagError))
    }

    if (j >= 100) { # Run at least 100 iterations
      dd <- filter(out, sets == i) %>%
        group_by(software) %>%
        summarise(ci = qt(0.975, df = n() - 1)*sd(diagError,
                                                  na.rm = TRUE)/sqrt(n()))

      # Stop when the 95% CI for each estimate is smaller than 1% in diagError
      if (all(dd$ci*2 < 0.01) && j >= 500)
        satisfied <- TRUE
    }

    j <- j + 1
  }
}

if (.Platform$OS.type == "windows")
  setwd(oldwd)

data_accuracy <- out %>%
  rename(Sets = sets) %>%
  mutate(software = as.factor(software), it = as.integer(it)) %>%
  gather("Metric", "Loss", stress, diagError, factor_key = TRUE)

if (i == 8 && j >= 500) {
  usethis::use_data(data_accuracy, overwrite = TRUE)
}
