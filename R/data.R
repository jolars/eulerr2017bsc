#' Consistency data
#'
#' A dataset of comparisons between venneuler and eulerr in terms of
#' reproducibility of diagrams constructed from sampling circles,
#' and thus diagrams that have perfect solutions.
#'
#'
#'#' \describe{
#'   \item{One}{First item}
#'   \item{Two}{Second item}
#' }
#' * Sets. A factor of the number of sets used in the diagram
#' * it. An integer variable of the number of the iteration
#' * software. A factor with two levels, signifying which software (eulerr or
#' venneuler) that was used.
#' * stress. A double of the stress metric from venneuler.
#'
#' @author Johan Larsson
#' @format A data frame with 1800 obs of 4 variables.
#' @usage data(data_consistency)
#'
"data_consistency"
