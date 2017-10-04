#' Consistency data
#'
#' A dataset of comparisons between Euler diagram methods in the
#' reproducibility of diagrams constructed from sampling circles,
#' and thus diagrams that have perfect solutions.
#'
#' \describe{
#'   \item{it}{The iteration. An integer.}
#'   \item{shape}{The shape of the diagram. A factor.}
#'   \item{sets}{The number of sets. A factor (integer) variable.}
#'   \item{software}{The software used in the observation. A factor.}
#'   \item{metric}{The loss metric. A factor.}
#'   \item{loss}{The loss of the function. A double vector.}
#' }
#'
#' @author Johan Larsson
#' @format A data frame with 1800 obs of 4 variables.
#' @usage data(data_consistency)
#'
"data_consistency"

#' Accuracy data
#'
#' A dataset of comparisons between venneuler and eulerr in terms of
#' accuracy of diagrams constructed from sampling set intersections
#'
#' \describe{
#'   \item{Sets}{A factor of the number of sets used in the diagram}
#'   \item{it}{an integer variable of the number of the iteration}
#'   \item{Metric}{A factor of the type of loss (diagError or stress)}
#'   \item{loss}{The loss of the solution}
#' }
#'
#' @author Johan Larsson
#' @format A data frame with 1800 obs of 4 variables.
#' @usage data(data_accuracy)
#'
"data_accuracy"

#' Performance data
#'
#' A dataset of comparisons between venneuler and eulerr in terms of
#' accuracy of diagrams constructed from sampling set intersections.
#'
#' \describe{
#'   \item{Sets}{The number of sets in the diagram}
#'   \item{Software}{The software (method) used. A factor variable.}
#'   \item{Time}{The computing speed in nanoseconds.}
#' }
#' * Sets. A factor of the number of sets used in the diagram
#' * it. An integer variable of the number of the iteration
#' * software. A factor with two levels, signifying which software (eulerr or
#' venneuler) that was used.
#' * stress. A double of the stress metric from venneuler.
#'
#' @author Johan Larsson
#' @format A data frame with 1800 obs of 4 variables.
#' @usage data(data_accuracy)
#'
"data_performance"
