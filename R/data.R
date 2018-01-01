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
#' @format A data frame with 226336 obs of 6 variables.
#' @usage data(data_consistency)
"data_consistency"

#' Accuracy data
#'
#' A dataset of comparisons between software in terms of
#' accuracy of diagrams constructed from sampling set intersections
#'
#' \describe{
#'   \item{it}{an integer variable of the number of the iteration}
#'   \item{Sets}{A factor of the number of sets used in the diagram}
#'   \item{software}{A factor of the software package}
#'   \item{Metric}{A factor of the type of loss (diagError or stress)}
#'   \item{Loss}{The loss of the solution}
#' }
#'
#' @author Johan Larsson
#' @format A data frame with 48000 obs of 5 variables.
#' @usage data(data_accuracy)
"data_accuracy"

#' Performance data
#'
#' A dataset of comparisons between software packages in terms of
#' computational performance in generating
#' diagrams constructed from sampling set intersections.
#'
#' \describe{
#'   \item{it}{an integer variable of the number of the iteration}
#'   \item{software}{The software (method) used. A factor variable.}
#'   \item{Sets}{The number of sets in the diagram}
#'   \item{Time}{The computing speed in nanoseconds.}
#' }
#'
#' @author Johan Larsson
#' @format A data frame with 18720 obs of 4 variables.
#' @usage data(data_accuracy)
"data_performance"
