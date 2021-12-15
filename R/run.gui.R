#' @title  Run GUI (shiny app) with leaflet map visualization.
#'
#' @description The \code{run.gui()} function executes shiny.
#'
#' @param The \code{run.gui()} function is called without arguments.
#'
#'
#' @details Run this function and you will observe visualization
#' of your location and the closest SPA or ex-SPA on leaflet map and the distance between these two points will be calculated.
#'
#'
#' @author
#' Dariusz Ekonomiuk, \email{ekodarek@gmail.com}
#'
#'
#' @rdname run.gui
#' @import shiny
#' @export
run.gui <- function() {
  shiny::runApp("inst/server_spa")
#  shiny::runApp(system.file("inst/server_spa/", package = "spa.monitor"))
}


