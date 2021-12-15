#' @title This function finds maximum distance between provided coordinates and listed object's coordinates
#'
#' @description
#' The \code{find.max.dist()} function analyzes touristic attractions coordinates and identifies two records
#' whith the longest distance
#'
#' @param The \code{find.max.dist()} function is called with data frame argument,
#' which contains a set of touristic attractions coordinates.
#'
#' @return The \code{find.max.dist()} function returns a data frame with two rows: the first row contains coordinates provided by user
#' and the second row contains coordinates for object with the longest distance from the first one.
#'
#' @details The \code{find.max.dist()} function is called with data frame argument
#' that should contain at least the following columns:
#' LAT, LON, LECZENIE, DATETIME, WOJEWODZTWO
#'
#' The result of the \code{find.max.dist()} is a data frame with two rows
#' containing two AIS signal observations
#' and with the longest distance between two consecutive observations reported
#' in new column named DIST.
#' Consequently, the result contains the following columns:
#' LAT, LON, DIST, LECZENIE, WOJEWODZTWO
#'
#'
#' Please find below short description of AIS signal columns:
#'
#' LAT - ship’s latitude
#'
#' LON - ship’s longitude
#'
#' DIST - calculated the longest distance between two consecutive observations
#'
#' LECZENIE - touristic object’s name
#'
#' WOJEWODZTWO - touristic object’s province (voivodeship)
#'
#' @author
#' Dariusz Ekonomiuk, \email{ekodarek@gmail.com}
#'
#' @examples
#' \dontrun{
#' find.max.dist(history_monuments)
#' }
#'
#' @rdname find.max.dist
#' @importFrom hans haversine
#' @export
find.max.dist <- function(touristic_data, lat_my, lon_my) {
  max_dist_square <- 0
#  la_prev <- as.double(touristic_data[1,"LAT"])
#  lo_prev <- as.double(touristic_data[1,"LON"])
#  LECZENIE_prev <- touristic_data[1,"LECZENIE"]
#  wojew_prev <- touristic_data[1,"WOJEWODZTWO"]
  #   datetime_prev <- touristic_data[1,"DATETIME"]
  for (i in 1:nrow(touristic_data)) {
    #   for(i in 2:14){
    la_now <- as.double(touristic_data[i,"LAT"])
    lo_now <- as.double(touristic_data[i,"LON"])
    LECZENIE_now <- touristic_data[i,"LECZENIE"]
    wojew_now <- touristic_data[i,"WOJEWODZTWO"]
    miejsc_now <- touristic_data[i,"MIEJSCOWOSC"]
    #      datetime_now <- touristic_data[i,"DATETIME"]
    d1 <- la_now - lat_my
    d2 <- lo_now - lon_my
    dist_square <- d1*d1 + d2*d2
    #      print(dist_square)
    if (dist_square >= max_dist_square) {
      max_dist_square <- dist_square
#      max_dist_la_prev <- la_prev
#      max_dist_lo_prev <- lo_prev
      max_dist_la_now <- la_now
      max_dist_lo_now <- lo_now
#      max_dist_LECZENIE_prev <- LECZENIE_prev
      max_dist_LECZENIE_now <- LECZENIE_now
#      max_dist_wojew_prev <- wojew_prev
      max_dist_wojew_now <- wojew_now
      max_dist_miejsc_now <- miejsc_now
      #         max_dist_datetime_prev <- datetime_prev
      #         max_dist_datetime_now <- datetime_now
    }
#    la_prev <- la_now
#    lo_prev <- lo_now
    #      datetime_prev <- datetime_now
  }
  #   print(max_dist_square)
  #   max_dist_meters <- haversine(lat1, lon1, lat2, lon2)
#  max_dist_meters <- haversine(max_dist_la_prev, max_dist_lo_prev,
#                               max_dist_la_now, max_dist_lo_now)*1000
max_dist_meters <- haversine(lat_my, lon_my,
                             max_dist_la_now, max_dist_lo_now)*1000
  max_dist_meters <- round(max_dist_meters, 1)
  LAT <- c(lat_my, max_dist_la_now)
  LON <- c(lon_my, max_dist_lo_now)
  DIST <- c(max_dist_meters, max_dist_meters)
  LECZENIE <- c(max_dist_LECZENIE_now, max_dist_LECZENIE_now)
  MIEJSC <- c(max_dist_miejsc_now, max_dist_miejsc_now)
  #   DATETIME <- c(as.character(max_dist_datetime_prev),
  #                 as.character(max_dist_datetime_now))
  WOJEWODZTWO <- c(max_dist_wojew_now, max_dist_wojew_now)
  df_max_dist <- data.frame(LAT, LON, DIST, LECZENIE, MIEJSC, WOJEWODZTWO)
  # df_max_dist <- data.frame(LAT, LON, DIST, LECZENIE, DATETIME, WOJEWODZTWO)
  #  print(df_max_dist)
  #   sprintf("Max distance: %.1f (meters) for coordinates: %f %f and %f %f",
  #           max_dist_meters, max_dist_la_prev,
  #           max_dist_lo_prev, max_dist_la_now, max_dist_lo_now)
  return(df_max_dist)
}


