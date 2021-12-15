#' @title This function finds all distances between provided coordinates and listed object's coordinates
#'
#' @description
#' The \code{find.all.dist()} function calculates all distances between the coordinates provided as input and all listed coordinates.
#' The function uses haversine function for distance calculations.
#'
#' @param The \code{find.all.dist()} function is called with data frame argument,
#' which contains a set of listed objects coordinates.
#'
#' @return The \code{find.all.dist()} function returns a data frame with two rows,
#' which contain two LAT LON coordinates with the longest distance.
#'
#' @details The \code{find.all.dist()} function is called with data frame argument
#' that should contain at least the following columns:
#' LAT, LON, LECZENIE, DATETIME, WOJEWODZTWO
#'
#' The result of the \code{find.all.dist()} is a data frame with two rows
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
#' find.all.dist(history_monuments)
#' }
#'
#' @rdname find.all.dist
#' @importFrom hans haversine
#' @export
find.all.dist <- function(touristic_data, lat_my, lon_my) {
#  all_dist_square <- 1000000
  # declare an empty data frame
#  df_all_dist <- data.frame(matrix(ncol = 6, nrow = 0))
#  colnames(df_all_dist) <- c('LAT', 'LON', 'DIST',
#                             'LECZENIE', 'MIEJSC', 'WOJEWODZTWO')
  df_all_dist <- data.frame(LAT=double(),
                            LON=double(),
                            DIST=double(),
                            LECZENIE=character(),
                            MIEJSC=character(),
                            WOJEWODZTWO=character(),
                            stringsAsFactors = FALSE)

  for (i in 1:nrow(touristic_data)) {
    #   for(i in 2:14){
    la_now <- as.double(touristic_data[i,"LAT"])
    lo_now <- as.double(touristic_data[i,"LON"])
    LECZENIE_now <- touristic_data[i,"LECZENIE"]
    miejsc_now <- touristic_data[i,"MIEJSCOWOSC"]
    wojew_now <- touristic_data[i,"WOJEWODZTWO"]
    d1 <- la_now - lat_my
    d2 <- lo_now - lon_my
    dist_square <- d1*d1 + d2*d2
  #   all_dist_meters <- haversine(lat1, lon1, lat2, lon2)
    all_dist_meters <- haversine(lat_my, lon_my,
                                 la_now, lo_now)*1000
    all_dist_meters <- round(all_dist_meters, 1)
    df_all_dist <- rbind(df_all_dist, c(la_now, lo_now, all_dist_meters,
                                        LECZENIE_now, miejsc_now, wojew_now))
  #   sprintf("all distance: %.1f (meters) for coordinates: %f %f and %f %f",
  }
  colnames(df_all_dist) <- c('LAT', 'LON', 'DIST',
                             'LECZENIE', 'MIEJSC', 'WOJEWODZTWO')
  return(df_all_dist)
}


