#' CoordConvert
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
CoordConvert <- function(CrimeData,
                         Shapefile,
                         Zipcode){

  Shapefile <-  Shapefile |>
    dplyr::filter(ZCTA5CE20 == Zipcode)

  CrimeData <- sf::st_as_sf(CrimeData,
                          coords = c('Lon', 'Lat'),
                          crs = sf::st_crs(Shapefile))

  CrimeData <- CrimeData |>
    sf::st_join(Shapefile,
            join = sf::st_intersects) |>
    dplyr::filter(!is.na(ZCTA5CE20)) |>
    dplyr::select(-c(Population)) |>
    dplyr::select(ZCTA5CE20,
                  dplyr::everything())

  return(CrimeData)

}
