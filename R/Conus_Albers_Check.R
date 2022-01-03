#' @title con_conus_albers
#'
#' @description Check projection of a function and convert to BC conus albers
#'
#' @param spat_object file of type spatial polygon, line, or points. Also accepts sf objects
#' @note
#' Author: Sarah Smith-Tripp, November 2021
#' @examples
#'
#' ## points is a data of some data in BC
#'
#' spat_object_albers <- check_proj(points)
#' }
#'
#'
#'
#' @export
#'


#points <- readOGR("D:/SmithTripp/Coding/Proprietary_Figures/BC_Layers_1.shp")

con_conus_albers <- function(spat_object)
{
  #Check file type
  if (class(spat_object) != 'sf') {
    print('warning:function will convert output to sf object')
    spat_object <- st_as_sf(spat_object)
  }
  else {
    spat_object <- spat_object
  }
  if (st_crs(spat_object) == '+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs') {
    print('spatial object in BC Conus albers')
    spat_object_trs <- spat_object
  }
  else if (st_crs(spat_object) !=
           "+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs")
  {
    print("Transforming crs to BC Albers")
    crs <-
      CRS(
        "+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"
      )
    point_trs <- st_transform(spat_object,
                              crs = crs)

  }
}
