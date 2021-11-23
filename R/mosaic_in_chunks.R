#' @title Mosaic small tiles to larger ones
#'
#' @description Mosaic raster tiles into chunks using arcmap MosaicToNewRaster and \link[reticulate]{import}
#'
#' @param basedir Character String. Path to input files
#' @param odir Character String. Path to output directory (Will be created if it doesnt exist)
#' @param file_type Character String. File identifier (e.g. ".tif" / "avg.tif")
#' @param idx Character String. String to append to output mosaics
#' @param chunk_size Number of files per chunk (defaults to all)
#' @param out_file_type Character String. Output file type e.g. ".tif" / ".bil" see
#' https://desktop.arcgis.com/en/arcmap/10.3/tools/data-management-toolbox/mosaic-to-new-raster.htm
#' @param pixel_type default: \code{"32_BIT_FLOAT"} - see
#' \href{https://desktop.arcgis.com/en/arcmap/10.3/tools/data-management-toolbox/mosaic-to-new-raster.htm}{MosaictoNewRaster}
#' @param cellsize default: \code{""} - see
#' \href{https://desktop.arcgis.com/en/arcmap/10.3/tools/data-management-toolbox/mosaic-to-new-raster.htm}{MosaictoNewRaster}
#' @param number_of_bands default: \code{1} - see
#' \href{https://desktop.arcgis.com/en/arcmap/10.3/tools/data-management-toolbox/mosaic-to-new-raster.htm}{MosaictoNewRaster}
#' @param mosaic_method default: \code{"MEAN"} - see
#' \href{https://desktop.arcgis.com/en/arcmap/10.3/tools/data-management-toolbox/mosaic-to-new-raster.htm}{MosaictoNewRaster}
#' @param mosaic_colormap_mode default: \code{"FIRST"} - see
#' \href{https://desktop.arcgis.com/en/arcmap/10.3/tools/data-management-toolbox/mosaic-to-new-raster.htm}{MosaictoNewRaster}
#'
#' @note
#' Mosaics files in \code{basedir} using mosaic to new raster functionality in arcmap
#'
#'
#' @author Tristan R.H. Goodbody
#'
#' @examples
#' \dontrun{
#'
#' #--- define path of ARCGIS python on your PC ---#
#' arcpy3_dir <- 'C:/Program Files/ArcGIS/Pro/bin/Python'
#'
#' #--- specify location of python for reticulate ---#
#' use_condaenv(condaenv = file.path(arcpy3_dir, 'envs', 'arcgispro-py3'),
#'              conda    = file.path(arcpy3_dir, 'Scripts' ,'conda.exe'),
#'              required = TRUE)
#'
#' arcpy <- reticulate::import("arcpy")
#'
#' mosaic_in_chunks(basedir = "F:/_2021_SSCALS/03_raster/dtm",
#'                  odir = "F:/_2021_SSCALS/03_raster/dtm/mosaics",
#'                  file_type = ".bil",
#'                  idx = "dtm",
#'                  chunk_size = 1000)
#'
#'
#' }
#'
#' @export

mosaic_in_chunks <- function(basedir,
                             odir,
                             file_type,
                             idx = "",
                             chunk_size = NULL,
                             out_file_type = ".tif",
                             pixel_type = "32_BIT_FLOAT",
                             cellsize = "",
                             number_of_bands = 1,
                             mosaic_method = "MEAN",
                             mosaic_colormap_mode = "FIRST"){

  arcpy <- counter <- NULL

  if(!is.character(basedir)){
    stop("basedir needs to be a character path")
  }

  if(!dir.exists(basedir)){
    stop("basedir doesn't exist")
  }

  if(!is.character(odir)){
    stop("basedir needs to be a character path")
  }

  if(!is.character(file_type)){
    stop("file_type needs to be a character string ")
  }

  if(!is.character(idx)){
    stop("idx needs to be a character string ")
  }

  if(!is.character(out_file_type)){
    stop("out_file_type needs to be a character string ")
  }

  if(!is.numeric(chunk_size)){
    if(is.null(chunk_size)){
      message("No chunk_size defined - mosaicing all available tiles")
    } else {
      stop("chunk_size must be a number")
    }
  }

  #--- list all files in output directory
  all <- list.files(path = basedir, pattern = glue::glue("{file_type}"), full.names = TRUE)

  #--- remove meta files from list ---#
  if(any(stringr::str_detect(all,'.tif'))){

    all <- all[stringr::str_detect(all, '.tif$')]

  }

  #--- remove meta files from list ---#
  if(any(stringr::str_detect(all,'.bil'))){

    all <- all[stringr::str_detect(all, '.bil$')]

  }

  #--- default to mosaicing all the files within a given list ---#
  if(is.null(chunk_size)){

    chunk_size <- length(all)

  }

  if(chunk_size > length(all)){
    stop("chunk_size must be <= the total number of tiles to mosaic")
  }

  #--- define number of files to be allocated to each new directory using chunk_size ---#
  lists <- split(all, ceiling(seq_along(all)/chunk_size))

  if(!dir.exists(basedir)){
    message("odir doesnt exist - creating folder")
  }

  dir.create(odir,showWarnings = FALSE)

  counter <<- 0

  mapply(mosaic_function,
         x = lists,
         idx = idx,
         odir = odir,
         out_file_type = out_file_type,
         pixel_type = pixel_type,
         cellsize = cellsize,
         number_of_bands = number_of_bands,
         mosaic_method = mosaic_method,
         mosaic_colormap_mode = mosaic_colormap_mode,
         chunk_size = chunk_size)
}

mosaic_function <- function(x,
                            idx,
                            odir,
                            out_file_type,
                            pixel_type,
                            cellsize,
                            number_of_bands,
                            mosaic_method,
                            mosaic_colormap_mode,
                            chunk_size){
  counter <<-  counter + 1

  if(is.null(chunk_size)){
    out <- glue::glue("{idx}_{out_file_type}")
  } else {
    out <- glue::glue("{idx}_{counter}{out_file_type}")
  }

  message("Mosaicing : ", out)

  #--- mosaic ---#
  arcpy$MosaicToNewRaster_management(input_rasters = x,
                                     output_location = odir,
                                     raster_dataset_name_with_extension = out,
                                     pixel_type = pixel_type,
                                     cellsize = cellsize,
                                     number_of_bands = number_of_bands,
                                     mosaic_method = mosaic_method,
                                     mosaic_colormap_mode = mosaic_colormap_mode)
}
