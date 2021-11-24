#' @title Call LAStools functions from R
#'
#' @description Run LAStools functions from R interface
#'
#' @param LAStoolsDir Character Path. Path to LAStools bin folder. Default is \code{""}
#' - where users have set the LAStools/bin folder to environment variables.
#' @param LAStool Character. Name of LAStools to use.
#' @param input Character Path. Input files path.
#' @param arguments Character string. Desired switches to be performed.
#'
#' @note
#' Adapted from \href{https://groups.google.com/g/lastools/c/Eje6fiFprzc/m/Xu9_O524AgAJ}{LAStools Google Group Thread: OP - Luiz Estraviz}
#'
#' @examples
#' \dontrun{
#' #--- Define input ---#
#' input  = "F:/test"
#'
#' # Define las/laz files to be processed
#' inFiles  = glue::glue("{input}/*.laz")
#'
#' # Define output directory (and creates it ... if doesn't exist)
#' outDir = glue::glue("{input}/info")
#'
#' dir.create(outDir, showWarnings = F)
#'
#' RLAStools(LAStool = "lasinfo",
#'           input = inFiles,
#'           cores = 4,
#'           arguments = glue::glue("-v -odix _info -otxt -odir {outDir}"))
#' }
#'
#' @export

RLAStools <-  function(LAStoolsDir = "",
                       LAStool,
                       input,
                       arguments = NULL){

  #--- start time ---#
  t1 <- Sys.time()

  #--- concatenate lastools code ---#
  cmd = paste(paste(LAStoolsDir, LAStool, sep=''), '-i', input , arguments)

  #--- execute code ---#
  shell(cmd)

  #--- end time ---#
  t2 <- Sys.time()

  #display total processing time
  difftime(t2, t1, units = "auto")

}
