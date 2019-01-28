# ------------------------------------------------------------------------------
# Generic functions
# ------------------------------------------------------------------------------

#' Download theia object
#'
#' Download an object of class Theia*
#'
#' @param override Logical: download files that are already present? (default to
#' FALSE)
#'
#' @return Invisibly, a vector of downloaded file path
#'
#' @export

setGeneric("theia_download", function(object, override = FALSE) standardGeneric("theia_download"))


#' @describeIn theia_download Download a single tile
#'
#' @param object
#'
#' @export

setMethod("theia_download", "TheiaTile",
          function(object, override = FALSE) {
            if (!(file.exists(object@file.path)) | override == TRUE) {
              tryCatch(download.file(object@url, destfile = object@file.path),
                       error = function(e) {
                         warning("Could not download file!")
                       })
            } else {
              message("File ", object@file.path, " already exists, skipping.")
            }
          })
