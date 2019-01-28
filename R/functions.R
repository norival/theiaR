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
#' @param object An object of class \linkS4class{Theia*}
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


#' @describeIn theia_download Download a full collection
#'
#' @export

setMethod("theia_download", "TheiaCollection",
          function(object, override = FALSE) {
            lapply(object@tiles, theia_download, override = override)
          })
