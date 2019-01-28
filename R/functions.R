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


#' Check theia object
#'
#' Check an object of class Theia*
#'
#' @return Invisibly, a \code{data.frame} with checking status
#'
#' @export

setGeneric("theia_check", function(object) standardGeneric("theia_check"))


# ------------------------------------------------------------------------------
# Methods
# ------------------------------------------------------------------------------

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


#' @describeIn theia_check Check a single tile
#'
#' @param object An object of class \linkS4class{TheiaTile}
#'
#' @export

setMethod("theia_check", "TheiaTile",
          function(object) {
            if (!(file.exists(object@file.path))) {
              object@status$exists  <- FALSE
              object@status$checked <- FALSE
              object@status$correct <- NA
            } else {
              object@status$exists  <- TRUE
              object@status$checked <- TRUE

              if (tools::md5sum(object@file.path) == object@file.hash) {
                object@status$correct <- TRUE
              } else {
                object@status$correct <- FALSE
              }
            }

            tmp <- data.frame(tile    = object@file.path,
                              exists  = object@status$exists,
                              checked = object@status$checked,
                              correct = object@status$correct)

            return(invisible(tmp))
          })


#' @describeIn theia_check Check a collection
#'
#' @param object An object of class \linkS4class{TheiaCollection}
#'
#' @export

setMethod("theia_check", "TheiaCollection",
          function(object) {
            tmp <- lapply(object@tiles, theia_check)
            tmp <- do.call(rbind, tmp)
            rownames(tmp) <- NULL

            return(invisible(tmp))
          })
