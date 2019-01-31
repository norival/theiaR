#' Class representing a single tile from Theia
#'
#' Internal representation of a tile from Theia, storing its location, status
#' and metadata.
#'
#' @docType class
#' @name TheiaTile
#'
#' @field file.path The path to the zip file containing the tile
#' @field file.hash The hash of the file, used to check if the file is correctly
#' downloaded
#' @field url The url to download the tile from Theia website
#' @field status A list giving the status of the tile: whether it is downloaded,
#' checked, correctly downloaded
#'
#' @section Methods:
#' \describe{
#'    \item{\code{$new()}}{
#'      Create a new instance of the class
#'    }
#'    \item{\code{$download()}}{
#'      Download the tile and check the resulting file
#'    }
#'    \item{\code{$check()}}{
#'      Check the tile
#'    }
#' }
#'
#' @export

TheiaTile <-
  R6Class("TheiaTile",
          # public -------------------------------------------------------------
          public =
            list(file.path  = NA,
                 file.hash  = NA,
                 url        = NA,
                 status     = list(),

                 initialize = function(file.path,
                                       url,
                                       file.hash)
                 {
                   self$file.path <- file.path
                   self$url       <- url
                   self$file.hash <- file.hash
                   self$status    <- list(exists  = FALSE,
                                          checked = FALSE,
                                          correct = NA)
                   self$check()
                 },

                 print = function(...)
                 {
                   cat("An Tile from Theia\n")

                   return(invisible(self))
                 },

                 check = function()
                 {
                   if (file.exists(self$file.path)) {
                     message("Checking downloaded file...")

                     self$status$exists  <- TRUE
                     self$status$checked <- TRUE

                     if (tools::md5sum(self$file.path) == self$file.hash) {
                       self$status$correct <- TRUE
                     } else {
                       self$status$correct <- FALSE
                     }
                   }

                   return(invisible(self))
                 },

                 download = function(override = FALSE)
                 {
                   if (!(file.exists(self$file.path)) | override == TRUE) {
                     tryCatch(download.file(self$url, destfile = self$file.path),
                              error = function(e) {
                                warning("Could not download file!")
                              })
                   } else {
                     message("File ", self$file.path, " already exists, skipping.")
                   }

                   self$check()

                   return(invisible(self))
                 })
          )


