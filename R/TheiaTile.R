#' A tile from Theia
#'
#' Generate and manage a tile from Theia (download, check, load).
#'
#' @name TheiaTile
#'
#' @section Usage:
#' \preformatted{
#'    t <- TheiaTile$new(file.path,
#'                       url,
#'                       file.hash)
#'
#'    t$download(override = FALSE)
#'    t$check()
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'    \item{t:}{A \code{TheiaTile} object}
#'    \item{file.path:}{The path to the zip file containing the tile}
#'    \item{url:}{The url to download the tile}
#'    \item{file.hash:}{The md5sum used to check the zip file}
#'    \item{override:}{Override existing tiles (default to `FALSE`)}
#'  }
#'
#' @section Details:
#'    \code{$new(file.path, url, file.hash)} Create a new instance of the class
#'
#'    \code{$download(override = FALSE)} Download the tiles of the collection
#'    and check the resulting files
#'
#'    \code{$check()} Check the tiles of the collection
#'
NULL


#' @export

TheiaTile <-
  R6Class("TheiaTile",
          # public -------------------------------------------------------------
          private =
            list(meta.data = NULL),

          # public -------------------------------------------------------------
          public =
            list(file.path  = NA,
                 file.hash  = NA,
                 url        = NA,
                 status     = list(),

                 initialize = function(file.path, url, file.hash)
                 {
                   .TheiaTile_initialize(self, file.path, url, file.hash)
                 },

                 print = function(...)
                 {
                   .TheiaTile_print(self)
                 },

                 check = function()
                 {
                   .TheiaTile_check(self)
                 },

                 download = function(override = FALSE)
                 {
                   .TheiaTile_download(self, override)
                 })
          )


# Functions definitions --------------------------------------------------------

.TheiaTile_print <- function(self)
{
  # Special method to print
  # TODO: better method to print
  cat("An Tile from Theia\n")

  return(invisible(self))
}


.TheiaTile_initialize <- function(self, file.path, url, file.hash)
{
  # Fill fiedls of the object
  self$file.path <- file.path
  self$url       <- url
  self$file.hash <- file.hash
  self$status    <- list(exists  = FALSE,
                         checked = FALSE,
                         correct = NA)

  # check the tile
  self$check()

  return(invisible(self))
}


.TheiaTile_check <- function(self)
{
  # check the tile
  if (file.exists(self$file.path)) {
    # if the file exists, check it
    message("Checking downloaded file...")

    self$status$exists  <- TRUE
    self$status$checked <- TRUE

    # compute the md5 sum and compare it to the hash
    if (tools::md5sum(self$file.path) == self$file.hash) {
      self$status$correct <- TRUE
    } else {
      self$status$correct <- FALSE
    }
  }

  return(invisible(self))
}


.TheiaTile_download <- function(self, override = FALSE)
{
  # download the file if it is not present and override is set to FALSE
  if (!(file.exists(self$file.path)) | override == TRUE) {
    tryCatch(download.file(self$url, destfile = self$file.path),
             error = function(e) {
               warning("Could not download file!")
             })
  } else {
    # The file already exists
    message("File ", self$file.path, " already exists, skipping.")
  }

  # check the tile
  self$check()

  return(invisible(self))
}
