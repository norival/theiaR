#' @include TheiaTile.R
NULL

#' Class representing a collection of tiles from Theia
#'
#' Internal representation of a collection of tiles from Theia, storing their
#' locations, statuses and metadata.
#'
#' @docType class
#' @name TheiaCollection
#'
#' @field dir.path The path to the directory containing zip files
#' @field tiles A list of TheiaTile objects
#' @field cart An XML cart parsed from a 'meta4' file downloaded from Theia
#' website. Used only if Collection is created from a cart
#' @field query A TheiaQuery object, used only if collection is created from a
#' TheiaQuery object
#'
#' @section Methods:
#' \describe{
#'    \item{\code{$new()}}{
#'      Create a new instance of the class
#'    }
#'    \item{\code{$download()}}{
#'      Download the tiles of the collection and check the resulting files
#'    }
#'    \item{\code{$check()}}{
#'      Check the tiles of the collection
#'    }
#' }
#'
#' @export

TheiaCollection <-
  R6Class("TheiaCollection",
          # public -------------------------------------------------------------
          public =
            list(cart     = NULL,
                 tiles    = NULL,
                 query    = NULL,
                 dir.path = NULL,

                 initialize = function(cart.path  = NULL,
                                       tiles      = NULL,
                                       query      = NULL,
                                       dir.path   = NULL)
                 {
                   if (!(missing(cart.path))) {
                     # build from cart file
                     # parse xml data contained in '.meta4' cart file
                     self$cart <- XML::xmlToList(XML::xmlParse(cart.path))

                     self$tiles <-
                       lapply(self$cart[-1],
                              function(x) {
                                TheiaTile$new(file.path = paste0(dir.path, as.character(x$.attrs)),
                                              file.hash = as.character(x$hash),
                                              url       = as.character(x$url$text))
                              })

                     self$tiles <- unname(self$tiles)
                   } else if (!(missing(tiles))) {
                     # build from list of tiles
                   } else if (!(missing(query))) {
                     self$tiles <-
                       apply(query$tiles, 1,
                             function(x) {
                               TheiaTile$new(file.path = paste0(dir.path, x[1]),
                                             file.hash = as.character(x[3]),
                                             url       = as.character(x[2]))
                             })

                     self$tiles <- unname(self$tiles)
                   }
                 },

                 check = function(...)
                 {
                   lapply(self$tiles, function(x) x$check())

                   return(invisible(self))
                 },

                 download = function(override = FALSE)
                 {
                   lapply(self$tiles,
                          function(x, override) {
                            x$download(override = override)
                          },
                          override = override)

                   return(invisible(self))
                 })
          )
