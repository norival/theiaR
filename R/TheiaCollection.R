#' @include TheiaTile.R
NULL


#' A collection of tiles from Theia
#'
#' Generate and manage collection of tiles from Theia. This collection can be
#' created either from a cart file ('.meta4') downloaded from Theia website,
#' from a \code{\link{TheiaQuery}} object or from a list of
#' \code{\link{TheiaTile}} (not implemented yet).
#'
#' @name TheiaCollection
#'
#' @section Usage:
#' \preformatted{
#'    c <- TheiaCollection$new(cart.path = NULL,
#'                             tiles     = NULL,
#'                             query     = NULL,
#'                             dir.path  = NULL)
#'
#'    c$download(override = FALSE)
#'    c$check()
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'    \item{c:}{A \code{TheiaCollection} object}
#'    \item{dir.path:}{The path to the directory containing zip files}
#'    \item{tiles:}{A list of TheiaTile objects}
#'    \item{cart:}{An XML cart parsed from a 'meta4' file downloaded from Theia}
#'    website. Used only if Collection is created from a cart
#'    \item{query:}{A TheiaQuery object, used only if collection is created
#'    from a TheiaQuery object}
#'    \item{override:}{Override existing tiles (default to `FALSE`}
#'  }
#'
#' @section Details:
#'    \code{$new()} Create a new instance of the class
#'
#'    \code{$download(override = FALSE)} Download the tiles of the collection
#'    and check the resulting files
#'
#'    \code{$check()} Check the tiles of the collection
#'
NULL


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
                   .TheiaCollection_initialize(self, cart.path, tiles, query, dir.path)
                 },

                 check = function()
                 {
                   .TheiaCollection_check(self)
                 },

                 download = function(override = FALSE)
                 {
                   .TheiaCollection_download(self, override)
                 })
          )


# Functions definitions --------------------------------------------------------

.TheiaCollection_initialize <- function(self, cart.path, tiles, query, dir.path)
{
  if (!(missing(cart.path))) {
    # build collection from cart file ------------------------------------------
    # parse xml data contained in '.meta4' cart file
    self$cart <- XML::xmlToList(XML::xmlParse(cart.path))

    # create needed TheiaTile objects
    self$tiles <-
      lapply(self$cart[-1],
             function(x) {
               TheiaTile$new(file.path = paste0(dir.path, as.character(x$.attrs)),
                             file.hash = as.character(x$hash),
                             url       = as.character(x$url$text))
             })

    # remove useless names of tiles list
    self$tiles <- unname(self$tiles)

  } else if (!(missing(tiles))) {
    # TODO: Implement building from a list of tiles
    # build from list of tiles

  } else if (!(missing(query))) {
    # build collection from a TheiaQuery object --------------------------------

    # create needed TheiaTile objects
    self$tiles <-
      apply(query$tiles, 1,
            function(x) {
              TheiaTile$new(file.path = paste0(dir.path, x[1]),
                            file.hash = as.character(x[3]),
                            url       = as.character(x[2]))
            })

    # remove useless names of tiles list
    self$tiles <- unname(self$tiles)
  }

  return(invisible(self))
}


.TheiaCollection_check <- function(self)
{
  # check all the tiles
  lapply(self$tiles, function(x) x$check())

  return(invisible(self))
}


.TheiaCollection_download <- function(self, override)
{
  # download needed tiles
  lapply(self$tiles,
         function(x, override) {
           x$download(override = override)
         },
         override = override)

  return(invisible(self))
}
