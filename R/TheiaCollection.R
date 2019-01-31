#' @include TheiaTile.R
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
