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
                     # build from query
                   }
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
