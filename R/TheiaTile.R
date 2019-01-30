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


