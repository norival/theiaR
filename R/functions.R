setGeneric("theia_download", function(object, override) standardGeneric("theia_download"))

setMethod("theia_download", "TheiaTile",
          function(object, override = F) {
            if (!(file.exists(object@file.path)) | override == TRUE) {
              tryCatch(download.file(object@url, destfile = object@file.path),
                       error = function(e) {
                         warning("Could not download file!")
                       })
            } else {
              message("File", object@file.path, "already exists, skipping.")
            }
          })
