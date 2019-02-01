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
#'    c$download(auth, overwrite = FALSE)
#'    c$check()
#'    c$get_bands()
#'    c$status
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
#'    \item{auth:}{A `TheiaAuth` object, for identication to Theia website}
#'    \item{overwrite:}{Overwrite existing tiles (default to `FALSE`}
#'  }
#'
#' @section Details:
#'    \code{TheiaCollection$new()} Create a new instance of the class
#'
#'    \code{c$download(overwrite = FALSE)} Download the tiles of the collection
#'    and check the resulting files
#'
#'    \code{$ccheck()} Check the tiles of the collection
#'
#'    \code{c$get_bands()} List bands available in each tile
#'
#'    \code{c$status} Return the status of each tile of teh collection
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

                 download = function(auth, overwrite = FALSE)
                 {
                   .TheiaCollection_download(self, auth, overwrite)
                 },
                 
                 get_bands = function()
                 {
                   .TheiaCollection_get_bands(self, private)
                 },

                 read = function(bands)
                 {
                   .TheiaCollection_read(self, private, bands)
                 }),

            # active -----------------------------------------------------------
            active =
              list(status = function()
                   {
                     .TheiaCollection_status(self)
                   })
  )


# Functions definitions --------------------------------------------------------

.TheiaCollection_initialize <- function(self, cart.path, tiles, query, dir.path)
{
  # fill dir.path field
  self$dir.path <- dir.path

  if (!(is.null(cart.path))) {
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

  } else if (!(is.null(tiles))) {
    # TODO: Implement building from a list of tiles
    # build from list of tiles

  } else if (!(is.null(query))) {
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


.TheiaCollection_download <- function(self, auth, overwrite)
{
  # download needed tiles
  lapply(self$tiles,
         function(x, auth, overwrite) {
           x$download(auth, overwrite = overwrite)
         },
         auth = auth, overwrite = overwrite)

  return(invisible(self))
}


.TheiaCollection_get_bands <- function(self, private)
{
  # get available bands in each tile
  bands <- lapply(self$tiles,
                      function(x)
                      {
                        cbind.data.frame(tile = x$file.path, x$get_bands())
                      })

  bands <- do.call(rbind, bands)

  # format tiles names
  bands$tile <- gsub(self$dir.path, "", bands$tile)
  bands$tile <- gsub(".zip$", "", bands$tile)

  # get bands that are present in each tile
  bands.common <- bands$band[duplicated(bands$band)]
  bands$common <- bands$band %in% bands.common

  return(bands)
}


.TheiaCollection_read <- function(self, private, bands)
{
  # check if requested bands are available
  avail.bands <- self$get_bands()
  if (any(!(bands %in% avail.bands$band))) {
    stop("Bands '",
         paste(bands[!(bands %in% avail.bands)], collapse = ", "),
         "' are not available!")
  }

  # check if requested bands hae the same ID
  bands.id <- avail.bands$band.id[avail.bands$band %in% bands]
  if (length(unique(bands.id)) > 1) {
    stop("Bands '",
         paste(bands[!(bands %in% avail.bands)], collapse = ", "),
         "' are not at the same resolution!")
  }

  # get file names to read from
  # files   <- unzip(self$file.path, list = TRUE)$Name
  # pattern <- paste(paste0("FRE_", bands, ".tif$"), collapse = "|")
  # files   <- files[grepl(pattern, files)]

  # # read tiles from zip file and create raster::rasterStack object
  # raster::stack(lapply(files, read_tiff_from_zip, zip.file = self$file.path))
}


.TheiaCollection_status <- function(self)
{
  status <- lapply(self$tiles,
                   function(x)
                   {
                     c(x$file.path, unlist(x$status))
                   })
  status <- do.call(rbind, status)
  status <- as.data.frame(status)
  colnames(status) <- c("tile", "exists", "checked", "correct")

  # format tiles names
  status$tile <- gsub(self$dir.path, "", status$tile)
  status$tile <- gsub(".zip$", "", status$tile)

  return(status)
}
