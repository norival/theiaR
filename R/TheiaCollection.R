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
#'    c$extract(overwrite = FALSE, dest.dir = NULL)
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
#'    from a TheiaQuery object. Can also be a list with search terms. In this
#'    case, it will create a `TheiaQuery` object from it. See
#'    \code{\link{TheiaQuery}} for details on query syntax}
#'    \item{auth:}{A character string giving the file path to Theia credentials.
#'    Or a \code{\link{TheiaAuth}} object}
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
#'    \code{c$status} Return the status of each tile of the collection
#'
#'    \code{c$read(bands)} Read band(s) from the zip files and returns a list of
#'    raster objects
#'
#'    \code{c$extract(overwrite = FALSE, dest.dir = NULL)} Extract archives to
#'    dest.dir if supplied, or to the same directory as the archives otherwise
#'
#' @examples
#'
#' \donttest{
#' # Create a collection from a query
#' ## Create a query to Theia database, looking for tiles from Sentinel2
#' ## satellite around Grenoble, between 2018-07-01 and 2018-07-06.
#'
#' query <- list(collection = "SENTINEL2",
#'               town       = "Grenoble",
#'               start.date = "2018-07-01",
#'               end.date   = "2018-07-06")
#'
#' ## Create a collecion of tiles from this query
#' mycollection <- TheiaCollection$new(query = query, dir.path = ".")
#' 
#' print(mycollection)
#' }
#'
#' # Alternatively, you can create a collection from a cart file (that you can
#' # download from Theia's website)
#' cart.path <- system.file("extdata", "cart.meta4", package = "theiaR")
#' 
#' mycollection <- TheiaCollection$new(cart.path = cart.path,
#'                                     dir.path  = ".")
#' 
#' print(mycollection)
#'
#' \dontrun{
#' # Finally, you can extract zip archives containing the tiles
#' mycollection$extract(overwrite = FALSE)
#' }
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

                 print = function(...)
                 {
                   .TheiaCollection_print(self)
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
                 },

                 extract = function(overwrite = FALSE, dest.dir = NULL)
                 {
                   .TheiaCollection_extract(self, private, overwrite, dest.dir)
                 }),

            # active -----------------------------------------------------------
            active =
              list(status = function()
                   {
                     .TheiaCollection_status(self)
                   })
  )


# Functions definitions --------------------------------------------------------

.TheiaCollection_print <- function(self)
{
  # Special method to print
  cat("An collection of tiles from Theia\n\n")

  cat("Number of tiles:", length(self$tiles), "\n")
  cat("Directory path : '", self$dir.path, "'\n", sep = "")

  if (!is.null(self$query)) {
    cat("\n")

    cat("Obtained from query\n")
  } 

  if (!is.null(self$cart)) {
    cat("\n")

    cat("Obtained from cart file\n")
  }

  return(invisible(self))
}


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
                             url       = as.character(x$url$text),
                             tile.name = as.character(x$.attrs))
             })

    # give names to the tiles
    names(self$tiles) <- lapply(self$tiles, function(x) x$tile.name)

  } else if (!(is.null(tiles))) {
    # TODO: Implement building from a list of tiles
    # build from list of tiles

  } else if (!(is.null(query))) {
    # build collection from a TheiaQuery object --------------------------------
    if (is.list(query)) {
      # automatically build TheiaQuery object if needed
      self$query <- TheiaQuery$new(query)
    } else {
      # clone TheiaQuery object
      self$query <- query$clone()
    }

    # create needed TheiaTile objects
    self$tiles <-
      apply(self$query$tiles, 1,
            function(x) {
              TheiaTile$new(file.path = paste0(dir.path, x[1]),
                            file.hash = as.character(x[3]),
                            url       = as.character(x[6]),
                            tile.name = as.character(x[1]))
            })

    # give names to the tiles
    names(self$tiles) <- lapply(self$tiles, function(x) x$tile.name)
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
  if (is.character(auth)) {
    # create authentification system if not supplied
    auth <- TheiaAuth$new(auth.file = auth)
  }

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

  # get file names to read from
  tiles.list <- lapply(self$tiles,
                       function(x, bands) {
                         x$read(bands = bands)
                       }, bands = bands)

  # give names to the layers
  names(tiles.list) <- lapply(self$tiles, function(x) x$tile.name)

  return(tiles.list)
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


.TheiaCollection_extract <- function(self, private, overwrite, dest.dir)
{
  # extract all tiles
  file.path <- lapply(self$tiles,
                      function(x, overwrite, dest.dir) {
                        x$extract(overwrite = overwrite, dest.dir = dest.dir)
                      }, overwrite = overwrite, dest.dir = dest.dir)

  return(invisible(unlist(file.path)))
}
