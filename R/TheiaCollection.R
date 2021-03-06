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
#'                             dir.path  = NULL,
#'                             check     = TRUE)
#'                             quiet     = TRUE)
#'
#'    c$download(auth, overwrite = FALSE, check = TRUE, quiet = TRUE)
#'    c$check()
#'    c$status
#'    c$extract(overwrite = FALSE, dest.dir = NULL)
#'    c$read(bands)
#'    c$as_gdalcube(out.file = "gdalcube_collection.sqlite")
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'    \item{c:}{A \code{TheiaCollection} object}
#'    \item{dir.path:}{The path to the directory containing zip files}
#'    \item{check:}{Whether or not to check existing files on collection's creation}
#'    \item{quiet:}{Control verbose output}
#'    \item{tiles:}{A list of TheiaTile objects}
#'    \item{cart:}{An XML cart parsed from a 'meta4' file downloaded from Theia}
#'    website. Used only if Collection is created from a cart
#'    \item{query:}{A TheiaQuery object, used only if collection is created
#'    from a TheiaQuery object. Can also be a list with search terms. In this
#'    case, it will create a `TheiaQuery` object from it. See
#'    \code{\link{TheiaQuery}} for details on query syntax}
#'    \item{auth:}{A character string giving the file path to Theia credentials.
#'    Or a \code{\link{TheiaAuth}} object}
#'    \item{overwrite:}{Overwrite existing tiles (default to `FALSE`)}
#'    \item{bands:}{A character vector of bands to load from tiles}
#'    \item{out.file:}{Filename to store gdalcubes' image collection}
#'  }
#'
#' @section Details:
#'    \code{TheiaCollection$new()} Create a new instance of the class
#'
#'    \code{c$download(overwrite = FALSE, check = TRUE)} Download the tiles of the collection
#'    and check the resulting files
#'
#'    \code{$ccheck()} Check the tiles of the collection
#'
#'    \code{c$status} Return the status of each tile of the collection
#'
#'    \code{c$extract(overwrite = FALSE, dest.dir = NULL)} Extract archives to
#'    dest.dir if supplied, or to the same directory as the archives otherwise
#'
#'    \code{c$read(bands)} Read requested bands, apply corrections on values
#'    (as specified in Theia's product information), and return a list of
#'    RasterStack objects (one stack per tile)
#'
#'    \code{c$as_gdalcube(out.file)} Create a `gdalcubes` image collection from
#'    downloaded tiles. See \url{https://github.com/appelmar/gdalcubes_R} for
#'    more details.
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
#' # Download the tiles in the collection
#' mycollection$download()
#' }
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
            list(cart       = NULL,
                 tiles      = NULL,
                 query      = NULL,
                 collection = NA,
                 dir.path   = NULL,

                 initialize = function(cart.path  = NULL,
                                       tiles      = NULL,
                                       query      = NULL,
                                       dir.path   = NULL,
                                       check      = TRUE,
                                       quiet      = TRUE)
                 {
                   if (quiet == TRUE) {
                     suppressMessages({
                       .TheiaCollection_initialize(self, cart.path, tiles, query, dir.path, check)
                     })
                   } else {
                     .TheiaCollection_initialize(self, cart.path, tiles, query, dir.path, check)
                   }
                 },

                 print = function(...)
                 {
                   .TheiaCollection_print(self)
                 },

                 check = function()
                 {
                   .TheiaCollection_check(self)
                 },

                 download = function(auth, overwrite = FALSE, check = TRUE, quiet = TRUE)
                 {
                   .TheiaCollection_download(self, auth, overwrite, check, quiet)
                 },

                 extract = function(overwrite = FALSE, dest.dir = NULL)
                 {
                   .TheiaCollection_extract(self, private, overwrite, dest.dir)
                 },
                  
                 read = function(bands)
                 {
                   .TheiaCollection_read(self, private, bands)
                 },
                  
                 as_gdalcube = function(out.file)
                 {
                   .TheiaCollection_as_gdalcube(self, private, out.file)
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


.TheiaCollection_initialize <- function(self, cart.path, tiles, query, dir.path, check)
{
  # fill dir.path field
  self$dir.path <- check_dir(dir.path)

  if (!(is.null(cart.path))) {
    # build collection from cart file ------------------------------------------
    # parse xml data contained in '.meta4' cart file
    self$cart <- XML::xmlToList(XML::xmlParse(cart.path))

    # create needed TheiaTile objects
    self$tiles <-
      lapply(self$cart[-1],
             function(x) {
               TheiaTile$new(file.path = paste0(self$dir.path, as.character(x$.attrs)),
                             file.hash = as.character(x$hash),
                             url       = as.character(x$url$text),
                             tile.name = as.character(x$.attrs),
                             check)
             })

    # get the collection from the tiles
    self$collection <- self$tiles[[1]]$collection
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

    # fill collection field
    self$collection <- self$query$query$collection

    # create needed TheiaTile objects
    self$tiles <-
      apply(self$query$tiles, 1,
            function(x) {
              TheiaTile$new(file.path  = paste0(self$dir.path, x[1]),
                            file.hash  = as.character(x[3]),
                            url        = as.character(x[6]),
                            tile.name  = as.character(x[1]),
                            collection = self$collection,
                            check)
            })
  }

  # give names to the tiles
  names(self$tiles) <- lapply(self$tiles, function(x) x$tile.name)

  return(invisible(self))
}


.TheiaCollection_check <- function(self)
{
  # check all the tiles
  lapply(self$tiles, function(x) x$check())

  return(invisible(self))
}


.TheiaCollection_download <- function(self, auth, overwrite, check, quiet)
{
  if (is.character(auth)) {
    # create authentification system if not supplied
    auth <- TheiaAuth$new(auth.file = auth)
  }

  # download needed tiles
  lapply(self$tiles,
         function(x, auth, overwrite, check, quiet) {
           x$download(auth, overwrite = overwrite, check = check, quiet)
         },
         auth = auth, overwrite = overwrite, check = check, quiet = quiet)

  return(invisible(self))
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
  colnames(status) <- c("tile", "exists", "checked", "correct", "extracted")
  rownames(status) <- NULL

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


.TheiaCollection_read <- function(self, private, bands)
{
  # read bands from tiles and return a list of RasterStack objects
  tiles.list <- lapply(self$tiles, function(x) x$read(bands))

  return(tiles.list)
}


.TheiaCollection_as_gdalcube <- function(self, private, out.file = "gdalcube_collection.sqlite")
{
  # export collection as a gdalcube image collection
  if (!(requireNamespace("gdalcubes", quietly = TRUE))) {
    # check if package 'gdalcubes' is installed
    stop("Package 'gdalcubes' needed for this function. Please install it.",
         call. = FALSE)
  }

  if (!(all(as.logical(self$status$exists)))) {
    # check if all files are downloaded
    stop("Some files in the collection are not downloaded. Please check `mycollection$status()`",
         call. = FALSE)
  }

  # extract file paths to collection
  files <- unname(sapply(self$tiles, function(x) x$file.path))

  # create gdalcubes image collection
  gdalcubes.col <-
    gdalcubes::create_image_collection(files    = files,
                                       format   = system.file("templates",
                                                              "gdalcubes_theia.json",
                                                              package = "theiaR"),
                                       out_file = out.file)

  return(gdalcubes.col)
}
