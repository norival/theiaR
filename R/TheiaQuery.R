#' A query to the Theia website
#'
#' Generate an send a query to Theia web API to get and download tiles based on
#' input given by the user.
#'
#' @name TheiaQuery
#'
#' @section Usage:
#' \preformatted{
#'    q <- TheiaQuery$new(query)
#'
#'    q$update_token()
#'    q$submit()
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'    \item{q:}{A \code{TheiaQuery} object}
#'    \item{query:}{\code{list}, the users' request, see `Queries` for
#'    more informations}
#' }
#'
#' @section Details:
#'    \code{TheiaQuery$new()} Create a new instance of the class, parse `query`
#'    list and submit the query to Theia to retrieve files catalog
#'
#'    \code{q$submit()} Submit the query to Theia and get a list of tiles
#'    corresponding to search criteria
#'
#' @section Queries:
#'
#'    Search criteria are given with a `list` accepting these fields:
#'    \itemize{
#'      \item{collection:} The collection to look for. Accepted values are:
#'        'SENTINEL2', 'Landsat', 'SpotWorldHeritage', 'Snow'
#'      \item{platform:} The platform to look for. Accepted values are:
#'        'LANDSAT5', 'LANDSAT7', 'LANDSAT8', 'SPOT1', 'SPOT2', 'SPOT3',
#'        'SPOT4', 'SPOT5', 'SENTINEL2A', 'SENTINEL2B'
#'      \item{level:} Processing level. Accepted velues are: 'LEVEL1C',
#'        'LEVEL2A', LEVEL3A'
#'      \item{town:} The location to look for. Give a common town name.
#'      \item{tile:} The tile identifier to retrieve.
#'      \item{start.date:} The first date to look for (format: YYYY-MM-DD).
#'      \item{end.date:} The last date to look for (format: YYYY-MM-DD).
#'      \item{latitude:} The x coordinate of a point
#'      \item{longitude:} The y coordinate of a point
#'      \item{latmin:} The minimum latitude to search
#'      \item{latmax:} The maximum latitude to search
#'      \item{lonmin:} The minimum longitude to search
#'      \item{lonmax:} The maximum longitude to search
#'      \item{max.clouds:} The maximum of cloud cover wanted (0-100)
#'    }
#'
#' @seealso
#'    \url{https://github.com/olivierhagolle/theia_download} for an alternative
#'    download method based on Python. Inspiration for this function.
#'
#' @examples
#'
#' \donttest{
#' # Create a query to Theia database, looking for tiles from Sentinel2
#' # satellite around Grenoble, between 2018-07-01 and 2018-07-06.
#'
#' query <- list(collection = "SENTINEL2",
#'               town       = "Grenoble",
#'               start.date = "2018-07-01",
#'               end.date   = "2018-07-06")
#' q <- TheiaQuery$new(query)
#'
#' # Show informations on found tiles
#' print(q$tiles)
#' }
#'
NULL


#' @export

TheiaQuery <-
  R6Class("TheiaQuery",
          # private ------------------------------------------------------------
          private =
            list(url        = NULL,
                 catalog    = NULL,
                 server.url = NULL,
                 resto      = NULL,

                 
                 check      = function()
                 {
                   .TheiaQuery_check(self, private)
                 }),

          # public -------------------------------------------------------------
          public =
            list(query    = NULL,
                 tiles    = NULL,

                 initialize = function(query)
                 {
                   .TheiaQuery_initialize(self, private, query)
                 },

                 print = function(...)
                 {
                   .TheiaQuery_print(self)
                 },

                 submit = function()
                 {
                   .TheiaQuery_submit(self, private)
                 })
          )


# Functions definitions --------------------------------------------------------

.TheiaQuery_print <- function(self)
{
  # Special method to print
  cat("A search query to Theia website\n\n")

  cat("Number of tiles:", nrow(self$tiles), "\n\n")

  cat("Search terms:\n")

  lapply(1:length(self$query),
         function(x) {
           l <- nchar(names(self$query))[[x]]

           cat("   ", names(self$query)[[x]], rep(" ", 11 - l), ": ",
               self$query[[x]], "\n",
               sep = "")

           return(invisible(NULL))
         })

  return(invisible(self))
}


.TheiaQuery_initialize <- function(self, private, query)
{
  # TODO: verification of request
  private$server.url <-
    ifelse(query$collection %in% c("Landsat", "SpotWorldHeritage"),
           "https://theia-landsat.cnes.fr/",
           "https://theia.cnes.fr/atdistrib/")
  private$resto <-
    ifelse(query$collection %in% c("Landsat", "SpotWorldHeritage"),
           "resto/",
           "resto2/")

  # fill query fields
  self$query <- query

  # check query list
  private$check()

  q.link <- list()

  q.link[["q"]]               <- self$query$town
  q.link[["location"]]        <- self$query$tile
  q.link[["platform"]]        <- self$query$platform
  q.link[["processingLevel"]] <- self$query$level
  q.link[["startDate"]]       <- self$query$start.date
  q.link[["completionDate"]]  <- self$query$end.date
  q.link[["lat"]]             <- self$query$latitude
  q.link[["lon"]]             <- self$query$longitude

  # search a rectangle
  if (all(c("latmin", "latmax", "lonmin", "lonmax") %in% names(self$query))) {
    q.link[["box"]] <- URLencode(paste(self$query$lonmin,
                                       self$query$latmin,
                                       self$query$lonmax,
                                       self$query$latmax,
                                       sep = ","),
                                 reserved = TRUE)
  }

  # build query links
  query.link  <- paste(names(q.link), q.link, sep = "=", collapse = "&")
  private$url <- paste0(private$server.url,
                        private$resto,
                        "api/collections/",
                        query$collection,
                        "/search.json?",
                        query.link)

  # submit the query
  self$submit()

  if (!(is.list(self$tiles)) && self$tiles ==  "No tiles matching search criteria") {
    # exit function if no tiles are found, with an error
    stop("No tiles matching search criteria", call. = FALSE)
  }

  if (!(is.null(query$max.clouds))) {
    # remove tiles with too much clouds
    clouds.over <- self$tiles$cloud.cover > query$max.clouds

    if (sum(clouds.over) > 0) {
      if (sum(clouds.over) == nrow(self$tiles)) {
        # if every tile has too much clouds, throw an informative error
        stop("Every tile has too much clouds (min cloud level: ",
             min(self$tiles$cloud.cover), ")",
             call. = FALSE)
      }

      # show number of bad tiles
      message("Removing ", sum(clouds.over), " over ", nrow(self$tiles),
              " tiles because too much clouds")

      # remove bad tiles
      self$tiles <- self$tiles[!(clouds.over), ]
    }
  }

  return(invisible(self))
}


.TheiaQuery_check <- function(self, private)
{
  # check for incompatible queries
  if (!(is.null(self$query$tile)) && self$query$collection != "SENTINEL2") {
    stop("'Tile' is only available for SENTINEL2 collection",
         call. = FALSE)
  }

  # check if user has not specified both a point and a rectangle
  box.names <- c("latmin", "latmax", "lonmin", "lonmax")
  if (any(box.names %in% names(self$query))) {
    if (!(all(box.names %in% names(self$query)))) {
      stop("Specify each of ", box.names, " to have a rectangle",
           call. = FALSE)
    }

    bad.names <-
    if (any(c("latitude", "longitude") %in% names(self$query))) {
      stop("Specify a point or a rectangle, not both", call. = FALSE)
    }
  }

  # available choices
  collection.choices <- c('Landsat', 'SpotWorldHeritage', 'SENTINEL2', 'Snow', 'VENUS')
  platform.choices   <- c('LANDSAT5', 'LANDSAT7', 'LANDSAT8', 'SPOT1', 'SPOT2',
                          'SPOT3', 'SPOT4', 'SPOT5', 'SENTINEL2A', 'SENTINEL2B',
                          'VENUS')
  level.choices      <- c('LEVEL1C', 'LEVEL2A', 'LEVEL3A')

  # check queries
  self$query$tile       <- parse_query(self$query$tile, "tile", "character")
  self$query$town       <- parse_query(self$query$town, "town", "character")
  self$query$collection <- parse_query(self$query$collection, "collection", "character",
                                       choices = collection.choices,
                                       default = "SENTINEL2")
  self$query$platform   <- parse_query(self$query$platform, "platform", "character",
                                       choices = platform.choices)
  self$query$level      <- parse_query(self$query$level, "level", "character",
                                       choices = level.choices,
                                       default = "LEVEL2A")
  self$query$start.date <- parse_query(self$query$start.date, "date", "character")
  self$query$end.date   <- parse_query(self$query$end.date, "date", "character")
  self$query$max.clouds <- parse_query(self$query$max.clouds, "max.clouds", "numeric",
                                       choices = 0:100)
  self$query$latitude   <- parse_query(self$query$latitude, "latitude", "numeric")
  self$query$longitude  <- parse_query(self$query$longitude, "longitude", "numeric")
  self$query$latmin     <- parse_query(self$query$latmin, "latmin", "numeric")
  self$query$latmax     <- parse_query(self$query$latmax, "latmax", "numeric")
  self$query$lonmin     <- parse_query(self$query$lonmin, "lonmin", "numeric")
  self$query$lonmax     <- parse_query(self$query$lonmax, "lonmax", "numeric")

  return(invisible(self))
}


.TheiaQuery_submit <- function(self, private)
{
  # TODO: gestion des erreurs
  # make http request to get catalog
  req <- httr::GET(private$url)

  httr::stop_for_status(req, task = paste0("retrieve data, invalid query url:\n", private$url))

  # parse and save catalog
  private$catalog <- httr::content(req, as = "parsed")

  if (length(private$catalog$features) == 0) {
    # exit function if no tiles are found
    self$tiles <- "No tiles matching search criteria"

    return(invisible(self))
  }

  # extract tiles
  cart <-
    lapply(private$catalog$features,
           function(x) {
             # get file extension
             file.ext <- ifelse(grepl("gzip", x$properties$services$download$mimeType),
                                ".tar.gz",
                                ".zip")

             # return important information
             data.frame(file.name   = paste0(x$properties$productIdentifier, file.ext),
                        tile.id     = x$id,
                        file.hash   = ifelse(is.null(x$properties$services$download$checksum),
                                             NA,
                                             x$properties$services$download$checksum),
                        cloud.cover = as.numeric(as.character(x$properties$cloudCover)),
                        snow.cover  = as.numeric(as.character(x$properties$snowCover)))
           })
  self$tiles <- do.call(rbind, cart)

  # build url for downloading
  self$tiles$url <- paste0(private$server.url,
                           private$resto,
                           "collections/",
                           self$query$collection,
                           "/",
                           self$tiles$tile.id,
                           "/download")

  return(invisible(self))
}
