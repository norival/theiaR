#' A query to the Theia website
#'
#' Generate an send a query to Theia web API to get and download tiles based on
#' input given by the user.
#'
#' @name TheiaQuery
#'
#' @section Usage:
#' \preformatted{
#'    q <- TheiaQuery$new(query, auth)
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
#'    \code{q$submit()} Submit the query to Theia and get a list af tiles
#'    corresponding to search criteria
#'
#' @section Queries:
#'
#'    Search criterai are given with a `list` accepting these fields:
#'    \itemize{
#'      \item{collection:} The collection to look for. Accepted values are:
#'        `SENTINEL2` (more will be added soon).
#'      \item{town:} The location to look for. Give a common town name.
#'      \item{tile:} The tile identifier to retrieve.
#'      \item{start.date:} The first date to look for (format: YYYY-MM-DD).
#'      \item{end.data:} The last date to look for (format: YYYY-MM-DD).
#'      \item{latitude:} The x coordinate of a point
#'      \item{longitude:} The y coordinate of a point
#'      \item{max.clouds:} The maximum of cloud cover wanted
#'    }
#'
#' @seealso
#'    \url{https://github.com/olivierhagolle/theia_download} for an alternative
#'    download method based on Python. Inspiration for this function.
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
                 resto      = NULL),

          # public -------------------------------------------------------------
          public =
            list(query    = NULL,
                 tiles    = NULL,

                 initialize = function(query)
                 {
                   .TheiaQuery_initialize(self, private, query)
                 },

                 submit = function()
                 {
                   .TheiaQuery_submit(self, private)
                 })
          )


# Functions definitions --------------------------------------------------------

.TheiaQuery_initialize <- function(self, private, query)
{
  # TODO: verification of request
  private$server.url <-
    ifelse(query$collection == "Landsat",
           "https://theia-landsat.cnes.fr/",
           "https://theia.cnes.fr/atdistrib/")
  private$resto <-
    ifelse(query$collection == "Landsat",
           "resto/",
           "resto2/")

  # fill query fields
  self$query <- query

  q.link <- list()

  q.link[["q"]]               <- query$town
  q.link[["location"]]        <- query$tile
  q.link[["platform"]]        <- query$platform
  q.link[["startDate"]]       <- query$start.date
  q.link[["completionDate"]]  <- query$end.date
  q.link[["lat"]]             <- query$latitude
  q.link[["lon"]]             <- query$longitude

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
