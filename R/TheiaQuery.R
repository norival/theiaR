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
            list(url     = NULL,
                 catalog = NULL,
                 baseurl = "https://theia.cnes.fr/atdistrib/resto2/api/collections/"),

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

  # how to display in query link
  q.link <- c("q",
              "location",
              "platform",
              "startDate",
              "completionDate",
              "lat",
              "lon")

  # how query is given by the user
  q.query <- c("town",
               "tile",
               "platform",
               "start.date",
               "end.date",
               "latitude",
               "longitude")

  # match search and links
  i <- which(q.query %in% names(query))
  j <- which(names(query) %in% q.query)

  # build query links
  query.link  <- paste(q.link[i], query[j], sep = "=", collapse = "&")
  private$url <- paste0(private$baseurl,
                        query$collection,
                        "/search.json?",
                        query.link)

  # submit the query
  self$submit()

  if (!(is.null(query$max.clouds))) {
    # remove tiles with too much clouds
    self$tiles <- self$tiles[self$tiles$cloud.cover <= query$max.clouds, ]
  }

  return(invisible(self))
}


.TheiaQuery_submit <- function(self, private)
{
  # TODO: gestion des erreurs
  # make http request to get catalog
  req <- httr::GET(private$url)

  # parse and save catalog
  private$catalog <- httr::content(req, as = "parsed")

  # extract tiles
  cart <-
    lapply(private$catalog$features,
           function(x) {
             data.frame(file.name   = paste0(x$properties$title, ".zip"),
                        url         = x$properties$services$download$url,
                        file.hash   = x$properties$services$download$checksum,
                        cloud.cover = as.numeric(as.character(x$properties$cloudCover)),
                        snow.cover  = as.numeric(as.character(x$properties$snowCover)))
           })
  self$tiles <- do.call(rbind, cart)

  return(invisible(self))
}
