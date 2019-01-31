#' A query to the Theia website
#'
#' Generate an send a query to Theia web API to get and download tiles based on
#' input given by the user.
#'
#' @name TheiaQuery
#'
#' @section Usage:
#' \preformatted{
#'    q <- TheiaQuery$new(login, passwd, query)
#'
#'    q$update_token()
#'    q$submit()
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'    \item{q:}{A \code{TheiaQuery} object}
#'    \item{login:}{\code{charcter}, the users' login}
#'    \item{passwd:}{\code{charcter}, the users' password}
#'    \item{query:}{\code{list}, the users' request, see `Queries` for
#'    more informations}
#' }
#'
#' @section Details:
#'    \code{TheiaQuery$new()} Create a new instance of the class and parse
#'    `query` list
#'
#'    \code{q$submit()} Submit the query to Theia and get a list af tiles
#'    corresponding to search criteria
#'
#'    \code{q$update_token()} Get a token to download tiles
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
            list(token    = NULL,
                 login    = NULL,
                 passwd   = NULL,
                 url      = NULL,
                 catalog  = NULL),

          # public -------------------------------------------------------------
          public =
            list(query    = NULL,
                 tiles    = NULL,

                 initialize = function(login, passwd, query)
                 {
                   .TheiaQuery_initialize(self, private, login, passwd, query)
                 },

                 update_token = function()
                 {
                   .TheiaQuery_update_token(self, private)
                 },

                 submit = function()
                 {
                   .TheiaQuery_submit(self, private)
                 })
          )


# Functions definitions --------------------------------------------------------

.TheiaQuery_initialize <- function(self, private, login, passwd, query)
{
  private$login  <- login
  private$passwd <- passwd

  # base url for theia
  base.url <- "https://theia.cnes.fr/atdistrib/resto2/api/collections"

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
  private$url <- paste0(base.url, "/", query$collection, "/",
                        "search.json?", query.link)

  return(invisible(self))
}


.TheiaQuery_update_token <- function(self, private)
{
  # base url for authentification
  baseurl <- "https://theia.cnes.fr/atdistrib/services/authenticate/"

  # make request to get a new token
  req <- httr::POST(baseurl,
                    body = list(ident = private$login,
                                pass  = private$passwd))

  # store token
  private$token <- content(req, as = "text")

  if (nchar(private$token) > 1 & any(grepl("download$", self$tiles$url))) {
    # if tiles if filled but does not have a token, adds the token to the links
    self$tiles$url <- gsub("\\?_tk=.*$", "", self$tiles$url)
    self$tiles$url <- paste0(self$tiles$url, "?_tk=", private$token)
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
             data.frame(file.name = paste0(x$properties$title, ".zip"),
                        url       = x$properties$services$download$url,
                        file.hash = x$properties$services$download$checksum)
           })
  self$tiles <- do.call(rbind, cart)

  if (!(is.null(private$token)) && nchar(private$token) > 1) {
    # add the token to the links
    self$tiles$url <- paste0(self$tiles$url, "?_tk=", private$token)
  }

  return(invisible(self))
}
