#' @export
TheiaQuery <-
  R6Class("TheiaQuery",
          # private ------------------------------------------------------------
          private =
            list(token    = NULL,
                 url      = NULL,
                 catalog  = NULL),

          # public -------------------------------------------------------------
          public =
            list(query    = NULL,
                 tiles    = NULL,
                 login    = NULL,
                 passwd   = NULL,

                 initialize = function(login, passwd, query)
                 {
                   self$login  <- login
                   self$passwd <- passwd

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
                 },

                 submit = function(...)
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

                   return(invisible(self))
                 })
          )

