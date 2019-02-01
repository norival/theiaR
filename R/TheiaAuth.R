#' @export
TheiaAuth <-
  R6Class("TheiaAuth",
          # private ------------------------------------------------------------
          private =
            list(.token     = NULL,
                 token.date = NULL,
                 baseurl    = "https://theia.cnes.fr/atdistrib/services/authenticate/"),

          # public -------------------------------------------------------------
          public =
            list(auth.file = NULL,

                 initialize = function(auth.file)
                 {
                   .TheiaAuth_initialize(self, private, auth.file)
                 }),
            
          active = 
            list(auth = function()
                 {
                   .TheiaAuth_auth(self, private)
                 },

                 token = function()
                 {
                   .TheiaAuth_token(self, private)

                   return(private$.token)
                 })
          )


# Functions definitions --------------------------------------------------------

.TheiaAuth_initialize <- function(self, private, auth.file)
{
  # fill auth.file field
  self$auth.file <- auth.file

  # fill token field
  private$.token <- self$token

  return(invisible(self))
}


.TheiaAuth_auth <- function(self, private)
{
  # read auth file
  return(readLines(auth.file))
}


.TheiaAuth_token <- function(self, private)
{
  if (!(is.null(private$.token))) {
    # token already exists: check its age and create a new one if needed
    token.age <- as.numeric(difftime(Sys.Date(), private$token.date, units = "hours"))
  }

  if (is.null(private$.token) || token.age > 2) {
    # token is too old or dose not exist yet, request a new one
    # make request to get a new token
    req <- httr::POST(private$baseurl,
                      body = list(ident = self$auth[1],
                                  pass  = self$auth[2]))

    # store token
    private$.token <- content(req, as = "text")

    # store token publiation date
    private$token.date <- Sys.Date()
  }

  return(invisible(self))
}
