TheiaAuth <-
  R6Class("TheiaAuth",
          # private ------------------------------------------------------------
          private =
            list(login      = NULL,
                 passwd     = NULL,
                 .token     = NULL,
                 token.date = NULL,
                 baseurl    = "https://theia.cnes.fr/atdistrib/services/authenticate/"),

          # public -------------------------------------------------------------
          public =
            list(initialize = function(login, passwd)
                 {
                   .TheiaAuth_initialize(self, private, login, passwd)
                 }),
            
          active = 
            list(token = function()
                 {
                   .TheiaAuth_token(self, private)

                   return(private$.token)
                 })
          )


# Functions definitions --------------------------------------------------------

.TheiaAuth_initialize <- function(self, private, login, passwd)
{
  # fill login and password fields
  private$login  <- login
  private$passwd <- passwd

  # fill token field
  private$.token <- self$token

  return(invisible(self))
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
                      body = list(ident = private$login,
                                  pass  = private$passwd))

    # store token
    private$.token <- content(req, as = "text")

    # store token publiation date
    private$token.date <- Sys.Date()
  }

  return(invisible(self))
}
