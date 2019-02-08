#' Authentication system to Theia website
#'
#' Generate and manage authentication to Theia website from login and
#' password. It requests a token to download tiles when created and
#' automatically request a new one when it has expired (after 2h). It is used
#' to download tiles from \link{TheiaTile} and \link{TheiaCollection} objects.
#'
#' @name TheiaAuth
#'
#' @section Usage:
#' \preformatted{
#'    a <- TheiaAuth$new(auth.file)
#'
#'    a$token()
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'    \item{a:}{A \code{TheiaAuth} object}
#'    \item{auth.file}{The path to the file containing login and password. It
#'    will be created if it does not exist. See `Details` for more informations}
#'  }
#'
#' @section Details:
#'    \code{TheiaAuth$new(auth.file)} Create a new instance of the class
#'
#'    \code{a$token()} Return the current token or generate a next one if it has
#'    expired
#'
#' @section Details:
#'    This class is used to manage authentication to Theia website, without
#'    intervention from the user. Login and password must be stored in a
#'    separate text file with these two lines:
#'    
#'    login
#'    password
#'    
#'    File content is read each time authentication is needed (to request a new
#'    token), so login and password are not stored in R's memory. If this file
#'    does not exist, R will prompt you to enter your login and password and
#'    will create the file.
#'
NULL


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

  # check if password file exists
  if (!(file.exists(self$auth.file))) {
    message(self$auth.file, " does not exist. Creating new one...")

    # prompt for login and password
    login <- readline("Enter login: ")
    pw    <- askpass::askpass("Enter password: ")

    # write to file
    writeLines(text = paste(login, pw, sep = "\n"), self$auth.file)

    rm("pw")
  }

  # fill token field
  private$.token <- self$token

  return(invisible(self))
}


.TheiaAuth_auth <- function(self, private)
{
  # read auth file
  return(readLines(self$auth.file))
}


.TheiaAuth_token <- function(self, private)
{
  if (!(is.null(private$.token))) {
    # token already exists: check its age and create a new one if needed
    token.age <- as.numeric(difftime(Sys.time(), private$token.date, units = "hours"))
  }

  if (is.null(private$.token) || token.age > 2) {
    # token is too old or dose not exist yet, request a new one
    # make request to get a new token
    req <- httr::POST(private$baseurl,
                      body = list(ident = self$auth[1],
                                  pass  = self$auth[2]))

    # check status of request
    httr::stop_for_status(req, task = "retrieve token.")

    # store token
    private$.token <- httr::content(req, as = "text")

    # check if token is ok
    if (nchar(private$.token) == 0) {
      stop("Failed to retrieve token. Check your IDs.")
    }

    # store token publiation date
    private$token.date <- Sys.time()
  }

  return(invisible(self))
}
