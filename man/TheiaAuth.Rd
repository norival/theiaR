% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TheiaAuth.R
\name{TheiaAuth}
\alias{TheiaAuth}
\title{Authentication system to Theia website}
\description{
Generate and manage authentication to Theia website from login and
password. It requests a token to download tiles when created and
automatically request a new one when it has expired (after 2h). It is used
to download tiles from \link{TheiaTile} and \link{TheiaCollection} objects.
}
\section{Usage}{

\preformatted{
   a <- TheiaAuth$new(auth.file)

   a$token()
}
}

\section{Arguments}{


\describe{
   \item{a:}{A \code{TheiaAuth} object}
   \item{auth.file}{The path to the file containing login and password. It
   will be created if it does not exist. See `Details` for more informations}
 }
}

\section{Details}{

   \code{TheiaAuth$new(auth.file)} Create a new instance of the class

   \code{a$token()} Return the current token or generate a next one if it has
   expired


   This class is used to manage authentication to Theia website, without
   intervention from the user. Login and password must be stored in a
   separate text file with these two lines:
   
   login
   password
   
   File content is read each time authentication is needed (to request a new
   token), so login and password are not stored in R's memory. If this file
   does not exist, R will prompt you to enter your login and password and
   will create the file.
}

\examples{

\dontrun{
# create an authentication object
myauth <- TheiaAuth$new("path_to_auth_file.txt")

# show the access token (and request a new one if needed)
myauth$token
}

}
