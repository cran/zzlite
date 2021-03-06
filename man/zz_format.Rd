% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zz_format.R
\name{zz_format}
\alias{zz_format}
\title{Accepted formats from Zamzar}
\usage{
zz_format(origin = NULL, usr = NULL)
}
\arguments{
\item{origin}{The origin format you want to convert from.
If a valid argument is passed to \code{origin}, \code{zz_format()} returns a dataframe of:
\itemize{
\item \code{targets}: The formats your origin can be converted to.
\item \code{costs}: The cost for converting between your origin and a given target.
}

See also: \url{https://developers.zamzar.com/formats}

If no argument has been passed to \code{orign}, a dataframe containing all
the accepted formats by the Zamzar API is returned.

See: \url{https://developers.zamzar.com/formats}}

\item{usr}{The username/API key you are using. If not set, the function
will check if a key exists as a \code{ZAMZAR_USR} variable  in \code{.Renviron} and use that.

See: \url{https://developers.zamzar.com/user}}
}
\value{
Either a dataframe of formats that you can convert to, or a
dataframe of accepted origin formats.
}
\description{
Get dataframe of all the formats accepted by Zamzar. Alternatively, a dataframe of formats
you can convert an origin to.
}
\details{
Please note that a Zamzar key passed as argument to \code{usr} takes precedence over a
Zamzar key extracted from an \code{.Renviron}.
}
\examples{
\dontrun{
# Returns a single column dataframe of all the accepted formats
# for the origin param.
zz_format(usr = "key")

# Same as above (assuming a valid key in .Renviron).
zz_format()

# Returns an error since the origin argument isn't recognized by the Zamzar API.
zz_format(origin = "invalid_origin")

# Returns a dataframe of targets that origin can be converted to,
# and of the cost of converting to a given target.
zz_format(origin = "emf")
}
}
