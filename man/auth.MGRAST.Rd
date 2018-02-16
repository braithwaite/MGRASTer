\name{auth.MGRAST}
\alias{auth.MGRAST}

\title{
Provide Auth Key to MG-RAST API
}

\description{
Provide an auth key for access to private resources using the MG-RAST API.
}

\usage{
auth.MGRAST(key, file)
}
\arguments{
  \item{key}{value of key (string)}
  \item{file}{file containing key (string)}
}

\details{
Accessing private resources with the MG-RAST API requires an auth key.
Log in at \url{https://mg-rast.org} to create a key, 
via "Preferences" on the "Account Management" page.
Note you must click not only "generate new key" but also "set preferences"!
Also note the key is only valid for a limited time.

The key must be entered during every R session.
Call \code{auth.MGRAST()} specifying at most one of \code{key} or \code{file}.
To enter the key at an interactive prompt instead, both may be omitted.

A length-one \code{character} vector is meant by "string", above.
}

\value{
The key value that has been set.
}

\references{
\url{https://mg-rast.org} \cr
\url{https://api.mg-rast.org} \cr
}

\author{
Daniel T. Braithwaite
}

\seealso{
\code{\link{call.MGRAST}}
}

\examples{
\dontrun{
auth.MGRAST("j6FNL61ekNarTgqupMma6eMx5")
auth.MGRAST(file="my_key.txt")
auth.MGRAST()
}
}
