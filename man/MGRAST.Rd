\name{MGRAST}
\alias{MGRAST}
\alias{.MGRAST}
\alias{build.MGRAST}
\alias{load.MGRAST}

\title{
Implementation Details of MG-RAST API
}

\description{
Manage implementation of the MG-RAST API via its local representation.
}

\usage{
load.MGRAST(file=API.filepath())
build.MGRAST(file=API.filename)
.MGRAST
}
\arguments{
  \item{file}{a file name (string)}
}

\details{
The MG-RAST API is developed in a dynamic environment, 
so stability of interface with respect to the R platform is an important concern.
Handling unanticipated API revisions and patches gracefully is desirable.
Naturally a new package version will always be compatible with a current API version,
so this consideration applies to intervals between API and package updates.

The environment \code{.MGRAST} contains a nested list structure \code{API}
mirroring the API specification published at \url{https://api.mg-rast.org}.
This "tree", consulted by \code{call.MGRAST()} and \code{parse.MGRAST()}, is local and prebuilt.
However, build.MGRAST() and load.MGRAST() provide some degree of 
adaptability to minor API changes by managing this local representation.

\code{build.MGRAST()} retrieves, structures, and saves an updated API tree.
The examples demonstrate how to make it take effect.
\code{load.MGRAST()} returns the tree saved in a file or the tree actually in use.
}

\value{
For \code{build.MGRAST()}, the file name \code{file},
or the new tree itself when \code{file=NULL}.
For \code{load.MGRAST()}, the tree stored in \code{file}
(by default the prebuilt version),
or the session's active version when \code{file=NULL}.
}

\seealso{
\code{\link{doc.MGRAST}}, 
\code{\link{call.MGRAST}}
}

\author{
Daniel T. Braithwaite
}

\references{
\url{http://mg-rast.org} \cr
\url{http://api.mg-rast.org} \cr
\url{http://www.json.org}
}

\examples{
\dontrun{
##  get API currently in use
API <- get ("API", .MGRAST)

##  compare to API distributed with the package
identical (API, load.MGRAST())

##  build and save a new (possibly updated) copy
build.MGRAST ("my_api_copy.rda")

##  load it
API <- load.MGRAST ("my_api_copy.rda")

##  put it into effect
assign ("API", API, .MGRAST)			
}}
