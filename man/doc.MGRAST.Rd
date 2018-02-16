\name{doc.MGRAST}
\alias{doc.MGRAST}

\title{
Read Documentation for MG-RAST API
}

\description{
Display sections of the documentation tree of the MG-RAST API.
}

\usage{
doc.MGRAST(depth=1, head=NULL, stratum=NULL, ...)
}
\arguments{
  \item{depth}{show this many levels (single \code{integer})}
  \item{head}{show elements beginning only here (\code{character})}
  \item{stratum}{show all subtrees matching this name (string)}
  \item{...}{additional arguments passed to \code{str()}}
}

\details{
The MG-RAST API is locally represented with as a nested list structure or "tree".
This function is a souped-up version of \code{str()}
to help examine parts of that structure.
Usage is best understood through the examples.
A length-one \code{character} vector is meant by "string", above.
}

\value{
None.  Output is printed to the screen, as with \code{str()}.
}

\seealso{
\code{\link{str}}, 
\code{\link{call.MGRAST}},
\code{\link{MGRAST}}
}

\author{
Daniel T. Braithwaite
}

\references{
\url{https://mg-rast.org} \cr
\url{https://api.mg-rast.org} \cr
}

\examples{
##  list resources, then list all requests of all resources
doc.MGRAST()
doc.MGRAST(2)

##  show detail for matrix resource (partial matching works)
doc.MGRAST (head="matrix")
doc.MGRAST (2, "mat")

##  show options for all requests
doc.MGRAST (stratum="options")
doc.MGRAST (2, stratum="options")

##  show options in detail for a specific request
doc.MGRAST (3, c("annot","seq","param","opt"))

##  show return values of all m5nr requests in detail
doc.MGRAST (5, "m5nr", "attributes.data", nchar.max=30)
}
