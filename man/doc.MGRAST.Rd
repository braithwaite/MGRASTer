\name{doc.MGRAST}
\alias{doc.MGRAST}

\title{
Documentation for MG-RAST API
}

\description{
A utility to flexibly inspect the documentation tree of the MG-RAST API.
}

\usage{
doc.MGRAST(depth=1, stratum=NULL, head=NULL, ...)
}
\arguments{
  \item{depth}{show this many levels (single \code{integer})}
  \item{stratum}{show all subtrees matching this name (string)}
  \item{head}{show elements beginning only here (\code{character})}
  \item{...}{additional arguments passed to \code{str()}}
}

\details{
This documentation assumes familiarity with the MG-RAST API, which is described elsewhere.

The MG-RAST API is locally represented with nested \code{list}s that this function helps to explore.
It is a specialized version of \code{str()}.  Usage is best understood through the examples.

A length-one \code{character} vector is meant by "string", above.
}

\value{
None.  Output is printed to the screen, as with \code{str()}.
}

\seealso{
\code{\link{str}}, 
\code{\link{call.MGRAST}}, 
\code{\link{parse.MGRAST}}, 
\code{\link{MGRASTAPI}}
}

\author{
Daniel T. Braithwaite
}

\references{
\url{http://metagenomics.anl.gov} \cr
\url{http://api.metagenomics.anl.gov} \cr
\url{http://www.json.org}
}

\examples{
##  list resources, then list all requests of all resources
doc.MGRAST()
doc.MGRAST(2)

##  show detail for matrix resource (partial matching works)
doc.MGRAST (head="matrix")
doc.MGRAST (2, head="mat")

##  show options for all requests
doc.MGRAST (stratum="options")
doc.MGRAST (2, "options")

##  show options in detail for a specific request
doc.MGRAST (3, head=c("annot","seq","param","opt"))

##  show return values of all m5nr requests in detail
doc.MGRAST (5, "attributes.data", "m5nr", nchar.max=30)
}
