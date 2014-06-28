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
  \item{depth}{show this many levels (single "integer")}
  \item{stratum}{show all subtrees matching this name (string)}
  \item{head}{show elements beginning only here ("character" vector)}
  \item{...}{additional arguments passed to "str"}
}

\details{
The MG-RAST API is locally represented with nested "list"s that this function helps to explore.
Usage is best understood through the examples.  It helps to be familiar with the "str" function, 
especially as applied to nested lists.
}

\value{
None.  Output is printed to the screen, as with "str".
}

\seealso{
"\link{str}", 
"\link{call.MGRAST}", 
"\link{parse.MGRAST}", 
"\link{MGRASTAPI}"
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
##  sth
doc.MGRAST()
doc.MGRAST(2)

##  sth
doc.MGRAST (head = "matrix")
doc.MGRAST (head = "mat")
doc.MGRAST (2, head = "matrix")

doc.MGRAST (stratum = "options")
doc.MGRAST (2, stratum = "options")

doc.MGRAST (2, head = c('annot','seq','param','opt'))
doc.MGRAST (3, head = c('annot','seq','param','opt'))

doc.MGRAST (5, s="attributes.data", h="m5nr", nchar.max=30)
}
