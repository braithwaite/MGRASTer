\name{.MGRAST}
\alias{.MGRAST}
\alias{loadAPI}
\alias{rebuildAPI}
\docType{data}

\title{State Information for MG-RAST API}

\description{".MGRAST" is hidden in the package environment to hold information about the 
API structure and state.}

\details{The examples demonstrate suggested ways to query the object.  Default values are
distributed with the package, but it can also be rebuilt dynamically, with the means
indicated below.}

\examples{
\dontrun{
.MGRAST$api$matrix$organism
rebuildAPI("APIcopy.Rda")
API <- loadAPI("APIcopy.Rda")
}}
\keyword{datasets}
