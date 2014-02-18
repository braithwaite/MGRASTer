\name{.MGRAST}
\alias{.MGRAST}
\alias{.build.API.tree}
\alias{.my.name}
\alias{.read.API.tree}
\docType{data}

\title{State Information for MG-RAST API}

\description{".MGRAST" is hidden in the package environment to hold information about the 
API structure and state.}

\details{The examples demonstrate suggested ways to query the object.  Default values are
distributed with the package, but it can also be rebuilt dynamically, with the means
indicated below.}

\examples{
.MGRAST$server()
str(.MGRAST$api())
.MGRAST$resources()
.MGRAST$requests()
.MGRAST$examples()
str(.MGRAST$required())
str(.MGRAST$options())
str(.MGRAST$cv())
str(.MGRAST$attributes())

\dontrun{
.MGRAST$api(renew = TRUE)
newAPIcopy <- .build.API.tree()
.build.API.tree(save=TRUE, "API.Rda")
}}
\keyword{datasets}
