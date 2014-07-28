\name{MGRASTAPI}
\alias{MGRASTAPI}
\alias{.MGRAST}
\alias{build.MGRAST}
\alias{load.MGRAST}

\title{
Local Representation of MG-RAST API
}

\description{
The session copy of the documentation tree of the MG-RAST API, and tools for
dynamic update.
}

\usage{
.MGRAST
load.MGRAST(ff=API.file())
build.MGRAST(ff=API.filename)
}
\arguments{
  \item{ff}{a file name (string or \code{NULL})}
}

\details{
This documentation assumes familiarity with the MG-RAST API, which is described elsewhere.

The environment \code{.MGRAST} contains a nested list structure \code{API} built from the 
documentation-qua-specification of the MG-RAST API published at 
\url{http://api.metagenomics.anl.gov}.  A prebuilt version of this tree is distributed with
the MGRASTer package, which the routines \code{call.MGRAST()} and \code{parse.MGRAST()} consult.

\code{build.MGRAST()} builds a new, accurate-up-to-the-minute copy of the documentation
tree, which may be desirable to do after a change or patch is implemented in the API.  
However, to take effect, a new copy must be assigned into \code{.MGRAST} 
(as in the examples).

\code{load.MGRAST()} returns the tree as saved in a specified file, or the
version of the tree actually in use.
}

\value{
For \code{build.MGRAST()}, the file name \code{ff}, or the new documentation tree itself when \code{ff=NULL}.
For \code{load.MGRAST()}, the copy of the API documentation tree stored in \code{ff} 
(by default the prepackaged copy), 
but the copy active in the current session when \code{ff=NULL}.
}

\seealso{
\code{\link{doc.MGRAST}}, 
\code{\link{call.MGRAST}}, 
\code{\link{parse.MGRAST}}
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
\dontrun{
API <- get ("API", .MGRAST)				# get copy currently in use
API <- load.MGRAST()					# get copy distributed with the package

build.MGRAST ("my_copy.rda")			# save a new (possibly updated) copy
API <- load.MGRAST ("my_copy.rda")		# load it into the workspace
assign ("API", API, .MGRAST)			# put it into efileect
}}


