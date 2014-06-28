\name{MGRASTAPI}
\alias{MGRASTAPI}
\alias{.MGRAST}
\alias{rebuild.MGRAST}
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
load.MGRAST (file = API.file())
rebuild.MGRAST (file = API.filename)
}
\arguments{
  \item{file}{a file name, or "NULL"}
}

\details{
The environment ".MGRAST" contains a nested list structure "API" built from the 
documentation-qua-specification of the MG-RAST API published at 
"http://api.metagenomics.anl.gov".  A prebuilt version of this tree is distributed with
the "MGRASTer" package, to which the routines "call.MGRAST()" and parse.MGRAST()" refer.

"rebuild.MGRAST()" builds a new, accurate-up-to-the-minute copy of the documentation
tree, which may be desirable after the MG-RAST group implements a change or patch to 
the API.  However, to take effect, a new copy must be assigned into ".MGRAST" 
(as in the examples).

"load.MGRAST()" returns the tree as saved in a specified file, or the
version of the tree actually in use.
}

\value{
For "rebuild.MGRAST", the file name "file", or the new documentation tree itself when "file" is "NULL".
For "load.MGRAST", the copy of the API documentation tree stored in "file" (by 
default the prepackaged copy), but the copy active in the current session when "file" is "NULL".
}

\seealso{
"\link{doc.MGRAST}", 
"\link{call.MGRAST}", 
"\link{parse.MGRAST}"
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

rebuild.MGRAST ("my_copy.rda")			# save a new (possibly updated) copy
API <- load.MGRAST ("my_copy.rda")		# load it into the workspace
assign ("API", API, .MGRAST)			# put it into efileect
}}


