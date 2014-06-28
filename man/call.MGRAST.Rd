\name{call.MGRAST}
\alias{call.MGRAST}
\alias{parse.MGRAST}

\title{
Interface to MG-RAST API
}

\description{
An interface to the MG-RAST API enabling convenient access in the R
environment to provided resources, as well as facilitating export.
}

\usage{
call.MGRAST(resource, request, ..., param=NULL, file=NULL, parse=is.null(file), verify=parse, 
  bugs=c("ignore", "warn", "stop", "ask", "report"), quiet=TRUE, timeout=300, issue=TRUE)
parse.MGRAST(call.url)
}
\arguments{
  \item{resource}{name of API resource (string)}
  \item{request}{name of request, valid for the specified resource (string)}
  \item{\dots}{required and/or optional parameters for the call}
  \item{param}{further parameters ("list")}
  \item{file}{file to save the retrieved resource (string)}
  \item{parse}{attempt JSON parsing of result? ("logical")}
  \item{verify}{check integrity of the returned object? ("logical")}
  \item{bugs}{reporting action for API-side problems -- not used}
  \item{quiet}{print debugging messages? ("logical")}
  \item{timeout}{number of seconds to allow for the call}
  \item{issue}{issue the call, or only return the constructed URL? ("logical")}
  \item{call.url}{URL for an API resource (string)}
}

\details{
This documentation assumes familiarity with the MG-RAST API, which is described elsewhere.

"resource" and "request" are required for all API calls.  Optional and required parameters may be 
given in "\ldots" and must be named.  In general, parameters should be length-one "character" vectors.  At any rate, they may be coerced by
"as.character".  Parameters may also be given with "param", which must be a named
list.  Partial "resource", "request", and parameter names are matched wherever possible, as are
partial parameter values tied to a controlled vocabulary.

The "id" parameter is specially handled with some helpful scrubbing.  In particular, prefixes
"mgl", "mgm", "mgp", and "mgs" are added as necessary, and multiple "id"s given in a vector are
separated.

For certain resource-request pairs, "file" is mandatory.  For all others, it is optional.
When "verify" is "TRUE" a check is made that all documented components for a resource-request
are actually received.

Setting "quiet" to "FALSE" can help shed light on unexpected results.

"parse.MGRAST" tokenizes a valid API URL.  If "x" is a URL validly requesting an API resource, 
then "do.call (call.MGRAST, parse.MGRAST (x))" will retrieve that resource.
}

\value{
For "call.MGRAST", a "list" mirroring the JSON structure of the received resource (invisibly).
But if "parse" is "FALSE", then the resource, uninterpreted.
For non-JSON resources, "parse" is ignored.

But if "file" is not "NULL" and the resource was successfully written there, then "file".
(If parse is "TRUE" and relevant, the file is written in .rda format, otherwise raw (usually meaning, 
as legible text.)  Only a URL for the specified resource, if "issue" is "FALSE".

For "parse.MGRAST", a named "list" including "resource", "request", and further parameters for the 
indicated API call.
}

\seealso{
"\link{doc.MGRAST}", 
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
\dontrun{
### --------> COMPLETE THESE ---- as prov

### note that resources returned by these calls will be invisible.
### use print(<call>) to see them.

### list available analysis files for a metagenome
call.MGRAST('download', 'setlist', 4447943.3)
call.MGRAST('do', 'se', 4447943.3, stage=650)

### save a specific stage of processing (into a file)
call.MGRAST('do', 'ins', 4447943.3, fi=350.1, file="350.genecalling.coding.faa.gz")

### retrieve some annotation data (into the R session)
call.MGRAST()

### retrieve same data, for different metagenomes, using a parameter list
call.MGRAST()

### query the M5NR for a particular function
call.MGRAST('m5nr', 'tax', filter='Bacteroidetes', filter_lev='phylum', mi='genus')

### get headers for MG-RAST projects
call.MGRAST('proj', 'query', lim=1000, ord='na')

### get extended metadata for a specific project
call.MGRAST()

### save unparsed JSON of same metadata
call.MGRAST()

### save parsed JSON of same metadata (Rda format)
# call.MGRAST()

### just return the URL without retrieving anything
call.MGRAST(issue=FALSE)
}
}
