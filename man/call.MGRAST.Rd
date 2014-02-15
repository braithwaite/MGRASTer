\name{call.MGRAST}
\alias{call.MGRAST}

\title{API Client for the MG-RAST Annotation Server}
\description{
A complete wrapper for the MG-RAST API.  Access to API resources is made
convenient to the R environment.  Export of MG-RAST data from the R session 
is facilitated.
}
\usage{
call.MGRAST(resource = .MGRAST$resources(), 
request = switch(match.arg(resource), annotation = "sequence", compute = "normalize", download = "instance", inbox = "view", library = "query", m5nr = "sources", matrix = "function", metadata = "template", metagenome = "query", profile = "info", project = "query", sample = "query", validation = "template", status = "info"), 
..., param = NULL, file = NULL, parse = is.null(file), verify = parse, bugs = c("ignore", "warn", "stop", "ask", "report"), debug = FALSE, 
issue = TRUE)
}
\arguments{
  \item{resource}{string naming an API resource}
  \item{request}{string naming a request valid for the specified resource}
  \item{\dots}{required and/or optional parameters to the API call}
  \item{param}{required and/or optional parameters (list)}
  \item{file}{name of a file to save the retrieved resource}
  \item{parse}{attempt JSON parsing of result? (logical)}
  \item{verify}{check integrity of the returned object? (logical)}
  \item{bugs}{reporting action for API-side problems}
  \item{debug}{print debugging messages? (logical)}
  \item{issue}{issue the call, or only return the constructed URL? (logical)}
}
\details{
Each parameter included in "\ldots" and every element of "param" should be named.
However, unnamed parameters are taken as the call's required parameters, if any.
All parameters will be coerced with "as.character".
Parameters should be length-one.
However, resource IDs of length greater than one are handled appropriately; see the examples.
Partial parameter names and values are completed wherever possible.
}
\value{
If "parse=TRUE" and the resource was successfully interpreted as JSON text, then a list mirroring the JSON structure.
If "parse=FALSE", then the resource as read from the connection.
The character string "file", if not "NULL" and the resource was successfully written there.
Only a URL for the specified resource, if "issue=FALSE".
Note that resources are returned invisible.
}
\references{
\url{http://metagenomics.anl.gov},
\url{http://api.metagenomics.anl.gov},
\url{http://www.json.org}
}
\author{Daniel T. Braithwaite}
\examples{
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
