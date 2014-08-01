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
call.MGRAST(resource, request, ..., args=NULL, destfile=NULL, parse=is.null(destfile), 
  verify=parse, bugs=c("ignore", "warn", "stop", "ask", "report"), quiet=TRUE,
  timeout=300, issue=TRUE)
parse.MGRAST(call.url)
}
\arguments{
  \item{resource}{name of API resource (string)}
  \item{request}{name of request, valid for the specified resource (string)}
  \item{\dots}{required and/or optional parameters for the call}
  \item{args}{further parameters (\code{list})}
  \item{destfile}{file to save the retrieved resource (string)}
  \item{parse}{attempt JSON parsing of result? (\code{logical})}
  \item{verify}{check integrity of the returned object? (\code{logical})}
  \item{bugs}{reporting action for API-side problems -- not used}
  \item{quiet}{print debugging messages? (\code{logical})}
  \item{timeout}{number of seconds to allow for the call (single \code{integer})}
  \item{issue}{issue the call, or only return the constructed URL? (\code{logical})}
  \item{call.url}{URL for an API resource (string)}
}

\details{
This documentation assumes familiarity with the MG-RAST API, which is described elsewhere.

\code{resource} and \code{request} are required for all API calls.  Optional and required parameters may be 
given in \code{\ldots} and must be named.  In general, parameters should be length-one \code{character} vectors,
which is what is meant by "string", above.  
Regardless, parameters are coerced by
\code{as.character}.  Parameters may also be given with \code{param}, which must be a named
list.  Partial \code{resource}, \code{request}, and parameter names are matched wherever possible, as are
partial parameter values tied to a controlled vocabulary.

Parameters named \code{id}  are specially handled with some helpful scrubbing.  In particular, prefixes
\code{mgl}, \code{mgm}, \code{mgp}, and \code{mgs} are added as necessary, and multiple \code{id}s given in a vector are
separated.

For certain resource-request pairs, \code{destfile} is mandatory.  For all others, it is optional.
When \code{verify=TRUE} a check is made that all documented components for a resource-request
are actually received.

Setting \code{quiet=FALSE} can help shed light on unexpected results.

\code{parse.MGRAST()} tokenizes a valid API URL.  If \code{x} is a URL validly requesting an API resource, 
then \code{do.call(call.MGRAST, parse.MGRAST(x))} will retrieve that resource.
}

\value{
For \code{call.MGRAST()}, a \code{list} mirroring the JSON structure of the received resource (invisibly).
But if \code{parse=FALSE}, then the resource, uninterpreted.
For non-JSON resources, \code{parse} is ignored.

But if \code{destfile} is not \code{NULL} and the resource was successfully written to \code{destfile}, then \code{destfile}.
If \code{parse=TRUE} and relevant, the file is written in .rda format, otherwise raw (usually meaning, 
as legible text).

Only a URL for the specified resource, if \code{issue=FALSE}.

For \code{parse.MGRAST()}, a named \code{list} including \code{resource}, \code{request}, and further parameters for the 
indicated API call.
}

\seealso{
\code{\link{doc.MGRAST}}, 
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
\dontrun{
##  The calls below are adapted from the MG-RAST API documentation pages.
##  For most resources types, at least one example is present.
##  Note that resources are returned invisibly by call.MGRAST().
##  These examples are "not run" because they take too long.

##
##  using call.MGRAST(), parse.MGRAST(), and do.call() together

parse.MGRAST ("http://api.metagenomics.anl.gov/download/mgm4447943.3?stage=650")
ll <- list (id=4447943.3, stage=650)
call.MGRAST ("down", "set", args=ll)

print (tt <- tempfile())
parse.MGRAST ("http://api.metagenomics.anl.gov/download/mgm4447943.3?file=350.1")
call.MGRAST ("down", "inst", id=4447943.3, file=350.1, destfile=tt)
parse.MGRAST (call.MGRAST (issue=FALSE, "down", "inst", id=4447943.3, file=350.1, 
	destfile=tt))
unlink (tt)

parse.MGRAST ("http://api.metagenomics.anl.gov/library?limit=20&order=name")
call.MGRAST ("lib", "query", lim=20, ord="name")
do.call (call.MGRAST, 
	parse.MGRAST ("http://api.metagenomics.anl.gov/library?limit=20&order=name"))

##
##  accessing the "m5nr" resource

call.MGRAST ("m5", "ont", source="Sub", min="level3")
call.MGRAST ("m5", "tax", filter="Bacteroidetes", filter_lev="phylum", min="genus")
call.MGRAST ("m5", "md5", id="000821a2e2f63df1a3873e4b280002a8", source="InterPro")
call.MGRAST ("m5", "func", text="sulfatase", source="GenBank")
call.MGRAST ("m5", "seq", text="MAGENHQWQGSIL", source="TrEMBL")

##
##  parsing URLs for the calls above

parse.MGRAST (
	"http://api.metagenomics.anl.gov/m5nr/ontology?source=Subsystems&min_level=level3")
parse.MGRAST (paste0 ("http://api.metagenomics.anl.gov/m5nr/taxonomy",
	"?filter=Bacteroidetes&filter_level=phylum&min_level=genus"))
parse.MGRAST (paste0 ("http://api.metagenomics.anl.gov/m5nr/md5/",
	"000821a2e2f63df1a3873e4b280002a8?source=InterPro"))
parse.MGRAST ("http://api.metagenomics.anl.gov/m5nr/function/sulfatase?source=GenBank")
parse.MGRAST (
	"http://api.metagenomics.anl.gov/m5nr/sequence/MAGENHQWQGSIL?source=TrEMBL")

##
##  annotation data via the "matrix" resource,
##  and different ways to provide arguments

xx <- c (4447943.3, 4447192.3, 4447102.3, 4447103.3)
yy <- "4447943.3 4447192.3 4447102.3 4447103.3"
aa <- list (group="level3", source="Sub", res="ab", ident=80, 
	filter_lev="phylum", filter="Firmicutes")
call.MGRAST ("matrix", "org", id=xx, group="family", source="Ref", result="abund", eval=15)
call.MGRAST ("matrix", "func", id=xx, args=aa)
call.MGRAST ("matrix", "feat", id=yy, source="KEGG", result="ev", len=25)

##
##  parsing URLs for the calls above

parse.MGRAST (paste0 (
	"http://api.metagenomics.anl.gov/matrix/organism",
	"?id=mgm4447943.3&id=mgm4447192.3&id=mgm4447102.3&id=mgm4447103.3",
	"&group_level=family&source=RefSeq&result_type=abundance&evalue=15"))
parse.MGRAST (paste0 (
	"http://api.metagenomics.anl.gov/matrix/function",
	"?id=mgm4447943.3&id=mgm4447192.3&id=mgm4447102.3&id=mgm4447103.3",
	"&group_level=level3&source=Subsystems&result_type=abundance&identity=80",
	"&filter_level=phylum&filter=Firmicutes"))
parse.MGRAST (paste0 (
	"http://api.metagenomics.anl.gov/matrix/feature",
	"?id=mgm4447943.3&id=mgm4447192.3&id=mgm4447102.3&id=mgm4447103.3",
	"&source=KEGG&result_type=evalue&length=25"))

##
##  examples for still other resources

parse.MGRAST ("http://api.metagenomics.anl.gov/metadata/export/mgp128")
call.MGRAST ("metadata", "exp", id=128)

parse.MGRAST (
	"http://api.metagenomics.anl.gov/metagenome/mgm4447943.3?verbosity=metadata")
call.MGRAST ("metagenome","inst", id=4447943.3, verb="meta")

parse.MGRAST ("http://api.metagenomics.anl.gov/project?limit=20&order=name")
call.MGRAST ("proj", "query", lim=20, ord="name")

parse.MGRAST ("http://api.metagenomics.anl.gov/sample/mgs25823?verbosity=full")
call.MGRAST ("samp", "inst", id=25823, verb="full")

##
##  finally, examples for annotation downloads (they take substantial time)

print (tt <- tempfile())
call.MGRAST ("annot", "seq", id=4447943.3, eval=10, type="org", source="Swiss",
	destfile=tt)
call.MGRAST ("annot", "sim", id=4447943.3, ident=80, type="fu", source="KO", 
	destfile=tt)
unlink (tt)
}
}
