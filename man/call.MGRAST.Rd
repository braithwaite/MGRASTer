\name{call.MGRAST}
\alias{call.MGRAST}
\alias{parse.MGRAST}

\title{
Call MG-RAST API
}

\description{
Call a specified resource of the MG-RAST API,
or parse the URL of an API resource into component arguments.
}

\usage{
call.MGRAST(resource, request, ..., args=NULL, file=NULL, timeout=300, 
  parse=is.null(file), verify=parse, quiet=TRUE, issue=TRUE)
parse.MGRAST(url)
}
\arguments{
  \item{resource}{name of resource (string)}
  \item{request}{name of request (string)}
  \item{\dots}{required and/or optional arguments}
  \item{args}{more arguments (named \code{list})}
  \item{file}{file to save received resource (string)}
  \item{timeout}{call timeout in seconds (single \code{integer})}
  \item{parse}{attempt JSON parsing? (\code{logical})}
  \item{verify}{check completeness of received resource? (\code{logical})}
  \item{quiet}{print less messages? (\code{logical})}
  \item{issue}{issue the call? (\code{logical})}
  \item{url}{URL of an API resource (string)}
}

\details{
\code{resource} and \code{request} are required.
Some calls require further arguments,
which may be given in \code{\ldots} along with optional arguments.
These all must be named.
Arguments may also be given with \code{args}.
Partial argument names,
as well as partial values when drawn from a controlled vocabulary, are allowed.
Arguments in general should be strings, that is, length-one \code{character} vectors.
All are coerced by \code{as.character()}.

Some helpful scrubbing is applied to any argument named \code{id}.
Multiple \code{id}s given in a string or vector are separated,
and prefixes \code{mgl,mgm,mgp,mgs} are guessed and added.
When \code{verify=TRUE} the received resource is checked for completeness.
A good way to investigate unexpected results is setting \code{quiet=FALSE}
to print additional diagnostic messages.
Some calls require \code{file}, and it is optional for the rest.

\code{parse.MGRAST()} separates the URL of an API resource into named components.
If \code{x} is the URL of an API resource, 
then \code{do.call(call.MGRAST, parse.MGRAST(x))} retrieves that resource.
}

\value{
Invisibly, a \code{list} mirroring the JSON structure of the received resource,
or if \code{parse=FALSE} then the uninterpreted resource.
(For non-JSON resources \code{parse} is ignored.)
If \code{file} is provided, the resource is written there and the filename returned instead.
Parsed JSON is written in .rda format (with \code{save()})
and anything else is written as received.
If \code{issue=FALSE} then a URL for the specified resource is returned and no call is made.

For \code{parse.MGRAST()}, a named \code{list} including \code{resource}, 
\code{request}, and any other arguments extracted from the URL provided.
}

\seealso{
\code{\link{doc.MGRAST}}, 
\code{\link{MGRAST}}
}

\author{
Daniel T. Braithwaite
}

\references{
\url{https://mg-rast.org} \cr
\url{https://api.mg-rast.org} \cr
\url{https://www.json.org}
}

\examples{
\dontrun{
##  The calls below are adapted from the MG-RAST API documentation pages.
##  An example is provided for most resource types.
##  These examples are "not run" because they take too long.

##
##  Use of call.MGRAST(), parse.MGRAST(), and do.call() together

parse.MGRAST ("http://api.mg-rast.org/download/mgm4447943.3?stage=650")
ll <- list (id=4447943.3, stage=650)
call.MGRAST ("down", "set", args=ll)

print (tt <- tempfile())
parse.MGRAST ("http://api.mg-rast.org/download/mgm4447943.3?file=350.1")
call.MGRAST ("down", "inst", id=4447943.3, file=350.1, destfile=tt)
parse.MGRAST (call.MGRAST (issue=FALSE, "down", "inst", id=4447943.3, file=350.1, 
	destfile=tt))
unlink (tt)

parse.MGRAST ("http://api.mg-rast.org/library?limit=20&order=name")
call.MGRAST ("lib", "query", lim=20, ord="name")
do.call (call.MGRAST, 
	parse.MGRAST ("http://api.mg-rast.org/library?limit=20&order=name"))

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
	"http://api.mg-rast.org/m5nr/ontology?source=Subsystems&min_level=level3")
parse.MGRAST (paste0 ("http://api.mg-rast.org/m5nr/taxonomy",
	"?filter=Bacteroidetes&filter_level=phylum&min_level=genus"))
parse.MGRAST (paste0 ("http://api.mg-rast.org/m5nr/md5/",
	"000821a2e2f63df1a3873e4b280002a8?source=InterPro"))
parse.MGRAST ("http://api.mg-rast.org/m5nr/function/sulfatase?source=GenBank")
parse.MGRAST (
	"http://api.mg-rast.org/m5nr/sequence/MAGENHQWQGSIL?source=TrEMBL")

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
	"http://api.mg-rast.org/matrix/organism",
	"?id=mgm4447943.3&id=mgm4447192.3&id=mgm4447102.3&id=mgm4447103.3",
	"&group_level=family&source=RefSeq&result_type=abundance&evalue=15"))
parse.MGRAST (paste0 (
	"http://api.mg-rast.org/matrix/function",
	"?id=mgm4447943.3&id=mgm4447192.3&id=mgm4447102.3&id=mgm4447103.3",
	"&group_level=level3&source=Subsystems&result_type=abundance&identity=80",
	"&filter_level=phylum&filter=Firmicutes"))
parse.MGRAST (paste0 (
	"http://api.mg-rast.org/matrix/feature",
	"?id=mgm4447943.3&id=mgm4447192.3&id=mgm4447102.3&id=mgm4447103.3",
	"&source=KEGG&result_type=evalue&length=25"))

##
##  examples for still other resources

parse.MGRAST ("http://api.mg-rast.org/metadata/export/mgp128")
call.MGRAST ("metadata", "exp", id=128)

parse.MGRAST (
	"http://api.mg-rast.org/metagenome/mgm4447943.3?verbosity=metadata")
call.MGRAST ("metagenome","inst", id=4447943.3, verb="meta")

parse.MGRAST ("http://api.mg-rast.org/project?limit=20&order=name")
call.MGRAST ("proj", "query", lim=20, ord="name")

parse.MGRAST ("http://api.mg-rast.org/sample/mgs25823?verbosity=full")
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
