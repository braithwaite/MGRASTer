Package MGRASTer is a complete wrapper for R language access to 
the API of the MG-RAST metagenome annotation engine, part of the
US Department of Energy (DOE) Systems Biology Knowledge Base (KBase).
See: http://api.metagenomics.anl.gov
and: http://metagenomics.anl.gov
and: http://kbase.us

The primary functions made available are:

call.MGRAST <- 
function(resource, request, ..., args=NULL, file=NULL, parse=is.null(file), verify=parse, bugs=c("ignore","warn","stop","ask","report"), debug=FALSE, timeout=300, issue=TRUE)

parse.MGRAST <-
function(call.url)

is.filebased <- 
function(resource, request)
