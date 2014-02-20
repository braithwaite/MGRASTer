##############################################################################
### utility for rebuilding API structure for distribution
##############################################################################

### --------> ADD HERE


##############################################################################
### for the loading scheme, also see data/session.R
##############################################################################

.onLoad <- function (libname, pkgname) { }

.onAttach <- function (libname, pkgname) { 
	packageStartupMessage(pkgname, " (", packageVersion(pkgname), " build XXXXXXX)")
}

##############################################################################
### an all-purpose interface for the MG-RAST API
##############################################################################

call.MGRAST <- function (
	resource = .MGRAST$resources(), 					# what resource
	request = .MGRAST$requests() [[match.arg(resource)]],	# what request
	..., 									# parameters
	param = NULL, 		 					# parameters in a list (instead or additionally)
	file=NULL,								# output to file
	parse=is.null(file), 					# parse JSON?
	verify=parse,  									# check retrieved object?
	bugs=c("ignore","warn","stop","ask","report"),	# report bugs?
	debug=FALSE,
	issue=TRUE							# issue the call?
	) {									# save to file

#-------just to save typing:
server <- .MGRAST$server()
api <- .MGRAST$api()
resources <- .MGRAST$resources()
requests <- .MGRAST$requests()
examples <- .MGRAST$examples()
required <- .MGRAST$required()
options <- .MGRAST$options()
cv <- .MGRAST$cv()
attributes <- .MGRAST$attributes()

# ERRORS and BUGS
# 
# error.handler <- function () 
# 	switch(match.arg(bugs),
# 		ignore=function(...) {1},
# 		warn=warning,
# 		stop=stop,
# 		report=function (...) { 
# 			bug.report(subject="API bug auto-report", address="mg-rast@mcs.anl.gov", package="matR", method="mailto")
# # cf. ?bug.report and ?create.post
# 			message("bug reporting is not yet implemented")
# 			stop(...)
# 			})

#-------match initial arguments
resource <- match.arg(resource)
request <- match.arg(request, requests[[resource]])
if (debug)
	cat(resource, "|", request, " ::: ",
		paste(required[[resource]][[request]], collapse=" "), " ::: ",
		paste(options[[resource]][[request]], collapse=" "), "\n", sep="")

#-------combine parameters from "..." and "param"
#-------and convert numbers (such as IDs) to strings
#-------"param" remains "list" to accommodate vectors of IDs
param <- append(list(...), param)
if (length (param) > 0) {
	param <- sapply (param, as.character)

#-------use names of required parameters for unnamed parameters (typically "id" or "text")
	if (is.null(names(param)))
		is.na(names(param)) <- TRUE
	j <- is.na(names(param)) | (names(param) == "")
	if (any (j)) 
		names(param)[j] <- required[[resource]][[request]]

#-------now in the list all parameters have a (possibly abbreviated) name; match them
	targets <- union(required[[resource]][[request]], options[[resource]][[request]])
	x <- targets [pmatch(names(param), targets, duplicates.ok=TRUE)]
	if (any(is.na(x)))
		warning("no match (or not unique) for parameter(s): ", paste(names(param)[is.na(x)], collapse=" "))
	names(param) <- x
	if (debug)
		cat(paste(names(param),"=",param,collapse=" ",sep=""), "\n")

#-------handle param(s) named "id" specially, by breaking apart vectors and add prefixes
	if("id" %in% names(param)) {
		ids <- param [names(param) == "id"]
		prefix <- switch (resource,
			annotation=,compute=,download=,matrix=,metagenome="metagenome",
			library=,project=,sample=resource,default=NULL)
		if (!is.null (prefix)) {
			ids <- as.vector (sapply(ids, scrubIDs, prefix))
			is.na(names(ids)) <- TRUE
			names(ids)[] <- "id"
			param <- append (param [names(param) != "id"], ids)
		}
	}

#-------check required parameters present
	required.index <- names(param) %in% required[[resource]][[request]]
	optional.index <- names(param) %in% options[[resource]][[request]]
	check.required <- required[[resource]][[request]] %in% names(param)
	if(!all(check.required)) 
		warning("required parameter(s) missing: ", 
				required[[resource]] [[request]] [!check.required])

#-------match to controlled vocabularies
	for (j in 1:length(param))
		if (optional.index[j]) {
			vocab <- cv[[resource]] [[request]] [[names(param)[j]]]
			if (length (vocab) > 0) {
				x <- vocab [pmatch(param[[j]], vocab)]
				if (is.na (x))
					warning("no match in controlled vocabulary: ", param[[j]])
				param[[j]] <- x
			}
		}

	required.str <- paste(param[required.index], sep="/")
	optional.str <- paste(names(param)[optional.index], param[optional.index], sep="=", collapse="&")
}
else {
	required.str <- character(0)
	optional.str <- character(0)
}

#-------construct the complete URL string
path <- paste(server, resource, sep="/")
#-------attach request name in the path
if (length(request) > 0)
#-------but omit request names that the API (inconsistently) does not want in the path
	if (! resource %in% c("download","library","metagenome","project","sample"))
#-------and omit "info" where it (inconsistently) fails to work: 
		if (! (resource %in% c('download','library','metagenome','profile','project','sample','status')
			&& request == "info"))
				path <- paste(path, request, sep="/")
#-------attach required parameters in the path
if (length(required.str) > 0)
	path <- paste(path, required.str, sep="/")
call.url <- path
#-------attach options
if(length(optional.str) > 0)
	call.url <- paste(call.url, optional.str, sep="?")

if(!issue) return(call.url)

#-------ADD RCURL CODE for resources requiring a header
# require(RCurl)

#-------warn if a resource/request should be saved to file but is not
if (is.null(file))
	if((resource == "annotation" && request %in% c("sequence","similarity")) ||
		(resource == "download" && request == "instance"))
		warning("a file name should be specified for this request")
if(!is.null(file)) {
	download.file(call.url, file, quiet=!debug)
	if (parse || verify)
		message("saved resource to file and ignored parse= or verify=TRUE")
	return(file)
}
x <- readLines(call.url, warn=debug)
if(!parse) return(invisible(x))

require(RJSONIO)
if (!isValidJSON(x, asText=TRUE)) warning("resource does not pass JSON validation")
px <- fromJSON(x, asText=TRUE, simplify=TRUE)
if(!verify) return(invisible(px))

#-------do not verify content for certain resources
if (request %in% c("info") ||
	resource %in% c("annotation","inbox","validation") ||
	all (c(resource,request) == c("download","instance")))
	message("ignoring verify=TRUE for inapplicable resource")
else {
	diff <- setdiff (names (x), attributes [[resource]] [[request]])
	if(length(diff) > 0)
		warning("resource is missing component(s): ", paste(diff, collapse=" "))
}

invisible(px)
}


##############################################################################
### chrome-plated ID scrubbing
##############################################################################

scrubIDs <- function (IDs, resources = "metagenome") {
	IDs <- unlist (sapply (as.list (IDs), strsplit, "[^[:alnum:]\\.]+"))
	names <- names (IDs)
	prefixes <- rep (resources, length.out = length (IDs))
	prefixes <- match.arg (prefixes, c ("metagenome", "project", "sample", "library"), TRUE)
	lookup <- c (metagenome="mgm", project="mgp", sample="mgs", library="mgl")
	scrubbed <- paste (ifelse (substr(IDs, 1, 3) %in% lookup, "", lookup[prefixes]), 
		IDs, sep = "")
	names (scrubbed) <- names (IDs)
	scrubbed
}
