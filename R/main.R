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


recommendations to repair broken API syntax

<request> is required in the path for <resource>="matrix" and others.  However:

For <resource>="sample" the API currently accepts:

(1) http://api.metagenomics.anl.gov/sample/				(<request>="info")
(2) http://api.metagenomics.anl.gov/sample?limit=20&order=name		(<request>="query")
(3) http://api.metagenomics.anl.gov/sample/mgs25823			(<request>="instance")

and does not accept:

(4) http://api.metagenomics.anl.gov/sample/info			(<request>="info")
(5) http://api.metagenomics.anl.gov/sample/query		(<request>="query")
(6) http://api.metagenomics.anl.gov/sample/instance/mgs25823	(<request>="instance")

Recommendation:
It should not accept (2) or (3), and it should accept (4), (5), (6).
<request> should be required in the path for all <resource>s.

The result of the status quo is that "optional" parameters are not optional for <request>="query".

That is aside from confusion caused by inconsistent syntax.  Related problems exist
for <resource> = 'download','library','metagenome','profile','project','sample','status'



call.MGRAST <- function (
	resource = c("m5nr", names (.session$api())), 					# what resource
	request = switch (match.arg (resource),
#	pmatch (resource, names (.session$api())),
			annotation="sequence",  ?
			compute="normalize",    ?
			download="instance",    ?
			inbox="view",           ?
			library="query",
			m5nr="sources",
			matrix="function",
			metadata="template",
			metagenome="query",
			profile="info",
			project="query",
			sample="query",
			validation="template",
			status="info"),							# what request (there is a default for some (all?) resources)
	..., 										# required and optional parameters appropriate to the resource requested
	param = NULL, 		 							# pass parameters in a list instead
	parse=TRUE, 									# parse JSON objects
	verify=parse,  									# attempt to validate resource structure and contents?
	bugs=c("ignore","warn","stop","ask","report"),					# report bugs
	issue=TRUE,										# issue the call or not
	file=NULL) {									# file destination for received resource

### just to save typing:
api <- .session$api()
resources <- names(api)
server.path <- .session$server()

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

### partial-match resource
# x <- resources [pmatch(resource, resources)]
# if(is.na(x)) stop("invalid resource: ", resource)
# resource <- x
x <- list()
resource <- match.arg(resource)

### partial-match request
# x <- names(api[[resource]]) [pmatch(request, names(api[[resource]]))]
# if(is.na(x)) stop("invalid request: ", request, " for resource: ", resource)
# request <- x
request <- match.arg(request, names(api[[resource]]))

cat(resource, "/", request, "-",
	"-[", paste(names(api[[resource]][[request]]$parameters$required), collapse="/"), "]-",
	"-[", paste(names(api[[resource]][[request]]$parameters$options), collapse="/"), "]-\n", sep="")

### combine parameters given in "..." with those given in "param"
--> check functionality of as.character here
param <- as.character (unlist(append(list(...), param)))

### partial-match params
if (length (param) > 0) {
	pnames <- union(names(api[[resource]][[request]]$parameters$required),
					names(api[[resource]][[request]]$parameters$options))
	x <- pnames [pmatch(names(param), pnames, dup=TRUE)]
	if (any (is.na (x))) stop("parameter(s) invalid or unidentified: ", paste(names(param) [is.na (x)], collapse=" "))
	names(param) <- x
	cat("matched: ", paste(names(param),"=",param,sep="",collapse="/"), "\n")
}

# cv-based value matching
--> api[[resource]][[request]]$parameters$required[[param]]$cv
--> api[[resource]][[request]]options[[param]]$cv

# where parameters are unnamed, try giving them the names of required parameters

required <- names(param) %in% names(api[[resource]][[request]]$parameters$required)
optional <- names(param) %in% names(api[[resource]][[request]]$parameters$options)
# cat("required:      ", names(param)[required],"\n")
# cat("options:       ", names(param)[optional],"\n")

### check required parameters present
check.required <- names(api[[resource]][[request]]$parameters$required) %in% names(param)
if(!all(check.required)) 
	warning("required parameter(s) missing: ", 
			names(api[[resource]][[request]]$parameters$required)[!check.required])

# add 'mgp' etc prefixes to IDs
if (cases under which we expect an ID) scrubIDs(, resource)

# also allow IDs to be vectors of length > 1

### do ID's require special handling here?  
### I remember something bizarre like http://path/path/id;id;id?options
### let's assume not.

required.str <- paste (param[required], sep="/")
optional.str <- paste(names(param)[optional], param[optional], sep="=", collapse="&")
# cat("required string:  ", required.str, "\n")
# cat("optional string:  ", optional.str,"\n")

path <- paste(server.path, resource, sep="/")
if (length(request) > 0)
--> # make exception for cases where request is omitted in tha path:  query/instance
--> request %in% c ("query","instance")
	path <- paste(path, request, sep="/")
if (length(required.str) > 0)
	path <- paste(path, required.str, sep="/")
call.url <- path
if(length(optional.str) > 0)
	call.url <- paste(call.url, optional.str, sep="?")
	
if(!issue) return(call.url)

# REPLACE WITH IMPLEMENTATION VIA RCURL
# DOWNLOAD NEEDS TESTING
# PARSING NEEDS TESTING
# CONFORMITY TEST NEEDS TESTING

require(RCurl)
if(is.null(file)) x <- readLines(call.url, warn=FALSE)
else x <- download.file(call.url, file, quiet=FALSE)

if(!parse) return(x)

require(RJSONIO)
if (!isValidJSON(x, asText=TRUE)) warning("valid JSON not received")
px <- fromJSON(x, asText=TRUE, simplify=TRUE)
if(!verify) return(px)

# if(!is.conforming(resource, request, px, quiet=TRUE)) stop("aborting due to non-conforming resource")

px
}


is.conforming <- function (resource, request, object, quiet=FALSE) {

### just to save typing:
api <- .session$api()
server.path <- .session$server()

# validation currently is only by presence/absence of top-level fields.
# more sophisticated validation would looke something like:
# errstr <- "not enough columns"
# warning(paste("non-conforming API resource received:",errstr))
# stop("fatally non-conforming API resource received")

check.content <- names(api[[resource]][[request]]$attributes) %in% names(object)
missing.content <- names(api[[resource]][[request]]$attributes) [!check.content]

# REFINE WITH A RECURSIVE CHECK FOR FIELDS WITHIN FIELDS
if (all (check.content)) return(TRUE)

# TWEAK: for approved exceptions, do not throw an error
subtype <- paste(resource, request, "/")
if (switch(subtype, 
	"matrix/function" = identical(missing.content,"generated_by"),
	"matrix/organism" = identical(missing.content,"date"),
	default=FALSE)) return(TRUE)

if (!quiet) warning("API resource of type ", subtype, " missing content: ", missing.content)
return(FALSE)
}
