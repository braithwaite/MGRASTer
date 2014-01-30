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
	resource, 										# what resource
	request=NULL, 									# what request (there is a default for some (all?) resources)
	..., 											# required and optional parameters appropriate to the resource requested
	param = NULL, 		 							# pass parameters in a list instead
	parse=TRUE, 									# parse JSON objects
	verify=parse,  									# attempt to validate resource structure and contents?
	bugs=c("ignore","warn","stop","ask","report"),	# report bugs
	issue=TRUE,										# issue the call or not
	file=NULL) {									# file destination for received resource

require(RJSONIO)
require(RCurl)
require(matR)

### just to save typing:
server.path <- .session$server()
api <- .session$api()
resources <- names(api)

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
x <- resources [pmatch(resource, resources)]
if(is.na(x)) stop("invalid resource: ", resource)
resource <- x

### assign default "request" if unspecified
if(is.null(request))
	request <- switch(resource,
		annotation="sequence",
		compute="normalize",
		download="instance",
		inbox="view",
		library="instance",
		m5nr="ontology",
		matrix="function",
		metadata="export",
		metagenome="instance",
		profile="instance",
		project="instance",
		sample="instance",
		validation="data",
		status="instance")

### partial-match request
x <- names(api[[resource]]) [pmatch(request, names(api[[resource]]))]
if(is.na(x)) stop("invalid request: ", request, " for resource: ", resource)
request <- x

cat("resource:", resource,
	"---request:", request,
	"---required:", paste(names(api[[resource]][[request]]$parameters$required), collapse="/"),
	"---options:", paste(names(api[[resource]][[request]]$parameters$options), collapse="/"), 
	"\n", sep="")

### combine parameters given in "..." with those given in "param"
param <- unlist(append(list(...), param))

## partial-match params
if (length (param) > 0) {
	cat("***given***\n")
	print(param)

	pnames <- union(names(api[[resource]][[request]]$parameters$required),
					names(api[[resource]][[request]]$parameters$options))
	x <- pnames [pmatch(names(param), pnames, dup=TRUE)]
	if (any (is.na (x))) stop("parameter(s) invalid or unidentified: ", paste(names(param) [is.na (x)], collapse=" "))
	names(param) <- x
	cat("***matched***\n")
	print(param)
}

required <- names(param) %in% names(api[[resource]][[request]]$parameters$required)
cat("required:      ", names(param)[required],"\n")
optional <- names(param) %in% names(api[[resource]][[request]]$parameters$options)
cat("options:       ", names(param)[optional],"\n")

# check required parameters present
check.required <- names(api[[resource]][[request]]$parameters$required) %in% names(param)
if(!all(check.required)) 
	warning("required parameter(s) missing: ", 
			names(api[[resource]][[request]]$parameters$required)[!check.required])

# DO IDs REQUIRE SPECIAL HANDLING HERE?  I remember something bizarre like http://path/path/id;id;id?options
# let's assume not.

required.str <- paste (param[required], sep="/")
cat("required string:  ", required.str, "\n")
optional.str <- paste(names(param)[optional], param[optional], sep="=", collapse="&")
cat("optional string:  ", optional.str,"\n")

path <- paste(server.path, resource, sep="/")
if (length(request) > 0)
	path <- paste(path, request, sep="/")
if (length(required.str) > 0)
	path <- paste(path, required.str, sep="/")
call.url <- path
if(length(optional.str) > 0)
	call.url <- paste(call.url, optional.str, sep="?")
# cat(call.url,"\n\n")

if(!issue) return(call.url)

# REPLACE WITH IMPLEMENTATION VIA RCURL

# DOWNLOAD NEEDS TESTING

if(is.null(file)) x <- readLine(call.url, warn=TRUE)
else x <- download.file(call.url, file, quiet=FALSE)

# PARSING NEEDS TESTING

if(!parse) return(x)
if (!isValidJSON(x, asText=TRUE)) warning("valid JSON not received")
px <- fromJSON(x, asText=TRUE, simplify=TRUE)
if(!verify) return(px)

# CONFORMITY TEST NEEDS TESTING

if(!is.conforming(resource, request, px, quiet=TRUE)) stop("aborting due to non-conforming resource")

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
