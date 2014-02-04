# ID required but not documented for compute/alphadiversity call
# repeated examples in m5nr resource?
# validation <template> and validation <data> work without ID but it is a required param

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
	request = switch (match.arg (resource),				# what request
			annotation="sequence", 				 #?
			compute="normalize",   				 #?
			download="instance",   				 #?
			inbox="view",          				 #?
			library="query",
			m5nr="sources",
			matrix="function",
			metadata="template",
			metagenome="query",
			profile="info",
			project="query",
			sample="query",
			validation="template",
			status="info"),
	..., 									# parameters
	param = NULL, 		 					# parameters in a list (instead or additionally)
	parse=TRUE, 							# parse JSON?
	verify=parse,  									# check retrieved object?
	bugs=c("ignore","warn","stop","ask","report"),	# report bugs?
	issue=TRUE,										# issue the call?
	file=NULL) {									# save to file

#-------just to save typing:
api <- .MGRAST$api()
resources <- .MGRAST$resources()
server <- .MGRAST$server()
requests <- .MGRAST$requests()
examples <- .MGRAST$examples()
required <- .MGRAST$required()
options <- .MGRAST$options()
cv <- .MGRAST$cv()

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
request <- match.arg(request, resources[[res]]))

cat(resource, "/", request, "-",
	"-[", paste(names(api[[resource]][[request]]$parameters$required), collapse="/"), "]-",
	"-[", paste(names(api[[resource]][[request]]$parameters$options), collapse="/"), "]-\n", sep="")

#-------combine parameters from "..." and "param"
#-------and convert numbers (such as IDs) to strings
param <- as.character (unlist(append(list(...), param)))

#-------match params
if (length (param) > 0) {
	pnames <- union(names(api[[resource]][[request]]$parameters$required),
					names(api[[resource]][[request]]$parameters$options))
	x <- pnames [pmatch(names(param), pnames, dup=TRUE)]
	if (any (is.na (x))) stop("parameter(s) invalid or unidentified: ", paste(names(param) [is.na (x)], collapse=" "))
	names(param) <- x
	cat("matched: ", paste(names(param),"=",param,sep="",collapse="/"), "\n")
}

#-------match values for controlled-vocabulary params

#-------give names (of required parameters) to unnamed parameters

required <- names(param) %in% names(api[[resource]][[request]]$parameters$required)
optional <- names(param) %in% names(api[[resource]][[request]]$parameters$options)
# cat("required:      ", names(param)[required],"\n")
# cat("options:       ", names(param)[optional],"\n")

#-------check required parameters present
check.required <- names(api[[resource]][[request]]$parameters$required) %in% names(param)
if(!all(check.required)) 
	warning("required parameter(s) missing: ", 
			names(api[[resource]][[request]]$parameters$required)[!check.required])

#-------add prefixes (mgp, mgm, mgl, mgs) to IDs
if (cases under which we expect an ID) scrubIDs(, resource)


#-------break up IDs of length > 1

required.str <- paste (param[required], sep="/")
optional.str <- paste(names(param)[optional], param[optional], sep="=", collapse="&")
# cat("required string:  ", required.str, "\n")
# cat("optional string:  ", optional.str,"\n")

path <- paste(server, resource, sep="/")
if (length(request) > 0)
#-------omit request names that the API (inconsistently) does not want: query, instance
#-------omit "info" where it (inconsistently) fails to work: 
#-------  'download','library','metagenome','profile','project','sample','status'
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
api <- .MGRAST$api()
server <- .MGRAST$server()

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
