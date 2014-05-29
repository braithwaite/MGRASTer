
##############################################################################
### for the loading scheme, also see data/session.R
##############################################################################

APIfilename <- "API.Rda"

.onLoad <- function (libname, pkgname) { }

.onAttach <- function (libname, pkgname) { 
	assign("myname", unname(pkgname), .MGRAST)
	assign("server", "http://api.metagenomics.anl.gov", .MGRAST)
	load(file.path(find.package(pkgname), "extdata", APIfilename), .MGRAST)
	packageStartupMessage(pkgname, " (", packageVersion(pkgname), " build xxxxxxx)")
	}


##############################################################################
### from URL make arg list ll suitable for:  do.call(call.MGRAST, ll)
##############################################################################

parse.MGRAST <- function (call.url) {
	xx <- regmatches(call.url,
		regexec("(http://)([.[:alnum:]]+)([/[:alnum:]+]*)(\\?{1})([[:print:]]*)",
			call.url)) [[1]]

	yy <- strsplit(xx[4], "/")[[1]][-1]

#-------but omit request names that the API (inconsistently) does not want in the path
#-------and omit "info" where it (inconsistently) fails to work: 
	if (yy[1] %in% c("download","library","metagenome","project","sample"))
	
	if (resource %in% c('download','library','mnnnetagenome','profile','project','sample','status') &&
		request == "info")
			path <- paste(path, request, sep="/")

	optional <- matrix(strsplit(xx[6], "[&=]")[[1]], nrow=2)
	zz <- optional[2,]
	names(zz) <- optional[1,]

	as.list(append(yy,zz))
	}


##############################################################################
### an all-purpose interface for the MG-RAST API
##############################################################################

call.MGRAST <- function (
	resource, 								# what resource
	request,								# what request
	..., 									# parameters
	args = NULL, 		 					# parameters in a list (instead or additionally)
	file=NULL,								# output to file
	parse=is.null(file), 					# parse JSON?
	verify=parse,  									# check retrieved object?
	bugs=c("ignore","warn","stop","ask","report"),	# report bugs?
	debug=FALSE,
	timeout=300,						# timeout for download
	issue=TRUE							# issue the call?
	) {									# save to file

# ...beginning of an error-handling concept
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

	dmess <- if (debug) message else function(...) { }
	api <- get("API", .MGRAST)

#-------match resource and request
	resource <- pmatch(resource, names(api))
	request <- pmatch(request, names(api[[resource]]))
	dmess(resource, "/", request)

#-------identify required and optional parameters
	required <- names(api [[c(resource, request, "parameters", "required")]])
	optional <- names(api [[c(resource, request, "parameters", "options")]])
	dmess("required:  ", collapse(required), "\noptional:  ", collapse(optional))


#-------this needs fixing
	cv <- sapply(optional,
		function(xx) api [[c(resource, request, "parameters", "options", xx)]])
#	if (.api[[res]][[req]]$parameters$options[[opt]][1] == "cv")
#		sapply (.api[[res]][[req]]$parameters$options[[opt]][[2]], `[`, 1),
# ...

#-------combine "..." and "args"; convert numbers (e.g. IDs) to strings
#-------"args" remains "list" to accommodate vectors of IDs
	args <- append(list(...), args)
	if (length (args) > 0) {
		args <- sapply (args, as.character)

#-------use names of required parameters for unnamed parameters (typically "id" or "text")
		if (is.null(names(args)))
			is.na(names(args)) <- TRUE
		j <- is.na(names(args)) | (names(args) == "")
		if (any (j)) 
			names(args)[j] <- required

#-------now in the list all parameters have a (possibly abbreviated) name; match them
		targets <- union(required, optional)
		x <- targets [pmatch(names(args), targets, duplicates.ok=TRUE)]
		if (any(is.na(x)))
			warning("no match or not unique for parameter(s): ", paste(names(args)[is.na(x)], collapse=" "))
		names(args) <- x

#-------handle args(s) named "id" specially, by breaking apart vectors and add prefixes
		if("id" %in% names(args)) {
			ids <- args [names(args) == "id"]
			prefix <- switch (resource,
				annotation=,compute=,download=,matrix=,metagenome="metagenome",
				library=,project=,sample=resource,default=NULL)
			if (!is.null (prefix)) {
				ids <- as.vector (sapply(ids, scrubIDs, prefix))
				is.na(names(ids)) <- TRUE
				names(ids)[] <- "id"
				args <- append (args [names(args) != "id"], ids)
			}
		}

		dmess(paste(names(args), "=", args, collapse=" ",sep=""))

#-------check required parameters present
		required.index <- names(args) %in% required
		optional.index <- names(args) %in% optional
		check.required <- required %in% names(args)
		if(!all(check.required)) 
			warning("required parameter(s) missing: ", 
					required [!check.required])

#-------match parameter values to controlled vocabularies
		for (j in 1:length(args))
			if (optional.index[j]) {
				vocab <- cv [[names(args)[j]]]
				if (length (vocab) > 0) {
					x <- vocab [pmatch(args[[j]], vocab)]
					if (is.na (x))
						warning("no match in controlled vocabulary: ", args[[j]])
					args[[j]] <- x
				}
			}

		required.str <- paste(args[required.index], sep="/")
		optional.str <- paste(names(args)[optional.index], args[optional.index], sep="=", collapse="&")
	}
	else {
		required.str <- character(0)
		optional.str <- character(0)
	}

#-------construct the complete URL string
	path <- paste(get("server", .MGRAST), resource, sep="/")
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
			stop("a file name should be specified for this request")

	dmess("URL: ", callurl)

	timeout.old <- getOption("timeout")
	options(timeout=timeout)
	if(!is.null(file)) {
		download.file(call.url, file, quiet=!debug)
		if (parse || verify)
			message("saved resource to file and ignored parse= or verify=TRUE")
		options(timeout=timeout.old)
		return(file)
	}
	x <- readLines(call.url, warn=debug)
	options(timeout=timeout.old)

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
		diff <- setdiff (names (x),  api [[c(resource, request, "attributes")]])
		if(length(diff) > 0)
			warning("resource is missing component(s): ", paste(diff, collapse=" "))
	}

	if (debug) px
	else invisible(px)
	}


##############################################################################
### utilities
##############################################################################

#-------return the installed copy of the API
loadAPI <- function (filename = 
				file.path(find.package(get("myname", .MGRAST)), "extdata", APIfilename)) {
	load(filename)
	if("API" %in% ls()) return(invisible(API))
	else stop("object \"API\" not found in ", filename)
	}

#-------retrieve the API and sanitize known inconsistencies
rebuildAPI <- function (filename = APIfilename) {
	require (RJSONIO)
	if(length(filename))
		message("rebuilding object \"API\" in ", filename, " in ", getwd())

	server.path <- get("server", .MGRAST)
	resource.page <- fromJSON(readLines(server.path, warn=FALSE), asText=TRUE, simplify=TRUE)
	resources <- unname(sapply(resource.page$resources, `[`, "name"))
	resources <- c(resources, "status")
	API <- list()
	for (rr in resources) {
		rr.url <- paste(server.path, rr, sep="/")
		request.page <- fromJSON(readLines(rr.url, warn=FALSE), asText=TRUE, simplify=TRUE)
		API[[rr]] <- request.page$requests
		names(API[[rr]]) <- sapply(API[[res]], `[[`, "name")
	}

#-------FIX ERROR: required "id" parameter undocumented in compute/alphadiversity
#-------(no, not exactly; one is GET, one is POST, so it's not a mistake)
	API$compute$alphadiversity$parameters$required <- API$annotation$sequence$parameters$required
#-------FIX ERROR: "m5nr" resource contains duplicates of requests
	API$m5nr <- API$m5nr [1:10]

	if(length(filename)) {
		save (API, file=filename)
		message("Done.  Move to ", file.path(get("myname",.MGRAST), "inst", "extdata"))
		filename
		}
	else invisible(API)
	}

#-------chrome-plated ID scrubbing
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

#-------tell whether a resource-request pair is a file-based resource
is.filebased <- function(resource, request) {
	(resource == "annotation" && request %in% c("sequence","similarity")) ||
	(resource == "download" && request == "instance")
	}

#-------fancy extraction from a list
extract.at <- function(x, name, at) {
	}

#-------just concatenate all elements, separated by spaces
collapse <- function (x) {
	paste(x, collapse=" ", sep="")
	}




##  sketches for validation
##
## 	check.content <- names(api[[resource]][[request]]$attributes) %in% names(object)
## 	missing.content <- names(api[[resource]][[request]]$attributes) [!check.content]
## 	
## 	# REFINE WITH A RECURSIVE CHECK FOR FIELDS WITHIN FIELDS
## 	if (all (check.content)) return(TRUE)
## 	
## 	# TWEAK: do not throw an error for known and/or approved exceptions
## 	subtype <- paste(resource, request, "/")
## 	if (switch(subtype, 
## 		"matrix/function" = identical(missing.content,"generated_by"),
## 		"matrix/organism" = identical(missing.content,"date"),
## 		default=FALSE)) return(TRUE)
## 	
## 	if (!quiet) warning("API resource of type ", subtype, " missing content: ", missing.content)
## 	return(FALSE)



# ...defunkt

api <- function (renew = FALSE) {
	built <- (renew || is.null (.api))
	if (renew) .api <<- .build.API.tree ()
	else if (is.null (.api)) .api <<- .read.API.tree ()
	if (built) {
		.resources <<- names(api())
		.requests <<- sapply(.resources, function (res) names(.api[[res]]))
		.examples <<- 
			sapply(.resources, function (res) 
				sapply (.requests[[res]], function (req) .api[[res]][[req]]$example[1], 
					simplify=F),
				simplify=F)
		.examples <<- lapply (.examples, function (x) { x$info <- NULL ; x } )
		.required <<- sapply(.resources, 
			function (res) 
				sapply (.requests[[res]],
					function (req) names(.api[[res]][[req]]$parameters$required), 
					simplify=F),
			simplify=F)
		.options <<- sapply(.resources, 
			function (res) 
				sapply (.requests[[res]],
					function (req) names(.api[[res]][[req]]$parameters$options),
					simplify=F),
			simplify=F)
#-------note: we only look for cvs among options, not required parameters
		.cv <<- sapply(.resources, 
			function (res) 
				sapply (.requests[[res]],
					function (req) 
						sapply (names (.api[[res]][[req]]$parameters$options),
							function (opt) 
								if (.api[[res]][[req]]$parameters$options[[opt]][1] == "cv")
									sapply (.api[[res]][[req]]$parameters$options[[opt]][[2]], `[`, 1),
							simplify=F),
					simplify=F),
			simplify=F)
		.attributes <<- sapply(.resources, 
			function (res) 
				sapply (.requests[[res]],
					function (req) 
						names (.api[[res]][[req]]$attributes),
					simplify=F),
			simplify=F)
	}
	.api
	}
