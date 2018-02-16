###############################################################################
#
#  Goals of this package:
#
#  (1) make the MGRAST API accessible within R
#  (2) do it with respect to R paradigms
#  (3) hide API inconsistencies from the user wherever possible
#
#  This package carries its single "data" object, namely the MGRAST API doc
#  tree, in a slightly odd way.  The doc tree is a list ("API") saved as .rda, 
#  not in /data but in /extdata.  In a formal sense, the data of the package
#  is an environment (.MGRAST) created by a source code data file in /data.
#  The doc tree is stored into that environment upon package loading.
#
#  The purpose of this rigamarole is to allow dynamic update, in any installation
#  of MGRASTer, of the API tree.  It cannot be updated permanently without an
#  update to the package installation, but it can be updated in each session.
#  This is an important ability for our working environment:  time-sensitive
#  API patches often need to be accessible immediately.
# 
#  API versioning may become a more complex issue.  For now it is simple:
#		"API.version" is hardcoded "1" in .onAttach()
#		call.MGRAST() appends it to any call;
#		parse.MGRAST() removes any version number from API URLs;
#		DESCRIPTION states this package calls API version 1.
#
###############################################################################


#------------------------------------------------------------------------------
#  set or query auth key in the package environment.
#------------------------------------------------------------------------------

auth.MGRAST <- function (key, file) {
	.MGRAST <- .MGRAST								# only for clean CRAN check
	if (! missing (file)) {
		key <- readLines (file, n=1, warn=FALSE)
	} else if (missing (key))
		key <- readLines (n=1)
	assign ('key', key, .MGRAST)
	key
	}

#------------------------------------------------------------------------------
#  little utility to inspect the API documentation tree.
#------------------------------------------------------------------------------

doc.MGRAST <- function (depth = 1, head = NULL, stratum = NULL, ...) {
	.MGRAST <- .MGRAST								# only for clean CRAN check
	api <- get ("API", .MGRAST)
	if (!is.null (head))
		api <- api [[head, exact=FALSE]]
	if (!is.null (stratum))
		repeat {
			ii <- grepl (stratum, names (api), fixed = TRUE)
			if (any (ii)) {
				api <- api [ii]	
				break
				}
			api <- unlist (api, recursive = FALSE, use.names = TRUE)
			if (is.character (api))
				stop (gettext ('stratum not found'))
			}
	str (api, max.level=depth, ...)
	}

#------------------------------------------------------------------------------
#  utility to return the installed (or other local) copy of the API tree.
#  simply returns it.
#  does not reload into .MGRAST
#------------------------------------------------------------------------------

load.MGRAST <- function (file = API.filepath()) {
	.MGRAST <- .MGRAST								# only for clean CRAN check
	if (!is.null (file)) {
		load (file)
		return (get ("API", inherits=FALSE))
	} else get ("API", .MGRAST)
	}

#------------------------------------------------------------------------------
#  utility to retrieve the current live API tree from home base.
#  also, sanitize known inconsistencies.
#  returns the object invisibly when ff=NULL.
#  otherwise saves it to the named file.
#  does not reload into .MGRAST
#------------------------------------------------------------------------------

build.MGRAST <- function (file = API.filename) {
	.MGRAST <- .MGRAST								# only for clean CRAN check

#------------------------------------------------------------------------------
#  make list of resources.
#  add undocumented resource "status"
#------------------------------------------------------------------------------
	server.path <- get ("server", .MGRAST)
	resource.page <- RJSONIO::fromJSON(
		readLines(server.path, warn=FALSE), asText=TRUE, simplify=TRUE)
	resources <- unname (sapply (resource.page$resources, `[`, "name"))
	resources <- c (resources, "status")

#------------------------------------------------------------------------------
#  download info page for each into the list
#------------------------------------------------------------------------------
	API <- list()
	for (rr in resources) {
		rr.url <- paste (server.path, rr, sep="/")
		request.page <- RJSONIO::fromJSON(
			readLines (rr.url, warn = FALSE), asText = TRUE, simplify = TRUE)
		API [[rr]] <- request.page$requests
		names (API [[rr]]) <- sapply (API[[rr]], `[[`, "name")
	}

#------------------------------------------------------------------------------
#  remove "POST" resources since we don't handle them.
#  also some of them are identically named with GET resources, which is crazy.
#------------------------------------------------------------------------------
	API <- mapply (`[`, API, lapply (API, function (xx) sapply (xx, `[[`, "method") == "GET"))

	if (length (file)) {
		save (API, file=file)
		message (gettextf ("saved \'API\' to \'%s\' in %s"), file, getwd())
		message (gettextf ("for package build, move to %s", 
			file.path (this.package(), "inst", "extdata")))
		file
		}
	else invisible(API)
	}

#------------------------------------------------------------------------------
#  utility to convert a given valid API url into list "li" such that:
#      do.call (call.MGRAST, li)
#  will fetch the intended resource.
#  this is just a question of tokenizing and tending to a few API inconsistencies.
#
#  regexp is matching:
#    (req) literal "http://" or "https://"
#    (req) server name as "anything up to /"
#    (opt) version number as "/ plus digits, decimal points"
# 1: (opt) path ("required") arguments as "/ plus anything up to ?"
#    (opt) literal "?"
# 2: (opt) optional arguments, as all remaining characters
#
#  index [-c(1,2)] drops unneeded parts of the match
#------------------------------------------------------------------------------

parse.MGRAST <- function (url) {
	.MGRAST <- .MGRAST								# only for clean CRAN check
	api <- get ("API", .MGRAST)
	xx <- regmatches (url,
		regexec("https?://[^/]+(/[[:digit:]\\.]+)?(/[^\\?]*)?\\??(.*)", url)) [[1]] [-c(1,2)]

#------------------------------------------------------------------------------
#  determine optional arguments
#  possibly this comes out length 0
#------------------------------------------------------------------------------
	yy <- strsplit (xx[2], "&") [[1]]
	zz <- strsplit (yy, "=")
	optional <- sapply (zz, `[`, 2)
	names(optional) <- sapply (zz, `[`, 1)

#------------------------------------------------------------------------------
#  determine resource
#  this is the consistent part of API syntax
#  index [-1] drops the empty split component before the first "/"
#------------------------------------------------------------------------------
	path <- strsplit (xx[1], "/") [[1]] [-1]
	resource <- path[1]

#------------------------------------------------------------------------------
#  determine the request ...
#  use the name of a request, if one is given;
#  or else, consider only requests for which:
#    all provided optional arguments are valid, and
#    argument(s) are both required and provided in the path, or the reverse
#  and store any provided path (required) arguments, along the way
#------------------------------------------------------------------------------
	couldbe <- rep (FALSE, length (api [[resource]]))
	names (couldbe) <- names (api [[resource]])
	if (path [2] %in% names (api [[resource]])) {
		couldbe [path [2]] <- TRUE
		required <- path [-c(1,2)]
	} else {
		for (request in names (couldbe))
			couldbe [request] <-
				!length (setdiff (
						names (optional),
						names (api [[ c(resource,request) ]] $ parameters $ options))) &&
				(length (path) > 1) ==
				as.logical (length (api [[ c(resource,request) ]] $ parameters $ required))
		required <- path [-1]
		}
#------------------------------------------------------------------------------
#  if that didn't make a unique determination,
#  the next line produces an error or arbitrary choice.
#  in other words:  we give up on it.
#------------------------------------------------------------------------------
	request <- names (couldbe) [which (couldbe) [1]]

#------------------------------------------------------------------------------
#  if there are required arguments, make an effort to give them names
#------------------------------------------------------------------------------
	if (length (required))
		names(required) <- names (api [[ c(resource,request) ]] $ parameters $ required)

	c (list(resource=resource, request=request), as.list (required), as.list (optional))
	}


###############################################################################
###############################################################################
##
##  general-purpose not-user-unfriendly interface to the MG-RAST API
##
###############################################################################
###############################################################################

call.MGRAST <- function (
	resource, 								# what resource
	request,								# what request
	..., 									# arguments
	args=NULL, 		 						# arguments in a list (instead or additionally)
	file=NULL,								# output to file
	timeout=300,							# timeout for download
	parse=is.null(file), 					# parse JSON?
	verify=parse,  							# check received resource?
	quiet=TRUE,								# less diagnostics?
	issue=TRUE,								# issue the call?
        destfile=NULL
	) {

	.MGRAST <- .MGRAST								# only for clean CRAN check
	checkpoint <- if (!quiet) message else function(...) { }
	api.root <- get("API", .MGRAST)

#------------------------------------------------------------------------------
#  match resource and request
#------------------------------------------------------------------------------
	resource <- match.arg (resource, names (api.root))
	request <- match.arg (request, names (api.root [[resource]]))
	checkpoint ('resource: ', resource, '\nrequest: ', request)

#------------------------------------------------------------------------------
#  warn if file name should be provided but is not
#------------------------------------------------------------------------------
	if (is.null (destfile) &&
		((resource == "annotation" && request %in% c("sequence", "similarity")) ||
		(resource == "download" && request == "instance")))
		stop (gettext ("resource requires \'destfile\'"))

#------------------------------------------------------------------------------
#  identify required and optional arguments
#------------------------------------------------------------------------------
	api <- api.root [[ c(resource, request) ]] $ parameters
	required <- names (api$required)
	optional <- names (api$options)
	checkpoint(
		"required arguments: ", collapse(required), 
		"\noptional arguments: ", collapse(optional))

#------------------------------------------------------------------------------
#  make table of relevant controlled vocabularies
#------------------------------------------------------------------------------
	type <- sapply (api$options, `[[`, 1)
	cv2 <- lapply (api$options [type == "cv"], `[[`, 2)
	cv <- lapply (cv2, sapply, `[`, 1)

#------------------------------------------------------------------------------
#  combine "..." and "args"; convert all arguments to character
#  convert unmeaningfull args (such as NULL, NA_character_, integer(0), ...) to character(0)
#  ... so we don't get an awkward character representation.
#  "args" remains "list" to accommodate vectors of IDs
#  (maybe there are some other good reasons too)
#------------------------------------------------------------------------------
	args <- append (list(...), args)
	args <- lapply (args, function (x) {
		if (is.null (x) || !length (x)) {
			""
		} else {
			x [is.na (x)] <- ""
			as.character (x)
			} 
		} )
	if (length (args)) {

#####------------------------------------------------------------------------------
#####  use names of required arguments for unnamed arguments (typically "id" or "text")
#####  after this, all arguments will have a (possibly abbreviated) name
#####------------------------------------------------------------------------------
#####
#####		if (is.null (names (args)))
#####			is.na (names (args)) <- TRUE
#####		j <- is.na (names (args)) | (names (args) == "")
#####		if (any (j)) 
#####			names (args)[j] <- required

#------------------------------------------------------------------------------
#  note:  instead of the above, we simply require all arguments to be named
#
#  then, match them
#------------------------------------------------------------------------------
		if(!length(names(args)) || any(!nzchar(names(args)))) 
			stop (gettext ("all arguments require names"))
		all.arg.names <- union(required, optional)
		x <- all.arg.names [pmatch(names(args), all.arg.names, duplicates.ok=TRUE)]
		if (any(is.na(x)))
			stop (gettext (
				"no match or not unique for argument(s): "), 
				collapse (names(args) [is.na (x)], x ))
		names(args) <- x

#------------------------------------------------------------------------------
#  handle args(s) named "id" specially, by breaking apart vectors and adding prefixes
#------------------------------------------------------------------------------
		ii <- (names(args) == "id")
		if(any (ii)) {
			prefix <- switch (
				resource,
				annotation=, compute=, download=, matrix=, metagenome="metagenome",
				library=, project=, sample=resource,
				default=NULL)
			if (!is.null (prefix)) {
				flat <- unlist (sapply(args [ii], scrubIDs, prefix))
				names(flat) <- rep ("id", length.out=length(flat))
				args <- append (as.list(flat), args [!ii])
				}
			}

#------------------------------------------------------------------------------
#  check required arguments present
#------------------------------------------------------------------------------
		required.index <- names(args) %in% required
		optional.index <- names(args) %in% optional
		check.required <- required %in% names(args)
		if (!all (check.required)) 
			warning (gettext ("required argument(s) missing: "),
				collapse (required [!check.required]))

#------------------------------------------------------------------------------
#  where a controlled vocabulary exists, apply partial matching
#  note: match.arg() signals error upon no match
#------------------------------------------------------------------------------

		args <- mapply (
			function (nn, aa, cv)
				if (nn %in% names (cv)) {
					match.arg (aa, cv [[nn]])
				} else aa,
			names(args), args, MoreArgs=list (cv))

		checkpoint(
			"scrubbed arguments:\n", 
			paste(names(args), "=", args, collapse="\n"))

		required.str <- paste(args[required.index], sep="/")
		optional.str <- paste(names(args)[optional.index], args[optional.index], sep="=", collapse="&")
	} else {
		required.str <- ""
		optional.str <- ""
		}

	key <- get ('key', .MGRAST, inherits=FALSE)
	if (! is.null (key)) {
		if (length (optional.str) && nchar (optional.str))
			optional.str <- paste0 (optional.str, "&", "auth=", key)
		else
			optional.str <- paste0 ("auth=", key)
		}

#------------------------------------------------------------------------------
#  construct the complete URL string.  first, the "path" (i.e., before "?").  but:
#  omit request names that the API (inconsistently) does not want there;
#  omit "info" where it, for some reason, fails to work; and
#  only add required arguments if there are any.
#------------------------------------------------------------------------------
	call.url <- paste (c (
		get("server", .MGRAST), 
		get("API.version", .MGRAST), 
		resource,
		if (length (request) && resource %in% c('annotation','compute','inbox','m5nr','matrix','metadata','validation'))
			request,
		if (length (required.str) && nchar (required.str))
			required.str),
		collapse = '/')

#------------------------------------------------------------------------------
#  attach any optional arguments after "?"
#------------------------------------------------------------------------------
	if (length (optional.str) && nchar (optional.str))
		call.url <- paste (call.url, optional.str, sep="?")

#------------------------------------------------------------------------------
#  now we are done, if the URL only is wanted
#------------------------------------------------------------------------------
	if (!issue) return (call.url)

#------------------------------------------------------------------------------
#  ... add RCurl code for resources requiring a header
#------------------------------------------------------------------------------
#	require (RCurl)

#------------------------------------------------------------------------------
#  ... add curl dependency for https support
#------------------------------------------------------------------------------
        library(curl)

	checkpoint ("requesting URL: ", call.url)

#
#  use "is.filebased()"
#
	timeout.old <- getOption ("timeout")
	options (timeout = timeout)
	showURIoutput <- function (cond) { # error handler for somethings wrong with download.file, show HTTP response + errors
		cat( "Error getting URL:\n" )
		cat (paste( gettext(cond)))
		cat("HTTP Response: ")
		cat ( system(  paste("curl -s ", gettext(call.url), " && echo" ), intern=TRUE ) )
		stop("\n")
              
		}
	if (!is.null (destfile)) {
		tryCatch(
		{download.file (call.url, destfile=destfile, quiet=FALSE)} ,
		error=showURIoutput,  warning = showURIoutput )
		if (parse || verify)
			warning (gettext (
				"saving to file unimplemented with \'parse\' and \'verify\'; ignoring these"))
		options (timeout = timeout.old)
		return (destfile)
		}
	tryCatch(
		{x <- readLines (curl(call.url), warn = !quiet)},
		error=showURIoutput,  warning = showURIoutput )
	options (timeout = timeout.old)

#------------------------------------------------------------------------------
#  now we are done, if JSON parsing is not wanted.  otherwise, parse it.
#------------------------------------------------------------------------------
	if (!parse) return (invisible (x))

	if (!RJSONIO::isValidJSON (x, asText=TRUE))
		warning (gettext ("resource failed JSON validation"))
	px <- RJSONIO::fromJSON (x, asText=TRUE, simplify=TRUE)

#------------------------------------------------------------------------------
#  now we are done, if no verification of content is wanted
#------------------------------------------------------------------------------
	if (!verify) return (invisible (px))

#------------------------------------------------------------------------------
#  for certain resources, we cannot verify content
#------------------------------------------------------------------------------
	if (resource %in% c ("annotation", "inbox", "validation") ||
		(resource == "download" && request == "instance") || 
		request == "info") {
		warning (gettext ("\'verify\' inapplicable; ignoring"))
	} else {
		diff <- setdiff (names (api.root [[c(resource, request, "attributes")]]), names (px))
		if (length (diff))
			warning (gettext ("resource missing component(s): "), collapse (diff))
		}

	invisible(px)
	}


###############################################################################
###############################################################################
##
##  internal utilities, little things, loading details
##
###############################################################################
###############################################################################

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

requiresFile <- function (resource, request)
	(resource == "annotation" && request %in% c("sequence","similarity")) ||
	(resource == "download" && request == "instance")

collapse		<- function (x) paste (x, collapse=" ", sep="")

warning			<- function (...) base::warning (this.package(), ": ", ..., call.=FALSE)

this.package	<- function () {
	.MGRAST <- .MGRAST								# only for clean CRAN check
	get ("this.package", .MGRAST)
	}

API.filename	<- "API.rda"

API.filepath	<- function () file.path (find.package (this.package ()), "extdata", API.filename)

.onAttach <- function (libname, pkgname) { 
	.MGRAST <- .MGRAST								# only for clean CRAN check
	..githash.. <- ""								# this line gets replaced
	packageStartupMessage (pkgname, " (", packageVersion (pkgname), ..githash.., ")")
	assign ("this.package", pkgname, .MGRAST)
	load (API.filepath(), .MGRAST)
	assign ("API.version", "1", .MGRAST)
	assign ("server", "https://api.mg-rast.org", .MGRAST)
	assign ('key', NULL, .MGRAST)
	}
