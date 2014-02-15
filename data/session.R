.my.name <- "MGRASTer"

### reads saved copy of API tree, upon package loading

.read.API.tree <- function (fname = "API.Rda") {
	fname <- file.path (path.package(.my.name), "extdata", fname)
	load(fname)
	api 	
}

### retrieves and saves the API tree (for development purposes),
### or returns it (for dynamic update)

### NOTE - WITH RENEW=TRUE SHOULD WRITE TO INSTALLATION?

.build.API.tree <- function (save = FALSE, fname = "API.Rda") {
	message(.my.name, ": rebuilding API resource tree")
	require (RJSONIO)

	server.path <- .MGRAST$server()
	resource.page <- fromJSON(readLines(server.path, warn=FALSE), asText=TRUE, simplify=TRUE)
	resources <- unname(sapply(resource.page$resources, `[`, "name"))
	resources <- c(resources, "status")
	api <- list()
	for (res in resources) {
		res.url <- paste(server.path, res, sep="/")
		request.page <- fromJSON(readLines(res.url, warn=FALSE), asText=TRUE, simplify=TRUE)
		api[[res]] <- request.page$requests
		names(api[[res]]) <- sapply(api[[res]], `[[`, "name")
	}

#-------FIX ERROR IN API DOCUMENTATION
#-------required "id" parameter undocumented in compute/alphadiversity
	api$compute$alphadiversity$parameters$required <- api$annotation$sequence$parameters$required

#-------FIX ERROR IN API DOCUMENTATION
#-------"m5nr" resource contains duplicates of requests
	api$m5nr <- api$m5nr [1:10]

	if (save) {
		save (api, file=fname)
		fname
	}
	else api
}


.MGRAST <- (function () {
	.server <- "http://api.metagenomics.anl.gov"
	.api <- NULL
	.resources <- NULL
	.requests <- NULL
	.examples <- NULL
	.required <- NULL
	.options <- NULL
	.cv <- NULL
	.attributes <- NULL

	server <- function (s = NULL)
		if (is.null(s)) .server
		else .server <<- s
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
	resources <- function () {
		if (is.null (.resources)) api()
		.resources
		}
	requests <- function () {
		if (is.null (.requests)) api()
		.requests
		}
	examples <- function () {
		if (is.null (.examples)) api()
		.examples
		}
	required <- function () {
		if (is.null (.required)) api()
		.required
		}
	options <- function () {
		if (is.null (.options)) api()
		.options
		}
	cv <- function () {
		if (is.null (.cv)) api()
		.cv
		}
	attributes <- function () {
		if (is.null (.attributes)) api()
		.attributes
		}

# try without names
	list(server=server, 
		api=api, 
		resources=resources, 
		requests=requests,
		examples=examples,
		required=required,
		options=options,
		cv=cv,
		attributes=attributes)
}) ()

