.my.name <- "MGRASTer"

### reads saved copy of API tree, upon package loading

.read.API.tree <- function (fname = "API.Rda") {
	fname <- file.path (path.package(.my.name), "extdata", fname)
	load(fname)
	api 	
}

### retrieves and saves the API tree (for development purposes),
### or returns it (for dynamic update)

.build.API.tree <- function (save = FALSE, fname = "API.Rda") {
	message(.my.name, ": rebuilding API resource tree")
	require (RJSONIO)

	server.path <- .session$server()
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
	if (save) {
		save (api, file=fname)
		fname
	}
	else api
}

.session <- (function () {
	.server <- "http://api.metagenomics.anl.gov"
	.api <- NULL

	server <- function (s = NULL)
		if (is.null(s)) .server
		else .server <<- s
	api <- function (renew = FALSE)
		if (renew) .api <<- .build.API.tree ()
		else if (is.null (.api)) .api <<- .read.API.tree ()
		else .api

	list (server = server, api = api)
}) ()

