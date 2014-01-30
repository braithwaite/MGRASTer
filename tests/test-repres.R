
# check the local API representation in a general way:
names(api)
names(api$matrix)
names(api$matrix$organism)
names(api$matrix$organism$method)
api$matrix$organism$method
names(api$matrix$organism$parameters)
names(api$matrix$organism$parameters$options)
names(api$matrix$organism$attributes)
names(api$status)
names(api$status$instance)
names(api$status$instance$attributes)
api$status$instance$attributes$data

# and again:
sapply(api, names)
sapply(api$annotation, names)
sapply(api$annotation$info, names)
sapply(api$annotation$info$parameters, names)
sapply(api$status, names)
sapply(api$status$instance, names)
sapply(api$status$instance$parameters, names)

# check all resource-requests pairs:
for(n in names(api)) cat(n, " ", names(api[[n]]), "\n")

# for each resource-request: check required & optional parameters
x <- sapply(names(api), function(x) { 
		sapply(names(api[[x]]), function(y) 
			c(required =
					paste(names(api[[x]][[y]]$parameters$required),collapse=" "),
				options=
				  	paste(names(api[[x]][[y]]$parameters$options),collapse=" ")),
			simplify=F)  }, 
		simplify=F)
x <- unlist(x, rec=F)
sapply(x, `[`, "required")
print(unname(sapply(x, `[`, "options")))
