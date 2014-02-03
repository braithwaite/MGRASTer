####################################################################################
### tests of the local API representation
###
### annotation compute download inbox library m5nr matrix metadata metagenome 
### profile project sample validation status
####################################################################################


####################################################################################
### resources
####################################################################################

api <- .session$api()
resources <- paste(names(api), collapse=",")
print(resources, quote=F)

####################################################################################
### resource-requests
####################################################################################

for(n in names(api)) {
	s <- paste(n, ":   ", paste(names(api[[n]]), collapse=","), sep = "")
	print(s, quote=F)
}

####################################################################################
### required & optional parameters
####################################################################################

x <- sapply(names(api), function(x) { 
		sapply(names(api[[x]]), function(y) 
			list(required =
					names(api[[x]][[y]]$parameters$required),
				options =
				  	names(api[[x]][[y]]$parameters$options)),
			simplify=F)  }, 
		simplify=F)
x <- unlist(x, rec=F)
t <- sapply(sapply(x, `[[`, "required"),paste,collapse=",")
required <- cbind(names(t),unname(t))
t <- sapply(sapply(x, `[[`, "options"),paste,collapse=",")
options <- cbind(names(t),unname(t))

print(required, quote=F)
print(options, quote=F)


####################################################################################
### examples
####################################################################################

x <- sapply(names(api), function(x)
		sapply(names(api[[x]]), function(y) api[[x]][[y]]$example[1], simplify=F),
		simplify=F)
x <- unlist(x)

print(unname(x), quote=F)