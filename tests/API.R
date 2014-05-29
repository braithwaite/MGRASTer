####################################################################################
### test contents of the package environment, including 
### the local API representation.
####################################################################################

library(MGRASTer)
ls(.MGRAST)
str(get("API", envir=.MGRAST))
get("server", envir=.MGRAST)
get("myname", envir=.MGRAST)
