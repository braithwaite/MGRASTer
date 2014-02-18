library(MGRASTer)

####################################################################################
### test integrity of the local API representation
####################################################################################

.MGRAST$server()
str(.MGRAST$api())
.MGRAST$resources()
.MGRAST$requests()
.MGRAST$examples()
str(.MGRAST$required())
str(.MGRAST$options())
str(.MGRAST$cv())
str(.MGRAST$attributes())
