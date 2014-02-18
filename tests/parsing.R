library(MGRASTer)

####################################################################################
### test parameter parsing; we don't issue calls here
###
### --------> this could use some expansion, for thoroughness
####################################################################################

call.MGRAST("ann",issue=F)
call.MGRAST("ann","seque",issue=F)

call.MGRAST("compu","alpha",issue=F)

call.MGRAST("downloa","setl",issue=F)
call.MGRAST("downloa","setl",id=5,issue=F)

# error: no such request
# call.MGRAST("downloa","sel",id=5,issue=F)

call.MGRAST("download","setlist",st=5,id=10,i=11,issue=F)		# ERROR: returns length-2 vector!
call.MGRAST("download","setlist",stage=5,id=10,id=11,issue=F)   # ERROR: returns length-2 vector!

call.MGRAST("mat","org",issue=F)

# not ok (ambiguous parameter):
call.MGRAST("mat", "func", fil="a",issue=F)

# ok:
call.MGRAST("mat", "func", filter="a",issue=F)

# not ok (no match in CV):
call.MGRAST("mat", "func", filter="a",filter_="b",issue=F)
call.MGRAST("mat", "func", filter="a",filter_l="b",filter_s="c",issue=F)
