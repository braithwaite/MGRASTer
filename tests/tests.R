library(MGRASTer)

#-----------------------------------------------------------------------------
#  test package environment & local API representation
#  also, function to access documentation
#-----------------------------------------------------------------------------

ls (.MGRAST)
get ("server", envir=.MGRAST)
get ("this.package", envir=.MGRAST)
get ("API", envir=.MGRAST)

API <- get ("API", envir=.MGRAST)
flat.API <- unlist (get ("API", .MGRAST))
ee <- flat.API [grep("example", names(flat.API), fixed=T)]
ee <- unname(ee) [substr(ee, 1, 4) == "http"]

doc.MGRAST()
doc.MGRAST(2)
doc.MGRAST(3)

doc.MGRAST (head="matrix")
doc.MGRAST (2, head="matrix")
doc.MGRAST (3, head="matrix")

doc.MGRAST (stratum="parameters")
doc.MGRAST (2, stratum="parameters")
doc.MGRAST (3, stratum="parameters")

doc.MGRAST (stratum="options")
doc.MGRAST (2, stratum="options")
doc.MGRAST (3, stratum="options")

doc.MGRAST (stratum="required")
doc.MGRAST (2, stratum="required")
doc.MGRAST (3, stratum="required")

doc.MGRAST (stratum="attributes")
doc.MGRAST (2, stratum="attributes")
doc.MGRAST (3, stratum="attributes")

doc.MGRAST (2, head = c('mat','orga','param','opt','group_level'))
doc.MGRAST (2, head = c('mat','func','param','opt','group_level'))

#-----------------------------------------------------------------------------
#  test parsing, partial matching, misc (no calls issued here)
#  "try" indicates calls that intend to produce errors
#  second set are the API doc's test calls, copied and abbreviated by hand
#  see below (end of file) for the original URLs
#-----------------------------------------------------------------------------

try (call.MGRAST ("ann", issue=F))
try (call.MGRAST ("ann", "seque", issue=F))
call.MGRAST ("ann", "seque", file="no.file", issue=F)
call.MGRAST ("compu", "alpha", issue=F)
call.MGRAST ("downloa", "setl", issue=F)
call.MGRAST ("downloa", "setl", id=5, issue=F)
try (call.MGRAST ("downloa", "sel", id=5, issue=F))
call.MGRAST ("download", "setlist", st=5, id=10, i=11, issue=F)
call.MGRAST ("download", "setlist", stage=5, id=10, id=11, issue=F)
call.MGRAST ("mat", "org", issue=F)
try (call.MGRAST ("mat", "func", fil="a", issue=F))
call.MGRAST ("mat", "func", filter="a", issue=F)
try (call.MGRAST ("mat", "func", filter="a", filter_="b", issue=F))
try (call.MGRAST ("mat", "func", filter="a", filter_l="b", filter_s="c", issue=F))

call.MGRAST ('an', 'se', id=4447943.3, ev=10, ty='or', so='Sw', file="no.file", quiet=FALSE, issue=FALSE)
call.MGRAST ('an', 'si', id=4447943.3, iden=80, ty='fu', so='KO', file="no.file", quiet=FALSE, issue=FALSE)
call.MGRAST ('co', 'al', id=4447943.3, le='or', quiet=FALSE, issue=FALSE)
call.MGRAST ('do', 'ins', id=4447943.3, fi=350.1, file="no.file", quiet=FALSE, issue=FALSE)
call.MGRAST ('do', 'se', id=4447943.3, st=650, quiet=FALSE, issue=FALSE)
call.MGRAST ('li', 'qu', li=20, or='na', quiet=FALSE, issue=FALSE)
*** call.MGRAST ('li', 'ins', id=52924, ve='fu', quiet=FALSE, issue=FALSE)
call.MGRAST ('m5', 'on', so='Subsystems', mi='level3', quiet=FALSE, issue=FALSE)
call.MGRAST ('m5', 'ta', filter='Bacteroidetes', filter_l='phylum', mi='genus', quiet=FALSE, issue=FALSE)
call.MGRAST ('m5', 'so', quiet=FALSE, issue=FALSE)
call.MGRAST ('m5', 'ac', id='YP_003268079.1', quiet=FALSE, issue=FALSE)
call.MGRAST ('m5', 'al', text='IPR001478', quiet=FALSE, issue=FALSE)
call.MGRAST ('m5', 'md', id='000821a2e2f63df1a3873e4b280002a8', so='InterPro', quiet=FALSE, issue=FALSE)
*** call.MGRAST ('m5', 'fu', text='sulfatase', so='Gen', quiet=FALSE, issue=FALSE)
call.MGRAST ('m5', 'or', text='akkermansia', so='KEGG', quiet=FALSE, issue=FALSE)
call.MGRAST ('m5', 'se', text='MAGENHQWQGSIL', so='TrEMBL', quiet=FALSE, issue=FALSE)
call.MGRAST ('ma', 'or', id=c(4447943.3, 4447192.3, 4447102.3, 4447103.3), gro='family', so='Ref', resu='ab', ev=15, quiet=FALSE, issue=FALSE)
call.MGRAST ('ma', 'fu', id=c(4447943.3, 4447192.3, 4447102.3, 4447103.3), gro='level3', so='Sub', resu='ab', iden=80, filter_l='phylum', filter='Firmicutes', quiet=FALSE, issue=FALSE)
call.MGRAST ('ma', 'fe', id=c(4447943.3, 4447192.3, 4447102.3, 4447103.3), so='KE', resu='ev', len=25, quiet=FALSE, issue=FALSE)
call.MGRAST ('metadata', 'te', quiet=FALSE, issue=FALSE)
call.MGRAST ('metadata', 'cv', quiet=FALSE, issue=FALSE)
call.MGRAST ('metadata', 'ex', id=128, quiet=FALSE, issue=FALSE)
call.MGRAST ('metageno','qu', li=20, ord='name', quiet=FALSE, issue=FALSE)
call.MGRAST ('metageno','ins', id=4447943.3, ve='me', quiet=FALSE, issue=FALSE)
call.MGRAST ('proj', 'qu', li=20, or='na', quiet=FALSE, issue=FALSE)
*** call.MGRAST ('proj', 'ins', id=128, ve='fu', quiet=FALSE, issue=FALSE)
*** call.MGRAST ('sa', 'qu', li=20, or='na', quiet=FALSE, issue=FALSE)
*** call.MGRAST ('sa', 'ins', id=25823, ve='fu', quiet=FALSE, issue=FALSE)
*** call.MGRAST ('va', 'te', quiet=FALSE, issue=FALSE)
*** call.MGRAST ('va', 'da', quiet=FALSE, issue=FALSE)

#-----------------------------------------------------------------------------
#  test parsing of URLs from API documentation into call arguments
#-----------------------------------------------------------------------------

for (xx in ee) {
	message ("Parsing test URL: ", xx)
	print (try (parse.MGRAST (xx)))
	}

#-----------------------------------------------------------------------------
#  test info pages
#-----------------------------------------------------------------------------

for (xx in names (API)) {
	message ("Making info call for: ", xx)
	res <- try (call.MGRAST (xx, 'info', quiet=FALSE))
	str (res)
	}

#-----------------------------------------------------------------------------
#  test calls from API documentation (with parsing)
#  for simplicity, we send all results to file
#  rather than needing to identify cases where that's required
#-----------------------------------------------------------------------------

ff <- tempfile()
for (xx in ee) {
	message ("Parsing and calling test URL: ", xx)
	li <- parse.MGRAST (xx)
	li [c ('quiet', 'file')] <- list (FALSE, "ff")
	res <- try (do.call (call.MGRAST, li))
	str (res)
	print (summary (res))
	invisible (readLines (n=1))
	}
unlink (ff)

##############################################################################
#
#  test URLs from API documentation (copied by hand)
#
# http://api.metagenomics.anl.gov/annotation/sequence/mgm4447943.3?evalue=10&type=organism&source=SwissProt
# http://api.metagenomics.anl.gov/annotation/similarity/mgm4447943.3?identity=80&type=function&source=KO
# http://api.metagenomics.anl.gov/compute/alphadiversity/mgm4447943.3?level=order
# http://api.metagenomics.anl.gov/download/mgm4447943.3?file=350.1
# http://api.metagenomics.anl.gov/download/mgm4447943.3?stage=650
# http://api.metagenomics.anl.gov/library?limit=20&order=name
# http://api.metagenomics.anl.gov/library/mgl52924?verbosity=full
# http://api.metagenomics.anl.gov/m5nr/ontology?source=Subsystems&min_level=level3
# http://api.metagenomics.anl.gov/m5nr/taxonomy?filter=Bacteroidetes&filter_level=phylum&min_level=genus
# http://api.metagenomics.anl.gov/m5nr/sources
# http://api.metagenomics.anl.gov/m5nr/accession/YP_003268079.1
# http://api.metagenomics.anl.gov/m5nr/alias/IPR001478
# http://api.metagenomics.anl.gov/m5nr/md5/000821a2e2f63df1a3873e4b280002a8?source=InterPro
# http://api.metagenomics.anl.gov/m5nr/function/sulfatase?source=GenBank
# http://api.metagenomics.anl.gov/m5nr/organism/akkermansia?source=KEGG
# http://api.metagenomics.anl.gov/m5nr/sequence/MAGENHQWQGSIL?source=TrEMBL
# http://api.metagenomics.anl.gov/matrix/organism?id=mgm4447943.3&id=mgm4447192.3&id=mgm4447102.3&id=mgm4447103.3&group_level=family&source=RefSeq&result_type=abundance&evalue=15
# http://api.metagenomics.anl.gov/matrix/function?id=mgm4447943.3&id=mgm4447192.3&id=mgm4447102.3&id=mgm4447103.3&group_level=level3&source=Subsystems&result_type=abundance&identity=80&filter_level=phylum&filter=Firmicutes
# http://api.metagenomics.anl.gov/matrix/feature?id=mgm4447943.3&id=mgm4447192.3&id=mgm4447102.3&id=mgm4447103.3&source=KEGG&result_type=evalue&length=25
# http://api.metagenomics.anl.gov/metadata/template
# http://api.metagenomics.anl.gov/metadata/cv
# http://api.metagenomics.anl.gov/metadata/export/mgp128
# http://api.metagenomics.anl.gov/metagenome?limit=20&order=name
# http://api.metagenomics.anl.gov/metagenome/mgm4447943.3?verbosity=metadata
# http://api.metagenomics.anl.gov/project?limit=20&order=name
# http://api.metagenomics.anl.gov/project/mgp128?verbosity=full
# http://api.metagenomics.anl.gov/sample?limit=20&order=name
# http://api.metagenomics.anl.gov/sample/mgs25823?verbosity=full
# http://api.metagenomics.anl.gov/validation/template/
# http://api.metagenomics.anl.gov/validation/data/?template=
#
##############################################################################