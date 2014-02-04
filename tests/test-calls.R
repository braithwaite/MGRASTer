
####################################################################################
### test actual calls to the API 
####################################################################################

issue <- FALSE

####################################################################################
### test info pages
####################################################################################

for (res in resources) call.MGRAST (res, 'info', issue=issue)

####################################################################################
### test examples provided in API online docs
####################################################################################

### http://api.metagenomics.anl.gov/annotation/sequence/mgm4447943.3?evalue=10&type=organism&source=SwissProt
call.MGRAST('an', 'se', 4447943.3, ev=10, ty='or', so='Sw', issue=issue)
### http://api.metagenomics.anl.gov/annotation/similarity/mgm4447943.3?identity=80&type=function&source=KO
call.MGRAST('an', 'si', 4447943.3, iden=80, ty='fu', so='KO', issue=issue)

### http://api.metagenomics.anl.gov/compute/alphadiversity/mgm4447943.3?level=order
call.MGRAST('co', 'al', id=4447943.3, le='or', issue=issue)

### http://api.metagenomics.anl.gov/download/mgm4447943.3?file=350.1
call.MGRAST('do', 'in', 4447943.3, fi=350.1, issue=issue)
### http://api.metagenomics.anl.gov/download/mgm4447943.3?stage=650
call.MGRAST('do', 'se', 4447943.3, st=650, issue=issue)

### http://api.metagenomics.anl.gov/library?limit=20&order=name
call.MGRAST('li', 'qu', li=20, or='na', issue=issue)
### http://api.metagenomics.anl.gov/library/mgl52924?verbosity=full
call.MGRAST('li', 'ins', 52924, ve='fu', issue=issue)

### http://api.metagenomics.anl.gov/m5nr/ontology?source=Subsystems&min_level=level3
call.MGRAST('m5', 'on', so='Sub', mi='level3', issue=issue)
### http://api.metagenomics.anl.gov/m5nr/taxonomy?filter=Bacteroidetes&filter_level=phylum&min_level=genus
call.MGRAST('m5', 'ta', filter='Bacteroidetes', filter_l='phylum', mi='genus', issue=issue)
### http://api.metagenomics.anl.gov/m5nr/sources
call.MGRAST('m5', 'so', issue=issue)
### http://api.metagenomics.anl.gov/m5nr/accession/YP_003268079.1
call.MGRAST('m5', 'ac', 'YP_003268079.1', issue=issue)
### http://api.metagenomics.anl.gov/m5nr/alias/IPR001478
call.MGRAST('m5', 'al', 'IPR001478', issue=issue)
### http://api.metagenomics.anl.gov/m5nr/md5/000821a2e2f63df1a3873e4b280002a8?source=InterPro
call.MGRAST('m5', 'md', '000821a2e2f63df1a3873e4b280002a8', so='Int', issue=issue)
### http://api.metagenomics.anl.gov/m5nr/function/sulfatase?source=GenBank
call.MGRAST('m5', 'fu', 'sulfatase', so='GenBank', issue=issue)
### http://api.metagenomics.anl.gov/m5nr/organism/akkermansia?source=KEGG
call.MGRAST('m5', 'or', 'akkermansia', so='KE', issue=issue)
### http://api.metagenomics.anl.gov/m5nr/sequence/MAGENHQWQGSIL?source=TrEMBL
call.MGRAST('m5', 'se', 'MAGENHQWQGSIL', so='Tr', issue=issue)

### http://api.metagenomics.anl.gov/matrix/organism?id=mgm4447943.3&id=mgm4447192.3&id=mgm4447102.3&id=mgm4447103.3&group_level=family&source=RefSeq&result_type=abundance&evalue=15
call.MGRAST('ma', 'or', id=c(4447943.3, 4447192.3, 4447102.3, 4447103.3), 
	gr='family', so='Ref', re='ab', ev=15, issue=issue)
### http://api.metagenomics.anl.gov/matrix/function?id=mgm4447943.3&id=mgm4447192.3&id=mgm4447102.3&id=mgm4447103.3&group_level=level3&source=Subsystems&result_type=abundance&identity=80&filter_level=phylum&filter=Firmicutes
call.MGRAST('ma', 'fu', id=c(4447943.3, 4447192.3, 4447102.3, 4447103.3), 
	gr='level3', so='Sub', re='ab', iden=80, filter_l='phylum', filter='Firmicutes', issue=issue)
### http://api.metagenomics.anl.gov/matrix/feature?id=mgm4447943.3&id=mgm4447192.3&id=mgm4447102.3&id=mgm4447103.3&source=KEGG&result_type=evalue&length=25
call.MGRAST('ma', 'fe', id=c(4447943.3, 4447192.3, 4447102.3, 4447103.3), 
	so='KE', re='ev', len=25, issue=issue)

### http://api.metagenomics.anl.gov/metadata/template
call.MGRAST('metadata', 'te', issue=issue)
### http://api.metagenomics.anl.gov/metadata/cv
call.MGRAST('metadata', 'cv', issue=issue)
### http://api.metagenomics.anl.gov/metadata/export/mgp128
call.MGRAST('metadata', 'ex', 128, issue=issue)

### http://api.metagenomics.anl.gov/metagenome?limit=20&order=name
call.MGRAST('metageno','qu', li=20, ord='na', issue=issue)
### http://api.metagenomics.anl.gov/metagenome/mgm4447943.3?verbosity=metadata
call.MGRAST('metageno','ins', 4447943.3, ve='me', issue=issue)

### http://api.metagenomics.anl.gov/project?limit=20&order=name
call.MGRAST('proj', 'qu', li=20, or='na', issue=issue)
### http://api.metagenomics.anl.gov/project/mgp128?verbosity=full
call.MGRAST('proj', 'ins', 128, ve='fu', issue=issue)

### http://api.metagenomics.anl.gov/sample?limit=20&order=name
call.MGRAST('sa', 'qu', li=20, or='na', issue=issue)
### http://api.metagenomics.anl.gov/sample/mgs25823?verbosity=full
call.MGRAST('sa', 'ins', 25823, ve='fu', issue=issue)

### http://api.metagenomics.anl.gov/validation/template/
call.MGRAST('va', 'te', issue=issue)
### http://api.metagenomics.anl.gov/validation/data/?template=
call.MGRAST('va', 'da', issue=issue)


# automate construction of the calls above, from the provided examples:
# tokenize
# match names/values (names sometimes missing)
# identify as opt / required
# identify param name
