impose <- function(nPageMinimum,nSheetsPerSignature){

	# Add two blank leaves for book binding, one at the front and one at the back
	nBindingPages <- 4
	nPageMinimumPlusBinding <- nPageMinimum+nBindingPages

	# Calculate counts
	pagesPerSignature <- nSheetsPerSignature*4
	nLeaf <- ceiling(nPageMinimumPlusBinding/2)
	nSheet <- ceiling(nLeaf/2)
	nSignature <- ceiling(nSheet/(nSheetsPerSignature))

	# Get page vector ordered as signatures for printing
	pagesSig <- c()
	for(s in 1:nSignature){
		pWorking <- ((s-1)*pagesPerSignature+1):(s*pagesPerSignature)
		for(. in 1:(length(pWorking)/4)){
			pagesSig <- c(
				pagesSig,
				pWorking[length(pWorking)],
				pWorking[1],
				pWorking[2],
				pWorking[length(pWorking)-1]
			)
			pWorking <- pWorking[!pWorking%in%pagesSig]
		}
	}

	# Mark blank leaves as NA
	orderFinal <- pagesSig-nBindingPages/2
	orderFinal[which(orderFinal%in%orderFinal[order(orderFinal)][1:2])] <- 0
	orderFinal[which(orderFinal%in%orderFinal[order(orderFinal)][length(orderFinal):(length(orderFinal)-1)])] <- 0
	orderFinal[which(orderFinal==0)] <- NA

	# Count extra pages added
	nExtraPages <- max(orderFinal,na.rm=TRUE)-nPageMinimum

	# Return final order and count of extra pages added
	final <- list(
		order=as.integer(orderFinal),
		extraPages=as.integer(nExtraPages),
		signatures=as.integer(nSignature)
	)
	return(final)
}
