# -*- coding: utf-8 -*-

combineRGB <- function(colarray){

		rotate <- function(x) t(apply(x, 2, rev))
		
		# pixalated image to a hex code matrix
		rmat <- colarray[,,1]
		gmat <- colarray[,,2]
		bmat <- colarray[,,3]

		### initate a new vec ###
		nRow <- dim(rmat)[1]
		nCol <- dim(rmat)[2]
		#mat <- matrix(0, len, dim(rmat)[2])
		mat <- matrix(0, nRow, nCol)
		
		for(r in 1:nRow){
			for(c in 1:nCol){
				mat[r,c] <- rgb(rmat[r,c], gmat[r,c], bmat[r,c] , maxColorValue=255)				
			}
		}

		return(rotate(mat))


}

#hex2Mat <- function(hexImage){
#
#	# generate a matrix to plot hex values on 
#	len <- length(unique(hexImage))
#	matOut <- matrix(seq(1:len),dim(hexImage)[1], dim(hexImage)[2])
#	return(matOut)
#}


plotImagePallet <- function(rgbImage, pallet){
	
	# plot the low res image alongside the chosen colour pallet
		# plots with more colours and in more detail - asthetic only

	rotate <- function(x) t(apply(x, 2, rev))
	nCols <- length(pallet)

	# generate a matrix of hex values			
		
	# pixalated image to a hex code matrix
	rmat <- rgbImage[,,1]
	gmat <- rgbImage[,,2]
	bmat <- rgbImage[,,3]

	### initate a new vec ###
	nRow <- dim(rmat)[1]
	nCol <- dim(rmat)[2]
	mat <- matrix(0, nRow, nCol)
	
	for(r in 1:nRow){
		for(c in 1:nCol){
			mat[r,c] <- rgb(rmat[r,c], gmat[r,c], bmat[r,c] , maxColorValue=255)				
		}
	}

	hexImage <- rotate(mat) # rotate

	#mat <- hex2Mat(hexImage)
	# generate a matrix to plot hex values on 
	len <- length(unique(hexImage))
	mat <- matrix(seq(1:len),dim(hexImage)[1], dim(hexImage)[2])


	# general configuration of plots
	hexCount <- length(hexImage)
	if(hexCount < nCols){nCols <- hexCount}
	size <- dim(mat)
	size <- c(size[1]/size[2], size[2]/size[1]) * 10
	counts <- as.matrix(rep(1/nCols, nCols))

	# generate plot
	pdf("plot.pdf", 5, 7)
	image(mat, axes=FALSE, col=hexImage)
	barplot(counts, col = pallet, axes=FALSE)
	dev.off()
	system("open -a Skim.app plot.pdf")
}


