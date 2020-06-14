# -*- coding: utf-8 -*-

array2Hex <- function(rgbImage){

		### generate a matrix of hex values
		rotate <- function(x) t(apply(x, 2, rev))
		rmat <- rgbImage[,,1]
		gmat <- rgbImage[,,2]
		bmat <- rgbImage[,,3]

		### initate a new vec ###
		nRow <- dim(rmat)[1]
		nCol <- dim(rmat)[2]
		hmat <- matrix(0, nRow, nCol)
		
		for(r in 1:nRow){
			for(c in 1:nCol){
				hmat[r,c] <- rgb(rmat[r,c], gmat[r,c], bmat[r,c] , maxColorValue=255)				
			}
		}

		return(rotate(hmat))
	}

plotImagePallet <- function(rgbImage, pallet){
	
	# plot the low res image alongside the chosen colour pallet
		# plots with more colours and in more detail - asthetic only

	
	nCols <- length(pallet)
	hexImage <- array2Hex(rgbImage)

	### generate a matrix to plot hex values on 
	len <- length(unique(hexImage))
	mat <- matrix(seq(1:len),dim(hexImage)[1], dim(hexImage)[2])



	# configuration of plot size
	hexCount <- length(hexImage)
	if(hexCount < nCols){nCols <- hexCount}
	counts <- as.matrix(rep(1/nCols, nCols))
	ratio <- dim(mat)[2]/dim(mat)[1]

	# generate plot
	pdf("plot.pdf", 10, 10)
	layout(matrix(c(1,1,2,2), ncol=2, byrow=F), 
			widths=c(3,1), # three quarters of the plot window width, second one quarter.
			heights=c(2,2))
	image(mat, axes=FALSE, col=hexImage, asp = ratio)
	barplot(counts, col = pallet, axes=FALSE)
	dev.off()
	system("open -a Skim.app plot.pdf")
}


