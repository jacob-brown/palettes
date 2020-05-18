# -*- coding: utf-8 -*-


library(jpeg)
#library(png)


indexGet <- function(len, n){ 
	if(len %% n == 0){
		index <- seq(0, len, n)
	}else{
		index <- seq(0, len, n)
		index[length(index)] <- len # extend the last index
	}
	return(index)
}


pixelator <- function(matrix, pixelRatio){
		# pixelRatio is the desired pixel ratio  
		#pixelRatio=0.03
		#matrix=scaled_data[,,1]
		totalPix <- dim(matrix)[1] * dim(matrix)[2]
		target_size <- dim(matrix) * pixelRatio
		scaledPix <- target_size[1] * target_size[2]
		psize <- floor(totalPix/scaledPix)
		#sqrt(psize)
		#psize <- floor(mean((dim(matrix)[1]/pixelRatio), (dim(matrix)[2]/pixelRatio)))

		dimensions <- dim(matrix)  # row x col

		long <-  indexGet(dimensions[1], psize)
		short <- indexGet(dimensions[2], psize)
		
		if(length(long) == 1 | length(short) == 1){
		
		#image_mat <- round(mean(matrix))
		stop("pixel ratio too small, choose a higher value")

		}else{

			long_blocks <- length(long)-1
			short_blocks <- length(short)-1

			mat_reduced <- matrix(,long_blocks, short_blocks)

			### iterate long edge ###
			for(i in 1:long_blocks){

				lSta=long[i]+1
				lEnd=long[i+1]

				### iterate short edge ###
				for(c in 1:short_blocks){
					sSta = short[c]+1
					sEnd = short[c+1]
					
					### CHANGE HERE !!!! ###
					tmp_mean <- round(mean(matrix[lSta:lEnd, sSta:sEnd]))
					#print(tmp_mean)
					mat_reduced[i, c] <- tmp_mean
					
				}


			}

		image_mat <- t(apply(mat_reduced, 2, rev)) # rotate

		}
		
		return(image_mat)
}

combineRGB <- function(rmat,gmat,bmat){

		if(length(rmat) == length(gmat) & 
			length(rmat) == length(bmat)){

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

			return(mat)

			}else{
				print("matrix different sizes")
			}

}


hex2Mat <- function(hexMat){

	len <- length(unique(hexMat))
	matOut <- matrix(seq(1:len),dim(hexMat)[1], dim(hexMat)[2])
	return(matOut)
}

choosePallet <- function(nColours, rmat, gmat, bmat){

	# choose pallet based on rgb distances #


	### convert to df for clustering ####
	diminsions <- dim(rmat)
	pixCount <- diminsions[1]*diminsions[2]
	x <- seq(1, diminsions[1])
	y <- seq(1, diminsions[1])
	coord <- expand.grid(x, y)

	df <- data.frame(
		rowCo = x,
		colCo = y,
		r=as.vector(rmat),
		g=as.vector(gmat),
		b=as.vector(bmat)
		)

	k <- kmeans(df[3:5], nColours)
	centers <- k$centers
	centers_ordered <- centers[order(centers[,1], centers[,2], centers[,3]),]
	pallet <- rgb(centers_ordered[,1], centers_ordered[,2], centers_ordered[,3], maxColorValue=255)
	pallet <- sort(pallet)

	return(pallet)
}


cMaxMin <- function(rmat, gmat, bmat){
	# R' = R/255 # if scaled
	# cmax = max(R', G', B')
	# cmin = min(R', G', B')
	if(sample(rmat,1) > 1){
		# already been converted to rgb
		rmat <- rmat/255
		gmat <- gmat/255
		bmat <- bmat/255
	}

	dims <- dim(rmat)
	cmax <- matrix(, dims[1], dims[2])
	cmin <- matrix(, dims[1], dims[2])
		
	for(row in 1:dims[1]){
		cmax[row,] <- pmax(rmat[row,], gmat[row,], bmat[row,])
		cmin[row,] <- pmin(rmat[row,], gmat[row,], bmat[row,])
	}

	return(list(cmax, cmin))

}

### HSL ###
getHSL <- function(rmat, gmat, bmat){

	cmaxmin <- cMaxMin(rmat, gmat, bmat)
	rr <- rmat/255
	gg <- gmat/255
	bb <- bmat/255

	cmax = cmaxmin[[1]]
	cmin = cmaxmin[[2]]
	diff <- cmax - cmin

	# luminescence
	lumi <- (cmax + cmin)/2

	### hue ###
		# ifelse for vectorisation
	hue <- ifelse(diff == 0, 0,
			ifelse(cmax == rr, (60 * ((gg - bb) / diff)), # r' is max
				ifelse(cmax == gg, (60 * (2 + ((bb - rr) / diff))), # g' is max
					(60 * (4 + ((rr - gg) / diff))) # b' is max
					)))

	hue_corrected <- ifelse(hue < 0, hue + 360, hue) # correct the negative hues

	### saturation ###
	sat <- ifelse(lumi < 0.5, (diff/(cmax - cmin)), (diff/(2 - cmax - cmin)))

	list_out <- list(hue_corrected, sat, lumi)
	
	return(list_out)
}

####################################################################################
####################################################################################

nCols <- 10
image_data <- readJPEG("img/lisa.jpg")
#image_data <- readJPEG("img/bill.jpg")
#image_data <- readJPEG("img/starry.jpg")
#image_data <- readJPEG("img/wave.jpg")
#image_data <- readJPEG("img/lily.jpg")
#image_data <- readJPEG("img/pollock.jpg")


pixRatio <- 0.1
scaled_data <- image_data * 255
r <- pixelator(scaled_data[,,1], pixRatio)
g <- pixelator(scaled_data[,,2], pixRatio)
b <- pixelator(scaled_data[,,3], pixRatio)


hsl_list <- getHSL(r,g,b)

hexImage <- combineRGB(r, g, b)
pallet <- choosePallet(6, r, g, b)

hexCount <- length(hexImage)
if(hexCount < nCols){
	nCols <- hexCount
}

#hexSample <- sort(sample(hexImage, nCols))
mat <- hex2Mat(hexImage)
size <- dim(mat)
size <- c(size[1]/size[2], size[2]/size[1]) * 10
counts <- as.matrix(rep(1/nCols, nCols))


pdf("plot.pdf", 5, 5)
image(mat, axes=FALSE, col=hexImage)
barplot(counts, col = pallet, axes=FALSE)
dev.off()
system("open -a Skim.app plot.pdf")



#ext <- tools::file_ext(image) 
#
#if(ext == "png"){
#
#}elseif(ext == "jpeg"){
#	
#}else{
#	print("unknown file type")
#}
