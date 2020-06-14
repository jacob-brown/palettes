# -*- coding: utf-8 -*-

readImage <- function(location){
	
	# read local image in and scale

	return(readJPEG(location) * 255)


}


pixelator <- function(colarray, shortLen=10){
		
		# convert image to a lower resolution based on the short length of the image

		indexGet <- function(len, n){ 
				if(len %% n == 0){
					index <- seq(0, len, n)
				}else{
					index <- seq(0, len, n)
					index[length(index)] <- len # extend the last index
				}
					return(index)
				}

		# calculate dimension targets
		ind_short <- which.min(c(dim(colarray[,,1])[1], dim(colarray[,,1])[2]))
		length_short <- dim(colarray[,,1])[ind_short]
		
		if(shortLen >= length_short){print("shortLen same length as image length.")}


		psize <- floor(length_short/shortLen) # pixel size
		dimensions <- dim(colarray)  # row x col
		long <-  indexGet(dimensions[1], psize)
		short <- indexGet(dimensions[2], psize)


		if(length(long) == 1 | length(short) == 1){
		
			stop("pixel ratio too small, larger length required.")

		}else{

			long_blocks <- length(long)-1
			short_blocks <- length(short)-1

			### iterate through r, g, and b
			image_pixalated <- array(0, dim = c(0,0,3))

			array_reduced <- array(0, dim = c(long_blocks, short_blocks,3))

			for(ar_n in 1:3){

				### iterate long edge ###
				for(i in 1:long_blocks){

					lSta=long[i]+1
					lEnd=long[i+1]

					### iterate short edge ###
					for(c in 1:short_blocks){
						sSta = short[c]+1
						sEnd = short[c+1]
						
						### CHANGE HERE - mean, mode, median ###
						tmp_mean <- round(mean(colarray[lSta:lEnd, sSta:sEnd, ar_n]))
						#print(tmp_mean)
						array_reduced[i, c, ar_n] <- tmp_mean
						
						}
				} # end rgb iter

			}

		image_out <- array_reduced

		}


		
		return(image_out)
}


optimalK <- function(df, kmin=2, kmax=10, threshold){

	# determine the optimal K value to use for K means clustering

	wss <- function(k){
	  kmeans(df, k, nstart = 10, iter.max=10)$tot.withinss
	}

	# Compute and plot wss for k = 1 to k = 15
	k.values <- kmin:kmax
	wss_values <- sapply(k.values, function(x) wss(x))

	scale_values <- wss_values/max(wss_values)
	condition <- scale_values <= threshold

	if(TRUE %in% condition){
		k_optimal = min(which(condition))
	}else{
		# use highest k value
		k_optimal = NULL
	}	
	
	return(k_optimal)

}


chooseRGB <- function(colarray, optimal=FALSE, nColours=10, sensitivity=0.2){

	# choose pallet based on rgb distances 


	### convert to df for clustering ####
	diminsions <- dim(colarray)
	pixCount <- diminsions[1]*diminsions[2]
	x <- seq(1, diminsions[1])
	y <- seq(1, diminsions[1])
	coord <- expand.grid(x, y)

	# HSLV
	#hslv = getHSLV(as.vector(rmat), as.vector(bmat), as.vector(gmat))

	df <- data.frame(
		rowCo = x,
		colCo = y,
		r=as.vector(colarray[,,1]),
		g=as.vector(colarray[,,2]),
		b=as.vector(colarray[,,3])
		)

	# optimal k
	if(optimal){
		nCol <- optimalK(df, threshold=sensitivity)
		if(is.null(nCol)){
			print(paste0("optimiser struggling...using: nColours=", as.character(nColours)))
		}else{
			nColours <- nCol
		}
	}

	k <- kmeans(df[3:5], nColours, iter.max = 100)
	#k <- kmeans(df[3:9], nColours_adj, iter.max = 100)
	centers <- k$centers

	# order by biggest val col first
	maxs <- c(max(centers[,1]), max(centers[,2]), max(centers[,3]))
	col_order <- order(maxs)

	centers_ordered <- centers[order(centers[,col_order[1]], centers[,col_order[2]], centers[,col_order[3]]),]

	pal <- rgb(centers_ordered[,1], centers_ordered[,2], centers_ordered[,3], maxColorValue=255)

	return(pal)
}









