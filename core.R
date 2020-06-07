# -*- coding: utf-8 -*-


library(jpeg)


pixelator <- function(matrix, pixelRatio){
	
	indexGet <- function(len, n){ 
	if(len %% n == 0){
		index <- seq(0, len, n)
	}else{
		index <- seq(0, len, n)
		index[length(index)] <- len # extend the last index
	}
		return(index)
	}

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
					
					### CHANGE HERE !?!?!? ###
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


plotVals <- function(df){

	pdf("plot.vals.pdf", 10, 10)
	par(mfrow = c(2, 3))
	# rgb
	df <- df[order(df$r),]
	barplot(df$r, main="r", col=df$pallet, names.arg=df$pallet, las=2)
	df <- df[order(df$g),]
	barplot(df$g, main="g", col=df$pallet, names.arg=df$pallet, las=2)
	df <- df[order(df$b),]
	barplot(df$b, main="b", col=df$pallet, names.arg=df$pallet, las=2)
	# hsl
	df <- df[order(df$hue),]
	barplot(df$hue, main="hue", col=df$pallet, names.arg=df$pallet, las=2)
	df <- df[order(df$saturation),]
	barplot(df$saturation, main="saturation", col=df$pallet, names.arg=df$pallet, las=2)
	df <- df[order(df$lightness),]
	barplot(df$lightness, main="lightness", col=df$pallet, names.arg=df$pallet, las=2)
	dev.off()
	system("open -a Skim.app plot.vals.pdf")

}


optimalK <- function(df, kmin=2, kmax=10){

	wss <- function(k) {
	  kmeans(df, k, nstart = 10, iter.max=30)$tot.withinss
	}

	# Compute and plot wss for k = 1 to k = 15
	k.values <- kmin:kmax
	wss_values <- sapply(k.values, function(x) wss(x))

	threshold <- 4500000
	k_optimal = min(which(wss_values <= threshold))
	return(k_optimal)

}



chooseRGB <- function(rmat, gmat, bmat){

	# choose pallet based on rgb distances #


	### convert to df for clustering ####
	diminsions <- dim(rmat)
	pixCount <- diminsions[1]*diminsions[2]
	x <- seq(1, diminsions[1])
	y <- seq(1, diminsions[1])
	coord <- expand.grid(x, y)

	# HSLV
	#hslv = getHSLV(as.vector(rmat), as.vector(bmat), as.vector(gmat))

	df <- data.frame(
		rowCo = x,
		colCo = y,
		r=as.vector(rmat),
		g=as.vector(gmat),
		b=as.vector(bmat)#,
		#hslv
		)

	# optimal k
	nColours <- optimalK(df[3:5])

	k <- kmeans(df[3:5], nColours, iter.max = 100)
	#k <- kmeans(df[3:9], nColours_adj, iter.max = 100)
	centers <- k$centers

	# order by biggest val col first
	maxs <- c(max(centers[,1]), max(centers[,2]), max(centers[,3]))
	col_order <- order(maxs)

	centers_ordered <- centers[order(centers[,col_order[1]], centers[,col_order[2]], centers[,col_order[3]]),]

	return(centers_ordered)
}

##################################################################

image_data <- readJPEG("img/lisa.jpg")
#image_data <- readJPEG("img/bill.jpg")
#image_data <- readJPEG("img/starry.jpg")
#image_data <- readJPEG("img/wave.jpg")
#image_data <- readJPEG("img/lily.jpg")
#image_data <- readJPEG("img/pollock.jpg")


pixRatio <- 0.3
scaled_data <- image_data * 255
rmat <- pixelator(scaled_data[,,1], pixRatio)
gmat <- pixelator(scaled_data[,,2], pixRatio)
bmat <- pixelator(scaled_data[,,3], pixRatio)



rgbVal <- chooseRGB(rmat, gmat, bmat)
pallet <- rgb(rgbVal[,1], rgbVal[,2], rgbVal[,3], maxColorValue=255)
nCols <- nrow(rgbVal)

hexImage <- combineRGB(rmat, gmat, bmat)

hexCount <- length(hexImage)
if(hexCount < nCols){
	nCols <- hexCount
}

mat <- hex2Mat(hexImage)
size <- dim(mat)
size <- c(size[1]/size[2], size[2]/size[1]) * 10
counts <- as.matrix(rep(1/nCols, nCols))

pdf("plot.pdf", 5, 5)
image(mat, axes=FALSE, col=hexImage)
barplot(counts, col = pallet, axes=FALSE)
dev.off()
system("open -a Skim.app plot.pdf")






