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


pixelator <- function(matrix, pixsize){

		#pixsize <- 100 # 2x2 pixel res
		dimensions <- dim(matrix)  # row x col

		# standadise orientation
		if(dimensions[1] >= dimensions[2]){
			# rows are longer
			matrix <- matrix
			transformed <- FALSE
		}else{
			#columns are longer
			matrix <- t(matrix) 
			transformed <- TRUE
		}

		# update dimensions
		dim_trans <- dim(matrix)

		long <-  indexGet(dim_trans[1], pixsize)
		short <- indexGet(dim_trans[2], pixsize)
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
				
				tmp_mean <- round(median(matrix[lSta:lEnd, sSta:sEnd]))
				#print(tmp_mean)
				mat_reduced[i, c] <- tmp_mean
				
			}


		}

		# transform back
		#if(transformed){mat_reduced <- t(mat_reduced)}
		rotate <- t(apply(mat_reduced, 2, rev))
		return(rotate)
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
					mat[r,c] <- rgb(rmat[r,c], gmat[r,c], bmat[r,c], , maxColorValue=255)				
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


#lightMatrix <- function(mat){
#	# return a matrix of binned categories of light 
#		# how intense the values are
#		# a balance from each category should be picked
#	reduced_dim <- dim(mat)
#	#### ROOM FOR IMPROVEMENT
#	light_mat <- mat
#	#### TO HERE ####
#	mat_dim <- dim(light_mat)
#	bins <- cut(light_mat, breaks=nCols, include.lowest=TRUE, labels=seq(1, nCols))
#	bins <- as.integer(bins)
#	bins_mat <- matrix(bins, mat_dim[1], mat_dim[2])
#	return(bins_mat)
#}

#image_data <- readJPEG("img/lisa.jpg")
#image_data <- readJPEG("img/bill.jpg")
image_data <- readJPEG("img/starry.jpg")
#image_data <- readJPEG("img/wave.jpg")
#image_data <- readJPEG("img/lily.jpg")
#image_data <- readJPEG("img/pollock.jpg")

# desired_pix should never be smaller than nCol
nCols <- 10
desired_pix <- 20

# scale
scaled_data <- image_data * 255

psize <- mean((dim(image_data)[1]/desired_pix), (dim(image_data)[2]/desired_pix))

r_reduced <- pixelator(scaled_data[,,1], psize)
g_reduced <- pixelator(scaled_data[,,2], psize)
b_reduced <- pixelator(scaled_data[,,3], psize)

cMaxMin <- cMaxMin(r_reduced, g_reduced, b_reduced)
microbenchmark::microbenchmark(cMaxMin(image_data[,,1], image_data[,,2], image_data[,,3])

### DO SOME MATH FOR THE RGB LIGHT MATRIX ####
#lightMatrix(r_reduced)
#lightMatrix(g_reduced)
#lightMatrix(b_reduced)
#r_reduced + g_reduced + b_reduced

###### TO HERE ######
### selecting the colours ###
# light

#lmat <- lightMatrix(r_reduced, g_reduced, b_reduced)
#hexImage <- combineRGB(r_reduced, g_reduced, b_reduced)
#set.seed(124) 
#
#long_hexImage <- matrix(hexImage, length(hexImage),1)
#long_lmat <- matrix(lmat, length(lmat),1)
#hex_light <- cbind(long_hexImage, long_lmat)
#runs <- sort(as.integer(unique(hex_light[,2])))
#store_hex <- c()
#for(i in runs){
#	tmp <- subset(hex_light, hex_light[,2] == i)
#	hex_val <- tmp[sample(nrow(tmp),1),1]
#	store_hex <- append(store_hex, hex_val)
#}
#
#hexSample <- store_hex



hexSample <- sort(sample(hexImage, nCols))
mat <- hex2Mat(hexImage)
size <- dim(mat)
size <- c(size[1]/size[2], size[2]/size[1]) * 10
counts <- as.matrix(rep(1/nCols, nCols))


#pdf("plot.pdf", size[1], size[2])
##image(mat, axes=FALSE, col=hexImage)
#barplot(counts, col = hexSample, axes=FALSE, horiz=TRUE)
#dev.off()
#system("open -a Skim.app plot.pdf")

#‘c(x1, x2, y1, y2)’

pdf("plot.pdf", 5, 5)
image(mat, axes=FALSE, col=hexImage)
barplot(counts, col = hexSample, axes=FALSE)
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
