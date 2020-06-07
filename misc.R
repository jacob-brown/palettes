# -*- coding: utf-8 -*-
# Author: Jacob Brown
# Email j.brown19@imperial.ac.uk
# Date:   2020-06-07
# Last Modified: 2020-06-07

# Desc: 


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

	# matrix
	if(!is.null(dim(rmat))){

		dims <- dim(rmat)
		cmax <- matrix(, dims[1], dims[2])
		cmin <- matrix(, dims[1], dims[2])
			
		for(row in 1:dims[1]){
			cmax[row,] <- pmax(rmat[row,], gmat[row,], bmat[row,])
			cmin[row,] <- pmin(rmat[row,], gmat[row,], bmat[row,])
		}

	}else{
		cmax <- pmax(rmat, gmat, bmat)
		cmin <- pmin(rmat, gmat, bmat)
	}
	return(list(cmax, cmin))

}

### HSL ###
getHSLV <- function(rmat, gmat, bmat){

	cmaxmin <- cMaxMin(rmat, gmat, bmat)
	rr <- rmat/255
	gg <- gmat/255
	bb <- bmat/255

	cmax <- cmaxmin[[1]]
	cmin <- cmaxmin[[2]]
	diff <- cmax - cmin

	### lightness (proportion) ###
	light <- ((cmax + cmin)/2)

	### hue degree ###
		# ifelse for vectorisation
	hue <- ifelse(diff == 0, 0,
			ifelse(cmax == rr, (60 * ((gg - bb) / diff)), # r' is max
				ifelse(cmax == gg, (60 * (2 + ((bb - rr) / diff))), # g' is max
					(60 * (4 + ((rr - gg) / diff))) # b' is max
					)))

	hue_corrected <- ifelse(hue < 0, hue + 360, hue) # correct the negative hues

	### saturation (proportion) ###
	sat <- ifelse(light < 0.5, (diff/(cmax - cmin)), (diff/(2 - cmax - cmin)))

	### value ###
	# cmax

	out <- data.frame(hue = hue_corrected, 
					saturation = sat, 
					lightness = light,
					value = cmax)

	
	return(out)
}




#####
### colour distance ###
colourDist <- function(colour1, colour2){
	
	r <- (colour1[,1] + colour2[,1])/2
	dR <- colour1[,1] - colour2[,1]
	dG <- colour1[,2] - colour2[,2]
	dB <- colour1[,3] - colour2[,3]

	dC <- sqrt((2+(r/256)) * dR^2 + 4 * dG^2 + (2 + ((255-r)/256)) * dB^2)
	return(dC)


}


colDistHandler <- function(df_rgb){

	cat("\ncomputing distances...\n")

	#class(df_rgb) <- "character"
	str <- paste(df_rgb[,1], df_rgb[,2], df_rgb[,3], df_rgb[,4], sep=",")
	expand <- expand.grid(str, str)

	break_str <- function (i) do.call(rbind,strsplit(as.character(expand[,i]),','))
	
	rgb_compar <- cbind(break_str(1), break_str(2))
	
	colnames(rgb_compar) <- rep(c("r","g","b", "hex"), 2)

	colour1 <- rgb_compar[,1:3]
	hex1 <- rgb_compar[,4]
	colour2 <- rgb_compar[,5:7]
	hex2 <- rgb_compar[,8]
	class(colour1) <- "numeric"
	class(colour2) <- "numeric"

	dist <- colourDist(colour1, colour2)

	bind <- data.frame(cbind(hex1, hex2, dist))
	bind_order <- bind[rev(order(bind$dist)),]

	# remove diagonal duplicates by taking odd indexs
	out <- bind_order[seq(1, nrow(bind_order), 2),]	
	cat("done.")
	return(out)
}





################ Explore ####################
nCols <- 10
#image_data <- readJPEG("img/lisa.jpg")
#image_data <- readJPEG("img/bill.jpg")
#image_data <- readJPEG("img/starry.jpg")
image_data <- readJPEG("img/wave.jpg")
#image_data <- readJPEG("img/lily.jpg")
#image_data <- readJPEG("img/pollock.jpg")


pixRatio <- 0.3
scaled_data <- image_data * 255
rgbVal <- chooseRGB(nCols, 
					pixelator(scaled_data[,,1], pixRatio), 
					pixelator(scaled_data[,,2], pixRatio), 
					pixelator(scaled_data[,,3], pixRatio))
pallet <- rgb(rgbVal[,1], rgbVal[,2], rgbVal[,3], maxColorValue=255)
df <- data.frame(rgbVal) 
df <- cbind(df, pallet)
df$pallet <- as.character(df$pallet)
hsl_list <- getHSLV(as.vector(rgbVal[,1]), 
				as.vector(rgbVal[,2]), 
				as.vector(rgbVal[,3]))
df <- cbind(df, hsl_list)

plotVals(df)

# smoothing and expanding # 
	# don't smooth colours, it looks odd
	# as does the others
# round up or down the steps required by each of the vals

require(plotwidgets)

smootherV1 <- function(vect){

	diff <- max(vect) - min(vect)

	# if diff isn't a certain size scale
	step <- diff/length(vect)
	# smooth data
	out <- seq(min(vect), max(vect), length.out=nCols)

	return(out)

}

smootherV1 <- function(vect){

	diff <- max(vect) - min(vect)

	# if diff isn't a certain size scale
	step <- diff/length(vect)
	# smooth data
	out <- seq(min(vect), max(vect), length.out=nCols)

	return(out)

}

round(vect, 2)

df_smooth <- df
df_smooth$saturation <- smoother(df_smooth$saturation)
df_smooth$lightness <- round(df_smooth$lightness, 2)
df_smooth$hue <- round(df_smooth$hue)
#df_smooth$hue <- smoother(df_smooth$hue)

# update rgb 
df_smooth[,1:3] <- t(hsl2rgb(t(df_smooth[,5:7])))
# update pallete
df_smooth[,4] <- rgb(df_smooth[,1], df_smooth[,2], df_smooth[,3], maxColorValue=255)
plotVals(df_smooth)




