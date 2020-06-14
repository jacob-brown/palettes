# -*- coding: utf-8 -*-

#if(missing(engine)){ 
#e <- match.arg(engine)	

# link limit is 20
	# first image is a google logo

getImages <- function(searchTerm, out="./", n=1, engine=c("google", "bing")){

	search_term <- gsub(" ", "+", searchTerm)


	# default to google search
	
	if(engine=="google"){ 

		url <- paste0("https://www.google.com/search?tbm=isch&q=", search_term)

	}else{

		url <- paste0("https://www.bing.com/images/search?q=sunset", search_term)

	}

	
	page <- read_html(url)
	node <- html_nodes(page, xpath = '//img')
	links <- html_attr(node, "src")
	cor_links <- links[2:length(links)]

	str_search <- gsub(" ", "_", searchTerm)
	command <- ""

	for(i in 1:n){
		tmp_command <- sprintf("wget -q -O web_images/%s.%s.jpg '%s';", str_search, as.character(i), cor_links[i])
		command <- paste0(command, tmp_command)

	}

	system(command)


}

plotSearch <- function(searchTerm, number, localpath, returnPaths=TRUE){

	# search for image term, plot images, and return paths

	getImages(searchTerm, out=localpath, n=number, engine="google")
	paths <- c()
	pdf("plot_images.pdf", 10, 5)
	
	if(number < 5){
		par(mfrow=c(1, number))
	}else{
		par(mfrow=c(2, ceiling(number/2)))
	}
	
	for(i in 1:number){
		
		sterm <- gsub(" ", "_", searchTerm)
		file <- sprintf("%s/%s.%s.jpg", localpath, sterm, as.character(i))
		paths <- append(paths, file)
		hex <- array2Hex(readImage(file))
		len <- length(unique(hex))
		mat <- matrix(seq(1:len),dim(hex)[1], dim(hex)[2])
		ratio <- dim(mat)[2]/dim(mat)[1]
		image(mat, axes=FALSE, xlab=file, col=hex, asp=ratio, useRaster=TRUE)
	}
	dev.off()
	system("open -a Skim.app plot_images.pdf")

	if(returnPaths){return(paths)}

}