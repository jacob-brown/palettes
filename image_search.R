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

