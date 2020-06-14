# Palettes



## Functions

Pallet from image
* pixelate images
	- set the short axis pixel length
		- keep the ratio the same
		- deafault is 10 pixels
	- choice selection
		- mean, mode, median?
* choose rgb values 
* optimal k
* user function to filter unwanted colours
* optimiser
* read image in
	- detect if png, jpeg, etc.
	- combine with web search

Plotting
* combineRGB: pixelated image to a hex code matrix
* hex2Mat: generate a matrix to plot hex values on
* plot pixelated image and colour palette

Image Search (web)
* get images from the web
* user function to filter unwanted images
* usage of pallet from image functions

General
* colour blind check
* colour blind filter
* shiny interface

Predefined Pallets (gallery.R)
* search tool
* summary table
* summary plots

Data 
* pre-defined storage 
* user defined colour pallets 
	- prevents re-adding each time 
	- should be able to name the image, store date created, comments, and the hex values





## To Do

### 1. Image manipulation
* png, svg, ect. - read file in extension
* sensivity varies between images - optimal k
	- is there a proxy that could be used? distance between min and max colours?

### 2. Data storage
* json file of palettes
	- predefined
	- user defined colours 

### 3. colour blind checker
* `http://web.archive.org/web/20081014161121/http://www.colorjack.com/labs/colormatrix/ (<canvas> + ColorMatrix = Color Blindness)`


### 4. Usage
* better plot of image 
* plot colour pallet order
* selection of poor colours
* selection of poor images to average
* shiny app for interactive viewing

### 5. General
* os non specific 
	- wget of images


### X. Misc
* other sources of images
	- https://artuk.org/discover/artworks/view_as/grid/search/keyword:warhol


