# -*- coding: utf-8 -*-
# Author: Jacob Brown
# Email j.brown19@imperial.ac.uk
# Date:   2020-06-13
# Last Modified: 2020-06-14

# Desc: 

library(jpeg)
library(rvest)

source("from_image.R")
source("image_search.R")
source("plotting.R")

###########################################
#----------- Pallet from image -----------#

# read in image 
#image_data <- readImage("img/lisa.jpg")
#image_data <- readImage("img/bill.jpg")
#image_data <- readImage("img/starry.jpg")
#image_data <- readImage("img/wave.jpg")
#image_data <- readImage("img/lily.jpg")
#image_data <- readImage("img/pollock.jpg")
image_data <- readImage("web_images/picasso.1.jpg")
#image_data <- readImage("web_images/mona_lisa.1.jpg")
#image_data <- readImage("web_images/kalahari_sunset.1.jpg")
#image_data <- readImage("web_images/frida_kahlo.3.jpg")

# lower the resolution
pixelated <- pixelator(image_data, shortLen=10)

# decide the pallete
pallet <- chooseRGB(pixelated, optimal=T, nColours=10, sensitivity=0.2)

###########################################
#--------------- Plotting ----------------#

plotImagePallet(pixelated, pallet)

###########################################
#----------- Image Search (web) ----------#

# option 1. user gets images and selects the desired one
getImages("mona lisa", out="web_images/", n=1, engine="google")
image_data <- readImage("web_images/mona_lisa.1.jpg")
	# continue as normal...

# option 2. built in functions to assist with selection
	# catch paths
paths <- plotSearch("mona lisa", 5, "web_images/")

# select the image
selected <- 1
image_data <- readImage(paths[selected])
	# continue as normal...


###########################################
#---------------- General ----------------#

###########################################
#----------- Predefined Pallets ----------#

###########################################
#------------------ Data -----------------#