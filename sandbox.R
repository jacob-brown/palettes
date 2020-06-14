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

# lower the resolution
pixelated <- pixelator(image_data, shortLen=10)

# decide the pallete
pallet <- chooseRGB(pixelated, optimal=T, nColours=10, sensitivity=0.2)


###########################################
#--------------- Plotting ----------------#

plotImagePallet(pixelated, pallet)

###########################################
#----------- Image Search (web) ----------#

getImages("mona lisa", out="web_images/", n=1, engine="google")
getImages("picasso", out="web_images/", n=1, engine="google")
getImages("kalahari sunset", out="web_images/", n=1, engine="google")

###########################################
#---------------- General ----------------#

###########################################
#----------- Predefined Pallets ----------#

###########################################
#------------------ Data -----------------#