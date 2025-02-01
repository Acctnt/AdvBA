#Git hub upload using the netflix titles 
#
#name: Autumn Schlecht
# purpose: to demonstate simple r-language to analyze raw data
# date: 01/31/2025
# script upload file: netflix.xls

install.packages(c("tidycensus", "dplyr", "ggplot2", "readxl"))

# Load the libraries
library(tidycensus)
library(dplyr)
library(ggplot2)
library(readxl)

# Set your working directory (replace with your actual path)
getwd()
setwd("C://Desktop/Adv BA Folder")

#pull in the file for netflix to set up for analysis
#data <- read_excel("path/to/your/file.xls")
data <- read.csv("netflix_titles.csv") 


#analyze how much data is in the file?
information <- file.info("netflix_titles.csv")
print(information$size)

#identify what are colomn headers
print(colnames(data))

#remove all empty cells or na from the data
data <- na.omit(data)

#analyze how many independent titles are listed
title <- data$title
unique_titles <- length(unique(title))
print(unique_titles)

# analyze what types of data are used in the titles
types <- class(title)
print(types)

"finding: based on the code pull it is hihgly possible that only the titles in 
the data set all, 8807 count, unique movie or TV Show titles are all characters and no
numbers."

#What kings of types are there from the Netflix data (movie, tv show, series)
cat_title <- data$type
unique_cat <- unique(cat_title)
print(unique_cat)

#revert to the clean data and anlyze the range of movie duration by minutes.
movies <- data[data$type == "Movie",]
movies$duration <- as.numeric(gsub(" min", "", movies$duration))

# Find the shortest and longest movie durations
shortest_movie <- min(movies$duration, na.rm = TRUE)
longest_movie <- max(movies$duration, na.rm = TRUE)

cat("Shortest movie duration:", shortest_movie, "minutes\n")
cat("Longest movie duration:", longest_movie, "minutes\n")


"Finding: Further analysis fo the Netflix data set identified there are only
two catagories of: Movie or TV Show. The Movies duration ranges from 3 minutes to 312 
minutes in run time duration."
