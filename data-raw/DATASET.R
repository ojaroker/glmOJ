## code to prepare `DATASET` dataset goes here

Greenberg26.dat = read.csv("/home/ojaroker/Colgate/Math/MATH454/Homework 3/Greenberg26.csv")
Greenberg26.dat$metro = as.factor(Greenberg26.dat$metro)
Greenberg26.dat$EPAregion = as.factor(Greenberg26.dat$EPAregion)

usethis::use_data(Greenberg26.dat, overwrite = TRUE)