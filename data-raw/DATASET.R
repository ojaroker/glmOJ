## code to prepare `DATASET` dataset goes here

Greenberg26.dat = read.csv("/home/ojaroker/Colgate/Math/MATH454/Homework 3/Greenberg26.csv")
Greenberg26.dat$metro = as.factor(Greenberg26.dat$metro)
Greenberg26.dat$EPAregion = as.factor(Greenberg26.dat$EPAregion)

usethis::use_data(Greenberg26.dat, overwrite = TRUE)

Dahir25.dat <- read.csv("/home/ojaroker/Colgate/Math/MATH454/Homework 4/cs_replication_data.csv")
Dahir25.dat$modal_zone <- as.factor(Dahir25.dat$modal_zone)
Dahir25.dat$city       <- as.factor(Dahir25.dat$city)

cols_to_scale <- c("pnhwht", "pnhblk", "mhmval", "entropy",
                   "total_crime_rate", "pop", "hinc", "pvac")
for (col in cols_to_scale) {
  Dahir25.dat[[col]] <- ave(Dahir25.dat[[col]], Dahir25.dat$city,
                             FUN = function(x) as.vector(scale(x)))
}

usethis::use_data(Dahir25.dat, overwrite = TRUE)
