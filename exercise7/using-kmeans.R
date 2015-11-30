rm(list = ls())
setwd('/home/nozes/github-machine-learning-coursera/exercise7')
source('kmeans-functions.R')

X <- as.matrix(read.csv('input/data2.txt', header=FALSE, sep=' '))
knn(X, 3)



## k-means on pixels.
library(png)
bird <- readPNG('input/bird_small.png')

red <- bird[,,1]
green <- bird[,,2]
blue <- bird[,,3]

bird[,,1] <- foo

foo <- matrix(data = rep(0, 128*128), nrow=128)
dim(foo)



install.packages('raster')
library(raster)
plotRGB(bird, r=3, g=2, b=1, )


writePNG(bird, target=raw())

plot(1:2, type='n')
rasterImage(bird, 1.2, 1.27, 1.8, 1.73)




X <- cbind(c(red), c(green), c(blue))
cluster <- knn(X, 16)



myColors <- 
  rbind(
    c(0.86513792, 0.71774791, 0.46023768),
    c(0.08159706, 0.08840137, 0.07808145),
    c(0.82020352, 0.73731695, 0.72639613),
    c(0.14874466, 0.15394778, 0.14580173),
    c(0.75523210, 0.60625627, 0.34445235),
    c(0.67686012, 0.60733106, 0.54879202),
    c(0.96345316, 0.85931373, 0.63301821),
    c(0.36383784, 0.34140425, 0.35832795),
    c(0.55853429, 0.46636616, 0.41160694),
    c(0.92068311, 0.58107211, 0.26357527),
    c(0.97558222, 0.94479366, 0.81820981),
    c(0.47230392, 0.34558370, 0.20297295),
    c(0.57379166, 0.72686194, 0.87089486),
    c(0.39875506, 0.47195767, 0.66756925),
    c(0.66205859, 0.47662106, 0.20584003),
    c(0.26301490, 0.23328013, 0.22124183)    
  )



dim(red)

for(i in 1:128) {
  for(j in 1:128) {
    index <- (i-1)*128 + j

    red[i, j] <- myColors[cluster[index],1]
    green[i, j] <- myColors[cluster[index],2]
    blue[i, j] <- myColors[cluster[index],3]
    
  }
}

bird[,,1] <- red
bird[,,2] <- green
bird[,,3] <- blue


plot(1:2, type='n')
rasterImage(bird, 1.2, 1.27, 1.8, 1.73)
