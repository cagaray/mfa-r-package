### Clean the wines data for the package
wines <- read.csv('../../wines.csv')
wines$country <- c(rep('NZ', 4), rep('FR', 4), rep('CA', 4))
g <- c(6, 6, 6, 5, 6, 5, 4, 6, 5, 4)
sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
