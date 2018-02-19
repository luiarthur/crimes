library(fields)
library(maps)

path = "../dat/Crimes_-_2001_to_present.csv"
#crimes = loadDF(path, "csv")
crimes = read.df(path, "csv", header="true", inferSchema="true")
dim(crimes)
head(crimes)


#N = 10000
#small = take(dropna(crimes),N) # Take 10000
#counties = c('Cook')
#map('county', paste0('Illinois,', counties))
#points(x=small$Lon, y=small$Lat, col=rgb(0,0,1,.1), cex=.4)

small = collect(sample(dropna(crimes), TRUE, .001))
N = NROW(small)

quilt.plot(small$Lon,small$Lat,rep(1,N), bty='n', axes=F)
map('county', add=TRUE, col='grey30')
