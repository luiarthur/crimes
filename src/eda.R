library(fields)
library(maps)

path = "../dat/Crimes_-_2001_to_present.csv"
#crimes = loadDF(path, "csv")
crimes = read.df(path, "csv", header="true", inferSchema="true")
colnames_df<-colnames(crimes)
colnames_df<-gsub(" ","_",colnames_df)
colnames(crimes)<-colnames_df
#dim(crimes)
#head(crimes)

#N = 10000
#small = take(dropna(crimes),N) # Take 10000
#counties = c('Cook')
#map('county', paste0('Illinois,', counties))
#points(x=small$Lon, y=small$Lat, col=rgb(0,0,1,.1), cex=.4)

small = collect(sample(dropna(crimes), TRUE, .001))
N = NROW(small)

quilt.plot(small$Lon,small$Lat,rep(1,N), bty='n', axes=F)
map('county', add=TRUE, col='grey30')

### Small model ###
mod = stats::glm(as.factor(Arrest) ~ as.factor(`Primary_Type`), family='binomial', data=small)

### Spark Model ###
ccrimes = dropna(crimes)
newDF = select(ccrimes, cast(ccrimes$Arrest, "Int"), ccrimes$Primary_Type, ccrimes$District)

### FIXME ??? ###
#modSpark = spark.glm(newDF, Arrest ~ Primary_Type, regParam=0.5, family='binomial')
modSpark <- spark.logit(newDF, Arrest ~ Primary_Type, regParam=0.5)
s <- summary(modSpark)
