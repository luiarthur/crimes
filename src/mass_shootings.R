library(maps)
library(sqldf)
library(rcommon)
source("logisticRegBayes.R")

states = read.csv('../dat/states.csv')

shoot = read.csv('../dat/mass_shooting_us.csv')
last = function(x) x[length(x)]

after = function(x) trimws(last(strsplit(as.character(x), ',')[[1]]))
before = function(x) trimws((strsplit(as.character(x), ',')[[1]])[1])

sanitize_state = function(x) {
  state = after(x)
  full = as.character(states[,1])
  abv = as.character(states[,2])
  idx = which(full == state | abv == state)
  state_pure = full[idx]
  abv_pure = abv[idx]
  paste0(state_pure, ',', abv_pure)
}

sane = as.character(sapply(shoot$Location, sanitize_state))
abv = as.character(sapply(sane, after))
state = as.character(sapply(sane, before))
county = as.character(sapply(shoot$Location, before))
shoot$state = state
shoot$abv = abv
shoot$county = county
shoot$countyState = sane
RaceClean = trimws(tolower(shoot$Race))
RaceClean[which(!is.na(match(RaceClean, c('other','unclear',''))))]='other'
shoot$RaceClean = RaceClean
shoot$WeaponsLegal = trimws(shoot$Weapons)
shoot$WeaponsLegal[24] = "Some"
shoot$WeaponsLegal[4] = "Yes"
shoot$Prior.signs.of.mental.health.issues = trimws(shoot$Prior)
shoot$WeaponsLegalYN = ifelse(shoot$WeaponsLegal == "Yes", 1, 0)
shoot$MentallyIll = ifelse(shoot$Prior == "Yes", 1, 0)

counts = sqldf('
  SELECT MEDIAN(shoot.`Total.Victims`) AS sumVictims, shoot.state, shoot.abv FROM shoot
  GROUP BY shoot.state
')

counts = sqldf('
  SELECT * FROM counts
  ORDER BY counts.sumVictims DESC
')

### Plot Map ###
#cutoff = quantile(counts$sum, c(.3, .5, .9, 1))
cutoff = c(1, 100, 500, max(counts$sum))
bucket = as.numeric(cut(counts$sum, cutoff))
col.pal <- colorRampPalette(c("grey", "red"))(length(cutoff) - 1)
map('state')
map('state', counts$state, col=col.pal[bucket], add=TRUE, fill=TRUE)

barplot(sort(table(shoot$RaceClean),dec=TRUE) / sum(table(shoot$RaceClean)))

### Multi-class Model ###
#mod = glm(as.factor(shoot$WeaponsLegal) ~ shoot$Prior, family='binomial')
#summary(mod)
#predict(mod, as.factor(shoot$WeaponsLegal))

table(shoot$WeaponsLegal, shoot$Prior)
plot(table(shoot$WeaponsLegal, shoot$Prior), main='',
     xlab='Weapons purchased legally', ylab='Prior signs of mental health issues')

### Binarized ###
mod = glm(shoot$WeaponsLegalYN ~ shoot$MentallyIll, family='binomial')
summary(mod)
table(shoot$WeaponsLegalYN, shoot$MentallyIll)

#source("logisticRegBayes.R")
### Bayesian Model ###
y = shoot$WeaponsLegalYN
X = cbind(1,shoot$MentallyIll)
out = logisticRegBayes(y, X, cs=1, thin=30, burn=2000)
b = t(sapply(out, function(o) o$coef))
post_summary(b)
plotPosts(b)

X_new = cbind(1,c(0,1))
p = sigmoid(X_new %*% t(b))

plotPost(p[2,] - p[1,], main='',
         xlab='Difference in proportion of legal gun-purchases by \n someone with history of mental health issues and someone with no history', trace=FALSE)
abline(v=0, lty=2, col='grey')

print(post_summary(as.matrix(cbind(p[2,] - p[1,])), alpha=.05))
