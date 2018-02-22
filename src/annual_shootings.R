library(rcommon)
source("clean_mass_shootings.R")

years = as.numeric(sapply(as.character(shoot$Date), right, 2))
years = ifelse(years <= 18, 2000+years, 1900+years)
all_years = min(years):max(years)
counts_years = sapply(all_years, function(yr) sum(yr == years))

N = length(all_years)
auto = double(N)
legal = double(N)
ill = double(N)
white = double(N)

for (i in 1:length(all_years)) {
  idx = all_years[i] == years
  ill[i] = sum(shoot$MentallyIll[idx])
  legal[i] = sum(shoot$WeaponsLegalYN[idx])
  white[i] = sum(shoot$RaceClean[idx] == 'white')
  auto[i]= sum(shoot$auto[idx])
}

annual_shootings = as.data.frame(cbind('year'=all_years, 'count'=counts_years, auto, legal, ill, white))
head(annual_shootings)

### Plots ###
plot(annual_shootings$year, annual_shootings$count, col='grey30',fg='grey',
     xaxt='n', xlab='', ylab='Mass Shootings', bty='n', type='l', lty=2)
axis(1, at=all_years, label=all_years, las=2, fg='grey', cex.axis=.8)

lines(annual_shootings$year, annual_shootings$legal, col='blue', pch=20, lwd=2)
lines(annual_shootings$year, annual_shootings$auto, col='red', pch=20, lwd=2)
lines(annual_shootings$year, annual_shootings$white, col='green', pch=20, lwd=2)
#lines(annual_shootings$year, annual_shootings$ill, col='orange', pch=20, lwd=2)
legend("topleft", col=c('grey','blue','red','green'), lwd=2, lty=c(2,1,1,1),
       leg=c('Counts','Legal','(semi)auto','white'), bty='n')

my.pairs(annual_shootings)
