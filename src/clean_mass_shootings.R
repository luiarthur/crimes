library(sqldf)

states = read.csv('../dat/states.csv')
right = function(x,n) substr(x, nchar(x)-n+1, nchar(x))
left = function(x,n) substr(x, 1, n)

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
shoot$auto= sapply(as.character(shoot$Type.of.weapons), function(x) grepl('auto', x))

counts = sqldf('
  SELECT MEDIAN(shoot.`Total.Victims`) AS sumVictims, shoot.state, shoot.abv FROM shoot
  GROUP BY shoot.state
')

counts = sqldf('
  SELECT * FROM counts
  ORDER BY counts.sumVictims DESC
')


write.csv(shoot, file='../dat/clean_mass_shooting_us.csv', quote=FALSE, row.names=F)
