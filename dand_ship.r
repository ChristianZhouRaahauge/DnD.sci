library(dplyr)
library(ggplot2)

#And now to import the data and change it a bit
original<-read.csv("dand_ship.csv")

#Now to turn it into a better system, we will try to clean it up a bit


for(i in 1:nrow(original)){
  original$damage[i] <- as.numeric( unlist( strsplit(original$damage[i],'%') )[1] )
}
original$damage <- as.numeric(original$damage)

df <- cbind(original, year = c(0))

for(i in 1:nrow(df)){
  date <- unlist(strsplit(df$month[i],'/'))
  df$month[i] <- date[1]
  df$year[i] <- date[2]
  
}

df$month <- as.numeric(df$month)
df$year <- as.numeric(df$year)

df$direction <- factor(df$direction)
df$encounter <- factor(df$encounter)

encounters <- levels(df$encounter)

#Now let us start just looking at it

boxplot(damage~encounter, df)

sunk <- (subset(df,damage==100)) #Okay, we never know the encounter if they sunk
#So we should probably make a new one, where they didn't sink

survived <-subset(df,damage <100)

boxplot(damage~encounter,survived)

#Sharks seem rather less dangerous than the rest. Probably the ones who are most likely to
#Actually destroy the ship is Demon Whales, Merpeople, Nessie and perhaps Crab monsters.
#The rest all seem to never do anything like 

#Now off to try to figure out how much damage each of these probably does

par(mfrow=c(4,3))
for( x in levels(df$encounter)){
  hist(subset(df,encounter==x)$damage, main=x)
  
}

#okay enought plotting, let us try to figure out what the chance of each known encounter
#is to actually cause a ship to sink

#For kraken, demon whale, nessie and merpeople I am going to assume it is nearly gaussian
#Of course, we can't find the exact mean from our clipped values....
#So we need to find the 
#   encounters<- rbind(encounters,number=c(0))
#encounters <-rbind(encounters, month=c(0))

#We make a correlation matrix and a heat map of it
bigbox <- cor(df)
heatmap(bigbox)

#Just by eyeballing it I can see that Earth and Ocean are anticorrelated, Lunar and Solar Correlated
#And Flame and Ash are correlated. The last is probably not very useful

par(mfrow=c(1,1))
boxplot(df$damage~df$month)

#Month and year doesn't really seem to play a factor in when monsters show up
boxplot(df$damage~df$encounter)

dw_dmg<-subset(survived,encounter=='demon whale')$damage
dw_dmg <- sort(dw_dmg)

crab_dmg <- sort( subset(survived,encounter=="crabmonsters")$damage)

pirate_dmg <- sort( subset(survived,encounter=="pirates")$damage)

nessie_dmg <- sort( subset(survived, encounter=='nessie')$damage)

mer_dmg <- sort( subset(survived, encounter=='merpeople')$damage)

kraken_dmg <- sort( subset(survived, encounter=="kraken")$damage)

water_dmg <- sort( subset(survived, encounter=="water elemental")$damage)
#Now some guesses. I guess that just eyeballing it, around 2/3 of attacks from 
#demon whales kill the ship. That is around 700 dead ships

#then the crabs. That actually seems pretty linear, perhaps 1/2 kills the ship.
#Add another 1000 dead ships

#Perhaps 20% of nessie attacks kills the ship, so a 200 more there

#Also perhaps 10% of merpeople do it. Another 300 or so.

#This gives a total expected amount of lost ships of close to 2200, close to what we see

#Just judgine from this, I will expect by far the best use of money is:
#Armed carpentes (20gp) halfing damage from crabmonsters
#20 oars, (20 gp) taking 40% of Demon Whales and Kraken
#At last, either 3 cannons or bribing the mer people. Problem with the bribe
#is Varsuvius law, so I stick to 3 cannons (30 gp)

#Let us say that we now still have a ca 1% chance of dying from merpeople pr trip, and perhaps
#if taking 40% of demonwhale damage reduce it from 700 to say 200 (another 1 % chance)
#And then the crabs have

