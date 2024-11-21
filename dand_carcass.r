df <- read.csv('dand_carcass.csv')

head(df)
#Now to clean it up, and make the prices into numbers etc

df$Species <- as.factor(df$Species)

df$Revenue <- as.numeric( unlist( strsplit(df$Revenue,'sp')))
df$Carver <- as.numeric( unlist( strsplit(df$Carver,'sp')))
df$Winning <- as.numeric( unlist( strsplit(df$Winning,'sp')))

head(df)

#Now, first I would like to try to figure out a way to see how much the revenue is
# based just on species and days. Lets check it

littlemod <- lm(Revenue ~ Species + Days, data=df)  #Residuals look alright...

#Na, I think it would be better if we mad three different models. But let us first check if 
#it looks linear

#make a subset for each
ww <- subset(df,Species=="Winter Wolf", Select = -c(Species))
#make a subset for each
ss <- subset(df,Species=="Snow Serpent", Select = -c(Species))
#make a subset for each
yt <- subset(df,Species=="Yeti", Select = -c(Species))

boxplot(ww$Revenue ~ ww$Days) #looks linear
boxplot(ss$Revenue ~ ss$Days) #does not look linear
boxplot(yt$Revenue ~ yt$Days) #looks linear again

#So we obviously need to make three models

wwmod <- lm(Revenue ~ Days, data=ww) #39 - 1.97 x
ssmod <- lm(Revenue ~ Days, data=ss)  #27 straight (sd 2.4)
ytmod <- lm(Revenue ~ Days, data=yt)  # 75 -3.37 x

#These give us an alright idea of what is going on. 

#But they might be distorted if Carver actually only bids on certain things. Let us check

carvers <- subset(df,!is.na(Revenue))

#It seems that carver usually win the bets for winter wolf less when it is very old or very young
#Also, carver win fewer bets for fresh snow serpent, but all in all he wins enough bets on all days
#to justify snow serpent revenue not depending on days of age

boxplot(Days ~ Species, data=carver)
boxplot(Days ~ Species, data=subset(df,is.na(Revenue)))
#Yep, it is definitely to young thirds where carver usually doesn't win the bid

#We now have an alright model for the expected revenue of each thing

#Let me just make another dataframe

ncarver <- subset(df, is.na(Revenue))

#To figure out the amount we expect each other person to bid from this is... tricky.

#But let me make super simple model

boxplot(Winning ~ Days, data=subset(ncarver, Species=="Snow Serpent"))

#There are some crazy people bidding wildly for new snake skins

boxplot(Winning ~ Days, data=subset(ncarver, Species=="Winter Wolf"))
#Same here, and for yetis. But after that it seems to be relatively constant after day 3

 

boxplot(Winning~Days,data=subset(carvers, Species=="Snow Serpent"))

#It seems that it might be a good idea to offer around 20 to 25 for a serpent on any day
#giving an alright chance of winning while still making some profit. 

#To estimate the winning bet I could just include the bets we won. That way we get
wwbetmod <- lm(Winning ~ Days, data=ww) #43.430 -3.105 * Days
#This model clearly underestimates for many days and overestimates for middle days.

ytbetmod <- lm(Winning ~ Days, data=yt) #55 - 2.2 * Days
#this one looks pretty good. Residuals have standard deviation 6.4

#What about only looking at bets that were not won?

wwotmod <- lm(Winning ~Days, data=subset(ncarver, Species=="Winter Wolf")) #Now 46 - 3.15 * Days
#But now the model is completely wacky. Obviously wrong. 

#A more likely model for both is that there is a linear drop which flattens out. For Winter Wolves
#The flattening happens at day 7, for Serpents at day 3. both seems to flatten around 
#day 3 with a winning bid of around 20 +- 5 for ss and 22 +- 2 for ww.
#Yetis actually seem to fall from 60 at day one down to 35 at day 10 with no flattening

#So a rough model would say. Yt win is 60 - 2.5* days
#                            ww is 43 - 3* days until day 7 where it stops at 22
                            #ss is 20 after day 2 and hard to predict before that

#Here of course we have counted ourselves as one of the betters. The real bets without us would 
#probably be slightly less. So betting with these prices should give us a good chance of
#winning. I will therefore suggest that we have a good chance of getting the carcass if we
#bid with these numbers

#however, let me just try to see if this have got something to do with when carver is winning
#and when others are

plot(Winning~Days, data=ww, col="blue", main="Winter Wolf wins")
points(Winning~Days, data=subset(ncarver, Species=="Winter Wolf"), col="red")

#Yes, it does seem that there is one guy who offers way too much in the early days, and another
#guy who always offer around 22. Since carver have been going with the 43 -3 * Days model
#He always win the early days and never the later ones. 


plot(Winning~Days, data=yt, col="blue", main="Yeti Wins")
points(Winning~Days , data=subset(ncarver, Species=="Yeti"), col="red")

#The same here, someone is bidding something on the low end for carver but decreasing, and then
#some person is always offering around 35. 

#The model tells me that winterwolves revenue drop below 22 at day (39 - 2 * 9 == 21) so we
#could easily bet say 27 from day 2 to day 6 and expect to make money

#Let us look at serpents

plot(Winning~Days, data=ss, col="blue", main="Serpent Wins")
points(Winning~Days , data=subset(ncarver, Species=="Snow Serpent"), col="red")

#Snow serpent again, someone is bidding around 15 +- 2 all the time, and some person is bidding
# a lot but very consistently the same for new ones. Since that guy takes all the new
# snakes we don't know what they are worth and will let him have them. For the others we
# could consistenly bid something like 19 and be almost certain to make at least 6 sp pr snake
# So let us just say we will always bid 19 for a snow serpent

#For a winter wolf we will bid 27, unless it is older than a week

#For a yeti...

#Let us see, the model (75 - 3.4 pr day) seems good. It also seems we can actually get them if
#we just consistently bet more than the one person who keeps bidding exactly 55- 6 * days 
#So we will bet exactly 56 - 6 * days (one extra to be safe) for day 0, 1, 2 and 3. 
#After that we are looking at a person bidding between 40 to 35. Here we have to remember
#by day 4 we expect around 61 in Revenue so we can keep going betting 38 to get them, until
# we hit day 10. Actually even there there is a small profit, but not really worth using money
#to get it. I will say we bid 38 until day 8. 

#Now to test these methods