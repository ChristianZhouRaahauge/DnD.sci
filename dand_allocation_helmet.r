
df<-read.csv("dand_allocation_helmet.csv",stringsAsFactors = TRUE)

#After plotting and fitting a line to the Ofstev Rating it seems that it is falling
#with rouhly 1 point pr century. The total SD of Ofstev is around 10, so it has fallen half a
#standard deviation
#From the story, it is clear that this is due to more and more random allocation of students
#The last decade has been completely random, so we could try to use the data from here to
#figure out which character traits fit best with which House.

#First, taking the hint of Harry Potter, my guess is that Courage and Integrity goes best with Dragonslayer
# Intellect with Sepentyne
# Patience with Thought-Talon
# and not sure at all what to do with Patience and with Humblescrumb.

#Hypothesis: The Ofstev Rating of a given student is random, with a Standard Deviation of around 10,
#but the mean can be increased if he is put in the right house.

#Alternative, and very tricky Hypothesis: Actually, the house names have zero effect! The only
#thing that affects the Ofstev Rating is whether people are put in houses with people who share
#their traits! In that case, we have to first find out which traits have been slightly overrepresented
#in current houses, and then put people who are highest on those traits into the relevant houses.

#Historically, this last hypothesis requires that houses are completely characterized by their
#students, and not by their teachers.

#Let us start!
#First I make averages of all people sent into the houses every ten years
#Since the data goes from 1511 to 2021 there is a total of 51 such instances.

Dragonslayer=data.frame(Intellect=numeric(51),
                        Integrity=numeric(51),
                        Courage=numeric(51),
                        Reflexes=numeric(51),
                        Patience=numeric(51),
                        Number=numeric(51),
                        Rating=numeric(51) 
                        )

Humblescrumble=data.frame(Intellect=numeric(51),
                        Integrity=numeric(51),
                        Courage=numeric(51),
                        Reflexes=numeric(51),
                        Patience=numeric(51),
                        Number=numeric(51),
                        Rating=numeric(51) )
Serpentyne=data.frame(Intellect=numeric(51),
                          Integrity=numeric(51),
                          Courage=numeric(51),
                          Reflexes=numeric(51),
                          Patience=numeric(51),
                          Number=numeric(51),
                          Rating=numeric(51))
Thought_Talon=data.frame(Intellect=numeric(51),
                          Integrity=numeric(51),
                          Courage=numeric(51),
                          Reflexes=numeric(51),
                          Patience=numeric(51),
                          Number=numeric(51),
                          Rating=numeric(51) )

Bigone=cbind(Dragonslayer,Humblescrumble,Serpentyne,Thought_Talon) #Slightly tricky, but alright

#And now for a quick loop
Houses=levels(df$House)
for (i in 1:51) { 
  q = subset(df, Year>1501+i*10 & Year <= 1511 +i*10)
  for (j in 1:4){
    k=j*7-6  #Start value of the houses indexes in Bigone
    p = subset(q, House== Houses[j]) #Only the given house in the given years
    for(l in 1:5){
      Bigone[ i, k+l-1] = mean(p[,l+1])
      
    }
    Bigone[ i, k+5 ] = nrow(p)
    Bigone[ i, k+6 ] = mean(p$Ofstev.Rating)
    
  }
  
  
}
#Let's just make it easy to see again

Dragonslayer = Bigone[,1:7]
Humblescrumble = Bigone[,8:14]
Serpentyne = Bigone[,15:21]
Thought_Talon = Bigone[,22:28]

#Seems my first thought was right, that the courage of Dragonslayer used to be high, but
#has been falling recently. I should make such a plot for each, and see how it goes. Or just
#get LM to check for me
#The fall in Courage is greatly correlated with the fall in Rating. Nothing else, except a tiny bit
#of correlation with Reflexes...
plot(Dragonslayer$Reflexes) #Seems to be a statistical fluke

#Gonna try with each of the houses

summary(lm(Rating ~ Intellect + Integrity + Courage + Reflexes + Patience, data=Dragonslayer))

summary(lm(Rating ~ Intellect + Integrity + Courage + Reflexes + Patience, data=Humblescrumble))
#Big correlation with Integrity, small correlation with Intellect
plot(Humblescrumble$Integrity)
plot(Humblescrumble$Intellect)#Strange, seems humblescrumble have gotten slightly smarter...

summary(lm(Rating ~ Intellect + Integrity + Courage + Reflexes + Patience, data=Serpentyne))
#Strong Intellect, slight negative with courage, slight negative, positive with Reflexes and patience

plot(Serpentyne$Intellect)
plot(Serpentyne$Reflexes)
plot(Serpentyne$Courage)#Going up, others going down

summary(lm(Rating ~ Intellect + Integrity + Courage + Reflexes + Patience, data=Thought_Talon))
#Strong on Intellect, Negative on Reflexes and Strong on Patience

plot(Thought_Talon$Intellect)
plot(Thought_Talon$Reflexes)  #Going up with time, other two down with time
plot(Thought_Talon$Patience)

#Okay, so now we have it. Originally the courageous was in Dragonslayer, the Integral in 
#Humblescrumble,  Intelligent and Reflexive in Serpentyne and Intelligent and Patient in 
#Thought_Talon.
#Later on this have become random, and the scores have fallen.
#So far it fits with both hypothesis. I will just try to see if within the last five years there
#is any real difference between the different houses in the abilities

latest <- subset(df, Year>2015)

boxplot(Intellect ~ House, data=latest) #HumbleScrumble well Ahead, Dragonslayer a bit behind
boxplot(Integrity ~ House, data= latest )#All equal, Serpentyne a bit ahead perhaps
boxplot(Courage ~ House, data  = latest)  #Dragonslayer slightly ahead
boxplot(Reflexes ~ House, data  = latest) #Dragonslayer slightly ahead
boxplot(Patience ~ House, data  = latest) #Serpentyne slightly ahead, Humble last

#Let's just look at the rating
boxplot(Ofstev.Rating ~ House, data  = latest) #Pretty much the same

#Now to test my hypothesis! If a house is very speicalized, in should mean that the standard
#deviation of the mean of the five traits would be higher than if it were not specialized.
#(The means move apart under specialization)
#So I can define for each 10 year interval the standard deviation of the five means, and see
#how well, for each house, it predicts the mean rating.

#Also, I can try to look back, and see if at earlier stages having high courage was predicted
#a high rating in Dragonslayer, etc

early <- subset(df, Year<1600)
plot(Ofstev.Rating ~ Courage, data=subset(early, House=='Dragonslayer'))
#Looks like a strong correlation here
plot(Ofstev.Rating ~ Intellect, data=subset(early, House=='Dragonslayer'))
#ANd none here

#Now to make my master plan. I need to get the sd of all the traits in Bigone

sds <- data.frame( Dragonslayer = numeric(51),
                   Humblescrumble = numeric(51),
                   Serpentyne = numeric(51),
                    Thought_Talon = numeric(51))


for (i in 1:51) {
  for(j in 1:4){
    sds[i,j] = sd( Bigone[i, (j-1)*7 + 1:5] )
  }
  
}

#Cool! I can immediatly see that the SD's have been falling dramatically lately.
#Let us look at how they interact with the rating
  
#Looks like a good correlation for all of them, most strongly for Dragonslayer and Thought-Talon

#So what we are looking for is specialisation.
#... however, let me just look at it if we only look at the time when specialisation was lower
plot(sds$Thought_Talon[40:51], Thought_Talon$Rating[40:51])
#Less certain, but this might be because there is a smaller variation here

#It might also be that what we want is to have one of each place associated with one particularly
#high stat. That should give around the same as otherwise.

#Now to test a new thery!
#Based on what I saw earlier, I think that Courage and Integrity are actually two distinct traits.
#(Fine)
#But Intellect, Reflexes and Patience are actually made from two better traits, lets call them
# Ambition = Intellect + Reflexes - Patience
# Wisdom = Intellect + Patience - Reflexes
# So for each of those two, you want to optimise it.
#I will try to add such a two columns to df

df$Ambition <- df$Intellect + df$Reflexes -df$Patience
df$Wisdom <- df$Intellect + df$Patience - df$Reflexes

#Let us see now
summary(lm( Ofstev.Rating ~ Intellect, data=subset(df, House=="Serpentyne") ))
#Nope, that was not a good tricky, Ambition is a worse predictor of Rating than intellect for
#house Serpentyne. And also
summary(lm( Ofstev.Rating ~ Wisdom, data=subset(df, House=="Thought-Talon") ))
#And wisdom is a bad predictor for house Talon
#So probably the negative effects from Reflexes and Patience are purely caused by selection
#effects when distributing the students.

#But I could also check if perhaps the max of each house is better than the standard deviation

mxs <- data.frame( Dragonslayer = numeric(51),
                   Humblescrumble = numeric(51),
                   Serpentyne = numeric(51),
                   Thought_Talon = numeric(51))


for (i in 1:51) {
  for(j in 1:4){
    mxs[i,j] = max( Bigone[i, (j-1)*7 + 1:5] )
  }
  
}

plot(mxs$Dragonslayer, Dragonslayer$Rating)
plot(mxs$Humblescrumble, Humblescrumble$Rating)
plot(mxs$Serpentyne, Serpentyne$Rating)
plot(mxs$Thought_Talon, Thought_Talon$Rating)

#At a cursory glance, the standard deviation seems to do a better job predicting the Rating
#But let us see if we can't make some better way to do it. Perhaps if we tried to make one long
#array with all of the data

ratings <- c( Dragonslayer$Rating, Humblescrumble$Rating, Serpentyne$Rating, Thought_Talon$Rating)

longsds <- c(sds$Dragonslayer, sds$Humblescrumble, sds$Serpentyne, sds$Thought_Talon)
longmxs <- c(mxs$Dragonslayer, mxs$Humblescrumble, mxs$Serpentyne, mxs$Thought_Talon)

#lets look at it
summary(lm( ratings~longsds)) #One extra sds equal 1.09 Rating points
summary(lm (ratings~longmxs)) #One extra mxs equal 0.71 Rating points
#Max actuallty have a slightly higher t-value. Since there is more spread in max, we can't say
#use the slopes directly

#Let us look at correlations
cor(longsds, ratings)   #0.729
cor(longmxs, ratings)   #0.764

#So perhaps it is the best to try to optimize for having the highest maximum in mean of each

#A very simple idea might then be to figure out, right now which house have randomly become the
#highest on each trait, and put people who excell in that trait into those houses.

#This will mean that we will try to figure out the best way

for(i in 1:4){
  boxplot( subset(latest, House==Houses[i])[,2:6], main=Houses[i])
}

#And looking at it another way
traits=c("Intellect","Integrity","Courage","Reflexes","Patience")

for(i in 1:5){
  boxplot( latest[,i+1]~latest$House, main=traits[i])
}

#Humblescrumble is taking the lead in Intellect, followed by THought Talon, Dragonslayer lowest
#Integrity pretty even, Serpentyne slightly ahead
#Serpentyne and Dragonslayer both ahead in Courage
#Dragonslayer ahead in Reflexes
#Serpentyne slightly ahead in Patience

#So I will say I will divide the students like this:
#High Intellect -> HumbleScrumble
#High Integrity -> Serpentyne
#High Reflexes -> Dragonslayer
#High Patience -> Serpentyne
#People with mixed characteristics goes to Thought-Talon

#Or perhaps, instead I should use euclidian stat space. That is, I should try to find
#each students distance to the mean of the current students.

#To test this, I will compare the mean squared distance of all the students in each ten year block
#to the mean of the house in trait space during that block

meansq <- data.frame(
        Dragonslayer = numeric(51),
        Humblescrumble = numeric(51),
        Serpentyne = numeric(51),
        Thought_Talon = numeric(51) )

sq <- 0
for(i in 1:51){
  q = Bigone[i,]
  for (j in 1:4){
    m = (j-1)*7
    studs <- subset(df, Year>1501+i*10 & Year <= 1511 +i*10 & House == Houses[j])
    for (k in 1:nrow(studs)) {
      for (s in 1:5){
       sq = sq + (studs[k,s+1]-q[m+s])^2
      }
    }
    meansq[i,j]<- sq/ nrow(studs)
    sq=0
  }
}

longmeansq= c(meansq$Dragonslayer, meansq$Humblescrumble, meansq$Serpentyne, meansq$Thought_Talon)

plot(longmeansq, ratings)

cor(longmeansq, ratings) #Correlation of -0.73
#So explains a bit worse than just going by the maximum
#Pity, it was a good guess for a system. Anyway, we will just use our maximum

newstuds <- read.csv("dand_allocation_newstudents.csv", stringsAsFactors = TRUE)



#My former method, using their maxes would say
#Dragonslayer: C, G, H, N
#Humblescrumble: F, J, O, S, 
#Serpentyne: A, L, M, P, Q, R, T
#Thought Talon gets the remainder: B, D, E, I, K, 
#Wait a second
cor(df$Reflexes, df$Intellect) #Okay, all correlations between traits seems very very weak

#Will try it now!
#Got 17.05 (expected was 16.575) which was very bad, apparently

#I was too smart for my own good! Shows out that each house actually is unique, and have
#some pretty unique functions mapping traits to Ofsteve Rating.

#Let me just see what would have happened if we instead had used a simple Randomforest

library(randomForest)

ds <- subset(df, House=="Dragonslayer", select = -c(X,Year,House,Ambition,Wisdom))
hs <- subset(df, House=="Humblescrumble", select = -c(X,Year,House,Ambition,Wisdom))
sp <- subset(df, House=="Serpentyne", select = -c(X,Year,House,Ambition,Wisdom))
tt <- subset(df, House=="Thought-Talon", select = -c(X,Year,House,Ambition,Wisdom))

#Now we make the random forests, and check them.
#But first, we make the testing and training data

bob <- sample(2, nrow(ds), replace=TRUE, prob=c(0.2, 0.8))
ds_train <- ds[bob==2,]
ds_test <- ds[bob==1,]

ds_rf <- randomForest(Ofstev.Rating~ ., data=ds_train, proximity=TRUE, ntree=50)

ds_pred <- predict(ds_rf, ds_test)

plot(ds_pred, ds_test$Ofstev.Rating) #Wauw, a nice correlation here
ds_cor <- cor(ds_pred, ds_test$Ofstev.Rating)#A 0.836 correlation right out of the box!

#I really should have done this here first. Now I will try to do it for all the others and see

bob <- sample(2, nrow(hs), replace=TRUE, prob=c(0.2, 0.8))
hs_train <- hs[bob==2,]
hs_test <- hs[bob==1,]

hs_rf <- randomForest(Ofstev.Rating~ ., data=hs_train, proximity=TRUE, ntree=50)

hs_pred <- predict(hs_rf, hs_test)
hs_cor <- cor(hs_pred, hs_test$Ofstev.Rating) #0.84

#And for Serpentyne

bob <- sample(2, nrow(sp), replace=TRUE, prob=c(0.2, 0.8))
sp_train <- sp[bob==2,]
sp_test <- sp[bob==1,]

sp_rf <- randomForest(Ofstev.Rating~ ., data=sp_train, proximity=TRUE, ntree=50)

sp_pred <- predict(sp_rf, sp_test)
sp_cor <- cor(sp_pred, sp_test$Ofstev.Rating) #0.86 again

#For the talon


bob <- sample(2, nrow(tt), replace=TRUE, prob=c(0.2, 0.8))
tt_train <- tt[bob==2,]
tt_test <- tt[bob==1,]

tt_rf <- randomForest(Ofstev.Rating~ ., data=tt_train, proximity=TRUE, ntree=50)

tt_pred <- predict(tt_rf, tt_test)
tt_cor <- cor(tt_pred, tt_test$Ofstev.Rating) #0.89 !

##
#For funs sake we will see what would have happened if we had sent some sps to ds

misallocated <- predict(ds_rf, sp_test)
plot(misallocated, sp_test$Ofstev.Rating)
cor(misallocated, sp_test$Ofstev.Rating) #0.198, so we see they need their own house to work

##
#Now we just try to put all our people into the forests, and see where they will get the best result

gnubox <- numeric(20)
for (i in 1:20) {
  x <- newstuds[i,]
  ratepreds <- c( predict(ds_rf, x), predict(hs_rf, x), predict(sp_rf, x), predict(tt_rf,x) )
  gnubox[i] <- which( ratepreds == max(ratepreds) ) [1]
}
  
Houses[gnubox]
#Oh FLUM, this super silly super fast random forest method gave me Expected Value 24.75, 
#pretty close to the perfect of 29! I would have come in together with the other good
#people in the top rank, if I had just done this!
#Waugh