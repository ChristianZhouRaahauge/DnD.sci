
df=read.csv("dand_dwarves.csv")

#Boxplotting a bit. Value is highly right skewed

boxplot(Fort.Value ~ Farmers.sent, data=df)

df$Biome = as.factor(df$Biome)

boxplot(Fort.Value ~ Biome, data=df) #No big difference

#Right now we have 168 000 observations. Let us first look at only the ones which
#match our future fort: Light Forest, Coal level 1 and good prospects of Copper, Hematite
#silver and Tin.

mydf <- subset(df, Biome=="Light Forest" & Coal.Level==1 & Copper.available.==1 
               & Hematite.available. ==1
               & Silver.available. ==1 
               & Tin.available. ==1,
               select = c(Size.of.Expedition, Miners.sent, Smiths.sent, Woodcutters.sent,
                          Farmers.sent, Brewers.sent, Warriors.sent, Crafters.sent,
                          Fort.Survived., Fort.Value))
#Okay, only 510 of these...
#But let us look at them!

summary(mydf)
boxplot(Fort.Value ~ Fort.Survived., data=mydf)
#Seems we have a 0.9 chance of survival using random dwarves. But let us see if we can make
#an even better way by making a randomforest!

library(randomForest)

bob <- sample(2, nrow(mydf), replace= TRUE, prob=c(0.2, 0.8))

mytrain <- mydf[ bob == 2, ]
mytest <- mydf[ bob ==1, ]

rf <- randomForest(Fort.Survived. ~  Miners.sent + Smiths.sent + Woodcutters.sent+
                                      Farmers.sent+ Brewers.sent + Warriors.sent + Crafters.sent,
                   data=mytrain, proximity=TRUE, ntrees=50) #ignore value and total, of course

pred <- predict(rf, mytest) #0l8 correlation. But that must partly be because it is 
#predicting into numbers between 0 and 1. What if we cut off and said:

sum(pred>0.6 & mytest$Fort.Survived.==1)
sum(mytest$Fort.Survived.) #Seems it got 103 correct, 1 false negative and 3 false positives.
    #Not bad

#I could then try to figure out the best way to use this tree model to plan my optimal selection of
#dwarves.

boxplot(mydf$Fort.Value ~ mydf$Miners.sent)
boxplot(mydf$Fort.Value ~mydf$Woodcutters.sent)
boxplot(mydf$Fort.Value~mydf$Smiths.sent)
boxplot(mydf$Fort.Value~mydf$Farmers.sent)
boxplot(mydf$Fort.Value~mydf$Brewers.sent)
#Etc, etc.
#So, we have 13 people to put in 7 boxes. I could try to make a totally random selection,
#run it, change it and see if the survival chance increases, and so on untill I get a maximal 
#value for survival.

Foo <- mydf[1,]
Foo[1,1:8] <- numeric(8)

#Now a loop for putting 13 dwarves in Foo, checking the prediction and seeing what happens
oldpred <- 0
best <- Foo
for(i in 1:1000){
  Bar <- sample(7,13, replace=TRUE)
  for (i in 1:13) {
    Foo[1, Bar[i]+1] = Foo[1, Bar[i]+1]+1
    
  }
  
  newpred <- predict(rf, Foo)
  if(newpred > oldpred){
    Best <-Foo
    oldpred <- newpred
  }
  Foo[1,2:8] <- numeric(7)
}

#This gives us the best place to start from a lot of random places. Let us see if
# we then can find something better starting from here

for (i in 1:100){
  Foo <- Best
  a1 <- sample(7,1)+1
  a2 <- sample(7,1)+1
  if( Foo[1, a1]>0){
    Foo[1,a1]=Foo[1,a1]-1
    Foo[1,a2]=Foo[1, a2]+1
    
    newpred <- predict(rf, Foo)
    if(newpred >= oldpred){
      Best <- Foo
      oldpred <- newpred
    }
  }
}

#Right now the best choice for survival seems to be 1, 2, 2, 3, 1, 2, 2
#Only 1 Brewer? Really?

#Okay, apparently you can change quite a lot around here and still hit the right amount
#of 100% predicted survival. But let us say that this is an alright method for starting

#Then we just need to find out how to change this to predict the highest possible value

valueforest <- randomForest(Fort.Value ~  Miners.sent + Smiths.sent + Woodcutters.sent+
                              Farmers.sent+ Brewers.sent + Warriors.sent + Crafters.sent,
                            data=mytrain, proximity=TRUE, ntrees=50)

predval <- predict(valueforest, mytest)
cor(predval, mytest$Fort.Value) #Only 0.79, but this seems mainly caused by bying unable to
#predict when it will not survive (value then goes to 0)

#If we instead combined these two, let us say that we only look at the cases where the other
#randomforest predicts survival

cor(predval[ pred >0.9], mytest$Fort.Value[pred>0.9]) #now it is 0.91
cor(predval[pred>0.95], mytest$Fort.Value[pred>0.95]) #still only 0.9

#So it seems we could chose to look at only the bits where rf predicts survival
#and then try to find the spot with the highest predicted value.

#I could easily reuse my loop for checking survival, and then iterate away from the current best
#spot for survival, checking random neighbors for higher value and whether they have an acceptable
#survival rate

oldval <- 0

for (i in 1:100){
  Foo <- Best
  a1 <- sample(7,1)+1
  a2 <- sample(7,1)+1
  if( Foo[1, a1]>0){
    Foo[1,a1]=Foo[1,a1]-1
    Foo[1,a2]=Foo[1, a2]+1
    
    newval <- predict(valueforest, Foo)
    
    newpred <- predict(rf, Foo)
    
    if(newval >= oldval && newpred >0.97){
      Best <- Foo
      oldpred <- newpred
      oldval <- newval
    }
  }
}

#Okay, the new Best seems to be 5 Miners, 1 smith 2 Farmers, 0 Brewers, 3 Warriors and 2 Crafters
#The model predicts a value of 569 for this
#Perhaps i should move one Crafter to be a brewer, just for fun.
#It seems to be quite a stable equilibrium

#Now, we will try to upload it, and see what happens:
#My fort has a 100% survival rate and expected value of 363. Quite Respectable

