df <- read.csv('dand_snark.csv', stringsAsFactors = TRUE)
summary(df)

wt <- numeric(nrow(df))
for(i in 1:nrow(df)){
      x <-unlist(strsplit(as.character(df$Waking.Time[i]),':'))
      wt[i] <- as.numeric(x[1])*60 + as.numeric( substr( x[2],1,2 ) ) + 12*60*(substr( x[2],3,3)=='p')
        
      
}

df$Waking.Time <- wt

df
#Now to check everything. First we will try to see if the mean of waking times are different for
#boojums

summary( subset(df, Boojum.=='Yes')$Waking.Time )
#Yes, there is a small change in their waking pattern, known Boojums get up around 15 minutes earlier,
#But have a lower spread

mosaicplot( table(df$Financial.Phobia, df$Boojum.) )

#Okay, I have a guess that most of the unknown Snarks are actually Boojums

mosaicplot( table(df$Cleanliness, df$Boojum.))
mosaicplot( table(df$Fondness.for.Washing.Machines,df$Cleanliness,df$Boojum.))

#Let us see if we cant find a strong predictor

#Perhaps I will try to add a two new columns to the data with tast

#df <- cbind(df, prefix = c(''), postfix=c(''))

for(i in 1:nrow(df)){
  #df$prefix[i] <- ( unlist( strsplit( as.character(df$Taste[i]), ', yet '))[1])
  #df$postfix[i] <- ( unlist( strsplit( as.character(df$Taste[i]), ', yet '))[2] )
}
#Now we turn them into factors

df$prefix <- as.factor(df$prefix)
df$postfix <- as.factor(df$postfix)

#One more thing to do. Apparently all the unknown Snarks are the ones that we never hunted.
#So there is absolutely no reason to include them in the dataset.
#Let us get rid of them altogether

df <- subset(df, Hunted.=='Yes')

#That saved us a good amount of space there
df <- droplevels(df)
#I will now try to make a random forest model to figure out which is which. I will take some time
#But let us try it anyway

selector <- sample(2, nrow(df), replace= TRUE, prob=c( 0.9, 0.1))

traindata <- df[selector==2,]
testdata <- df[selector==1,]


#remember to include the library

library(randomForest)

#Now we make a randomForest model

snark_rf <- randomForest(Boojum. ~ prefix + postfix + Waking.Time + Fondness.for.Washing.Machines
                         + Cleanliness + Financial.Phobia,
                         data=traindata,
                         proximity=TRUE,
                         ntree=50)

smallpred <- predict(snark_rf, testdata[1:5000,])

cluck <- sum(smallpred == testdata$Boojum.[1:5000])/5000
#Okay, it seems to have an almost 98% prediction rate


#This is so impressive that I will immediatly try to predict from the possible snarks


snark_marks <- read.csv('dand_snark_test.csv', stringsAsFactors = TRUE)

#Now just need to run it throught the same cleanup process as I did df (should have made a function)

wt <- numeric(nrow(snark_marks))
for(i in 1:nrow(snark_marks)){
  x <-unlist(strsplit(as.character(snark_marks$Waking.Time[i]),':'))
  wt[i] <- as.numeric(x[1])*60 + as.numeric( substr( x[2],1,2 ) ) + 12*60*(substr( x[2],3,3)=='p')
  
  
}

snark_marks$Waking.Time <-wt

#snark_marks <- cbind(snark_marks, prefix = c(''), postfix=c(''))

for(i in 1:nrow(snark_marks)){
  snark_marks$prefix[i] <- ( unlist( strsplit( as.character(snark_marks$Taste[i]), ', yet '))[1])
  snark_marks$postfix[i] <- ( unlist( strsplit( as.character(snark_marks$Taste[i]), ', yet '))[2] )
}
#Now we turn them into factors

snark_marks$prefix <- as.factor(snark_marks$prefix)
snark_marks$postfix <- as.factor(snark_marks$postfix)

#Now I will try to make a prediction for them

q=numeric(nrow(snark_marks))
for(i in 1:nrow(snark_marks)){
  q[i] <- as.numeric(charToRaw(as.character(snark_marks$Snark[i])))-65
}


#rename, so they have same names
names(snark_marks)[1] <- 'Snark.ID'

snark_marks$Snark.ID <- q

#Big problem! We have never hunted a crumbling snark! That means the taste crumbling totally confuses
#the tree model. Let us get rid of it

ts <- subset(snark_marks, prefix !='Crumbling')
ts <- droplevels(ts)
#Now prediction time

pred <- predict(snark_rf,ts)

#It worked (kindof)

risks <- read.csv('dand_snark_risks.csv')

s <- 1
numb <- 0
for(i in 1:length(ts$Snark.ID)){
  if(risks$chance[ ts$Snark.ID[i]+1]<50){
    numb = numb+1
    s = s*(1-risks$chance[ ts$Snark.ID[i]+1]*0.01)
  
  }
}
  