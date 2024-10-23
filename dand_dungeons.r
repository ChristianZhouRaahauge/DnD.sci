#A file to explore the dungeoncrawling problem from D&D.SCI on LessWrong
#If you want to try the challenge yourself you can go find it at lesswrong.com/posts/boLvvZiRazNqipup7/d-and-d-sci-dungeoncrawling-the-crown-of-command

#Without further ado, let us look at the data (which I have saved as dand_dungeons.csv)

df <- read.csv("dand_dungeons.csv", stringsAsFactors = TRUE)
#There's 61 variables, so very tricky to get an overview. Let me just note what it is
#First column is the name of the dungeon
#Column 2 to 9 is the class and levels of the four adventurers (class, lvl, class, lvl etc)
#Column 10 to 21 is the list of encounters in the dungeon (some are empty)
#column 22 is the total number of encounters in the dungeon
#column 23 is the number that was beaten
#column 24 is whether or not the whole dungeon was beaten (0 or 1)
#column 25 is which encounter beat the adventurers (Goblins or Basilisks)
#The remaining columns are just derived from these 25 ones (numbers of each class, of each encounter etc)

#We need to find the optimal teams of adventurers for the following problems:
#1: Skeletons -> Poison Needle Trap -> Zombies -> Snake Pit -> Poison Needle Trap -> Skeletons -> Snake Pit -> Ghosts
#2: Orcs -> Snake Pit -> Wolves -> Snake Pit -> Unknown
#3: Goblins -> Boulder Trap -> Unknown x 8

#I will just start by getting some visual ideas of the problems

hist(df$X..Encounters.Beaten)
#None have been defeated at the first encounter

#I need to figure out if the dungeons are unique. With a bit of fiddling I see that for example
df[5,1] #Sunken Lair of Stormwind
#appear 7 times in the names. I check if it they are the same

slos <- subset(df, Dungeon.Name==df[5,1])
slos[,10:21]
#Nope, these are completely different encounters. So apparently it is not exactly the same dungeons even 
#if they have the same name
#Or, perhaps it is the same just going by different directions...'

#Let us try to make some subsets of the data to use

won <- subset(df, Victory.==1)
lost <- subset(df, Victory.==0)

hist(won$X..Encounters.Beaten)
hist(lost$X..Encounters.Beaten)

#Now, to make some assumptions and test them. First I will assume that each encounter is randomly
#chosen, so the encounter encountered does not depend on how far down you are. I will check this
#by looking at the summarys
# A quick look proves this wrong as for example Orc Warlords, there's 3 in lvl 1.  
# 11 in level 3 and 28 in level 4 so this seems highly unlikely to be chance

#SO obviously there is some level dependence on this. Let's examine a bit more

owl <- numeric(12)
tots <- numeric(12)
for (i in 1:12) {
  owl[i] <- sum(df[,9+i]=="Orc Warlord")
  tots[i] <- sum(df[,9+i]!="")
}

plot(owl/tots)
#Yes, definitely some level dependence on what encounters are met

#Another assumption is that each encounters hardness only depends on itself and the adventuring party
#not on the level you are at and the formerly encountered. This assumption could break down if for
#example orcs as encounter 5 were more dangerous than as encounter 1, or if the
#party took damage on the way

#This assumption seems pretty hard to check directly, so we will keep it for a while but be ready
#to abandon it if something is discovered. Let us now look at the adventurers

#looking at a few histograms like these
hist(lost$X..of.Fighter.Adventurers)
hist(won$X..of.Fighter.Adventurers)
#.. it seems that it its best to have a diverse party
#There are 6 classes: Cleric, Druid, Fighter, Mage, Ranger and Rogue

#Let us try to figure out the amount of

fdef <- summary(subset(lost, X..of.Fighter.Adventurers>0)$Defeated.By)
nfdef <- summary(subset(lost, X..of.Fighter.Adventurers==0)$Defeated.By)

fdef <- fdef/sum(fdef)
nfdef <- nfdef/sum(nfdef)

#Seems that there really is something going on here, but perhaps rather weakly

#There are 18 different encounters in total (column 44 to 61), I can find them quickly like this
encs <- levels(df$Encounter.3)

#I will now try to see what encounters are correlated to other encounters, like this:

tot_enc <- matrix(0, 18, 18) #Diagonals will just be the count of each
cor_enc <- matrix(0,18, 18)
for (i in 1:18) {
  for (j in 1:18){
    tot_enc[i,j] <- sum(df[,43+i]>0 & df[,43+j]>0)
    cor_enc[i,j] <- cor(df[,43+i],df[43+j])
  }  
}
#Now the problem is that the numbers on these are not in the same order as encs. Still we
#see that goblins and goblin chieftains are highly correlated, and so are wolves and all orcs,
#and all the undead (skeletons, ghosts, zombies and liches) are so, though a bit less

#I can play a bit around with the chances of winning based on whether a certain adventurer was
#present, and whether a certain encounter was met..
sum(df$X..of.Fighter.Adventurers==0 & df$X..of.Snake.Pit.Encounters>0 & df$Victory.==0)

#It seems the presence of a cleric changes the win rate from ca 50% to 80% when skeletons
#was present. Now to make a function that does this better and for all the 

class_enc_cor <- function(n_class,n_enc){
  
  #Takes as input a number betwen 1 and 6 (class) and between 1 and 18 (encounter)
  #returns how much higher the chances of winning with that encounter was if you had at
  #least 1 represent of the class compared to if you had none
  a= sum(df[,23+3*n_class]==0 & df[,43+n_enc]>0)
  b= sum(df[,24+3*n_class]==0 & df[,43+n_enc]>0 & df$Victory.==1 )
  c1 =b/a
  
  
  a= sum(df[,23+3*n_class]>0 & df[,43+n_enc]>0)
  b= sum(df[,23+3*n_class]>0 & df[,43+n_enc]>0 & df$Victory.==1 )
  c2 =b/a
  
  return( c2/c1 )
  
}

#Now I will run the function over all encounters and all classes

class_enc_mat <- matrix(0,6,18)

for (i in 1:6) {
  for (j in 1:18){
    class_enc_mat[i,j] <- class_enc_cor(i,j)
  }
  
}

image(class_enc_mat)

# In order to help me see the meaning, I will just make this helpful namelist

enc_list <- c("Goblins","Goblin Chieftain", "Wolves", "Orcs", "Orc Warlord", "Orc Shaman",
              "Skeletons", "Zombies", "Ghosts", "Basilisk", "Lich", "Dragon", "Boulder Trap",
              "Lever Puzzle Room", "Riddle Door", "Cursed Altar", "Snake Pit", 
              "Poison Needle Trap")
#By far the strongest correlation is between having a fighter and defeating orcs, 
#a ranger also helps with orcs, and a cleric seems pretty good against undead.
#strangely enough having or not having a rogue or a druid does not seem to matter very
#much... But this might be an artefact because of some selection effect.

#This is a good simple observation. We could try to make a slightly more complicated model
#where the chance of handling a given obstacle rise with the total level of each class

#But making this model would be easier if we instead looked at which encounters defeated
#the party and made predictions on which classes were present and at what level

#Making something like this:
summary(lm(lost$X..of.Cleric.Adventurers~lost$Defeated.By))
#Seems having clerics is statistically significantly related to being winning over:
#Cursed Altars, Ghosts, Liches, Skeletons and Zombies
#(as we expected)

summary(lm(lost$Total.Level.of.Cleric.Adventurers~lost$Defeated.By))
#Same story
summary(lm(lost$Total.Level.of.Rogue.Adventurers~lost$Defeated.By))
#Rogues are only statistically related to Poison Needle Traps, but there they are
#very much useful (coeff -2.8)

summary(lm(lost$Total.Level.of.Fighter.Adventurers~lost$Defeated.By))
#It seems that fighters are more often defeated by dragons, but that might just
#be because they are better at getting there. You also see more fighters when defeated by
#skeletons and liches and sombies and other undead, but this might just be because there is
#fewer levels for clerics

summary(lm(lost$Total.Level.of.Mage.Adventurers~lost$Defeated.By))
#Mages are best against Riddle Doors (-1.5), all else are not very significant

summary(lm(lost$Total.Level.of.Ranger.Adventurers~lost$Defeated.By))
#Rangers seems to excell against goblins(-1.4) but they are overrepresented when defeated by wolves(?)

summary(lm(lost$Total.Level.of.Druid.Adventurers~lost$Defeated.By))
#It would appear druids are extra good against Basilisks, perhaps? Seem very strange, they
#are positively correlated with all other things you could be defeated by

#_______________
#Time for a temporaray solution to be worked on from here
#The first dungeon filled with undead and posioned needles need clerics and a rogue

#The second dungeon have orcs, and perhaps an orc boss at the end. Needs some fighers

#Last one is probably a goblin dungeon with goblins and boulder traps. Definitely want
#some rangers in that group

#__________________________________________
#Just need to inspect the distribution of total levels
boxplot(rowSums(df[,24+3*(1:6)]))
#Pretty well distributed around a median of 14, with 50% between 11 and 17. Close to what
#our party will need
#_______________________
#Now I will check if there is some selection effect, ie if higher level groups go to longer
#dungeons

total_lvls <- rowSums(df[,24+3*(1:6)])
summary(lm(df$X..Encounters ~total_lvls))
#Yes, highly significant, one extra level of party equals 0.3 extra encounters

boxplot(df$X..Encounters~total_lvls) #Visually it seems a good description, with perhaps
#a standard deviation of 2 or 3 around the expected value
boxplot(total_lvls~df$X..Encounters) #Looks good here to, each extra encounter means around 3 more total levels
#________________________
#Now I will make a linear model for the chance of winning given number of encounters and total level

summary(lm(df$Victory.~df$X..Encounters+total_lvls))
# Start at 0.57, each encounter subtracts 0.036, each level adds +0.02

df$total_lvls <- total_lvls #Will just bind this here for ease in the future

#______________________________________________________

#Will try to look at only the dungeons that resemble the first one

like1 <- subset(df, df$X..of.Skeletons.Encounters>0 & df$X..of.Poison.Needle.Trap.Encounters>0
                & df$X..of.Snake.Pit.Encounters>0)
#130 dungeons of which 97 was won. Let us see if the ones who won had a lot of clerics
summary(like1$Defeated.By)

like2 <- subset(df, df$X..of.Orcs.Encounters>0 & df$X..of.Wolves.Encounters>0 & df$X..of.Snake.Pit.Encounters>0)
summary(like2$Defeated.By)
#Okay, pretty dangerous, of the ones that looked like 2, out of 63 dungeons 9 had dragons who beat the party
#So there is a chance the second dungeon have a dragon at the end
#That would also explain the name (den of Cheliax)

like3 <-subset(df, df$X..of.Goblins.Encounters>0 & df$X..of.Boulder.Trap.Encounters>0)

#After looking at these 3, I think I will do the following: 
#Dungeon 1: 2 clerics, 1 fighter 1 rogue
#Dungeon 2: 2 fighters, 2 clerics
#Dungeon 3: 2 rangers, 1 fighter, 1 cleric

#Since I am more afraid of nr 2 than nr 1 I will make all level 2 in that, level 4 in the second
# and level 3 in the third