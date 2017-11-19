## An implementation of FP Growth is at http://www.borgelt.net/src/fpgrowth.zip, very hard to install
## Instead, we will be using the very similar eclat algorithm
## Dataset is at http://www.biz.uiowa.edu/faculty/jledolter/datamining/lastfm.csv

lastfm <- read.csv("http://www.biz.uiowa.edu/faculty/jledolter/datamining/lastfm.csv")
[1:19,]
length(lastfm$user)  ## 289,955 records in the file
lastfm$user <- factor(lastfm$user)
levels(lastfm$user)   ## 15,000 users
levels(lastfm$artist) ##  1,004 artists

## We are going to use apriori first to understand the difference in output
## So we need to install the arules package and load the arules library

library(arules)

## We will manipulate the data as follows
## 1. Split the data in vector x into groups defined in vector f

playlist <- split(x=lastfm[,"artist"],f=lastfm$user) 
playlist[1:2]

## 2.  An artist may be mentioned by the same user more than once, so we need to remove
## the duplicates before creating the incidence matrix

playlist <- lapply(playlist,unique)

## So, now let's see what the non-duplicate playlists look like!

playlist[1:2]

## The first two listeners (1 and 3) listen to the following bands 
## (What, no Nickelback?) (If you don't get the joke, just google Nickelback!)

playlist <- as(playlist,"transactions") 

## We will display the output as a list of "transactions" (data class defined in arules)
## with their item frequency (a.k.a. their support)

itemFrequency(playlist) 

## Woohoo!  Output with user support for each of the 1,004 bands
## In other words, this is the number of times the band is listened to 
## on the shopping trips of 15,000 users, computed as the relative frequency each artist is mentioned
## Let's plot the output of just the bands with a support of .08 and higher
## (Because how many people listen to Mozart voluntarily, anyway!?)

itemFrequencyPlot(playlist,support=.08,cex.names=1.5) 

## Plots the item frequencies (only bands with > % support)
## Let's see how that changes if we lower the support to .05

itemFrequencyPlot(playlist,support=.05,cex.names=1.5) 

## Ew! Too much data! Let's stick with .08 (although, hey, Rage Against the Machine is cooler
## than the Red Got Chili Peppers!)

## But we came here to build association rules, right?
## We want only rules with support > 0.01 and confidence > .50
## We don't want to clutter the output with super rare bands 

musicrules <- apriori(playlist,parameter=list(support=.01,confidence=.5)) 

inspect(musicrules)

## Too much unordered data. We need to sort this mess!
## Sorting by support, i.e. frequency

inspect(sort(subset(musicrules), by="support"))

## Now let's see what this looks like by confidence, i.e. rule strength 

inspect(sort(subset(musicrules), by="confidence"))

## That's a lot of Coldplay and Radiohead.  Ugh.
## Let's see how support and confidence work together in the lift metric.

inspect(sort(subset(musicrules), by="lift"))

## Ha! We can remove Coldplay and Radiohead from our output if we set the 
## cutoff for lift > 5.
## Remember that lift gives us the best quality rules

inspect(subset(musicrules, subset=lift > 5)) 

## Ha! No more Coldplay! No more Radiohead!

## Lastly, let's sort by confidence to make it easier to understand

inspect(sort(subset(musicrules, subset=lift > 5), by="confidence"))

## No surprises here ... Judas Priest => Iron Maiden! We might offer them Motorhead, too!

########################################################################

## Remember the Adult dataset?  We can play with that, too!
## This dataset installs with the arules package, so nothing to download.
## Because we have a lot of data to analyze (and this is apriori),
## we specify minlen (an integer value for the minimal number of items per item set) 
## as 1 and maxlen (an integer value for the maximal number of items per item set) as 10

library(arules)
data(AdultUCI)
dim(AdultUCI)
AdultUCI[1:3,]

## We are now converting the AdultUCI dataset into a typical sparse market basket dataset

AdultUCI[["fnlwgt"]] <- NULL
AdultUCI[["education-num"]] <- NULL
AdultUCI[["age"]] <- ordered(cut(AdultUCI[["age"]], c(15, 25, 45, 65, 100)), labels = c("Young", "Middle-aged", "Senior", "Old"))
AdultUCI[["hours-per-week"]] <- ordered(cut(AdultUCI[["hours-per-week"]], c(0, 25, 40, 60, 168)), labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))
AdultUCI[["capital-gain"]] <- ordered(cut(AdultUCI[["capital-gain"]], c(-Inf, 0, median(AdultUCI[["capital-gain"]][AdultUCI[["capital-gain"]] > 0]), Inf)), labels = c("None", "Low", "High"))
AdultUCI[["capital-loss"]] <- ordered(cut(AdultUCI[["capital-loss"]], c(-Inf, 0, median(AdultUCI[["capital-loss"]][AdultUCI[["capital-loss"]] > 0]), Inf)), labels = c("none", "low", "high"))

Adult_533 <- as(AdultUCI, "transactions")
Adult_533
summary(Adult_533)

## Ha! We now have a spare dataset and transaction matrix.
## Now we can transform the transaction matrix into incidence matrix

aa=as(Adult_533,"matrix")

## What does such a spare matrix look like?

aa[1:2,]

## A lot of true and false for each option--just like a good sparse dataset.
## That's why it is so important to condense the original attributes; 
## otherwise, we'd be looking at thousands of dimensions.
## Let's see what this looks like in a frequency plot.

itemFrequencyPlot(Adult_533[, itemFrequency(Adult) > 0.2], cex.names = 1)

## We see that capital-loss=None, capital-gain=None, and native-country=United States
## Are the biggest factors.  Makes sense, given the list we have.
## Ready for the rules

rules <- apriori(Adult_533, parameter = list(support = 0.01, confidence = 0.6))
rules

## Ugh. "set of 276443 rules". Well, with 115 dimensions, what would you expect?
## Let's take a look and then whittle the number down.

summary(rules)

## Looks like we have most rules between 4 and 8 parameters. It's easy to get lost ...
## What was the actual initial question?  INCOME, of course!
## Let's look at the income rules, then.

rulesIncomeSmall <- subset(rules, subset = rhs %in% "income=small" & lift > 1.2)
inspect(sort(rulesIncomeSmall, by = "confidence")[1:3])
rulesIncomeLarge <- subset(rules, subset = rhs %in% "income=large" & lift > 1.2)
inspect(sort(rulesIncomeLarge, by = "confidence")[1:3])

## How else can we look at the dataset?

######################### ECLAT ##########################################

## The Eclat algorithm is defined recursively: 
## 1. The initial call uses all the single items with their tidsets. 
## 2. In each recursive call, the function IntersectTidsets verifies each itemset-tidset pair 
## with all the other pairs to generate new candidates. If the new candidate is frequent
## it is added to the set. The algorithm searches in a DFS manner to find all the frequent sets.

library(arules)
data("Adult")
Adult

## Mine itemsets with minimum support of 0.1.
itemsets <- eclat(Adult, parameter = list(supp = 0.1, maxlen = 15))
inspect(itemsets)

## Mine frequent itemsets with Eclat.
fsets <- eclat(Adult, parameter = list(supp = 0.5))
inspect(fsets)

## Display the 5 itemsets with the highest support.
## STEPWISE: fsets.sorted <- (sort(subset(fsets),by="support"))
## STEPWISE: inspect(fsets.sorted)
## STEPWISE: fsets.top5 <- fsets.sorted [1:5,]
## STEPWISE: inspect(fsets.top5)

fsets.top5 <- (sort(subset(fsets,by="support",1:5,)))
                       
## Get the itemsets as a list
as(items(fsets.top5), "list")

## Get the itemsets as a binary matrix
as(items(fsets.top5), "matrix")

## Get the itemsets as a sparse matrix, a ngCMatrix from package Matrix.
## Warning: for efficiency reasons, the ngCMatrix you get is transposed
as(items(fsets.top5), "ngCMatrix")
