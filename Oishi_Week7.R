##PC1. Download this dataset in Stata DTA format which contains an anonymized and 
##reduced version of thedata visualized in the Buechley and Hill paper on Lilypad. 

setwd("documents")
lilydata <- read.dta("lilypad_anonymized.dta")

##Once you have it: 
##(PC1 a) Reproduce both Table 1 and Table 2 (just US users) using the dataset (as closely as possible). 

gendertable<- table(lilydata$gender, lilydata$order_type)
colnames(gendertable)<- c("Arduino", "Both", "Lilypad")
row.names(gendertable)<- c("Female", "Male", "Unknown")
gendertable

USdata<- subset(lilydata, country == 81)
UStable<- table(USdata$gender, USdata$order_type)
colnames(UStable)<- c("Arduino", "Both", "Lilypad")
row.names(UStable)<- c("Female", "Male", "Unknown")
UStable

##(PC1 b) Run a {\displaystyle \chi ^{2}} {\displaystyle \chi ^{2}}-test on both tables. 
##Compare to the paper (for Table 1, there doesn't seem to be a {\displaystyle \chi ^{2}} {\displaystyle \chi ^{2}} test for Table 2). 
##Did you reproduce it?
chisq.test(gendertable)
chisq.test(UStable)

##(PC1 c) Install the package "gmodels" and try to display the table using the function CrossTable(). 
##This will give you output very similar to SPSS.

install.packages("gmodels")
library("gmodels")
CrossTable(gendertable)
CrossTable(UStable)

##(PC1 d) It's important to be able to import tables directly into your word processor without cuttingand pasting individual cells. 
##Can you export the output of your table? There are a bunch of functions you can use to do this. 
##I used the "xtable" package but I think that write.table() and Excel would do the job just as well.

write.table(gendertable)
write.table(gendertable, file = "SOS")

#PC2. At the Community Data Science Workshops we had two parallel afternoon sessions on Day 1. 
#In my session, there were 42 participants. In Tommy Guy's session, there were only 19. 
#The next week (Day 2), we asked folks to raise their hands if they had been in Tommy's session (14 did ) 
#and how many had been in mine (31 did). There was clearly attrition in both groups! 
#Was there more attrition in one group than another? 
#Try answering this both with a test of proportions 
#(prop.test()) and with a {\displaystyle \chi ^{2}} {\displaystyle \chi ^{2}}. 
#Compare your answers. 
#Is there convincing evidence that there is a dependence between instructor and attrition?
tommytable<- rbind(c(42, 31), c(19, 14))
tommytable

colnames(tommytable)<- c("Day 1", "Day 2")
rownames(tommytable)<- c("Mako", "Tommy")

prop.test(tommytable)

chisq.test(tommytable)
# p > 1: we retain the null hypothesis that they are independent 


##PC3. Download this dataset that was just published on 
##"The Effect of Images of Michelle Obama’s Face on Trick-or-Treaters’ Dietary Choices: A Randomized Control Trial." 
##The paper doesn't seem to have even been published yet so I think the abstract is all we have. 
##We'll come back to it again next week.

##(a) Download and import the data into R. I needed to install the "readstata13" package to do so.

Halloween2012.2014.2015_PLOS <- read.delim("~/Downloads/Halloween2012-2014-2015_PLOS.tab")
View(Halloween2012.2014.2015_PLOS)

##(b) Take a look at the codebook if necessary.
##Recode the data on being presented with Michelle Obama's face and the data on whether or not kids picked up fruit. 
##we'll leave it at that for now.

fruitface<-table(Halloween2012.2014.2015_PLOS$obama, Halloween2012.2014.2015_PLOS$fruit)
colnames(fruitface)<- c("No", "yes")
rownames(fruitface)<- c("No", "Yes")
fruitface 


##(c) Do a simple test on whether or not the two groups are dependent. 
##Be ready to report those tests. 
##The results in the paper will use linear regression. 
##Do you have a sense, from your reading, why your results using the coding material we've learned might be different?


prop.test(fruitface)



