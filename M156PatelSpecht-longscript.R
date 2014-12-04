### MATH E-156 Final Project 
### AP & HMS

######################################################################################
#                                                                                    #
# Statistical Analysis on the Political Contribution of Harvard University Employees #
#                                                                                    #
######################################################################################


#################################################
##### Libraries
# install.packages("zoo", "reshape2")
library("zoo")
library("reshape2")
#################################################


######################################################################################
##### Required Technical Elements - The Dataset
######################################################################################


#################################################
##### Loading the data sets
contribs <- read.csv("harvard-contributions.csv")
people <- read.csv("harvard-people.csv"); head(people); nrow(people)
contribs.tagged <- read.csv("harvard-contributions-2011-2014-tagged.csv")
#################################################


#################################################
##### Explaining the data sets

##### First data set: all Harvard University employee contributions, 2001-2014
head(contribs) # As you can see, this data set has individual entries for each transaction

# Let's look at the kind of data columns we have here:
names(contribs)
# X is a remnant of the row number from the original spreadsheet
# NAME, ZIP_CODE, EMPLOYER, OCCUPATION, TRANSACTION_DT (date), 
# TRANSACTION_AMT (amount), PARTY are all fairly self-explanatory

# TRANSACTION_PGI indicates which election the donation was made for:
contribs$TRANSACTION_PGI[1:40] # We will mainly be concerned with the Presidential (P),
# Gubernatorial (G), and Congressional (C)

# CMTE_NM, CMTE_DSGN, CMTE_TP = Committee Name, Designation, and Type

levels(contribs$PARTY)
# DEM = Democratic, REP = Republican, DFL = Democratic-Farmer-Labor, GRE = Green
# IND = Independent, NNE = None, UNK = Unknown
# UNK contributions are to Women's Campaign Party = non-partisan


##### Second data set: the unique Harvard Univeristy employee contributors,
# with additional employment information for 2011-2014 donations
head(people)


##### Third data set: the union of the first two data sets, for years 2011-2014
head(contribs.tagged)
#################################################


#################################################
##### Checking off required elements: the dataset

# 1.  A dataframe
class(people); class(contribs); class(contribs.tagged) 
# We have three! 

# 2.  At least two categorical or logical columns
head(contribs.tagged) 
# Look at all those categorical columns!  TITLE, SCHOOL, etc 

# 3.  At least two numeric columns
head(contribs.tagged) 
# We have transaction date and transaction amounts for numerical columns

# 4.  At least 20 rows, preferably more, but real-world data may be limited
nrow(people); nrow(contribs); nrow(contribs.tagged)

#################################################


#################################################
##### Modifications of the data sets for easier analysis

# Let's start by modifying the 2001-2014 contribution data, 'contribs' 
# to better suit our analysis

# Read TRANSACTION_DT as Date objects
contribs$TRANSACTION_DT <- as.Date(contribs$TRANSACTION_DT); head(contribs$TRANSACTION_DT)
# Add a column for month-year of transaction as well
contribs$YEARMON <- as.yearmon(contribs$TRANSACTION_DT); head(contribs$YEARMON)

# Create a column where we lump some of the party affiliations together
contribs$PARTY2<-contribs$PARTY
levels(contribs$PARTY2)
levels(contribs$PARTY2) <- c("DEM", "DEM", "GRE", "IND", "NA", "REP", "NA", "NA")
levels(contribs$PARTY2)


# Now modify the second data set, 'people'
# Merge schools with less than 10 contributors into OTHER (except HMC)
levels(people$SCHOOL) <- c("CADM","FAS","OTHER","GSE","OTHER","HBS","HKS","HLS","HMC","HMS","OTHER","OTHER","OTHER","SEAS","SPH","OTHER")


# Now modify the third data set, 'contribs.tagged'
# Add level merges for SCHOOL from people data frame
levels(contribs.tagged$SCHOOL) <- c("CADM","FAS","OTHER","GSE","OTHER","HBS","HKS","HLS","HMC","HMS","OTHER","OTHER","OTHER","SEAS","SPH","OTHER")
# Add a column with formatted transaction date
contribs.tagged$TRANSACTION_DT <- as.character(contribs.tagged$TRANSACTION_DT); class(contribs.tagged$TRANSACTION_DT)
contribs.tagged$TRANSACTION_DT <- as.Date(contribs.tagged$TRANSACTION_DT,format="%m%d%Y"); head(contribs.tagged$TRANSACTION_DT)
# Add a column for month-year just like in the 2001-2014 data
contribs.tagged$YEARMON <- as.yearmon(contribs.tagged$TRANSACTION_DT); head(contribs.tagged$YEARMON)

#################################################


#################################################
##### Initial Exploration of data sets






#################################################


#################################################
#### Linear regression 

# Let's take a look and see if we can fit a linear regression model to
# some of the time intervals over which Harvard Univeristy employees
# have donated

# For instance, and for a sanity check really, let's take the example of
# our most recent presidential elections: 
# President Barack Obama was elected for a first term in November, 2008
# Let's look at the donations leading up to and after his election

# Do this first for the sum and median and count of all donations / month
party.sum <- as.data.frame(as.list(aggregate(TRANSACTION_AMT ~ PARTY + YEARMON, data = contribs, sum))); 
names(party.sum)<-c("PARTY", "DATE", "DONATION"); head(party.sum)
party.med <- as.data.frame(as.list(aggregate(TRANSACTION_AMT ~ PARTY + YEARMON, data = contribs, median)));
names(party.med)<-c("PARTY", "DATE", "DONATION"); head(party.med)


# Let's see what they look like plotted

plot(party.sum$DATE, party.sum$DONATION)
plot(party.sum[party.sum$PARTY =="REP",2], party.sum[party.sum$PARTY =="REP",3]) # Just the Republican
plot(party.sum[party.sum$PARTY =="DEM",2], party.sum[party.sum$PARTY =="DEM",3]) # Just the Democratic

plot(party.med$DATE, party.med$DONATION)
plot(party.med[party.med$PARTY =="REP",2], party.med[party.med$PARTY =="REP",3]) # Just the Republican
plot(party.med[party.med$PARTY =="DEM",2], party.med[party.med$PARTY =="DEM",3]) # Just the Democratic


# Now separate the democratic donations
party.sum.dem<-data.frame(subset(party.sum,subset = party.sum$PARTY == "DEM")); head(party.sum.dem) # Democratic donations
# Zoom in on Jan 2005 - Oct 2008
plot(party.sum.dem[130:139,3],party.sum.dem[130:139,4])



##################################################################################################
# PREVIOUS ANALYSIS, not yet added above
##################################################################################################


# breakdown sum/median of donations by party by month
d <- as.data.frame(as.list(aggregate(TRANSACTION_AMT ~ PARTY + YEARMON, data = contribs, sum)))
m1 <- acast(d, PARTY~YEARMON, value.var='TRANSACTION_AMT', fill=0)
# three-month rolling mean
m1.rm <- t(m1); m1.rm[,1:5] <- rbind(rbind(rep(0, 5), rollmean(t(m1), 3)), rep(0, 5)); m1.rm <- t(m1.rm)
barplot(m1, col=c("blue", "green", "yellow", "grey", "red"), main="Harvard Political Contributions, 2011-2014:\n Total Contributions, by Month",
        xlab="Date", ylab="Amount (USD)", xpd=FALSE, border=TRUE)
legend('topright', legend=row.names(m1), fill=c("blue", "green", "yellow", "grey", "red"))
# party
parties <- aggregate(TRANSACTION_AMT ~ PARTY, data=contribs, sum)
parties <- parties[order(-parties$TRANSACTION_AMT),]; parties
# top donation recipients
top_recipients <- aggregate(TRANSACTION_AMT ~ CMTE_NM + PARTY, data=contribs, sum)
top_recipients <- top_recipients[order(-top_recipients$TRANSACTION_AMT),]
head(top_recipients, n = 20)
# top donors
top_donors <- aggregate(TRANSACTION_AMT ~ NAME, data=contribs, sum)
top_donors <- top_donors[order(-top_donors$TRANSACTION_AMT),]
top_donors.party <- aggregate(TRANSACTION_AMT ~ PARTY + NAME, data=contribs, sum)
top_donors.party <- top_donors.party[order(-top_donors.party$TRANSACTION_AMT),]
head(top_donors, n = 20); head(top_donors.party, n = 20)
summary(top_donors$TRANSACTION_AMT)
table(head(top_donors.party$PARTY, n = 100))


# PEOPLE
# Unique employees who contributed between 2011 and 2014
nrow(people) # 913 unique verified contributors between 2011 and 2014
# Gender
# 358 F, 555 M
# 39.2% F, 60.8% M
table(people$GENDER)
round(prop.table(table(people$GENDER)), 3)
# School
# merge schools with < 10 contributors into OTHER (except HMC)
levels(people$SCHOOL) <- c("CADM","FAS","OTHER","GSE","OTHER","HBS","HKS","HLS","HMC",
                           "HMS","OTHER","OTHER","OTHER","SEAS","SPH","OTHER")
table(people$SCHOOL)
round(prop.table(table(people$SCHOOL)), 3)
# Title
table(people$TITLE)
round(prop.table(table(people$TITLE)), 3)
# School vs. Gender
school.gender.table <- table(people$SCHOOL, people$GENDER); school.gender.table
school.gender.perc <- round(prop.table(school.gender.table, 1), 3); school.gender.perc
# average gender distribution per school
# 41% female, 59% male
school.gender.favg <- mean(school.gender.perc[,1])
school.gender.mavg <- mean(school.gender.perc[,2])
school.gender.favg; school.gender.mavg
# Gender vs. Title
title.gender.table <- table(people$TITLE, people$GENDER); title.gender.table
title.gender.perc <- round(prop.table(title.gender.table, 1), 3); title.gender.perc

# TAGGED CONTRIBUTIONS (2011 - 2014)
# Contribution data, tagged with gender/school/title, from 2011 - 2014
names(contribs.tagged)
# add level merges for SCHOOL from people data frame
levels(contribs.tagged$SCHOOL) <- c("CADM","FAS","OTHER","GSE","OTHER","HBS","HKS","HLS","HMC",
                                    "HMS","OTHER","OTHER","OTHER","SEAS","SPH","OTHER")
# Add a column with formatted transaction date
contribs.tagged$TRANSACTION_DT <- as.Date(sprintf("%08d", contribs.tagged$TRANSACTION_DT), '%m%d%Y')
# contributions between 01-04-2011 and 10-15-2014
summary(contribs.tagged$TRANSACTION_DT)
# only 47 contributions greater than $5000
hist(contribs.tagged$TRANSACTION_AMT, breaks="FD", freq=TRUE)
# let's break it down by school
# first, merge CAND and CMTE party affiliation to get party
#contribs.tagged$PARTY <- ifelse(!(contribs.tagged$CMTE_PTY_AFFILIATION == ""), as.character(contribs.tagged$CMTE_PTY_AFFILIATION), as.character(contribs.tagged$CAND_PTY_AFFILIATION))
# now, look at total contributions (amt + count) per party per school
party.school.sum <- aggregate(TRANSACTION_AMT ~ PARTY + SCHOOL, data = contribs.tagged, sum); party.school.sum
party.school.freq <- aggregate(TRANSACTION_AMT ~ PARTY + SCHOOL, data = contribs.tagged, length); party.school.freq
# now per party per school
party.title.freq <- aggregate(TRANSACTION_AMT ~ PARTY + TITLE, data = contribs.tagged, length)
party.title.freq <- party.title.sum[order(party.title.freq$TRANSACTION_AMT, decreasing=TRUE),]; party.title.freq
party.title.sum <- aggregate(TRANSACTION_AMT ~ PARTY + TITLE , data = contribs.tagged, sum)
party.title.sum <- party.title.sum[order(party.title.sum$TRANSACTION_AMT, decreasing=TRUE),]; party.title.sum
party.title.mean <- aggregate(TRANSACTION_AMT ~ PARTY + TITLE , data = contribs.tagged, mean)
party.title.mean <- party.title.mean[order(party.title.mean$TRANSACTION_AMT, decreasing=TRUE),]; party.title.mean
# per party per gender
party.gender.sum <- aggregate(TRANSACTION_AMT ~ CMTE_PTY_AFFILIATION + GENDER, data = contribs.tagged, sum); party.gender.sum 
party.gender.freq <- aggregate(TRANSACTION_AMT ~ GENDER + CMTE_PTY_AFFILIATION, data = contribs.tagged, length); party.gender.freq
