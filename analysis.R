##
##   analysis.R: statistical analysis on political contributions of Harvard 
##               employees.
##

## Dependencies ##
install.packages("zoo")
library("zoo")
install.packages("reshape2")
library("reshape2")
install.packages("gdata")
library(gdata)

## Data ##
# all contributions, 2001 - 2014
contribs <- read.csv("harvard-contributions.csv")
names(contribs)
head(contribs)

# all unique contributors, 2011 - 2014
people <- read.csv("harvard-people.csv") 
nrow(people) # 913 unique contributors
names(people) # manually added gender, school, and title
head(people)

# all contributions, 2011 - 2014, with contributor information
contribs.tagged <- read.csv("harvard-contributions-2011-2014-tagged.csv")
names(contribs.tagged)
nrow(contribs.tagged) # 3638 contributions
head(contribs.tagged)

## Wrangling ##
# contribs
## format date column
contribs$TRANSACTION_DT <- as.Date(contribs$TRANSACTION_DT)# read col as Date obj
head(contribs$TRANSACTION_DT)
contribs$YEARMON <- as.yearmon(contribs$TRANSACTION_DT) # Add new col with month-year
head(contribs$YEARMON)
contribs$YEAR <- format(as.yearmon(contribs$YEARMON), "%Y") # Add new col with just year
# group parties
levels(contribs$PARTY) # current party levels
## DEM = Democratic, REP = Republican, DFL = Democratic-Farmer-Labor, GRE = Green
## IND = Independent, NNE = None, UNK = Unknown
## UNK contributions are to Women's Campaign Party = non-partisan
contribs[which(contribs$PARTY == "UNK"),]
## DFL is affiliated with DEM, so we merge them
## NA = not affiliated
levels(contribs$PARTY) <- c("DEM", "DEM", "GRE", "IND", "NA", "REP", "NA")
levels(contribs$PARTY)

## people ##
# group titles into more general classes in a new column
people$TITLE_TYPE <- people$TITLE
levels(people$TITLE_TYPE)
title_type <- c("ADJUNCT", "ADJUNCT", "ADMINISTRATOR", "LADDER", "OTHER", "LADDER", "STAFF", "OTHER", 
                          "VISITING", "NON-LADDER", "NON-LADDER", "STAFF", "OTHER", "OTHER", "LADDER", "LADDER", 
                          "RESEARCHER", "RESEARCHER", "RESEARCHER", "RESEARCHER", "RESEARCHER", "NON-LADDER", "RESEARCHER", "NON-LADDER",
                          "NON-LADDER", "NON-LADDER", "STAFF", "STAFF", "LADDER", "VISITING", "VISITING", "VISITING", 
                          "VISITING", "VISITING", "VISITING")
levels(people$TITLE_TYPE) <- title_type; levels(people$TITLE_TYPE)

## contribs.tagged ##
# format date column (this is a bit more work because Calc fucked up the column format)
contribs.tagged$TRANSACTION_DT <- as.character(contribs.tagged$TRANSACTION_DT)
## Append a 0 to dates with day value < 10
for (i in 1:length(contribs.tagged$TRANSACTION_DT)){
  contribs.tagged$TRANSACTION_DT[i]<-ifelse(nchar(contribs.tagged$TRANSACTION_DT[i])!=8, paste0("0",contribs.tagged$TRANSACTION_DT[i]),contribs.tagged$TRANSACTION_DT[i])
}
contribs.tagged$TRANSACTION_DT <- as.Date(contribs.tagged$TRANSACTION_DT,format="%m%d%Y"); head(contribs.tagged$TRANSACTION_DT)
## Add month-year col
contribs.tagged$YEARMON <- as.yearmon(contribs.tagged$TRANSACTION_DT); head(contribs.tagged$YEARMON)
# group parties
levels(contribs.tagged$PARTY)
levels(contribs.tagged$PARTY) <- c("DEM", "DEM", "GRE", "IND", "NA", "REP")
levels(contribs.tagged$PARTY)
# group title into general classes
contribs.tagged$TITLE_TYPE <- contribs.tagged$TITLE
levels(contribs.tagged$TITLE_TYPE) <- title_type; levels(contribs.tagged$TITLE_TYPE)

## Analysis ##
# CONTRIBS
nrow(contribs) # 9361 contributions
range(contribs$TRANSACTION_DT) # range from Feb 14 2001 to Oct 15 2014

# total amount given
sum(contribs$TRANSACTION_AMT) 
# total amount given, by party
parties <- aggregate(TRANSACTION_AMT ~ PARTY, data=contribs, sum)
parties <- parties[order(-parties$TRANSACTION_AMT),]; parties 

# breakdown total donations by party by month
d <- as.data.frame(as.list(aggregate(TRANSACTION_AMT ~ PARTY + YEARMON, data = contribs, sum)))
m1 <- acast(d, PARTY~YEARMON, value.var='TRANSACTION_AMT', fill=0)
total.party.mon <- t(m1) # for later
# three-month rolling mean
m1.rm <- t(m1); m1.rm[,1:5] <- rbind(rbind(rep(0, 5), rollmean(t(m1), 3)), rep(0, 5)); m1.rm <- t(m1.rm)
barplot(m1, col=c("blue", "green", "yellow", "grey", "red"), main="Total Contributions, by Month (2001 - 2014)\n3 Month Rolled Mean",
        xlab="Date", ylab="Amount (USD)", xpd=FALSE, border=TRUE)
legend('topright', legend=row.names(m1), fill=c("blue", "green", "yellow", "grey", "red"))

total.party.mon <- data.frame(total.party.mon)
nrow(total.party.mon)

# when did reps give more than dems?
reps.more <- which(total.party.mon[,1] < total.party.mon[,5]); length(reps.more)
total.party.mon[reps.more,]
perc.dem.mon <- total.party.mon$DEM / (total.party.mon$DEM + total.party.mon$REP); 
summary(perc.dem.mon)

# repeat the calculation by year, not month
total.party.year <- data.frame(aggregate(TRANSACTION_AMT ~ PARTY + YEAR, data = contribs, sum))
total.party.year <- data.frame(t(acast(total.party.year, PARTY~YEAR, value.var='TRANSACTION_AMT', fill=0)))
rowSums(total.party.year) # total given each year
total.party.year; summary(total.party.year$PERC_DEM) # broken down by party, with percent democrat added

summary(contribs$TRANSACTION_AMT)
summary(contribs.dem$TRANSACTION_AMT)
summary(contribs.rep$TRANSACTION_AMT)
# plot probability density for how often each contribution amount is given, by party
hist(contribs.dem$TRANSACTION_AMT, xlim=c(0, 10000), breaks="FD", freq=FALSE, col=rgb(0,0,1,.5),
     main="Probability Density, Contribution Amount",
     xlab="Contribution Amount")
hist(contribs.rep$TRANSACTION_AMT, xlim=c(0, 10000), breaks="FD", freq=FALSE, add=TRUE, col=rgb(1,0,0,.4))
legend('topright', legend=c("Republican", "Democrat"), fill=c("red", "blue"))

# party subsets
contribs.rep <- contribs[which(contribs$PARTY == "REP"),]
contribs.dem <- contribs[which(contribs$PARTY == "DEM"),]
contribs.gre <- contribs[which(contribs$PARTY == "GRE"),]
contribs.ind <- contribs[which(contribs$PARTY == "IND"),]
contribs.na <- contribs[which(contribs$PARTY == "NA"),]

# number of unique recipients in 'contributions' (could be a subset, like all REP contributions)
num_recipients <- function(contributions) length(unique(contributions$CMTE_NM))
# number of unique donors in 'contributions' 
num_donors <- function(contributions) length(unique(contributions$NAME))

# all recipients in 'contributions', in decreasing order of total amount received
group_recipients <- function(contributions) {
  tr <- aggregate(TRANSACTION_AMT ~ CMTE_NM + PARTY, data=contributions, sum)
  return(tr[order(-tr$TRANSACTION_AMT),])
}

# all donors in 'contributions', in decreasing order of total amount given
group_donors <- function(contributions){
  donors <- aggregate(TRANSACTION_AMT ~ NAME, data=contributions, sum)
  return(donors[order(-donors$TRANSACTION_AMT),])
}

# range of contribution date, by party
# dems and rep basically span the same range
range(contribs.rep$TRANSACTION_DT)
range(contribs.dem$TRANSACTION_DT)
range(contribs.na$TRANSACTION_DT) # first na contribution in Feb 2003

# number of unique recipients / donors per party
# all data - 747 recipients, 2394 unique contributors
num_recipients(contribs); num_donors(contribs)
# dems - 564 recipients, 2126 donors
num_recipients(contribs.dem); num_donors(contribs.dem)
# reps - 140 recipients, 237 donors
num_recipients(contribs.rep); num_donors(contribs.rep)
num_recipients(contribs.gre); num_donors(contribs.gre)
num_recipients(contribs.ind); num_donors(contribs.ind)
num_recipients(contribs.na); num_donors(contribs.na)

all.recipients <- group_recipients(contribs)
dem.recipients <- group_recipients(contribs.dem)
rep.recipients <- group_recipients(contribs.rep)
na.recipients <- group_recipients(contribs.na)

# all recipients that have been donated >= $10k and $100k
recipients.top.10k <- subset(all.recipients, TRANSACTION_AMT >= 10000)
recipients.top.100k <- subset(all.recipients, TRANSACTION_AMT >= 100000)

# barplot, total amount received by each recipient in 'recipients'
graph.recipients <- function(recipients, main) {
  graph <- data.frame(recipients$CMTE_NM, recipients$TRANSACTION_AMT)
  graph <- graph[rev(rownames(graph)),]
  party_cols <- rev(ifelse(recipients$PARTY == "DEM", "blue", "red"))
  names(graph) <- c("Recipient", "Amount")
  mainlab = ifelse(main == "", "Top Recipients, 2001 - 2014", main)
  mar.default <- c(5,4,4,2) + 0.1
  par(mar = mar.default + c(3, 27, 0, 0))
  bplot <- barplot(graph$Amount, 
          names.arg=graph$Recipient,
          horiz=TRUE, 
          las=1,
          main=mainlab,
          xlab="Amount Received (USD)", 
          ylab="",
          xlim=c(0, 1200000),
          col=party_cols,)
  text(x=graph$Amount + 55000, y=bplot, labels=as.character(graph$Amount), xpd=TRUE)
}

# all recipients that have received over 100k
graph.recipients(recipients.top.100k, "Top Recipients, 2001 - 2014\nGreater than 100k Received")
# top 15 dem recipients
graph.recipients(head(dem.recipients, n=15), "Top Recipients, 2001 - 2014\nDemocratic Party")
# top 15 rep recipients
graph.recipients(head(rep.recipients, n=15), "Top Recipients, 2001 - 2014\nRepublican Party")
# weave rep and dem recipients and graph top 5 from each
weave <- interleave(head(dem.recipients, n=10), head(rep.recipients, n=10))
graph.recipients(weave, "Top Recipients, 2001 - 2014\nDemocratic and Republican Party")
par(mar = c(5,4,4,2) + 0.1)

all.donors <- group_donors(contribs)
dem.donors <- group_donors(contribs.dem)
rep.donors<- group_donors(contribs.rep)

summary(all.donors$TRANSACTION_AMT)
summary(dem.donors$TRANSACTION_AMT)
summary(rep.donors$TRANSACTION_AMT)

# presidential races
romney <- contribs[grep("ROMNEY", contribs$CMTE_NM),]
obama <- contribs[grep("OBAMA", contribs$CMTE_NM),]
mccain <- contribs[grep("MCCAIN", contribs$CMTE_NM),]
unique(romney$CMTE_NM); unique(obama$CMTE_NM); unique(mccain$CMTE_NM)

date_range <- range(contribs$TRANSACTION_DT)
g <- seq(date_range[1], date_range[2], by="1 month")
blank <- data.frame(as.yearmon(g), rep(0, length(g))); names(blank) <- c("YEARMON", "TRANSACTION_AMT")
obama.d <- as.data.frame(as.list(aggregate(TRANSACTION_AMT ~ YEARMON, data = obama, sum)))
obama.d.2008 <- subset(obama.d, YEARMON < "Nov 2008")
obama.d.2012 <- subset(obama.d, YEARMON > "Nov 2008")
romney.d <- as.data.frame(as.list(aggregate(TRANSACTION_AMT ~ YEARMON, data = romney, sum)))
romney.d <- subset(romney.d, YEARMON > "Nov 2008")
mccain.d <- as.data.frame(as.list(aggregate(TRANSACTION_AMT ~ YEARMON, data = mccain, sum)))
# plot lines
plot(obama.d.2008, col=rgb(0,0,1,1),
     main="Contributions to Presidential Candidates, Total Amount by Month\nRepublican and Democratic Nominees", 
     xlab="Date", 
     ylab="Amount (USD)",
     type="o", pch=22, lty=2, xaxt="n",
     xlim=c(as.yearmon("Nov 2003"), as.yearmon("Nov 2014")))
axis(side = 1, at = obama.d$TRANSACTION_DT, labels=T)
points(obama.d.2012, type="o", col="blue", pch=22, lty=2)
points(romney.d, type="o", col="red", pch=21, lty=2)
points(mccain.d, type="o", col="red", pch=23, lty=2,)

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
contribs.tagged$TRANSACTION_DT <- as.Date(contribs.tagged$TRANSACTION_DT)
# add a column for year-month
contribs.tagged$YEARMON <- as.yearmon(contribs.tagged$TRANSACTION_DT) 
# DEM = Democratic, REP = Republican, DFL = Democratic-Farmer-Labor, GRE = Green
# IND = Independent, NNE = None, UNK = Unknown
# UNK contributions are to Women's Campaign Party = non-partisan
# contribs[which(contribs$PARTY == "UNK"),]
# DFL is affiliated with DEM, so we merge them
# NA = not affiliated
contribs.tagged$PARTY <- contribs.tagged$CMTE_PTY_AFFILIATION
levels(contribs.tagged$PARTY)
levels(contribs.tagged$PARTY) <- c("DEM", "DEM", "GRE", "IND", "NA", "REP")
levels(contribs.tagged$PARTY)
# merge schools
levels(contribs.tagged$SCHOOL)
levels(contribs.tagged$SCHOOL) <- c("CADM", "FAS", "GSD", "GSE", "OTHER", "HBS", "HKS", "HLS", "HMC", "HMS", "OTHER", "OTHER", "OTHER", "SEAS", "SPH", "OTHER")
levels(contribs.tagged$SCHOOL)

d<- aggregate(TRANSACTION_AMT ~ PARTY + SCHOOL, data = contribs.tagged, sum); d
m1 <- acast(d, PARTY~SCHOOL, value.var='TRANSACTION_AMT', fill=0)
m1 <- m1[c(0,1,5),]
barplot(m1, col=c("blue", "red"), legend=rownames(m1), xlab="School", ylab="Amount (USD)", 
        main="Harvard Political Contributions, 2011 - 2014 \n Total Contributions, By School", xpd=FALSE, border=TRUE, beside=TRUE)

d<- aggregate(TRANSACTION_AMT ~ PARTY + GENDER, data = contribs.tagged, sum); d
m1 <- acast(d, PARTY~GENDER, value.var='TRANSACTION_AMT', fill=0)
barplot(m1, col=c("blue", "green", "yellow", "grey", "red"), legend=rownames(m1), xlab="GENDER", ylab="Amount (USD)", xpd=FALSE, border=TRUE, beside=TRUE)

d<- aggregate(TRANSACTION_AMT ~ PARTY + TITLE, data = contribs.tagged, sum);
m1 <- acast(d, PARTY~TITLE, value.var='TRANSACTION_AMT', fill=0)
m2 <- m1[,c( 3, 10, 11, 15, 16, 21, 24, 29)]
m2 <- m2[c(0,1,5),]
barplot(m2, cex.names=.7, col=c("blue", "red"), legend=rownames(m2), xlab="Title", ylab="Amount (USD)", 
        main="Harvard Political Contributions, 2011 - 2014 \n Total Contributions, By Position", xpd=FALSE, border=TRUE, beside=TRUE)


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
