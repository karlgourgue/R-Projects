# Part 1, Loading Files Into R
a = seq(1880,2014)
babynames = NULL
#Files are in a folder with csv files, each labeled "yob1880.txt" -> "yob2014.txt"
for (i in a) { #a loop that loads each file into a df "babynams"
  temp = read.csv(paste("yob", toString(i), ".txt", sep=''), header=FALSE)
  b = c(rep(i, nrow(temp)))
  d = cbind(temp,b)
  babynames = rbind(d,babynames)
}

names(babynames) = c("Name", "Gender", "Count", "Year")

#Part 2, The Popularity of "Karl" (my name) Over TIME
myname = babynames[babynames$Name == "Karl" & babynames$Gender == "M",]
plot(myname$Year,myname$Count,xlab="Year",ylab="Count",main="Popularity of Karl over time")


#Part 3, Plotting Number of Unique Girl Names Over Time
onlygirls = babynames[babynames$Gender == "F",]
onlygirlsf = data.frame(table(onlygirls$Year))
names(onlygirlsf) = c("Year","Count")
plot(onlygirlsf$Year,onlygirlsf$Count,xlab="Year",ylab="Number of Unique Names",main="Growth in Unique Names")

#Part 4, Finding Most Popular Name / Gender + Year
toptennames <- function (year,gender) {
  ttn = babynames[babynames$Year == year & babynames$Gender == gender,]
  ttn[order("Count",decreasing = TRUE)]
  head(ttn, n=10)
} #function takes year,gender as input and outputs top 10 names


#Part 5, Fingin Most Gender Neutral Names
#Accomplished by ranking frequency of male + female names, then putting those together to see 
#which names rank highest across both
male = babynames[babynames$Year == 2014 & babynames$Gender == "M",]
male = cbind(male, rank(-male$Count))
male = subset(male, select=c("Name", "rank(-male$Count)"))

female = babynames[babynames$Year == 2014 & babynames$Gender == "F",]
female = cbind(female, rank(-female$Count))
female = subset(female, select=c("Name", "rank(-female$Count)"))

neutral = merge(male,female)
names(neutral)[names(neutral)=="rank(-male$Count)"] <- "mrank"
names(neutral)[names(neutral)=="rank(-female$Count)"] <- "frank"

maxrank = pmax(neutral$mrank,neutral$frank)
neutral = cbind(neutral,maxrank)
head(neutral[order(maxrank),])
