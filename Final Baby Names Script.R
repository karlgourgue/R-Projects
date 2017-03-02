# Part 1, Loading Files Into R
setwd("~/Downloads/Names")

a = seq(1880,2014)
babynames = NULL
for (i in a) {
  temp = read.csv(paste("yob", toString(i), ".txt", sep=''), header=FALSE)
  b = c(rep(i, nrow(temp)))
  d = cbind(temp,b)
  babynames = rbind(d,babynames)
}

names(babynames) = c("Name", "Gender", "Count", "Year")

#Part 2, The Popularity of "Karl"
myname = babynames[babynames$Name == "Karl" & babynames$Gender == "M",]
plot(myname$Year,myname$Count,xlab="Year",ylab="Count",main="Popularity of Karl over time")


#Part 3, Unique Girl Names Over Time
onlygirls = babynames[babynames$Gender == "F",]
onlygirlsf = data.frame(table(onlygirls$Year))
names(onlygirlsf) = c("Year","Count")
plot(onlygirlsf$Year,onlygirlsf$Count,xlab="Year",ylab="Number of Unique Names",main="Growth in Unique Names")

#Part 4, Most Popular Name / Gender + Year
toptennames <- function (year,gender) {
  ttn = babynames[babynames$Year == year & babynames$Gender == gender,]
  ttn[order("Count",decreasing = TRUE)]
  head(ttn, n=10)
}


#Part 5, Most Gender Neutral Names
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


#Part 6, Expected Age of Class
age = read.csv("classnames.csv")
pvallist = NULL
years = c(1880:2014)

agep = function (name) {
  for (num in years) {
    countsum = sum(babynames[babynames$Name == name & babynames$Year == num,]$Count)
    total = sum(babynames[babynames$Year == num,]$Count)
    prob = countsum/total
    quicknamelist = c(name)
    pvallist = lapply(quicknamelist,agep) 
  }
  return (head(pvallist))
}

#This doesn't work, I would probably loop everything into a list and plot it against years.
