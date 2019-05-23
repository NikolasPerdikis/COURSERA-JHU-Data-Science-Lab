# arrange() the result by size_mb, in descending order.
# If you want your results printed to the console, add
# print to the end of your chain.

cran %>%
  select(ip_id, country, package, size) %>%
  mutate(size_mb = size / 2^20) %>%
  filter(size_mb <= 0.5) %>%
  arrange(desc(size_mb))
  
# Your call to arrange() goes here

#WEEK 3 Quiz

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileURL, destfile="week3q1.csv", method="curl")
w3q1 <- read.csv("week3q1.csv",header=FALSE,skip=1,sep=",")

# ACS == 3 AGS == 6 , columns 11,12
