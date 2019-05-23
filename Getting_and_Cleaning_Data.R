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
agricultureLogical<-(w3q1$V11==3 & w3q1$V12==6)
#or you can rename the columns by their index
names(w3q1)[11:12] <- c("ACS","AGS") and then do the same as abovelibrary(jpeg)

fileURLJPEG="https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(fileURLJPEG,destfile="jeff.jpg",method="curl")
res <- readJPEG("jeff.jpg",native=TRUE)
class(res)
quantile(res,probs=c(0.3,0.8))

fileURLq3_1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileURLq3_1,destfile="GDP.csv", method="curl")
fileURLq3_2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileURLq3_2,destfile="EDU.csv", method="curl")
dfEDU <- read.csv("EDU.csv")
names(dfEDU)
dfGDP <- read.csv("GDP.CSV", skip=5,header=FALSE, stringsAsFactors = FALSE)
names(dfGDP)[1:2] <- c("CountryCode", "Ranking")
head(dfGDP)
# keep only Rankings 1:190
dfGDP <- dfGDP[dfGDP$Ranking>=1,]
dfGDP190 <- dfGDP[-c(191:193),]
mergeddf <- merge(dfEDU,dfGDP190,by="CountryCode")
mergeddf$Ranking <- as.numeric(as.character(mergeddf$Ranking))
arrange(mergeddf,desc(Ranking))

 nrow(z)
[1] 189
   CountryCode                                    Long.Name        Income.Group                    Region
1          TUV                                       Tuvalu Lower middle income       East Asia & Pacific
2          KIR                         Republic of Kiribati Lower middle income       East Asia & Pacific
3          MHL             Republic of the Marshall Islands Lower middle income       East Asia & Pacific
4          PLW                            Republic of Palau Upper middle income       East Asia & Pacific
5          STP Democratic Republic of Săo Tomé and Principe Lower middle income        Sub-Saharan Africa
6          FSM               Federated States of Micronesia Lower middle income       East Asia & Pacific
7          TON                             Kingdom of Tonga Lower middle income       East Asia & Pacific
8          DMA                     Commonwealth of Dominica Upper middle income Latin America & Caribbean
9          COM                         Union of the Comoros          Low income        Sub-Saharan Africa
10         WSM                                        Samoa Lower middle income       East Asia & Pacific
11         VCT               St. Vincent and the Grenadines Upper middle income Latin America & Caribbean
12         GRD                                      Grenada Upper middle income Latin America & Caribbean
13         KNA                          St. Kitts and Nevis Upper middle income Latin America & Caribbean

res <- mergeddf  %>% filter(Income.Group == "High income: OECD") %>% select (Ranking)
ave(res$Ranking)

