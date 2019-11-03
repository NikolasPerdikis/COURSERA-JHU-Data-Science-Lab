# Example of reading fixed width files

> df <- read.fwf(
+     file=url("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"),
+     widths=c(-1, 9, -5, 4, 4, -5, 4, 4, -5, 4, 4, -5, 4, 4),
+     skip=4
+ )
> sum(df[4])
[1] 32426.7

#rename a dataframe column  
names(w4q2df[4]) 
[1] "X.2"
> class(w4q2df)
[1] "data.frame"
> names(w4q2df)[names(w4q2df)=="X.2"] <- "countryNames"

#WEEK 2
# HOW TO READ XML
fileURLXML <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
download.file(fileURLXML,"ih8.xml")
res <- xmlParse("ih8.xml")
length(xpathApply(res, "//zipcode[text()='21231']", xmlValue))


breakfast <- xmlTreeParse("breakfast.xml",useInternalNodes = TRUE
broot <- xmlRoot(breakfast)                        
names(broot)
  food   food   food   food   food 
"food" "food" "food" "food" "food" 
broot[[1]]
<food>
  <name>Belgian Waffles</name>
  <price>$5.95</price>
  <description>
   Two of our famous Belgian Waffles with plenty of real maple syrup
   </description>
  <calories>650</calories>
</food> 
> names(broot[[1]])
         name         price   description      calories 
       "name"       "price" "description"    "calories" 
y <- xpathApply(broot,"//food/name")
[> sapply(y,xmlValue)
[1] "Belgian Waffles"             "Strawberry Belgian Waffles"  "Berry-Berry Belgian Waffles"
[4] "French Toast"                "Homestyle Breakfast"

 q4 <-xmlTreeParse("w1q4.xml",useInternalNodes=TRUE)
 q4root <- xmlRoot(q4)
 names(q4root)
 names(q4root[[1]][[1]])
             name           zipcode      neighborhood   councildistrict    policedistrict        location_1 
           "name"         "zipcode"    "neighborhood" "councildistrict"  "policedistrict"      "location_1" 
y <- xpathApply(q4root,"//row/row/zipcode")
res <- sapply(y,xmlValue)
table(unlist(res))

-21226  21201  21202  21205  21206  21207  21208  21209  21210  21211  21212  21213  21214  21215  21216 
     1    136    201     27     30      4      1      8     23     41     28     31     17     54     10 
 21217  21218  21220  21222  21223  21224  21225  21226  21227  21229  21230  21231  21234  21237  21239 
    32     69      1      7     56    199     19     18      4     13    156    127      7      1      3 
 21251  21287 
     2      1  
 
 library(httr)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. To make your own application, register at
#    https://github.com/settings/developers. Use any URL for the homepage URL
#    (http://github.com is fine) and  http://localhost:1410 as the callback url
#
#    Replace your key and secret below.
myapp <- oauth_app("NotNikolas63500",
  key = "",
  secret = ""
)

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# GITHUB API
gtoken <- config(token = github_token)
req <- with_config(gtoken, GET("https://api.github.com/users/jtleek/repos"))
stop_for_status(req)
res <- content(req)
 
 lapply(res,function(ch) grep("datasharing",ch))
 res[[16]]$url
[1] "https://api.github.com/repos/jtleek/datasharing"
> res[[16]]$created_at
[1] "2013-11-07T13:25:07Z"

# TWITTTER API
myapp = oauth_app("twitter",
                   key="56kJmOPAQ0XyELShaPixq4oe1",secret="4VNFHJVK9kxODSHhsR8Wh5sLUVGZtOzIjeiVsET4bMviRQJVl3")
sig = sign_oauth1.0(myapp,
                     token = "1129684236733227008-xQ277iJSrY0KdDrgH65qAICcr81EVn",
                      token_secret = "AK6Yfe4GmTQz9HUe6Qb0b1wD4J325ccVhOt52DMO6dLYR")
homeTL = GET("https://api.twitter.com/1.1/statuses/home_timeline.json", sig)
        > 
 


students2
  grade male_1 female_1 male_2 female_2
1     A      3        4      3        4
2     B      6        4      3        5
3     C      7        4      3        8
4     D      4        0      8        1
5     E      1        1      2        7
res <- gather(students2,sex_class,count,-grade)
> res
   grade sex_class count
1      A    male_1     3
2      B    male_1     6
3      C    male_1     7
4      D    male_1     4
5      E    male_1     1
6      A  female_1     4
7      B  female_1     4
8      C  female_1     4
...

separate(data=res, col=sex_class,into = c("sex","class"))
   grade    sex class count
1      A   male     1     3
2      B   male     1     6
3      C   male     1     7
4      D   male     1     4
5      E   male     1     1
6      A female     1     4
7      B female     1     4
8      C female     1     4
...
OR
students2 %>%
  gather(sex_class ,count , -grade ) %>%
  separate( col=sex_class,into= c("sex", "class")) %>%
  print

# arrange() the result by size_mb, in descending order.
# If you want your results printed to the console, add
# print to the end of your chain.

cran %>%
  select(ip_id, country, package, size) %>%
  mutate(size_mb = size / 2^20) %>%
  filter(size_mb <= 0.5) %>%
  arrange(desc(size_mb))
  
# Your call to arrange() goes here

students3
    name    test class1 class2 class3 class4 class5
1  Sally midterm      A   <NA>      B   <NA>   <NA>
2  Sally   final      C   <NA>      C   <NA>   <NA>
3   Jeff midterm   <NA>      D   <NA>      A   <NA>
4   Jeff   final   <NA>      E   <NA>      C   <NA>
5  Roger midterm   <NA>      C   <NA>   <NA>      B
6  Roger   final   <NA>      A   <NA>   <NA>      A

students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  spread( test, grade ) %>%
  print
  name  class final midterm
1  Brian class1     B       B
2  Brian class5     C       A
3   Jeff class2     E       D
4   Jeff class4     C       A
5  Karen class3     C       C
# use readr libary, parse_number() function to extract number out of a string
> sat
# A tibble: 6 x 10
  score_range read_male read_fem read_total math_male math_fem math_total write_male write_fem write_total
  <chr>           <int>    <int>      <int>     <int>    <int>      <int>      <int>     <int>       <int>
1 700-800         40151    38898      79049     74461    46040     120501      31574     39101       70675
2 600-690        121950   126084     248034    162564   133954     296518     100963    125368      226331
3 500-590        227141   259553     486694    233141   257678     490819     202326    247239      449565
4 400-490        242554   296793     539347    204670   288696     493366     262623    302933      565556
5 300-390        113568   133473     247041     82468   131025     213493     146106    144381      290487
6 200-290         30728    29154      59882     18788    26562      45350      32500     24933       57433
sat %>%
  select(-contains("total")) %>%
  gather(key = part_sex, value = count, -score_range) %>%
  separate(col=part_sex,into=c("part","sex")) %>%
  print
# A tibble: 36 x 4
   score_range part  sex    count
   <chr>       <chr> <chr>  <int>
 1 700-800     read  male   40151
 2 600-690     read  male  121950
 3 500-590     read  male  227141
 4 400-490     read  male  242554
 5 300-390     read  male  113568
 6 200-290     read  male   30728

sat %>%
  select(-contains("total")) %>%
  gather(part_sex, count, -score_range) %>%
  separate(part_sex, c("part", "sex")) %>%
  group_by(part,sex) %>%
  mutate(total = sum(count),
         prop = count / total
  ) %>% print


# A tibble: 36 x 6
# Groups:   part, sex [6]
   score_range part  sex    count  total   prop
   <chr>       <chr> <chr>  <int>  <int>  <dbl>
 1 700-800     read  male   40151 776092 0.0517
 2 600-690     read  male  121950 776092 0.157 
 3 500-590     read  male  227141 776092 0.293 
 4 400-490     read  male  242554 776092 0.313 
 5 300-390     read  male  113568 776092 0.146 
 6 200-290     read  male   30728 776092 0.0396
 7 700-800     read  fem    38898 883955 0.0440
 8 600-690     read  fem   126084 883955 0.143 
 9 500-590     read  fem   259553 883955 0.294 
10 400-490     read  fem   296793 883955 0.336 

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

