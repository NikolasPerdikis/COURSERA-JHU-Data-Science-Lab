#Read the files
NEI <- readRDS("D:/Downloads/exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("D:/Downloads/exdata_data_NEI_data/Source_Classification_Code.rds")

# QUESTION 2: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips=="24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question
# Prepare the data: Group by fips and year

library(dplyr)

# Variables 
# em_balt: dataframe to hold aggregates for Baltimore
# em_tot: new field to hold aggregate result (sum of all emissions)

em_balt <- NEI %>% subset(fips=="24510") %>% group_by(year) %>% summarize(em_tot = sum(Emissions))

plot(em_balt,
	main="Total PM2.5 Emission for Baltimore (Tons)",
	bg="red",pch=25,cex=3,type="b",
	xlab="Years in Research: 1999,2002,2005,2008",
	ylab="Total Emissions (Tons)",xaxt="n")
axis(1,xaxp=c(1999,2008,3))

dev.copy(png,file="plot2.png")
dev.off()