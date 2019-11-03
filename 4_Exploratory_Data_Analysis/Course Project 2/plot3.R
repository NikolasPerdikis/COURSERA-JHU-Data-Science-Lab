#Read the files
NEI <- readRDS("D:/Downloads/exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("D:/Downloads/exdata_data_NEI_data/Source_Classification_Code.rds")


#QUESTION 3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?

library(dplyr)
library(ggplot2)

# Variables 
# em_type_balt: dataframe to hold aggregates for Baltimore group by the 4 values of field NEI$type
# em_tot: new field to hold aggregate result (sum of all emissions)

em_type_balt <- NEI %>% subset(fips=="24510") %>% group_by(year,type) %>% summarize(em_tot = sum(Emissions))
 ggplot(em_type_balt,aes(year)) + 
 geom_bar(aes(weight=em_tot,fill=type)) + 
 scale_x_continuous(breaks=c(1999,2002,2005,2008)) + 
 xlab("Years in Research") + ylab("Emission by Source Types (Tons)") + ggtitle("Breakdown of Emissions by Type in Baltimore")

dev.copy(png,file="plot3.png")
dev.off()

