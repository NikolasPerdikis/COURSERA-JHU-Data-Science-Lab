#Read the files
NEI <- readRDS("D:/Downloads/exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("D:/Downloads/exdata_data_NEI_data/Source_Classification_Code.rds")

#QUESTION 6: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips=="06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?
# Add FIPS to each dataframe

library(dplyr)
library(ggplot2)

# Variables 
# filter_vehicles: Holds all SCC$EI.Sector values that are vehicles 
# filter_scc: Holds all SCC$SCC values that relate to vehicles
# emv_24510: The aggregate dataframe, for Balitmore City (based on fips) and only vehicle related SSC
# emv_06037: The aggregate dataframe, for LA county (based on fips) and only vehicle related SSC
# em_tot: new field to hold aggregate result (sum of all emissions)
# emv_combined: the result of rbind (merge the rows of both into one dataframe) 

filter_vehicles <- unique(SCC$EI.Sector[grep("(v|V)ehicle",SCC$EI.Sector,perl=TRUE)])
filter_scc <- SCC$SCC[SCC$EI.Sector %in% filter_vehicles]
emv_24510 <- NEI %>% subset(fips=="24510") %>% subset(SCC %in% filter_scc) %>% group_by(year) %>% summarize(em_tot = sum(Emissions))

emv_24510$FIPS <- "24510"
emv_06037 <- NEI %>% subset(fips=="06037") %>% subset(SCC %in% filter_scc) %>% group_by(year) %>% summarize(em_tot = sum(Emissions))
emv_06037$FIPS <- "06037"
emv_combined <- rbind(emv_06037,emv_24510)
ggplot(data=emv_combined,aes(x=year, y=em_tot,fill=FIPS)) + 
geom_bar(stat="identity", position=position_dodge(), colour="black") + 
scale_x_continuous(breaks=c(1999,2002,2005,2008)) + 
xlab("Years in Research") + ylab("PM2.5 Emission of vehicles (Tons)") + 
ggtitle("Emissions Comparison between Baltimore (24510) and LA County (06037)")

dev.copy(png,file="plot6.png")
dev.off()
