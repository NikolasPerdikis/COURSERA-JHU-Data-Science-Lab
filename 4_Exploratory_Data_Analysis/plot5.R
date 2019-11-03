#Read the files
NEI <- readRDS("D:/Downloads/exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("D:/Downloads/exdata_data_NEI_data/Source_Classification_Code.rds")

#QUESTION 5: How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
# Filter for Baltimore % vehicle sources

library(dplyr)


# Variables 
# filter_vehicles: Holds all SCC$EI.Sector values that are vehicles 
# filter_scc: Holds all SCC$SCC values that relate to vehicles
# emv_24510: The aggregate dataframe, for Balitmore City (based on fips) and only vehicle related SSC
# em_tot: new field to hold aggregate result (sum of all emissions)

filter_vehicles <- unique(SCC$EI.Sector[grep("(v|V)ehicle",SCC$EI.Sector,perl=TRUE)])
filter_scc <- SCC$SCC[SCC$EI.Sector %in% filter_vehicles]
emv_24510 <- NEI %>% subset(fips=="24510") %>% subset(SCC %in% filter_scc) %>% group_by(year) %>% summarize(em_tot = sum(Emissions))

bp5 <- as.vector(emv_24510$em_tot)

barplot(bp5,horiz=TRUE,names.arg=c("1999","2002","2005","2008"),
xlab="PM2.5 Emission (Tons)", main="Baltimore Motor Vehicle Emissions", 
col=c("darkolivegreen1"),cex.axis=0.75)

dev.copy(png,file="plot5.png")
dev.off()
