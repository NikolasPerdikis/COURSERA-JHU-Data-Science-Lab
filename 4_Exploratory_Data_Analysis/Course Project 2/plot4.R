#Read the files
NEI <- readRDS("D:/Downloads/exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("D:/Downloads/exdata_data_NEI_data/Source_Classification_Code.rds")

#QUESTION 4: Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

library(dplyr)

# Variables 
# filter_scc_coal: Holds all SCC$SCC values that correspond to SCC$EI.Sector containing the world "coal"
# em_coal_US: dataframe to hold aggregates for US, for all emissions related to coal
# em_tot: new field to hold aggregate result (sum of all emissions)

# Search for "coal" in field SCC$EI.Sector
filter_scc_coal <- SCC$SCC[grep("(C|c)oal",SCC$EI.Sector,perl=TRUE)]
em_coal_US <- NEI %>% subset(SCC %in% filter_scc_coal) %>% group_by(year) %>% summarize(em_tot = sum(Emissions))

par(las=2)
par(mar=c(5,5,4,4))

 bp4 <- as.matrix(t(em_coal_US))
barplot(bp4,main="US PM2.5 Emissions from Coal Sources (1999-2008)",horiz=TRUE, names.arg=c("1999","2002","2005","2008"),
ylab="Year in Research", xlab = "Coal PM2.5 Emissions(Tons)" , col=c("seagreen"),cex.axis=0.75)
dev.copy(png,file="plot4.png")
dev.off()
