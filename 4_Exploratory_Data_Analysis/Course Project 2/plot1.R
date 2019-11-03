#Read the files
NEI <- readRDS("D:/Downloads/exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("D:/Downloads/exdata_data_NEI_data/Source_Classification_Code.rds")

library(plyr)
# Variables 
# all_em: dataframe to hold aggregates per year
# em_tot: new field to hold aggregate result (sum of all emissions)

em_US <- ddply(NEI,.(year),summarize,em_tot=sum(Emissions))
bp1 <- as.matrix(t(em_US))

par(las=2)
par(mar=c(5,5,4,4))
barplot(bp1,main="US PM2.5 Emissions (1999-2008)",horiz=TRUE,names.arg=c("1999","2002","2005","2008"),
xlab="Total PM2.5 Emission (Tons) From All Sources", col=c("gray75"),cex.axis=0.75)

dev.copy(png,file="plot1.png")
dev.off()
