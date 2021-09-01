plot1 <- function(){
  
  library(tidyverse)
  
  NEI <- readRDS("summarySCC_PM25.rds")
  
  # NEI year stored as int. We'll set to factors
  NEI$year <- as.factor(NEI$year)
  
  
  # Plot1.R
  totals <- NEI %>%
    group_by(year) %>%
    summarise(Total = sum(Emissions, na.rm=TRUE)/1e6)
  
  # png device for plotting
  png(filename = "plot1.png", width = 480, height = 480) 
  
  barplot(totals$Total, names.arg = totals$year,
          col = "#fa9fb5",
          main = "Total PM2.5 Emissions\n in United States",
          ylab = "Amount (10^6 tons)")
  
  dev.off() # close device
}