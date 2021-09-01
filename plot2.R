plot2 <- function(){
  
  library(tidyverse)
  
  NEI <- readRDS("summarySCC_PM25.rds")
  
  # NEI year stored as int. We'll set to factors
  NEI$year <- as.factor(NEI$year)
  
  
  # Plot2.R
  
  # Baltimore City, Maryland: fips = 24510
  bc_m <- NEI %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    summarise(Total = sum(Emissions, na.rm = TRUE))
  
  
  # png device for plotting
  png(filename = "plot2.png", width = 480, height = 480)
  
  barplot(bc_m$Total, names.arg = bc_m$year,
          col = "#9ecae1",
          main = "Total PM2.5 Emissions\n in Baltimore City, Maryland",
          ylab = "Amount (tons)")
  
  dev.off() # close device
}