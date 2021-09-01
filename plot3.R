plot3 <- function(){
  
  library(tidyverse)
  
  NEI <- readRDS("summarySCC_PM25.rds")
  
  # NEI year stored as int. We'll set to factors
  NEI$year <- as.factor(NEI$year)
  
  # NEI type stored chr. We'll set to factors
  NEI$type <- as.factor(NEI$type)
  
  # Plot3.R
  
  # Baltimore City, Maryland: fips = 24510
  bc_m_type <- NEI %>%
    filter(fips == "24510") %>%
    group_by(type, year) %>%
    summarise(Total = sum(Emissions, na.rm = TRUE))
  
  # png device for plotting
  png(filename = "plot3.png", width = 480, height = 480)
  
  ggplot(bc_m_type, aes(x = year, y = Total, fill = type)) + 
    geom_col(position = "dodge", colour = "black") +
    ylab("Total (tons)") + xlab(NULL) +
    ggtitle("Total PM2.5 Emissions\n in Baltimore City, Maryland")
  
  
  dev.off() # close device
}