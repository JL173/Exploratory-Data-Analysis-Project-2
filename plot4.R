plot4 <- function(){
  
  library(tidyverse)
  library(ggplot2)
  
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  # NEI year stored as int. We'll set to factors
  NEI$year <- as.factor(NEI$year)
  
  # Plot4.R
  
  # Need all "Combustion" "Coal" SCC codes from SCC
  # to cross-reference NEI. That is if Short.Name
  # contains BOTH "Comb" and "Coal"
  
  coal_scc <- SCC %>%
    filter(grepl("Comb.*Coal", Short.Name))%>%
    select(SCC)
  
  us_coal <- NEI %>%
    filter(SCC %in% coal_scc$SCC) %>%
    group_by(year) %>%
    summarise(Total = sum(Emissions, na.rm=TRUE)/1e3)
  
  
  # png device for plotting
  png(filename = "plot4.png", width = 480, height = 480)
  
  ggplot(us_coal, aes(x = year, y = Total)) + 
    geom_col(position = "dodge", colour = "black") +
    ylab("Total (thousand tons)") + xlab(NULL) +
    ggtitle("Total PM2.5 Emissions in United States\nfrom Coal combustion-related sources")
  
  dev.off() # close device
}