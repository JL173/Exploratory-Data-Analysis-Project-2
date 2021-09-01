plot6 <- function(){
  
  library(tidyverse)
  
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  # NEI year stored as int. We'll set to factors
  NEI$year <- as.factor(NEI$year)
  
  # Plot6.R
  
  # We need all "Vehicle" entries from SCC, though
  # these are split across multiple columns
  
  veh_scc <- SCC %>%
    filter(grepl("Veh",Short.Name)|
             grepl("Veh",Short.Name)|
             grepl("Veh",EI.Sector)|
             grepl("Veh",SCC.Level.One)|
             grepl("Veh",SCC.Level.Two)|
             grepl("Veh",SCC.Level.Three)|
             grepl("Veh",SCC.Level.Four)) %>%
    select(SCC)
  
  # Baltimore City, Maryland: fips = 24510
  # Los Angeles County, California fips == "06037"
  
  cities_m_veh <- NEI %>%
    filter(fips %in% c("24510", "06037")) %>%
    filter(SCC %in% veh_scc$SCC) %>%
    group_by(fips, year) %>%
    summarise(Total = sum(Emissions, na.rm=TRUE))
  
  # png device for plotting
  png(filename = "plot6.png", width = 480, height = 480)
  
  ggplot(cities_m_veh, aes(x = year, y = Total, fill = fips)) +
    scale_y_log10() +
    geom_col(position = "dodge", colour = "black") +
    ylab("Total (tons)") + xlab(NULL) +
    ggtitle("Total PM2.5 Emissions from Motor Vehicle Sources") + 
    scale_fill_discrete(name = "City",
                        labels = c("Baltimore City,\nMaryland",
                                   "Los Angeles County,\nCalifornia"))
  
  dev.off() # close device
}