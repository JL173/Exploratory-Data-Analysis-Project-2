plot5 <- function(){
  
  library(tidyverse)
  library(ggplot2)
  
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  # NEI year stored as int. We'll set to factors
  NEI$year <- as.factor(NEI$year)
  
  # Plot5.R
  
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
  bc_m_veh <- NEI %>%
    filter(fips == "24510") %>%
    filter(SCC %in% veh_scc$SCC) %>%
    group_by(year) %>%
    summarise(Total = sum(Emissions, na.rm=TRUE))
  
  # png device for plotting
  png(filename = "plot5.png", width = 480, height = 480)
  
  ggplot(bc_m_veh, aes(x = year, y = Total, fill = "#005F6A")) +
    geom_col(position = "dodge", colour = "black") +
    scale_fill_manual(values = "#005F6A") +
    ylab("Total (tons)") + xlab(NULL) +
    ggtitle("Total PM2.5 Emissions in Baltimore City, Maryland\nfrom Motor Vehicle Sources") + 
    theme(legend.position="none")
  
  dev.off() # close device
}