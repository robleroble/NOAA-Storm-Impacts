# dplyr and ggplot2
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)


# download data
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile="noaa_data.csv.bz2")

# read data
noaa_raw_data <- read.csv("noaa_data.csv.bz2")

# first: which types of events most harmful to public health?

# arrange by mort
evtTypeMortData <- noaa_raw_data %>%
  group_by(EVTYPE) %>%
  summarize(totMort=sum(FATALITIES), totInjuries=sum(INJURIES)) %>%
  arrange(desc(totMort)) %>%
  slice_head(n=10)

evtTypeInjData <- evtTypeMortData %>%
  arrange(desc(totInjuries)) %>%
  slice_head(n=10)

# make 2 plots with top 10 killers and injurers
mortplot <- ggplot(data=evtTypeMortData, aes(x=EVTYPE, y=totMort))

mortplot + geom_bar(stat="identity")


injplot <- ggplot(data=evtTypeInjData, aes(x=EVTYPE, y=totInjuries))

injplot + 
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label=totInjuries),
            hjust = -0.1) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x="Total Injuries", y="Weather Event Type", title="Total Injuries Caused by Top 10 Weather Events") +
  expand_limits(y= c(0, 100000))
