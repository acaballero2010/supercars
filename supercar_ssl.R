# dplyr + ggplot2 exercise from sharp sights lab
# source website: http://www.sharpsightlabs.com/data-analysis-example-r-supercars-part2/

###########
#  PART 1 #
###########
setwd("C:/Users/Miah Alexa/Documents/supercar")
library(dplyr)
library(ggplot2)

# Downloading the required dataset
# Car torque
fileUrl <- "http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_torque_DATA.txt"

if (!file.exists("data")) {
  dir.create("data")
}

download.file(fileUrl, destfile = "./data/car_torque.csv")

# Car 0 60
fileUrl <- "http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_0-60-times_DATA.txt"

if (!file.exists("data")) {
  dir.create("data")
}

download.file(fileUrl, destfile = "./data/car_0_60_time.csv")

# Car Engine Size
fileUrl <- "http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_engine-size_DATA.txt"

if (!file.exists("data")) {
  dir.create("data")
}

download.file(fileUrl, destfile = "./data/car_engine_size.csv")

# Car horsepower
fileUrl <- "http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_horsepower_DATA.txt"

if (!file.exists("data")) {
  dir.create("data")
}

download.file(fileUrl, destfile = "./data/car_horsepower.csv")

# Car Top Speed
fileUrl <- "http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_top-speed_DATA.txt"

if (!file.exists("data")) {
  dir.create("data")
}

download.file(fileUrl, destfile = "./data/car_top_speed.csv")

# Car Power to Weight
fileUrl <- "http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_power-to-weight_DATA.txt"

if (!file.exists("data")) {
  dir.create("data")
}

download.file(fileUrl, destfile = "./data/car_power_to_weight.csv")


# Loading dataset 
df.car_torque <- read.csv("data/car_torque.csv")
df.car_0_60_times  <- read.csv("data/car_0_60_time.csv")
df.car_engine_size <- read.csv("data/car_engine_size.csv")
df.car_horsepower  <- read.csv("data/car_horsepower.csv")
df.car_top_speed   <- read.csv("data/car_top_speed.csv")
df.car_power_to_weight <- read.csv("data/car_power_to_weight.csv")

# INSPECT DATA WITH head()
head(df.car_torque)
head(df.car_0_60_times)
head(df.car_engine_size)
head(df.car_horsepower)
head(df.car_top_speed)
head(df.car_power_to_weight)

# EXAMINE THE DATA
# Look for duplicater records
df.car_torque %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count != 1)
df.car_0_60_times %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count != 1)
df.car_engine_size %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count != 1)
df.car_horsepower %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count != 1)
df.car_top_speed %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count != 1)
df.car_power_to_weight %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count != 1)

# Duplicate records is TRUE
# Dedupe data
df.car_0_60_times  <- distinct(df.car_0_60_times ,car_full_nm)
df.car_engine_size <- distinct(df.car_engine_size ,car_full_nm)
df.car_horsepower  <- distinct(df.car_horsepower ,car_full_nm)
df.car_top_speed   <- distinct(df.car_top_speed ,car_full_nm)
df.car_torque      <- distinct(df.car_torque ,car_full_nm)
df.car_power_to_weight <- distinct(df.car_power_to_weight, car_full_nm)

# Join datasets together into one data frame
df.car_spec_data <- left_join(df.car_horsepower, df.car_torque, by="car_full_nm") # count after join: 1578
df.car_spec_data <- left_join(df.car_spec_data, df.car_0_60_times, by="car_full_nm") # count after join: 1578
df.car_spec_data <- left_join(df.car_spec_data, df.car_engine_size, by="car_full_nm") # count after join: 1578
df.car_spec_data <- left_join(df.car_spec_data, df.car_top_speed, by="car_full_nm") # count after join: 1578
df.car_spec_data <- left_join(df.car_spec_data, df.car_power_to_weight, by="car_full_nm") # count after join: 1578

# Test for duplicates
df.car_spec_data %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count != 1)
str(df.car_spec_data)
head(df.car_spec_data)

# Add new variables
# new var: year
df.car_spec_data <- mutate(df.car_spec_data, year=sub(".*\\[([0-9]{4})\\]", "\\1", car_full_nm))
str(df.car_spec_data$year)

# new var: decade
df.car_spec_data <- mutate(df.car_spec_data,
                           decade = as.factor(
                            ifelse(substring(df.car_spec_data$year,1,3) == '193', '1930s',
                            ifelse(substring(df.car_spec_data$year,1,3) == '194', '1940s',
                            ifelse(substring(df.car_spec_data$year,1,3) == '195', '1950s',
                            ifelse(substring(df.car_spec_data$year,1,3) == '196', '1960s',
                            ifelse(substring(df.car_spec_data$year,1,3) == '197', '1970s',
                            ifelse(substring(df.car_spec_data$year,1,3) == '198', '1980s',
                            ifelse(substring(df.car_spec_data$year,1,3) == '199', '1990s',
                            ifelse(substring(df.car_spec_data$year,1,3) == '200', '2000s',
                            ifelse(substring(df.car_spec_data$year,1,3) == '201', '2010s',"ERROR"
                            )))))))))
                            )
                           )
head(df.car_spec_data)
str(df.car_spec_data)

# new var: make_nm
#(i.e, the "make" of the car;
# the "brand name" of the car)
df.car_spec_data <- mutate(df.car_spec_data, make_nm = gsub(" .*$", "", df.car_spec_data$car_full_nm))

# new var: car_weight_tons
df.car_spec_data <- mutate(df.car_spec_data, car_weight_tons = horsepower_bhp / horsepower_per_ton_bhp)

# new var: torque_per_ton
df.car_spec_data <- mutate(df.car_spec_data, torque_per_ton = torque_lb_ft / car_weight_tons)

# Inspect the data
# Quick checks to make sure that the variables were created properly
head(df.car_spec_data)

# Frequency table (AGGREGATE)
# - decade
# Check 'decade' variable
df.car_spec_data %>%
  group_by(decade) %>%
  summarise(count=n())

# Frequency table (AGGREGATE)
# - make
df.car_spec_data %>%
  group_by(make_nm) %>%
  summarise(make_count = length(make_nm)) %>%
  arrange(desc(make_count))

###########
# PART 2  #
###########
# df.car_spec_data: 1578 obs of 16 variables
df.car_spec_data$year <- as.character(df.car_spec_data$year)

# Create ggplot2 themes
# Basic Theme
theme.car_chart <- 
  theme(legend.position = "none") +
  theme(plot.title = element_text(size=26, family="Trebuchet MS", face="bold", hjust=0, color="#666666")) +
  theme(axis.title = element_text(size=18, family="Trebuchet MS", face="bold", color="#666666")) +
  theme(axis.title.y = element_text(angle=0)) 


# Scatterplot Theme
theme.car_chart_SCATTER <- theme.car_chart +
  theme(axis.title.x = element_text(hjust=0, vjust=-.5))

# Histogram Theme
theme.car_chart_HIST <- theme.car_chart +
  theme(axis.title.x = element_text(hjust=0, vjust=-.5))

# Small Multiple Theme
theme.car_chart_SMALLM <- theme.car_chart +
  theme(panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size=16, family="Trebuchet MS", face="bold", color="#666666")) 


# Date exploration with gglot2 and dplyr
###########################################
# PLOT DATA (Preliminary Data Inspection) #
###########################################

#-------------------------
# Horsepower vs. Top Speed
#-------------------------

ggplot(data=df.car_spec_data, aes(x=horsepower_bhp, y=top_speed_mph)) +
  geom_point(alpha=.4, size=4, color="#880011") +
  ggtitle("Horsepower vs. Top Speed") +
  labs(x="Horsepower, bhp", y="Top Speed,\n mph") +
  theme.car_chart_SCATTER

#------------------------
# Histogram of Top Speed
#------------------------

ggplot(data=df.car_spec_data, aes(x=top_speed_mph)) +
  geom_histogram(fill="#880011") +  
  ggtitle("Histogram of Top Speed") +
  labs(x="Top Speed, mph", y="Count\nof Records") +
  theme.car_chart_HIST

#----------------------------------
# ZOOM IN ON SPEED CONTROLLED CARS
#
# What is the 'limited' speed?
#  (create bar chart)
#----------------------------------

df.car_spec_data %>%
  filter(top_speed_mph >149 & top_speed_mph <159) %>%
  ggplot(aes(x= as.factor(top_speed_mph))) +
  geom_bar(fill="#880011") +
  labs(x="Top Speed, mph") +
  theme.car_chart

#------------------------
# Histogram of Top Speed
#  By DECADE
#------------------------

ggplot(data=df.car_spec_data, aes(x=top_speed_mph)) +
  geom_histogram(fill="#880011") +
  ggtitle("Histogram of Top Speed\nby decade") +
  labs(x="Top Speed, mph", y="Count\nof Records") +
  facet_wrap(~decade) +
  theme.car_chart_SMALLM

#-------------------------------
# TABLE OF CAR COMPANIES WITH 
#  CARS AT MAX SPEED = 155
#-------------------------------
df.car_spec_data %>%
  filter(top_speed_mph == 155 & year>=1990) %>%
  group_by(make_nm) %>% 
  summarize(count_speed_controlled = n()) %>%
  arrange(desc(count_speed_controlled))

#-------------------------------
# BHP by SPEED (faceted: decade)
#-------------------------------
ggplot(data=df.car_spec_data, aes(x=horsepower_bhp, y=top_speed_mph)) +
  geom_point(alpha=.6,color="#880011") +
  facet_wrap(~decade) +
  ggtitle("Horsepower vs Top Speed\nby decade") +
  labs(x="Horsepower, bhp", y="Top Speed\n mph") +
  theme.car_chart_SMALLM

#-----------------------------
# Top Speed vs Year (all cars)
#-----------------------------
ggplot(data=df.car_spec_data, aes(x=year, y=df.car_spec_data$top_speed_mph)) +
  geom_point(alpha=.35, size=4.5, color="#880011", position = position_jitter()) +
  scale_x_discrete(breaks = c("1950","1960","1970","1980","1990","2000","2010")) +
  ggtitle("Car Top Speeds by Year") +
  labs(x="Year" ,y="Top Speed\nmph") +
  theme.car_chart_SCATTER

#------------------------------------------
# PLOT: Maximum Speed (fastest car) by Year
#------------------------------------------

df.car_spec_data %>%
  group_by(year) %>%
  summarize(max_speed = max(top_speed_mph, na.rm=TRUE)) %>%
  ggplot(aes(x=year,y=max_speed,group=1)) + 
  geom_point(size=5, alpha=.8, color="#880011") +
  stat_smooth(method="auto",size=1.5) +
  scale_x_discrete(breaks = c("1950","1960","1970","1980","1990","2000","2010")) +
  ggtitle("Speed of Year's \nFastest Car by Year") +
  labs(x="Year",y="Top Speed\n(fastest car)") +
  theme.car_chart_SCATTER

#----------------------
# Bar Chart
#  top 10 fastest cars
#----------------------
df.car_spec_data %>%
  select(car_full_nm,top_speed_mph) %>%
  filter(min_rank(desc(top_speed_mph)) <= 10) %>%
  arrange(desc(top_speed_mph)) %>%
  ggplot(aes(x=reorder(car_full_nm,top_speed_mph), y=top_speed_mph)) +
  geom_bar(stat="identity",fill="#880011") +
  coord_flip() +
  ggtitle("Top 10 Fastest Cars (through 2012)") +
  labs(x="",y="") +
  theme.car_chart +
  theme(axis.text.y = element_text(size=rel(1.5))) +
  theme(plot.title = element_text(hjust=1))



