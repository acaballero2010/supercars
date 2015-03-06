# dplyr + ggplot2 exercise from sharp sights lab
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


# IMPORT THE DATASETS
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

Data analysis example with ggplot and dplyr (analyzing 'supercar' data, part 2)
December 23, 2014 by Sharp Sight Labs

(This post is a continuation of analyzing 'supercar' data part 1, where we create a dataset using R's dplyr package. To learn how we created our dataset, please review that post.)



Data analysis example with ggplot2 and dplyr
In part 1 of this post, I demonstrated how to create a master dataset using dplyr.

Now that we have our dataset, we'll explore it using a combination of ggplot2 and dplyr.

Load dataset

First, let's load the dataset (it's being stored on the Sharp Sight Labs website).

df.car_spec_data <- read.csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/12/auto-snout_car-specifications_COMBINED.txt"))
df.car_spec_data$year <- as.character(df.car_spec_data$year)


Create themes

Before diving into data visualization, I'm going to create some ggplot2 "themes."

Themes will be covered in-depth in a separate tutorial.

But to summarize, they are a way of combining formatting code into a single pre-made "theme" that you can apply to a plot to change its appearance. Once you create a pre-set theme, you can apply the theme to multiple charts.

This also gives you a single location to edit your chart formatting; if you have a theme applied to multiple charts, you can edit the theme itself, instead of each individual chart. This can yield large time savings if you have to change the formatting on a large number of charts.


#--------------
# Create Theme
#--------------

# BASIC THEME
theme.car_chart <- 
  theme(legend.position = "none") +
  theme(plot.title = element_text(size=26, family="Trebuchet MS", face="bold", hjust=0, color="#666666")) +
  theme(axis.title = element_text(size=18, family="Trebuchet MS", face="bold", color="#666666")) +
  theme(axis.title.y = element_text(angle=0)) 


# SCATTERPLOT THEME
theme.car_chart_SCATTER <- theme.car_chart +
  theme(axis.title.x = element_text(hjust=0, vjust=-.5))

# HISTOGRAM THEME
theme.car_chart_HIST <- theme.car_chart +
  theme(axis.title.x = element_text(hjust=0, vjust=-.5))

# SMALL MULTIPLE THEME
theme.car_chart_SMALLM <- theme.car_chart +
  theme(panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size=16, family="Trebuchet MS", face="bold", color="#666666"))    


Now that we have a few themes set up, we're going to move directly into data exploration.

Data Exploration with ggplot2 and dplyr
For our purposes here, data exploration is the application of data visualization and data manipulation techniques to understand the properties of our dataset.

We're going to be looking for interesting features: things that stand out, trends, and relationships between variables.

Note that in the following data analysis example, the data manipulation tools from dplyr and our visualization techniques from ggplot2 work hand-in-hand.

Let's start with a simple scatterplot of horsepower vs speed.


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

data-analysis-example_horsepower-vs-speed_scatterplot_ggplot2_550x450

Nothing mind-blowing here, but you can see the general relationship. More horsepower, more speed.

There is an odd feature though: see that "stripe" of points at about top_speed_mph=150? What is that?

When I started exploring this data, I noticed that feature in the data and remembered "governor systems" that limit a car's maximum speed. But, I didn't remember the exact details of those systems. After some research, I learned more about speed limiter systems on Wikipedia, but some of the details were still a little fuzzy.

Having said that, let's see if we can uncover more information in our dataset itself.

First, let's look at the speed variable alone. We'll make a histogram to show the distribution of car speeds.

#------------------------
# Histogram of Top Speed
#------------------------

ggplot(data=df.car_spec_data, aes(x=top_speed_mph)) +
  geom_histogram(fill="#880011") +  
  ggtitle("Histogram of Top Speed") +
  labs(x="Top Speed, mph", y="Count\nof Records") +
  theme.car_chart_HIST

data-analysis-example_top-speed_histogram_ggplot2_550x400

Typically, histograms allow us to see the distribution of a variable; the general shape.

Indeed, this histogram is helping us see the distribution of speeds for this dataset of high-performance cars.

But in this particular case, it's helping us investigate something specific. It reveals more information about the feature we identified above in our scatterplot: there's a large number of cars that max out at 150 to 155 miles per hour.

But still, the resolution of the chart doesn't allow us to perfectly identify the spike.

So, we'll zoom in one more time, by first subsetting our dataset to records where speed is between 149 and 159. Then we'll pipe that output into ggplot() using the %>% operator and make a bar chart.

This will allow us to identify that spike more specifically.


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


data-analysis-example_bar-chart-top-speed-subset_450x419

Ok, now you can immediately see the spike at 155 mph.

There's another question we could ask: assuming this spike is due to "speed limiters," when did they start being used?

We'll use the 'small multiples' technique (i.e., faceting) to look at different decades, side by side.

Top Speed by Decade

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
data-analysis-example_top-speed_histogram-small-multiple_ggplot2_550x475

Ok, we can quickly see that this spike begins sometime in the 90s.

We could dive deeper with a line chart to identify the exact year, but this is good enough for right now.

Let's do one more thing. I'm curious about which car companies are limiting car speeds. Let's create a quick list of car companies. We'll do that by using several dplyr verbs chained together: we'll filter the data down to cars made after 1990 with a top speed of 155, then group our data by car manufacturer (make_nm), and count the number of cars.


#-------------------------------
# TABLE OF CAR COMPANIES WITH 
#  CARS AT MAX SPEED = 155
#-------------------------------
df.car_spec_data %>%
  filter(top_speed_mph == 155 & year>=1990) %>%
  group_by(make_nm) %>% 
  summarize(count_speed_controlled = n()) %>%
  arrange(desc(count_speed_controlled))

#          make_nm          count_speed_controlled
#             BMW                     53
#            Audi                     51
#        Mercedes                     41
#          Jaguar                     14
#          Nissan                      9
#          Subaru                      7
#  Volkswagen(VW)                      7
#           Volvo                      7
#            Ford                      5
#     Mitsubishi                      5
#     Alfa-Romeo                      4
#       Infiniti                      4
#          Lexus                      4
#  Vauxhall-Opel                      4
#        Bentley                      3
#       Chrysler                      3
#        Pontiac                      3
#    Rolls-Royce                      3
#       Cadillac                      2
#       Caterham                      2
#      Chevrolet                      2
#          Mazda                      2
#        Porsche                      2
#         Toyota                      2
#             AC                      1
#          Dodge                      1
#           Fiat                      1
#         Fisker                      1
#         Holden                      1
#          Honda                      1
#           Jeep                      1
#          Lotus                      1
#             MG                      1
#        Maybach                      1
#          Noble                      1
#           Saab                      1
#           Seat                      1

At this point, we've gone from identifying a unique feature, and "drilled down" into the dataset to identify exact car companies related to that data-feature.

We've gone from an overview, found something interesting, and "zoomed in" to get richer details.

I want to you remember this. This is an important principle:
  
  Overview first, zoom and filter, then details-on-demand

We've revealed details by following a specific process: overview first, zoom and filter, then details on demand. (This visual-information seeking mantra was originally described by Ben Shneiderman.)

We started with a high-level view with a chart of horsepower vs speed. Then, we saw something that looked unusual and "took a closer look" by examining the speed variable independently. We did this by filtering our data and using new charts to "zoom in."

Finally, we gathered an initial set of details by creating list of car companies that have (probably) been using speed limiter systems. We've uncovered details about what was causing the data-feature in question.

Hypothetically, we could do more research (or ask a team member to do more research). We could read about these systems, talk to subject matter experts, etc.

To recap, we started with a high-level overview, and used filtering and different chart types to zoom in.

This is important. Learn to approach data exploration in this way.

Let's move on and explore our data with some more visualizations.

Horsepower vs Speed


#-------------------------------
# BHP by SPEED (faceted: decade)
#-------------------------------
ggplot(data=df.car_spec_data, aes(x=horsepower_bhp, y=top_speed_mph)) +
  geom_point(alpha=.6,color="#880011") +
  facet_wrap(~decade) +
  ggtitle("Horsepower vs Top Speed\nby decade") +
  labs(x="Horsepower, bhp", y="Top Speed\n mph") +
  theme.car_chart_SMALLM

data-analysis-example_speed-vs-horsepower_histogram-small-multiple_ggplot2_550x500

In the '60s and '70s, you can see increases in horsepower, but just looking at the graphs, the correlation between horsepower and top speed isn't that tight. (My suspicion is that the "slow" cars with high-BHP were heavy cars. If we wanted, we could "zoom in" on that and get more details. In the interest of time, we won't investigate right now.)

In the '80s though, the correlation between BHP and speed becomes much tighter.

Later, through the '80s and '90s, you see some mild increases in horsepower and speed, but in the 2000s, you start to see the rise of the proper 'supercar;' cars appear with horsepower well over 750 and even over 1000.

data-analysis-example_porsche-9ff_2010



Evolution of Top Speed over Time

We just investigated how horsepower and speed have evolved decade-by-decade using a small multiple chart, but I'm curious about speed specifically.

Let's get more detail by plotting the top speed of every car vs the year the car was made.


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
  ggtitle("Speed of Year's Fastest Car by Year") +
  labs(x="Year",y="Top Speed\n(fastest car)") +
  theme.car_chart_SCATTER












