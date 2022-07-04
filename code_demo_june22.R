# Setting up R and R Studi0
# Visit these links:
# Go to: r-project.org to download R (base R language)
# https://www.rstudio.com/products/rstudio/download/ to download RStudio Desktop

# Panes: source, console, environment, files|plots
# evaluating code # Run icon
# Mac = command and return | Windows = control and enter

# Projects - everything should be in a project

# comment your code - unless you have an amazing memory!

# stackoverflow, twitter, rstudio online community,

# Install package
#install.packages("tidyverse")

# Load libraries 
library(tidyverse)
library(janitor)

# set the working directory
setwd("~/Documents/hockey/smww_tutorial")

dm <- read_csv("s2022_all.csv")

# Structure of data frame
str(dm)

# We should clean the names so data is "tidy"
# snake_case vs camelCase = be consistent
# pipe command - basically means "and then"
# ?clean_names - camelCase argument
dm <- dm %>% 
  clean_names()

# Take a glimpse of the data frame
glimpse(dm)


# data acquisition
# Load in previously cleaned data from pick224.com
df <- read_csv("pick224_data_2008_2022.csv")

# Type of data frame
class(df)

# just the column names
names(df)

# extract the unique values in a column
unique(df$league)

# create a table of the values in a column
table(df$league)
table(df$pos)

# min, max, mean of a column
min(df$ev_g)
max(df$pp_g)
mean(df$pp_g)

# Class of a column
class(df$dob)

# What different leagues are there and how many observations
league_totals <- count(df, league)
league_totals

# Some common and functions to help you dig in
# Create a new tibble called OHL
# Filter function (start digging in)
ohl <- df %>% 
  filter(league == "OHL") 
ohl

# select function to pick columns you want
ohl_drafted <- ohl %>% 
  select(id, name, nhl_draft)
ohl_drafted

# drop the na rows
ohl_drafted <- ohl %>% 
  select(id, name, nhl_draft) %>% 
  drop_na(nhl_draft)
ohl_drafted


# Group_by function
# show total ev goals for all leagues
league_goals <- df %>%
  group_by(league) %>% 
  summarise(ev_g = sum(ev_g),
            .groups = "drop")

# show total ev goals by season for all leagues
league_goals <- df %>%
  group_by(league, season) %>% # add grouping variable
  summarise(ev_g = sum(ev_g),
            .groups = "drop")

# show ev goals by season for OHL
league_goals <- df %>%
  group_by(league, season) %>% # add grouping variable
  summarise(ev_g = sum(ev_g),
            .groups = "drop") %>% 
  filter(league == "OHL") # filter by league

### plot OHL goals by season
x <- league_goals$season
y <- league_goals$ev_g
plot(x, y)
plot(x, y, type="o", col="blue", pch=19, lty=1,
     ylab = "Goals", xlab = "Season",
     main = "OHL even strength goals")

# show total ev goals by season by position for OHL
league_goals <- df %>%
  group_by(league, season, pos) %>% # add grouping variable
  summarise(ev_g = sum(ev_g),
            .groups = "drop") %>% 
  filter(league == "OHL")

# show total ev and pp and sh goals by position for OHL
# create a column with mutate
league_goals <- df %>%
  group_by(league, season, pos) %>% 
  summarise(ev_g = sum(ev_g),
            pp_g = sum(pp_g),
            sh_g = sum(sh_g),
            .groups = "drop") %>% 
  filter(league == "OHL") %>% 
  mutate(total_g = ev_g + pp_g + sh_g) # mutate function

# show with league, season, total goals then everything
league_goals <- df %>%
  group_by(league, season) %>% 
  summarise(ev_g = sum(ev_g),
            pp_g = sum(pp_g),
            sh_g = sum(sh_g),
            .groups = "drop") %>% 
  filter(league == "OHL") %>% 
  mutate(total_g = ev_g + pp_g + sh_g) %>% 
  select(league, season, total_g, everything())

# show a plot
xdata <- league_goals$season
y_ev_g <- league_goals$ev_g
y_pp_g <- league_goals$pp_g
z_g <- league_goals$total_g

# plot the first curve by calling plot() function
# First curve is plotted
plot(xdata, y_ev_g, type="o", col="blue", pch="o", lty=1,
     ylab = "Goals", xlab = "Season",
     ylim=range(league_goals$sh_g, z_g))

# Add second curve to the same plot by calling points() and lines()
# Use symbol 'o' for points.
points(xdata, y_pp_g, col="red", pch="o")
lines(xdata, y_pp_g, col="red",lty=1)

# Add third curve to the same plot by calling points() and lines()
# Use symbol 'o' for points.
points(xdata, z_g, col="green", pch="o")
lines(xdata, z_g, col="green",lty=1)

# Add fourth curve to the same plot by calling points() and lines()
# Use symbol 'o' for points.
points(xdata, league_goals$sh_g, col="purple", pch="o")
lines(xdata, league_goals$sh_g, col="purple",lty=1)

# Add title
title("OHL Goals by Situation 2008-2022")

# Add a legend
legend(2018,3000,c("total_g","ev_g","pp_g","sh_g"), lwd=c(2,2,2,2),
       col=c("green","blue","red", "purple"), y.intersp=0.5)

# Curious about d-men
d_goals <- df %>%
  group_by(league, season, pos) %>% 
  summarise(ev_g = sum(ev_g),
            pp_g = sum(pp_g),
            sh_g = sum(sh_g),
            .groups = "drop") %>% 
  filter(league == "WHL",
         pos == "D") %>% 
  mutate(total_g = ev_g + pp_g + sh_g)

# plot d-men 
plot(d_goals$season, d_goals$ev_g, type="o", col="blue", pch="o", lty=1,
     ylab = "Goals", xlab = "Season",
     ylim=range(d_goals$total_g, d_goals$pp_g))

# Add second line for ppg
points(d_goals$season, d_goals$pp_g, col="red", pch="o")
lines(d_goals$season, d_goals$pp_g, col="red",lty=1)

# Add a third line for total goals
points(d_goals$season, d_goals$total_g, col="green", pch="o")
lines(d_goals$season, d_goals$total_g, col="green",lty=1)

# Add title
title("WHL Defensemen Goals by Situation 2008-2020")

# Add a legend
legend(2017,400,c("total_g","ev_g","pp_g"), lwd=c(2,2,2),
       col=c("green","blue","red"), y.intersp=.5)

# Write file to csv
write.csv(d_goals,
          file = "~/Documents/hockey/smww_tutorial/d_goals.csv",
          row.names = F)








  








