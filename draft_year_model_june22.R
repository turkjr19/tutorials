library(tidyverse)
library(janitor)
library(lubridate)

# Load in previously cleaned data from pick224.com
df <- read_csv("pick224_data_2008_2022.csv") %>% 
  clean_names()

# Everyone likes Draft Year prospects ----
# How about we look at the draft year (DY season) forwards
# in the OHL and their goal data

# Filter the data for only DY year forwards in the OHL for pick224 data set
# Filter the data and select the columns we want
# What data should be included in the model?
# games played, goal data, time on ice, birth data
mydata <- df %>% 
  filter(dy == "DY",
         league %in% c("OHL", "WHL", "QMJHL"),
         pos == "F") %>%
  mutate(age_group = case_when(
    (month(dob) >=9 & day(dob) >15) | month(dob)>=10 ~ "late",
    TRUE ~ "early")) %>%
  select(league, name, season, dy, dob, age_sept_15, age_group, gp, tg, ev_g, pp_g, sh_g,
         e_toi_gp)

# Who scored the most?
# arranging a column by descending order
top10 <- mydata %>% 
  arrange(desc(tg)) %>% 
  slice(1:10)#arrange a column

# Where is McDavid?
# Maybe he didn't play as many games as Stammer or JT?
mcJesus <- mydata %>% filter(name == "Connor Mcdavid")
mcJesus

# Is goals per estimated 60 a better metric?
# how about goals per 60 (rate) - level the playing field
mydata <- mydata %>%
  mutate(eTOI = e_toi_gp*gp) %>% 
  mutate(g_p60 = round((tg/eTOI)*60, 2))
mydata

ohl <- mydata %>% filter(season == 2022,
                         league == "OHL")

top10_g_p60 <- mydata %>% 
  arrange(desc(g_p60))

# Filter out by OHL forwards drafted to the NHL
# what about goal data for players drafted to the NHL
drafted <- df %>% 
  filter(dy == "DY",
         league %in% c("OHL", "WHL", "QMJHL"),
         pos == "F") %>% 
  select(name, pick = "nhl_draft_pick") %>% 
  drop_na()
drafted

# Join drafted with goals by "name"
modelData <- mydata %>% 
  left_join(drafted,  by = "name")
modelData

# Summary of tibble
# Make sure to drop the NA's
modelData <- modelData %>% drop_na() %>% arrange(-g_p60)
summary(modelData)

# pull out what we want to look at
x <- modelData$pick
y <- modelData$g_p60

# correlation test
cor.test (x, y)

#plot scatter
plot(x,y,
     ylab = "Estimated Goals per 60", xlab = "NHL draft pick")
abline(lm(y~x), col = "red")

#  fit a regression model
model <- lm(pick ~ g_p60, data = modelData)
summary(model)

# plot
ggplot(modelData, aes(pick, g_p60)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "NHL Draft Pick",
       y = "Estimated Goals per 60")

# histogram
boxplot(modelData$g_p60)

### let's look at this year's DY players
draft02 <- df %>% 
  filter(dy == "DY",
         league %in% c("OHL", "WHL", "QMJHL"),
         pos == "F",
         season == 2022,
         gp>=10) %>%
  mutate(age_group = case_when(
    (month(dob) >=9 & day(dob) >15) | month(dob)>=10 ~ "late",
    TRUE ~ "early")) %>%
  mutate(eTOI = e_toi_gp*gp) %>% 
  mutate(g_p60 = round((tg/eTOI)*60, 2)) %>% 
  select(league, name, season, dy, dob, age_sept_15, age_group, gp, tg, ev_g, pp_g, sh_g,
         e_toi_gp, eTOI, g_p60) %>% 
  arrange(-g_p60)

# plot distribution
library(plotly)
x <- draft02$g_p60
y <- dnorm(x, mean = mean(x), sd = sd(x))

fig <- plot_ly(draft02, x = x, y = y, type = 'scatter', mode = 'markers',
               text = ~paste('Player: ', name)) %>%
  add_segments(x=mean(x), xend = mean(x), y = 0, yend = 1) %>% 
  add_segments(x=(mean(x)+sd(x)), xend = (mean(x)+sd(x)),
               y = 0, yend = 1) %>% 
  add_segments(x=(mean(x)+(2*sd(x))), xend = (mean(x)+(2*sd(x))),
               y = 0, yend = 1)

fig 


first_rounders <- modelData %>% filter(pick <=31) %>% arrange(pick)

# total points per 60
draft02_tp_p60 <- df %>% 
  filter(dy == "DY",
         league %in% c("OHL", "WHL", "QMJHL"),
         pos == "F",
         season == 2022,
         gp>=10) %>%
  mutate(age_group = case_when(
    (month(dob) >=9 & day(dob) >15) | month(dob)>=10 ~ "late",
    TRUE ~ "early")) %>%
  mutate(eTOI = e_toi_gp*gp) %>% 
  mutate(g_p60 = round((tg/eTOI)*60, 2)) %>% 
  mutate(tp_p60 = round((tp/eTOI)*60, 2)) %>% 
  select(league, name, season, dy, dob, age_sept_15, age_group, gp, tg, ev_g, pp_g, sh_g,
         e_toi_gp, eTOI, g_p60, tp_p60) %>% 
  arrange(-tp_p60)



























