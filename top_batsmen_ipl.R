library(readr)
library(tidyverse)
library(gganimate)

matches <- read_csv("ipl/matches.csv")
deliveries <- read_csv("ipl/deliveries.csv")

# rename id to match_id
names(matches)[names(matches) == 'id'] <- 'match_id'

# Join deliveried and matches 
ipl <- merge(deliveries, matches, by = 'match_id')

# Aggreate runs / season for each player
batsmens <- aggregate(ipl$batsman_runs, by=list(Category=ipl$batsman,ipl$season), FUN=sum)

# naming the columns properly
names(batsmens) <- c("batsman", "season", "runs")

# Add all time total_runs scored in IPL for each batsmen 
batsmens$total_runs <- ave(batsmens$runs, batsmens$batsman, FUN=cumsum)

# Filter top 10 batsmen acress each season
batsmen_top10 <- batsmens %>%
  group_by(season) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-total_runs)) %>%
  group_by(batsman) %>% 
  filter(rank <=10) %>%
  ungroup()

# Create plots
anim <- ggplot(batsmen_top10, aes(rank, group = batsman, 
                                  fill = as.factor(batsman), color = as.factor(batsman))) +
  geom_tile(aes(y = total_runs/2,
                height = total_runs,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(batsman, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=total_runs,label = total_runs, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="black", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="blue"),
        plot.caption =element_text(size=18, hjust=0.5, face="italic", color="red"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) +
  transition_states(season, transition_length = 50, state_length = 100, wrap=FALSE) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'IPL top scorers : {closest_state}',  
       subtitle  =  "Top 10 Batsman's",
       caption = "Total runs ") 

# Animate the plots
animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("ipl.gif")) 

