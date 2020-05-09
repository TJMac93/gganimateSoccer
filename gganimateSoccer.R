# Metrtica data
# https://github.com/metrica-sports/sample-data

library(ggsoccer)
library(gganimate)
library(ggplot2)
library(av)
library(dplyr)
library(lubridate)

# Read in data
# Tracking Data
awayTrack <- read.csv("AwayTracking.csv", stringsAsFactors = F, skip = 1)
homeTrack <- read.csv("HomeTracking.csv", stringsAsFactors = F, skip = 1)
events <- read.csv("Event.csv", stringsAsFactors = F)


# goal at 91.56s, passage of play starts at 85.72 - found manually in excel
# Away tracking data
oneNilA <- subset(awayTrack, Time..s. > 85.72 & Time..s. < 92.36)

# Home tracking data
oneNilH <- subset(homeTrack, Time..s. > 85.72 & Time..s. < 92.36)

# All tracking data
oneNil <- dplyr::full_join(oneNilA, oneNilH)


# drop colums for players  who are not on the pitch
# https://stackoverflow.com/a/34903938/10575353
subs <- oneNil[1,] == "NaN"                      # Returns boolean for if their coordinates are not a number
oneNil <- oneNil[, !subs, drop = FALSE]          # in oneNil, for every row, if subs is FALSE, remove


# change time to get clock - will be used in animation
oneNil$clock <- floor(oneNil$Time..s.)
oneNil$clock <- seconds_to_period(oneNil$clock)
oneNil$clock <- paste0(as.character(minute(oneNil$clock)), ":", as.character(second(oneNil$clock)))



# check event data for same time period
oneNilEvent <- subset(events, Start.Time..s. > 84 & End.Time..s. < 93)



# For the plot, we need players in a table like in this example
# https://www.rostrum.blog/2020/05/02/aguerooooo/
# Need to make columns for X, Y, Player Name, and Frame number
# Alternatively use one geom_point for each player to include in the animation
# This is what we will use


# Metrica values are between 0 and 1. We need between 0 and 100. Multiply co-ordinate values by 100 to get correct scale
oneNil[,4:49] <- oneNil[,4:49] * 100

# We can see which players we need by looking at the event
unique(oneNilEvent$From)
unique(oneNilEvent$To)
# Player Numbers 1-14 are home, 15+ are away
# Players named in event are only those involved in passage of play leading to the goal



# Frame is apparently a compuited variable so gganimate doesn't like it. Add new frame variable for transitions 
# If you get error that length of columns are different this is why.
# https://github.com/thomasp85/gganimate/issues/122
oneNil$frameCount <- c(1:length(oneNil$Period)) # - possibly not necessary, error still shows when using base animation, 
                                                # but animate() seems to work


# plot
plot <- 
  ggplot(oneNil) +
  
  annotate_pitch(
    colour = "white",                         # Pitch lines
    fill = "#7fc47f"                          # Pitch colour
  ) +
  
  theme_pitch() +                             # removes xy labels
  
  coord_cartesian(                            # crop pitch to limits, works best inside coord_cartesian rather than
    xlim = c(45, 103),                        # just using xlim and ylim, not sure why
    ylim = c(-3, 103)
  ) +
  
  geom_point(                                 # add ball location data
    aes(x = BallX, y = BallY), 
    colour = "black", fill = "white", pch = 21, size = 4
  ) +
  
  # HOME players
  # add player6 location data
  geom_point(
    aes(x = Player6X, y = Player6Y), 
    colour = "black", fill = "red", pch = 21, size = 4
  ) +
  
  # add player9 location data
  geom_point(
    aes(x = Player9X, y = Player9Y), 
    colour = "black", fill = "red", pch = 21, size = 4
  ) +
  
  # add player10 location data
  geom_point(
    aes(x = Player10X, y = Player10Y), 
    colour = "black", fill = "red", pch = 21, size = 4
  ) +
  
  # add title/subtitle/caption
  labs(
    title = "Home [1] - 0 Away", 
    subtitle = "Player9 Goal - 1'", 
    caption = "Made by @statnamara | Data source: Metrica"
  ) +
  
  # Add clock to top left
  geom_label(aes(x = 50,
                 y = 103,
                 label = clock),
             size = 7) +
  
  theme(title = element_text(face = "italic", size = 14), 
        panel.border = element_rect(colour = "black", fill=NA, size=1),
  )

# shows static plot with all frames in one
plot

plot <- plot  +
  transition_states(
    Frame,                 # variable used to change frame
    state_length = 0.01,   # duration of frame
    transition_length = 1, # duration between frames
    wrap = FALSE           # restart, don't loop animation
  )

animate(plot, 
        duration = 9, 
        fps = 30, 
        detail = 30, 
        width = 1000, 
        height = 700, 
        end_pause = 90, 
        # renderer = av_renderer()          # for save as mp4
)

# Save animation 
anim_save(filename = "goal.mp4", animation = last_animation()) # requires renderer line in animate function
anim_save(filename = "goal.gif", animation = last_animation())




# Let's add addtional players
# Player25 is Away GK so add him
# Player22 is marking Player9
# add some random players for depth


# Add away players
plot2 <- 
  ggplot(oneNil) +
  
  annotate_pitch(
    colour = "white",                         # Pitch lines
    fill = "#7fc47f"                          # Pitch colour
  ) +
  
  theme_pitch() +                             # removes xy labels
  
  coord_cartesian(                            # crop pitch to limits, works best inside coord_cartesian rather than
    xlim = c(45, 103),                        # just using xlim and ylim, not sure why
    ylim = c(-3, 103)
  ) +
  
  geom_point(                                 # add ball location data
    aes(x = BallX, y = BallY), 
    colour = "black", fill = "white", pch = 21, size = 4
  ) +
  
  # HOME players
  # add player6 location data
  geom_point(
    aes(x = Player6X, y = Player6Y), 
    colour = "black", fill = "red", pch = 21, size = 4
  ) +
  
  # add player9 location data
  geom_point(
    aes(x = Player9X, y = Player9Y), 
    colour = "black", fill = "red", pch = 21, size = 4
  ) +
  
  # add player10 location data
  geom_point(
    aes(x = Player10X, y = Player10Y), 
    colour = "black", fill = "red", pch = 21, size = 4
  ) +
  
  # add player7 location data
  geom_point(
    aes(x = Player7X, y = Player7Y), 
    colour = "black", fill = "red", pch = 21, size = 4
  ) +
  
  # add player8 location data
  geom_point(
    aes(x = Player8X, y = Player8Y), 
    colour = "black", fill = "red", pch = 21, size = 4
  ) +
  
  # AWAY players
  # add player22 location data
  geom_point(
    aes(x = Player22X, y = Player22Y), 
    colour = "white", fill = "blue", pch = 21, size = 4
  ) +
  
  # add player25 (GK) location data - give different colour shirt
  geom_point(
    aes(x = Player25X, y = Player25Y), 
    colour = "white", fill = "black", pch = 21, size = 4
  ) + 
  
  # add player16 location data
  geom_point(
    aes(x = Player16X, y = Player16Y), 
    colour = "white", fill = "blue", pch = 21, size = 4
  ) +
  
  # add player18 location data
  geom_point(
    aes(x = Player18X, y = Player18Y), 
    colour = "white", fill = "blue", pch = 21, size = 4
  ) +
  

  # add title/subtitle/caption
  labs(
    title = "Home [1] - 0 Away", 
    subtitle = "Player9 Goal - 1'", 
    caption = "Made by @statnamara | Source: Metrica"
  ) +
  
  # Add clock to top left
  geom_label(aes(x = 50,
                 y = 103,
                 label = clock),
             size = 7) +
  
  theme(title = element_text(face = "italic", size = 14), 
        panel.border = element_rect(colour = "black", fill=NA, size=1),
  ) +
  
  transition_states(
    Frame,              # time-step variable
    state_length = 0.01,   # duration of frame
    transition_length = 1, # duration between frames
    wrap = FALSE           # restart, don't loop
  )



animate(plot2, 
        duration = 10, 
        fps = 30, 
        detail = 30, 
        width = 1000, 
        height = 700, 
        end_pause = 90, 
        # renderer = av_renderer()          # for save as mp4
)


# Save animation 
anim_save(filename = "goal2.mp4", animation = last_animation()) # requires renderer line in animate function
anim_save(filename = "goal2.gif", animation = last_animation())
