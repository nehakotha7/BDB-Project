library(nflverse)
library(nflreadr)
library(nflplotR)
library(ggplot2)
library(tidyverse)
library(gganimate)
library(dplyr)
library(gganimate)
library(ggfootball)

#remotes::install_github("robkravec/ggfootball")

ggfootball()
ggfootball(left_endzone = "red", right_endzone = "blue",
           field_alpha = 0.7)
ggfootball() + geom_point(data = 
                            data.frame(x = c(10, 20), y = c(20, 30)),
                          aes(x = x, y = y))

library(help = "nflreadr")

games <- read.csv("games.csv")
player_play <- read.csv("player_play.csv")
players <- read.csv("players.csv")
plays <- read.csv("plays.csv")
tracking_w_1 <- read.csv("tracking_week_1.csv")
tracking_w_4 <- read.csv("tracking_week_4.csv")

# ------------------- Determine who is the player in motion after specifying gameId and playId
#(gameId == 2022100300, playId == 438) Kyle Juszczyk play - LA vs SF
# identify specific play to analyze
specific_play <- plays |> 
  filter(possessionTeam == 'SF', 
         defensiveTeam == 'LA', 
         quarter == 1, down == 1, 
         yardsToGo == 10, preSnapHomeScore==0, preSnapVisitorScore==3) 
View(specific_play)


# Function to get players in motion

get_player_in_motion <- function(playId, gameId, plays, tracking_w_4) { 
  # Filter the plays data to get the specific play 
  specific_play <- plays |>
    filter(gameId ==2022100213, playId == 344)
  
  # Extract the possession team from the specific play
  possession_team <- specific_play$possessionTeam
  
  # Merge specific play with tracking data
  merged_data <- merge(specific_play, tracking_w_4, by = c("gameId", "playId"))

  # Filter the merged data for the specific play and frameType 'SNAP'
  play_data <- merged_data |> 
    filter(frameType == "SNAP")
  
  # Identify players in motion (dis >= 0.1) from the possession team
  player_in_motion <- play_data |>  
    filter(dis >= 0.1 & club == possession_team)
  
  # Return the players in motion
  return(player_in_motion$displayName)
}
print(player_in_motion$displayName)
View(player_in_motion)

# Example usage
playId <- 344
gameId <- 2022100213
player_in_motion <- get_player_in_motion(playId, gameId, plays, tracking_w_4) 
print(player_in_motion)
     

# ------------------- motion is TRUE and RB had rush attempt
# Merge datasets by 'gameId' and 'playId'
merged_data <- player_play |> 
  inner_join(plays, by = c("gameId", "playId")) |> 
  inner_join(players, by = "nflId")  # Join with player data using 'nflId'

# Define running backs by position
running_back_positions <- c("RB", "FB")

# Filter for WR motion and RB rush attempts
filtered_data <- merged_data |> 
  filter(
    (inMotionAtBallSnap == TRUE | motionSinceLineset == TRUE) & 
      hadRushAttempt == TRUE & 
      position %in% running_back_positions  # Use 'position' from player data
  )

# Add a column to classify play outcome: Positive yardage or not
filtered_data <- filtered_data |> 
  mutate(positive_yardage = ifelse(yardsGained > 0, TRUE, FALSE))

# Summarize data: Include displayName and nflId in the grouping and output
summary_data <- filtered_data |> 
  group_by(gameId, playId, nflId, displayName) |>  # Group by player info
  summarise(
    total_yards = sum(yardsGained, na.rm = TRUE),  # Total yards gained
    positive_yardage = any(positive_yardage),  # TRUE if any play in the group had positive yardage
    hadRushAttempt = any(hadRushAttempt),  # TRUE if any rush attempt occurred in the group
    .groups = "drop"  # Avoid grouped data frames in the output
  )

positive_count <- summary_data |> 
  filter(positive_yardage == TRUE) |> 
  nrow()

negative_count <- summary_data |> 
  filter(positive_yardage == FALSE) |> 
  nrow()

# Create a data frame to store the counts
positive_negative_counts <- data.frame(
  Positive_Yardage = c("TRUE", "FALSE"),
  Count = c(positive_count, negative_count)
)

# View the data frame
View(positive_negative_counts)
# View the summarized data
View(summary_data)

#-------------------possible logistic reg model and plot (incorrect right now)

model <- glm(positive_yardage ~ inMotionAtBallSnap + motionSinceLineset, 
             data = filtered_data, family = "binomial")
summary(model)

# Generate the predicted probabilities based on the logistic model
filtered_data$predicted_prob <- predict(model, type = "response")

# Plot the predicted probabilities based on the motion variables
ggplot(filtered_data, aes(x = inMotionAtBallSnap, y = predicted_prob, color = factor(motionSinceLineset))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "black") +
  labs(title = "Predicted Probability of Positive Yardage",
       x = "In Motion at Ball Snap",
       y = "Predicted Probability",
       color = "Motion Since Lineset") +
  theme_minimal()
# ------------------- direction of motion, targeted player, and yards gained

# 1. Filter for players in motion
motion_tracking_data <- player_play |> 
  filter(inMotionAtBallSnap == TRUE | shiftSinceLineset == TRUE | motionSinceLineset == TRUE) |> 
  left_join(tracking_w_4, by = c("gameId", "playId", "nflId")) |> 
  arrange(gameId, playId, time)

# 2. Join with play data to add contextual play information
motion_tracking_data <- motion_tracking_data |> 
  left_join(plays, by = c("gameId", "playId")) |> 
  filter(down %in% c(1, 2)) %>%  # Analyze 1st and 2nd downs
  mutate(
    motionDirection = ifelse(dir >= 90 & dir <= 270, "left-to-right", "right-to-left"),
    positiveYardage = ifelse(yardsGained > 0, TRUE, FALSE)
  )

# 3. Join with game data 
motion_tracking_data <- motion_tracking_data |> 
  left_join(games, by = "gameId")

# 4. Identify if the motioned player was targeted
motion_tracking_data <- motion_tracking_data |> 
  mutate(
    motionedPlayerTargeted = ifelse(wasTargettedReceiver == 1, TRUE, FALSE),
    playResult = case_when(
      yardsGained > 0 ~ "Positive Yardage",
      yardsGained <= 0 ~ "No Gain or Loss"
    )
  )

# 5. Select relevant variables for analysis
motion_analysis <- motion_tracking_data |> 
  select(
    gameId, playId, nflId, displayName, teamAbbr, x, y, s, a, dis, 
    motionDirection, motionedPlayerTargeted, playResult, yardsGained, 
    gameClock, down, possessionTeam, defensiveTeam, passResult, 
    targetX, targetY, homeTeamAbbr, visitorTeamAbbr, gameDate
  )

# 6. Summary statistics and insights
motion_summary <- motion_analysis |> 
  group_by(motionDirection, motionedPlayerTargeted, playResult) |> 
  summarise(
    averageYards = mean(yardsGained, na.rm = TRUE),
    totalPlays = n(),
    .groups = 'drop'
  )

# Display results
View(motion_analysis)
View(motion_summary)


# -------------------
# Join player_play with tracking data on gameId, playId, arwnd nflId
motion_tracking_data <- player_play |> 
  filter(inMotionAtBallSnap == TRUE | shiftSinceLineset == TRUE | motionSinceLineset == TRUE) |> 
  left_join(tracking_w_4, by = c("gameId", "playId", "nflId")) |> 
  arrange(gameId, playId, time)

# Join with plays data to add gameClock and down, then filter for 1st and 2nd downs
motion_tracking_data <- motion_tracking_data |> 
  left_join(plays |> select(gameId, playId, down, gameClock, yardsGained), by = c("gameId", "playId")) |>  # Add `down` and `gameClock`
  filter(down %in% c(1, 2)) |>  # Filter for 1st and 2nd downs
  filter(yardsGained>0) |> 
  select(gameId, playId, nflId, time, x, y, s, a, dis, inMotionAtBallSnap, shiftSinceLineset, motionSinceLineset, gameClock, down,  yardsGained)

# Calculate distance traveled, speed, and acceleration. Check lag stuff, not sure what that does
motion_tracking_data <- motion_tracking_data |>
  group_by(gameId, playId, nflId) |>
  mutate(
    movement_type = ifelse(s > 6 & dis < 1, "Jet", 
                            ifelse(s > 6 & abs(lag(x) - x) > 1, "Fly", 
                                    ifelse(s < 4 & abs(lag(y) - y) > 0.5, "Glide", "Other")))
  ) |>
  ungroup()

# View the processed data
View(motion_tracking_data)

motion_tracking_data <- motion_tracking_data |> 
  group_by(gameId, playId, nflId) |> 
  mutate(
    movement_type = case_when(
      s > 6 & dis < 1 ~ "Jet",  # Lower threshold for 'Jet' speed
      s > 6 & abs(diff(x)) > 1 ~ "Fly",  # Adjust threshold for 'Fly' speed and distance
      s < 4 & abs(diff(y)) > 0.5 ~ "Glide",  # Adjust thresholds for 'Glide'
      TRUE ~ "Other"
    )
  ) |> 
  ungroup()
# -------------------

# data frame for different offense formations, count, and frequency 
unique_formations <- as.data.frame(table(plays$offenseFormation, useNA = "ifany"))
colnames(unique_formations) <- c("formation", "count")
total_rows <- nrow(plays)
unique_formations$frequency <- (unique_formations$count / total_rows) *100
View(unique_formations)

# data frame for different offensive routes, count, and frequency 
unique_routes_ran <- as.data.frame(table(player_play$routeRan, useNA = "ifany"))
colnames(unique_routes_ran) <- c("route", "count")
total_rows <- nrow(player_play)
unique_routes_ran$frequency <- (unique_routes_ran$count / total_rows) *100
View(unique_routes_ran)

# Filter data for specific gameId, nflId, and displayName
filtered_data <- tracking_w_1 |> 
  filter(gameId == 2022091200, nflId == 35459, displayName == "Kareem Jackson")

# View the filtered data
head(filtered_data)

# Sanders play
PHI_JAG_Game <- plays |> 
  filter(
    gameId==2022100209,
    quarter == 3,
    possessionTeam=="PHI",
    gameClock >= "03:35" & gameClock <= "03:45"  # Filter for gameClock between 3:35 and 3:45
  ) |> 
  arrange(gameClock)  

View(PHI_JAG_Game)

# filter data for club, timem, and player (only Sanders)
filtered_data_sanders <- tracking_w_4  |> 
  filter(
    gameId==2022100209,
    club == "PHI",
    grepl("2022-10-02", time),
    nflId == 47836, 
    playId==2652
  ) |> 
arrange(desc(s)) 
View(filtered_data_sanders)

# start of code for play animation --------------------------------------------
# TO DO: add the ball, fix JAG color to make it red, make the animation prettier, 
# get a better understanding of arrow orientation/make it flow smoother

# all players on the field nflId's during sandars run play-----------------
players_on_field <- filtered_sanders_run_play |> 
  select(nflId, club, jerseyNumber) |> 
  distinct()

View(players_on_field)

# list of nflId's (players) on the field during Sander's run play
nfl_Ids <- c(37266, 39950, 43352, 43368, 43787, 44902, 44926, 
                46118, 46269, 47790, 47834, 47836, 52461, 52481, 
                52553, 52608, 53439, 53462, 53466, 53494, 54492, 54758)

# filter tracking data to include specific players for this play and time range
filtered_sanders_run_play <- tracking_w_4 |> 
  filter(
    gameId == 2022100209,  
    playId == 2652, 
    nflId %in% nfl_Ids,  
    grepl("2022-10-02", time),
    !is.na(x) & !is.na(y) & !is.na(o) 
  ) 
View(filtered_sanders_run_play)

# Define team colors
team_colors <- c("PHI" = "blue", "JAG" = "red")

# Create an animation of all players' movement during the run play (3rd Q, PHI vs. JAG) ----------------
animation_all_players <- ggfootball(left_endzone = "red", right_endzone = "blue", field_alpha = 0.8) +
  
  # plot all players positions over time as points
  geom_point(data = filtered_sanders_run_play, 
             aes(x = x, y = y, fill = club), size = 4, shape = 21, color = "black", stroke = 0.5, show.legend = FALSE) + 
  # add arrows for player orientation
  geom_segment(data = filtered_sanders_run_play, 
               aes(x = x, y = y, 
                   xend = pmin(pmax(x + cos(o * pi / 180) * 5, 0), 120),  # multiplier to adjust arrow length
                   yend = pmin(pmax(y + sin(o * pi / 180) * 5, 0), 53.3),   
                   color = club),
               arrow = arrow(type = "closed", length = unit(0.05, "inches")), size = 0.2, color = "black", show.legend = FALSE) +
  # add labels for jersey numbers
  geom_text(data = filtered_sanders_run_play, 
            aes(x = x, y = y, label = jerseyNumber, color = club),
            size = 3, vjust = -1, show.legend = FALSE) +
  # set theme and limits for the plot
  coord_fixed(xlim = c(0, 120), ylim = c(0, 53.3)) + 
  scale_color_manual(values = team_colors) +  # map team colors for the borders (arrows and text)
  scale_fill_manual(values = team_colors) +   # map team colors for the fill (inside of the points)
  labs(title = "Player Movements During Run Play", 
       subtitle = "Frame: {frame_time}", 
       x = "X Position (yards)", y = "Y Position (yards)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# create the animation with transition by frameId
animation_all_players <- animation_all_players + 
  transition_time(frameId) +  # animate based on frameId
  ease_aes('linear') 

# display animation with proper Frames Per Second (FPS)
total_frames <- length(unique(filtered_sanders_run_play$frameId))
animate(animation_all_players, nframes = total_frames, fps = 25, duration = total_frames / 25)


# create an animation of Miles Sanders run play (only the runner)--------------------
animation <- ggfootball(left_endzone = "red", right_endzone = "blue", field_alpha = 0.7) +
  # Plot the player's position over time as points
  geom_point(data = filtered_data_sanders, aes(x = x, y = y, color = s), size = 4) + 
  # Add arrows for player orientation
  geom_segment(data = filtered_data_sanders, 
               aes(x = x, y = y, 
                   xend = x + cos(o * pi / 180) * 2,  # Adjust multiplier for arrow length
                   yend = y + sin(o * pi / 180) * 2),  # Adjust multiplier for arrow length
               arrow = arrow(type = "open", length = unit(0.1, "inches")), size = 0.5) +
  # Set theme and limits for the plot
  coord_fixed(xlim = c(0, 120), ylim = c(0, 53.3)) + 
  labs(title = "Player Movement: Miles Sanders", x = "X Position (yards)", y = "Y Position (yards)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create the animation
animation <- animation + 
  transition_time(frameId) +  # Animate based on frameId (assuming frames are sequential)
  ease_aes('linear')  # Smooth linear transition

# Display the animation with the correct frames per second
animate(animation, nframes = nrow(filtered_data_sanders), fps = 25)

#--------------------------

# Get unique club names
unique_clubs <- tracking_w_1 |> 
  select(club) |> 
  distinct()

# View the unique club names
print(unique_clubs)
View(unique_clubs)


# View the first few rows
head(data)

# other random EDA -------------------------------------------------
# nflreadr::load_data()

pbp <- load_participation(season = 2023, include_pbp = TRUE)
View(pbp)

pass_rush_epa <- pbp |>
  filter (pass == 1) |> 
  group_by(defteam) |> 
  summarise(plays=n(),
            avg_pass_rushers = round(mean(number_of_pass_rushers,na.rm=T),3),
            epa = mean (epa, na.rm=T))

ggplot2::ggplot(pass_rush_epa,aes(x=avg_pass_rushers, y=epa)) +
  geom_mean_lines(aes(x0=avg_pass_rushers, y0= epa)) +
  geom_nfl_logos(aes(team_abbr = defteam),width=0.055) + 
  theme_classic()+
  ggtitle("EPA/play on Average Amount of Pass Rushers",
          "2023 Season Through Week 12") +
  xlab('Average Amount of Pass Rushers') +
  ylab('EPA/play') +
  labs(caption = 'Data: nflreadr')

participation <- load_participation(seasons = 2023)
View(participation)

rosters_2023 <- load_rosters(seasons = 2023)
View(rosters_2023)

pbp_2023 <- load_pbp(seasons = 2023)
View(pbp_2023)

schedules_2023 <- load_schedules(seasons = 2023)
View(schedules_2023)

snap_counts_2023 <- load_snap_counts(seasons = 2023)
View(snap_counts)

pbp_2020 <- nflreadr::load_pbp(2020) |> 
  dplyr::filter(season_type == "REG") |>
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1))

offense <- pbp_2020  |>
  dplyr::group_by(team = posteam) |>
  dplyr::summarise(off_epa = mean(epa, na.rm = TRUE))

defense <- pbp_2020  |>
  dplyr::group_by(team = defteam) |>
  dplyr::summarise(def_epa = mean(epa, na.rm = TRUE))

combined <- offense |>
  dplyr::inner_join(defense, by = "team")

qbs <- pbp_2020  |>
  dplyr::filter(pass == 1 | rush == 1) |>
  dplyr::filter(down %in% 1:4) |>
  dplyr::group_by(id) |>
  dplyr::summarise(
    name = dplyr::first(name),
    team = dplyr::last(posteam),
    plays = dplyr::n(),
    qb_epa = mean(qb_epa, na.ram = TRUE)
  ) |>
  dplyr::filter(plays > 200) |>
  dplyr::slice_max(qb_epa, n = 10)
# ----------------
# Logos in Scatter Plots
ggplot2::ggplot(combined, aes(x = off_epa, y = def_epa)) +
  ggplot2::geom_abline(slope = -1.5, intercept = seq(0.4, -0.3, -0.1), alpha = .2) +
  nflplotR::geom_mean_lines(aes(x0 = off_epa , y0 = def_epa)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  ggplot2::labs(
    x = "Offense EPA/play",
    y = "Defense EPA/play",
    caption = "Data: @nflfastR",
    title = "2020 NFL Offensive and Defensive EPA per Play"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    plot.background = ggplot2::element_rect(fill = "#F0F0F0")
  ) +
  ggplot2::scale_y_reverse()

