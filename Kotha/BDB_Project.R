library(nflverse)
library(nflreadr)
library(nflplotR)
library(ggplot2)
library(tidyverse)

library(help = "nflreadr")

load_

# nflreadr::load_data()

pbp <- load_participation(include_pbp = T)

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

