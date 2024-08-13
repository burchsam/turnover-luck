library(tidyverse)
library(nflreadr)



pbp_tot = load_pbp(1999:2023)

pbp_23 |> 
  filter(season == 2023) |> 
  filter(!is.na(yards_gained), (pass == 1 |  rush == 1), (interception == 1 | fumble_forced == 1 | sack == 1)) |> 
  group_by(posteam) |> 
  summarise(avg_to = mean(epa),
            tot_to = sum(epa),
            fum_lost_rate = sum(fumble_lost == 1) / sum(fumble_forced == 1),
            ints = sum(interception),
            fumbles = sum(fumble_forced),
            sacks = sum(sack == 1),
            tos = n(),
            sacks_pct = sacks / tos,
            .groups = "drop") |> 
  arrange(tot_to)




# Offense TO
off_to = pbp_tot |> 
  filter(!is.na(yards_gained), (pass == 1 |  rush == 1), (interception == 1 | fumble_forced == 1 | sack == 1)) |> 
  group_by(posteam, season) |> 
  summarise(avg_to_epa = mean(epa),
            tot_to_epa = sum(epa),
            fum_lost_rate = sum(fumble_lost == 1) / sum(fumble_forced == 1),
            ints = sum(interception),
            fumbles = sum(fumble_forced),
            sacks = sum(sack == 1 & fumble_forced != 1),
            tos = n(),
            sacks_pct = sacks / tos,
            .groups = "drop") |> 
  arrange(tot_to_epa)

# Defense TO
def_to = pbp_tot |> 
  filter(!is.na(yards_gained), (pass == 1 |  rush == 1), (interception == 1 | fumble_forced == 1 | sack == 1)) |> 
  group_by(defteam, season) |> 
  summarise(avg_to_forced_epa = mean(epa),
            tot_to_forced_epa = sum(epa),
            fum_lost_rate_forced = sum(fumble_lost == 1) / sum(fumble_forced == 1),
            ints_forced = sum(interception),
            fumbles_forced = sum(fumble_forced),
            sacks_forced = sum(sack == 1 & fumble_forced != 1),
            tos_forced = n(),
            sacks_forced_pct = sacks_forced / tos_forced,
            .groups = "drop") |> 
  arrange(-tot_to_forced_epa)



to_df = off_to |>
  left_join(def_to, by = c("posteam" = "defteam", "season")) |> 
  rename(team = posteam) |> 
  mutate(net_tos = tos - tos_forced,
         net_to_epa_tot = tot_to_epa - tot_to_forced_epa,
         net_to_epa_avg = net_to_epa_tot / (tos + tos_forced)) |> 
  arrange(-net_to_epa_tot)


# write.csv(to_df, file = "turnover-value.csv")


ggplot(to_df, aes(x = sacks_pct, y = tot_to)) +
  labs(x = "Sacks / Total TOs",
       y = "TO EPA Lost",
       title = "NFL Offensive Turnover Luck (2023)",
       subtitle = "regular + postseason  |  turnovers defined as interception, fumble forced, or sack",
       caption = "By: Sam Burch  |  Data @nflfastR") +
  scale_x_reverse() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank()
  ) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .07, alpha = .8) +
  stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se=FALSE, color='gray') +
  nflplotR::geom_mean_lines(aes(x0 = sacks_pct, y0 = tot_to))



ggplot(to_df, aes(x = sacks_forced_pct, y = tot_to_forced)) +
  labs(x = "Sacks Forced / Total TOs",
       y = "TO EPA Gained",
       title = "NFL Defensive Turnover Luck (2023)",
       subtitle = "regular + postseason  |  turnovers defined as interception, fumble forced, or sack",
       caption = "By: Sam Burch  |  Data @nflfastR") +
  # scale_y_reverse() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank()
  ) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .07, alpha = .8) +
  stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se=FALSE, color='gray') +
  nflplotR::geom_mean_lines(aes(x0 = sacks_forced_pct, y0 = tot_to_forced))





