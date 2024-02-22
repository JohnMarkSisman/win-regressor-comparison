install.packages("devtools")
devtools::install_github("statsbomb/SDMTools")
devtools::install_github("statsbomb/StatsBombR")
library(StatsBombR)

#Load in data from PL, filter for just shots. Gives ~400 matches
#StatsBombFreeEvents_() is a custom modified version of StatsBombR::StatsBombFreeEvents() to overcome an exception
shots_SB <- FreeCompetitions() |> 
  filter(competition_id == 2) |> 
  FreeMatches() |>
  StatsBombFreeEvents_() |> 
  filter(!is.na(shot.statsbomb_xg))

#Group by match by team and summarise 
#xG_breakdown is a flat list of all the shot xGs for that team for that match
shots_grouped <- shots_SB |> 
  group_by(match_id, team.id) |> 
  summarise(shots = length(shot.statsbomb_xg),
            shots_on_target = sum(shot.outcome.id %in% c(97,100)),
            xG_total = sum(shot.statsbomb_xg),
            goals = sum(shot.outcome.name == "Goal"))

#Currently have 1 row per team per match. Reorganise to get both teams per match on
#the same row, with one team's cols suffixed .x and the other suffixed .y
slice1 <- shots_grouped |> group_by(match_id) |> slice(1)
slice2 <- shots_grouped |> group_by(match_id) |> slice(2)
df <- merge(slice1, slice2, by = "match_id")

#derive features
df <- df |> mutate(shots_diff = shots.x - shots.y,
                   shots_on_target_diff = shots_on_target.x - shots_on_target.y,
                   xG_diff = xG_total.x - xG_total.y,
                   win.x = goals.x > goals.y)

#train models
int_mod <- glm(win.x ~ 1,
               family = binomial(link = 'logit'),
               data = df)

sho_mod <- glm(win.x ~ shots_diff,
               family = binomial(link = 'logit'),
               data = df)

sot_mod <- glm(win.x ~ shots_on_target_diff,
               family = binomial(link = 'logit'),
               data = df)

xGd_mod <- glm(win.x ~ xG_diff,
               family = binomial(link = 'logit'),
               data = df)

#collate
data.frame("model" = c("intercept", "shots", "shots_on_target", "xG"),
           "aic" = c(int_mod$aic, sho_mod$aic, sot_mod$aic, xGd_mod$aic))
