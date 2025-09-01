library(dplyr)
load(here::here("RData", "b_conflicts.RData"))

# percent conflicts per club
work_df |>
  group_by(club_name) |>
  summarise(
    conflict = sum(is_conflict),
    total = n()
  ) |>
  mutate(
    conflict_percent = round(100 * conflict / total, 1)
  ) |> 
  arrange(desc(total))
