library(dplyr)
library(openxlsx)
load(here::here("RData", "orig.RData"))
load(here::here("RData", "b_conflict.RData"))

format_df <- cbind(
  orig_df |> mutate(Date = work_df$date),
  is_conflict = work_df$is_conflict
)

list_format <- c(
  list(all = format_df),
  split(format_df, work_df$weapon)
)

# vote columns present?, ind weapons present?
# may just need to add conflict formatting to preserve vote logic
# also add
# filter to each sheet
# freeze top row of each sheet
# freeze left two columns of each sheet
