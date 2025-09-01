library(dplyr)

# orig_df <- readxl::read_xlsx(
#   here::here("inst", "data", 
#              "R1 Tournament requests Sept - Oct.xlsx"), 
#   sheet = 4,
#   col_types = rep(c("text", "date", "text"), c(1, 1, 17)))
orig_df <- readxl::read_xlsx(
  here::here("inst", "data", 
             "R1 Tournament requests Sept - Oct.xlsx"), 
  sheet = 4)

work_df <- orig_df |>
  janitor::clean_names() |>
  select(club_name:club_notes)

work_df <- work_df |>
  mutate(
    date = lubridate::days(as.numeric(orig_df$Date)) +  
      as.POSIXct(strptime("1899-12-30", format = "%Y-%m-%d")),
    weapon = BDanalysis::cleanseText(weapon),
    category = BDanalysis::cleanseText(category, remove_punc = FALSE),
    gender = BDanalysis::cleanseText(gender)
  )


## date trouble
# manual till entry standardized
work_df |> filter(is.na(date))
index_trouble <- which(is.na(work_df$date))
orig_df$Date[index_trouble]
# [1] "10/11/2025"
work_df$date[index_trouble] <- as.POSIXct(strptime("10/11/2025", format = "%m/%d/%Y"))


table(work_df$weapon)
# doubt 3 weapon common need
work_df$weapon[work_df$weapon == "3 weapon"] <- c("epee", "foil", "sabre")

table(work_df$gender)
# mixed the same as all?
work_df$gender <- replace(work_df$gender, work_df$gender == "all",
                          "mixed")
table(work_df$gender)

x <- work_df$category
table(x)
x <- gsub(
  pattern = " epee| foil| sabre| 3 weapons", 
  replacement = "",
  x = x)
x <- gsub(
  pattern = "iii", 
  replacement = "3",
  x = x)
x <- gsub(
  pattern = "ii", 
  replacement = "2",
  x = x)
x <- gsub(
  pattern = "division", 
  replacement = "div",
  x = x)
table(x)

x[x == "u"] <- "unrated"
x[x == "youth (y8-y14)"] <- 
  "y8, y10, y12, y14"
x <- gsub(pattern = "y8-y14", replacement = "y8, y10, y12, y14", x = x)
x <- gsub(
  pattern = "e & u|e & under", 
  replacement = "e and under",
  x = x)
grepv("senior", x)
x <- gsub(
  pattern = "senior(?! )", 
  replacement = "open",
  x = x,
  perl = TRUE)
x <- gsub(
  pattern = "senior", 
  replacement = "",
  x = x)
table(x)

# divvy category for one last check
work_df$category <- x 
# y <- strsplit(x, split = ",|&")
# table(BDanalysis::cleanseText(unlist(y)))
work_df$cat_split <- strsplit(work_df$category, split = ",|&")
work_df$cat_split <- lapply(work_df$cat_split, BDanalysis::cleanseText)
table(unlist(work_df$cat_split))


## save data
save(orig_df, 
     file = here::here("RData", "orig.RData"))
save(work_df, 
     file = here::here("RData", "a_prepped_data.RData"))

