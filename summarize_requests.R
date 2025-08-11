library(tidyverse)

list.files(here::here("inst", "data"))
a_df <- read.csv(
  here::here("inst", "data", "tourn_requests_2025_09_10.csv"))
a_df <- a_df[!is.na(a_df$Club.name) & a_df$Club.name != "", ]
a_df <- a_df |>
  select(Club.name:Club.Notes)

a_df <- a_df |>
  mutate(
    Weapon = replace(x = Weapon, Weapon == "EPEE", "Epee")
  )
table(a_df$Weapon)
table(a_df$Club.name)

# divvy category
x <- a_df$cat2
table(x)
# x <- gsub(pattern = "Senior ", replacement = "", x)
# x <- gsub(pattern = "unrated", replacement = "U", x, ignore.case = TRUE)
# # x <- gsub(pattern = "under", replacement = "U", x, ignore.case = TRUE)
# 
# x <- gsub("E and Under", "E & U", x)
# x <- gsub("Division", "Div", x)
# x <- gsub("II", 2, x)
# Div_3 <- "D & E & U"
# Div_2 <- paste("C &", Div_3)
x <- strsplit(x, split = ",")
table(unlist(x))


# make classification cols
a_df <- a_df |>
  mutate(
    ABCDEU = grepl("A&B&C&D&E&U", cat2),
    CDEU = grepl("C&D&E&U", cat2),
    DEU = grepl("D&E&U", cat2),
    EU = grepl("E&U", cat2),
    U = grepl("U", cat2),
    Y = grepl("Y", cat2),
    V = grepl("V", cat2)
  )
a_df |>
  select(ABCDEU:V) |>
  summarise(across(everything(), sum))
#   ABCDEU CDEU DEU EU  U  Y V
# 1     13   18  26 41 48 15 7

# make conflict indicator
# not checking for conflicts from adjacent identical events:
#  The same events may not be run within a calendar day of another. 
#  Example: If an unrated foil tournament is run on a Friday, 
#  the same tournament may not be run on Thursday or Saturday, 
#  but may be run on Wednesday and Sunday.
# focusing on classification conflicts on same day:
#  Div 2 Events conflict with Opens, Div 2, Div 3, E and Under, and Us.
#  Div 3 Events conflict with Opens, Div2, Div3, E and Under, and Us.
#  E and Under and Us conflict with Div2 and Div3 events.

# unique_date <- a_df$Date
x <- map_lgl(
  seq_len(nrow(a_df)),
  function(i_row) {
    # i_row <- 1
    not_i <- seq_len(nrow(a_df))[-i_row]
    out <- FALSE
    index_date_trouble <- which(a_df$Date[i_row] == a_df$Date[not_i])
    index_club_trouble <- which(a_df$Club.name[i_row] != a_df$Club.name[not_i])
    index_weapon_trouble <- which(a_df$Weapon[i_row] == a_df$Weapon[not_i])
    
    index_trouble <- intersect(index_date_trouble, index_club_trouble)
    index_trouble <- intersect(index_trouble, index_weapon_trouble)
    if (length(index_trouble) > 1) {
      browser()
      # only proceed if conflicting classifications
      class_i <- a_df$cat2[i_row]
      if (class_i == "CDEU") {
        a_df$cat2 %in% c()
      }
    }
    out
  }
)
a_df <- a_df |>
  mutate(
    is_conflict
  )