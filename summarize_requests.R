library(tidyverse)

list.files(here::here("inst", "data"))
a_df <- read.csv(
  here::here("inst", "data", "tourn_requests_2025_09_10.csv"))
a_df <- a_df[!is.na(a_df$Club.name) & a_df$Club.name != "", ]
a_df <- a_df |>
  select(Club.name:Club.Notes)

a_df <- a_df |>
  mutate(
    Date =  as.POSIXct(strptime(Date, format = "%m/%d/%Y")),
    Weapon = replace(x = Weapon, Weapon == "EPEE", "Epee"),
    cat2 = gsub(pattern = "&", replacement = "", x = cat2)
  )

table(a_df$Weapon)
table(a_df$Club.name)

# divvy category
x <- a_df$cat2
table(x)

x <- strsplit(x, split = ",")
table(unlist(x))
# ABCDEU   CDEU    DEU     EU      U      V      Y 
#     13      5      8     21      7      7     15


## make conflict indicator
# same or adjacent day conflicts:
#  The same events may not be run within a calendar day of another. 
#  Example: If an unrated foil tournament is run on a Friday, 
#  the same tournament may not be run on Thursday or Saturday, 
#  but may be run on Wednesday and Sunday.
# classification conflicts apply within weapon and on same or adjacent days:
#  Div 2 Events conflict with Opens, Div2, Div3, E and Under, and Us.
#  Div 3 Events conflict with Opens, Div2, Div3, E and Under, and Us.
#  E and Under and Us conflict with Div2 and Div3 events.
n_request <- nrow(a_df)
a_df$cat_split <- strsplit(a_df$cat2, split = ",")
is_con <- map_lgl(
  seq_len(n_request),
  function(i_row) {
    # i_row <- 5; i_row <- 11
    not_i <- seq_len(n_request)[-i_row]
    
    # check if same or adjacent day
    index_date_trouble <- not_i[
      a_df$Date[i_row] == a_df$Date[not_i]  - days(1)|
        a_df$Date[i_row] == a_df$Date[not_i] |
        a_df$Date[i_row] == a_df$Date[not_i] + days(1)
    ]
    
    # check if same weapon
    index_weapon_trouble <- not_i[a_df$Weapon[i_row] == a_df$Weapon[not_i]]
    
    index_trouble <- intersect(index_date_trouble, index_weapon_trouble)
    if (length(index_trouble) == 0) {
      return(FALSE)
    } else {
      x_vec <- a_df$cat_split[[i_row]]
      y_vec <- a_df$cat_split[index_trouble] |>
        unlist() |>
        unique()
      x_y_df <- expand.grid(x = x_vec, y = y_vec)
      out_vec <- map_lgl(
        seq_len(nrow(x_y_df)),
        function(i_expand){
          # x <- x_vec[2]
          x <- x_y_df$x[i_expand]
          y <- x_y_df$y[i_expand]
          case_when(
            x == "ABCDEU" ~ y %in% c("ABCDEU", "CDEU", "DEU"),
            x == "CDEU" ~ y %in% c("ABCDEU", "CDEU", "DEU", "EU", "U"),
            x == "DEU" ~ y %in% c("ABCDEU", "CDEU", "DEU", "EU", "U"),
            x == "EU" ~ y %in% c("CDEU", "DEU", "EU"),
            x == "U" ~ y %in% c("CDEU", "DEU", "U"),
            x == "Y" ~ y == "Y",
            x == "V" ~ y == "V",
            TRUE ~ FALSE  
          )
        }
      )
      any(out_vec)
    }
  }
)

which(is_con) + 1  # row numbers in Total sheet


a_df <- a_df |>
  mutate(
    is_conflict = is_con
  )

# filter per weapon
a_df |> filter(Weapon == "Foil" & is_conflict) |>
  select(Club.name, Date, Weapon, cat2)
#    Club.name       Date Weapon      cat2
# 1       NJFA 2025-09-04   Foil       DEU
# 2   Freehold 2025-09-05   Foil         U
# 3       NJFA 2025-09-11   Foil    DEU,EU
# 4    Manchen 2025-09-12   Foil        EU
# 5   Valhalla 2025-09-12   Foil        EU
# 6       NJFA 2025-09-13   Foil   CDEU,EU
# 7    Manchen 2025-09-26   Foil         U
# 8       NJFA 2025-09-27   Foil  DEU,EU,V
# 9    Manchen 2025-10-17   Foil         U
# 10      NJFA 2025-10-18   Foil CDEU,EU,V
# 11  Freehold 2025-10-19   Foil    ABCDEU
a_df |> filter(Weapon == "Foil") |> pull(is_conflict) |> which() + 1
# [1]  2  3  4  5  6  7 10 11 16 17 18

a_df |> filter(Weapon == "Epee" & is_conflict) |>
  select(Club.name, Date, Weapon, cat2)
# Club.name       Date Weapon      cat2
# 1    Wanglei 2025-09-06   Epee  Y,ABCDEU
# 2   Valhalla 2025-09-06   Epee        EU
# 3       BCAF 2025-09-07   Epee    ABCDEU
# 4   Freehold 2025-09-07   Epee        EU
# 5   Ultimate 2025-09-14   Epee  Y,ABCDEU
# 6   Freehold 2025-09-14   Epee         Y
# 7      Medeo 2025-09-14   Epee      CDEU
# 8    Manchen 2025-09-19   Epee        EU
# 9       NJFA 2025-09-19   Epee        EU
# 10  Freehold 2025-10-03   Epee        EU
# 11      NJFA 2025-10-04   Epee CDEU,EU,V
# 12   Wanglei 2025-10-11   Epee  ABCDEU,Y
# 13      BCAF 2025-10-12   Epee    ABCDEU
# 14  Ultimate 2025-10-12   Epee  ABCDEU,Y
a_df |> filter(Weapon == "Epee") |> pull(is_conflict) |> which() + 1
# [1]  3  4  5  6  9 10 11 12 13 18 19 21 22 23

a_df |> filter(Weapon == "Sabre" & is_conflict) |>
  select(Club.name, Date, Weapon, cat2)
#      Club.name       Date Weapon       cat2
# 1 Escrimeur FC 2025-09-14  Sabre Y,ABCDEU,V
# 2         NJFA 2025-09-14  Sabre        Y,V
a_df |> filter(Weapon == "Sabre") |> pull(is_conflict) |> which() + 1
# [1] 5 6


## look at percent conflicts per club
a_df
group_by(Club.name) |>
  summarise(
    conflict = sum(is_conflict),
    total = n()
  ) |>
  mutate(
    conflict_percent = round(100 * conflict / total, 1)
  ) |> 
  arrange(desc(total))
#   Club.name    conflict total conflict (%)
# 1 Freehold            5    17         29.4
# 2 NJFA                8    14         57.1
# 3 Manchen             4     8         50  
# 4 Valhalla            5     6         83.3
# 5 Medeo               1     4         25  
# 6 BCAF                2     2        100  
# 7 Escrimeur FC        1     2         50  
# 8 Ultimate            2     2        100  
# 9 Wanglei             2     2        100 


a2_df <- a_df |> 
  group_by(Weapon) |>
  mutate(
    sheet_row = 1 + 1:n(),
    .before = 1
  ) |>
  ungroup() |> 
  select(sheet_row, Club.name, Date, Weapon, cat2, is_conflict)
a2_df |> 
  filter(Weapon == "Foil", 
         Club.name %in% c("Wanglei", "Ultimate", "Escrimeur FC", "BCAF"))
a2_df |> 
  filter(Weapon == "Epee", 
         Club.name %in% c("Wanglei", "Ultimate", "Escrimeur FC", "BCAF"))
a2_df |> 
  filter(Weapon == "Sabre", 
         Club.name %in% c("Wanglei", "Ultimate", "Escrimeur FC", "BCAF"))
