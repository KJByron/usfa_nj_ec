library(dplyr)
load(here::here("RData", "a_prepped_data.RData"))


## make conflict indicator
# same or adjacent day conflicts:
#  The same events may not be run within a calendar day of another. 
#  Example: If an unrated foil tournament is run on a Friday, 
#  the same tournament may not be run on Thursday or Saturday, 
#  but may be run on Wednesday and Sunday.
# classification conflicts apply within weapon and on same or adjacent days:
#  div 2 conflict with open, div 2, div 3, e and under, and unrated
#  div 3 conflict with Open, div 2, div 3, e and Under, and unrated
#  e and under conflict with div 2 and div 3
#  unrated conflict with div 2 and div 3
#  per these directions e and under do not conflict with unrated, but
#    this needs confirmation
n_request <- nrow(work_df)
is_con <- purrr::map_lgl(
  seq_len(n_request),
  function(i_row) {
    # i_row <- 5; i_row <- 11
    not_i <- seq_len(n_request)[-i_row]
    
    # check if same or adjacent day
    index_date_trouble <- not_i[
      work_df$date[i_row] == work_df$date[not_i] - lubridate::days(1)|
        work_df$date[i_row] == work_df$date[not_i] |
        work_df$date[i_row] == work_df$date[not_i] + lubridate::days(1)
    ]
    
    # check if same weapon
    index_weapon_trouble <- not_i[work_df$weapon[i_row] == work_df$weapon[not_i]]
    
    # right now gender trouble not checked as all are both or mixed
    
    # combine troubles
    index_trouble <- intersect(index_date_trouble, index_weapon_trouble)
    if (length(index_trouble) == 0) {
      return(FALSE)
    } else {
      x_vec <- work_df$cat_split[[i_row]]
      y_vec <- work_df$cat_split[index_trouble] |>
        unlist() |>
        unique()
      x_y_df <- expand.grid(x = x_vec, y = y_vec)
      out_vec <- purrr::map_lgl(
        seq_len(nrow(x_y_df)),
        function(i_expand){
          # x <- x_vec[2]
          x <- x_y_df$x[i_expand]
          y <- x_y_df$y[i_expand]
          case_when(
            x == "open" ~ y %in% c("open", "div 2", "div 3"),
            x == "div 2" ~ y %in% c("open", "div 2", "div 3", "e and under", "unrated"),
            x == "div 3" ~ y %in% c("open", "div 2", "div 3", "e and under", "unrated"),
            x == "e and under" ~ y %in% c("div 2", "div 3", "e and under"),
            x == "unrated" ~ y %in% c("div 2", "div 3", "unrated"),
            x == "y8" ~ y == "y8",
            x == "y10" ~ y == "y10",
            x == "y12" ~ y == "y12",
            x == "y14" ~ y == "y14",
            x == "vet" ~ y == "vet",
            TRUE ~ FALSE  
          )
        }
      )
      any(out_vec)
    }
  }
)

which(is_con) + 1  # row numbers in Total sheet


work_df <- work_df |>
  mutate(
    is_conflict = is_con
  )


## save data
save(work_df, 
     file = here::here("RData", "b_conflict.RData"))
