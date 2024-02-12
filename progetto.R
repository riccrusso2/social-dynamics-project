library(haven)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(vegan)
library(gplots)
library(patchwork)
library(reshape2)
library(gridExtra)
library(arules)
library(arulesViz)
library(igraph)


# Leggiamo i dati e join dei due dataframe -----------------------------------------------------
td_ita = read_dta( "C:/Users/ricca/Documents/social_dynamics/td_ita.dta" )
data4diarynew_ITA = read_dta( "C:/Users/ricca/Documents/social_dynamics/data4diarynew_ITA.dta" )
data4diarynew_ITA$userid = as.double(data4diarynew_ITA$userid)
tdita_data4diary = left_join(td_ita, data4diarynew_ITA, by = c("id"= "userid"))
View(tdita_data4diary)
# rimuoviamo le colonne--------------------------------------------------------------------------
tdita_data4diary = select(tdita_data4diary, 
                          -datein_ques, -datein_answ,-pilot.x,-YY_not,-MM_not,-tag_td,
                          -travel_fromto,-travel_medium,-sport,
                          -tag_mo,-answerduration_td, -answerduration_mo,
                          -answerduration_ev, -answerduration_sn,
                          -tag_sn, -delta_td,-delta_mo, -delta_ev,
                          -delta_sn,-tm,-what1,-what2,-where2, -c3mis, -b6mis, -c6mis,
                          -withw2,-Dep_UNITN,-motot,-tag_ev,-evtot,
                          -w1_idpilot,-w1_A09UNITN,-w1_A10UNITN, -tdtot, -sntot,token,
                          -Extraversion, -Agreeableness, -Conscientiousness, -Neuroticism, -Openness)

# troviamo gli id per la nostra analisi (chiunque abbia risposto 18gg/18gg per almeno 16 notifiche al giorno)
#-----------------------------------------------------------------------------------------------------
filter_data <- tdita_data4diary %>%
  filter(first2w == 1 & what != 99 & what != 98) %>%
  filter(date_not < as.POSIXct("2020-11-30 11:00:00", format = "%Y-%m-%d %H:%M")) %>%
  group_by(id, DD_not) %>%
  summarize(response_count = n()) %>%
  filter(all(response_count >= 16))

id_with_18_rows <- filter_data %>%
  group_by(id) %>%
  filter(n() == 18)

selected_ids2 <- unique(id_with_18_rows$id)

tdita_data4diary <- tdita_data4diary %>%
  filter(id %in% selected_ids2 & first2w == 1 & what != 99 & what != 98 &
           date_not < as.POSIXct("2020-11-30 11:00:00", format = "%Y-%m-%d %H:%M"))


#calcoliamo il nuovo timestamp
#--------------------------------------------------------------------------------------
calculate_new_hh_not <- function(hh_not) {
  
  ifelse(hh_not >= 5, hh_not - 5, hh_not + 19)
}
calculate_new_DD_not <- function(DD_not, hh_not) {
  
  ifelse(hh_not >= 5, DD_not, ifelse(DD_not==1, 30, DD_not - 1))
}
calculate_new_week <- function(week, hh_not) {
  
  ifelse(hh_not >= 5, week, ifelse(week==1, 7, week - 1))
}

tdita_data4diary <- tdita_data4diary %>%
  mutate(
    hh_not_new = calculate_new_hh_not(hh_not),
    week_new = calculate_new_week(week, hh_not),
    DD_not_new = calculate_new_DD_not(DD_not, hh_not),
  )

tdita_data4diary$date_not <- as.POSIXct(tdita_data4diary$date_not, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


#estraiamo il dataset degli snack
#---------------------------------------------------------------------------------------
snacks_df_filtered <- tdita_data4diary %>%
  filter(b6_2 == 1 | b6_3 == 1 | b6_4 == 1 | b6_5 == 1) %>%
  select(id, date_not, first2w, week, DD_not, hh_not, mm_not,
         what,week_new, c6_1, c6_2, c6_3, c6_4, c6_5, c6_6, c6_7, c6_8, 
         c6_9, c6_10, c6_11 , c6_12, c6_13, c6_14, c6_16, c6_17, 
         c6_18, c6_19, c6_20, c6_21, c6_22, c6_23, c6_24, c6_25, c6_26, b6_2, b6_3,
         b6_4, b6_5 )

b6_2 <- snacks_df_filtered %>%
  filter(b6_2 == 1) %>%
  mutate(
    original_date_not = date_not, 
    date_not= date_not - minutes(30),
    what = 101,
    week = ifelse(day(date_not) == day(original_date_not), week, ifelse(week-1 == 0, 7, week-1)),
    DD_not = day(date_not),
    hh_not = hour(date_not),
    mm_not = minute(date_not)
  )

b6_3 <- snacks_df_filtered %>%
  filter(b6_3 == 1) %>%
  mutate(
    original_date_not = date_not, 
    date_not= date_not - minutes(60),
    what = 101,
    week = ifelse(day(date_not) == day(original_date_not), week, ifelse(week-1 == 0, 7, week-1)),
    DD_not = day(date_not),  
    hh_not = hour(date_not),
    mm_not = minute(date_not)
  ) %>%
  select(id, date_not, first2w, week, DD_not, hh_not, mm_not, what, week_new,original_date_not, b6_2, b6_3,b6_4, b6_5,starts_with("c6_"))
b6_3 <- b6_3 %>%
  mutate(across(starts_with("c6_"), ~ifelse(b6_2 == 1, 99, .)))

b6_4 <- snacks_df_filtered %>%
  filter(b6_4 == 1) %>%
  mutate(
    original_date_not = date_not, 
    date_not= date_not - minutes(90),
    what = 101,
    week = ifelse(day(date_not) == day(original_date_not), week, ifelse(week-1 == 0, 7, week-1)),
    DD_not = day(date_not),  
    hh_not = hour(date_not),
    mm_not = minute(date_not)
  ) %>%
  select(id, date_not, first2w, week, DD_not, hh_not, mm_not, what, week_new,original_date_not, b6_2, b6_3,b6_4, b6_5,starts_with("c6_"))
b6_4 <- b6_4 %>%
  mutate(across(starts_with("c6_"), ~ifelse(b6_2 == 1|b6_3 == 1, 99, .)))

b6_5 <- snacks_df_filtered %>%
  filter(b6_5 == 1) %>%
  mutate(
    original_date_not = date_not, 
    date_not= date_not - minutes(120),
    what = 101,
    week = ifelse(day(date_not) == day(original_date_not), week, ifelse(week-1 == 0, 7, week-1)),
    DD_not = day(date_not),  
    hh_not = hour(date_not),
    mm_not = minute(date_not)
  ) %>%
  select(id, date_not, first2w, week, DD_not, hh_not, mm_not, what, week_new,original_date_not,b6_2, b6_3,b6_4, b6_5, starts_with("c6_"))
b6_5 <- b6_5 %>%
  mutate(across(starts_with("c6_"), ~ifelse(b6_2 == 1|b6_3 == 1|b6_4==1, 99, .)))

snacks_df <- rbind( b6_2, b6_3, b6_4, b6_5)
snacks_df <-select(snacks_df,-b6_2, -b6_3, -b6_4, -b6_5, -original_date_not)

#estraiamo il dataset dei pasti principali e uniamo snack + pasti principali
#--------------------------------------------------------------------------------------
main_meal_df <- tdita_data4diary %>%
  filter(what==3) %>%
  select(id, date_not, first2w, week, DD_not, hh_not, mm_not,
         what,week_new,c3_1, c3_2, c3_3, c3_4, c3_6, c3_7, c3_8, 
         c3_9, c3_10, c3_11 , c3_12, c3_13, c3_14, c3_15, c3_16, c3_17, 
         c3_18, c3_19, c3_20)

total_meal_df <- bind_rows(snacks_df, main_meal_df) 
total_meal_df <- total_meal_df %>% mutate_all(~na_if(., NA) %>% replace_na(0))

total_meal_df <- total_meal_df %>%
  mutate(
    hh_not_new = calculate_new_hh_not(hh_not),
    week_new = calculate_new_week(week, hh_not),
    DD_not_new = calculate_new_DD_not(DD_not, hh_not),
  )


#uniamo i pasti consecutivi
#-----------------------------------------------------------------------------------------------
total_meal_df <- total_meal_df %>%
  group_by(id) %>%
  arrange(id, date_not) 


total_meal_df <- total_meal_df %>%
  mutate(
    prev_what = lag(what),
    prev_time_diff = date_not - lag(date_not),
    is_consecutive = (
      !is.na(what) & !is.na(prev_what)& 
        !is.na(prev_time_diff) &
        what == 3 & prev_what == 3 & prev_time_diff <= 30*60))

total_meal_df <- total_meal_df %>%
  mutate(prev_what = ifelse(is.na(prev_what), 0, prev_what),
         prev_time_diff = ifelse(is.na(prev_time_diff), 0, prev_time_diff))%>%
  arrange(id, date_not)

for (i in 2:(nrow(total_meal_df))) {
  
  current_row <- total_meal_df[i, ]
  previous_row <- total_meal_df[i - 1, ]
  
  if (!is.na(current_row$is_consecutive) && !is.na(previous_row$is_consecutive) && 
      current_row$is_consecutive) {
    total_meal_df[i, "date_not"] <- min(current_row$date_not, previous_row$date_not)
    total_meal_df[i, "first2w"] <- min(current_row$first2w, previous_row$first2w)
    total_meal_df[i, "week"] <- min(current_row$week, previous_row$week)
    total_meal_df[i, "week_new"] <- min(current_row$week_new, previous_row$week_new)
    total_meal_df[i, "DD_not"] <- day(current_row$date_not)
    total_meal_df[i, "hh_not"] <- hour(current_row$date_not)
    total_meal_df[i, "mm_not"] <- minute(current_row$date_not)
    for (col_name in names(current_row)) {
      if (col_name != "date_not" && col_name != "first2w" && col_name != "week" &&
          col_name != "week_new" && col_name != "DD_not" && col_name != "hh_not" && col_name != "mm_not") {
        total_meal_df[i, col_name] <- max(current_row[[col_name]], previous_row[[col_name]])
      }
    }
    
    total_meal_df <- total_meal_df[-(i - 1), ]
  }
}

total_meal_df <- select(total_meal_df, -prev_what, -prev_time_diff, 
                        -is_consecutive) %>% arrange(id, date_not)

#aggiungiamo label colazione pranzo cena
#--------------------------------------------------------------------------------------
total_meal_df <- total_meal_df %>%
  mutate(
    type_meal = case_when(
      what == 3 & (hh_not >= 23 | hh_not <= 11) ~ 1,
      what == 3 & (hh_not >= 12 & hh_not <= 15) ~ 2,
      what == 3 & (hh_not >= 16 & hh_not <= 22) ~ 3,
      TRUE ~ NA_integer_
    )
  )
#dividiamo in prima e seconda settimana, feriali e festivi
#---------------------------------------------------------------------------------------------------
total_meal_df_firstweek= filter(total_meal_df, DD_not_new>=16, DD_not_new <= 22)
total_meal_df_firstweek_weekdays= filter(total_meal_df, DD_not_new>=16, DD_not_new <= 20)
total_meal_df_firstweek_weekend= filter(total_meal_df, DD_not_new>=21, DD_not_new <= 22)
total_meal_df_secondweek= filter(total_meal_df, DD_not_new>=23, DD_not_new <= 29)
total_meal_df_secondweek_weekdays= filter(total_meal_df, DD_not_new>=23, DD_not_new <= 27)
total_meal_df_secondweek_weekend= filter(total_meal_df, DD_not_new>=28, DD_not_new <= 29)
total_meal_df_weekdays = filter(total_meal_df, (DD_not_new>=16 & DD_not_new <= 20)|(DD_not_new>=23 & DD_not_new <= 27))
total_meal_df_weekend = filter(total_meal_df, (DD_not_new>=21 & DD_not_new <= 22)|(DD_not_new>=28 & DD_not_new <= 29))

# food_df_breakfast che ci indica chi mangia cosa e quando, lo dividiamo per tutte le combinazioni sopra
#---------------------------------------------------------------------------------------------------------
process_data <- function(df) {
  df %>%
    filter(type_meal == 1) %>%
    mutate(
      d1 = c3_3 + c3_4 + c6_12 + c6_11,
      d2 = c3_1 + c3_2 + c6_4 + c6_9 + c6_10,
      d3 = c6_6 + c6_7 + c3_11 + c3_20 + c6_26,
      d4 = c3_8 + c3_9 + c6_14 + c6_13,
      d5 = c3_6 + c3_7 + c6_5 + c6_16 + c6_17,
      d6 = c3_10 + c6_1 + c6_2 + c6_3 + c6_8
    ) %>%
    select(-matches("c3_[0-9]+|c6_[0-9]+")) %>%
    filter(d1 != 0 | d2 != 0 | d3 != 0 | d4 != 0 | d5 != 0 | d6 != 0)
}

food_df_breakfast <- process_data(total_meal_df)
food_df_breakfast_firstweek_weekdays <- process_data(total_meal_df_firstweek_weekdays)
food_df_breakfast_firstweek_weekend <- process_data(total_meal_df_firstweek_weekend)
food_df_breakfast_secondweek_weekdays <- process_data(total_meal_df_secondweek_weekdays)
food_df_breakfast_secondweek_weekend <- process_data(total_meal_df_secondweek_weekend)
food_df_breakfast_weekdays <- process_data(total_meal_df_weekdays)
food_df_breakfast_weekend <- process_data(total_meal_df_weekend)


# food_df_lunch che ci indica chi mangia cosa e quando, lo dividiamo per tutte le combinazioni sopra
# ---------------------------------------------------------------------------------------------
process_data <- function(df) {
 df %>%
    filter(type_meal == 2) %>%
    mutate(
      d1 = c3_3 + c3_4 + c6_12 + c6_11,  # Fruits and Vegetables
      d2 = c3_1 + c3_2 + c6_4 + c6_9 + c6_10,  # Bread, Pasta, Rice, Biscuits, and Potatoes
      d3 = c6_6 + c6_7 + c3_11 + c3_20 + c6_26, # Junk food
      d4 = c3_8 + c3_9 + c6_14 + c6_13,  # Milk, Yogurt, and Cheese
      d5 = c3_6 + c3_7 + c6_5 + c6_16 + c6_17,  # Meat, Fish, Eggs, Legumes, and Cured Meats
      d6 = c3_10 + c6_1 + c6_2 + c6_3 + c6_8, # Sweets
    ) %>%
    select(-c3_3, -c3_4, -c6_12, -c6_11,
           -c3_1, -c3_2, -c6_4, -c6_9, -c6_10,
           -c6_6, -c6_7, -c3_11, -c3_20, -c6_26,
           -c3_8, -c3_9, -c6_14, -c6_13,
           -c3_6, -c3_7, -c6_5, -c6_16, -c6_17,
           -c3_10, -c6_1, -c6_2, -c6_3, -c6_8, -c3_12, -c6_18,
           -c3_13, -c3_14, -c3_15, -c6_19, -c6_20, -c6_21,
           -c3_16, -c3_17, -c3_18, -c3_19, -c6_22, -c6_23, -c6_24, -c6_25, -type_meal) %>%
    filter(d1 != 0 | d2 != 0 | d3 != 0 | d4 != 0 | d5 != 0 | d6 != 0)
  
}

food_df_lunch <- process_data(total_meal_df)
food_df_lunch_firstweek_weekdays <- process_data(total_meal_df_firstweek_weekdays)
food_df_lunch_firstweek_weekend <- process_data(total_meal_df_firstweek_weekend)
food_df_lunch_secondweek_weekdays <- process_data(total_meal_df_secondweek_weekdays)
food_df_lunch_secondweek_weekend <- process_data(total_meal_df_secondweek_weekend)
food_df_lunch_weekdays <- process_data(total_meal_df_weekdays)
food_df_lunch_weekend <- process_data(total_meal_df_weekend)

# food_df_dinner che ci indica chi mangia cosa e quando, lo dividiamo per tutte le combinazioni sopra
#---------------------------------------------------------------------------------------------------------
# Define a function to filter and transform the data frames
process_food_data <- function(df) {
  df %>%
    filter(type_meal == 3) %>%
    mutate(
      d1 = c3_3 + c3_4 + c6_12 + c6_11,  # Fruits and Vegetables
      d2 = c3_1 + c3_2 + c6_4 + c6_9 + c6_10,  # Bread, Pasta, Rice, Biscuits, and Potatoes
      d3 = c6_6 + c6_7 + c3_11 + c3_20 + c6_26, # Junk food
      d4 = c3_8 + c3_9 + c6_14 + c6_13,  # Milk, Yogurt, and Cheese
      d5 = c3_6 + c3_7 + c6_5 + c6_16 + c6_17,  # Meat, Fish, Eggs, Legumes, and Cured Meats
      d6 = c3_10 + c6_1 + c6_2 + c6_3 + c6_8, # Sweets
    ) %>%
    select(-c3_3, -c3_4, -c6_12, -c6_11,
           -c3_1, -c3_2, -c6_4, -c6_9, -c6_10,
           -c6_6, -c6_7, -c3_11, -c3_20, -c6_26,
           -c3_8, -c3_9, -c6_14, -c6_13,
           -c3_6, -c3_7, -c6_5, -c6_16, -c6_17,
           -c3_10, -c6_1, -c6_2, -c6_3, -c6_8, -c3_12, -c6_18,
           -c3_13, -c3_14, -c3_15, -c6_19, -c6_20, -c6_21,
           -c3_16, -c3_17, -c3_18, -c3_19, -c6_22, -c6_23, -c6_24, -c6_25, -type_meal) %>%
    filter(d1 != 0 | d2 != 0 | d3 != 0 | d4 != 0 | d5 != 0 | d6 != 0)
  
}

food_df_dinner <- process_food_data(total_meal_df)
food_df_dinner_firstweek_weekdays <- process_food_data(total_meal_df_firstweek_weekdays)
food_df_dinner_firstweek_weekend <- process_food_data(total_meal_df_firstweek_weekend)
food_df_dinner_secondweek_weekdays <- process_food_data(total_meal_df_secondweek_weekdays)
food_df_dinner_secondweek_weekend <- process_food_data(total_meal_df_secondweek_weekend)
food_df_dinner_weekdays <- process_food_data(total_meal_df_weekdays)
food_df_dinner_weekend <- process_food_data(total_meal_df_weekend)

# food_df_snack che ci indica chi mangia cosa e quando, lo dividiamo per tutte le combinazioni sopra
#---------------------------------------------------------------------------------------------------------
process_data <- function(df) {
  df %>%
    filter(what == 101) %>%
    mutate(
      d1 = c3_3 + c3_4 + c6_12 + c6_11,
      d2 = c3_1 + c3_2 + c6_4 + c6_9 + c6_10,
      d3 = c6_6 + c6_7 + c3_11 + c3_20 + c6_26,
      d4 = c3_8 + c3_9 + c6_14 + c6_13,
      d5 = c3_6 + c3_7 + c6_5 + c6_16 + c6_17,
      d6 = c3_10 + c6_1 + c6_2 + c6_3 + c6_8
    ) %>%
    select(-c3_3, -c3_4, -c6_12, -c6_11,
           -c3_1, -c3_2, -c6_4, -c6_9, -c6_10,
           -c6_6, -c6_7, -c3_11, -c3_20, -c6_26,
           -c3_8, -c3_9, -c6_14, -c6_13,
           -c3_6, -c3_7, -c6_5, -c6_16, -c6_17,
           -c3_10, -c6_1, -c6_2, -c6_3, -c6_8, -c3_12, -c6_18,
           -c3_13, -c3_14, -c3_15, -c6_19, -c6_20, -c6_21,
           -c3_16, -c3_17, -c3_18, -c3_19, -c6_22, -c6_23, -c6_24, -c6_25, -type_meal) %>%
    filter(d1 != 0 | d2 != 0 | d3 != 0 | d4 != 0 | d5 != 0 | d6 != 0)
  
}

food_df_snack <- process_data(total_meal_df)
food_df_snack_firstweek_weekdays <- process_data(total_meal_df_firstweek_weekdays)
food_df_snack_firstweek_weekend <- process_data(total_meal_df_firstweek_weekend)
food_df_snack_secondweek_weekdays <- process_data(total_meal_df_secondweek_weekdays)
food_df_snack_secondweek_weekend <- process_data(total_meal_df_secondweek_weekend)
food_df_snack_weekdays <- process_data(total_meal_df_weekdays)
food_df_snack_weekend <- process_data(total_meal_df_weekend)


#analisi su breakfast, percentuale di persone che fanno colazione ogni giorno
#---------------------------------------------------------------------------------------------
create_breakfast_status_plot <- function(data, title, weekdays = TRUE) {
  total_people <- length(selected_ids2)
  
  if (weekdays) {
    custom_labels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  } else {
    custom_labels <- c("Saturday", "Sunday")
  }
  
  breakfast_counts <- data %>%
    group_by(week_new, id) %>%
    summarise(
      breakfast_days = case_when(
        n_distinct(DD_not_new) == 1 ~ "one out of two",
        n_distinct(DD_not_new) > 1 ~ "both"
      )
    ) %>%
    ungroup()
  
  breakfast_counts <- breakfast_counts %>%
    group_by(week_new, breakfast_days) %>%
    summarise(
      number_people = n()
    ) %>%
    ungroup()
  
  breakfast_counts <- breakfast_counts %>%
    mutate(
      percent = number_people / total_people * 100,
    )
  
  breakfast_counts <- breakfast_counts %>%
    bind_rows(
      breakfast_counts %>%
        group_by(week_new) %>%
        summarise(
          breakfast_days = "neither",
          number_people = total_people - sum(number_people),
          percent = 100 - sum(percent)
        )
    )
  
  breakfast_counts <- breakfast_counts %>%
    arrange(week_new, breakfast_days)
  
  breakfast_counts$week_new <- factor(breakfast_counts$week_new, levels = unique(breakfast_counts$week_new))
  breakfast_counts_plot <- breakfast_counts %>%
    pivot_longer(cols = c("percent"), names_to = "breakfast_status", values_to = "percentage")
  
  p <- ggplot(breakfast_counts_plot, aes(x = factor(week_new, labels = custom_labels), y = percentage, fill = as.factor(breakfast_days))) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    labs(
      title = title,
      x = "Day",
      y = "Percentage",
      fill = "Breakfast Days"
    ) +
    scale_fill_manual(values = c("neither" = "#e74c3c", "one out of two" = "#f39c12", "both" = "#6fbf4e")) +
    theme_minimal() +
    theme(legend.title = element_blank()) +
    theme(axis.text.x = element_text(hjust = 1)) +
    theme(legend.position = "top") +
    theme(plot.title = element_text(hjust = 0.5, size = 16))
  
  p + geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_dodge(width = 0.7), vjust = 1, size = 3)
}

create_breakfast_status_plot(food_df_breakfast_weekdays, "percentage breakfast weekdays", weekdays = TRUE)
create_breakfast_status_plot(food_df_breakfast_weekend, "percentage breakfast weekend", weekdays = FALSE)

breakfast_results_weekdays <- food_df_breakfast_weekdays %>%
  group_by(id, DD_not_new) %>%
  summarize(breakfast_count = n()) %>%
  mutate(breakfast_indicator = ifelse(breakfast_count > 0, 1, 0))

breakfast_counts_weekdays <- breakfast_results_weekdays %>%
  group_by(id) %>%
  summarize(total_breakfasts = sum(breakfast_indicator))

all_ids_breakfast <- data.frame(id = selected_ids2)

breakfast_counts_weekdays <- breakfast_counts_weekdays %>%
  right_join(all_ids_breakfast, by = "id") %>%
  replace_na(list(total_breakfasts = 0))

create_groups <- breakfast_counts_weekdays %>%
  group_by(total_breakfasts) %>%
  summarize(
    num_people = n()
  )

ggplot(create_groups, aes(x = as.factor(total_breakfasts), y = num_people)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(
    title = "Distribution of Total Breakfast Counts over 10 days (weekdays)",
    x = "Total Breakfast Counts",
    y = "Number of People"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


breakfast_counts_weekdays <- breakfast_counts_weekdays %>%
  mutate(
    breakfast_group = case_when(
      total_breakfasts <= 4 ~ "0-4 days",
      total_breakfasts >= 5 & total_breakfasts < 9 ~ "5-8 days",
      total_breakfasts >= 9 ~ "9+ days"
    )
  )

percentage_breakfast_weekdays <- breakfast_counts_weekdays %>%
  group_by(breakfast_group) %>%
  summarize(
    num_people = n(),
    percentage = (num_people / length(selected_ids2)) * 100
  )

ggplot(percentage_breakfast_weekdays, aes(x = "", y = percentage, fill = breakfast_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("0-4 days" = "#3498db", "5-8 days" = "#f1c40f", "9+ days" = "#ff0000")) +
  labs(
    title = "Breakfast Frequency Distribution over 10 days (weekdays)",
    fill = "Breakfast Frequency"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))+
  geom_text(aes(label = paste0(round(percentage, 1), "%"), 
              angle = 90 - cumsum(percentage) + percentage / 2),
            position = position_stack(vjust = 0.5), size = 5, color = "black", angle = 0)

breakfast_results_weekend <- food_df_breakfast_weekend %>%
  group_by(id, DD_not_new) %>%
  summarize(breakfast_count = n()) %>%
  mutate(breakfast_indicator = ifelse(breakfast_count > 0, 1, 0))

breakfast_counts_weekend <- breakfast_results_weekend %>%
  group_by(id) %>%
  summarize(total_breakfasts = sum(breakfast_indicator))

all_ids_breakfast <- data.frame(id = selected_ids2)

breakfast_counts_weekend <- breakfast_counts_weekend %>%
  right_join(all_ids_breakfast, by = "id") %>%
  replace_na(list(total_breakfasts = 0))

create_groups <- breakfast_counts_weekend %>%
  group_by(total_breakfasts) %>%
  summarize(
    num_people = n()
  )

ggplot(create_groups, aes(x = as.factor(total_breakfasts), y = num_people)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(
    title = "Distribution of Total Breakfast Counts over 4 days (weekends)",
    x = "Total Breakfast Counts",
    y = "Number of People"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

median_breakfast <- median(breakfast_counts_weekend$total_breakfasts)

breakfast_counts_weekend <- breakfast_counts_weekend %>%
  mutate(
    breakfast_group = case_when(
      total_breakfasts < median_breakfast ~ "0-2 days",
      total_breakfasts >= median_breakfast  ~ "3+ days"
    )
  )

percentage_breakfast_weekend <- breakfast_counts_weekend %>%
  group_by(breakfast_group) %>%
  summarize(
    num_people = n(),
    percentage = (num_people / length(selected_ids2)) * 100
  )

ggplot(percentage_breakfast_weekend, aes(x = "", y = percentage, fill = breakfast_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("0-2 days" = "#3498db", "3+ days" = "#e74c3c")) +
  labs(
    title = "Breakfast Frequency Distribution over 4 days (weekends)",
    fill = "Breakfast Frequency"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))+
  geom_text(aes(label = paste0(round(percentage, 1), "%"), 
                angle = 90 - cumsum(percentage) + percentage / 2),
            position = position_stack(vjust = 0.5), size = 5, color = "black", angle = 0)


# analisi su lunch, percentuale di persone che fanno pranzo ogni giorno
# ---------------------------------------------------------------------------------------------
create_lunch_status_plot <- function(data, title, weekdays = TRUE) {
  total_people <- length(selected_ids2)
  
  if (weekdays) {
    custom_labels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  } else {
    custom_labels <- c("Saturday", "Sunday")
  }
  
  lunch_counts <- data %>%
    group_by(week_new, id) %>%
    summarise(
      lunch_days = case_when(
        n_distinct(DD_not_new) == 1 ~ "one out of two",
        n_distinct(DD_not_new) > 1 ~ "both"
      )
    ) %>%
    ungroup()
  
  lunch_counts <- lunch_counts %>%
    group_by(week_new, lunch_days) %>%
    summarise(
      number_people = n()
    ) %>%
    ungroup()
  
  lunch_counts <- lunch_counts %>%
    mutate(
      percent = number_people / total_people * 100,
    )
  
  lunch_counts <- lunch_counts %>%
    bind_rows(
      lunch_counts %>%
        group_by(week_new) %>%
        summarise(
          lunch_days = "neither",
          number_people = total_people - sum(number_people),
          percent = 100 - sum(percent)
        )
    )
  
  lunch_counts <- lunch_counts %>%
    arrange(week_new, lunch_days)
  
  lunch_counts$week_new <- factor(lunch_counts$week_new, levels = unique(lunch_counts$week_new))
  lunch_counts_plot <- lunch_counts %>%
    pivot_longer(cols = c("percent"), names_to = "lunch_status", values_to = "percentage")
  
  p <- ggplot(lunch_counts_plot, aes(x = factor(week_new, labels = custom_labels), y = percentage, fill = as.factor(lunch_days))) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    labs(
      title = title,
      x = "Day",
      y = "Percentage",
      fill = "lunch Days"
    ) +
    scale_fill_manual(values = c("neither" = "#e74c3c", "one out of two" = "#f39c12", "both" = "#6fbf4e")) +
    theme_minimal() +
    theme(legend.title = element_blank()) +
    theme(axis.text.x = element_text(hjust = 1)) +
    theme(legend.position = "top") +
    theme(plot.title = element_text(hjust = 0.5, size = 16))
  
  p + geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_dodge(width = 0.7), vjust = 1, size = 3)
}

create_lunch_status_plot(food_df_lunch_weekdays, "percentage lunch weekdays", weekdays = TRUE)
create_lunch_status_plot(food_df_lunch_weekend, "percentage lunch weekend", weekdays = FALSE)

lunch_results_weekdays <- food_df_lunch_weekdays %>%
  group_by(id, DD_not_new) %>%
  summarize(lunch_count = n()) %>%
  mutate(lunch_indicator = ifelse(lunch_count > 0, 1, 0))

lunch_counts_weekdays <- lunch_results_weekdays %>%
  group_by(id) %>%
  summarize(total_lunches = sum(lunch_indicator))

all_ids_lunch <- data.frame(id = selected_ids2)

lunch_counts_weekdays <- lunch_counts_weekdays %>%
  right_join(all_ids_lunch, by = "id") %>%
  replace_na(list(total_lunches = 0))

create_groups <- lunch_counts_weekdays %>%
  group_by(total_lunches) %>%
  summarize(
    num_people = n()
  )

ggplot(create_groups, aes(x = as.factor(total_lunches), y = num_people)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(
    title = "Distribution of Total lunch Counts over 10 days (weekdays)",
    x = "Total lunch Counts",
    y = "Number of People"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

lunch_counts_weekdays <- lunch_counts_weekdays %>%
  mutate(
    lunch_group = case_when(
      total_lunches < 5 ~ "0-4 days",
      total_lunches >= 5 & total_lunches < 9 ~ "5-8 days",
      total_lunches >= 9 ~ "9+ days"
    )
  )

percentage_lunch_weekdays <- lunch_counts_weekdays %>%
  group_by(lunch_group) %>%
  summarize(
    num_people = n(),
    percentage = (num_people / length(selected_ids2)) * 100
  )

ggplot(percentage_lunch_weekdays, aes(x = "", y = percentage, fill = lunch_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("0-4 days" = "#3498db", "5-8 days" = "#f1c40f", "9+ days" = "#ff0000")) +
  labs(
    title = "Lunch Frequency Distribution over 10 days (weekdays)",
    fill = "Lunch Frequency"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  geom_text(aes(label = paste0(round(percentage, 1), "%"),
                angle = 90 - cumsum(percentage) + percentage / 2),
            position = position_stack(vjust = 0.5), size = 5, color = "black", angle = 0)

lunch_results_weekend <- food_df_lunch_weekend %>%
  group_by(id, DD_not_new) %>%
  summarize(lunch_count = n()) %>%
  mutate(lunch_indicator = ifelse(lunch_count > 0, 1, 0))

lunch_counts_weekend <- lunch_results_weekend %>%
  group_by(id) %>%
  summarize(total_lunches = sum(lunch_indicator))

all_ids_lunch <- data.frame(id = selected_ids2)

lunch_counts_weekend <- lunch_counts_weekend %>%
  right_join(all_ids_lunch, by = "id") %>%
  replace_na(list(total_lunches = 0))

create_groups <- lunch_counts_weekend %>%
  group_by(total_lunches) %>%
  summarize(
    num_people = n()
  )

ggplot(create_groups, aes(x = as.factor(total_lunches), y = num_people)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(
    title = "Distribution of Total lunch Counts over 4 days (weekends)",
    x = "Total lunch Counts",
    y = "Number of People"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

median_lunch <- median(lunch_counts_weekend$total_lunches)

lunch_counts_weekend <- lunch_counts_weekend %>%
  mutate(
    lunch_group = case_when(
      total_lunches < median_lunch ~ "0-2 days",
      total_lunches >= median_lunch  ~ "3+ days"
    )
  )

percentage_lunch_weekend <- lunch_counts_weekend %>%
  group_by(lunch_group) %>%
  summarize(
    num_people = n(),
    percentage = (num_people / length(selected_ids2)) * 100
  )

ggplot(percentage_lunch_weekend, aes(x = "", y = percentage, fill = lunch_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("0-2 days" = "#3498db", "3+ days" = "#ff0000")) +
  labs(
    title = "Lunch Frequency Distribution over 4 days (weekends)",
    fill = "Lunch Frequency"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  geom_text(aes(label = paste0(round(percentage, 1), "%"),
                angle = 90 - cumsum(percentage) + percentage / 2),
            position = position_stack(vjust = 0.5), size = 5, color = "black", angle = 0)


# analisi su dinner, percentuale di persone che fanno cena ogni giorno
# ---------------------------------------------------------------------------------------------
create_dinner_status_plot <- function(data, title, weekdays = TRUE) {
  total_people <- length(selected_ids2)
  
  if (weekdays) {
    custom_labels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  } else {
    custom_labels <- c("Saturday", "Sunday")
  }
  
  dinner_counts <- data %>%
    group_by(week_new, id) %>%
    summarise(
      dinner_days = case_when(
        n_distinct(DD_not_new) == 1 ~ "one out of two",
        n_distinct(DD_not_new) > 1 ~ "both"
      )
    ) %>%
    ungroup()
  
  dinner_counts <- dinner_counts %>%
    group_by(week_new, dinner_days) %>%
    summarise(
      number_people = n()
    ) %>%
    ungroup()
  
  dinner_counts <- dinner_counts %>%
    mutate(
      percent = number_people / total_people * 100,
    )
  
  dinner_counts <- dinner_counts %>%
    bind_rows(
      dinner_counts %>%
        group_by(week_new) %>%
        summarise(
          dinner_days = "neither",
          number_people = total_people - sum(number_people),
          percent = 100 - sum(percent)
        )
    )
  
  dinner_counts <- dinner_counts %>%
    arrange(week_new, dinner_days)
  
  dinner_counts$week_new <- factor(dinner_counts$week_new, levels = unique(dinner_counts$week_new))
  dinner_counts_plot <- dinner_counts %>%
    pivot_longer(cols = c("percent"), names_to = "dinner_status", values_to = "percentage")
  
  p <- ggplot(dinner_counts_plot, aes(x = factor(week_new, labels = custom_labels), y = percentage, fill = as.factor(dinner_days))) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    labs(
      title = title,
      x = "Day",
      y = "Percentage",
      fill = "dinner Days"
    ) +
    scale_fill_manual(values = c("neither" = "#e74c3c", "one out of two" = "#f39c12", "both" = "#6fbf4e")) +
    theme_minimal() +
    theme(legend.title = element_blank()) +
    theme(axis.text.x = element_text(hjust = 1)) +
    theme(legend.position = "top") +
    theme(plot.title = element_text(hjust = 0.5, size = 16))
  
  p + geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_dodge(width = 0.7), vjust = 1, size = 3)
}

create_dinner_status_plot(food_df_dinner_weekdays, "percentage dinner weekdays", weekdays = TRUE)
create_dinner_status_plot(food_df_dinner_weekend, "percentage dinner weekend", weekdays = FALSE)

dinner_results_weekdays <- food_df_dinner_weekdays %>%
  group_by(id, DD_not_new) %>%
  summarize(dinner_count = n()) %>%
  mutate(dinner_indicator = ifelse(dinner_count > 0, 1, 0))

dinner_counts_weekdays <- dinner_results_weekdays %>%
  group_by(id) %>%
  summarize(total_dinners = sum(dinner_indicator))

all_ids_dinner <- data.frame(id = selected_ids2)

dinner_counts_weekdays <- dinner_counts_weekdays %>%
  right_join(all_ids_dinner, by = "id") %>%
  replace_na(list(total_dinners = 0))

create_groups <- dinner_counts_weekdays %>%
  group_by(total_dinners) %>%
  summarize(
    num_people = n()
  )

ggplot(create_groups, aes(x = as.factor(total_dinners), y = num_people)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(
    title = "Distribution of Total dinners Counts over 10 days (weekdays)",
    x = "Total lunch Counts",
    y = "Number of People"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


dinner_counts_weekdays <- dinner_counts_weekdays %>%
  mutate(
    dinner_group = case_when(
      total_dinners < 5 ~ "0-4 days",
      total_dinners >= 5 & total_dinners < 9 ~ "5-8 days",
      total_dinners >= 9 ~ "9+ days"
    )
  )

percentage_dinner_weekdays <- dinner_counts_weekdays %>%
  group_by(dinner_group) %>%
  summarize(
    num_people = n(),
    percentage = (num_people / length(selected_ids2)) * 100
  )

ggplot(percentage_dinner_weekdays, aes(x = "", y = percentage, fill = dinner_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("0-4 days" = "#3498db", "5-8 days" = "#f1c40f", "9+ days" = "#ff0000")) +
  labs(
    title = "Dinner Frequency Distribution over 10 days (weekdays)",
    fill = "Dinner Frequency"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  geom_text(aes(label = paste0(round(percentage, 1), "%"),
                angle = 90 - cumsum(percentage) + percentage / 2),
            position = position_stack(vjust = 0.5), size = 5, color = "black", angle = 0)

dinner_results_weekend <- food_df_dinner_weekend %>%
  group_by(id, DD_not_new) %>%
  summarize(dinner_count = n()) %>%
  mutate(dinner_indicator = ifelse(dinner_count > 0, 1, 0))

dinner_counts_weekend <- dinner_results_weekend %>%
  group_by(id) %>%
  summarize(total_dinners = sum(dinner_indicator))

all_ids_dinner <- data.frame(id = selected_ids2)

dinner_counts_weekend <- dinner_counts_weekend %>%
  right_join(all_ids_dinner, by = "id") %>%
  replace_na(list(total_dinners = 0))

create_groups <- dinner_counts_weekend %>%
  group_by(total_dinners) %>%
  summarize(
    num_people = n()
  )

ggplot(create_groups, aes(x = as.factor(total_dinners), y = num_people)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(
    title = "Distribution of Total dinners Counts over 4 days (weekends)",
    x = "Total lunch Counts",
    y = "Number of People"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

median_dinner <- median(dinner_counts_weekend$total_dinners)

dinner_counts_weekend <- dinner_counts_weekend %>%
  mutate(
    dinner_group = case_when(
      total_dinners < median_dinner  ~ "0-2 days",
      total_dinners >= median_dinner  ~ "3+ days"
    )
  )

percentage_dinner_weekend <- dinner_counts_weekend %>%
  group_by(dinner_group) %>%
  summarize(
    num_people = n(),
    percentage = (num_people / length(selected_ids2)) * 100
  )

ggplot(percentage_dinner_weekend, aes(x = "", y = percentage, fill = dinner_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("0-2 days" = "#3498db", "3+ days" = "#e74c3c")) +
  labs(
    title = "Dinner Frequency Distribution over 4 days (weekends)",
    fill = "Dinner Frequency"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  geom_text(aes(label = paste0(round(percentage, 1), "%"),
                angle = 90 - cumsum(percentage) + percentage / 2),
            position = position_stack(vjust = 0.5), size = 5, color = "black", angle = 0)


# analisi su snack, quanti snack ogni persona fa ogni giorno
# ---------------------------------------------------------------------------------------------
create_snack_group_plot <- function(data, title,weekdays=TRUE) {
  if (weekdays) {
    custom_labels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  } else {
    custom_labels <- c("Saturday", "Sunday")
  }
  
  all_ids_snack <- expand.grid(id = selected_ids2, week_new = unique(data$week_new))
  
  snack_counts <- data %>%
    group_by(id, week_new) %>%
    summarize(snack_count = n()) %>%
    right_join(all_ids_snack, by = c("id", "week_new")) %>%
    replace_na(list(snack_count = 0))
  
  snack_counts <- snack_counts %>%
    mutate(
      snack_group = case_when(
        snack_count %in% 0:1 ~ "0-1 Snacks",
        snack_count >= 2 ~ "2+ Snacks"
      )
    )
  
  summary_data <- snack_counts %>%
    group_by(week_new, snack_group) %>%
    summarise(count = n()) %>%
    mutate(percentage = (count / length(selected_ids2)) * 100)
  
  p <- ggplot(summary_data, aes(x = factor(week_new, labels = custom_labels), y = percentage, fill = snack_group)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    labs(
      title = title,
      x = "Weekday",
      y = "Percentage",
      fill = "snack_group"
    ) +
    theme_minimal() +
    theme(legend.title = element_blank()) +
    theme(axis.text.x = element_text(hjust = 1)) +
    theme(legend.position = "top") +
    theme(plot.title = element_text(hjust = 0.5, size = 16)
    )
  
  p + geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_dodge(width = 0.7), vjust = 1, size = 3)
}

plot1 <-create_snack_group_plot(food_df_snack_firstweek_weekdays, "Weekdays - First Week")
plot2 <-create_snack_group_plot(food_df_snack_firstweek_weekend, "Weekend - First Week",weekdays = FALSE)
plot3 <-create_snack_group_plot(food_df_snack_secondweek_weekdays, "Weekdays - Second Week")
plot4 <-create_snack_group_plot(food_df_snack_secondweek_weekend, "Weekend - Second Week",weekdays = FALSE)
grid.arrange(plot1, plot3, ncol = 1)
grid.arrange(plot2, plot4, ncol = 1)

snack_counts_weekdays <- food_df_snack_weekdays %>%
  group_by(id) %>%
  summarize(total_snack = n()) 

all_ids_snack <- data.frame(id = selected_ids2)

snack_counts_weekdays <- snack_counts_weekdays %>%
  right_join(all_ids_snack, by = "id") %>%
  replace_na(list(total_snack = 0))

create_groups <- snack_counts_weekdays %>%
  group_by(total_snack) %>%
  summarize(
    num_people = n()
  )

ggplot(create_groups, aes(x = as.factor(total_snack), y = num_people)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(
    title = "Distribution of Total snack Counts over 10 days (weekdays)",
    x = "Total snack Counts",
    y = "Number of People"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

q1_snacks <- quantile(snack_counts_weekdays$total_snack, probs = 0.25)
q3_snacks <- quantile(snack_counts_weekdays$total_snack, probs = 0.75)
snack_counts_weekdays <- snack_counts_weekdays %>%
  mutate(
    snack_group = case_when(
      total_snack < q1_snacks ~ "0-4 times",
      total_snack >= q1_snacks & total_snack < q3_snacks ~ "5-13 times",
      total_snack >= q3_snacks ~ "14+ times"
    )
  )

summary_data_weekdays <- snack_counts_weekdays %>%
  group_by(snack_group) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / length(selected_ids2)) * 100)


ggplot(summary_data_weekdays, aes(x = "", y = percentage, fill = snack_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Snack Frequency Distribution over 10 days (weekdays)",
    fill = "Snack Group"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 5, color = "black", angle = 0)


snack_counts_weekend <- food_df_snack_weekend %>%
  group_by(id) %>%
  summarize(total_snack = n()) 

all_ids_snack <- data.frame(id = selected_ids2)

snack_counts_weekend <- snack_counts_weekend %>%
  right_join(all_ids_snack, by = "id") %>%
  replace_na(list(total_snack = 0))

create_groups <- snack_counts_weekend %>%
  group_by(total_snack) %>%
  summarize(
    num_people = n()
  )

ggplot(create_groups, aes(x = as.factor(total_snack), y = num_people)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(
    title = "Distribution of Total snack Counts over 4 days (weekends)",
    x = "Total snack Counts",
    y = "Number of People"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


median_snacks <- median(snack_counts_weekend$total_snack)
snack_counts_weekend <- snack_counts_weekend %>%
  mutate(
    snack_group = case_when(
      total_snack < median_snacks ~ "0-2 times",
      total_snack >= median_snacks ~ "3+ times"
    )
  )

summary_data_weekend <- snack_counts_weekend %>%
  group_by(snack_group) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / length(selected_ids2)) * 100)


ggplot(summary_data_weekend, aes(x = "", y = percentage, fill = snack_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Snack Frequency Distribution over 4 days (weekends)",
    fill = "Snack Group"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 5, color = "black", angle = 0)


# analisi dieta breakfast
# ---------------------------------------------------------------------------------------------
calculate_diet_patterns <- function(data) {
  data %>%
    group_by(id) %>%
    summarize(
      frt_vgt = n_distinct(week_new[d1 > 0]),
      brd_pst_rc_pt = n_distinct(week_new[d2 > 0]),
      jnkf = n_distinct(week_new[d3 > 0]),
      mlk_ygrt_chs = n_distinct(week_new[d4 > 0]),
      mt_fsh_eg_lgm = n_distinct(week_new[d5 > 0]),
      swt = n_distinct(week_new[d6 > 0])
    )
}

diet_patterns_breakfast_firstweek_weekdays <- calculate_diet_patterns(food_df_breakfast_firstweek_weekdays)
diet_patterns_breakfast_firstweek_weekend <- calculate_diet_patterns(food_df_breakfast_firstweek_weekend)
diet_patterns_breakfast_secondweek_weekdays <- calculate_diet_patterns(food_df_breakfast_secondweek_weekdays)
diet_patterns_breakfast_secondweek_weekend <- calculate_diet_patterns(food_df_breakfast_secondweek_weekend)
diet_patterns_breakfast_weekdays <- calculate_diet_patterns(food_df_breakfast_weekdays)
diet_patterns_breakfast_weekend <- calculate_diet_patterns(food_df_breakfast_weekend)

food_categories <- c("frt_vgt", "brd_pst_rc_pt", "jnkf", "mlk_ygrt_chs", "mt_fsh_eg_lgm", "swt")

generate_food_category_plot <- function(data, food_categories, title) {
  results <- data.frame()
  for (food_category in food_categories) {
    categorized_data <- data %>%
      filter(id %in% selected_ids2) %>%
      mutate(category = case_when(
        .data[[food_category]] == 0 ~ "0 days",
        .data[[food_category]] %in% 1:2 ~ "1-2 days",
        .data[[food_category]] >= 3 ~ "3+ days"
      )) %>%
      group_by(category) %>%
      summarise(count = n()) %>%
      mutate(percentage = (count / sum(count)) * 100) %>%
      mutate(food_category = food_category)
    
    results <- bind_rows(results, categorized_data)
  }
  
  ggplot(results, aes(x = food_category, y = percentage, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = 0.7), position = position_dodge(width = 0.8) , size = 3) +
    labs(
      title = title,
      x = "Food Category",
      y = "Percentage"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
}



plot1 <- generate_food_category_plot(diet_patterns_breakfast_firstweek_weekdays, food_categories, "Percentage of People by Food Category (Breakfast, First Week, Weekdays)")
plot2 <- generate_food_category_plot(diet_patterns_breakfast_firstweek_weekend, food_categories, "Percentage of People by Food Category (Breakfast, First Week, Weekend)")
plot3 <- generate_food_category_plot(diet_patterns_breakfast_secondweek_weekdays, food_categories, "Percentage of People by Food Category (Breakfast, Second Week, Weekdays)")
plot4 <- generate_food_category_plot(diet_patterns_breakfast_secondweek_weekend, food_categories, "Percentage of People by Food Category (Breakfast, Second Week, Weekend)")

generate_food_category_plot <- function(data, food_categories, title, wd= TRUE) {
  results <- data.frame()
  if(wd==TRUE){
  for (food_category in food_categories) {
    categorized_data <- data %>%
      filter(id %in% selected_ids2) %>%
      mutate(category = case_when(
        .data[[food_category]] %in% 0 ~ "0 days",
        .data[[food_category]] %in% 1:4 ~ "1-4 days",
        .data[[food_category]] >= 5 ~ "5+ days"
      )) %>%
      group_by(category) %>%
      summarise(count = n()) %>%
      mutate(percentage = (count / sum(count)) * 100) %>%
      mutate(food_category = food_category)
    
    results <- bind_rows(results, categorized_data)
  }
    }else{
    for (food_category in food_categories) {
    categorized_data <- data %>%
      filter(id %in% selected_ids2) %>%
      mutate(category = case_when(
        .data[[food_category]] %in% 0:1 ~ "0 days",
        .data[[food_category]] %in% 1:2 ~ "1-2 days",
        .data[[food_category]] >= 3 ~ "3+ days"
      )) %>%
      group_by(category) %>%
      summarise(count = n()) %>%
      mutate(percentage = (count / sum(count)) * 100) %>%
      mutate(food_category = food_category)
    
    results <- bind_rows(results, categorized_data)
  }}
  
  ggplot(results, aes(x = food_category, y = percentage, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = 0.7), position = position_dodge(width = 0.8) , size = 3) +
    labs(
      title = title,
      x = "Food Category",
      y = "Percentage"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
}
plot5 <- generate_food_category_plot(diet_patterns_breakfast_weekdays, food_categories, "Percentage of People by Food Category (Breakfast, Weekdays)", wd=TRUE)
plot6 <- generate_food_category_plot(diet_patterns_breakfast_weekend, food_categories, "Percentage of People by Food Category (Breakfast, Weekend)", wd=FALSE)

grid.arrange(plot1, plot3, ncol = 1)
grid.arrange(plot2, plot4, ncol = 1)
grid.arrange(plot5, plot6, ncol = 1)


tdita_data4diary_bf_analysis_weekdays <- tdita_data4diary %>%
  left_join(breakfast_counts_weekdays, by = c("id" = "id")) %>%
  distinct(id, .keep_all = TRUE) %>% select(id, department, w1_A01, degree, breakfast_group)

groups_gender_bf_weekdays <- tdita_data4diary_bf_analysis_weekdays %>%
  filter(!is.na(w1_A01))%>%
  group_by(w1_A01, breakfast_group) %>%
  summarise(count = n()) %>%
  group_by(w1_A01) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = breakfast_group, values_from = percentage)

groups_gender_bf_weekdays <- groups_gender_bf_weekdays %>%
  select(w1_A01, "0-4 days", "5-8 days",  "9+ days")

groups_gender_bf_weekdays <- groups_gender_bf_weekdays %>%
  replace(is.na(.), 0) %>%
  group_by(w1_A01) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_gender_bf_weekdays[, c("w1_A01", "0-4 days", "5-8 days", "9+ days")])
print(chi_squared_result)

melted_data <- tidyr::gather(groups_gender_bf_weekdays, key = "Breakfast_Group", value = "Percentage", -w1_A01)
melted_data$w1_A01 <- factor(melted_data$w1_A01, labels = c("Male", "Female"))
ggplot(melted_data, aes(x = w1_A01, y = Percentage, fill = Breakfast_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of Breakfast Groups by Gender (Weekdays)",
    x = "Gender",
    y = "Percentage",
    fill = "Breakfast Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

groups_department_bf_weekdays <- tdita_data4diary_bf_analysis_weekdays %>%
  filter(!is.na(department))%>%
  group_by(department, breakfast_group) %>%
  summarise(count = n()) %>%
  group_by(department) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = breakfast_group, values_from = percentage)

groups_department_bf_weekdays <- groups_department_bf_weekdays %>%
  select(department, "0-4 days", "5-8 days",  "9+ days")

groups_department_bf_weekdays <- groups_department_bf_weekdays %>%
  replace(is.na(.), 0) %>%
  group_by(department) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_department_bf_weekdays[, c("department", "0-4 days", "5-8 days", "9+ days")])
print(chi_squared_result)

melted_data_department <- tidyr::gather(groups_department_bf_weekdays, key = "Breakfast_Group", value = "Percentage", -department)
department_names <- c(
  "1" = "Business/Economics",
  "2" = "International Relations and Public Admin",
  "3"= "Law",
  "4" = "Engineering and Applied Sciences",
  "5" = "Social Sciences",
  "6" = "Natural Sciences",
  "7" = "Humanities",
  "9" = "Medicine and Veterinary Medicine"
)
ggplot(melted_data_department, aes(x = factor(department, levels = names(department_names)), y = Percentage, fill = Breakfast_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = 0.5, size = 2) +
  labs(
    title = "Distribution of Breakfast Groups by Department (Weekdays)",
    x = "Department",
    y = "Percentage",
    fill = "Breakfast Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels = department_names) + 
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

groups_degree_bf_weekdays <- tdita_data4diary_bf_analysis_weekdays %>%
  filter(!is.na(degree))%>%
  group_by(degree, breakfast_group) %>%
  summarise(count = n()) %>%
  group_by(degree) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = breakfast_group, values_from = percentage)

groups_degree_bf_weekdays <- groups_degree_bf_weekdays %>%
  select(degree, "0-4 days", "5-8 days",  "9+ days")

groups_degree_bf_weekdays <- groups_degree_bf_weekdays %>%
  replace(is.na(.), 0) %>%
  group_by(degree) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_degree_bf_weekdays[, c("degree", "0-4 days", "5-8 days", "9+ days")])
print(chi_squared_result)

melted_data <- tidyr::gather(groups_degree_bf_weekdays, key = "Breakfast_Group", value = "Percentage", -degree)
melted_data$degree <- factor(melted_data$degree, labels = c("BSc", "MSc"))
ggplot(melted_data, aes(x = degree, y = Percentage, fill = Breakfast_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of Breakfast Groups by Degree (Weekdays)",
    x = "degree",
    y = "Percentage",
    fill = "Breakfast Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


tdita_data4diary_bf_analysis_weekend <- tdita_data4diary %>%
  left_join(breakfast_counts_weekend, by = c("id" = "id")) %>%
  distinct(id, .keep_all = TRUE) %>% select(id, department, degree, w1_A01, breakfast_group)

groups_gender_bf_weekend <- tdita_data4diary_bf_analysis_weekend %>%
  filter(!is.na(w1_A01))%>%
  group_by(w1_A01, breakfast_group) %>%
  summarise(count = n()) %>%
  group_by(w1_A01) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = breakfast_group, values_from = percentage)

groups_gender_bf_weekend <- groups_gender_bf_weekend %>%
  select(w1_A01, "0-2 days", "3+ days")

groups_gender_bf_weekend <- groups_gender_bf_weekend %>%
  replace(is.na(.), 0) %>%
  group_by(w1_A01) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_gender_bf_weekend[, c("w1_A01", "0-2 days", "3+ days")])
print(chi_squared_result)

melted_data <- tidyr::gather(groups_gender_bf_weekend, key = "Breakfast_Group", value = "Percentage", -w1_A01)
melted_data$w1_A01 <- factor(melted_data$w1_A01, labels = c("Male", "Female"))
ggplot(melted_data, aes(x = w1_A01, y = Percentage, fill = Breakfast_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of Breakfast Groups by Gender (Weekend)",
    x = "Gender",
    y = "Percentage",
    fill = "Breakfast Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))



groups_department_bf_weekend <- tdita_data4diary_bf_analysis_weekend %>%
  filter(!is.na(department))%>%
  group_by(department, breakfast_group) %>%
  summarise(count = n()) %>%
  group_by(department) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = breakfast_group, values_from = percentage)

groups_department_bf_weekend <- groups_department_bf_weekend %>%
  select(department, "0-2 days", "3+ days")

groups_department_bf_weekend <- groups_department_bf_weekend %>%
  replace(is.na(.), 0) %>%
  group_by(department) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_department_bf_weekend[, c("department", "0-2 days", "3+ days")])
print(chi_squared_result)

melted_data_department <- tidyr::gather(groups_department_bf_weekend, key = "Breakfast_Group", value = "Percentage", -department)
department_names <- c(
  "1" = "Business/Economics",
  "2" = "International Relations and Public Admin",
  "3"= "Law",
  "4" = "Engineering and Applied Sciences",
  "5" = "Social Sciences",
  "6" = "Natural Sciences",
  "7" = "Humanities",
  "9" = "Medicine and Veterinary Medicine"
)

ggplot(melted_data_department, aes(x = factor(department, levels = names(department_names)), y = Percentage, fill = Breakfast_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = 0.5, size = 2) +
  labs(
    title = "Distribution of Breakfast Groups by Department (Weekend)",
    x = "Department",
    y = "Percentage",
    fill = "Breakfast Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels = department_names) +  
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

groups_degree_bf_weekend <- tdita_data4diary_bf_analysis_weekend %>%
  filter(!is.na(degree))%>%
  group_by(degree, breakfast_group) %>%
  summarise(count = n()) %>%
  group_by(degree) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = breakfast_group, values_from = percentage)

groups_degree_bf_weekend <- groups_degree_bf_weekend %>%
  select(degree, "0-2 days", "3+ days")

groups_degree_bf_weekend <- groups_degree_bf_weekend %>%
  replace(is.na(.), 0) %>%
  group_by(degree) %>%
  summarize_all(sum) 

chi_squared_result <- chisq.test(groups_degree_bf_weekend[, c("degree", "0-2 days", "3+ days")])
print(chi_squared_result)

melted_data <- tidyr::gather(groups_degree_bf_weekend, key = "Breakfast_Group", value = "Percentage", -degree)
melted_data$degree <- factor(melted_data$degree, labels = c("BSc", "MSc"))
ggplot(melted_data, aes(x = degree, y = Percentage, fill = Breakfast_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of Breakfast Groups by degree (Weekend)",
    x = "degree",
    y = "Percentage",
    fill = "Breakfast Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# analisi dieta lunch
# ---------------------------------------------------------------------------------------------
calculate_diet_patterns <- function(data) {
  data %>%
    group_by(id) %>%
    summarize(
      frt_vgt = n_distinct(week_new[d1 > 0]),
      brd_pst_rc_pt = n_distinct(week_new[d2 > 0]),
      jnkf = n_distinct(week_new[d3 > 0]),
      mlk_ygrt_chs = n_distinct(week_new[d4 > 0]),
      mt_fsh_eg_lgm = n_distinct(week_new[d5 > 0]),
      swt = n_distinct(week_new[d6 > 0])
    )
}

diet_patterns_lunch_firstweek_weekdays <- calculate_diet_patterns(food_df_lunch_firstweek_weekdays)
diet_patterns_lunch_firstweek_weekend <- calculate_diet_patterns(food_df_lunch_firstweek_weekend)
diet_patterns_lunch_secondweek_weekdays <- calculate_diet_patterns(food_df_lunch_secondweek_weekdays)
diet_patterns_lunch_secondweek_weekend <- calculate_diet_patterns(food_df_lunch_secondweek_weekend)
diet_patterns_lunch_weekdays <- calculate_diet_patterns(food_df_lunch_weekdays)
diet_patterns_lunch_weekend <- calculate_diet_patterns(food_df_lunch_weekend)


generate_food_category_plot <- function(data, food_categories, title) {
  results <- data.frame()
  for (food_category in food_categories) {
    categorized_data <- data %>%
      filter(id %in% selected_ids2) %>%
      mutate(category = case_when(
        .data[[food_category]] == 0 ~ "0 days",
        .data[[food_category]] %in% 1:2 ~ "1-2 days",
        .data[[food_category]] >= 3 ~ "3+ days"
      )) %>%
      group_by(category) %>%
      summarise(count = n()) %>%
      mutate(percentage = (count / sum(count)) * 100) %>%
      mutate(food_category = food_category)
    
    results <- bind_rows(results, categorized_data)
  }
  
  ggplot(results, aes(x = food_category, y = percentage, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = 0.7), position = position_dodge(width = 0.8) , size = 3) +
    labs(
      title = title,
      x = "Food Category",
      y = "Percentage"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
}

plot1 <- generate_food_category_plot(diet_patterns_lunch_firstweek_weekdays, food_categories, "Percentage of People by Food Category (Lunch, First Week, Weekdays)")
plot2 <- generate_food_category_plot(diet_patterns_lunch_firstweek_weekend, food_categories, "Percentage of People by Food Category (Lunch, First Week, Weekend)")
plot3 <- generate_food_category_plot(diet_patterns_lunch_secondweek_weekdays, food_categories, "Percentage of People by Food Category (Lunch, Second Week, Weekdays)")
plot4 <- generate_food_category_plot(diet_patterns_lunch_secondweek_weekend, food_categories, "Percentage of People by Food Category (Lunch, Second Week, Weekend)")

generate_food_category_plot <- function(data, food_categories, title, wd= TRUE) {
  results <- data.frame()
  if(wd==TRUE){
    for (food_category in food_categories) {
      categorized_data <- data %>%
        filter(id %in% selected_ids2) %>%
        mutate(category = case_when(
          .data[[food_category]] %in% 0 ~ "0 days",
          .data[[food_category]] %in% 1:4 ~ "1-4 days",
          .data[[food_category]] >= 5 ~ "5+ days"
        )) %>%
        group_by(category) %>%
        summarise(count = n()) %>%
        mutate(percentage = (count / sum(count)) * 100) %>%
        mutate(food_category = food_category)
      
      results <- bind_rows(results, categorized_data)
    }
  }else{
    for (food_category in food_categories) {
      categorized_data <- data %>%
        filter(id %in% selected_ids2) %>%
        mutate(category = case_when(
          .data[[food_category]] %in% 0 ~ "0 days",
          .data[[food_category]] %in% 1:2 ~ "1-2 days",
          .data[[food_category]] >= 3 ~ "3+ days"
        )) %>%
        group_by(category) %>%
        summarise(count = n()) %>%
        mutate(percentage = (count / sum(count)) * 100) %>%
        mutate(food_category = food_category)
      
      results <- bind_rows(results, categorized_data)
    }}
  
  ggplot(results, aes(x = food_category, y = percentage, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = 0.7), position = position_dodge(width = 0.8) , size = 3) +
    labs(
      title = title,
      x = "Food Category",
      y = "Percentage"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
}
plot5 <- generate_food_category_plot(diet_patterns_lunch_weekdays, food_categories, "Percentage of People by Food Category (lunch, Weekdays)")
plot6 <- generate_food_category_plot(diet_patterns_lunch_weekend, food_categories, "Percentage of People by Food Category (lunch, Weekend)",wd=F)

grid.arrange(plot1, plot3, ncol = 1)
grid.arrange(plot2, plot4, ncol = 1)
grid.arrange(plot5, plot6, ncol = 1)

tdita_data4diary_lc_analysis_weekdays <- tdita_data4diary %>%
  left_join(lunch_counts_weekdays, by = c("id" = "id")) %>%
  distinct(id, .keep_all = TRUE) %>% select(id, department, w1_A01, degree,lunch_group)

groups_gender_lc_weekdays <- tdita_data4diary_lc_analysis_weekdays %>%
  filter(!is.na(w1_A01))%>%
  group_by(w1_A01, lunch_group) %>%
  summarise(count = n()) %>%
  group_by(w1_A01) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = lunch_group, values_from = percentage)

groups_gender_lc_weekdays <- groups_gender_lc_weekdays %>%
  select(w1_A01, "0-4 days", "5-8 days",  "9+ days")

groups_gender_lc_weekdays <- groups_gender_lc_weekdays %>%
  replace(is.na(.), 0) %>%
  group_by(w1_A01) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_gender_lc_weekdays[, c("w1_A01", "0-4 days", "5-8 days", "9+ days")])
print(chi_squared_result)

melted_data <- tidyr::gather(groups_gender_lc_weekdays, key = "lunch_Group", value = "Percentage", -w1_A01)
melted_data$w1_A01 <- factor(melted_data$w1_A01, labels = c("Male", "Female"))
ggplot(melted_data, aes(x = w1_A01, y = Percentage, fill = lunch_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of lunch Groups by Gender (Weekdays)",
    x = "Gender",
    y = "Percentage",
    fill = "lunch Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


groups_department_lc_weekdays <- tdita_data4diary_lc_analysis_weekdays %>%
  filter(!is.na(department))%>%
  group_by(department, lunch_group) %>%
  summarise(count = n()) %>%
  group_by(department) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = lunch_group, values_from = percentage)

groups_department_lc_weekdays <- groups_department_lc_weekdays %>%
  select(department, "0-4 days", "5-8 days",  "9+ days")

groups_department_lc_weekdays <- groups_department_lc_weekdays %>%
  replace(is.na(.), 0) %>%
  group_by(department) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_department_lc_weekdays[, c("department", "0-4 days", "5-8 days", "9+ days")])
print(chi_squared_result)

melted_data_department <- tidyr::gather(groups_department_lc_weekdays, key = "lunch_Group", value = "Percentage", -department)
department_names <- c(
  "1" = "Business/Economics",
  "2" = "International Relations and Public Admin",
  "3"= "Law",
  "4" = "Engineering and Applied Sciences",
  "5" = "Social Sciences",
  "6" = "Natural Sciences",
  "7" = "Humanities",
  "9" = "Medicine and Veterinary Medicine"
)

ggplot(melted_data_department, aes(x = factor(department, levels = names(department_names)), y = Percentage, fill = lunch_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = 0.5, size = 2) +
  labs(
    title = "Distribution of lunch Groups by Department (Weekdays)",
    x = "Department",
    y = "Percentage",
    fill = "lunch Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels = department_names) +  
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


groups_degree_lc_weekdays <- tdita_data4diary_lc_analysis_weekdays %>%
  filter(!is.na(degree))%>%
  group_by(degree, lunch_group) %>%
  summarise(count = n()) %>%
  group_by(degree) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = lunch_group, values_from = percentage)

groups_degree_lc_weekdays <- groups_degree_lc_weekdays %>%
  select(degree, "0-4 days", "5-8 days",  "9+ days")

groups_degree_lc_weekdays <- groups_degree_lc_weekdays %>%
  replace(is.na(.), 0) %>%
  group_by(degree) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_degree_lc_weekdays[, c("degree", "0-4 days", "5-8 days", "9+ days")])
print(chi_squared_result)

melted_data <- tidyr::gather(groups_degree_lc_weekdays, key = "lunch_Group", value = "Percentage", -degree)
melted_data$degree <- factor(melted_data$degree, labels = c("BSc", "MSc"))
ggplot(melted_data, aes(x = degree, y = Percentage, fill = lunch_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of lunch Groups by degree (Weekdays)",
    x = "degree",
    y = "Percentage",
    fill = "lunch Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

tdita_data4diary_lc_analysis_weekend <- tdita_data4diary %>%
  left_join(lunch_counts_weekend, by = c("id" = "id")) %>%
  distinct(id, .keep_all = TRUE) %>% select(id, department, w1_A01, degree,lunch_group)

groups_gender_lc_weekend <- tdita_data4diary_lc_analysis_weekend %>%
  filter(!is.na(w1_A01))%>%
  group_by(w1_A01, lunch_group) %>%
  summarise(count = n()) %>%
  group_by(w1_A01) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = lunch_group, values_from = percentage)

groups_gender_lc_weekend <- groups_gender_lc_weekend %>%
  select(w1_A01, "0-2 days", "3+ days")

groups_gender_lc_weekend <- groups_gender_lc_weekend %>%
  replace(is.na(.), 0) %>%
  group_by(w1_A01) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_gender_lc_weekend[, c("w1_A01", "0-2 days", "3+ days")])
print(chi_squared_result)

melted_data <- tidyr::gather(groups_gender_lc_weekend, key = "lunch_Group", value = "Percentage", -w1_A01)
melted_data$w1_A01 <- factor(melted_data$w1_A01, labels = c("Male", "Female"))
ggplot(melted_data, aes(x = w1_A01, y = Percentage, fill = lunch_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of lunch Groups by Gender (Weekend)",
    x = "Gender",
    y = "Percentage",
    fill = "lunch Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


groups_department_lc_weekend <- tdita_data4diary_lc_analysis_weekend %>%
  filter(!is.na(department))%>%
  group_by(department, lunch_group) %>%
  summarise(count = n()) %>%
  group_by(department) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = lunch_group, values_from = percentage)

groups_department_lc_weekend <- groups_department_lc_weekend %>%
  select(department, "0-2 days", "3+ days")

groups_department_lc_weekend <- groups_department_lc_weekend %>%
  replace(is.na(.), 0) %>%
  group_by(department) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_department_lc_weekend[, c("department", "0-2 days", "3+ days")])
print(chi_squared_result)

melted_data_department <- tidyr::gather(groups_department_lc_weekend, key = "lunch_Group", value = "Percentage", -department)
department_names <- c(
  "1" = "Business/Economics",
  "2" = "International Relations and Public Admin",
  "3"= "Law",
  "4" = "Engineering and Applied Sciences",
  "5" = "Social Sciences",
  "6" = "Natural Sciences",
  "7" = "Humanities",
  "9" = "Medicine and Veterinary Medicine"
)

ggplot(melted_data_department, aes(x = factor(department, levels = names(department_names)), y = Percentage, fill = lunch_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = 0.5, size = 2) +
  labs(
    title = "Distribution of lunch Groups by Department (Weekend)",
    x = "Department",
    y = "Percentage",
    fill = "lunch Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels = department_names) +  
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


groups_degree_lc_weekend <- tdita_data4diary_lc_analysis_weekend %>%
  filter(!is.na(degree))%>%
  group_by(degree, lunch_group) %>%
  summarise(count = n()) %>%
  group_by(degree) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = lunch_group, values_from = percentage)

groups_degree_lc_weekend <- groups_degree_lc_weekend %>%
  select(degree, "0-2 days", "3+ days")

groups_degree_lc_weekend <- groups_degree_lc_weekend %>%
  replace(is.na(.), 0) %>%
  group_by(degree) %>%
  summarize_all(sum) 

chi_squared_result <- chisq.test(groups_degree_lc_weekend[, c("degree", "0-2 days", "3+ days")])
print(chi_squared_result)

melted_data <- tidyr::gather(groups_degree_lc_weekend, key = "lunch_Group", value = "Percentage", -degree)
melted_data$degree <- factor(melted_data$degree, labels = c("BSc", "MSc"))
ggplot(melted_data, aes(x = degree, y = Percentage, fill = lunch_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of lunch Groups by degree (Weekend)",
    x = "degree",
    y = "Percentage",
    fill = "lunch Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# analisi dieta dinner
# ---------------------------------------------------------------------------------------------
calculate_diet_patterns <- function(data) {
  data %>%
    group_by(id) %>%
    summarize(
      frt_vgt = n_distinct(week_new[d1 > 0]),
      brd_pst_rc_pt = n_distinct(week_new[d2 > 0]),
      jnkf = n_distinct(week_new[d3 > 0]),
      mlk_ygrt_chs = n_distinct(week_new[d4 > 0]),
      mt_fsh_eg_lgm = n_distinct(week_new[d5 > 0]),
      swt = n_distinct(week_new[d6 > 0])
    )
}

diet_patterns_dinner_firstweek_weekdays <- calculate_diet_patterns(food_df_dinner_firstweek_weekdays)
diet_patterns_dinner_firstweek_weekend <- calculate_diet_patterns(food_df_dinner_firstweek_weekend)
diet_patterns_dinner_secondweek_weekdays <- calculate_diet_patterns(food_df_dinner_secondweek_weekdays)
diet_patterns_dinner_secondweek_weekend <- calculate_diet_patterns(food_df_dinner_secondweek_weekend)
diet_patterns_dinner_weekdays <- calculate_diet_patterns(food_df_dinner_weekdays)
diet_patterns_dinner_weekend <- calculate_diet_patterns(food_df_dinner_weekend)

generate_food_category_plot <- function(data, food_categories, title) {
  results <- data.frame()
  for (food_category in food_categories) {
    categorized_data <- data %>%
      filter(id %in% selected_ids2) %>%
      mutate(category = case_when(
        .data[[food_category]] == 0 ~ "0 days",
        .data[[food_category]] %in% 1:2 ~ "1-2 days",
        .data[[food_category]] >= 3 ~ "3+ days"
      )) %>%
      group_by(category) %>%
      summarise(count = n()) %>%
      mutate(percentage = (count / sum(count)) * 100) %>%
      mutate(food_category = food_category)
    
    results <- bind_rows(results, categorized_data)
  }
  
  ggplot(results, aes(x = food_category, y = percentage, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = 0.7), position = position_dodge(width = 0.8) , size = 3) +
    labs(
      title = title,
      x = "Food Category",
      y = "Percentage"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
}

plot1 <- generate_food_category_plot(diet_patterns_dinner_firstweek_weekdays, food_categories, "Percentage of People by Food Category (Dinner, First Week, Weekdays)")
plot2 <- generate_food_category_plot(diet_patterns_dinner_firstweek_weekend, food_categories, "Percentage of People by Food Category (Dinner, First Week, Weekend)")
plot3 <- generate_food_category_plot(diet_patterns_dinner_secondweek_weekdays, food_categories, "Percentage of People by Food Category (Dinner, Second Week, Weekdays)")
plot4 <- generate_food_category_plot(diet_patterns_dinner_secondweek_weekend, food_categories, "Percentage of People by Food Category (Dinner, Second Week, Weekend)")

generate_food_category_plot <- function(data, food_categories, title, wd= TRUE) {
  results <- data.frame()
  if(wd==TRUE){
    for (food_category in food_categories) {
      categorized_data <- data %>%
        filter(id %in% selected_ids2) %>%
        mutate(category = case_when(
          .data[[food_category]] %in% 0 ~ "0 days",
          .data[[food_category]] %in% 1:4 ~ "1-4 days",
          .data[[food_category]] >= 5 ~ "5+ days"
        )) %>%
        group_by(category) %>%
        summarise(count = n()) %>%
        mutate(percentage = (count / sum(count)) * 100) %>%
        mutate(food_category = food_category)
      
      results <- bind_rows(results, categorized_data)
    }
  }else{
    for (food_category in food_categories) {
      categorized_data <- data %>%
        filter(id %in% selected_ids2) %>%
        mutate(category = case_when(
          .data[[food_category]] %in% 0 ~ "0 days",
          .data[[food_category]] %in% 1:2 ~ "1-2 days",
          .data[[food_category]] >= 3 ~ "3+ days"
        )) %>%
        group_by(category) %>%
        summarise(count = n()) %>%
        mutate(percentage = (count / sum(count)) * 100) %>%
        mutate(food_category = food_category)
      
      results <- bind_rows(results, categorized_data)
    }}
  
  ggplot(results, aes(x = food_category, y = percentage, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = 0.7), position = position_dodge(width = 0.8) , size = 3) +
    labs(
      title = title,
      x = "Food Category",
      y = "Percentage"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
}
plot5 <- generate_food_category_plot(diet_patterns_dinner_weekdays, food_categories, "Percentage of People by Food Category (dinner, Weekdays)")
plot6 <- generate_food_category_plot(diet_patterns_dinner_weekend, food_categories, "Percentage of People by Food Category (dinner, Weekend)", wd=F)

grid.arrange(plot1, plot3, ncol = 1)
grid.arrange(plot2, plot4, ncol = 1)
grid.arrange(plot5, plot6, ncol = 1)

tdita_data4diary_dn_analysis_weekdays <- tdita_data4diary %>%
  left_join(dinner_counts_weekdays, by = c("id" = "id")) %>%
  distinct(id, .keep_all = TRUE) %>% select(id, department, w1_A01, degree, dinner_group)

groups_gender_dn_weekdays <- tdita_data4diary_dn_analysis_weekdays %>%
  filter(!is.na(w1_A01))%>%
  group_by(w1_A01, dinner_group) %>%
  summarise(count = n()) %>%
  group_by(w1_A01) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = dinner_group, values_from = percentage)

groups_gender_dn_weekdays <- groups_gender_dn_weekdays %>%
  select(w1_A01, "0-4 days", "5-8 days",  "9+ days")

groups_gender_dn_weekdays <- groups_gender_dn_weekdays %>%
  replace(is.na(.), 0) %>%
  group_by(w1_A01) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_gender_dn_weekdays[, c("w1_A01", "0-4 days", "5-8 days", "9+ days")])
print(chi_squared_result)

melted_data <- tidyr::gather(groups_gender_dn_weekdays, key = "dinner_Group", value = "Percentage", -w1_A01)
melted_data$w1_A01 <- factor(melted_data$w1_A01, labels = c("Male", "Female"))
ggplot(melted_data, aes(x = w1_A01, y = Percentage, fill = dinner_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of dinner Groups by Gender (Weekdays)",
    x = "Gender",
    y = "Percentage",
    fill = "dinner Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


groups_department_dn_weekdays <- tdita_data4diary_dn_analysis_weekdays %>%
  filter(!is.na(department))%>%
  group_by(department, dinner_group) %>%
  summarise(count = n()) %>%
  group_by(department) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = dinner_group, values_from = percentage)

groups_department_dn_weekdays <- groups_department_dn_weekdays %>%
  select(department, "0-4 days", "5-8 days",  "9+ days")

groups_department_dn_weekdays <- groups_department_dn_weekdays %>%
  replace(is.na(.), 0) %>%
  group_by(department) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_department_dn_weekdays[, c("department", "0-4 days", "5-8 days", "9+ days")])
print(chi_squared_result)

melted_data_department <- tidyr::gather(groups_department_dn_weekdays, key = "dinner_Group", value = "Percentage", -department)
department_names <- c(
  "1" = "Business/Economics",
  "2" = "International Relations and Public Admin",
  "3"= "Law",
  "4" = "Engineering and Applied Sciences",
  "5" = "Social Sciences",
  "6" = "Natural Sciences",
  "7" = "Humanities",
  "9" = "Medicine and Veterinary Medicine"
)

ggplot(melted_data_department, aes(x = factor(department, levels = names(department_names)), y = Percentage, fill = dinner_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = 0.5, size = 2) +
  labs(
    title = "Distribution of dinner Groups by Department (Weekdays)",
    x = "Department",
    y = "Percentage",
    fill = "dinner Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels = department_names) +  
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

groups_degree_dn_weekdays <- tdita_data4diary_dn_analysis_weekdays %>%
  filter(!is.na(degree)) %>%
  group_by(degree, dinner_group) %>%
  summarise(count = n()) %>%
  group_by(degree) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = dinner_group, values_from = percentage)

groups_degree_dn_weekdays <- groups_degree_dn_weekdays %>%
  select(degree, "0-4 days", "5-8 days",  "9+ days")

groups_degree_dn_weekdays <- groups_degree_dn_weekdays %>%
  replace(is.na(.), 0) %>%
  group_by(degree) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_degree_dn_weekdays[, c("degree", "0-4 days", "5-8 days", "9+ days")])
print(chi_squared_result)

melted_data <- tidyr::gather(groups_degree_dn_weekdays, key = "dinner_Group", value = "Percentage", -degree)
melted_data$degree <- factor(melted_data$degree, labels = c("BSc", "MSc"))
ggplot(melted_data, aes(x = degree, y = Percentage, fill = dinner_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of dinner Groups by degree (Weekdays)",
    x = "degree",
    y = "Percentage",
    fill = "dinner Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

tdita_data4diary_dn_analysis_weekend <- tdita_data4diary %>%
  left_join(dinner_counts_weekend, by = c("id" = "id")) %>%
  distinct(id, .keep_all = TRUE) %>% select(id, department,degree, w1_A01, dinner_group)

groups_gender_dn_weekend <- tdita_data4diary_dn_analysis_weekend %>%
  filter(!is.na(w1_A01))%>%
  group_by(w1_A01, dinner_group) %>%
  summarise(count = n()) %>%
  group_by(w1_A01) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = dinner_group, values_from = percentage)

groups_gender_dn_weekend <- groups_gender_dn_weekend %>%
  select(w1_A01, "0-2 days", "3+ days")

groups_gender_dn_weekend <- groups_gender_dn_weekend %>%
  replace(is.na(.), 0) %>%
  group_by(w1_A01) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_gender_dn_weekend[, c("w1_A01", "0-2 days", "3+ days")])
print(chi_squared_result)

melted_data <- tidyr::gather(groups_gender_dn_weekend, key = "dinner_Group", value = "Percentage", -w1_A01)
melted_data$w1_A01 <- factor(melted_data$w1_A01, labels = c("Male", "Female"))
ggplot(melted_data, aes(x = w1_A01, y = Percentage, fill = dinner_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of dinner Groups by Gender (Weekend)",
    x = "Gender",
    y = "Percentage",
    fill = "dinner Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

groups_department_dn_weekend <- tdita_data4diary_dn_analysis_weekend %>%
  filter(!is.na(department))%>%
  group_by(department, dinner_group) %>%
  summarise(count = n()) %>%
  group_by(department) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = dinner_group, values_from = percentage)

groups_department_dn_weekend <- groups_department_dn_weekend %>%
  select(department, "0-2 days", "3+ days")

groups_department_dn_weekend <- groups_department_dn_weekend %>%
  replace(is.na(.), 0) %>%
  group_by(department) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_department_dn_weekend[, c("department", "0-2 days", "3+ days")])
print(chi_squared_result)

melted_data_department <- tidyr::gather(groups_department_dn_weekend, key = "dinner_Group", value = "Percentage", -department)
department_names <- c(
  "1" = "Business/Economics",
  "2" = "International Relations and Public Admin",
  "3"= "Law",
  "4" = "Engineering and Applied Sciences",
  "5" = "Social Sciences",
  "6" = "Natural Sciences",
  "7" = "Humanities",
  "9" = "Medicine and Veterinary Medicine"
)

ggplot(melted_data_department, aes(x = factor(department, levels = names(department_names)), y = Percentage, fill = dinner_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = 0.5, size = 2) +
  labs(
    title = "Distribution of dinner Groups by Department (Weekend)",
    x = "Department",
    y = "Percentage",
    fill = "dinner Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels = department_names) +  
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


groups_degree_dn_weekend <- tdita_data4diary_dn_analysis_weekend %>%
  filter(!is.na(degree)) %>%
  group_by(degree, dinner_group) %>%
  summarise(count = n()) %>%
  group_by(degree) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = dinner_group, values_from = percentage)

groups_degree_dn_weekend <- groups_degree_dn_weekend %>%
  select(degree, "0-2 days", "3+ days")

groups_degree_dn_weekend <- groups_degree_dn_weekend %>%
  replace(is.na(.), 0) %>%
  group_by(degree) %>%
  summarize_all(sum) 

chi_squared_result <- chisq.test(groups_degree_dn_weekend[, c("degree", "0-2 days", "3+ days")])
print(chi_squared_result)

melted_data <- tidyr::gather(groups_degree_dn_weekend, key = "dinner_Group", value = "Percentage", -degree)
melted_data$degree <- factor(melted_data$degree, labels = c("BSc", "MSc"))
ggplot(melted_data, aes(x = degree, y = Percentage, fill = dinner_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of dinner Groups by degree (Weekend)",
    x = "degree",
    y = "Percentage",
    fill = "dinner Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# analisi dieta snack
# ---------------------------------------------------------------------------------------------
calculate_diet_patterns <- function(data) {
  data %>%
    filter(d1 != 198 | d2 != 297 | d3 != 297 | d4 != 198 | d5 != 297 | d6 != 396 ) %>%
    group_by(id) %>%
    summarize(
      frt_vgt = n_distinct(week_new[d1 > 0]),
      brd_pst_rc_pt = n_distinct(week_new[d2 > 0]),
      jnkf = n_distinct(week_new[d3 > 0]),
      mlk_ygrt_chs = n_distinct(week_new[d4 > 0]),
      mt_fsh_eg_lgm = n_distinct(week_new[d5 > 0]),
      swt = n_distinct(week_new[d6 > 0])
    )
}

diet_patterns_snack_firstweek_weekdays <- calculate_diet_patterns(food_df_snack_firstweek_weekdays)
diet_patterns_snack_firstweek_weekend <- calculate_diet_patterns(food_df_snack_firstweek_weekend)
diet_patterns_snack_secondweek_weekdays <- calculate_diet_patterns(food_df_snack_secondweek_weekdays)
diet_patterns_snack_secondweek_weekend <- calculate_diet_patterns(food_df_snack_secondweek_weekend)
diet_patterns_snack_weekdays <- calculate_diet_patterns(food_df_snack_weekdays)
diet_patterns_snack_weekend <- calculate_diet_patterns(food_df_snack_weekend)


generate_food_category_plot <- function(data, food_categories, title) {
  results <- data.frame()
  for (food_category in food_categories) {
    categorized_data <- data %>%
      filter(id %in% selected_ids2) %>%
      mutate(category = case_when(
        .data[[food_category]] == 0 ~ "0 days",
        .data[[food_category]] %in% 1:2 ~ "1-2 days",
        .data[[food_category]] >= 3 ~ "3+ days"
      )) %>%
      group_by(category) %>%
      summarise(count = n()) %>%
      mutate(percentage = (count / sum(count)) * 100) %>%
      mutate(food_category = food_category)
    
    results <- bind_rows(results, categorized_data)
  }
  
  ggplot(results, aes(x = food_category, y = percentage, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = 0.7), position = position_dodge(width = 0.8) , size = 3) +
    labs(
      title = title,
      x = "Food Category",
      y = "Percentage"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
}

plot1 <- generate_food_category_plot(diet_patterns_snack_firstweek_weekdays, food_categories, "Percentage of People by Food Category (Snack, First Week, Weekdays)")
plot2 <- generate_food_category_plot(diet_patterns_snack_firstweek_weekend, food_categories, "Percentage of People by Food Category (Snack, First Week, Weekend)")
plot3 <- generate_food_category_plot(diet_patterns_snack_secondweek_weekdays, food_categories, "Percentage of People by Food Category (Snack, Second Week, Weekdays)")
plot4 <- generate_food_category_plot(diet_patterns_snack_secondweek_weekend, food_categories, "Percentage of People by Food Category (Snack, Second Week, Weekend)")

generate_food_category_plot <- function(data, food_categories, title, wd= TRUE) {
  results <- data.frame()
  if(wd==TRUE){
    for (food_category in food_categories) {
      categorized_data <- data %>%
        filter(id %in% selected_ids2) %>%
        mutate(category = case_when(
          .data[[food_category]] %in% 0 ~ "0 days",
          .data[[food_category]] %in% 1:4 ~ "1-4 days",
          .data[[food_category]] >= 5 ~ "5+ days"
        )) %>%
        group_by(category) %>%
        summarise(count = n()) %>%
        mutate(percentage = (count / sum(count)) * 100) %>%
        mutate(food_category = food_category)
      
      results <- bind_rows(results, categorized_data)
    }
  }else{
    for (food_category in food_categories) {
      categorized_data <- data %>%
        filter(id %in% selected_ids2) %>%
        mutate(category = case_when(
          .data[[food_category]] %in% 0 ~ "0 days",
          .data[[food_category]] %in% 1:2 ~ "1-2 days",
          .data[[food_category]] >= 3 ~ "3+ days"
        )) %>%
        group_by(category) %>%
        summarise(count = n()) %>%
        mutate(percentage = (count / sum(count)) * 100) %>%
        mutate(food_category = food_category)
      
      results <- bind_rows(results, categorized_data)
    }}
  
  ggplot(results, aes(x = food_category, y = percentage, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = 0.7), position = position_dodge(width = 0.8) , size = 3) +
    labs(
      title = title,
      x = "Food Category",
      y = "Percentage"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
}
plot5 <- generate_food_category_plot(diet_patterns_snack_weekdays, food_categories, "Percentage of People by Food Category (snack, Weekdays)")
plot6 <- generate_food_category_plot(diet_patterns_snack_weekend, food_categories, "Percentage of People by Food Category (snack, Weekend)", wd= FALSE)

grid.arrange(plot1, plot3, ncol = 1)
grid.arrange(plot2, plot4, ncol = 1)
grid.arrange(plot5, plot6, ncol = 1)

tdita_data4diary_sn_analysis_weekdays <- tdita_data4diary %>%
  left_join(snack_counts_weekdays, by = c("id" = "id")) %>%
  distinct(id, .keep_all = TRUE) %>% select(id, department,degree, w1_A01, snack_group)

groups_gender_sn_weekdays <- tdita_data4diary_sn_analysis_weekdays %>%
  filter(!is.na(w1_A01))%>%
  group_by(w1_A01, snack_group) %>%
  summarise(count = n()) %>%
  group_by(w1_A01) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = snack_group, values_from = percentage)

groups_gender_sn_weekdays <- groups_gender_sn_weekdays %>%
  select(w1_A01, "0-4 times", "5-13 times",  "14+ times")

groups_gender_sn_weekdays <- groups_gender_sn_weekdays %>%
  replace(is.na(.), 0) %>%
  group_by(w1_A01) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_gender_sn_weekdays[, c("w1_A01", "0-4 times", "5-13 times", "14+ times")])
print(chi_squared_result)

melted_data <- tidyr::gather(groups_gender_sn_weekdays, key = "snack_Group", value = "Percentage", -w1_A01)
melted_data$w1_A01 <- factor(melted_data$w1_A01, labels = c("Male", "Female"))
ggplot(melted_data, aes(x = w1_A01, y = Percentage, fill = snack_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of snack Groups by Gender (Weekdays)",
    x = "Gender",
    y = "Percentage",
    fill = "snack Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


groups_department_sn_weekdays <- tdita_data4diary_sn_analysis_weekdays %>%
  filter(!is.na(department))%>%
  group_by(department, snack_group) %>%
  summarise(count = n()) %>%
  group_by(department) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = snack_group, values_from = percentage)

groups_department_sn_weekdays <- groups_department_sn_weekdays %>%
  select(department, "0-4 times", "5-13 times",  "14+ times")

groups_department_sn_weekdays <- groups_department_sn_weekdays %>%
  replace(is.na(.), 0) %>%
  group_by(department) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_department_sn_weekdays[, c("department", "0-4 times", "5-13 times", "14+ times")])
print(chi_squared_result)

melted_data_department <- tidyr::gather(groups_department_sn_weekdays, key = "snack_Group", value = "Percentage", -department)
department_names <- c(
  "1" = "Business/Economics",
  "2" = "International Relations and Public Admin",
  "3"= "Law",
  "4" = "Engineering and Applied Sciences",
  "5" = "Social Sciences",
  "6" = "Natural Sciences",
  "7" = "Humanities",
  "9" = "Medicine and Veterinary Medicine"
)

ggplot(melted_data_department, aes(x = factor(department, levels = names(department_names)), y = Percentage, fill = snack_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = 0.5, size = 2) +
  labs(
    title = "Distribution of snack Groups by Department (Weekdays)",
    x = "Department",
    y = "Percentage",
    fill = "snack Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels = department_names) +  
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

groups_degree_sn_weekdays <- tdita_data4diary_sn_analysis_weekdays %>%
  filter(!is.na(degree)) %>%
  group_by(degree, snack_group) %>%
  summarise(count = n()) %>%
  group_by(degree) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = snack_group, values_from = percentage)

groups_degree_sn_weekdays <- groups_degree_sn_weekdays %>%
  select(degree, "0-4 times", "5-13 times",  "14+ times")

groups_degree_sn_weekdays <- groups_degree_sn_weekdays %>%
  replace(is.na(.), 0) %>%
  group_by(degree) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_degree_sn_weekdays[, c("degree", "0-4 times", "5-13 times",  "14+ times")])
print(chi_squared_result)

melted_data <- tidyr::gather(groups_degree_sn_weekdays, key = "snack_Group", value = "Percentage", -degree)
melted_data$degree <- factor(melted_data$degree, labels = c("BSc", "MSc"))
ggplot(melted_data, aes(x = degree, y = Percentage, fill = snack_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of Snack Groups by Degree (Weekdays)",
    x = "Degree",
    y = "Percentage",
    fill = "Snack Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


tdita_data4diary_sn_analysis_weekend <- tdita_data4diary %>%
  left_join(snack_counts_weekend, by = c("id" = "id")) %>%
  distinct(id, .keep_all = TRUE) %>% select(id, department,degree, w1_A01, snack_group)

groups_gender_sn_weekend <- tdita_data4diary_sn_analysis_weekend %>%
  filter(!is.na(w1_A01))%>%
  group_by(w1_A01, snack_group) %>%
  summarise(count = n()) %>%
  group_by(w1_A01) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = snack_group, values_from = percentage)

groups_gender_sn_weekend <- groups_gender_sn_weekend %>%
  select(w1_A01, "0-2 times", "3+ times")

groups_gender_sn_weekend <- groups_gender_sn_weekend %>%
  replace(is.na(.), 0) %>%
  group_by(w1_A01) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_gender_sn_weekend[, c("w1_A01", "0-2 times", "3+ times")])
print(chi_squared_result)

melted_data <- tidyr::gather(groups_gender_sn_weekend, key = "snack_Group", value = "Percentage", -w1_A01)
melted_data$w1_A01 <- factor(melted_data$w1_A01, labels = c("Male", "Female"))
ggplot(melted_data, aes(x = w1_A01, y = Percentage, fill = snack_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of snack Groups by Gender (Weekend)",
    x = "Gender",
    y = "Percentage",
    fill = "snack Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


groups_department_sn_weekend <- tdita_data4diary_sn_analysis_weekend %>%
  filter(!is.na(department))%>%
  group_by(department, snack_group) %>%
  summarise(count = n()) %>%
  group_by(department) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = snack_group, values_from = percentage)

groups_department_sn_weekend <- groups_department_sn_weekend %>%
  select(department, "0-2 times", "3+ times")

groups_department_sn_weekend <- groups_department_sn_weekend %>%
  replace(is.na(.), 0) %>%
  group_by(department) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_department_sn_weekend[, c("department", "0-2 times", "3+ times")])
print(chi_squared_result)

melted_data_department <- tidyr::gather(groups_department_sn_weekend, key = "snack_Group", value = "Percentage", -department)
department_names <- c(
  "1" = "Business/Economics",
  "2" = "International Relations and Public Admin",
  "3"= "Law",
  "4" = "Engineering and Applied Sciences",
  "5" = "Social Sciences",
  "6" = "Natural Sciences",
  "7" = "Humanities",
  "9" = "Medicine and Veterinary Medicine"
)

ggplot(melted_data_department, aes(x = factor(department, levels = names(department_names)), y = Percentage, fill = snack_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = 0.5, size = 2) +
  labs(
    title = "Distribution of snack Groups by Department (Weekend)",
    x = "Department",
    y = "Percentage",
    fill = "snack Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels = department_names) +  
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

groups_degree_sn_weekend <- tdita_data4diary_sn_analysis_weekend %>%
  filter(!is.na(degree)) %>%
  group_by(degree, snack_group) %>%
  summarise(count = n()) %>%
  group_by(degree) %>%
  mutate(percentage = count / sum(count)) %>%
  pivot_wider(names_from = snack_group, values_from = percentage)

groups_degree_sn_weekend <- groups_degree_sn_weekend %>%
  select(degree, "0-2 times", "3+ times")

groups_degree_sn_weekend <- groups_degree_sn_weekend %>%
  replace(is.na(.), 0) %>%
  group_by(degree) %>%
  summarize_all(sum)

chi_squared_result <- chisq.test(groups_degree_sn_weekend[, c("degree", "0-2 times", "3+ times")])
print(chi_squared_result)

melted_data <- tidyr::gather(groups_degree_sn_weekend, key = "snack_Group", value = "Percentage", -degree)
melted_data$degree <- factor(melted_data$degree, labels = c("BSc", "MSc"))
ggplot(melted_data, aes(x = degree, y = Percentage, fill = snack_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage * 100, 1), "%")),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of Snack Groups by Degree (Weekend)",
    x = "Degree",
    y = "Percentage",
    fill = "Snack Groups"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

#analisi su combinazioni di cibi mangiati di più a colazione
#----------------------------------------------------------------------------
generate_combination_plot <- function(df, title, threshold = 2) {
  df_combined <- df %>%
    mutate(
      combination = 
        ifelse(d1 > 0, "frt_vgt", "") %>%
        paste(ifelse(d2 > 0, "brd_pst_rc_pt", "")) %>%
        paste(ifelse(d3 > 0, "jnkf", "")) %>%
        paste(ifelse(d4 > 0, "mlk_ygrt_chs", "")) %>%
        paste(ifelse(d5 > 0, "mt_fsh_eg_lgm", "")) %>%
        paste(ifelse(d6 > 0, "swt", ""))
    )
  
  combination_counts <- df_combined %>%
    group_by(combination) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  
  total_count <- sum(combination_counts$count)
  
  combination_counts <- combination_counts %>%
    mutate(percentage = (count / total_count) * 100)
  
  combination_counts_filtered <- combination_counts %>%
    filter(percentage > threshold)
  
  ggplot(combination_counts_filtered, aes(x = combination, y = percentage)) +
    geom_bar(stat = "identity", fill = "dodgerblue") +
    ggtitle(title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = -0.5), size = 4) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
    theme(legend.position = "none") +  
    theme(plot.title = element_text(hjust = 0.5, size = 16)) 
}

plot1 <- generate_combination_plot(food_df_breakfast_firstweek_weekdays, "Combination of Food (Breakfast - 1st Week - Weekdays)")
plot2 <- generate_combination_plot(food_df_breakfast_firstweek_weekend, "Combination of Food (Breakfast - 1st Week - Weekend)")
plot3 <- generate_combination_plot(food_df_breakfast_secondweek_weekdays, "Combination of Food (Breakfast - 2nd Week - Weekdays)")
plot4 <- generate_combination_plot(food_df_breakfast_secondweek_weekend, "Combination of Food (Breakfast - 2nd Week - Weekend)")
plot5 <- generate_combination_plot(food_df_breakfast_weekdays, "Combination of Food (Breakfast - Weekdays)")
plot6 <- generate_combination_plot(food_df_breakfast_weekend, "Combination of Food (Breakfast - Weekend)")

grid.arrange(plot1, plot2, ncol=2)
grid.arrange(plot3, plot4, ncol=2)
grid.arrange(plot5, plot6, ncol=2)

#analisi su combinazioni di cibi mangiati di più a pranzo
#----------------------------------------------------------------------------
generate_combination_plot <- function(df, title, threshold = 2) {
  df_combined <- df %>%
    mutate(
      combination = 
        ifelse(d1 > 0, "frt_vgt", "") %>%
        paste(ifelse(d2 > 0, "brd_pst_rc_pt", "")) %>%
        paste(ifelse(d3 > 0, "jnkf", "")) %>%
        paste(ifelse(d4 > 0, "mlk_ygrt_chs", "")) %>%
        paste(ifelse(d5 > 0, "mt_fsh_eg_lgm", "")) %>%
        paste(ifelse(d6 > 0, "swt", ""))
    )
  
  combination_counts <- df_combined %>%
    group_by(combination) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  
  total_count <- sum(combination_counts$count)
  
  combination_counts <- combination_counts %>%
    mutate(percentage = (count / total_count) * 100)
  
  combination_counts_filtered <- combination_counts %>%
    filter(percentage > threshold)
  
  ggplot(combination_counts_filtered, aes(x = combination, y = percentage)) +
    geom_bar(stat = "identity", fill = "dodgerblue") +
    ggtitle(title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = -0.5), size = 4) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
    theme(legend.position = "none") +  
    theme(plot.title = element_text(hjust = 0.5, size = 16))
}

plot1 <- generate_combination_plot(food_df_lunch_firstweek_weekdays, "Combination of Food (Lunch - 1st Week - Weekdays)")
plot2 <- generate_combination_plot(food_df_lunch_firstweek_weekend, "Combination of Food (Lunch - 1st Week - Weekend)")
plot3 <- generate_combination_plot(food_df_lunch_secondweek_weekdays, "Combination of Food (Lunch - 2nd Week - Weekdays)")
plot4 <- generate_combination_plot(food_df_lunch_secondweek_weekend, "Combination of Food (Lunch - 2nd Week - Weekend)")
plot5 <- generate_combination_plot(food_df_lunch_weekdays, "Combination of Food (Lunch - Weekdays)")
plot6 <- generate_combination_plot(food_df_lunch_weekend, "Combination of Food (Lunch - Weekend)")

grid.arrange(plot1, plot2,ncol=2)
grid.arrange(plot3, plot4,ncol=2)
grid.arrange(plot5, plot6,ncol=2)

#analisi su combinazioni di cibi mangiati di più a cena
#----------------------------------------------------------------------------
generate_combination_plot <- function(df, title, threshold = 2) {
  df_combined <- df %>%
    mutate(
      combination = 
        ifelse(d1 > 0, "frt_vgt", "") %>%
        paste(ifelse(d2 > 0, "brd_pst_rc_pt", "")) %>%
        paste(ifelse(d3 > 0, "jnkf", "")) %>%
        paste(ifelse(d4 > 0, "mlk_ygrt_chs", "")) %>%
        paste(ifelse(d5 > 0, "mt_fsh_eg_lgm", "")) %>%
        paste(ifelse(d6 > 0, "swt", ""))
    )
  
  combination_counts <- df_combined %>%
    group_by(combination) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  
  total_count <- sum(combination_counts$count)
  
  combination_counts <- combination_counts %>%
    mutate(percentage = (count / total_count) * 100)
  
  combination_counts_filtered <- combination_counts %>%
    filter(percentage > threshold)
  
  ggplot(combination_counts_filtered, aes(x = combination, y = percentage)) +
    geom_bar(stat = "identity", fill = "dodgerblue") +
    ggtitle(title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = -0.5), size = 4) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
    theme(legend.position = "none") +  
    theme(plot.title = element_text(hjust = 0.5, size = 16))
}

plot1 <- generate_combination_plot(food_df_dinner_firstweek_weekdays, "Combination of Food (Dinner - 1st Week - Weekdays)")
plot2 <- generate_combination_plot(food_df_dinner_firstweek_weekend, "Combination of Food (Dinner - 1st Week - Weekend)")
plot3 <- generate_combination_plot(food_df_dinner_secondweek_weekdays, "Combination of Food (Dinner - 2nd Week - Weekdays)")
plot4 <- generate_combination_plot(food_df_dinner_secondweek_weekend, "Combination of Food (Dinner - 2nd Week - Weekend)")
plot5 <- generate_combination_plot(food_df_dinner_weekdays, "Combination of Food (Dinner - Weekdays)")
plot6 <- generate_combination_plot(food_df_dinner_weekend, "Combination of Food (Dinner - Weekend)")

grid.arrange(plot1, plot2,ncol=2)
grid.arrange(plot3, plot4,ncol=2)
grid.arrange(plot5, plot6,ncol=2)

#analisi su combinazioni di cibi mangiati di più come snack
#----------------------------------------------------------------------------
generate_combination_plot <- function(df, title, threshold = 2) {
  df_combined <- df %>%
    filter(d1 != 198 | d2 != 297 | d3 != 297 | d4 != 198 | d5 != 297 | d6 != 396 ) %>%
    mutate(
      combination = 
        ifelse(d1 > 0, "frt_vgt", "") %>%
        paste(ifelse(d2 > 0, "brd_pst_rc_pt", "")) %>%
        paste(ifelse(d3 > 0, "jnkf", "")) %>%
        paste(ifelse(d4 > 0, "mlk_ygrt_chs", "")) %>%
        paste(ifelse(d5 > 0, "mt_fsh_eg_lgm", "")) %>%
        paste(ifelse(d6 > 0, "swt", ""))
    )
  
  combination_counts <- df_combined %>%
    group_by(combination) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  
  total_count <- sum(combination_counts$count)
  
  combination_counts <- combination_counts %>%
    mutate(percentage = (count / total_count) * 100)
  
  combination_counts_filtered <- combination_counts %>%
    filter(percentage > threshold)
  
  ggplot(combination_counts_filtered, aes(x = combination, y = percentage)) +
    geom_bar(stat = "identity", fill = "dodgerblue") +
    ggtitle(title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = -0.5), size = 4) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
    theme(legend.position = "none") +  
    theme(plot.title = element_text(hjust = 0.5, size = 16))
}

plot1 <- generate_combination_plot(food_df_snack_firstweek_weekdays, "Combination of Food (Snack - 1st Week - Weekdays)")
plot2 <- generate_combination_plot(food_df_snack_firstweek_weekend, "Combination of Food (Snack - 1st Week - Weekend)")
plot3 <- generate_combination_plot(food_df_snack_secondweek_weekdays, "Combination of Food (Snack - 2nd Week - Weekdays)")
plot4 <- generate_combination_plot(food_df_snack_secondweek_weekend, "Combination of Food (Snack - 2nd Week - Weekend)")
plot5 <- generate_combination_plot(food_df_snack_weekdays, "Combination of Food (Snack - Weekdays)")
plot6 <- generate_combination_plot(food_df_snack_weekend, "Combination of Food (Snack - Weekend)")

grid.arrange(plot1, plot2,ncol=2)
grid.arrange(plot3, plot4,ncol=2)
grid.arrange(plot5, plot6,ncol=2)

#analisi su association rules (breakfast)
#---------------------------------------------------------------------------
food_df_breakfast_weekdays_ar<- food_df_breakfast_weekdays %>%
                                select(id,d1,d2,d3,d4,d5,d6)%>%
                                mutate(d1 = ifelse(d1 > 0, 1, 0),
                                       d2 = ifelse(d2 > 0, 1, 0),
                                       d3 = ifelse(d3 > 0, 1, 0),
                                       d4 = ifelse(d4 > 0, 1, 0),
                                       d5 = ifelse(d5 > 0, 1, 0),
                                       d6 = ifelse(d6 > 0, 1, 0))


transactions_matrix <- as.matrix(food_df_breakfast_weekdays_ar[, -1])
transactions <- as(transactions_matrix, "transactions")
itemLabels(transactions) <- c("Fruit_Vegetable", "Bread_Pasta_Rice_Potato", "Junk_Food", "Milk_Yogurt_Cheese", "Meat_Fish_Eggs_Legumes", "Sweets")
rules <- apriori(transactions, supp = 0.01, conf = 0.7, target = "rules")
plot(rules, method = "graph", engine = "htmlwidget")


food_df_breakfast_weekend_ar<- food_df_breakfast_weekend %>%
  select(id,d1,d2,d3,d4,d5,d6)%>%
  mutate(d1 = ifelse(d1 > 0, 1, 0),
         d2 = ifelse(d2 > 0, 1, 0),
         d3 = ifelse(d3 > 0, 1, 0),
         d4 = ifelse(d4 > 0, 1, 0),
         d5 = ifelse(d5 > 0, 1, 0),
         d6 = ifelse(d6 > 0, 1, 0))


transactions_matrix <- as.matrix(food_df_breakfast_weekend_ar[, -1])
transactions <- as(transactions_matrix, "transactions")
itemLabels(transactions) <- c("Fruit_Vegetable", "Bread_Pasta_Rice_Potato", "Junk_Food", "Milk_Yogurt_Cheese", "Meat_Fish_Eggs_Legumes", "Sweets")
rules <- apriori(transactions, supp = 0.01, conf = 0.7, target = "rules")
plot(rules, method = "graph", engine = "htmlwidget")


#analisi su association rules (lunch)
#---------------------------------------------------------------------------
food_df_lunch_weekdays_ar<- food_df_lunch_weekdays %>%
  select(id,d1,d2,d3,d4,d5,d6)%>%
  mutate(d1 = ifelse(d1 > 0, 1, 0),
         d2 = ifelse(d2 > 0, 1, 0),
         d3 = ifelse(d3 > 0, 1, 0),
         d4 = ifelse(d4 > 0, 1, 0),
         d5 = ifelse(d5 > 0, 1, 0),
         d6 = ifelse(d6 > 0, 1, 0))


transactions_matrix <- as.matrix(food_df_lunch_weekdays_ar[, -1])
transactions <- as(transactions_matrix, "transactions")
itemLabels(transactions) <- c("Fruit_Vegetable", "Bread_Pasta_Rice_Potato", "Junk_Food", "Milk_Yogurt_Cheese", "Meat_Fish_Eggs_Legumes", "Sweets")
rules <- apriori(transactions, supp = 0.01, conf = 0.7, target = "rules")
plot(rules, method = "graph", engine = "htmlwidget")


food_df_lunch_weekend_ar<- food_df_lunch_weekend %>%
  select(id,d1,d2,d3,d4,d5,d6)%>%
  mutate(d1 = ifelse(d1 > 0, 1, 0),
         d2 = ifelse(d2 > 0, 1, 0),
         d3 = ifelse(d3 > 0, 1, 0),
         d4 = ifelse(d4 > 0, 1, 0),
         d5 = ifelse(d5 > 0, 1, 0),
         d6 = ifelse(d6 > 0, 1, 0))


transactions_matrix <- as.matrix(food_df_lunch_weekend_ar[, -1])
transactions <- as(transactions_matrix, "transactions")
itemLabels(transactions) <- c("Fruit_Vegetable", "Bread_Pasta_Rice_Potato", "Junk_Food", "Milk_Yogurt_Cheese", "Meat_Fish_Eggs_Legumes", "Sweets")
rules <- apriori(transactions, supp = 0.01, conf = 0.7, target = "rules")
plot(rules, method = "graph", engine = "htmlwidget")


#analisi su association rules (dinner)
#---------------------------------------------------------------------------
food_df_dinner_weekdays_ar<- food_df_dinner_weekdays %>%
  select(id,d1,d2,d3,d4,d5,d6)%>%
  mutate(d1 = ifelse(d1 > 0, 1, 0),
         d2 = ifelse(d2 > 0, 1, 0),
         d3 = ifelse(d3 > 0, 1, 0),
         d4 = ifelse(d4 > 0, 1, 0),
         d5 = ifelse(d5 > 0, 1, 0),
         d6 = ifelse(d6 > 0, 1, 0))


transactions_matrix <- as.matrix(food_df_dinner_weekdays_ar[, -1])
transactions <- as(transactions_matrix, "transactions")
itemLabels(transactions) <- c("Fruit_Vegetable", "Bread_Pasta_Rice_Potato", "Junk_Food", "Milk_Yogurt_Cheese", "Meat_Fish_Eggs_Legumes", "Sweets")
rules <- apriori(transactions, supp = 0.01, conf = 0.7, target = "rules")
plot(rules, method = "graph", engine = "htmlwidget")


food_df_dinner_weekend_ar<- food_df_dinner_weekend %>%
  select(id,d1,d2,d3,d4,d5,d6)%>%
  mutate(d1 = ifelse(d1 > 0, 1, 0),
         d2 = ifelse(d2 > 0, 1, 0),
         d3 = ifelse(d3 > 0, 1, 0),
         d4 = ifelse(d4 > 0, 1, 0),
         d5 = ifelse(d5 > 0, 1, 0),
         d6 = ifelse(d6 > 0, 1, 0))


transactions_matrix <- as.matrix(food_df_dinner_weekend_ar[, -1])
transactions <- as(transactions_matrix, "transactions")
itemLabels(transactions) <- c("Fruit_Vegetable", "Bread_Pasta_Rice_Potato", "Junk_Food", "Milk_Yogurt_Cheese", "Meat_Fish_Eggs_Legumes", "Sweets")
rules <- apriori(transactions, supp = 0.01, conf = 0.7, target = "rules")
plot(rules, method = "graph", engine = "htmlwidget")


#analisi su association rules (snack)
#---------------------------------------------------------------------------
food_df_snack_weekdays_ar<- food_df_snack_weekdays %>%
  filter(d1 != 198 | d2 != 297 | d3 != 297 | d4 != 198 | d5 != 297 | d6 != 396 ) %>%
  select(id,d1,d2,d3,d4,d5,d6)%>%
  mutate(d1 = ifelse(d1 > 0, 1, 0),
         d2 = ifelse(d2 > 0, 1, 0),
         d3 = ifelse(d3 > 0, 1, 0),
         d4 = ifelse(d4 > 0, 1, 0),
         d5 = ifelse(d5 > 0, 1, 0),
         d6 = ifelse(d6 > 0, 1, 0))


transactions_matrix <- as.matrix(food_df_snack_weekdays_ar[, -1])
transactions <- as(transactions_matrix, "transactions")
itemLabels(transactions) <- c("Fruit_Vegetable", "Bread_Pasta_Rice_Potato", "Junk_Food", "Milk_Yogurt_Cheese", "Meat_Fish_Eggs_Legumes", "Sweets")
rules <- apriori(transactions, supp = 0.01, conf = 0.9, target = "rules")
plot(rules, method = "graph", engine = "htmlwidget")


food_df_snack_weekend_ar<- food_df_snack_weekend %>%
  select(id,d1,d2,d3,d4,d5,d6)%>%
  filter(d1 != 198 | d2 != 297 | d3 != 297 | d4 != 198 | d5 != 297 | d6 != 396 ) %>%
  mutate(d1 = ifelse(d1 > 0, 1, 0),
         d2 = ifelse(d2 > 0, 1, 0),
         d3 = ifelse(d3 > 0, 1, 0),
         d4 = ifelse(d4 > 0, 1, 0),
         d5 = ifelse(d5 > 0, 1, 0),
         d6 = ifelse(d6 > 0, 1, 0))


transactions_matrix <- as.matrix(food_df_snack_weekend_ar[, -1])
transactions <- as(transactions_matrix, "transactions")
itemLabels(transactions) <- c("Fruit_Vegetable", "Bread_Pasta_Rice_Potato", "Junk_Food", "Milk_Yogurt_Cheese", "Meat_Fish_Eggs_Legumes", "Sweets")
rules <- apriori(transactions, supp = 0.01, conf = 0.7, target = "rules")
plot(rules, method = "graph", engine = "htmlwidget")





#solo drink 

# drink_df_breakfast che ci indica chi mangia cosa e quando, lo dividiamo per tutte le combinazioni sopra
#---------------------------------------------------------------------------------------------------------
create_drink_df <- function(data) {
  data %>%
    filter(type_meal == 1) %>%
    mutate(
      d7 = c3_12 + c6_18,   # water
      d8 = c3_14 + c6_20,   # caffe/tè
      d9 = c3_16 + c3_17 + c3_18 + c3_19 + c6_22 + c6_23 + c6_24 + c6_25,  # alcoholic
      d10 = c3_13 + c3_15 + c6_19 + c6_21  # non-alcoholic
    ) %>%
    select(
      -c3_3, -c3_4, -c6_12, -c6_11,
      -c3_1, -c3_2, -c6_4, -c6_9, -c6_10,
      -c6_6, -c6_7, -c3_11, -c3_20, -c6_26,
      -c3_8, -c3_9, -c6_14, -c6_13,
      -c3_6, -c3_7, -c6_5, -c6_16, -c6_17,
      -c3_10, -c6_1, -c6_2, -c6_3, -c6_8, -c3_12, -c6_18,
      -c3_13, -c3_14, -c3_15, -c6_19, -c6_20, -c6_21,
      -c3_16, -c3_17, -c3_18, -c3_19, -c6_22, -c6_23, -c6_24, -c6_25
    ) %>%
    filter(d7 != 0 | d8 != 0 | d9 != 0 | d10 != 0)
}

drink_df_breakfast <- create_drink_df(total_meal_df)
drink_df_breakfast_firstweek_weekdays <- create_drink_df(total_meal_df_firstweek_weekdays)
drink_df_breakfast_firstweek_weekend <- create_drink_df(total_meal_df_firstweek_weekend)
drink_df_breakfast_secondweek_weekdays <- create_drink_df(total_meal_df_secondweek_weekdays)
drink_df_breakfast_secondweek_weekend <- create_drink_df(total_meal_df_secondweek_weekend)
drink_df_breakfast_weekdays <- create_drink_df(total_meal_df_weekdays)
drink_df_breakfast_weekend2 <- create_drink_df(total_meal_df_weekend)


# drink_df_lunch che ci indica chi mangia cosa e quando, lo dividiamo per tutte le combinazioni sopra
# ---------------------------------------------------------------------------------------------
create_drink_df <- function(data) {
  data %>%
    filter(type_meal == 2) %>%
    mutate(
      d7 = c3_12 + c6_18,   # water
      d8 = c3_14 + c6_20,   # caffe/tè
      d9 = c3_16 + c3_17 + c3_18 + c3_19 + c6_22 + c6_23 + c6_24 + c6_25,  # alcoholic
      d10 = c3_13 + c3_15 + c6_19 + c6_21  # non-alcoholic
    ) %>%
    select(
      -c3_3, -c3_4, -c6_12, -c6_11,
      -c3_1, -c3_2, -c6_4, -c6_9, -c6_10,
      -c6_6, -c6_7, -c3_11, -c3_20, -c6_26,
      -c3_8, -c3_9, -c6_14, -c6_13,
      -c3_6, -c3_7, -c6_5, -c6_16, -c6_17,
      -c3_10, -c6_1, -c6_2, -c6_3, -c6_8, -c3_12, -c6_18,
      -c3_13, -c3_14, -c3_15, -c6_19, -c6_20, -c6_21,
      -c3_16, -c3_17, -c3_18, -c3_19, -c6_22, -c6_23, -c6_24, -c6_25
    ) %>%
    filter(d7 != 0 | d8 != 0 | d9 != 0 | d10 != 0)
}

drink_df_lunch <- create_drink_df(total_meal_df)
drink_df_lunch_firstweek_weekdays <- create_drink_df(total_meal_df_firstweek_weekdays)
drink_df_lunch_firstweek_weekend <- create_drink_df(total_meal_df_firstweek_weekend)
drink_df_lunch_secondweek_weekdays <- create_drink_df(total_meal_df_secondweek_weekdays)
drink_df_lunch_secondweek_weekend <- create_drink_df(total_meal_df_secondweek_weekend)
drink_df_lunch_weekdays <- create_drink_df(total_meal_df_weekdays)
drink_df_lunch_weekend <- create_drink_df(total_meal_df_weekend)

# drink_df_dinner che ci indica chi mangia cosa e quando, lo dividiamo per tutte le combinazioni sopra
#---------------------------------------------------------------------------------------------------------
create_drink_df <- function(data) {
  data %>%
    filter(type_meal == 2) %>%
    mutate(
      d7 = c3_12 + c6_18,   # water
      d8 = c3_14 + c6_20,   # caffe/tè
      d9 = c3_16 + c3_17 + c3_18 + c3_19 + c6_22 + c6_23 + c6_24 + c6_25,  # alcoholic
      d10 = c3_13 + c3_15 + c6_19 + c6_21  # non-alcoholic
    ) %>%
    select(
      -c3_3, -c3_4, -c6_12, -c6_11,
      -c3_1, -c3_2, -c6_4, -c6_9, -c6_10,
      -c6_6, -c6_7, -c3_11, -c3_20, -c6_26,
      -c3_8, -c3_9, -c6_14, -c6_13,
      -c3_6, -c3_7, -c6_5, -c6_16, -c6_17,
      -c3_10, -c6_1, -c6_2, -c6_3, -c6_8, -c3_12, -c6_18,
      -c3_13, -c3_14, -c3_15, -c6_19, -c6_20, -c6_21,
      -c3_16, -c3_17, -c3_18, -c3_19, -c6_22, -c6_23, -c6_24, -c6_25
    ) %>%
    filter(d7 != 0 | d8 != 0 | d9 != 0 | d10 != 0)
}

drink_df_lunch <- create_drink_df(total_meal_df)
drink_df_lunch_firstweek_weekdays <- create_drink_df(total_meal_df_firstweek_weekdays)
drink_df_lunch_firstweek_weekend <- create_drink_df(total_meal_df_firstweek_weekend)
drink_df_lunch_secondweek_weekdays <- create_drink_df(total_meal_df_secondweek_weekdays)
drink_df_lunch_secondweek_weekend <- create_drink_df(total_meal_df_secondweek_weekend)
drink_df_lunch_weekdays <- create_drink_df(total_meal_df_weekdays)
drink_df_lunch_weekend <- create_drink_df(total_meal_df_weekend)

# drink_df_dinner che ci indica chi mangia cosa e quando, lo dividiamo per tutte le combinazioni sopra
#---------------------------------------------------------------------------------------------------------

create_drink_df <- function(data) {
  data %>%
    filter(type_meal == 1) %>%
    mutate(
      d7 = c3_12 + c6_18,   # water
      d8 = c3_14 + c6_20,   # caffe/tè
      d9 = c3_16 + c3_17 + c3_18 + c3_19 + c6_22 + c6_23 + c6_24 + c6_25,  # alcoholic
      d10 = c3_13 + c3_15 + c6_19 + c6_21  # non-alcoholic
    ) %>%
    select(
      -c3_3, -c3_4, -c6_12, -c6_11,
      -c3_1, -c3_2, -c6_4, -c6_9, -c6_10,
      -c6_6, -c6_7, -c3_11, -c3_20, -c6_26,
      -c3_8, -c3_9, -c6_14, -c6_13,
      -c3_6, -c3_7, -c6_5, -c6_16, -c6_17,
      -c3_10, -c6_1, -c6_2, -c6_3, -c6_8, -c3_12, -c6_18,
      -c3_13, -c3_14, -c3_15, -c6_19, -c6_20, -c6_21,
      -c3_16, -c3_17, -c3_18, -c3_19, -c6_22, -c6_23, -c6_24, -c6_25
    ) %>%
    filter(d7 != 0 | d8 != 0 | d9 != 0 | d10 != 0)
}

drink_df_breakfast <- create_drink_df(total_meal_df)
drink_df_breakfast_firstweek_weekdays <- create_drink_df(total_meal_df_firstweek_weekdays)
drink_df_breakfast_firstweek_weekend <- create_drink_df(total_meal_df_firstweek_weekend)
drink_df_breakfast_secondweek_weekdays <- create_drink_df(total_meal_df_secondweek_weekdays)
drink_df_breakfast_secondweek_weekend <- create_drink_df(total_meal_df_secondweek_weekend)
drink_df_breakfast_weekdays <- create_drink_df(total_meal_df_weekdays)
drink_df_breakfast_weekend2 <- create_drink_df(total_meal_df_weekend)


# drink_df_lunch che ci indica chi mangia cosa e quando, lo dividiamo per tutte le combinazioni sopra
# ---------------------------------------------------------------------------------------------
create_drink_df <- function(data) {
  data %>%
    filter(type_meal == 3) %>%
    mutate(
      d7 = c3_12 + c6_18,   # water
      d8 = c3_14 + c6_20,   # caffe/tè
      d9 = c3_16 + c3_17 + c3_18 + c3_19 + c6_22 + c6_23 + c6_24 + c6_25,  # alcoholic
      d10 = c3_13 + c3_15 + c6_19 + c6_21  # non-alcoholic
    ) %>%
    select(
      -c3_3, -c3_4, -c6_12, -c6_11,
      -c3_1, -c3_2, -c6_4, -c6_9, -c6_10,
      -c6_6, -c6_7, -c3_11, -c3_20, -c6_26,
      -c3_8, -c3_9, -c6_14, -c6_13,
      -c3_6, -c3_7, -c6_5, -c6_16, -c6_17,
      -c3_10, -c6_1, -c6_2, -c6_3, -c6_8, -c3_12, -c6_18,
      -c3_13, -c3_14, -c3_15, -c6_19, -c6_20, -c6_21,
      -c3_16, -c3_17, -c3_18, -c3_19, -c6_22, -c6_23, -c6_24, -c6_25
    ) %>%
    filter(d7 != 0 | d8 != 0 | d9 != 0 | d10 != 0)
}

drink_df_dinner <- create_drink_df(total_meal_df)
drink_df_dinner_firstweek_weekdays <- create_drink_df(total_meal_df_firstweek_weekdays)
drink_df_dinner_firstweek_weekend <- create_drink_df(total_meal_df_firstweek_weekend)
drink_df_dinner_secondweek_weekdays <- create_drink_df(total_meal_df_secondweek_weekdays)
drink_df_dinner_secondweek_weekend <- create_drink_df(total_meal_df_secondweek_weekend)
drink_df_dinner_weekdays <- create_drink_df(total_meal_df_weekdays)
drink_df_dinner_weekend <- create_drink_df(total_meal_df_weekend)

# drink_df_snack che ci indica chi mangia cosa e quando, lo dividiamo per tutte le combinazioni sopra
#---------------------------------------------------------------------------------------------------------
create_drink_df <- function(data) {
  data %>%
    filter(type_meal == 2) %>%
    mutate(
      d7 = c3_12 + c6_18,   # water
      d8 = c3_14 + c6_20,   # caffe/tè
      d9 = c3_16 + c3_17 + c3_18 + c3_19 + c6_22 + c6_23 + c6_24 + c6_25,  # alcoholic
      d10 = c3_13 + c3_15 + c6_19 + c6_21  # non-alcoholic
    ) %>%
    select(
      -c3_3, -c3_4, -c6_12, -c6_11,
      -c3_1, -c3_2, -c6_4, -c6_9, -c6_10,
      -c6_6, -c6_7, -c3_11, -c3_20, -c6_26,
      -c3_8, -c3_9, -c6_14, -c6_13,
      -c3_6, -c3_7, -c6_5, -c6_16, -c6_17,
      -c3_10, -c6_1, -c6_2, -c6_3, -c6_8, -c3_12, -c6_18,
      -c3_13, -c3_14, -c3_15, -c6_19, -c6_20, -c6_21,
      -c3_16, -c3_17, -c3_18, -c3_19, -c6_22, -c6_23, -c6_24, -c6_25
    ) %>%
    filter(d7 != 0 | d8 != 0 | d9 != 0 | d10 != 0)
}

drink_df_lunch <- create_drink_df(total_meal_df)
drink_df_lunch_firstweek_weekdays <- create_drink_df(total_meal_df_firstweek_weekdays)
drink_df_lunch_firstweek_weekend <- create_drink_df(total_meal_df_firstweek_weekend)
drink_df_lunch_secondweek_weekdays <- create_drink_df(total_meal_df_secondweek_weekdays)
drink_df_lunch_secondweek_weekend <- create_drink_df(total_meal_df_secondweek_weekend)
drink_df_lunch_weekdays <- create_drink_df(total_meal_df_weekdays)
drink_df_lunch_weekend <- create_drink_df(total_meal_df_weekend)

# drink_df_dinner che ci indica chi mangia cosa e quando, lo dividiamo per tutte le combinazioni sopra
#---------------------------------------------------------------------------------------------------------

create_drink_df <- function(data) {
  data %>%
    filter(type_meal == 1) %>%
    mutate(
      d7 = c3_12 + c6_18,   # water
      d8 = c3_14 + c6_20,   # caffe/tè
      d9 = c3_16 + c3_17 + c3_18 + c3_19 + c6_22 + c6_23 + c6_24 + c6_25,  # alcoholic
      d10 = c3_13 + c3_15 + c6_19 + c6_21  # non-alcoholic
    ) %>%
    select(
      -c3_3, -c3_4, -c6_12, -c6_11,
      -c3_1, -c3_2, -c6_4, -c6_9, -c6_10,
      -c6_6, -c6_7, -c3_11, -c3_20, -c6_26,
      -c3_8, -c3_9, -c6_14, -c6_13,
      -c3_6, -c3_7, -c6_5, -c6_16, -c6_17,
      -c3_10, -c6_1, -c6_2, -c6_3, -c6_8, -c3_12, -c6_18,
      -c3_13, -c3_14, -c3_15, -c6_19, -c6_20, -c6_21,
      -c3_16, -c3_17, -c3_18, -c3_19, -c6_22, -c6_23, -c6_24, -c6_25
    ) %>%
    filter(d7 != 0 | d8 != 0 | d9 != 0 | d10 != 0)
}

drink_df_breakfast <- create_drink_df(total_meal_df)
drink_df_breakfast_firstweek_weekdays <- create_drink_df(total_meal_df_firstweek_weekdays)
drink_df_breakfast_firstweek_weekend <- create_drink_df(total_meal_df_firstweek_weekend)
drink_df_breakfast_secondweek_weekdays <- create_drink_df(total_meal_df_secondweek_weekdays)
drink_df_breakfast_secondweek_weekend <- create_drink_df(total_meal_df_secondweek_weekend)
drink_df_breakfast_weekdays <- create_drink_df(total_meal_df_weekdays)
drink_df_breakfast_weekend2 <- create_drink_df(total_meal_df_weekend)


# drink_df_lunch che ci indica chi mangia cosa e quando, lo dividiamo per tutte le combinazioni sopra
# ---------------------------------------------------------------------------------------------
create_drink_df <- function(data) {
  data %>%
    filter(what==101) %>%
    mutate(
      d7 = c3_12 + c6_18,   # water
      d8 = c3_14 + c6_20,   # caffe/tè
      d9 = c3_16 + c3_17 + c3_18 + c3_19 + c6_22 + c6_23 + c6_24 + c6_25,  # alcoholic
      d10 = c3_13 + c3_15 + c6_19 + c6_21  # non-alcoholic
    ) %>%
    select(
      -c3_3, -c3_4, -c6_12, -c6_11,
      -c3_1, -c3_2, -c6_4, -c6_9, -c6_10,
      -c6_6, -c6_7, -c3_11, -c3_20, -c6_26,
      -c3_8, -c3_9, -c6_14, -c6_13,
      -c3_6, -c3_7, -c6_5, -c6_16, -c6_17,
      -c3_10, -c6_1, -c6_2, -c6_3, -c6_8, -c3_12, -c6_18,
      -c3_13, -c3_14, -c3_15, -c6_19, -c6_20, -c6_21,
      -c3_16, -c3_17, -c3_18, -c3_19, -c6_22, -c6_23, -c6_24, -c6_25
    ) %>%
    filter(d7 != 0 | d8 != 0 | d9 != 0 | d10 != 0)
}

drink_df_snack <- create_drink_df(total_meal_df)
drink_df_snack_firstweek_weekdays <- create_drink_df(total_meal_df_firstweek_weekdays)
drink_df_snack_firstweek_weekend <- create_drink_df(total_meal_df_firstweek_weekend)
drink_df_snack_secondweek_weekdays <- create_drink_df(total_meal_df_secondweek_weekdays)
drink_df_snack_secondweek_weekend <- create_drink_df(total_meal_df_secondweek_weekend)
drink_df_snack_weekdays <- create_drink_df(total_meal_df_weekdays)
drink_df_snack_weekend <- create_drink_df(total_meal_df_weekend)

# analisi drink breakfast
# ---------------------------------------------------------------------------------------------
calculate_diet_patterns <- function(data) {
  data %>%
    group_by(id) %>%
    summarize(
      water = n_distinct(week_new[d7 > 0]),
      caffe_tè = n_distinct(week_new[d8 > 0]),
      alch = n_distinct(week_new[d9 > 0]),
      not_alch = n_distinct(week_new[d10 > 0]),
    )
}

diet_patterns_breakfast_firstweek_weekdays <- calculate_diet_patterns(drink_df_breakfast_firstweek_weekdays)
diet_patterns_breakfast_firstweek_weekend <- calculate_diet_patterns(drink_df_breakfast_firstweek_weekend)
diet_patterns_breakfast_secondweek_weekdays <- calculate_diet_patterns(drink_df_breakfast_secondweek_weekdays)
diet_patterns_breakfast_secondweek_weekend <- calculate_diet_patterns(drink_df_breakfast_secondweek_weekend)
diet_patterns_breakfast_weekdays <- calculate_diet_patterns(drink_df_breakfast_weekdays)
diet_patterns_breakfast_weekend <- calculate_diet_patterns(drink_df_breakfast_weekend)

drink_categories <- c("water", "caffe_tè", "alch", "not_alch")

generate_drink_category_plot <- function(data, drink_categories, title) {
  results <- data.frame()
  for (drink_category in drink_categories) {
    categorized_data <- data %>%
      filter(id %in% selected_ids2) %>%
      mutate(category = case_when(
        .data[[drink_category]] == 0 ~ "0 days",
        .data[[drink_category]] %in% 1:2 ~ "1-2 days",
        .data[[drink_category]] >= 3 ~ "3+ days"
      )) %>%
      group_by(category) %>%
      summarise(count = n()) %>%
      mutate(percentage = (count / sum(count)) * 100) %>%
      mutate(drink_category = drink_category)
    
    results <- bind_rows(results, categorized_data)
  }
  
  ggplot(results, aes(x = drink_category, y = percentage, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = 0.7), position = position_dodge(width = 0.8) , size = 3) +
    labs(
      title = title,
      x = "drink Category",
      y = "Percentage"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
}



plot1 <- generate_drink_category_plot(diet_patterns_breakfast_firstweek_weekdays, drink_categories, "Percentage of People by drink Category (Breakfast, First Week, Weekdays)")
plot2 <- generate_drink_category_plot(diet_patterns_breakfast_firstweek_weekend, drink_categories, "Percentage of People by drink Category (Breakfast, First Week, Weekend)")
plot3 <- generate_drink_category_plot(diet_patterns_breakfast_secondweek_weekdays, drink_categories, "Percentage of People by drink Category (Breakfast, Second Week, Weekdays)")
plot4 <- generate_drink_category_plot(diet_patterns_breakfast_secondweek_weekend, drink_categories, "Percentage of People by drink Category (Breakfast, Second Week, Weekend)")

generate_drink_category_plot <- function(data, drink_categories, title, wd=T) {
  results <- data.frame()
  if(wd==TRUE){
  for (drink_category in drink_categories) {
    categorized_data <- data %>%
      filter(id %in% selected_ids2) %>%
      mutate(category = case_when(
        .data[[drink_category]] == 0 ~ "0 days",
        .data[[drink_category]] %in% 1:4 ~ "1-4 days",
        .data[[drink_category]] >= 5 ~ "5+ days"
      )) %>%
      group_by(category) %>%
      summarise(count = n()) %>%
      mutate(percentage = (count / sum(count)) * 100) %>%
      mutate(drink_category = drink_category)
    
    results <- bind_rows(results, categorized_data)
  }
  }else{
    for (drink_category in drink_categories) {
      categorized_data <- data %>%
        filter(id %in% selected_ids2) %>%
        mutate(category = case_when(
          .data[[drink_category]] == 0 ~ "0 days",
          .data[[drink_category]] %in% 1:2 ~ "1-2 days",
          .data[[drink_category]] >= 3 ~ "3+ days"
        )) %>%
        group_by(category) %>%
        summarise(count = n()) %>%
        mutate(percentage = (count / sum(count)) * 100) %>%
        mutate(drink_category = drink_category)
      
      results <- bind_rows(results, categorized_data)
    }
    }
  
  ggplot(results, aes(x = drink_category, y = percentage, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = 0.7), position = position_dodge(width = 0.8) , size = 3) +
    labs(
      title = title,
      x = "drink Category",
      y = "Percentage"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
}

plot5 <- generate_drink_category_plot(diet_patterns_breakfast_weekdays, drink_categories, "Percentage of People by drink Category (Breakfast, Weekdays)")
plot6 <- generate_drink_category_plot(diet_patterns_breakfast_weekend, drink_categories, "Percentage of People by drink Category (Breakfast, Weekend)", wd=F)

grid.arrange(plot1, plot3, ncol = 1)
grid.arrange(plot2, plot4, ncol = 1)
grid.arrange(plot5, plot6, ncol = 1)


# analisi drink lunch
# ---------------------------------------------------------------------------------------------
calculate_diet_patterns <- function(data) {
  data %>%
    group_by(id) %>%
    summarize(
      water = n_distinct(week_new[d7 > 0]),
      caffe_tè = n_distinct(week_new[d8 > 0]),
      alch = n_distinct(week_new[d9 > 0]),
      not_alch = n_distinct(week_new[d10 > 0]),
    )
}

diet_patterns_lunch_firstweek_weekdays <- calculate_diet_patterns(drink_df_lunch_firstweek_weekdays)
diet_patterns_lunch_firstweek_weekend <- calculate_diet_patterns(drink_df_lunch_firstweek_weekend)
diet_patterns_lunch_secondweek_weekdays <- calculate_diet_patterns(drink_df_lunch_secondweek_weekdays)
diet_patterns_lunch_secondweek_weekend <- calculate_diet_patterns(drink_df_lunch_secondweek_weekend)
diet_patterns_lunch_weekdays <- calculate_diet_patterns(drink_df_lunch_weekdays)
diet_patterns_lunch_weekend <- calculate_diet_patterns(drink_df_lunch_weekend)

generate_drink_category_plot <- function(data, drink_categories, title) {
  results <- data.frame()
  for (drink_category in drink_categories) {
    categorized_data <- data %>%
      filter(id %in% selected_ids2) %>%
      mutate(category = case_when(
        .data[[drink_category]] == 0 ~ "0 days",
        .data[[drink_category]] %in% 1:2 ~ "1-2 days",
        .data[[drink_category]] >= 3 ~ "3+ days"
      )) %>%
      group_by(category) %>%
      summarise(count = n()) %>%
      mutate(percentage = (count / sum(count)) * 100) %>%
      mutate(drink_category = drink_category)
    
    results <- bind_rows(results, categorized_data)
  }
  
  ggplot(results, aes(x = drink_category, y = percentage, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = 0.7), position = position_dodge(width = 0.8) , size = 3) +
    labs(
      title = title,
      x = "drink Category",
      y = "Percentage"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
}

plot1 <- generate_drink_category_plot(diet_patterns_lunch_firstweek_weekdays, drink_categories, "Percentage of People by drink Category (Lunch, First Week, Weekdays)")
plot2 <- generate_drink_category_plot(diet_patterns_lunch_firstweek_weekend, drink_categories, "Percentage of People by drink Category (Lunch, First Week, Weekend)")
plot3 <- generate_drink_category_plot(diet_patterns_lunch_secondweek_weekdays, drink_categories, "Percentage of People by drink Category (Lunch, Second Week, Weekdays)")
plot4 <- generate_drink_category_plot(diet_patterns_lunch_secondweek_weekend, drink_categories, "Percentage of People by drink Category (Lunch, Second Week, Weekend)")

generate_drink_category_plot <- function(data, drink_categories, title, wd=T) {
  results <- data.frame()
  if(wd==TRUE){
    for (drink_category in drink_categories) {
      categorized_data <- data %>%
        filter(id %in% selected_ids2) %>%
        mutate(category = case_when(
          .data[[drink_category]] == 0 ~ "0 days",
          .data[[drink_category]] %in% 1:4 ~ "1-4 days",
          .data[[drink_category]] >= 5 ~ "5+ days"
        )) %>%
        group_by(category) %>%
        summarise(count = n()) %>%
        mutate(percentage = (count / sum(count)) * 100) %>%
        mutate(drink_category = drink_category)
      
      results <- bind_rows(results, categorized_data)
    }
  }else{
    for (drink_category in drink_categories) {
      categorized_data <- data %>%
        filter(id %in% selected_ids2) %>%
        mutate(category = case_when(
          .data[[drink_category]] == 0 ~ "0 days",
          .data[[drink_category]] %in% 1:2 ~ "1-2 days",
          .data[[drink_category]] >= 3 ~ "3+ days"
        )) %>%
        group_by(category) %>%
        summarise(count = n()) %>%
        mutate(percentage = (count / sum(count)) * 100) %>%
        mutate(drink_category = drink_category)
      
      results <- bind_rows(results, categorized_data)
    }
  }
  
  ggplot(results, aes(x = drink_category, y = percentage, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = 0.7), position = position_dodge(width = 0.8) , size = 3) +
    labs(
      title = title,
      x = "drink Category",
      y = "Percentage"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
}

plot5 <- generate_drink_category_plot(diet_patterns_lunch_weekdays, drink_categories, "Percentage of People by drink Category (Lunch, Weekdays)")
plot6 <- generate_drink_category_plot(diet_patterns_lunch_weekend, drink_categories, "Percentage of People by drink Category (Lunch, Weekend)",wd=F)

grid.arrange(plot1, plot3, ncol = 1)
grid.arrange(plot2, plot4, ncol = 1)
grid.arrange(plot5, plot6, ncol = 1)


# analisi drink dinner
# ---------------------------------------------------------------------------------------------
calculate_diet_patterns <- function(data) {
  data %>%
    group_by(id) %>%
    summarize(
      water = n_distinct(week_new[d7 > 0]),
      caffe_tè = n_distinct(week_new[d8 > 0]),
      alch = n_distinct(week_new[d9 > 0]),
      not_alch = n_distinct(week_new[d10 > 0]),
    )
}

diet_patterns_dinner_firstweek_weekdays <- calculate_diet_patterns(drink_df_dinner_firstweek_weekdays)
diet_patterns_dinner_firstweek_weekend <- calculate_diet_patterns(drink_df_dinner_firstweek_weekend)
diet_patterns_dinner_secondweek_weekdays <- calculate_diet_patterns(drink_df_dinner_secondweek_weekdays)
diet_patterns_dinner_secondweek_weekend <- calculate_diet_patterns(drink_df_dinner_secondweek_weekend)
diet_patterns_dinner_weekdays <- calculate_diet_patterns(drink_df_dinner_weekdays)
diet_patterns_dinner_weekend <- calculate_diet_patterns(drink_df_dinner_weekend)

generate_drink_category_plot <- function(data, drink_categories, title) {
  results <- data.frame()
  for (drink_category in drink_categories) {
    categorized_data <- data %>%
      filter(id %in% selected_ids2) %>%
      mutate(category = case_when(
        .data[[drink_category]] == 0 ~ "0 days",
        .data[[drink_category]] %in% 1:2 ~ "1-2 days",
        .data[[drink_category]] >= 3 ~ "3+ days"
      )) %>%
      group_by(category) %>%
      summarise(count = n()) %>%
      mutate(percentage = (count / sum(count)) * 100) %>%
      mutate(drink_category = drink_category)
    
    results <- bind_rows(results, categorized_data)
  }
  
  ggplot(results, aes(x = drink_category, y = percentage, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = 0.7), position = position_dodge(width = 0.8) , size = 3) +
    labs(
      title = title,
      x = "drink Category",
      y = "Percentage"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
}

plot1 <- generate_drink_category_plot(diet_patterns_dinner_firstweek_weekdays, drink_categories, "Percentage of People by drink Category (Dinner, First Week, Weekdays)")
plot2 <- generate_drink_category_plot(diet_patterns_dinner_firstweek_weekend, drink_categories, "Percentage of People by drink Category (Dinner, First Week, Weekend)")
plot3 <- generate_drink_category_plot(diet_patterns_dinner_secondweek_weekdays, drink_categories, "Percentage of People by drink Category (Dinner, Second Week, Weekdays)")
plot4 <- generate_drink_category_plot(diet_patterns_dinner_secondweek_weekend, drink_categories, "Percentage of People by drink Category (Dinner, Second Week, Weekend)")

generate_drink_category_plot <- function(data, drink_categories, title, wd=T) {
  results <- data.frame()
  if(wd==TRUE){
    for (drink_category in drink_categories) {
      categorized_data <- data %>%
        filter(id %in% selected_ids2) %>%
        mutate(category = case_when(
          .data[[drink_category]] == 0 ~ "0 days",
          .data[[drink_category]] %in% 1:4 ~ "1-4 days",
          .data[[drink_category]] >= 5 ~ "5+ days"
        )) %>%
        group_by(category) %>%
        summarise(count = n()) %>%
        mutate(percentage = (count / sum(count)) * 100) %>%
        mutate(drink_category = drink_category)
      
      results <- bind_rows(results, categorized_data)
    }
  }else{
    for (drink_category in drink_categories) {
      categorized_data <- data %>%
        filter(id %in% selected_ids2) %>%
        mutate(category = case_when(
          .data[[drink_category]] == 0 ~ "0 days",
          .data[[drink_category]] %in% 1:2 ~ "1-2 days",
          .data[[drink_category]] >= 3 ~ "3+ days"
        )) %>%
        group_by(category) %>%
        summarise(count = n()) %>%
        mutate(percentage = (count / sum(count)) * 100) %>%
        mutate(drink_category = drink_category)
      
      results <- bind_rows(results, categorized_data)
    }
  }
  
  ggplot(results, aes(x = drink_category, y = percentage, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = 0.7), position = position_dodge(width = 0.8) , size = 3) +
    labs(
      title = title,
      x = "drink Category",
      y = "Percentage"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
}
plot5 <- generate_drink_category_plot(diet_patterns_dinner_weekdays, drink_categories, "Percentage of People by drink Category (Dinner, Weekdays)")
plot6 <- generate_drink_category_plot(diet_patterns_dinner_weekend, drink_categories, "Percentage of People by drink Category (Dinner, Weekend)",wd=F)

grid.arrange(plot1, plot3, ncol = 1)
grid.arrange(plot2, plot4, ncol = 1)
grid.arrange(plot5, plot6, ncol = 1)



# analisi drink snack
# ---------------------------------------------------------------------------------------------
calculate_diet_patterns <- function(data) {
  data %>%
    filter(d7 != 99 | d8 != 99 | d9 != 396 | d10 != 198)%>%
    group_by(id) %>%
    summarize(
      water = n_distinct(week_new[d7 > 0]),
      caffe_tè = n_distinct(week_new[d8 > 0]),
      alch = n_distinct(week_new[d9 > 0]),
      not_alch = n_distinct(week_new[d10 > 0]),
    )
}
diet_patterns_snack_firstweek_weekdays <- calculate_diet_patterns(drink_df_snack_firstweek_weekdays)
diet_patterns_snack_firstweek_weekend <- calculate_diet_patterns(drink_df_snack_firstweek_weekend)
diet_patterns_snack_secondweek_weekdays <- calculate_diet_patterns(drink_df_snack_secondweek_weekdays)
diet_patterns_snack_secondweek_weekend <- calculate_diet_patterns(drink_df_snack_secondweek_weekend)
diet_patterns_snack_weekdays <- calculate_diet_patterns(drink_df_snack_weekdays)
diet_patterns_snack_weekend <- calculate_diet_patterns(drink_df_snack_weekend)

generate_drink_category_plot <- function(data, drink_categories, title) {
  results <- data.frame()
  for (drink_category in drink_categories) {
    categorized_data <- data %>%
      filter(id %in% selected_ids2) %>%
      mutate(category = case_when(
        .data[[drink_category]] == 0 ~ "0 days",
        .data[[drink_category]] %in% 1:2 ~ "1-2 days",
        .data[[drink_category]] >= 3 ~ "3+ days"
      )) %>%
      group_by(category) %>%
      summarise(count = n()) %>%
      mutate(percentage = (count / sum(count)) * 100) %>%
      mutate(drink_category = drink_category)
    
    results <- bind_rows(results, categorized_data)
  }
  
  ggplot(results, aes(x = drink_category, y = percentage, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = 0.7), position = position_dodge(width = 0.8) , size = 3) +
    labs(
      title = title,
      x = "drink Category",
      y = "Percentage"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
}

plot1 <- generate_drink_category_plot(diet_patterns_snack_firstweek_weekdays, drink_categories, "Percentage of People by drink Category (Snack, First Week, Weekdays)")
plot2 <- generate_drink_category_plot(diet_patterns_snack_firstweek_weekend, drink_categories, "Percentage of People by drink Category (Snack, First Week, Weekend)")
plot3 <- generate_drink_category_plot(diet_patterns_snack_secondweek_weekdays, drink_categories, "Percentage of People by drink Category (Snack, Second Week, Weekdays)")
plot4 <- generate_drink_category_plot(diet_patterns_snack_secondweek_weekend, drink_categories, "Percentage of People by drink Category (Snack, Second Week, Weekend)")

generate_drink_category_plot <- function(data, drink_categories, title, wd=T) {
  results <- data.frame()
  if(wd==TRUE){
    for (drink_category in drink_categories) {
      categorized_data <- data %>%
        filter(id %in% selected_ids2) %>%
        mutate(category = case_when(
          .data[[drink_category]] == 0 ~ "0 days",
          .data[[drink_category]] %in% 1:4 ~ "1-4 days",
          .data[[drink_category]] >= 5 ~ "5+ days"
        )) %>%
        group_by(category) %>%
        summarise(count = n()) %>%
        mutate(percentage = (count / sum(count)) * 100) %>%
        mutate(drink_category = drink_category)
      
      results <- bind_rows(results, categorized_data)
    }
  }else{
    for (drink_category in drink_categories) {
      categorized_data <- data %>%
        filter(id %in% selected_ids2) %>%
        mutate(category = case_when(
          .data[[drink_category]] == 0 ~ "0 days",
          .data[[drink_category]] %in% 1:2 ~ "1-2 days",
          .data[[drink_category]] >= 3 ~ "3+ days"
        )) %>%
        group_by(category) %>%
        summarise(count = n()) %>%
        mutate(percentage = (count / sum(count)) * 100) %>%
        mutate(drink_category = drink_category)
      
      results <- bind_rows(results, categorized_data)
    }
  }
  
  ggplot(results, aes(x = drink_category, y = percentage, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), vjust = 0.7), position = position_dodge(width = 0.8) , size = 3) +
    labs(
      title = title,
      x = "drink Category",
      y = "Percentage"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
}
plot5 <- generate_drink_category_plot(diet_patterns_snack_weekdays, drink_categories, "Percentage of People by drink Category (Snack, Weekdays)")
plot6 <- generate_drink_category_plot(diet_patterns_snack_weekend, drink_categories, "Percentage of People by drink Category (Snack, Weekend)", wd=F)

grid.arrange(plot1, plot3, ncol = 1)
grid.arrange(plot2, plot4, ncol = 1)
grid.arrange(plot5, plot6, ncol = 1)




