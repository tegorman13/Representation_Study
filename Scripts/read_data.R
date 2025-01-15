
# read sav file "CLEAN NSF Frequency and Probability - 3-9-18.sav"

#library(foreign)

# data <- read.spss("CLEAN NSF Frequency and Probability - 3-9-18.sav", to.data.frame = TRUE)
# colnames(data)
# head(data,1)

# read sav file "Frequency & Probability Study 1 - 3-24-19.sav"
#data2 <- read.spss("Frequency & Probability Study 1 - 3-24-19.sav", to.data.frame = TRUE)
# read sav file "Frequency & Probability - 9-25-18 cleaned.sav"
#data3 <- read.spss("Frequency & Probability - 9-25-18 cleaned.sav", to.data.frame = TRUE)



#------

# list.files('data')
# [1] "aggr.sav"  - aggregates for 12 conditions (RefClass *ROUNDED * TENPERCENT)                                                                        
# [2] "CLEAN NSF Frequency and Probability - 3-9-18.sav"                                  
# [3] "Format_1.sav"                                                                      
# [4] "Format_3.sav"                                                                      
# [5] "Format_new1.sav"  - 252x2353;  accuracy - no group variables?                                                              
# [6] "Format_revised1.sav"                                                               
# [8] "Frequency & Probability - 9-25-18 cleaned.sav"                                     
# [9] "Frequency & Probability - 9-25-18_2.sav"                                           
# [10] "Frequency & Probability 2 10-25-18 140p 207count.sav"                              
# [11] "Frequency & Probability 2 3-22-19.sav"    - 204x145;  refclass; rounding; tenpercent;                                         
# [12] "Frequency & Probability old.sav"                                                   
# [13] "Frequency & Probability Study 1 - 3-24-19.sav" - 1840x252; ReferenceClass and States; responseid; sept 24-25th 2018                                    
# [14] "Frequency & Probability Study 2 - 3-24-19.sav"    - 208x146; refclass; rounding; tenpercent; states;  Oct 21-22 2018                           
# [16] "Study2_1.sav"    - 207x143 -  refclass; rounding; tenpercent; states; correct; Oct 21-22 2018                                                              
# [17] "Study2_1revised.sav"                                                               
# [18] "Study2_3revised.sav"   
  
  
  
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)

# Read the SPSS file
data <- read_spss("CLEAN NSF Frequency and Probability - 3-9-18.sav")

data2 <- read_spss("Frequency & Probability Study 1 - 3-24-19.sav")
data3 <- read_spss("Frequency & Probability - 9-25-18 cleaned.sav")

d1 <- haven::read_sav("data/Frequency & Probability Study 1 - 3-24-19.sav")
d2 <- haven::read_sav("data/Frequency & Probability Study 2 - 3-24-19.sav")

# write d2 to csv
write.csv(d2, "data/Frequency & Probability Study 2 - 3-24-19.csv", row.names = FALSE)


unique((data2$ReferenceClass))
unique((data3$ReferenceClass))


# how many unique ResponseId in data2 and in data3, and how much overlap in ids between data2 and data3

length(unique(data2$ResponseId))
length(unique(data3$ResponseId))

length(intersect(unique(data2$ResponseId), unique(data3$ResponseId)))

data2$ResponseId %in% data3$ResponseId
data2$IPAddress %in% data3$IPAddress



# print colnames that are in data2 but not in data3
setdiff(colnames(data2), colnames(data3))

# print colnames in data2 but not in data
setdiff(colnames(data2), colnames(data))



# Function to clean column names
clean_names <- function(x) {
  x <- tolower(x)
  x <- gsub("__in_seconds_", "_seconds", x)
  x <- gsub("[^a-z0-9_]", "_", x)
  x <- gsub("(^_|_$)", "", x)
  x <- make.unique(x, sep = "_")
  return(x)
}

# Clean column names
colnames(data) <- clean_names(colnames(data))

# Identify demographic columns
demographic_cols <- c("dem01", "dem02", "dem03", "dem04", "dem05", "dem06", "dem07")

# Rename demographic columns
data <- data %>%
  rename(
    gender = dem01,
    birth_year = dem02,
    income = dem03,
    education = dem04,
    employment = dem05,
    state = dem06,
    years_in_state = dem07
  )

# Function to identify scenario columns
identify_scenario_cols <- function(prefix) {
  grep(paste0("^", prefix), names(data), value = TRUE)
}

# Identify scenario columns
scenario_prefixes <- c("dcal", "pflo", "atex", "acal", "dflo", "ptex", "pcal", "aflo", "dtex", "dcol", "pmas", "aill", "acol", "dmas", "pill", "pcol", "amas", "dill")
scenario_cols <- unlist(lapply(scenario_prefixes, identify_scenario_cols))

# Identify numeric and character columns
numeric_cols <- scenario_cols[sapply(data[scenario_cols], is.numeric)]
character_cols <- scenario_cols[sapply(data[scenario_cols], is.character)]

# Pivot numeric columns
data_long_numeric <- data %>%
  select(responseid, all_of(numeric_cols)) %>%
  pivot_longer(
    cols = all_of(numeric_cols),
    names_to = c("scenario", ".value"),
    names_pattern = "([a-z]{4})(.*)"
  )

# Pivot character columns
data_long_character <- data %>%
  select(responseid, all_of(character_cols)) %>%
  pivot_longer(
    cols = all_of(character_cols),
    names_to = c("scenario", ".value"),
    names_pattern = "([a-z]{4})(.*)"
  )

# Combine numeric and character data
data_long <- full_join(data_long_numeric, data_long_character, by = c("responseid", "scenario"))

# Merge demographic data with scenario data
final_data <- data %>%
  select(-all_of(scenario_cols)) %>%
  left_join(data_long, by = "responseid")

# Display the structure of the final dataset
str(final_data)








# rename responseid to id, and remove ipaddress variable
final_data <- final_data %>%
  rename(id = responseid) %>%
  select(-ipaddress)


# Save the processed data
write.csv(final_data, "processed_energy_study_data.csv", row.names = FALSE)

colnames(final_data)
head(final_data, 2)
glimpse(final_data)




id1 <- final_data |> filter(id == "R_2TZTO4IQItQVzQG")


# count number of unique ids
final_data %>% summarise(n_distinct(id)) #204

final_data |> summarise(n_distinct(rs04)) #204

