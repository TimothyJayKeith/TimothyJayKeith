# Comparing each of the 8 characters from the Square Enix video game "Octopath Traveler."

library(tidyverse)
library(rvest)
library(googlesheets4)

# Drawing from data tables from
# https://octopathtraveler.fandom.com/wiki/Experience/Table
# https://docs.google.com/spreadsheets/d/11dnbKMGLmoFTGLt2yxmeNRZSGu56_WqYIdWAdvB7Ab4/edit#gid=2106068011
#  (Data table posted by u/shelagog July 19, 2019)

# Using multiple sources, I will attempt to create a table that compares experience growth and stat growth
#  For each character in Octopath traveler

# I will start by copying the experience table from https://octopathtraveler.fandom.com/wiki/Experience/Table into R
Experience.url <- "https://octopathtraveler.fandom.com/wiki/Experience/Table"
# That done, I will take the tables from the website and transform them into tibbles from tidyverse.
Experience <- Experience.url %>%
  # Read the html file
  read_html() %>%
  # Extract the table
  html_node("table") %>%
  # Turn it into a dataframe
  html_table(header = FALSE)
# Eliminate the first row
Experience <- Experience[-1,]
# Separate and the table into three seperate tables and add column names
Exp_Table_1 <- Experience %>% transmute(Level = X1, Next_Level = X2, Total_EXP = X3)
Exp_Table_2 <- Experience %>% transmute(Level = X5, Next_Level = X6, Total_EXP = X7)
Exp_Table_3 <- Experience %>% transmute(Level = X9, Next_Level = X10, Total_EXP = X11)
# Recombine each of the tables into one
Exp_Table <- rbind(Exp_Table_1, Exp_Table_2, Exp_Table_3) %>%
  # Turn the dataframe into a tibble
  as_tibble() %>%
  # Parse all the character observations into integers
  mutate(Level = parse_number(Level),
         Next_Level = parse_number(Next_Level),
         Total_EXP = parse_number(Total_EXP))
# Expected outcome: Level Next_Level Total_EXP
#                     1       6          6
#                     2       10         16


# Now read in the google sheets document from 
# https://docs.google.com/spreadsheets/d/11dnbKMGLmoFTGLt2yxmeNRZSGu56_WqYIdWAdvB7Ab4/edit#gid=2106068011
# To get a table of the stats of each character
# Be sure to do this part separately as it will ask you to log into your Google account
Stats.sheet <- "https://docs.google.com/spreadsheets/d/11dnbKMGLmoFTGLt2yxmeNRZSGu56_WqYIdWAdvB7Ab4/edit#gid=2106068011"
# Read the sheet into R
Stats <- Stats.sheet %>%
  read_sheet(sheet = "Level Stats", skip = 2)

# Create a function that will break the table down into several, separate tables for each character
Make_Character <- function(Character_Name, Lower_Bound, Upper_Bound){
  # Before we do anything else, we will save the character name as a character variable
  Character_String <- deparse(substitute(Character_Name))
  # Now we will begin constructing our table
  Character_Name <- Stats %>% 
    # select all the variables that describe this character
    select(Lower_Bound:Upper_Bound) %>%
    # Correct column names
    rename(Level = starts_with("Level"), HP = starts_with("HP"), SP = starts_with("SP"), Phys_Attack = starts_with("P. Attack"), 
           Elem_Attack = starts_with("E. Attack"), Phys_Defense = starts_with("P. Def"), Elem_Defense = starts_with("E. Def"), 
           Accuracy = starts_with("Accuracy"), Speed = starts_with("Speed"), Critical = starts_with("Crit"),
           Evasion = starts_with("Evasion"), Total = starts_with("Total")) %>%
    # Add a new row with identifying the character (important for the later binding of all the tables)
    mutate(Character = Character_String) %>%
    # Combine with Exp_Table
    right_join(Exp_Table) %>%
    # Rearrange the order of column names
    select(Level, Next_Level, Total_EXP, Character, everything())
}

# Try it with Alfyn
  Alfyn <- Make_Character(Alfyn, "Level...2", "Total...13")
# Expected Outcome: Character HP  SP  Phys_Attack  Elem_Attack  Phys_Defense  Elem_Defense  ... Total
#                     Alfyn   300 50      88           80            80            80            800

# Repeat the process with Alfyn for all the other characters 

# Cyrus
Cyrus <- Make_Character(Cyrus, "Level...16", "Total...27")

# H'aanit
Haanit <- Make_Character(Haanit, "Level...30", "Total...41") %>%
  # Insert the tick mark into H'aanit's name
  mutate(Character = "H'aanit")

# Olberic
Olberic <- Make_Character(Olberic, "Level...44", "Total...55")

# Ophilia
Ophilia <- Make_Character(Ophilia, "Level...58", "Total...69")

# Primrose
Primrose <- Make_Character(Primrose, "Level...72", "Total...83")

# Therion
Therion <- Make_Character(Therion, "Level...86", "Total...97")

# Tressa
Tressa <- Make_Character(Tressa, "Level...100", "Total...111")

# Now to bring them all together
All_Char_Stats <- bind_rows(Alfyn, Cyrus, Haanit, Olberic, Ophilia, Primrose, Therion, Tressa) %>%
  # Rearrange so the table sorts by level
  arrange(Level)
# Expected Outcome:  Level Next_Level Total_EXP Character HP ...
#                      1       6          16      Alfyn   300

# Oddly enought, some of the later entries print as floating point numbers, even though they were not so on the google sheet
# Attempting to fix this by printing the numbers as integers
All_Char_Stats <- All_Char_Stats %>%
  mutate( HP = as.integer(HP),
          SP = as.integer(SP),
          Phys_Attack = as.integer(Phys_Attack),
          Elem_Attack = as.integer(Elem_Attack),
          Phys_Defense = as.integer(Phys_Defense),
          Elem_Defense = as.integer(Elem_Defense),
          Accuracy = as.integer(Accuracy),
          Speed = as.integer(Speed),
          Critical = as.integer(Critical),
          Evasion = as.integer(Evasion),
          Total = as.integer(Total))
# Now all the erratically behaving numbers are integers
####
