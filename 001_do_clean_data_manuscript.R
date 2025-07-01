# File purpose: Clean data and generate summary tables for rmd
# File author: Nanette Nelson

# Load libraries
library(tidyverse)
library(here)
##### clean data and generate summary tables ######

# Reminder: Delete rows 2 & 3 in downloaded data file before importing
mydata <- read.csv(
  here("data/raw_data/EPA_fish_consumption.csv"))
mydata <- dplyr::select(mydata, Q2:Q45)

# add an id number
mydata <- mydata %>%
  mutate(id = 1:n()) %>%
  relocate(id)

# convert character vectors to numeric
mydata <- mydata %>% mutate_if(is.character, as.numeric)

### filter data for eligible respondents - 
# 55 years of age or younger
mydata <- filter(mydata, Q43_1_TEXT < 56)
mydata <- filter(mydata, Q43_1_TEXT != -99)

# Potential respondents intercepted at food pantries
answers <- c("Yes", "No", "Don't know/Not sure", "Preferred not to answer")

coding <- function(x) {
  if_else(x==1, "Yes", if_else(x==2,"No", "Don't know/Not sure"))
}
mydata <- mydata %>%
  mutate_at(vars("Q2","Q4", "Q6"), list(factor=coding))

mydata <- mydata %>%  
  mutate(across(c(Q2_factor, Q4_factor, Q6_factor), ~factor(.x, levels=answers))) 

# Replace -99 with NA
mydata <- mydata %>%
  mutate(across(where(is.numeric), ~na_if(., -99)))

### generate new variables as factors ###
# tribal enrollment
tribal <- c("Salish", "Kootenai", "Pend d'Oreille","Other", "Don't know/Prefer not to answer")
mydata <- mydata %>%
  mutate(
    Q3_factor = case_when(
      Q2 == 1 & Q3 == 1 ~ "Salish",
      Q2 == 1 & Q3 == 2   ~ "Kootenai",
      Q2 == 1 & Q3 == 3  ~ "Pend d'Oreille",
      Q2 == 1 & Q3 == 4 ~ "Don't know/Prefer not to answer"),
    Q3_factor = factor(Q3_factor, levels = tribal)
  )
mydata <- mydata %>%
  mutate(
    Tribal.factor = case_when(
      Q2 == 1 & Q3 == 1 ~ "Salish",
      Q2 == 1 & Q3 == 2   ~ "Kootenai",
      Q2 == 1 & Q3 == 3  ~ "Pend d'Oreille",
      Q4 == 1 ~ "Other"),
    Tribal.factor = factor(Tribal.factor, levels = tribal)
    )

# for those who replied yes to eating fish, but number of meals was recorded as zero, assumed "1 or fewer" meals per month
fish_meals <- c("Zero", "One meal or less", "2-3 meals", "4-7 meals", "8 or more")
mydata <- mydata %>%
  mutate(
    Meal.factor = case_when(
      is.na(Q13_1_TEXT) ~ "Zero",
      Q13_1_TEXT == 0 ~ "One meal or less",
      Q13_1_TEXT == 1 ~ "One meal or less",
      between(Q13_1_TEXT,2,3) ~ "2-3 meals",
      between(Q13_1_TEXT,4,7) ~ "4-7 meals",
      TRUE ~ "8 or more"),
    Meal.factor = factor(Meal.factor, levels = fish_meals)
    )
# Size of catch
catch_size <- c("Small (less than 14 in.)", "Medium (14 to 26 in.)", "Large (over 26 in.)", "Don't know/Not sure", "Preferred not to answer")
mydata <- mydata %>%
  mutate(
    Catch.factor = case_when(
      Q32 == 1 ~ "Small (less than 14 in.)",
      Q32 == 2 ~ "Medium (14 to 26 in.)",
      Q32 == 3 ~ "Large (over 26 in.)",
      Q32 == 4 ~ "Don't know/Not sure",
      Q32 == 5 ~ "Preferred not to answer"),
    Catch.factor = factor (Catch.factor, levels  = catch_size)
    )
# Knowledge of Hg contamination
Hg_know <- c("A lot", "Some", "Very little", "Nothing at all", "Don't know/Not sure", "Preferred not to answer")
mydata <- mydata %>%
  mutate(
    HgKnow.factor = case_when(
      Q34 == 1 ~ "A lot",
      Q34 == 2  ~ "Some",
      Q34 == 3 ~ "Very little",
      Q34 == 4 ~ "Nothing at all",
      Q34 == 5 ~ "Don't know/Not sure",
      Q34 == 6 ~ "Preferred not to answer"),
    HgKnow.factor = factor(HgKnow.factor, levels = Hg_know)
    )
# Knowledge of fish species
mydata <- mydata %>%
  mutate(
    Species.factor = case_when(
      Q38 == 1 ~ "A lot",
      Q38 == 2  ~ "Some",
      Q38 == 3 ~ "Very little",
      Q38 == 4 ~ "Nothing at all",
      Q38 == 5 ~ "Don't know/Not sure",
      Q38 == 6 ~ "Preferred not to answer"),
    Species.factor = factor(Species.factor, levels = Hg_know)
    )
# Knowledge of size restrictions
mydata <- mydata %>%
  mutate(
    Size.factor = case_when(
      Q39 == 1 ~ "A lot",
      Q39 == 2  ~ "Some",
      Q39 == 3 ~ "Very little",
      Q39 == 4 ~ "Nothing at all",
      Q39 == 5 ~ "Don't know/Not sure",
      Q39 == 6 ~ "Preferred not to answer"),
    Size.factor = factor(Size.factor, levels = Hg_know)
    )
# Knowledge of amount of fish one can eat
mydata <- mydata %>%
  mutate(
    Quantity.factor = case_when(
      Q40 == 1 ~ "A lot",
      Q40 == 2  ~ "Some",
      Q40 == 3 ~ "Very little",
      Q40 == 4 ~ "Nothing at all",
      Q40 == 5 ~ "Don't know/Not sure",
      Q40 == 6 ~ "Preferred not to answer"),
    Quantity.factor = factor(Quantity.factor, levels = Hg_know)
    )
# Visits to food pantry
visits <- c("Every month", "Every other month", "Once every 3 months", "Infrequently", "Don't know/Not sure", "Preferred not to answer")
mydata <- mydata %>%
  mutate(
    Visit.factor = case_when(
      Q42 == 1 ~ "Every month",
      Q42 == 2 ~ "Every other month",
      Q42 == 3 ~ "Once every 3 months",
      Q42 == 4 ~ "Infrequently",
      Q42 == 5 ~ "Don't know/Not sure",
      Q42 == 6 ~ "Preferred not to answer"),
    Visit.factor = factor (Visit.factor, levels = visits)
    )
# Age categories
ages <- c("Under 18", "18-25", "26-35", "36-45", "46-55", "55+", "Preferred not to answer")
mydata <- mydata %>%
  mutate(
    Age.factor = case_when(
      is.na(Q43_1_TEXT) ~ "Preferred not to answer",
      between(Q43_1_TEXT,0,17)  ~ "Under 18",
      between(Q43_1_TEXT,18,25) ~ "18-25",
      between(Q43_1_TEXT,26,35) ~ "26-35",
      between(Q43_1_TEXT,36,45) ~ "36-45",
      between(Q43_1_TEXT,46,55) ~ "46-55",
      TRUE ~ "55+"),
    Age.factor = factor(Age.factor, levels = ages)
  )
# Relationship categories
relation <- c("Single", "Married", "Member of an unmarried couple", "Divorced/Separated", "Don't know/Not sure", "Preferred not to answer")
mydata <- mydata %>%
  mutate(
    Relationship.factor = case_when(
      Q44 == 1 ~ "Single",
      Q44 == 2 ~ "Married",
      Q44 == 3 ~ "Member of an unmarried couple",
      Q44 == 4 ~ "Divorced/Separated",
      Q44 == 5 ~ "Don't know/Not sure",
      TRUE ~ "Preferred not to answer"),
    Relationship.factor = factor(Relationship.factor, levels = relation)
    )
# Education categories
education <- c("Less than high school", "High school or GED", "Associates degree", "Bachelors degree", "Graduate degree", "Don't know/Not sure", "Preferred not to answer")
mydata <- mydata %>%
  mutate(
    Education.factor = case_when(
      Q45 == 1 ~ "Less than high school",
      Q45 == 2 ~ "High school or GED",
      Q45 == 3 ~ "Associates degree",
      Q45 == 4 ~ "Bachelors degree",
      Q45 == 5 ~ "Graduate degree",
      Q45 == 6 ~ "Don't know/Not sure",
      TRUE ~ "Preferred not to answer"),
    Education.factor = factor(Education.factor, levels = education)
    )
# Yes, no and don't know questions
answers <- c("Yes", "No", "Don't know/Not sure", "Preferred not to answer")

coding <- function(x) {
  if_else(x==1, "Yes", if_else(x==2,"No", "Don't know/Not sure"))
}
mydata <- mydata %>%
  mutate_at(vars("Q14":"Q31", "Q35"), list(factor=coding))

mydata <- mydata %>%  
  mutate(across(c(Q14_factor:Q31_factor, Q35_factor), ~factor(.x, levels=answers))) 

# Eats fish
##fish_meals <- c("Zero", "Less than one", "One meal", "2-4 meals", "5-8 meals", "9 or more")
mydata <- mydata %>%
  mutate(Fish.eater = ifelse(`Meal.factor` == "Zero", 0, 1))
### Save cleaned data file
write.csv(mydata,"data/clean_data/EPA_Hg_clean.csv")

### Function that generates summary tables ###
# Enrolled member of CSKT  

Enrolled.CSKT.out <- mydata %>%
  count(Q2_factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))

# Tribal identification among CSKT
Tribe.CSKT.out <- mydata %>%
  filter(!is.na(Q3_factor)) %>%
  count(Q3_factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))

# Enrolled in any American Indian tribe
Enrolled.Other.out <- mydata %>%
  filter(!is.na(Q4_factor)) %>%
  count(Q4_factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))

# Tribal affiliation -all- of surveyed individuals
Tribal.out <- mydata %>%
  filter(!is.na(Tribal.factor)) %>%
  count(Tribal.factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))

### filter data for eligible respondents - 
# enrolled in CSKT or other recognized tribe
mydata <- filter(mydata, Q9 == 1)

# Meal frequency 
Meal.out <- mydata %>%
  count(Meal.factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))%>%
  group_by(Meal.factor)

### Fish products bought at markets
# Shellfish
Market.Shellfish.out <- mydata %>%
  filter(!is.na(Q14_factor)) %>%
  count(Q14_factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
# Fish fillets, fish sticks, or fish sandwiches
Market.Fillets.out <- mydata %>%
  filter(!is.na(Q16_factor)) %>%
  count(Q16_factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
# Light tuna
Market.Tuna.out <- mydata %>%
  filter(!is.na(Q19_factor)) %>%
  count(Q19_factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
# Albacore tuna
Market.Albacore.out <- mydata %>%
  filter(!is.na(Q20_factor)) %>%
  count(Q20_factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
### Fish products received from the food pantry
# Shellfish
Pantry.Shellfish.out <- mydata %>%
  filter(!is.na(Q21_factor)) %>%
  count(Q21_factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
Pantry.Fillets.out <- mydata %>%
  filter(!is.na(Q22_factor)) %>%
  count(Q22_factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
# Light tuna
Pantry.Tuna.out <- mydata %>%
  filter(!is.na(Q24_factor)) %>%
  count(Q24_factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
# Albacore tuna
Pantry.Albacore.out <- mydata %>%
  filter(!is.na(Q25_factor)) %>%
  count(Q25_factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
# Ate fish donated to food pantry (Q26)
Donated.fish.out <- mydata %>%
  filter(!is.na(Q26_factor)) %>%
  count(Q26_factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
# Ate donated Lake Trout (Q27)
Donated.lake.trout.out <- mydata %>%
  filter(!is.na(Q27_factor)) %>%
  count(Q27_factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
# Ate donated whitefish (Q28)
Donated.whitefish.out <- mydata %>%
  filter(!is.na(Q28_factor)) %>%
  count(Q28_factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
# Ate angler caught fish (Q29) 
Caught.fish.out <- mydata %>%
  filter(!is.na(Q29_factor)) %>%
  count(Q29_factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
Caught.fish.out <- Caught.fish.out %>%
  rename(n.2021=n, percent.2021=percent)%>%
  group_by(Q29_factor)
Caught.fish.out <- setNames(Caught.fish.out, c("Q29_factor","n", "percent")) # rename columns in base R
# Ate angler caught lake trout (Q30)
Caught.lake.trout.out <- mydata %>%
  filter(!is.na(Q30_factor)) %>%
  count(Q30_factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
# Ate angler caught whitefish (Q31)
Caught.whitefish.out <- mydata %>%
  filter(!is.na(Q31_factor)) %>%
  count(Q31_factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
# Size of angler caught fish
Catch.Size.out <- mydata %>%
  filter(!is.na(Catch.factor)) %>%
  count(Catch.factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
### Knows about mercury contamination 
HgKnow.out <- mydata %>%
  count(HgKnow.factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
HgKnow.out <- HgKnow.out %>%
  rename(n.2021=n, percent.2021=percent) %>%  
  group_by(HgKnow.factor)
# rename columns in base R
HgKnow.out <- setNames(HgKnow.out, c("HgKnow.factor","n", "percent")) 
### Awareness of CSKT's fish advisory (Q35) 
Advisory.out <- mydata %>%
  count(Q35_factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
Advisory.out <- Advisory.out %>%
  rename(n.2021=n, percent.2021=percent) %>%
  group_by(Q35_factor)
# rename columns in base R
Advisory.out <- setNames(Advisory.out, c("Advisory.factor","n", "percent")) 
### Source for learning about CSKT's fish advisory
Knew <- as.vector(Advisory.out[1,2])  # total number of recipients that knew of advisory

Advisory.source <- mydata %>% 
  summarise(across(Q36_1:Q36_9, \(x) sum(x, na.rm=TRUE))) %>% 
  t()  # rotates columns to rows
Advisory.source <- as.data.frame(Advisory.source) 
  
colnames(Advisory.source)[1] <- "Count"
rownames(Advisory.source) <- NULL

Advisory.source <- Advisory.source %>%
  dplyr::select(Count) %>% 
  mutate(Percent=round((Count/19*100), digits=0))

Advisory.source <- cbind(c("TV", "Radio", "Internet", "Brochure", "Mack Days", 
                                 "Tribal government agencies", "Talking to doctors/health professional",
                                 "Talking to people you know", "Signs or posters"), 
                               Advisory.source)
colnames(Advisory.source)[1] <- "Source"

### Knowledge of fish species 
Species.out <- mydata %>%
  filter(!is.na(Species.factor)) %>%
  count(Species.factor, .drop=FALSE) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
Species.out <- Species.out %>% 
  filter(!row_number() %in% c(5,6))
Species.out <- Species.out %>%
  rename(n.2021=n, percent.2021=percent) %>%
  group_by(Species.factor)
# rename columns in base R
Species.out <- setNames(Species.out, c("Species.factor","n", "percent")) 
### Knowledge of fish size
Size.out <- mydata %>%
  filter(!is.na(Size.factor)) %>%
  count(Size.factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
Size.out <- Size.out %>%
  rename(n.2021=n, percent.2021=percent) %>%
  group_by(Size.factor)
# rename columns in base R
Size.out <- setNames(Size.out, c("Size.factor","n", "percent")) 
### Knowledge of amount of fish
Quantity.out <- mydata %>%
  filter(!is.na(Quantity.factor)) %>%
  count(Quantity.factor) %>%
  mutate(percent=round((n/sum(n)*100), digits = 0))
Quantity.out <- Quantity.out %>%
  rename(n.2021=n, percent.2021=percent) %>%
  group_by(Quantity.factor)
# rename columns in base R
Quantity.out <- setNames(Quantity.out, c("Quantity.factor","n", "percent")) 

### Demographics
# Number of visits to the food pantry per month
Visits.out <- mydata %>% 
  count(Visit.factor) %>% 
  mutate(percent=round((n/sum(n)*100), digits = 0))
# Age
Age.out <- mydata %>% 
  count(Age.factor) %>% 
  mutate(percent=round((n/sum(n)*100), digits = 0))
# Relationship status
Relationship.out <- mydata %>% 
  count(Relationship.factor) %>% 
  mutate(percent=round((n/sum(n)*100), digits = 0))
# Education
Education.out <- mydata %>% 
  count(Education.factor) %>% 
  mutate(percent=round((n/sum(n)*100), digits = 0))


