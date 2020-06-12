# Solomon islands dimension construction script

# Load packages
library(tidyverse)
library(car)

# Define functions

## Deprive
##
## The deprive function is used to allocate deprivation labels to raw indicator, 
## theme and dimension scores

deprive <- function (raw_score){
  raw_score <- raw_score * 4
  cats_score <- cut(raw_score ,breaks=c(-0.001,1.001,2.001,3.001,4.001),
           labels=factor(c("Most deprived","Deprived","Somewhat deprived","Least deprived")), ordered = TRUE)
  return(cats_score)}

## norm_IDM
##
## This function is used to normalise scores to a 1-4 range
norm_IDM <- function(score, min, max) {
  # Check data
  if (max(score, na.rm = TRUE) > max) warning('Maximum value for score is higher than the maximum provided to function')
  if (max(score, na.rm = TRUE) < min) warning('Minimum value for score is lower than the minimum provided to function')
  
  rescale <- ((score-min)/(max - min))
  return(rescale)
}

## Test dimension construction
dim_test <- function(x) {
  if(min(x, na.rm = TRUE) < 0) {
    return(2)
  } else if (max(x, na.rm = TRUE) > 1) {
    return(3)
  } else {
    return(1)
  }
}

## Test all scores
all_test <- function(data = data.ellipse()) {
  data %>%
    select(contains('score')) %>%
    map_int(dim_test) %>%
    return()
}

# Read in data

setwd("/Users/patrickrehill/Documents/IWDA Consultancy 2/data/Dimension construction")
data <- read.csv('~/Downloads/SI_Dwell_HH_Ind_renamed_20200521.csv')

# Incorporate other scripts

# WG disability
data$see1 <- ifelse(data$WG_see<=2, 0,1)
data$hear1 <- ifelse(data$WG_hear<=2, 0,1)
data$walk1 <- ifelse(data$WG_walking.climbing<=2, 0,1)
data$memory1 <- ifelse(data$WG_memory<=2, 0,1)
data$memory1[is.na(data$WG_memory)] <- 0 #is the only var with <NA>
data$selfcare1 <- ifelse(data$WG_selfcare<=2, 0,1)
data$comm1 <- ifelse(data$WG_communicate<=2, 0,1)
data$totaldisab1 <- data$see1 + data$hear1 + data$walk1 + data$memory1 + data$selfcare1 + data$comm1
data$disab1_bin <- ifelse(data$totaldisab1==0,0,1)
data$Disabled_ind <- factor(data$disab1_bin, levels = c(0,1),
                            labels = c("Without disabilities", "With disabilities"))

## Code provinces

data$Province_ind <- car::recode(data$Province_ind, "1 = 'Central'; 2 = 'Guadalcanal'")

## Age brackets

data$Age_bracket_ind <- cut(data$Age_ind, breaks = c(18, 24, 34, 44, 54, 64, 74, 84, 150),
                            ordered_result = TRUE, labels = c('18-24', '25-34', '35-44', 
                                                              '45-54', '55-64', '65-74', '75-84', 
                                                              '85+'))

data$Age_bracket_big_ind <- cut(data$Age_ind, breaks = c(18, 35, 65, 150),
                                ordered_result = TRUE, labels = c('18-34', '35-64', '65+'))

# Dimensions

## Dimension 1 - Food

### Select food variables

food_vars <- data %>% select(Food_insecurity.worried, 
                             Food_insecurity.low_nutrition,
                             Food_insecurity.low_variation,
                             Food_insecurity.skip_meal,
                             Food_insecurity.ate_less,
                             Food_insecurity.ran_out,
                             Food_insecurity.hungry,
                             Food_insecurity.wholeday)

### Determine highest scoring variable (apologies for the loop)

for (i in 1: nrow(food_vars)) {
  ### Find the 'highest' question answered yes
  food_row_which <- which(food_vars[i,] == 'Yes')
  
  ### Make sure data isn't 'refused to answer'
  if (all(is.na(food_vars[i,]))) {
    food_row_max <- NA
  } else if (all(food_vars[i,] == 'Refused to answer')) {
    food_row_max <- NA
  } else if (length(food_row_which) == 0) {
    food_row_max <- 0
  } else {
    food_row_max <- max(food_row_which)
  }

  data[i, 'food_raw_score'] <- food_row_max
}

#### Set indicator score
data$food_raw_score <- car::recode(data$food_raw_score, "0 = 4; 1:3 = 3; 4:6 = 2; 7:8 = 1")

data$score1.1.1 <- norm_IDM(data$food_raw_score, 1,4)

### Set theme and dimension scores

data$score1.1 <- data$score1.1.1
data$score1 <- data$score1.1

### Get deprivation categories
data$DepCat01 <- deprive(data$score1)


## Dimension 2 - Water

### Theme 1 - Drinking water source
#### Indicator - Drinking water source
##### We can find this indicator by summing the component variables
##### First we need to recode drinking water source

data$Water_drinking.source_recoded <- car::recode(
  data$Water_drinking.source,
  "'Treated water piped to dwelling' = 5;
  'Treated piped yard, neighbour, or public tap'	=	4;
  'Untreated water piped to dwelling'	=	3;
  'Untreated piped yard/neighbour/ public tap'	=	2;
  'Borehole/Tube well'	=	2;
  'Dug well (protected)' =	2;
  'Spring (protected)' =	2;
  'Rainwater'	=	2;
  'Bottled water' = 2;
  'Tanker/truck' = 2;
  'Water kiosk'	= 1;
  'Cart with small water tank' = 1;
  'Dug well (unprotected)' = 1;
  'Spring (unprotected)' = 1;
  'Surface water' = 1;
  'Refused to answer' = 2.5;
  'Untreated water piped to yard, neighbour, or public tap' = 2;
  'Borehole/Tube well 6 Dug well (protected)' = 2;
  else = NA"
)

##### Next reliability doesn't need recoding but we'll clean the 97 to the midpoint

data$Water_drinking.sufficient_recoded <- car::recode(data$Water_drinking.sufficient, 
                                                      "1 = 4; 2 = 3; 3 = 2; 4 = 1; 97 = 2.5; else = NA")

##### Recode water quality

# Water_drinking.treat_recoded <- 1.5
# Due to survey skip issue we'll just use midpoint for treatment

##### Then add the two together for an indicator score

data$water_drinking_raw <- data$Water_drinking.source_recoded + data$Water_drinking.sufficient_recoded +
  1.5

data$score2.1.1 <- norm_IDM(data$water_drinking_raw, 3, 11)
data$score2.1 <- data$score2.1.1

### Theme 2 - Domestic water source and reliability
#### Indicator 1 - Domestic water source and reliability

##### Source
data$Water_domestic.source_recoded <- car::recode(
  data$Water_domestic.source,
  "'Treated water piped to dwelling' = 5;
  'Treated piped yard/neighbour, or public tap'	=	4;
  'Treated water piped to yard, neighbour, or public tap' = 4;
  'Untreated water piped to dwelling'	=	3;
  'Untreated piped yard/neighbour/ public tap'	=	2;
  'Borehole/Tube well'	=	2;
  'Borehole/Tube well 6 Dug well (protected)' = 2;
  'Dug well (protected)' =	2;
  'Spring (protected)' =	2;
  'Rainwater'	=	2;
  'Bottled water' = 2;
  'Tanker/truck' = 2;
  'Water kiosk'	= 1;
  'Cart with small water tank' = 1;
  'Dug well (unprotected)' = 1;
  'Spring (unprotected)' = 1;
  'Surface water' = 1;
  'Refused to answer' = 2.5;
  'Untreated water piped to yard, neighbour, or public tap' = 2;
  else = NA;
  NA = NA"
)

##### Reliability

data$Water_domestic.sufficient_recoded <- car::recode(
  data$Water_domestic.sufficient,
  "'Always' = 4;
  'Most of the time' = 3;
  'Some of the time' = 2;
  'Never' = 1;
  'Refused to answer' = 2;
  else = NA"
)

##### Sum source and reliability

data$Water_domestic_sourcereliable <- data$Water_domestic.sufficient_recoded + 
           data$Water_domestic.source_recoded

##### Indicator score

data$score2.2.1 <- norm_IDM(data$Water_domestic_sourcereliable, 2, 9)

##### Theme score
data$score2.2 <- data$score2.2.1

### Theme 3 - Water collection threats

#### Indicator - Threats

##### Recode variables to allow us to sum them to their indicator scores

data$Water_collection.responsible_recode <- car::recode(
  data$Water_collection.responsible,
  "2 = 2;
  1 = 0;
  else = 0"
)

data$Water_collection.threats_recode <- car::recode(
  data$Water_collection.threats,
  "2 = 2;
  1 = 1;
  else = 0"
)

data$Water_collection <- data$Water_collection.responsible_recode + 
           data$Water_collection.threats_recode + 1

##### Refused to answer
data[(data$Water_collection.responsible == 97 |
      data$Water_collection.threats == 97) &
      !is.na(data$Water_collection.responsible) &
      !is.na(data$Water_collection.threats),
      'Water_collection'] <- 2

##### Indicator score
data$score2.3.1 <- norm_IDM(data$Water_collection, 1, 3)

#### Theme score
data$score2.3 <- data$score2.3.1

### Dimension score
data$score2 <- rowMeans(data[,c('score2.1', 'score2.2', 'score2.3')])
data$DepCat02 <- deprive(data$score2)




## Dimension 3 - Shelter

### Theme 1 - Habitability

#### Indicator 1 - Floor material

data$Shelter_habitability_floor <- car::recode(data$Shelter_habitability_floor.material, 
                                               "1:2 = 1; 3:4 = 2; 5:9 = 3; else = NA")

##### Indicator score
data$score3.1.1 <- norm_IDM(data$Shelter_habitability_floor, 1, 3)


#### Indicator 2 - Roof material

data$Shelter_habitability_roof <- car::recode(data$Shelter_habitability_roof.material, 
                                               "1=1;2:3 = 2; 4:7 = 3; 8:14 = 4; else = NA")

##### Indicator score
data$score3.1.2 <- norm_IDM(data$Shelter_habitability_roof, 1, 4)


#### Indicator 3 - Exterior Wall material

data$Shelter_habitability_walls <- car::recode(data$Shelter_habitability_walls.material, 
                                               "1=1;c(2,3,15)=2; 4:9=3; 17=3;10:14 = 4; 16=4; else = NA")

##### Indicator score
data$score3.1.3 <- norm_IDM(data$Shelter_habitability_walls, 1, 4)


#### Indicator 4 - Housing condition

##### First we need to clean the data which contain multiple resonses in one cell. 

data <- data %>%
  mutate(Shelter_habitability_condition.no_problems = grepl('4', Shelter_habitability_condition.problems),
         Shelter_habitability_condition.unsafe_structure = grepl('3', Shelter_habitability_condition.problems),
         Shelter_habitability_condition.wall_hole = grepl('2', Shelter_habitability_condition.problems),
         Shelter_habitability_condition.roof_leak = grepl('1', Shelter_habitability_condition.problems),
         
         # Next we allocate points for the indicator score.
         Shelter_habitability_condition.problem_points = Shelter_habitability_condition.wall_hole + 
           Shelter_habitability_condition.roof_leak + # Minor problem points
           (3 * Shelter_habitability_condition.unsafe_structure)
         )

##### Get indicator scores by subtracting problem points from the max possible indicator value

data$Shelter_habitability_condition <- 7 - data$Shelter_habitability_condition.problem_points
data$score3.1.4 <- norm_IDM(data$Shelter_habitability_condition, 1, 7)


#### Indicator - Crowdedness

data$Shelter_habitability_crowded <- car::recode(data$Shelter_habitability_crowded.too_crowded, 
                                              "1 = 1;2 = 2; 97 = 1.5; else = NA")
data$score3.1.5 <- norm_IDM(data$Shelter_habitability_crowded, 1, 2)

#### Calculate theme score

data$score3.1 <- rowMeans(data[,c('score3.1.1', 'score3.1.2', 'score3.1.3', 'score3.1.4', 'score3.1.5')])

### Theme 2 - Ownership of essential household items
#### Indicator 1 - Ownership of essential household items

# Filter for 'Yes' and 'Refused to answer' responses. Note that some vars are numeric.

item_ownership_vars <- data[,c('Water_vessels', 'Shelter_hhitems.bedding', 'Shelter.hhitems.utensils', 
                               'Shelter.hhitems.crockery')]
items_owned <- item_ownership_vars == 'Yes' | item_ownership_vars == 1

items_owned_refused <- item_ownership_vars == 'Refused to answer' | item_ownership_vars == 97

#### Indicator score
##### Sum number of items owned plus half of refused to answer responses
data$Shelter_hhitems_hhitems <- rowSums(items_owned) + (rowSums(items_owned_refused) / 2)

##### Normalise
data$score3.2.1 <- norm_IDM(data$Shelter_hhitems_hhitems, 0, 4)

#### Theme score
data$score3.2 <- data$score3.2.1

### Theme 3 - Security of tenure

#### Indicator 1 - Eviction concern
data$Shelter_security_worry.worried_recoded  <- car::recode(
  data$Shelter_security_worry.worried,
  "2 = 2;
  1 = 1;
  'yes' = 2;
  'Refused to answer' = 1.5;
  97 = 1.5;
  else = NA"
)


##### Indicator score
data$score3.3.1 <- norm_IDM(data$Shelter_security_worry.worried_recoded, 1, 2)

#### Indicator 2 - Recognition of ownership
data[data$Assets_dwelling == 2 & !is.na(data$Assets_dwelling), 
     'Shelter_security_recognition'] <- 2
data[data$Assets_dwelling == 1 & data$Assets_dwelling.recognised == 1 &
     !is.na(data$Assets_dwelling) & !is.na(data$Assets_dwelling.recognised),
  'Shelter_security_recognition'] <- 2
data[data$Assets_dwelling == 1 & data$Assets_dwelling.recognised == 2 &
       !is.na(data$Assets_dwelling) & !is.na(data$Assets_dwelling.recognised),
     'Shelter_security_recognition'] <- 1
data[data$Assets_dwelling == 1 & data$Assets_dwelling.recognised == 94 &
       !is.na(data$Assets_dwelling) & !is.na(data$Assets_dwelling.recognised),
     'Shelter_security_recognition'] <- 1
data[(data$Assets_dwelling == 97 | data$Assets_dwelling.recognised == 97) &
       !is.na(data$Assets_dwelling) & !is.na(data$Assets_dwelling.recognised),
     'Shelter_security_recognition'] <- 1.5

data$score3.3.2 <- norm_IDM(data$Shelter_security_recognition, 1, 2)


#### Indicator - Mortgage/rent stress
data[data$Shelter_security_stress.rent_mortgage == 'No' &
       !is.na(data$Shelter_security_stress.rent_mortgage),
     'Shelter_security_stress'] <- 4
data[data$Shelter_security_stress.rent_mortgage == 'Refused to answer' &
       !is.na(data$Shelter_security_stress.rent_mortgage),
     'Shelter_security_stress'] <- 2.5
data[data$Shelter_security_stress.rent_mortgage_ontime == 1 &
       !is.na(data$Shelter_security_stress.rent_mortgage_ontime),
     'Shelter_security_stress'] <- 4
data[data$Shelter_security_stress.rent_mortgage_ontime == 2 &
       !is.na(data$Shelter_security_stress.rent_mortgage_ontime),
      'Shelter_security_stress'] <- 3
data[data$Shelter_security_stress.rent_mortgage_ontime == 3 &
       !is.na(data$Shelter_security_stress.rent_mortgage_ontime),
     'Shelter_security_stress'] <- 2
data[data$Shelter_security_stress.rent_mortgage_ontime == 4 &
       !is.na(data$Shelter_security_stress.rent_mortgage_ontime),
       'Shelter_security_stress'] <- 1
data[data$Shelter_security_stress.rent_mortgage_ontime == 97 &
       !is.na(data$Shelter_security_stress.rent_mortgage_ontime),
     'Shelter_security_stress'] <- 2.5

data$score3.3.3 <- norm_IDM(data$Shelter_security_stress, 1, 4)

#### Theme score

data$score3.3 <- rowMeans(data[,c('score3.3.1', 'score3.3.2', 'score3.3.3')])

### Dimension score
data$score3 <- rowMeans(data[,c('score3.1', 'score3.2', 'score3.3')])
data$DepCat03 <- deprive(data$score3)





## Dimension 4 - Health

### Theme 1 - Health status

#### Indicator 1 - Physical health status
##### Long-term health problem
##### Sum together scores
data$Health_status_physical.problems_last_6months_recoded  <- car::recode(
  data$Health_status_physical.problems_last_6months,
  "'No' = 2;
  'Yes' = 0;
  'Refused to answer' = 1;
  else = NA"
)

##### Smoke related health problem
data$Health_status_physical.smoke_recoded  <- car::recode(
  data$Health_status_physical.smoke,
  "1 = 0;
  2 = 2;
  97 = 1;
  else = NA"
)

##### Short term health problems
data$Health_status_physical.problems_last_month_recoded  <- car::recode(
  data$Health_status_physical.problems_last_month,
  "1 = 0;
  2 = 1;
  97 = 0.5;
  else = NA"
)

##### Indicator score
data$Health_status_physical <- data$Health_status_physical.problems_last_month_recoded +
  data$Health_status_physical.problems_last_6months_recoded +
  data$Health_status_physical.smoke_recoded

data$score4.1.1 <- norm_IDM(data$Health_status_physical, 0,5)

##### Mental health
data$Health_status_mental.depression_frequency[data$Health_status_mental.depression_frequency == 97 &
                                                 !is.na(data$Health_status_mental.depression_frequency)] <- 3
data$Health_status_mental.anxiety_frequency[data$Health_status_mental.anxiety_frequency == 97 &
                                              !is.na(data$Health_status_mental.anxiety_frequency)] <- 3
data$Health_status_mental <- data$Health_status_mental.anxiety_frequency +
  data$Health_status_mental.depression_frequency

##### Indicator score
data$score4.1.2 <- norm_IDM(data$Health_status_mental, 2, 10)

##### Theme score
data$score4.1 <- rowMeans(data[,c('score4.1.1', 'score4.1.2')])


### Theme 2 - Health care

#### Indicator - General health care access and quality
##### Check if respondent didn't access healthcare
data[,'Health_care_general'] <- NA

##### If they did was it due to lack of need...
data[(data$Health_care_general.accessed == 'No' &
        data$Health_care_general_problems_notused.reason <= 2) &
        !is.na(data$Health_care_general.accessed) &
       !is.na(data$Health_care_general_problems_notused.reason), 'Health_care_general'] <- 9

##### Or barriers
data[(data$Health_care_general.accessed == 'No' &
        data$Health_care_general_problems_notused.reason <= 12) &
       !is.na(data$Health_care_general.accessed) &
       !is.na(data$Health_care_general_problems_notused.reason), 'Health_care_general'] <- 1

##### Or because they didn't respond?
data[(data$Health_care_general.accessed == 'No' &
        data$Health_care_general_problems_notused.reason <= 97) &
       !is.na(data$Health_care_general.accessed) &
       !is.na(data$Health_care_general_problems_notused.reason), 'Health_care_general'] <- 5

##### If they did access healthcare, how many problems did they have?

##### The respect question is phrased inversely to others so we need to recode it
data$Health_care_general_problems.respect <- car::recode(data$Health_care_general_problems.respect,
                                                         "1 = 2; 2 = 1; 97 = 97; else = NA")
health_care_problems <- data %>%
  dplyr::select(dplyr::contains('Health_care_general_problems.'))

health_care_problems_yes <- health_care_problems == 1
health_care_problems_refused <- health_care_problems == 97

data$Health_care_problems.points <- rowSums(health_care_problems_yes, na.rm = TRUE) +
  (rowSums(health_care_problems_refused, na.rm = TRUE) / 2)

##### Sum together positive responses and half of refused answers
data$Health_care_general[data$Health_care_general.accessed == 'Yes' &
                           !is.na(data$Health_care_general.accessed)] <- 9 -
  data$Health_care_problems.points[data$Health_care_general.accessed == 'Yes' &
                                  !is.na(data$Health_care_general.accessed)]

##### Deal with refused to answer
data[data$Health_care_general.accessed == 'Refused to answer' &
       !is.na(data$Health_care_general.accessed), 'Health_care_general'] <- 5

##### Indicator score
data$score4.2.1 <- norm_IDM(data$Health_care_general, 1, 9)


#### Indicator 2 - Prenatal care
##### Previous pregnanct
##### Set the scores of males to 9
data[data$Gender_ind == 'Male' & !is.na(data$Gender_ind), 'Health_care_prenatal'] <- 9

#### Set scores for those who aren't and haven't been in past 12 months
data[data$Gender_ind == 'Female' & data$Health_care.pregnant_12_months == 2
     & !is.na(data$Gender_ind) & !is.na(data$Health_care.pregnant_12_months),
     'Health_care_prenatal'] <- 9

#### Set score for previous pregnancy
##### Refused to answer
data[data$Health_birth.location == 97 &
       !is.na(data$Health_birth.location), 'Health_care_prenatal'] <- 5
##### Did not access prenatal care - couldn't access
##### Why is refused to answer here midpoint when every answer is high deprivation?
data[(data$Health_birth.location == 9 |
       data$Health_birth.location == 10 |
       data$Health_birth.location == 11) &
       !is.na(data$Health_birth.location)
       , 'Health_care_prenatal'] <- 1

##### If they went, check how many problems
birth_care_problems <- data %>%
  dplyr::select(dplyr::contains('Health_birth_problems.'))

##### Recode respect
data$Health_birth_problems.respect <- car::recode(data$Health_birth_problems.respect,
                                                         "1 = 2; 2 = 1; 97 = 97; else = NA")

birth_care_problems_yes <- rowSums(birth_care_problems == 1 | 
                                        birth_care_problems == 'Yes')
birth_care_problems_refused <- rowSums(birth_care_problems == 97 |
                                            birth_care_problems == 'Refused to answer')

##### Get problem points for everyone
data$birth_problems <- 9 - (birth_care_problems_yes +
  (0.5 * birth_care_problems_refused))

##### Sum together positive responses and half of refused answers
data[!is.na(data$birth_problems),
     'Health_care_prenatal'] <- data$birth_problems[!is.na(data$birth_problems)]

#### Current pregnancy

##### Refused to answer
data[data$Health_care_prenatal.recieved == 97, 'Health_care_prenatal'] <- 5
##### Did not access prenatal care - couldn't access
##### Why is refused to answer here midpoint when every answer is high deprivation?
data[data$Health_care_prenatal.recieved == 2, 'Health_care_prenatal'] <- 1

##### If they went, check how many problems
prenatal_care_problems <- data %>%
  dplyr::select(dplyr::contains('Health_care_prenatal_problems.'))

prenatal_care_problems_yes <- rowSums(prenatal_care_problems == 1 | 
                                        prenatal_care_problems == 'Yes', na.rm = TRUE)
prenatal_care_problems_refused <- rowSums(prenatal_care_problems == 97| 
                                            prenatal_care_problems == 'Refused to answer', na.rm = TRUE)

##### Get problem points for everyone
prenatal_problems <-9 - (prenatal_care_problems_yes +
  (0.5 * prenatal_care_problems_refused))

##### Sum together positive responses and half of refused answers
data[!is.na(prenatal_problems),
    'Health_care_prenatal'] <- prenatal_problems[!is.na(prenatal_problems)]

##### Construct indicator
data$score4.2.2 <- norm_IDM(data$Health_care_prenatal, 2, 9)

#### Construct theme
data$score4.2 <- rowMeans(data[,c('score4.2.1', 'score4.2.2')])

### Construct dimension
data$score4 <- rowMeans(data[,c('score4.1', 'score4.2')])
data$DepCat04 <- deprive(data$score4)




## Dimension 5 - Education
### Theme 1 - Education level
#### Indicator 1 - Education level
data$Education_level <- car::recode(data$Education_level_completion.highest,
                                    "1 = 1; 2 = 2; 3 = 3; 4 = 4; 6:7 = 6; 8:9 = 7; 97 = 3.5")

##### Construct indicator score
data$score5.1.1 <- norm_IDM(data$Education_level, 1, 7)

#### Construct theme score
data$score5.1 <- data$score5.1.1

### Theme 2 - Functional literacy and numeracy
education_marks <- read_csv('education_marks.csv') %>%
  select(i1903_mark, i1904_mark, i1905_mark, Identifier) %>%
  right_join(data, by = 'Identifier')

##### Recode missings
education_marks <- education_marks %>%
  select(i1903_mark, i1904_mark, i1905_mark) %>%
  map_df(~car::recode(.x, "1 = 1; 2 = 2; 3 = 3; 4 = 4; 96 = 1; 97 = 2.5; 99 = 2.5; else = NA"))

#### Indicator 1 - Literacy competency
data$Education_litnum_literacy.writing <- education_marks$i1903_mark
data$Education_litnum_literacy <- data %>%
  select(Education_litnum_literacy.writing) %>%
  mutate(Education_litnum_literacy.writing = as.numeric(Education_litnum_literacy.writing)) %>%
  norm_IDM(1, 4)

data$Education_litnum_literacy <- pull(data$Education_litnum_literacy)
data$Education_litnum_literacy[data$Education_litnum_literacy > 1] <- NA

data$score5.2.1 <- data$Education_litnum_literacy

#### Indicator 2 - Numeracy
data$Education_litnum_numeracy.add_subtract <- education_marks$i1904_mark
data$Education_litnum_numeracy.multiply_divide <- education_marks$i1905_mark
data$Education_litnum_numeracy <- data %>%
  select(Education_litnum_numeracy.add_subtract, Education_litnum_numeracy.multiply_divide) %>%
  mutate_all(as.numeric) %>%
  transmute(numeracy = Education_litnum_numeracy.add_subtract + 
              Education_litnum_numeracy.multiply_divide) %>%
  norm_IDM(1, 4)

data$Education_litnum_numeracy[data$Education_litnum_numeracy > 1] <- NA
data$Education_litnum_numeracy <- pull(data$Education_litnum_numeracy)

data$score5.2.2 <- data$Education_litnum_numeracy

#### Create theme score
data$score5.2 <- rowMeans(data[,c('score5.2.1', 'score5.2.2')])

### Create dimension score
data$score5 <- rowMeans(data[,c('score5.1', 'score5.2')])

##### DepCat
data$DepCat05 <- deprive(data$score5)




## Energy
### Theme 1 - Cooking energy
#### Indicator - Cooking energy source and reliability
cooking_sources <- car::recode(data$Energy_cooking.source,
                                    "
                                    'Electricity' = 4;
                                    'Natural gas/LPG' = 4;
                                    'Biogas' = 4;
                                    'Diesel/gasoline' = 1;
                                    'Alcohol/ethanol' = 1;
                                    'Kerosene/paraffin' = 1;
                                    'Coal/lignite' = 1;
                                    'Processed biomass (pellets) or woodchips' = 1;
                                    'Charcoal' = 1;
                                    'Wood' = 1;
                                    'Straw/shrubs/grass/crop residue' = 1;
                                    'Animal dung' = 1;
                                    'Garbage/plastic' = 1;
                                    'Sawdust' = 1;
                                    'None' = 1;
                                    'Refused to answer' = 2.5;
                                    else = NA
                                    ")

##### Reliability
cooking_reliability <- car::recode(data$Energy_cooking.needs_met, 
                           "1 = 4; 2 = 3; 3 = 2; 4 = 1; 5 = 4; 97 = 2.5")

##### Combine for indicator scores
data$Energy_cooking <- cooking_sources + cooking_reliability

##### Deal with cases where people don't cook or don't have energy sources
data[data$Energy_cooking.source == 'We do not cook our own food here' &
       !is.na(data$Energy_cooking.source), 
                    'Energy_cooking'] <- 8

data[data$Energy_cooking.source == 'We do not have energy for cooking, even if we need it' &
       !is.na(data$Energy_cooking.source), 
                    'Energy_cooking'] <- 1

##### Indicator score
data$score6.1.1 <- norm_IDM(data$Energy_cooking, 1, 8)

#### Theme score
data$score6.1 <- data$score6.1.1


### Theme 2 - Lighting energy
#### Indicator - Lighting source and reliability
lighting_sources <- car::recode(data$Energy_lighting.source,
                              "
                              'Electricity' = 4;
                              'Natural gas/LPG' = 4;
                              'Biogas' = 4;
                              'Solar' = 4;
                              'Diesel/gasoline' = 1;
                              'Alcohol/ethanol' = 1;
                              'Kerosene/paraffin' = 1;
                              'Candles' = 1;
                              'Torches' = 1;
                              'Wood' = 1;
                              'Refused to answer' = 2.5;
                              else = NA
                              ")

##### Reliability
lighting_reliability <- car::recode(data$Energy_lighting.needs_met, 
                           "'Always' = 4; 'Most of the time' = 3; 'Some of the time' = 2; 
                           'Never' = 1; 'Refused to answer' = 2.5")

##### Combine for indicator scores
data$Energy_lighting <- lighting_sources + lighting_reliability

##### Deal with cases where people don't cook or don't have energy sources
data[data$Energy_lighting.source == 'There is no lighting in the household' &
       !is.na(data$Energy_lighting.source), 
                    'Energy_lighting'] <- 1

##### Indicator score
data$score6.2.1 <- norm_IDM(data$Energy_lighting, 1, 8)

#### Theme score
data$score6.2 <- data$score6.2.1


### Heating energy
heating_sources <- car::recode(data$Energy_heating.source,
                                "
                                'Electricity' = 4;
                                'Natural gas/LPG' = 4;
                                'Biogas' = 4;
                                'Diesel/gasoline' = 1;
                                'Alcohol/ethanol' = 1;
                                'Kerosene/paraffin' = 1;
                                'Coal/lignite' = 1;
                                'Processed biomass (pellets) or woodchips' = 1;
                                'Charcoal' = 1;
                                'Wood' = 1;
                                'Straw/shrubs/grass/crop residue' = 1;
                                'Animal dung' = 1; 
                                'Garbage/plastic' = 1;
                                'Sawdust' = 1;
                                'Refused to answer' = 2.5;
                                else = 0
                                ")

##### Reliability
heating_reliability <- car::recode(data$Energy_heating.needs_met, 
                            "1 = 4; 2 = 3; 3 = 2; 
                            4 = 1; 5 = NA; 97 = 2.5")

##### Combine for indicator scores
data$Energy_heating <- heating_sources + heating_reliability

##### Deal with cases where people don't cook or don't have energy sources
data[data$Energy_heating.source == 'We do not have energy for cooking, even if we need it' &
       !is.na(data$Energy_heating.source), 
     'Energy_heating'] <- 1

data[data$Energy_heating.needs_met == 5 &
       !is.na(data$Energy_heating.needs_met), 
     'Energy_heating'] <- 8

##### Indicator score
data$score6.3.1 <- norm_IDM(data$Energy_heating, 1, 8)

#### Theme score
data$score6.3 <- data$score6.3.1


### Theme 4 - Energy collection threats

responsible_collection <- car::recode(data$Energy_collection.responsible,
                                      "2 = 2; 1 = 1; 97 = NA")

experience_threats <- car::recode(data$Energy_collection.threats,
                                      "'No' = 2; 'Yes' = 1; 'Refused to answer' = 1.5; else = 0")

data$Energy_collection <- responsible_collection + experience_threats -  1

##### Refused to answer
data[(data$Energy_collection.responsible == 97 |
      data$Energy_collection.threats == 97) &
       !is.na(data$Energy_collection.responsible) &
       !is.na(data$Energy_collection.threats), 
     'Energy_heating'] <- 2

##### Indicator score
data$score6.4.1 <- norm_IDM(data$Energy_collection, 1, 3)

#### Theme score
data$score6.4 <- data$score6.4.1

### Dimension score
data$score6 <- rowMeans(data[, c('score6.1', 'score6.2', 'score6.3', 'score6.4')])
data$DepCat06 <- deprive(data$score6)




## Dimension 7 - Sanitation
### Theme 1 - Toilet facilities
#### Indicator - Toilet type
##### Initialise empty series
data$Sanitation_toilet_type <- NA

##### Recode toilet type to improved and unimproved scores
toilet_type <- car::recode(data$Sanitation_toilet_type.type, "1 = 4; 2 = 4; 3 = 4; 4 = 2; 5 = 2;
  6 = 4; 7 = 4; 8 = 2; 9 = 4; 10 = 2; 11 = 2; 12 = 1; 97 = 3; else = NA")

##### Recode sufficient water to improved and unimproved scores
toilet_water <- car::recode(data$Sanitation_toilet_type.type, "1 = 1; 2 = 0; 97 = 0.5; else = 0")

##### Create indicator score
data$Sanitation_toilet_type <- toilet_type + toilet_water
data$score7.1.1 <- norm_IDM(data$Sanitation_toilet_type, 1, 5)


#### Indicator - Toilet ownership
# TODO: There is no refused to answer
##### Initialise with NA series
data$Sanitation_toilet_ownership <- NA

##### Set scores to 1 for those without toilets
data[data$Sanitation_toilet_type.type == 12 &
       !is.na(data$Sanitation_toilet_type.type),
     'Sanitation_toilet_ownership'] <- 1

##### Set scores for those with private toilets
data[data$Sanitation_toilet_ownership.shared == 2 &
     !is.na(data$Sanitation_toilet_ownership.shared), 
     'Sanitation_toilet_ownership'] <- 4

##### Set scores for those with public, permission and non-permission toilets
data[data$Sanitation_toilet_ownership.public == 1 &
       !is.na(data$Sanitation_toilet_ownership.public), 
     'Sanitation_toilet_ownership'] <- 2

data[data$Sanitation_toilet_ownership.shared == 1 & 
       data$Sanitation_toilet_ownership.public == 2 &
       !is.na(data$Sanitation_toilet_ownership.shared) &
       !is.na(data$Sanitation_toilet_ownership.public), 
     'Sanitation_toilet_ownership'] <- 3

##### Indicator score
data$score7.1.2 <- norm_IDM(data$Sanitation_toilet_ownership, 1, 4)

#### Theme score
data$score7.1 <- rowMeans(data[,c('score7.1.1', 'score7.1.2')])


### Theme 2 - Washing facilities
#### Indicator - Handwashing facilities
##### Facility scores
data[data$Sanitation_washing_hands.place_to_wash == 2 &
     !is.na(data$Sanitation_washing_hands.place_to_wash),
     'Sanitation_washing_hands'] <- 1

data[data$Sanitation_washing_hands.place_to_wash == 97 &
     !is.na(data$Sanitation_washing_hands.place_to_wash), 
     'Sanitation_washing_hands'] <- 2.5

##### Sufficient water
# TODO: Coding guide doesn't mention sometimes response
data[data$Sanitation_washing_hands.enough_water == 2 &
      !is.na(data$Sanitation_washing_hands.enough_water), 
     'Sanitation_washing_hands'] <- 2

data[data$Sanitation_washing_hands.enough_water == 97 &
     !is.na(data$Sanitation_washing_hands.enough_water),
     'Sanitation_washing_hands'] <- 2.5

##### Soap
# TODO: Coding guide doesn't mention sometimes response
data[data$Sanitation_washing_hands.other_substances == 1 &
     !is.na(data$Sanitation_washing_hands.other_substances),
     'Sanitation_washing_hands'] <- 4

data[(data$Sanitation_washing_hands.other_substances == 2 | 
    data$Sanitation_washing_hands.other_substances == 3) &
    !is.na(data$Sanitation_washing_hands.other_substances),
    'Sanitation_washing_hands'] <- 3

data[data$Sanitation_washing_hands.other_substances == 97 &
     !is.na(data$Sanitation_washing_hands.other_substances),
     'Sanitation_washing_hands'] <- 3.5

##### Indicator score

data$score7.2.1 <- norm_IDM(data$Sanitation_washing_hands, 1, 4)


#### Toiletries
data$Sanitation_washing_toiletries <- car::recode(data$Sanitation_washing_toiletries.sufficient, 
                                                  "1 = 4; 2 = 3; 3 = 2; 4 = 1; 97 = 2.5; else = NA")

##### Indicator score
data$score7.2.2 <- norm_IDM(data$Sanitation_washing_toiletries, 1, 4)

#### Theme score
data$score7.2 <- rowMeans(data[,c('score7.2.1', 'score7.2.2')])


### Theme 3 - Private changing place
#### Indicator - Private changing place
##### Male or haven't had period
data[(data$Clothing_sanitaryproduct_sanitaryproduct.recentmenstruation == 2 |
      data$Gender_ind == 'Male') &
     !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.recentmenstruation),
     'Sanitation_privatechanging'] <- 2

data[is.na(data$Clothing_sanitaryproduct_sanitaryproduct.recentmenstruation),
     'Sanitation_privatechanging'] <- 2

data[data$Clothing_sanitaryproduct_sanitaryproduct.recentmenstruation == 97 &
     !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.recentmenstruation),
     'Sanitation_privatechanging'] <- 1.5

##### Had privacy during last period
data[data$Clothing_sanitaryproduct_sanitaryproduct.privacy == 1 &
     !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.privacy),
     'Sanitation_privatechanging'] <- 2

data[data$Clothing_sanitaryproduct_sanitaryproduct.privacy == 2 &
     !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.privacy),
     'Sanitation_privatechanging'] <- 1

data[data$Clothing_sanitaryproduct_sanitaryproduct.privacy == 97 &
     !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.privacy),
     'Sanitation_privatechanging'] <- 1.5

##### Indicator score
data$score7.3.1 <- norm_IDM(data$Sanitation_privatechanging, 1, 2)

#### Theme score
data$score7.3 <- data$score7.3.1
### Dimension score
data$score7 <- rowMeans(data[,c('score7.1', 'score7.2', 'score7.3')])
data$DepCat07 <- deprive(data$score7)





## Dimension 8 - Relationships
### Theme 1 - Dependence and support
#### Indicator 1 - Dependence and support
# TODO: Check why there are no nos in can't provide for self
##### It's simpler to add together recoded scores rather than setting a score for every single outcome.
data$Relationships_dependence_dependence.depend_on_others_recode <- car::recode(
  data$Relationships_dependence_dependence.depend_on_others,
  "1 = 0; 2 = 16; else = 0")

data$Relationships_dependence_dependence.reciprocated_recode <- car::recode(
  data$Relationships_dependence_dependence.reciprocated,
  "1 = 4; 2 = 3; 3 = 2; 4 = 1; else = 0")

data$Relationships_dependence_dependence.available_recode <- car::recode(
  data$Relationships_dependence_dependence.available,
  "1 = 12; 2 = 8; 3 = 4; 4 = 0; else = 0")

##### Set refused to answer

relationship_variables <- c('Relationships_dependence_dependence.depend_on_others_recode',
                            'Relationships_dependence_dependence.available_recode',
                            'Relationships_dependence_dependence.reciprocated_recode'
                            )

data[rowSums(data[,relationship_variables] == 97) > 0 &
    !is.na(rowSums(data[,relationship_variables])),
     'Relationships_dependence'] <- 10.5

##### Set valid answers
##### Sorry for this awful code
data[rowSums(data[,relationship_variables] != 97) > 0 &
       !is.na(rowSums(data[,relationship_variables])),
     'Relationships_dependence'] <- rowSums(
       data[,relationship_variables])[rowSums(data[,relationship_variables] != 97) > 0 &
                                      !is.na(rowSums(data[,relationship_variables]))]

##### Set all NA responses to NA
data[data$Relationships_dependence == 0 &
       !is.na(data$Relationships_dependence),
     'Relationships_dependence'] <- NA

##### Set indicator scores
data$score8.1.1 <- norm_IDM(data$Relationships_dependence, 1, 20)

#### Set theme score
data$score8.1 <- data$score8.1.1


### Theme 2 - Participation in community events
#### Indicator 1 - Community event participation
data$Relationships_participation_events.attendance_recode <- car::recode(
                  data$Relationships_participation_events.attendance,
                  "1 = 7; 2 = 5; 3 = 3; 4 = 1; 5 = 7; 6 = 4.5; else = 0")
data$Relationships_participation_events.contributed_recode <- car::recode(
                  data$Relationships_participation_events.contributed,
                  "1 = 1; 2 = 0; else = 0")
data$Relationships_participation_events.reason_didnt_want_attend_recode <- car::recode(
                  data$Relationships_participation_events.reason_didnt_want_attend,
                  "1 = 0; 2 = 1; 3 = 1; 4 = 0; 5 = 1; 6 = 0; 7 = 0; else = 0"                                  )
data$Relationships_participation_events.reason_not_contributed_recode <- car::recode(
                  data$Relationships_participation_events.reason_not_contributed,
                  "1 = 0; 2 = 0; 3 = 1; 4 = 0; 97 = 0; else = 0")

data$Relationships_participation_events <- NA

##### Set scores for any refused to answer
event_participation_vars <- c('Relationships_participation_events.reason_not_contributed',
          'Relationships_participation_events.reason_didnt_want_attend',
          'Relationships_participation_events.contributed',
          'Relationships_participation_events.attendance'
          )

##### Add values
data$Relationships_participation_events <- rowSums(data[, paste0(event_participation_vars, '_recode')])

##### Add refused to answer responses
data[rowSums(data[,event_participation_vars] == 97) > 0 &
     !is.na(rowSums(data[,event_participation_vars])),
     'Relationships_participation_events'] <- 4.5

##### Set scores below minimum to NA (we used zero coding for NAs previously because we had)
##### No entirely valid indicator scores
data[data$Relationships_participation_events < 1 &
     !is.na(data$Relationships_participation_events),
     'Relationships_participation_events'] <- NA

##### Indicator score
data$score8.2.1 <- norm_IDM(data$Relationships_participation_events, 1, 8)


#### Indicator 2 - Participation during menstruation
##### Set scores for men
data[data$Gender_ind == 'Male' &
       !is.na(data$Gender_ind), 
     'Relationships_participation_menstruation'] <- 6

##### Set scores for non-menstruating women
data[(data$Clothing_sanitaryproduct_sanitaryproduct.recentmenstruation == 2 |
        data$Clothing_sanitaryproduct_sanitaryproduct.recentmenstruation == 99) &
       !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.recentmenstruation), 
     'Relationships_participation_menstruation'] <- 6

##### Set scores for always sufficient, never missing activities due to stigma
data[data$Clothing_sanitaryproduct_sanitaryproduct.sufficient == 1 &
     data$Clothing_sanitaryproduct_sanitaryproduct.taboo == 4 &
     !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.sufficient) &
       !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.taboo) ,
     'Relationships_participation_menstruation'] <- 6

##### Set scores for always sufficient, sometimes missing activities due to stigma
data[data$Clothing_sanitaryproduct_sanitaryproduct.sufficient == 1 &
       data$Clothing_sanitaryproduct_sanitaryproduct.taboo < 4 &
       !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.sufficient) &
       !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.taboo),
     'Relationships_participation_menstruation'] <- 5

##### Set scores for insufficient sanitary products, never missing school and work or activites
##### due to stigma
data[data$Clothing_sanitaryproduct_sanitaryproduct.sufficient == 1 &
       data$Clothing_sanitaryproduct_sanitaryproduct.interfere_activities == 4 &
       data$Clothing_sanitaryproduct_sanitaryproduct.taboo == 4 &
       !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.sufficient) &
       !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.taboo) &
       !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.interfere_activities),
     'Relationships_participation_menstruation'] <- 4

##### Set scores for insufficient sanitary products, never missing school and work but sometimes 
##### activites due to stigma
data[data$Clothing_sanitaryproduct_sanitaryproduct.sufficient == 1 &
       data$Clothing_sanitaryproduct_sanitaryproduct.interfere_activities == 4 &
       data$Clothing_sanitaryproduct_sanitaryproduct.taboo != 4 &
       !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.sufficient) &
       !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.taboo) &
       !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.interfere_activities),
     'Relationships_participation_menstruation'] <- 3

##### Set scores for insufficient sanitary products, sometimes missing school and work or activites
##### but not due to stigma
data[data$Clothing_sanitaryproduct_sanitaryproduct.sufficient == 1 &
       data$Clothing_sanitaryproduct_sanitaryproduct.interfere_activities != 4 &
       data$Clothing_sanitaryproduct_sanitaryproduct.taboo == 4 &
       !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.sufficient) &
       !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.taboo) &
       !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.interfere_activities),
     'Relationships_participation_menstruation'] <- 2

##### Set scores for insufficient sanitary products, sometimes missing school and work and sometimes 
##### activites due to stigma
data[data$Clothing_sanitaryproduct_sanitaryproduct.sufficient == 1 &
       data$Clothing_sanitaryproduct_sanitaryproduct.interfere_activities != 4 &
       data$Clothing_sanitaryproduct_sanitaryproduct.taboo != 4 &
       !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.sufficient) &
       !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.taboo) &
       !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.interfere_activities),
     'Relationships_participation_menstruation'] <- 1

##### Set refused to answer
events_menstruation_vars <- c(
  'Clothing_sanitaryproduct_sanitaryproduct.sufficient',
    'Clothing_sanitaryproduct_sanitaryproduct.interfere_activities',
    'Clothing_sanitaryproduct_sanitaryproduct.taboo'
)

# TODO: Is this midpoint estimate right?
data[(rowSums(data[,c(events_menstruation_vars)] == 97) > 0) &
     !is.na(rowSums(data[,c(events_menstruation_vars)])),
           'Relationships_participation_menstruation'] <- 5.5

##### Set indicator score
data$score8.2.2 <- norm_IDM(data$Relationships_participation_menstruation, 1, 6)

#### Set theme score
data$score8.2 <- rowMeans(data[,c('score8.2.1', 'score8.2.2')], na.rm = TRUE)

### Set dimension score
data$score8 <- rowMeans(data[,c('score8.1', 'score8.2')])
data$DepCat08 <- deprive(data$score8)




## Dimension 9 - Clothing
### Theme 1 - Basic footwear and clothing
#### Indicator - Basic footwear and clothing ownership
##### Initialise empty series
data$Clothing_basic_ownership <- NA

##### Own two sets of clothing and footwear
data[data$Clothing_basic_ownership.2clothes == 1 &
       data$Clothing_basic_ownership.2shoes == 1 &
       !is.na(data$Clothing_basic_ownership.2clothes) &
       !is.na(data$Clothing_basic_ownership.2shoes),
     'Clothing_basic_ownership'] <- 4

##### Own two sets of clothing but not footwear
data[data$Clothing_basic_ownership.2clothes == 1 &
       data$Clothing_basic_ownership.2shoes == 2 &
       !is.na(data$Clothing_basic_ownership.2clothes) &
       !is.na(data$Clothing_basic_ownership.2shoes),
     'Clothing_basic_ownership'] <- 3

##### Own two sets of footwear but not clothing
data[data$Clothing_basic_ownership.2clothes == 2 &
       data$Clothing_basic_ownership.2shoes == 1 &
       !is.na(data$Clothing_basic_ownership.2clothes) &
       !is.na(data$Clothing_basic_ownership.2shoes),
     'Clothing_basic_ownership'] <- 2

##### Own two sets of neither clothing nor footwear
data[data$Clothing_basic_ownership.2clothes == 2 &
       data$Clothing_basic_ownership.2shoes == 2 &
       !is.na(data$Clothing_basic_ownership.2clothes) &
       !is.na(data$Clothing_basic_ownership.2shoes),
     'Clothing_basic_ownership'] <- 1

##### Refused to answer
data[data$Clothing_basic_ownership.2clothes == 97 &
       !is.na(data$Clothing_basic_ownership.2clothes),
     'Clothing_basic_ownership'] <- 2.5

data[data$Clothing_basic_ownership.2clothes == 2 &
       data$Clothing_basic_ownership.2shoes == 2 &
       !is.na(data$Clothing_basic_ownership.2clothes) &
       !is.na(data$Clothing_basic_ownership.2shoes),
     'Clothing_basic_ownership'] <- 1

# TODO: How do refused to answer responses work?
##### Calculate indicator score
data$score9.1.1 <- norm_IDM(data$Clothing_basic_ownership, 1, 4)

#### Indicator 2 - Appropriate
##### Initialise empty series
data$Clothing_basic_acceptability <- NA

##### Both appropriate and protective
data[data$Clothing_basic_acceptability.appropriate >= 2 &
       data$Clothing_basic_acceptability.protective >= 2 &
       !is.na(data$Clothing_basic_acceptability.appropriate) &
       !is.na(data$Clothing_basic_acceptability.protective),
     'Clothing_basic_acceptability'] <- 4

##### Appropriate but not protective
data[data$Clothing_basic_acceptability.appropriate >= 2 &
       data$Clothing_basic_acceptability.protective < 2 &
       !is.na(data$Clothing_basic_acceptability.appropriate) &
       !is.na(data$Clothing_basic_acceptability.protective),
     'Clothing_basic_acceptability'] <- 3

##### Not appropriate but protective
data[data$Clothing_basic_acceptability.appropriate < 2 &
       data$Clothing_basic_acceptability.protective >= 2 &
       !is.na(data$Clothing_basic_acceptability.appropriate) &
       !is.na(data$Clothing_basic_acceptability.protective),
     'Clothing_basic_acceptability'] <- 2

##### Neither appropriate nor protective
data[data$Clothing_basic_acceptability.appropriate < 2 &
       data$Clothing_basic_acceptability.protective < 2 &
       !is.na(data$Clothing_basic_acceptability.appropriate) &
       !is.na(data$Clothing_basic_acceptability.protective),
     'Clothing_basic_acceptability'] <- 1

##### Refused to respond
data[rowSums(data[,c('Clothing_basic_acceptability.protective', 
                     'Clothing_basic_acceptability.appropriate')] == 97) > 0 &
      !is.na(rowSums(data[,c('Clothing_basic_acceptability.protective', 
                             'Clothing_basic_acceptability.appropriate')])) , 
     'Clothing_basic_acceptability'] <- 2.5

##### Calculate indicator score
data$score9.1.2 <- norm_IDM(data$Clothing_basic_acceptability, 1, 4)

#### Calculate theme 1 score
data$score9.1 <- rowMeans(data[,c('score9.1.1', 'score9.1.2')])


### Theme 2 - Other clothing and footwear
#### Indicator - School and work clothing
##### Initialise series
data$Clothing_other_formal <- NA

##### We'll sum scores from these variables to make the indicator
data$Clothing_other_schoolwork.sufficiency_recode <- car::recode(data$Clothing_other_schoolwork.sufficiency,
                                                                 "1 = 4; 2 = 1; else = NA")

data$Clothing_other_schoolwork.acceptability_recode <- car::recode(data$Clothing_other_schoolwork.acceptability,
                                                                 "1 = 1; 2 = 1; 3 = 0; 4 = 0; else = NA")

data$Clothing_other_schoolwork.protection_recode <- car::recode(data$Clothing_other_schoolwork.protection,
                                                                 "1 = 1; 2 = 1; 3 = 0; 4 = 0; else = NA")

##### Sum scores
data$Clothing_other_schoolwork <- data$Clothing_other_schoolwork.protection_recode +
  data$Clothing_other_schoolwork.acceptability_recode +
  data$Clothing_other_schoolwork.sufficiency_recode

##### No need for school or work clothing
data[(data$Clothing_other_schoolwork.sufficiency == 3 |
       data$Clothing_other_schoolwork.acceptability == 5 |
       data$Clothing_other_schoolwork.protection == 5) |
       is.na(data$Clothing_other_schoolwork.sufficiency) &
       is.na(data$Clothing_other_schoolwork.acceptability) &
       is.na(data$Clothing_other_schoolwork.protection),
     'Clothing_other_schoolwork'] <- 6

##### Refused to answer
data[rowSums(data[,c('Clothing_other_schoolwork.protection', 
                     'Clothing_other_schoolwork.acceptability',
                     'Clothing_other_schoolwork.sufficiency')] == 97) > 0 &
     !is.na(rowSums(data[,c('Clothing_other_schoolwork.protection', 
                     'Clothing_other_schoolwork.acceptability',
                     'Clothing_other_schoolwork.sufficiency')])), 
     'Clothing_other_schoolwork'] <- 3.5

##### Indicator score

data$score9.2.1 <- norm_IDM(data$Clothing_other_schoolwork, 1, 6)


#### Indicator - Formal clothing
##### We'll add together variable scores for this.
data$Clothing_other_formal.sufficiency_recode <- car::recode(data$Clothing_other_formal.sufficiency,
                                                             "1 = 3; 3 = 0; 97 = 1.5")

data$Clothing_other_formal.acceptability_recode <- car::recode(data$Clothing_other_formal.acceptability,
                                                             "1 = 4; 2 = 3; 3 = 2; 4 = 1; 97 = 2.5")

data$Clothing_other_formal <- data$Clothing_other_formal.sufficiency_recode +
  data$Clothing_other_formal.acceptability_recode

data$score9.2.2 <- norm_IDM(data$Clothing_other_formal, 1, 7)

#### Calculate Theme 2 score

data$score9.2 <- rowMeans(data[,c('score9.2.1', 'score9.2.2')])

### Theme 3 - Sanitary product use
##### Initialise empty series
data$Clothing_sanitaryproduct <- car::recode(data$Clothing_sanitaryproduct_sanitaryproduct.sufficient,
                                             "1 = 4; 2 = 3; 3 = 2; 4 = 1; else = NA")

##### Set scores to max for men and non-menstruating women
data[(data$Gender_ind == 'Male' | 
       data$Clothing_sanitaryproduct_sanitaryproduct.recentmenstruation == 2 |
       data$Clothing_sanitaryproduct_sanitaryproduct.recentmenstruation == 99 |
       is.na(data$Clothing_sanitaryproduct_sanitaryproduct.recentmenstruation)) &
       !is.na(data$Gender_ind), 
     'Clothing_sanitaryproduct'] <- 4

##### Refused to answer
data[(data$Clothing_sanitaryproduct_sanitaryproduct.recentmenstruation == 97 |
       data$Clothing_sanitaryproduct_sanitaryproduct.sufficient == 97) &
       !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.recentmenstruation) &
       !is.na(data$Clothing_sanitaryproduct_sanitaryproduct.sufficient),
     'Clothing_sanitaryproduct'] <- 2.5

##### Calculate indicator score
data$score9.3.1 <- norm_IDM(data$Clothing_sanitaryproduct, 1, 4)

#### Calculate Theme 3 score
data$score9.3 <- data$score9.3.1

### Calculate Dimension 9 score
data$score9 <- rowMeans(data[,c('score9.1', 'score9.2', 'score9.3')])

### Assign deprivation categories
data$DepCat09 <- deprive(data$score9)





## Dimension 11 - Family planning
### Theme 1 - Unmet need for contraception
#### Indicator 1 - Unment need for contraception

##### Initialise empty series

##### If using contraception
##### Traditional
data[!is.na(data$FamPlan_contraception_contraception.own_traditional) &
       data$FamPlan_contraception_contraception.own_traditional != 5,  
     'FamPlan_contraception'] <- 4

##### Modern
data[!is.na(data$FamPlan_contraception_contraception.own_modern) &
       data$FamPlan_contraception_contraception.own_modern != 13,  
     'FamPlan_contraception'] <- 7

##### Refused
data[(data$FamPlan_contraception_contraception.own_modern == 97 |
      data$FamPlan_contraception_contraception.own_tradition == 97) &
       !is.na(data$FamPlan_contraception_contraception.own_modern) &
       !is.na(data$FamPlan_contraception_contraception.own_traditional),  
     'FamPlan_contraception'] <- 4

##### Not using contraception
##### Doesn't know if partner is using
data[(data$FamPlan_contraception_contraception.own_use == 2 |
      data$FamPlan_contraception_contraception.own_use == 3) &
    data$FamPlan_contraception_contraception.partner_use == 4 &
      !is.na(data$FamPlan_contraception_contraception.own_use) &
      !is.na(data$FamPlan_contraception_contraception.partner_use),
    'FamPlan_contraception'] <- 1

##### Doesn't have a partner
data[(data$FamPlan_contraception_contraception.own_use == 2 |
        data$FamPlan_contraception_contraception.own_use == 3) &
       data$FamPlan_contraception_contraception.partner_use == 3 &
       !is.na(data$FamPlan_contraception_contraception.own_use) &
       !is.na(data$FamPlan_contraception_contraception.partner_use),
     'FamPlan_contraception'] <- 7

##### Male respondent's partner is using contraception
##### Traditional
data[(data$FamPlan_contraception_contraception.own_use == 2 |
        data$FamPlan_contraception_contraception.own_use == 3) &
       data$FamPlan_contraception_contraception.partner_use == 1 &
       data$Gender_ind == 'Male' &
       !is.na(data$FamPlan_contraception_contraception.partner_traditional) &
       !is.na(data$FamPlan_contraception_contraception.own_use) &
       !is.na(data$FamPlan_contraception_contraception.partner_use) &
       !is.na(data$Gender_ind),
     'FamPlan_contraception'] <- 3

##### Modern
data[(data$FamPlan_contraception_contraception.own_use == 2 |
        data$FamPlan_contraception_contraception.own_use == 3) &
       data$FamPlan_contraception_contraception.partner_use == 1 &
       data$Gender_ind == 'Male' &
       (!is.na(data$FamPlan_contraception_contraception.partner_modern) &
          data$FamPlan_contraception_contraception.partner_modern < 97) &
       !is.na(data$FamPlan_contraception_contraception.own_use) &
       !is.na(data$FamPlan_contraception_contraception.partner_use) &
       !is.na(data$Gender_ind),
     'FamPlan_contraception'] <- 6

##### Female respondent's partner is using contraception
##### Traditional
data[(data$FamPlan_contraception_contraception.own_use == 2 |
        data$FamPlan_contraception_contraception.own_use == 3) &
       data$FamPlan_contraception_contraception.partner_use == 1 &
       data$Gender_ind == 'Female' &
       !is.na(data$FamPlan_contraception_contraception.partner_traditional) &
       !is.na(data$FamPlan_contraception_contraception.own_use) &
       !is.na(data$FamPlan_contraception_contraception.partner_use) &
       !is.na(data$Gender_ind),
     'FamPlan_contraception'] <- 2

##### Modern
data[(data$FamPlan_contraception_contraception.own_use == 2 |
        data$FamPlan_contraception_contraception.own_use == 3) &
       data$FamPlan_contraception_contraception.partner_use == 1 &
       data$Gender_ind == 'Female' &
       (!is.na(data$FamPlan_contraception_contraception.partner_modern) &
          data$FamPlan_contraception_contraception.partner_modern < 97) &
       !is.na(data$FamPlan_contraception_contraception.own_use) &
       !is.na(data$FamPlan_contraception_contraception.partner_use) &
       !is.na(data$Gender_ind),
     'FamPlan_contraception'] <- 5

##### Neither partner is using contraception
##### Set options considered not deprived
not_deprived_contraception_choices <- c(1,2,3,4:7, 11, 18, 19, 21)
deprived_contraception_choices <- c(8:10, 12:17, 20)

data[(data$FamPlan_contraception_contraception.own_use == 2 |
      data$FamPlan_contraception_contraception.own_use == 3) &
       data$FamPlan_contraception_contraception.partner_use == 2 &
       data$FamPlan_contraception_contraception.reason_not %in% not_deprived_contraception_choices &
     
     !is.na(data$FamPlan_contraception_contraception.own_use) &
      !is.na(data$FamPlan_contraception_contraception.partner_use) &
      !is.na(data$FamPlan_contraception_contraception.reason_not),
     'FamPlan_contraception'] <- 7

##### Set deprived answers
data[(data$FamPlan_contraception_contraception.own_use == 2 |
        data$FamPlan_contraception_contraception.own_use == 3) &
       data$FamPlan_contraception_contraception.partner_use == 2 &
       data$FamPlan_contraception_contraception.reason_not %in% deprived_contraception_choices &
       
       !is.na(data$FamPlan_contraception_contraception.own_use) &
       !is.na(data$FamPlan_contraception_contraception.partner_use) &
       !is.na(data$FamPlan_contraception_contraception.reason_not),
     'FamPlan_contraception'] <- 1

##### Set refused to answer
data[(data$FamPlan_contraception_contraception.own_use == 97 |
      data$FamPlan_contraception_contraception.partner_use == 97) &
      !is.na(data$FamPlan_contraception_contraception.own_use) &
      !is.na(data$FamPlan_contraception_contraception.partner_use),
     'FamPlan_contraception'] <- 4

##### Set indicator score
data$score11.1.1 <- norm_IDM(data$FamPlan_contraception, 1, 7)

#### Set theme score
data$score11.1 <- data$score11.1.1

### Set dimension score
data$score11 <- data$score11.1
data$DepCat11 <- deprive(data$score11)







## Dimension 13 - Voice
### Theme 1 - Voice in the public domain
#### Indicator 1 - Voting
##### Initialise empty series
data$Voice_public_voting <- NA

##### Set scores for those who didn't vote or can't remember
data[(data$Voice_public_voting.voted_last_election == 2 |
      data$Voice_public_voting.voted_last_election == 3) &
      !is.na(data$Voice_public_voting.voted_last_election),
     'Voice_public_voting'] <- 1

##### Set scores for those who refused to answer
data[data$Voice_public_voting.voted_last_election == 97 &
    !is.na(data$Voice_public_voting.voted_last_election),
     'Voice_public_voting'] <- 2

##### Set scores for those who voted
##### Were free to choose
data[data$Voice_public_voting.voted_last_election == 1 &
       data$Voice_public_voting.free_choice == 1 &
       !is.na(data$Voice_public_voting.voted_last_election) &
       !is.na(data$Voice_public_voting.free_choice),
     'Voice_public_voting'] <- 3

##### Weren't free to choose
data[data$Voice_public_voting.voted_last_election == 1 &
       data$Voice_public_voting.free_choice == 2 &
       !is.na(data$Voice_public_voting.voted_last_election) &
       !is.na(data$Voice_public_voting.free_choice),
     'Voice_public_voting'] <- 1

##### Refused to say if they were free
data[data$Voice_public_voting.voted_last_election == 1 &
       data$Voice_public_voting.free_choice == 97 &
       !is.na(data$Voice_public_voting.voted_last_election) &
       !is.na(data$Voice_public_voting.free_choice),
     'Voice_public_voting'] <- 2

##### Set indicator score
data$score13.1.1 <- norm_IDM(data$Voice_public_voting, 1, 3)

#### Indicator 2 - Local decision making
##### Participated in local decision-making
##### A lot
data[data$Voice_public_local.participated == 1 &
     data$Voice_public_local.influence == 1 &
     !is.na(data$Voice_public_local.participated) &
     !is.na(data$Voice_public_local.influence),
     'Voice_public_local'] <- 4

##### Somewhere between a lot and a little
data[data$Voice_public_local.participated == 1 &
       data$Voice_public_local.influence == 3 &
       !is.na(data$Voice_public_local.participated) &
       !is.na(data$Voice_public_local.influence),
     'Voice_public_local'] <- 3

##### A little
data[data$Voice_public_local.participated == 1 &
       data$Voice_public_local.influence == 2 &
       !is.na(data$Voice_public_local.participated) &
       !is.na(data$Voice_public_local.influence),
     'Voice_public_local'] <- 2

##### Refused to say
data[data$Voice_public_local.participated == 1 &
       data$Voice_public_local.influence == 97 &
       !is.na(data$Voice_public_local.participated) &
       !is.na(data$Voice_public_local.influence),
     'Voice_public_local'] <- 3

##### Did not participate in local decisionmaking
##### Because there was no process to participate in
data[data$Voice_public_local.participated == 2 &
       data$Voice_public_local.reason_not_participate == 4 &
       !is.na(data$Voice_public_local.participated) &
       !is.na(data$Voice_public_local.reason_not_participate),
     'Voice_public_local'] <- 2

##### Other reason
data[data$Voice_public_local.participated == 2 &
       data$Voice_public_local.reason_not_participate != 4 &
       data$Voice_public_local.reason_not_participate != 97 &
       !is.na(data$Voice_public_local.participated) &
       !is.na(data$Voice_public_local.reason_not_participate),
     'Voice_public_local'] <- 1

##### Refused to answer
data[data$Voice_public_local.participated == 2 &
       data$Voice_public_local.reason_not_participate == 97&
       !is.na(data$Voice_public_local.participated) &
       !is.na(data$Voice_public_local.reason_not_participate),
     'Voice_public_local'] <- 1.5

##### Set indicator score
data$score13.1.2 <- norm_IDM(data$Voice_public_local, 1, 4)


#### Indicator 3 - Perception of concern raising
##### We'll add variable scores to construct this indicator
data$Voice_public_concerns.difficulty_recode <- car::recode(data$Voice_public_concerns.difficulty,
                                                            "1 = 4; 2 = 3; 3 = 2; 4 = 1; 97 = 2.5")

data$Voice_public_concerns.treated_seriously_recode <- car::recode(data$Voice_public_concerns.treated_seriously,
                                                            "1 = 3; 2 = 2; 3 = 1; 94 = 2; 97 = 2")

data$Voice_public_concerns <- data$Voice_public_concerns.difficulty_recode +
  data$Voice_public_concerns.treated_seriously_recode

##### Set indicator score
data$score13.1.3 <- norm_IDM(data$Voice_public_concerns, 2, 7)

#### Set theme score
data$score13.1 <- rowMeans(data[,c('score13.1.1', 'score13.1.2', 'score13.1.3')], na.rm = TRUE)


### Theme 2 - Personal control
#### Indicator - Personal control
##### Count the number of yeses
personal_control_vars <- data %>% select(starts_with('Voice_personal_personal.prevented')) %>%
  select(-Voice_personal_personal.prevented_interview_consent)
data$Voice_personal <- 6 - rowSums(personal_control_vars == 1, na.rm = TRUE)

##### Set refused to answer and other midpoints
##### Set scores for those who did not consent to participate in this section
data[data$Voice_personal_personal.prevented_interview_consent == 2 &
    !is.na(data$Voice_personal_personal.prevented_interview_consent),
     'Voice_personal'] <- 3

##### Those with any refused or interrupted
data[rowSums(personal_control_vars >= 97, na.rm = TRUE) > 0 &
      !is.na(rowSums(personal_control_vars >= 97, na.rm = TRUE)),
      'Voice_personal'] <- 3

##### Set indicator scores
data$score13.2.1 <- norm_IDM(data$Voice_personal, 0, 6)


#### Indicator 2 - Household decision making
##### Recode scores for sum
data$Voice_personal_decisionmaker <- data %>%
  select(contains('Voice_personal_personal.decisionmaker')) %>%
  map_df(~car::recode(.x, "1 = 5; 2 = 4; 3 = 1; 4 = 2; 5 = 3; 6 = 1; 94 = 3; 97 = 3")) %>%
  rowSums()

##### Set indicator scores
data$score13.2.2 <- norm_IDM(data$Voice_personal_decisionmaker, 8 , 40)

#### Set theme score
data$score13.2 <- rowMeans(data[,c('score13.2.1', 'score13.2.2')], na.rm = TRUE)

### Set dimension score
data$score13 <- rowMeans(data[,c('score13.1', 'score13.2')], na.rm = TRUE)
data$DepCat13 <- deprive(data$score13)



## Dimension 15 

### Theme 1 - Paid work
#### Indicator 1 - Employment status
##### For people who have not been employed in the past month
data$Work_paid_status <- car::recode(data$Work_paid_status.reason_not_worked,
                                     "1 = 4; 2 = 3; 3 = 2; 4 = 2; 5 = 1; 6 = 2; 7 = 2; 8 = 4; 
                                     9 = 2; 10 = 3; 11 = 4; 12 = 1; 13 = 1; 97 = 2.5; else = NA")

##### For people who have been in employment past week
data[data$Work_paid_status.past_week == 1 &
     !is.na(data$Work_paid_status.past_week),
     'Work_paid_status'] <- 4

##### For people who have been in employment past month but not week
data[data$Work_paid_status.past_week == 2 &
     data$Work_paid_status.past_month == 1 &
     !is.na(data$Work_paid_status.past_week) &
     !is.na(data$Work_paid_status.past_month),
     'Work_paid_status'] <- 3

##### Indicator score
data$score15.1.1 <- norm_IDM(data$Work_paid_status, 1, 4)


#### Indicator 2 - Job security
##### We can just sum variable scores for this
##### Recode number of jobs
data$Work_paid_security.number_jobs_recode <- car::recode(data$Work_paid_security.number_jobs,
                                                          "2:3 = 3; 4:5 = 2; 6:11 = 1; 97 = 1.5;
                                                          else = NA")

##### Employers forcing a job change
data$Work_paid_security.times_fired_recode <- car::recode(data$Work_paid_security.times_fired,
                                                          "'Very often' = 1; 'Often' = 2; 
                                                          'Not often' = 3; 'Never' = 4; 
                                                          'Refused to answer' = 2.5; else = NA")

##### Employment benefits
data$Work_paid_security.benefits_recode <- car::recode(data$Work_paid_security.benefits,
                                                          "1 = 2; 2 = 1; 97 = 1.5; else = NA")

##### Indicator score
data$Work_paid_security <- data$Work_paid_security.number_jobs_recode +
  data$Work_paid_security.times_fired_recode +
  data$Work_paid_security.benefits_recode

data$score15.1.2 <- norm_IDM(data$Work_paid_security, 3, 9)


#### Indicator 3 - Hazards
##### Sum number of hazards then recode
data$Work_paid_danger <- data %>%
  select(Work_paid_danger.exposed, Work_paid_danger.injury, Work_paid_danger.space) %>%
  transmute(dangers = 3 - rowSums(. == 'Yes'), refused = rowSums(. == 'Refused to answer')) %>%
  transmute(ifelse(refused > 0, 1.5, dangers))

data$score15.1.3 <- norm_IDM(data$Work_paid_danger, 0, 3)


#### Indicator 4 - Respect and autonomy
##### Recode breaks allowed to be consistent
data$Work_paid_respect.breaks_allowed_recode <- car::recode(data$Work_paid_respect.breaks_allowed,
                                                            "'No' = 'Yes'; 'Yes' = 'No';
                                                            'Refused to answer' = 'Refused to answer'")

##### Sum number of issues and half number of refused to answers
data$Work_paid_respect <- data %>%
  select(Work_paid_respect.breaks_allowed_recode, Work_paid_respect.sexual_harrassment,
         Work_paid_respect.physical_abuse, Work_paid_respect.humiliating) %>%
  transmute(issues = rowSums(. == 'Yes'), refused = rowSums(. == 'Refused to answer')) %>%
  transmute(4 - (issues + (refused/2)))

data$score15.1.4 <- norm_IDM(data$Work_paid_respect, 0, 4)

#### Theme score
data$score15.1 <- rowMeans(data[,c('score15.1.1', 'score15.1.2', 'score15.1.3', 'score15.1.4')],
                           na.rm = TRUE)



### Theme 2 - Unpaid work
#### Indicator 1 - Hazards
##### No injuries
data[data$Work_unpaid_injury.injured_12months == 2 &
     !is.na(data$Work_unpaid_injury.injured_12months),
      'Work_unpaid_injury'] <- 3

##### Refused to say
data[data$Work_unpaid_injury.injured_12months == 97 &
       !is.na(data$Work_unpaid_injury.injured_12months),
     'Work_unpaid_injury'] <- 2

##### If they had had an injury...
##### Did it cause time off?
##### No
data[data$Work_unpaid_injury.temp_effect == 'No' &
       !is.na(data$Work_unpaid_injury.temp_effect),
     'Work_unpaid_injury'] <- 3

##### Refused to answer
data[data$Work_unpaid_injury.temp_effect == 'Refused to answer' &
       !is.na(data$Work_unpaid_injury.temp_effect),
     'Work_unpaid_injury'] <- 2

##### If yes, was there a permanent effect?
##### No
data[data$Work_unpaid_injury.permanent_effect== 'No' &
       !is.na(data$Work_unpaid_injury.permanent_effect),
     'Work_unpaid_injury'] <- 2

##### Yes
data[data$Work_unpaid_injury.permanent_effect== 'Yes' &
       !is.na(data$Work_unpaid_injury.permanent_effect),
     'Work_unpaid_injury'] <- 1

##### Refused
data[data$Work_unpaid_injury.permanent_effect== 'Refused to answer' &
       !is.na(data$Work_unpaid_injury.permanent_effect),
     'Work_unpaid_injury'] <- 1.5


##### Indicator score
data$score15.2.1 <- norm_IDM(data$Work_unpaid_injury, 1, 3)


#### Indicator 2 - Valuing unpaid work
##### Here we can just sum recoded scores
data$Work_unpaid_respect.valued_recode <- car::recode(data$Work_unpaid_respect.valued,
                                                      "'Yes' = 1; 'No' = 0; 'Refused to answer' = 0.5")

data$Work_unpaid_respect.humiliating_recode <- car::recode(data$Work_unpaid_respect.humiliating,
                                                      "'Yes' = 0; 'No' = 1; 'Refused to answer' = 0.5")

data$Work_unpaid_respect <- data$Work_unpaid_respect.valued_recode +
  data$Work_unpaid_respect.humiliating_recode

##### Form indicator score
data$score15.2.2 <- norm_IDM(data$Work_unpaid_respect, 0, 2)

#### Theme score
data$score15.2 <- rowMeans(data[,c('score15.2.1', 'score15.2.2')], na.rm = TRUE)


### Theme 3 - Double work burden
#### Indicator 1 - Double work burden

##### Read in recodes
data1 <- read_csv('recoded_time.csv') %>%
  map_df(as.numeric)

names(data1) <- paste0(names(data1), '_num')

data <- cbind(data, data1)

##### Deal with NAs
##### NAs put in in recoding
##### Gender means
mean_narm <- function (x) mean(as.numeric(x), na.rm = TRUE)
impute_mean_group <- function (x) {
  # ifelse(is.na(x),
  #        return(mean_narm(x)),
  #        return(x))
  
  replace(x, is.na(x), mean(x, na.rm=TRUE))
}

numtime <- data %>%
  group_by(Gender_ind) %>%
  select(Work_unpaid.days_per_wk_num, 
         Work_unpaid.hrs_per_day_num, 
         Work_paid.days_per_wk_num,
         Work_paid.hrs_per_day_num) %>%
  transmute_all(impute_mean_group) %>%
  ungroup(Gender_ind) %>%
  select(Work_unpaid.days_per_wk_num, 
         Work_unpaid.hrs_per_day_num, 
         Work_paid.days_per_wk_num,
         Work_paid.hrs_per_day_num)

##### Enumerator NAs
originalNA <- data %>%
  select(Work_unpaid.days_per_wk, 
         Work_unpaid.hrs_per_day, 
         Work_paid.days_per_wk,
         Work_paid.hrs_per_day) %>%
  is.na() %>%
  as.tibble()

##### Apply these
numtime$Work_unpaid.days_per_wk_num[originalNA$Work_unpaid.days_per_wk] <- 0
numtime$Work_paid.days_per_wk_num[originalNA$Work_paid.days_per_wk] <- 0
numtime$Work_unpaid.hrs_per_day_num[originalNA$Work_unpaid.hrs_per_day] <- 0
numtime$Work_paid.hrs_per_day_num[originalNA$Work_paid.hrs_per_day] <- 0



##### Join

##### Calculate
data$Work_doubleburden <- numtime %>%
  transmute((Work_paid.hrs_per_day_num * Work_paid.days_per_wk_num) +
              (Work_unpaid.hrs_per_day_num * Work_unpaid.days_per_wk_num)) %>%
  pull() %>%
  cut(.,breaks = c(0,35.01, 55.01, 1000), labels = c(3, 2, 1)) %>%
  as.character() %>%
  as.numeric()

##### Indicator score
data$score15.3.1 <- norm_IDM(data$Work_doubleburden, 1, 3)

#### Theme score
data$score15.3 <- data$score15.3.1

### Dimension score
data$score15 <- rowMeans(data[,c('score15.1', 'score15.2', 'score15.3')])

## Add dims 12 and 14 ##

data_new <- readRDS('~/Downloads/SI_scored12_14.rds') %>% 
  select(contains('score'))

data_new <- data_new / 4
  
data <- data_new %>%
  cbind(data, .)

data$DepCat12 <- deprive(data$score12)
data$DepCat14 <- deprive(data$score14)
data$DepCat15 <- deprive(data$score15)

## Add one and multiply by 3 for 1 - 4 scale
convert_1_4 <- function (x) return ((x * 3) + 1)

data <- data %>%
  mutate_at(vars(contains('score'), -contains('.'), -contains('-')), convert_1_4)

## Export to csv ##
write_csv(data, 'full_dims.csv')

## Save as RDS
saveRDS(data, 'full_dims.rds')
