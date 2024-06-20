#Author: James Sturges
#Creation Date: 03 Nov 2021
#Title: EMObs Formatting
#Output: Create a df dat_fish that will be used for all thesis analysis

#We want the use the Here package for project management
#This will allow project work-flow to be easier across multiple devices/developers
# library(here)
# here()
# 
# here::i_am("EM_Input_Directory/dat_EMObs_Cleaning.R")

#Load Packages
library(tidyverse)
library(lubridate)
library(hms)


#Read in the Event Measure observation text file 
# dat_EMObs <- read_tsv("data/dat_EMObs.txt")
dat_EMObs = read.csv("data/ALL_Lengths.csv")
#take a quick look at these data in case you need to alter column types
glimpse(dat_EMObs)

#We want to rename certain columns from EM as meaningful characteristics that describe the reef
#Module_Side is either East or West
#Habitat_Type is either Mid_High, Mid_Medium, Mid_Low, or Ecotone_High, Ecotone_Medium, Ecotone_Low
#Module is the letter & number name given for each Modules (6 Block numbers and A-D letters)
#We also decide to rename the character values from midline transects
#sub-Modules now have Mid_High, Mid_Medium, & Mid_Low
dat_EMObs <- dat_EMObs %>%
  rename(Module_Side = Attribute9,
         Habitat_Type = Attribute10,
         Module = OpCode) %>% 
  mutate(Genus_spp = str_c(Genus, Species, sep = " "),
         Habitat_Type = recode(Habitat_Type, 
                               High = "Mid_High", 
                               Medium = "Mid_Medium", 
                               Low = "Mid_Low"))


#This code checks that all 18 Modules have 20 start and end points
#This is needed to generate the time of each transect
dat_EMObs %>%
  filter(Code == "Start" | Code == "End") %>%
  count(Module)

#Check to see that all 18 Modules have a Comment on the Time measurement which starts with the string "DT"
dat_EMObs %>%
  filter(grepl('DT', Comment))

#Here we create a date time based on the Comment column
#Note that some observations will not parse 
#because they have other comments that are not date times this error is OK
dat_EMObs <- dat_EMObs %>%
  mutate(Start_Time = dmy_hms(Comment), 
         Video_Elapsed_hms = as_hms(Time * 60))

#Fill in that column so that every point for a given module has the same start time
dat_EMObs <- dat_EMObs %>%
  group_by(Module) %>%
  fill(Start_Time) %>%
  # min Video_Elapsed_hms should be Code == Time for each module
  mutate(Video_Sync_hms = min(Video_Elapsed_hms), 
         # subtract off sync time prior to filming clock in video       
         Video_Corrected_hms = Video_Elapsed_hms - Video_Sync_hms, 
         # add correct video time to clock time to get real date time
         Real_Time = Start_Time + Video_Corrected_hms)

# Calculate the transect time for each transect
# The purpose of this code is to estimate how long we spent surveying each section of the reef
# We need the time of each survey and the area viewed in the camera to calculate fish densities
# This created a df of just the Start and End points (360 of them across all 18 Modules) 
Transect_Duration <- dat_EMObs %>%
  group_by(Module, Module_Side, Habitat_Type) %>%
  select(Module, Module_Side, Habitat_Type, Code, Real_Time) %>%
  filter(Code %in% c("Start", "End"))
glimpse(Transect_Duration)


#Using that new string I created a table of the differences between the End and Start time for each of the transects
#There are 10 transects per Module (Ecotone, Mid, High, Medium, Low on each side)
#Here we have a new df with start and end times and the difference between them for all 180 transects conducted
Start_End_Time <- Transect_Duration %>% 
  pivot_wider(names_from = Code, values_from = Real_Time) %>% 
  mutate(Diff_s = End - Start) %>%
  ungroup()

#I wanted to quickly look at the summary stats for each of the transect types
#There is probably a way to break apart the string I created for Zone but I'm not sure how 
#Instead I just filtered the string with the phrase for each transect type (Ecotone, High, etc)
Start_End_Time %>%
  ggplot(aes(Diff_s)) +
  geom_histogram() +
  facet_grid(rows = vars(Habitat_Type))

Start_End_Time %>%
  ggplot(aes(Diff_s)) +
  geom_histogram() +
  facet_grid(rows = vars(Module), cols = vars(Habitat_Type))

time_comp_plot <- Start_End_Time %>%
  select(!c(Start, End)) %>%
  pivot_wider(names_from = Module_Side, values_from = Diff_s) %>%
  ggplot(aes(x = West, y = East, col = Habitat_Type, label = Module)) +
  geom_point() +
  geom_label()

time_comp_plot

Start_End_Time %>%
  filter(grepl('Ecotone', Habitat_Type)) %>%
  summary()

Start_End_Time %>%
  filter(grepl('Mid', Habitat_Type)) %>%
  summary()

Start_End_Time %>%
  filter(grepl('High', Habitat_Type)) %>%
  summary()

Start_End_Time %>%
  filter(grepl('Medium', Habitat_Type)) %>%
  summary()

Start_End_Time %>%
  filter(grepl('Low', Habitat_Type)) %>%
  summary()




# We want to find the average swim rate along the Ecotone
# We will take the average of the East & West Ecotone transect times
# Then divide that by 48 meters to get a rate (Step completed)
# We can apply that rate to the midline to estimate distance swam (in progress)
# Here we look at just the start and end values for Ecotone transects
Avg_Eco_Time_Per_Mod <- Start_End_Time %>%
  group_by(Module) %>%
  filter(Habitat_Type %in% "Ecotone") %>%
  summarize(Mean_Ecotone_Time = mean(Diff_s))

#Look at the distribution of mean ecotone transects across all 18 modules
#Create a numeric column from the difftimes because summary() does not work for class difftime
Avg_Eco_Time_Per_Mod <- Avg_Eco_Time_Per_Mod %>%
  mutate(Mean_Ecotone_Time_Num = as.numeric(Mean_Ecotone_Time))

Avg_Eco_Time_Per_Mod %>%
  summary(Mean_Ecotone_Time_Num)
# Calculate the average swim rate along the ecotone for each Module
# These values can be multiplied with the transect times for Midlines & sub-Modules to estimate distance
Avg_Eco_Time_Per_Mod <- Avg_Eco_Time_Per_Mod %>%
  mutate(Eco_Swim_Rate_m_s = 48/Mean_Ecotone_Time_Num)


Start_End_Time <- Start_End_Time %>%
  left_join(Avg_Eco_Time_Per_Mod) %>%
  mutate(Dis_Swim_m = as.numeric(Diff_s) * Eco_Swim_Rate_m_s)

Start_End_Time %>%
  ggplot(aes(Dis_Swim_m)) +
  geom_histogram() +
  facet_grid(rows = vars(Habitat_Type))

# Trying to estimate distance with just the midline first
Mid_Time_Per_Mod <- Start_End_Time %>%
  group_by(Module) %>%
  filter(Habitat_Type %in% "Mid")

Mid_Time_Per_Mod <- Mid_Time_Per_Mod %>%
  mutate(Diff_num = as.numeric(Diff_s))


#Checking to see which of the modules had long/short midline transects
#Going to create a boxplot with a jitter layer and label each point by Module
#Retroactively I can look at the unique features of each Module
#The first boxplot looks at both the East and West Side together
Start_End_Time %>%
  ggplot(aes(x = Habitat_Type, y = Dis_Swim_m, label = Module)) +
  geom_boxplot(outlier.color = "red") +
  geom_text(position = position_jitter(seed = 1))

Start_End_Time %>%
  filter(Module_Side %in% "East") %>% 
  ggplot(aes(x = Habitat_Type, y = Dis_Swim_m, label = Module)) +
  geom_boxplot(outlier.color = "red") +
  geom_text(position = position_jitter(seed = 1))

Start_End_Time %>%
  filter(Module_Side %in% "West") %>% 
  ggplot(aes(x = Habitat_Type, y = Dis_Swim_m, label = Module)) +
  geom_boxplot(outlier.color = "red") +
  geom_text(position = position_jitter(seed = 1))


# I want to divide the ecotone into 3 sections and assign fish to these sections based on the time they were observed
# This helps us account for differences in the ecotone when adjacent to different sub-modules
# Consider for West ecotones the 1st section is next to high sub-modules
# But for East ecotones the 1st section is the medium sub-modules
# Look at just the ecotone   

Start_End_Time_w <- Start_End_Time  %>%
  filter(Habitat_Type == "Ecotone" & Module_Side == "West") %>% 
  mutate(Diff_third_s = Diff_s/3, 
         End_high = Start + Diff_third_s,
         End_low = Start + (2*Diff_third_s),
         End_medium = End)

# We need to add rows to Start_End_Time_w
# We want 3 rows per module 
#These 3 chunks change the habitat type from ecotone to Ecotone_High/Medium/or Low
#We want to divide the ecotone to see if proximity to different sub-Module heights influenced fish dist
Start_End_Time_w_h <- Start_End_Time_w %>% 
  mutate(Habitat_Type = "Ecotone_High",
         Start = Start,
         End = End_high) %>% 
  select(Module:Diff_third_s)


Start_End_Time_w_m <- Start_End_Time_w %>% 
  mutate(Habitat_Type = "Ecotone_Medium",
         Start = End_low,
         End = End_medium) %>% 
  select(Module:Diff_third_s)



Start_End_Time_w_l <- Start_End_Time_w %>% 
  mutate(Habitat_Type = "Ecotone_Low",
         Start = End_high,
         End = End_low) %>% 
  select(Module:Diff_third_s)

#Going to do the same thing as above for the East Ecotone
#Here we create a Start_End_Time East table only for Ectones
#Note that the order or sub-modules is different on the east side
#The first part of the east ecotone is medium sub-module
Start_End_Time_e <- Start_End_Time  %>%
  filter(Habitat_Type == "Ecotone" & Module_Side == "East") %>% 
  mutate(Diff_third_s = Diff_s/3, 
         End_high = End,
         End_low = Start + (2*Diff_third_s),
         End_medium = Start + Diff_third_s,)

#Now we create 3 tables to modify the habitat type

Start_End_Time_e_h <- Start_End_Time_e %>% 
  mutate(Habitat_Type = "Ecotone_High",
         Start = End_low,
         End = End) %>% 
  select(Module:Diff_third_s)


Start_End_Time_e_m <- Start_End_Time_e %>% 
  mutate(Habitat_Type = "Ecotone_Medium",
         Start = Start,
         End = End_medium) %>% 
  select(Module:Diff_third_s)



Start_End_Time_e_l <- Start_End_Time_e %>% 
  mutate(Habitat_Type = "Ecotone_Low",
         Start = End_medium,
         End = End_low) %>% 
  select(Module:Diff_third_s)



#Create a new start_end_time table
#This time it will have 3 different levels of Ecotone for each Habitat_Type (high, medium, low)

Start_End_Time_eco <- bind_rows(Start_End_Time_w_h, Start_End_Time_w_m, Start_End_Time_w_l, 
                                Start_End_Time_e_h, Start_End_Time_e_m, Start_End_Time_e_l) %>% 
  arrange(Module, Module_Side, Habitat_Type) %>% 
  mutate(Eco_Int = interval(start = Start, end = End), 
         Habitat_Type_eco = Habitat_Type) %>% 
  select(Module, Module_Side, Habitat_Type_eco, Eco_Int)


glimpse(Start_End_Time_eco)


# Create ecotone version of dat_EMObs (will row_bind it back onto dat_EMObs at the end)
dat_EMObs_eco <- dat_EMObs %>% 
  filter(Habitat_Type == "Ecotone") %>%
  # expand so row for each fish repeats 3x, one for each ecotone type  
  inner_join(Start_End_Time_eco) %>% 
  mutate(Habitat_Type = if_else(Real_Time %within% Eco_Int, Habitat_Type_eco, NA_character_)) %>% 
  drop_na(Habitat_Type)


### FIGURE OUT WHICH 1 FISH (use function to compare dataframes... )
dat_EMObs_eco_test <- dat_EMObs %>% 
  filter(Habitat_Type == "Ecotone") 
select(dat_EMObs_eco_test, c(Module, ImagePtPair)) %>% anti_join(select(dat_EMObs_eco, c(Module, ImagePtPair)))
##!!! So row was 7A Rhinobatos productus which was not on a transect but was observed




### Bind new dat_EMObs_eco rows back onto dat_EMObs
dat_fish <- dat_EMObs %>% 
  bind_rows(dat_EMObs_eco)

dat_fish <- dat_fish %>%   
  filter(Habitat_Type %in% c("Ecotone_High", 
                             "Ecotone_Low",
                             "Ecotone_Medium", 
                             "Mid_High",
                             "Mid_Low", 
                             "Mid_Medium")) 
#filter
dat_fish <- dat_fish %>% 
  filter(!Code %in% c("Start","End")) 



# Set factor level order for tables, plots etc.
dat_fish <- dat_fish %>%
  mutate(Habitat_Type = fct_relevel(Habitat_Type, "Ecotone_High", 
                                    "Ecotone_Medium",
                                    "Ecotone_Low",
                                    "Mid_High", 
                                    "Mid_Medium",
                                    "Mid_Low")) %>%
  arrange(Module, Module_Side, Habitat_Type)



## !!! TO DO
## add fct_relevel for all relevant variables: module, etc...





#Add on transect areas for each transect segments
#1) left_join from calculated areas above
#2) manually set all ecotone segment areas to 48m x 2 m wide / 3 segments

#going to take the Start_End_Time table and mutate it to get transect area
#the ecotone areas should set to 48m x 2m
#the swim rate for each module will be applied to the midline

Start_End_Mids <- Start_End_Time %>% 
  filter(Habitat_Type %in% c("Mid_High",
                             "Mid_Low",
                             "Mid_Medium"))

#We have already calculated the distance swam along the midline
#We take the distance swam in m and multiple it by 2m (transect width)
#the tsect_area_m2 can be used for fish density calculations

Start_End_Mids <- Start_End_Mids %>% 
  mutate(tsect_area_m2 = Dis_Swim_m * 2)

#I wanted to create a single table that had all transect areas
#This table is called Start_End_Complete
#There are 12 points per Module
#High, Medium, Low For Eco and Mid on the East and West Side
#All ecotone distances were manually set to 16m
#All ecotone transect areas were set to 32m
#The midlines are based on module specific ecotone swim times
#distance on midlines is estimated based on ecotone swim rate
#then distance is multipled by 2m to estimate midline transect area

Start_End_Complete <- Start_End_Mids %>% 
  bind_rows(Start_End_Time_eco)

#This is amazing!!!
#Using coalesce you can merge columns where there are NAs
#Here we take the Habitat_Type_eco for ecotones and replace NAs in Habitat_Type
Start_End_Complete <- Start_End_Complete %>% 
  mutate(Habitat_Type = coalesce(Habitat_Type, Habitat_Type_eco))
#Here we take the ecotone interval and seperate it into Start and End times 
#Fill the start column using coalesce and int_start function
Start_End_Complete <- Start_End_Complete %>% 
  mutate(Start = coalesce(Start, int_start(Eco_Int)))
#Fill the End column with int_end function
Start_End_Complete <- Start_End_Complete %>% 
  mutate(End = coalesce(End, int_end(Eco_Int)))

#Manually set the ecotone distance to 16m for all segments 
Start_End_Complete <- Start_End_Complete %>% 
  mutate(Dis_Swim_m = replace_na(Dis_Swim_m, 16))

#Manually set the area survey to 32 meters squared for ecotone segments
Start_End_Complete <- Start_End_Complete %>% 
  mutate(tsect_area_m2 = replace_na(tsect_area_m2, 32))

#arrange values by Module
Start_End_Complete <- Start_End_Complete %>% 
  arrange(Module)

#Fill in missing values from 
Start_End_Complete <- Start_End_Complete %>% 
  group_by(Module) %>% 
  fill(Mean_Ecotone_Time, Mean_Ecotone_Time_Num, Eco_Swim_Rate_m_s)
#Quickly just recalculated the difference between start and end times 
#This was faster than pulling the ecotone values from another table
Start_End_Complete <- Start_End_Complete %>% 
  mutate(Diff_s = End - Start)

#Then I dropped the last two columns 
Start_End_Complete <- Start_End_Complete %>% 
  select(-c("Habitat_Type_eco", "Eco_Int"))



#Quickly look at the average distance and transect area
#Interesting Mid_Low distance = 16.4m
#About the same as ecotone! 
#High standard deviation though 3.56m
#Still evidence that we swam more area on higher modules
ht_dist_st <- Start_End_Complete %>% 
  group_by(Habitat_Type) %>% 
  summarise(mean_dis = mean(Dis_Swim_m),
            sd_dis = sd(Dis_Swim_m))

ht_dist_st

Start_End_Complete %>% 
  ggplot(aes(Dis_Swim_m)) +
  geom_histogram() +
  facet_grid(rows = vars(Habitat_Type), scales = 'free_y')

#Before we go on to dat fish analysis I merged the Start_End_complete Table
#dat_fish now has transect distance and area surveyed 
dat_fish <- 
  left_join(dat_fish, Start_End_Complete, by = c("Module", "Module_Side", "Habitat_Type"))

#We also want to give every fish a density value
#1 observation over the transect area
dat_fish <- dat_fish %>% 
  mutate(density_m2 = 1/tsect_area_m2)
#Then start using dat_fish in your analysis script by using
#source("dat_EMObs_Cleaning.R")


# Further cleaning needed to remove Unknown species and stop points
dat_fish <- dat_fish %>% 
  filter(Genus_spp != "Pause_Start Pause_Start",
         Genus_spp != "Unknown Unknown")

# We converted our fish counts to density during data cleaning
# But we then wanted to put these values in a more realistic context so set the densities to 100 m^2 
dat_fish <- dat_fish %>% 
  mutate(dens_100m2 = density_m2*100)


# Here we add a new column where decide if the mid high sub-modules are 3 or 4m tall
# half of the Modules have approx 3m and the other half is approx 4m in maximum height
# I apply this value to every fish even though it does not matter for ecotone, low, med
dat_fish <- dat_fish %>% 
  mutate(mid_high_hgt_m = (case_when(startsWith(Module, "2") ~ "4",
                                     startsWith(Module, "6") ~ "4",
                                     startsWith(Module, "8") ~ "4",
                                     startsWith(Module, "4") ~ "3",
                                     startsWith(Module, "5") ~ "3",
                                     startsWith(Module, "7") ~ "3")))

#I then decided the orientation of each module relative to shore
#Perpendicular means the transect heading goes towards or away from shore
#parallel means they go along the shore
dat_fish <- dat_fish %>% 
  mutate(Orientation = case_when(startsWith(Module, "2A") ~ "Perpendicular",
                                 startsWith(Module, "2B") ~ "Parallel",
                                 startsWith(Module, "2C") ~ "Parallel",
                                 startsWith(Module, "4") ~ "Parallel",
                                 startsWith(Module, "5") ~ "Parallel",
                                 startsWith(Module, "6") ~ "Parallel",
                                 startsWith(Module, "7A") ~ "Parallel",
                                 startsWith(Module, "7B") ~ "Perpendicular",
                                 startsWith(Module, "7C") ~ "Perpendicular",
                                 startsWith(Module, "8") ~ "Perpendicular"))




# We need to standardize the width of our transects to account for variation in visibility.
#MidX column on event measure indicates the width of the transect
#Range for MidX is -2500 to 3000
#We decided to use the 2m wide transect data
#The power of the stereo video camera is that you can accurately count a larger area
#This only removed 587 fish observations overall
dat_fish_1m_wide <- dat_fish %>% 
  filter(MidX <= 1000 & MidX >= -1000)

dat_fish_2m_wide <- dat_fish %>% 
  filter(MidX <= 2000 & MidX >= -2000)



#We need a table for all of our transect variables
#Some of these characteristics were recorded during the survey period
#Others are fixed conditions like construction date and orientation
#Edit this as needed with additional variables

#here we import our field notes
dive_dat <- read.csv("data/DiveEvent.csv", fileEncoding = 'UTF-8-BOM')

#No idea why the imported name was like that for Module?
#So I renamed it
dive_dat <- dive_dat %>% 
  #rename(Module = ï..Module) %>% 
  select(Module, construction_date, construction_month, construction_group, survey_date, survey_time)


#Here we associated the characteristics for each of the 216 transects
transect_var <- dat_fish %>% 
  select(Module, Module_Side, Habitat_Type, Orientation, mid_high_hgt_m) %>% 
  mutate(transect = paste(Module, Module_Side, Habitat_Type)) %>% 
  distinct()

#This splits Habitat type into two characteristics 
#Ecotone vs Mid
#Sub-mods into three groups High, Medium, Low
transect_var <- transect_var %>% 
  separate(Habitat_Type, c("Zone", "Sub_Mod"), remove = F)

#and then add our field notes to transect variables
transect_var <- transect_var %>% 
  left_join(dive_dat)

transect_var <- transect_var %>% 
  mutate(Block = case_when(startsWith(Module, "2") ~ "2",
                           startsWith(Module, "4") ~ "4",
                           startsWith(Module, "5") ~ "5",
                           startsWith(Module, "6") ~ "6",
                           startsWith(Module, "7") ~ "7",
                           startsWith(Module, "8") ~ "8"))


## create reduced versions of transect_var for module specific characteristics
module_var <- transect_var %>% 
  select(Module, Orientation, mid_high_hgt_m, construction_date, survey_date, 
         survey_time, construction_group, Block) %>%
  distinct() %>% 
  arrange(Module)


#The 11 most abundant spp
#These spp were also observed on at least 12/18 modules 
#We will use these species later
focal_spp <- c("Chromis punctipinnis",
               "Paralabrax clathratus",
               "Oxyjulis californica",
               "Semicossyphus pulcher",
               "Embiotoca jacksoni",
               "Hypsurus caryi",
               "Paralabrax nebulifer",
               "Girella nigricans",
               "Damalichthys vacca",
               "Sebastes serranoides",
               "Halichoeres semicinctus")


# Creating a new metric that combines the Orientation and inshore/offshore component
dat_fish_2m_wide <- dat_fish_2m_wide %>% 
  mutate(os = paste(Module_Side, Orientation),
         ht_os = paste(os, Habitat_Type))


dat_fish_2m_wide <- dat_fish_2m_wide %>% 
  mutate(compass_heading = (case_when(startsWith(ht_os, "West Perpendicular Mid_High") ~ 341,
                                      startsWith(ht_os, "West Perpendicular Ecotone_High") ~ 341,
                                      startsWith(ht_os, "West Perpendicular Mid_Low") ~ 297,
                                      startsWith(ht_os, "West Perpendicular Ecotone_Low") ~ 297,
                                      startsWith(ht_os, "West Perpendicular Mid_Medium") ~ 257,
                                      startsWith(ht_os, "West Perpendicular Ecotone_Medium") ~ 257,
                                      startsWith(ht_os, "East Perpendicular Mid_High") ~ 167,
                                      startsWith(ht_os, "East Perpendicular Ecotone_High") ~ 167,
                                      startsWith(ht_os, "East Perpendicular Mid_Low") ~ 115,
                                      startsWith(ht_os, "East Perpendicular Ecotone_Low") ~ 115,
                                      startsWith(ht_os, "East Perpendicular Mid_Medium") ~ 72,
                                      startsWith(ht_os, "East Perpendicular Ecotone_Medium") ~ 72,
                                      startsWith(ht_os, "West Parallel Mid_High") ~ 254,
                                      startsWith(ht_os, "West Parallel Ecotone_High") ~ 254,
                                      startsWith(ht_os, "West Parallel Mid_Low") ~ 200,
                                      startsWith(ht_os, "West Parallel Ecotone_Low") ~ 200,
                                      startsWith(ht_os, "Mid_Medium West Parallel") ~ 164,
                                      startsWith(ht_os, "West Parallel Ecotone_Medium") ~ 164,
                                      startsWith(ht_os, "East Parallel Mid_High") ~ 78,
                                      startsWith(ht_os, "East Parallel Ecotone_High") ~ 78,
                                      startsWith(ht_os, "East Parallel Mid_Low") ~ 18,
                                      startsWith(ht_os, "East Parallel Ecotone_Low") ~ 18,
                                      startsWith(ht_os, "East Parallel Mid_Medium") ~ 347,
                                      startsWith(ht_os, "East Parallel Ecotone_Medium") ~ 347)))



# When we want to use size class data we will only want to use fish that could be measure
# If there was a comment the length is invalid
# We remove those points but may keep them for counts
dat_fish_l <- dat_fish_2m_wide %>% 
  filter(!Comment %in% "no vis right camera") %>% 
  filter(!Comment %in% "no vis left camera") %>% 
  filter(Precision < 20)

# There were 24 fish that had max precision values (9999-10000)
# These fish were likely "no vis" fish that did not get a comment
# I filter them out here for length estimates
# I can double check those in EM when I have a computer again
# Can still use "no vis" for counts and density
# but not for length and biomass density
#There are still fish that should be removed due to high precision
#but need to talk about species specific cut offs
#or a hard set number for all fish?
#should it be relatively to max length?


dat_fish_l <- dat_fish_l %>% 
  separate(Habitat_Type, c("Zone", "Sub_Mod"), remove = F) %>% 
  mutate(Block = case_when(startsWith(Module, "2") ~ "2",
                           startsWith(Module, "4") ~ "4",
                           startsWith(Module, "5") ~ "5",
                           startsWith(Module, "6") ~ "6",
                           startsWith(Module, "7") ~ "7",
                           startsWith(Module, "8") ~ "8"))


dat_fish_l <- dat_fish_l %>% 
  mutate(transect = paste(Module, Module_Side, Habitat_Type),
         dens_100m2_4rt = dens_100m2^0.25) %>% 
  left_join(transect_var)


dat_fish_l <- dat_fish_l %>% 
  mutate(mid_high_hgt_m = as.character(mid_high_hgt_m))

dat_fish_l <- dat_fish_l %>% 
  mutate(Mid_High_3_4 = paste(Habitat_Type, mid_high_hgt_m))

dat_fish_l <- dat_fish_l %>% 
  mutate(Mid_High_3_4 = str_replace(Mid_High_3_4, " ", "_"))

dat_fish_l <- dat_fish_l %>% 
  mutate(Mid_High_3_4 = as.character(Mid_High_3_4))

dat_fish_l <- dat_fish_l %>% 
  mutate(Mid_High_3_4 = (case_when(startsWith(Mid_High_3_4, "Ecotone_High") ~ "Ecotone_High",
                                   startsWith(Mid_High_3_4, "Ecotone_Medium") ~ "Ecotone_Medium",
                                   startsWith(Mid_High_3_4, "Ecotone_Low") ~ "Ecotone_Low",
                                   startsWith(Mid_High_3_4, "Mid_High_3") ~ "Mid_High_3",
                                   startsWith(Mid_High_3_4, "Mid_High_4") ~ "Mid_High_4",
                                   startsWith(Mid_High_3_4, "Mid_Low") ~ "Mid_Low",
                                   startsWith(Mid_High_3_4, "Mid_Medium") ~ "Mid_Medium")))