#### Project Information ----
#### Project Title: Peak Performance: vertical relief influences fish density and assemblage structure on heterogenous restoration reef
#### Authors: James W. Sturges & Jeremy T. Claisse
#### Last Updated: July 3rd 2024

#### Required Packages ----
library(here)
library(Hmisc)
library(RColorBrewer)
library(randomcoloR)
library(vegan)
library(tidyverse)
library(lubridate)
library(flextable) 
library(ggrepel)
library(officer)
library(wesanderson)
library(dendextend)
library(ggdendro)
library(hms)
library(dendextend)
library(ggdendro)
library(cluster)

#### Event Measure Observation Files ----
#Read in the Event Measure observation text file 

dat_EMObs = read.csv("data/ALL_Lengths.csv")


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
# Start_End_Time %>%
#   ggplot(aes(Diff_s)) +
#   geom_histogram() +
#   facet_grid(rows = vars(Habitat_Type))
# 
# Start_End_Time %>%
#   ggplot(aes(Diff_s)) +
#   geom_histogram() +
#   facet_grid(rows = vars(Module), cols = vars(Habitat_Type))
# 
time_comp_plot <- Start_End_Time %>%
  select(!c(Start, End)) %>%
  pivot_wider(names_from = Module_Side, values_from = Diff_s) %>%
  ggplot(aes(x = West, y = East, col = Habitat_Type, label = Module)) +
  geom_point() +
  geom_label()

# time_comp_plot

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

# Start_End_Time %>%
#   ggplot(aes(Dis_Swim_m)) +
#   geom_histogram() +
#   facet_grid(rows = vars(Habitat_Type))

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
# Start_End_Time %>%
#   ggplot(aes(x = Habitat_Type, y = Dis_Swim_m, label = Module)) +
#   geom_boxplot(outlier.color = "red") +
#   geom_text(position = position_jitter(seed = 1))
# 
# Start_End_Time %>%
#   filter(Module_Side %in% "East") %>% 
#   ggplot(aes(x = Habitat_Type, y = Dis_Swim_m, label = Module)) +
#   geom_boxplot(outlier.color = "red") +
#   geom_text(position = position_jitter(seed = 1))
# 
# Start_End_Time %>%
#   filter(Module_Side %in% "West") %>% 
#   ggplot(aes(x = Habitat_Type, y = Dis_Swim_m, label = Module)) +
#   geom_boxplot(outlier.color = "red") +
#   geom_text(position = position_jitter(seed = 1))


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

#going to take the Start_End_Time table and mutate it to get transect area
#the ecotone areas should set to 48m x 2m
#the swim rate for each module will be applied to the midline

Start_End_Mids <- Start_End_Time %>% 
  filter(Habitat_Type %in% c("Mid_High",
                             "Mid_Low",
                             "Mid_Medium"))

#We have already calculated the distance swam along the midline
#We take the distance swam in m and multiple it by m (transect width)
#the tsect_area_m2 can be used for fish density calculations

Start_End_Mids <- Start_End_Mids %>% 
  mutate(tsect_area_m2 = Dis_Swim_m * 4)

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
  mutate(tsect_area_m2 = replace_na(tsect_area_m2, 64))

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

# ht_dist_st

# Start_End_Complete %>% 
#   ggplot(aes(Dis_Swim_m)) +
#   geom_histogram() +
#   facet_grid(rows = vars(Habitat_Type), scales = 'free_y')

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


# dat_fish_2m_wide <- dat_fish_2m_wide %>% 
#   mutate(compass_heading = (case_when(startsWith(ht_os, "West Perpendicular Mid_High") ~ 341,
#                                       startsWith(ht_os, "West Perpendicular Ecotone_High") ~ 341,
#                                       startsWith(ht_os, "West Perpendicular Mid_Low") ~ 297,
#                                       startsWith(ht_os, "West Perpendicular Ecotone_Low") ~ 297,
#                                       startsWith(ht_os, "West Perpendicular Mid_Medium") ~ 257,
#                                       startsWith(ht_os, "West Perpendicular Ecotone_Medium") ~ 257,
#                                       startsWith(ht_os, "East Perpendicular Mid_High") ~ 167,
#                                       startsWith(ht_os, "East Perpendicular Ecotone_High") ~ 167,
#                                       startsWith(ht_os, "East Perpendicular Mid_Low") ~ 115,
#                                       startsWith(ht_os, "East Perpendicular Ecotone_Low") ~ 115,
#                                       startsWith(ht_os, "East Perpendicular Mid_Medium") ~ 72,
#                                       startsWith(ht_os, "East Perpendicular Ecotone_Medium") ~ 72,
#                                       startsWith(ht_os, "West Parallel Mid_High") ~ 254,
#                                       startsWith(ht_os, "West Parallel Ecotone_High") ~ 254,
#                                       startsWith(ht_os, "West Parallel Mid_Low") ~ 200,
#                                       startsWith(ht_os, "West Parallel Ecotone_Low") ~ 200,
#                                       startsWith(ht_os, "Mid_Medium West Parallel") ~ 164,
#                                       startsWith(ht_os, "West Parallel Ecotone_Medium") ~ 164,
#                                       startsWith(ht_os, "East Parallel Mid_High") ~ 78,
#                                       startsWith(ht_os, "East Parallel Ecotone_High") ~ 78,
#                                       startsWith(ht_os, "East Parallel Mid_Low") ~ 18,
#                                       startsWith(ht_os, "East Parallel Ecotone_Low") ~ 18,
#                                       startsWith(ht_os, "East Parallel Mid_Medium") ~ 347,
#                                       startsWith(ht_os, "East Parallel Ecotone_Medium") ~ 347)))



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

#### Adjusting for Sampling Effort ----
# mean distance per habitat type flextable 
# This estimates the mean length of video transect areas across each habitat type
# As expected we traveled further on taller modules
# We standardized ecotone survey lengths to 16 m and adjusted our fish density estimates for reef transects

ht_dist_st <- ht_dist_st %>% 
  mutate(new_names = case_when(startsWith(Habitat_Type, "Mid_High") ~ "High Relief",
                               startsWith(Habitat_Type, "Ecotone_High") ~ "High Ecotone",
                               startsWith(Habitat_Type, "Mid_Low") ~ "Low Relief",
                               startsWith(Habitat_Type, "Ecotone_Low") ~ "Low Ecotone",
                               startsWith(Habitat_Type, "Mid_Medium") ~ "Medium Relief",
                               startsWith(Habitat_Type, "Ecotone_Medium") ~ "Medium Ecotone"))



ht_dist_ft <- flextable(ht_dist_st,
                        col_keys = c("new_names", "mean_dis",
                                     "sd_dis")) %>% 
  add_header_row(colwidths = 3, values = c("Estimated Transect Length (m)")) %>% 
  set_header_labels(new_names = "Habitat Type",
                    mean_dis = "Mean",
                    sd_dis = "sd") %>% 
  colformat_double(digits = 1) %>% 
  theme_box() %>% 
  align(align = "center") %>% 
  align(part = "header", align = "center") %>% 
  bg(j = "mean_dis", i = ~ mean_dis < 16.1, bg = "tan", part = "body") %>% 
  bg(j = "mean_dis", i = ~ mean_dis > 16.1, bg = "grey", part = "body") %>% 
  bg(j = "new_names", i = ~ mean_dis < 16.1, bg = "tan", part = "body") %>% 
  bg(j = "new_names", i = ~ mean_dis > 16.1, bg = "grey", part = "body") %>% 
  bg(j = "sd_dis", i = ~ mean_dis < 16.1, bg = "tan", part = "body") %>% 
  bg(j = "sd_dis", i = ~ mean_dis > 16.1, bg = "grey", part = "body")


# ht_dist_ft

write_csv(ht_dist_st, "tables/ht_dist_st.csv")

# save_as_docx(ht_dist_ft, path = "ht_dist_ft.docx")



## swim time per transect east v west comparison 

# We adjusted reef transect lengths based on ecotone swim rates for each module
# This assumes that during a single dive we maintained relative constant speed and accounts for sampling intensity variance between dives and days

time_comp_plot <- time_comp_plot +
  theme_classic() +
  labs(x = "Time of Upcurrent Transect (s)", y = "Time of Downcurrent Transect (s)") +
  guides(color = guide_legend(title = "Transect Type"),
         guide_legend(override.aes = aes(label = ""))) +
  theme(legend.position = "top") +
  theme(legend.text = element_text(color = "black")) 






# time_comp_plot

# ggsave("figures/time_comp_plot.png", time_comp_plot,
#        width = 6, height = 5, dpi = 600)




# create new variables for Zone and current flow 

dat_fish_2m_wide <- dat_fish_2m_wide %>% 
  mutate(Zone = case_when(startsWith(Habitat_Type, "Ecotone") ~ "Ecotone",
                          startsWith(Habitat_Type, "Mid") ~ "Mid"))

dat_fish_2m_wide <- dat_fish_2m_wide %>% 
  filter(!c(Genus_spp %in% "Chromis punctipinnis"& Length > 260))

dat_fish_l <- dat_fish_l %>% 
  filter(!c(Genus_spp %in% "Chromis punctipinnis"& Length > 260))

dat_fish_2m_wide <- dat_fish_2m_wide %>% 
  arrange(Genus_spp, desc(Length))



dat_fish_2m_wide <- dat_fish_2m_wide %>% 
  mutate(current =  case_when(startsWith(Module_Side, "West") ~ "Upcurrent",
                              startsWith(Module_Side, "East") ~ "Downcurrent"))

#### Creating Primary Dataframe ----

# 31 species were observed over 216 "transects" on 18 modules
# creates density values for each species across each unique transect
dat_fish_t <- dat_fish_2m_wide %>% 
  group_by(Module, Module_Side, Habitat_Type, Genus_spp) %>% 
  summarise(dens_100m2 = sum(dens_100m2)) %>% 
  ungroup()

#Add in 0's
dat_fish_t <- dat_fish_t %>% 
  complete(Genus_spp, nesting(Module, Module_Side, Habitat_Type),  
           fill = list(dens_100m2 = 0)) %>% 
  arrange(Module, Module_Side, Habitat_Type, Genus_spp)  %>% 
  left_join(transect_var)



#relevel variables
dat_fish_t <- dat_fish_t %>%
  mutate(Module = factor(Module, levels = c("2A", "2B", "2C",
                                            "4D", "4B", "4C",
                                            "5A", "5B", "5C",
                                            "6A", "6D", "6C",
                                            "7A", "7B", "7C",
                                            "8A", "8B", "8C")))
dat_fish_t <- dat_fish_t %>%
  mutate(Habitat_Type = factor(Habitat_Type, levels = c("Ecotone_High",
                                                        "Ecotone_Medium",
                                                        "Ecotone_Low",
                                                        "Mid_High",
                                                        "Mid_Medium",
                                                        "Mid_Low")))
dat_fish_t <- dat_fish_t %>%
  mutate(Sub_Mod = factor(Sub_Mod, levels = c("High", "Medium", "Low")))

dat_fish_t <- dat_fish_t %>% 
  mutate(os = paste(Module_Side, Orientation),
         os_ht = paste(os, Habitat_Type))

dat_fish_t <- dat_fish_t %>% 
  mutate(cluster = (case_when(startsWith(os_ht, "West Perpendicular Mid_High") ~ "Group 4",
                              startsWith(os_ht, "West Perpendicular Ecotone_High") ~ "Group 3",
                              startsWith(os_ht, "West Perpendicular Mid_Low") ~ "Group 3",
                              startsWith(os_ht, "West Perpendicular Ecotone_Low") ~ "Group 3",
                              startsWith(os_ht, "West Perpendicular Mid_Medium") ~ "Group 2",
                              startsWith(os_ht, "West Perpendicular Ecotone_Medium") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Mid_High") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Ecotone_High") ~ "Group 1",
                              startsWith(os_ht, "East Perpendicular Mid_Low") ~ "Group 4",
                              startsWith(os_ht, "East Perpendicular Ecotone_Low") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Mid_Medium") ~ "Group 4",
                              startsWith(os_ht, "East Perpendicular Ecotone_Medium") ~ "Group 3",
                              startsWith(os_ht, "West Parallel Mid_High") ~ "Group 2",
                              startsWith(os_ht, "West Parallel Ecotone_High") ~ "Group 2",
                              startsWith(os_ht, "West Parallel Mid_Low") ~ "Group 4",
                              startsWith(os_ht, "West Parallel Ecotone_Low") ~ "Group 2",
                              startsWith(os_ht, "West Parallel Mid_Medium") ~ "Group 2",
                              startsWith(os_ht, "West Parallel Ecotone_Medium") ~ "Group 4",
                              startsWith(os_ht, "East Parallel Mid_High") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Ecotone_High") ~ "Group 4",
                              startsWith(os_ht, "East Parallel Mid_Low") ~ "Group 5",
                              startsWith(os_ht, "East Parallel Ecotone_Low") ~ "Group 5",
                              startsWith(os_ht, "East Parallel Mid_Medium") ~ "Group 5",
                              startsWith(os_ht, "East Parallel Ecotone_Medium") ~ "Group 5")))


#### Summary Tables ----

# List species observed in study and total abundance indentified 
spp_ID <- dat_fish_2m_wide %>% 
  group_by(Genus_spp, Code) %>% 
  count(Genus_spp, sort = T) %>% 
  arrange(desc(n))

dat_fish_l = dat_fish_l %>% 
  mutate(Length = as.numeric(Length))

dat_fish_min_max_l <- dat_fish_l %>% 
  group_by(Genus_spp) %>% 
  summarise(min_length = min(Length),
            max_length = max(Length),
            mean_length = mean(Length),
            sd_length = sd(Length),
            median_length = median(Length))

dat_fish_l_obs <- dat_fish_l %>% 
  group_by(Genus_spp, .drop = F) %>% 
  count(Genus_spp, sort = T)

dat_fish_l_obs <- dat_fish_l_obs %>% 
  rename(measured = n)

spp_ID <- spp_ID %>% 
  left_join(dat_fish_l_obs)

spp_ID <- spp_ID %>% 
  left_join(dat_fish_min_max_l)

all_spp_mods_obs <- dat_fish_2m_wide %>% 
  group_by(Genus_spp) %>% 
  summarise(mods_obs = n_distinct(Module))

spp_ID <- spp_ID %>% 
  left_join(all_spp_mods_obs)


#create a table of module characteristics 

mod_ft <- flextable(module_var, 
                    col_keys = c("Module", "Orientation", "construction_group", "mid_high_hgt_m", "construction_date", "survey_date", "survey_time")) %>% 
  set_header_labels(Module = "Module",
                    Orientation = "Orientation",
                    construction_group = "Construction Phase",
                    mid_high_hgt_m = "Maximum Submodule Relief",
                    construction_date = "Construction Date",
                    survey_date = "Survey Date",
                    survey_time = "Survey Time"
  ) %>% 
  colformat_double(digits = 0) %>%
  #theme_box() %>%
  align(align = "center") %>%
  align(part = "header", align = "center")

mod_ft

write_csv(module_var, "tables/module_var.csv")
# save_as_docx(mod_ft, path = "mod_ft.docx")
# creates a flextable for spp ID'd and identified
spp_ID_ft <- flextable(spp_ID,
                       col_keys = c("Genus_spp", "Code","mods_obs",
                                    "n", "measured", "min_length", "median_length", "max_length","mean_length", "sd_length")) %>%
  add_header_row(colwidths = c(1,1,1,1,1,5), values = c("Species Name", "Common Name", "Modules","Total", "Measured", "Total Length (mm)")) %>%
  set_header_labels(Genus_spp = "Species Name",
                    Code = "Common Name",
                    mods_obs = "Modules",
                    n = "Total",
                    measured = "Measured",
                    min_length = "Min",
                    mean_length = "Mean",
                    median_length = "Median",
                    max_length = "Max",
                    sd_length = "sd") %>% 
  colformat_double(digits = 0) %>%
  #theme_box() %>%
  align(align = "center") %>%
  align(part = "header", align = "center") %>%
  compose(j = "Genus_spp",
          value = as_paragraph(as_i(Genus_spp))) %>%
  merge_v(part = "header")

spp_ID_ft
write_csv(spp_ID, "tables/spp_ID.csv")
# save_as_docx(spp_ID_ft, path = "spp_ID_ft.docx")
#### Focal Species Summary Table ----
# fish_images <- read.csv("focal_spp_image.csv")
# fish_images <- fish_images  
#rename(Genus_spp = ï..Genus_spp)


dat_fish_min_max_l <- dat_fish_l %>% 
  group_by(Genus_spp) %>% 
  summarise(max_length = max(Length),
            min_length = min(Length),
            sd_length = sd(Length),
            mean_length = mean(Length),
            median_length = median(Length)) %>% 
  filter(Genus_spp %in% focal_spp)

dat_fish_min_max_l

dat_fish_l_obs <- dat_fish_l %>% 
  filter(Genus_spp %in% focal_spp) %>% 
  group_by(Genus_spp) %>% 
  count(Genus_spp, sort = T)

dat_fish_l_obs <- dat_fish_l_obs %>% 
  rename(measured = n)

dat_fish_l_obs

focal_spp_st <- dat_fish_2m_wide %>% 
  filter(Genus_spp %in% focal_spp) %>% 
  group_by(Genus_spp, Code) %>% 
  count(Genus_spp, sort = T) %>% 
  ungroup()

focal_spp_st <- focal_spp_st %>% 
  left_join(dat_fish_min_max_l)

focal_spp_st <- focal_spp_st %>% 
  left_join(dat_fish_l_obs)

focal_spp_mods_obs <- dat_fish_2m_wide %>% 
  filter(Genus_spp %in% focal_spp) %>% 
  group_by(Genus_spp) %>% 
  summarise(mods_obs = n_distinct(Module))

focal_spp_st <- focal_spp_st %>% 
  left_join(focal_spp_mods_obs)

spp_dens_ht <- dat_fish_2m_wide %>% 
  group_by(Genus_spp, Habitat_Type) %>% 
  summarise(Density_100m2 = sum(dens_100m2)) %>% 
  replace(is.na(.), 0)

focal_spp_dens_ht <- spp_dens_ht %>% 
  filter(Genus_spp %in% focal_spp)

focal_spp_dens_ht <- focal_spp_dens_ht %>% 
  pivot_wider(names_from = Habitat_Type, values_from = Density_100m2)

focal_spp_st <- focal_spp_st %>% 
  left_join(focal_spp_dens_ht)


focal_spp_ft <- flextable(focal_spp_st,
                          col_keys = c("Genus_spp", "Code", "mods_obs",
                                       "n", "measured", "min_length", "median_length", "max_length", "mean_length", "sd_length")) %>%
  add_header_row(colwidths = c(1,1,1,1,1,5), values = c("Species Name", "Common Name", "Modules", "Total", "Measured", "Total Length (mm)")) %>%
  set_header_labels(Genus_spp = "Species Name",
                    Code = "Common Name",
                    mods_obs = "Modules",
                    n = "Total",
                    measured = "Measured",
                    min_length = "Min",
                    median_length = "Median",
                    max_length = "Max",
                    mean_length = "Mean",
                    sd_length = "sd") %>% 
  colformat_double(digits = 0) %>%
  theme_box() %>%
  align(align = "center") %>%
  align(part = "header", align = "center") %>%
  compose(j = "Genus_spp",
          value = as_paragraph(as_i(Genus_spp))) %>%
  merge_v(part = "header")


#focal_spp_ft
write_csv(focal_spp_st, "tables/focal_spp_st.csv")
# save_as_docx(focal_spp_ft, path = "focal_spp_ft.docx")

#### Species Richness ----

# transect level
spp_rich_t <- dat_fish_t %>% 
  filter(dens_100m2 > 0) %>%
  group_by(transect) %>%
  summarise(SR_all = n_distinct(Genus_spp))

spp_rich_t_focal <- dat_fish_t %>% 
  filter(dens_100m2 > 0) %>%
  filter(Genus_spp %in% focal_spp) %>% 
  group_by(transect) %>%
  summarise(SR_focal = n_distinct(Genus_spp))


spp_rich_t <- spp_rich_t %>%
  left_join(spp_rich_t_focal) %>%
  replace_na(list(SR_focal = 0)) # manually fills transect w/ 0 focal SR

spp_rich_t <- spp_rich_t %>% 
  left_join(transect_var)

spp_rich_t <- spp_rich_t %>% 
  mutate(os = paste(Module_Side, Orientation),
         os_ht = paste(os, Habitat_Type))

spp_rich_t <- spp_rich_t %>% 
  mutate(cluster = (case_when(startsWith(os_ht, "West Perpendicular Mid_High") ~ "Group 4",
                              startsWith(os_ht, "West Perpendicular Ecotone_High") ~ "Group 3",
                              startsWith(os_ht, "West Perpendicular Mid_Low") ~ "Group 3",
                              startsWith(os_ht, "West Perpendicular Ecotone_Low") ~ "Group 3",
                              startsWith(os_ht, "West Perpendicular Mid_Medium") ~ "Group 2",
                              startsWith(os_ht, "West Perpendicular Ecotone_Medium") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Mid_High") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Ecotone_High") ~ "Group 1",
                              startsWith(os_ht, "East Perpendicular Mid_Low") ~ "Group 4",
                              startsWith(os_ht, "East Perpendicular Ecotone_Low") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Mid_Medium") ~ "Group 4",
                              startsWith(os_ht, "East Perpendicular Ecotone_Medium") ~ "Group 3",
                              startsWith(os_ht, "West Parallel Mid_High") ~ "Group 2",
                              startsWith(os_ht, "West Parallel Ecotone_High") ~ "Group 2",
                              startsWith(os_ht, "West Parallel Mid_Low") ~ "Group 4",
                              startsWith(os_ht, "West Parallel Ecotone_Low") ~ "Group 2",
                              startsWith(os_ht, "West Parallel Mid_Medium") ~ "Group 2",
                              startsWith(os_ht, "West Parallel Ecotone_Medium") ~ "Group 4",
                              startsWith(os_ht, "East Parallel Mid_High") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Ecotone_High") ~ "Group 4",
                              startsWith(os_ht, "East Parallel Mid_Low") ~ "Group 5",
                              startsWith(os_ht, "East Parallel Ecotone_Low") ~ "Group 5",
                              startsWith(os_ht, "East Parallel Mid_Medium") ~ "Group 5",
                              startsWith(os_ht, "East Parallel Ecotone_Medium") ~ "Group 5")))


spp_rich_t <- spp_rich_t %>% 
  mutate(current =  case_when(startsWith(os, "West") ~ "Up-current",
                              startsWith(os, "East") ~ "Down-current"))
spp_rich_t <- spp_rich_t %>%
  mutate(cluster_2 = cluster)

spp_rich_t <- spp_rich_t %>%
  mutate(cluster_2 = str_replace_all(cluster_2, "Group 6", "Perpendicular Up-current High Relief"),
         cluster_2 = str_replace_all(cluster_2, "Group 5", "Perpendicular Down-current High Ecotone"),
         cluster_2 = str_replace_all(cluster_2, "Group 1", "Inshore Parallel"),
         cluster_2 = str_replace_all(cluster_2, "Group 2", "Low Relief & Ecotones"),
         cluster_2 = str_replace_all(cluster_2, "Group 3", "Intermediate"),
         cluster_2 = str_replace_all(cluster_2, "Group 4", "Offshore High Relief"))

spp_rich_t <- spp_rich_t %>% 
  mutate(Habitat_Type = str_replace_all(Habitat_Type, "Ecotone_High", "High Ecotone"),
         Habitat_Type = str_replace_all(Habitat_Type, "Ecotone_Medium", "Medium Ecotone"),
         Habitat_Type = str_replace_all(Habitat_Type, "Ecotone_Low", "Low Ecotone"),
         Habitat_Type = str_replace_all(Habitat_Type, "Mid_High", "High Relief"),
         Habitat_Type = str_replace_all(Habitat_Type, "Mid_Medium", "Medium Relief"),
         Habitat_Type = str_replace_all(Habitat_Type, "Mid_Low", "Low Relief"))




# Habitat Type level
spp_rich_ht <- spp_rich_t %>% 
  group_by(Habitat_Type) %>% 
  summarise(mean_sr_focal = mean(SR_focal),
            sd_sr_focal = sd(SR_focal),
            max_sr_focal = max(SR_focal),
            min_sr_focal = min(SR_focal),
            median_sr_focal = median(SR_focal))


spp_rich_ht_ft <- flextable(spp_rich_ht,
                            col_keys = c("Habitat_Type", 
                                         "min_sr_focal",
                                         "median_sr_focal",
                                         "max_sr_focal",
                                         "mean_sr_focal",
                                         "sd_sr_focal")) %>%
  add_header_row(colwidths = c(1,5), values = c("Habitat Type", "Focal Species Richness")) %>%
  set_header_labels(Habitat_Type = "Habitat Type",
                    min_sr_focal = "Min",
                    median_sr_focal = "Median",
                    max_sr_focal = "Max",
                    mean_sr_focal = "Mean SR",
                    sd_sr_focal = "sd")%>% 
  colformat_double(digits = 1) %>%
  theme_box() %>%
  align(align = "center") %>%
  align(part = "header", align = "center") %>%
  merge_v(part = "header")

#spp_rich_ht_ft
write_csv(spp_rich_ht, "tables/spp_rich_ht.csv")
# save_as_docx(spp_rich_ht_ft, path = "spp_rich_ht_ft.docx")


##Species richness by zone

spp_rich_zone <- spp_rich_t %>% 
  group_by(Zone) %>% 
  summarise(mean_sr_focal = mean(SR_focal),
            sd_sr_focal = sd(SR_focal),
            max_sr_focal = max(SR_focal),
            min_sr_focal = min(SR_focal),
            median_sr_focal = median(SR_focal))


spp_rich_zone_ft <- flextable(spp_rich_zone,
                              col_keys = c("Zone", 
                                           "min_sr_focal",
                                           "median_sr_focal",
                                           "max_sr_focal",
                                           "mean_sr_focal",
                                           "sd_sr_focal")) %>%
  add_header_row(colwidths = c(1,5), values = c("Zone", "Focal Species Richness")) %>%
  set_header_labels(Zone = "Zone",
                    min_sr_focal = "Min",
                    median_sr_focal = "Median",
                    max_sr_focal = "Max",
                    mean_sr_focal = "Mean SR",
                    sd_sr_focal = "sd")%>% 
  colformat_double(digits = 1) %>%
  theme_box() %>%
  align(align = "center") %>%
  align(part = "header", align = "center") %>%
  merge_v(part = "header")

#spp_rich_zone_ft
write_csv(spp_rich_zone, "tables/spp_rich_zone.csv")
# save_as_docx(spp_rich_zone, path = "spp_rich_zone.docx")



# Module level
spp_rich_mod <- spp_rich_t %>% 
  group_by(Module) %>% 
  summarise(mean_sr_all = mean(SR_all),
            sd_sr_all = sd(SR_all),
            max_sr_all = max(SR_all),
            min_sr_all = min(SR_all),
            mean_sr_focal = mean(SR_focal),
            sd_sr_focal = sd(SR_focal),
            max_sr_focal = max(SR_focal),
            min_sr_focal = min(SR_focal))


spp_rich_mod_ft <- flextable(spp_rich_mod,
                             col_keys = c("Module", 
                                          "mean_sr_all",
                                          "sd_sr_all",
                                          "mean_sr_focal",
                                          "sd_sr_focal")) %>%
  add_header_row(colwidths = c(1,2,2), values = c("Module", "All Species", "Focal Species")) %>%
  set_header_labels(Module = "Module",
                    mean_sr_all = "Mean SR",
                    sd_sr_all = "SD",
                    mean_sr_focal = "Mean SR",
                    sd_sr_focal = "SD")%>% 
  colformat_double(digits = 1) %>%
  theme_box() %>%
  align(align = "center") %>%
  align(part = "header", align = "center") %>%
  merge_v(part = "header")

#spp_rich_mod_ft
write_csv(spp_rich_mod, "tables/spp_rich_mod.csv")
# save_as_docx(spp_rich_mod_ft, path = "spp_rich_mod_ft.docx")


# OS_HT level
spp_rich_os_ht <- spp_rich_t %>% 
  group_by(cluster, Orientation, current, Habitat_Type) %>% 
  summarise(mean_sr_focal = mean(SR_focal),
            sd_sr_focal = sd(SR_focal),
            max_sr_focal = max(SR_focal),
            min_sr_focal = min(SR_focal),
            median_sr_focal = median(SR_focal))

spp_rich_os_ht_ft <- flextable(spp_rich_os_ht,
                               col_keys = c("cluster",
                                            "Orientation",
                                            "current",
                                            "Habitat_Type", 
                                            "min_sr_focal",
                                            "median_sr_focal",
                                            "max_sr_focal",
                                            "mean_sr_focal",
                                            "sd_sr_focal")) %>%
  add_header_row(colwidths = c(1,1,1,1,5), values = c("Cluster","Orientation", "Side", "Habitat Type", "Focal Species Richness")) %>%
  set_header_labels(cluster = "Cluster",
                    Orientation = "Orientation",
                    current = "Side",
                    Habitat_Type = "Habitat Type",
                    min_sr_focal = "Min",
                    median_sr_focal = "Median",
                    max_sr_focal = "Max",
                    mean_sr_focal = "Mean",
                    sd_sr_focal = "sd")%>% 
  colformat_double(digits = 1) %>%
  theme_box() %>%
  align(align = "center") %>%
  align(part = "header", align = "center") %>%
  bg(j = "cluster", i = ~ cluster == "Group 1", bg = "purple4", part = "body") %>% 
  bg(j = "cluster", i = ~ cluster == "Group 2", bg = "steelblue3", part = "body") %>% 
  bg(j = "cluster", i = ~ cluster == "Group 3", bg = "springgreen4", part = "body") %>% 
  bg(j = "cluster", i = ~ cluster == "Group 4", bg = "coral2", part = "body") %>% 
  bg(j = "cluster", i = ~ cluster == "Group 5", bg = "khaki4", part = "body") %>% 
  bg(j = "cluster", i = ~ cluster == "Group 6", bg = "gray", part = "body") %>% 
  merge_v(part = "header")

#spp_rich_os_ht_ft
write_csv(spp_rich_os_ht, "tables/spp_rich_os_ht.csv")
# save_as_docx(spp_rich_os_ht_ft, path = "spp_rich_os_ht_ft.docx")





#BLOCK
spp_rich_block <- spp_rich_t %>% 
  group_by(Block) %>% 
  summarise(mean_sr_all = mean(SR_all),
            sd_sr_all = sd(SR_all),
            max_sr_all = max(SR_all),
            min_sr_all = min(SR_all), 
            mean_sr_focal = mean(SR_focal),
            sd_sr_focal = sd(SR_focal),
            max_sr_focal = max(SR_focal),
            min_sr_focal = min(SR_focal))

spp_rich_block_ft <- flextable(spp_rich_block,
                               col_keys = c("Block", 
                                            "mean_sr_all",
                                            "sd_sr_all",
                                            "mean_sr_focal",
                                            "sd_sr_focal")) %>%
  add_header_row(colwidths = c(1,2,2), values = c("Block", "All Species", "Focal Species")) %>%
  set_header_labels(Block = "Block",
                    mean_sr_all = "Mean SR",
                    sd_sr_all = "SD",
                    mean_sr_focal = "Mean SR",
                    sd_sr_focal = "SD")%>% 
  colformat_double(digits = 1) %>%
  theme_box() %>%
  align(align = "center") %>%
  align(part = "header", align = "center") %>%
  merge_v(part = "header")

#spp_rich_block_ft
write_csv(spp_rich_block, "tables/spp_rich_block.csv")
# save_as_docx(spp_rich_block_ft, path = "spp_rich_block_ft.docx")


###cluster spp richness
spp_rich_clust <- spp_rich_t %>% 
  group_by(cluster) %>% 
  summarise(mean_sr_focal = mean(SR_focal),
            sd_sr_focal = sd(SR_focal),
            max_sr_focal = max(SR_focal),
            min_sr_focal = min(SR_focal),
            median_sr_focal = median(SR_focal))

spp_rich_clust_ft <- flextable(spp_rich_clust,
                               col_keys = c("cluster",
                                            "min_sr_focal",
                                            "median_sr_focal",
                                            "max_sr_focal",
                                            "mean_sr_focal",
                                            "sd_sr_focal")) %>%
  add_header_row(colwidths = c(1,5), values = c("Cluster", "Focal Species Richness")) %>%
  set_header_labels(cluster = "Cluster",
                    min_sr_focal = "Min",
                    median_sr_focal = "Median",
                    max_sr_focal = "Max",
                    mean_sr_focal = "Mean",
                    sd_sr_focal = "sd")%>% 
  colformat_double(digits = 1) %>%
  align(align = "center") %>%
  align(part = "header", align = "center") %>%
  merge_v(part = "header")

spp_rich_clust_ft
write_csv(spp_rich_clust, "tables/spp_rich_clust.csv")
# save_as_docx(spp_rich_clust_ft, path = "spp_rich_clust_ft.docx")



spp_rich_clust <- spp_rich_t %>% 
  group_by(cluster) %>% 
  summarise(mean_sr_all = mean(SR_all),
            sd_sr_all = sd(SR_all),
            max_sr_all = max(SR_all),
            min_sr_all = min(SR_all), 
            mean_sr_focal = mean(SR_focal),
            sd_sr_focal = sd(SR_focal),
            max_sr_focal = max(SR_focal),
            min_sr_focal = min(SR_focal))

spp_rich_clust_ft <- flextable(spp_rich_clust,
                               col_keys = c("cluster", 
                                            "mean_sr_all",
                                            "sd_sr_all",
                                            "mean_sr_focal",
                                            "sd_sr_focal")) %>%
  add_header_row(colwidths = c(1,2,2), values = c("Cluster", "All Species", "Focal Species")) %>%
  set_header_labels(cluster = "Cluster",
                    mean_sr_all = "Mean SR",
                    sd_sr_all = "SD",
                    mean_sr_focal = "Mean SR",
                    sd_sr_focal = "SD")%>% 
  colformat_double(digits = 1) %>%
  theme_box() %>%
  align(align = "center") %>%
  align(part = "header", align = "center") %>%
  # bg(j = "cluster", i = ~ cluster == "Group 1", bg = "gray", part = "body") %>% 
  # bg(j = "cluster", i = ~ cluster == "Group 2", bg = "khaki4", part = "body") %>% 
  # bg(j = "cluster", i = ~ cluster == "Group 3", bg = "purple4", part = "body") %>% 
  # bg(j = "cluster", i = ~ cluster == "Group 4", bg = "steelblue3", part = "body") %>% 
  # bg(j = "cluster", i = ~ cluster == "Group 5", bg = "springgreen4", part = "body") %>% 
  # bg(j = "cluster", i = ~ cluster == "Group 6", bg = "coral2", part = "body") %>% 
  merge_v(part = "header")

#spp_rich_clust_ft
write_csv(spp_rich_clust, "tables/spp_rich_clust.csv")
# save_as_docx(spp_rich_clust_ft, path = "spp_rich_clust_ft.docx")

#Table for focal spp density across 6 habitat_types (complete)

## TO DO - could make single SR table formatted in flextable (mod, HT, OS_HT?)
# round sr values to 0.1

## TO DO - could use spp_rich_t to do richness comparison plot exact same format as
# single species plots, but w/ SR_focal instead of density per transect. (probably just with focal species - detectability not good enough to use all species SR)


#### Fish Density ----
# species level (across all transects)
dat_fish_t <- dat_fish_t %>% 
  mutate(current =  case_when(startsWith(os, "West") ~ "Up-current",
                              startsWith(os, "East") ~ "Down-current"))
# dat_fish_t <- dat_fish_t %>%
#   mutate(cluster_2 = cluster)

# dat_fish_t <- dat_fish_t %>%
#   mutate(cluster_2 = str_replace_all(cluster_2, "Group 6", "Perpendicular Inshore High Relief"),
#          cluster_2 = str_replace_all(cluster_2, "Group 5", "Perpendicular Offshore High Ecotone"),
#          cluster_2 = str_replace_all(cluster_2, "Group 1", "Sheltered Inshore Parallel"),
#          cluster_2 = str_replace_all(cluster_2, "Group 2", "Low Relief & Ecotones"),
#          cluster_2 = str_replace_all(cluster_2, "Group 3", "Intermediate"),
#          cluster_2 = str_replace_all(cluster_2, "Group 4", "Offshore High & Medium Relief"))

dat_fish_t <- dat_fish_t %>% 
  mutate(Habitat_Type = str_replace_all(Habitat_Type, "Ecotone_High", "High Ecotone"),
         Habitat_Type = str_replace_all(Habitat_Type, "Ecotone_Medium", "Medium Ecotone"),
         Habitat_Type = str_replace_all(Habitat_Type, "Ecotone_Low", "Low Ecotone"),
         Habitat_Type = str_replace_all(Habitat_Type, "Mid_High", "High Relief"),
         Habitat_Type = str_replace_all(Habitat_Type, "Mid_Medium", "Medium Relief"),
         Habitat_Type = str_replace_all(Habitat_Type, "Mid_Low", "Low Relief"))


dens_sp <- dat_fish_t %>% 
  group_by(Genus_spp) %>%
  summarise(median_dens = median(dens_100m2), 
            mean_dens = mean(dens_100m2),
            sd_dens = sd(dens_100m2),
            max_dens = max(dens_100m2),
            min_dens = min(dens_100m2)) %>% 
  filter(Genus_spp %in% focal_spp) %>% 
  ungroup() %>% 
  arrange(desc(mean_dens))

set_flextable_defaults(font.family = "Arial", font.size = 9, digits = 1, padding = 0.1)

dens_sp_ft <- flextable(dens_sp,
                        col_keys = c("Genus_spp", "median_dens",
                                     "mean_dens", "sd_dens", "min_dens", "max_dens")) %>% 
  add_header_row(colwidths = c(1,5), values = c("Species Name", "Focal fish density (No./100 m2) ")) %>% 
  set_header_labels(Genus_spp = "Species Name",
                    median_dens = "Median",
                    mean_dens = "Mean",
                    sd_dens = "SD",
                    min_dens = "Min",
                    max_dens = "Max") %>% 
  colformat_double(digits = 1) %>% 
  theme_box() %>% 
  align(align = "center") %>% 
  align(part = "header", align = "center") %>% 
  compose(j = "Genus_spp",
          value = as_paragraph(as_i(Genus_spp))) %>% 
  merge_v(part = "header") 

### Tried to get superscript to work
# dens_sp_ft <- dens_sp_ft %>% 
#   compose(j = "mean_dens", part = "header",
#           value = as_paragraph("Focal Fish Density 100m ",
#                                as_chunk("2",
#                                porps = fp_text_default(color = "black", vertical.align = "superscript"))))

#dens_sp_ft
write_csv(dens_sp, "tables/dens_sp.csv")
# save_as_docx(dens_sp_ft, path = "dens_sp_ft.docx")



# sp dens by Zone
dens_sp_zone <- dat_fish_t %>% 
  group_by(Genus_spp, Zone) %>%
  summarise(median_dens = median(dens_100m2), 
            mean_dens = mean(dens_100m2),
            sd_dens = sd(dens_100m2),
            max_dens = max(dens_100m2),
            min_dens = min(dens_100m2)) %>% 
  filter(Genus_spp %in% focal_spp)

dens_sp_zone_ft <- flextable(dens_sp_zone,
                             col_keys = c("Genus_spp", "Zone", "median_dens",
                                          "mean_dens", "sd_dens", "max_dens", "min_dens")) %>% 
  add_header_row(colwidths = c(1, 1, 5), values = c("Species Name", "Zone", "Focal fish density (No./100 m2)")) %>% 
  set_header_labels(Genus_spp = "Species Name",
                    Zone = "Zone",
                    median_dens = "Median",
                    mean_dens = "Mean",
                    sd_dens = "Standard Deviation",
                    max_dens = "Max",
                    min_dens = "Min") %>% 
  colformat_double(digits = 1) %>% 
  theme_box() %>% 
  align(align = "center") %>% 
  align(part = "header", align = "center") %>% 
  compose(j = "Genus_spp",
          value = as_paragraph(as_i(Genus_spp))) %>% 
  merge_v(part = "header")

#dens_sp_zone_ft




# sp dens by Habitat_Type
dens_sp_ht <- dat_fish_t %>% 
  group_by(Genus_spp, Habitat_Type) %>%
  summarise(median_dens = median(dens_100m2), 
            mean_dens = mean(dens_100m2),
            sd_dens = sd(dens_100m2),
            max_dens = max(dens_100m2),
            min_dens = min(dens_100m2)) %>% 
  filter(Genus_spp %in% focal_spp)


dens_sp_ht_ft <- flextable(dens_sp_ht,
                           col_keys = c("Genus_spp", "Habitat_Type", "median_dens",
                                        "mean_dens", "sd_dens", "max_dens", "min_dens")) %>% 
  add_header_row(colwidths = c(1, 1, 5), values = c("Species Name", "Habitat Type", "Focal fish density (No./100 m2)")) %>% 
  set_header_labels(Genus_spp = "Species Name",
                    Habitat_Type = "Habitat Type",
                    median_dens = "Median",
                    mean_dens = "Mean",
                    sd_dens = "SD",
                    max_dens = "Max",
                    min_dens = "Min") %>% 
  colformat_double(digits = 1) %>% 
  theme_box() %>% 
  align(align = "center") %>% 
  align(part = "header", align = "center") %>% 
  compose(j = "Genus_spp",
          value = as_paragraph(as_i(Genus_spp))) %>% 
  merge_v(part = "header")

#dens_sp_ht_ft
write_csv(dens_sp_ht, "tables/dens_sp_ht.csv")
# save_as_docx(dens_sp_ht_ft, path = "dens_sp_ht_ft.docx")





# sp dens by OS_HT
dens_sp_os_ht <- dat_fish_t %>% 
  group_by(Genus_spp, Orientation, current, Habitat_Type, os_ht) %>%
  summarise(median_dens = median(dens_100m2), 
            mean_dens = mean(dens_100m2),
            sd_dens = sd(dens_100m2),
            max_dens = max(dens_100m2),
            min_dens = min(dens_100m2)) %>% 
  filter(Genus_spp %in% focal_spp)


dens_sp_os_ht <- dens_sp_os_ht %>% 
  mutate(cluster = (case_when(startsWith(os_ht, "West Perpendicular Mid_High") ~ "Group 6",
                              startsWith(os_ht, "West Perpendicular Ecotone_High") ~ "Group 2",
                              startsWith(os_ht, "West Perpendicular Mid_Low") ~ "Group 3",
                              startsWith(os_ht, "West Perpendicular Ecotone_Low") ~ "Group 3",
                              startsWith(os_ht, "West Perpendicular Mid_Medium") ~ "Group 4",
                              startsWith(os_ht, "West Perpendicular Ecotone_Medium") ~ "Group 4",
                              startsWith(os_ht, "East Perpendicular Mid_High") ~ "Group 4",
                              startsWith(os_ht, "East Perpendicular Ecotone_High") ~ "Group 5",
                              startsWith(os_ht, "East Perpendicular Mid_Low") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Ecotone_Low") ~ "Group 3",
                              startsWith(os_ht, "East Perpendicular Mid_Medium") ~ "Group 3",
                              startsWith(os_ht, "East Perpendicular Ecotone_Medium") ~ "Group 3",
                              startsWith(os_ht, "West Parallel Mid_High") ~ "Group 4",
                              startsWith(os_ht, "West Parallel Ecotone_High") ~ "Group 4",
                              startsWith(os_ht, "West Parallel Mid_Low") ~ "Group 3",
                              startsWith(os_ht, "West Parallel Ecotone_Low") ~ "Group 2",
                              startsWith(os_ht, "West Parallel Mid_Medium") ~ "Group 4",
                              startsWith(os_ht, "West Parallel Ecotone_Medium") ~ "Group 2",
                              startsWith(os_ht, "East Parallel Mid_High") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Ecotone_High") ~ "Group 1",
                              startsWith(os_ht, "East Parallel Mid_Low") ~ "Group 2",
                              startsWith(os_ht, "East Parallel Ecotone_Low") ~ "Group 1",
                              startsWith(os_ht, "East Parallel Mid_Medium") ~ "Group 1",
                              startsWith(os_ht, "East Parallel Ecotone_Medium") ~ "Group 1")))




dens_sp_os_ht <- dens_sp_os_ht %>% 
  arrange(cluster)

dens_sp_os_ht <- dens_sp_os_ht %>% 
  mutate(os = paste(Orientation, current))

dens_sp_os_ht <- dens_sp_os_ht %>% 
  mutate(os = (case_when(startsWith(os, "Perpendicular Up-current") ~ "West",
                         startsWith(os, "Perpendicular Down-current") ~ "East",
                         startsWith(os, "Parallel Up-current") ~ "Offshore",
                         startsWith(os, "Parallel Down-current") ~ "Inshore")))


dens_sp_os_ht <- dens_sp_os_ht %>% 
  arrange(desc(Genus_spp))

dens_sp_os_ht_ft <- flextable(dens_sp_os_ht,
                              col_keys = c("Genus_spp", "Orientation", "os", "Habitat_Type", "cluster","min_dens", "median_dens","max_dens", "mean_dens", "sd_dens")) %>% 
  add_header_row(colwidths = c(1,1,1,1,1,5), values = c("Species Name", "Orientation", "Side", "Habitat Type", "Cluster", "Focal fish density (No./100 m2)")) %>% 
  set_header_labels(Genus_spp = "Species Name",
                    Orientation = "Orientation",
                    os = "Side",
                    Habitat_Type = "Habitat Type",
                    cluster = "Cluster",
                    min_dens = "Min",
                    median_dens = "Median",
                    max_dens = "Max",
                    mean_dens = "Mean",
                    sd_dens = "sd") %>% 
  colformat_double(digits = 1) %>% 
  padding(padding = 0.1) %>% 
  # theme_box() %>% 
  align(align = "center") %>% 
  align(part = "header", align = "center") %>% 
  compose(j = "Genus_spp",
          value = as_paragraph(as_i(Genus_spp))) %>% 
  # bg(j = "cluster", i = ~ cluster == "Group 1", bg = "gray", part = "body") %>% 
  # bg(j = "cluster", i = ~ cluster == "Group 2", bg = "khaki4", part = "body") %>% 
  # bg(j = "cluster", i = ~ cluster == "Group 3", bg = "purple4", part = "body") %>% 
  # bg(j = "cluster", i = ~ cluster == "Group 4", bg = "steelblue3", part = "body") %>% 
  # bg(j = "cluster", i = ~ cluster == "Group 5", bg = "springgreen4", part = "body") %>% 
  # bg(j = "cluster", i = ~ cluster == "Group 6", bg = "coral2", part = "body") %>% 
  merge_v(part = "header")

dens_sp_os_ht_ft

write_csv(dens_sp_os_ht, "tables/dens_sp_os_ht.csv")
# save_as_docx(dens_sp_os_ht_ft, path = "dens_sp_os_ht_ft.docx")


#### Heatmap ----
dens_sp_order <- dens_sp_os_ht %>% 
  group_by(Genus_spp) %>% 
  summarise(mean_dens = mean(mean_dens)) %>% 
  arrange(mean_dens)

dens_sp_os_ht$Genus_spp <- factor(dens_sp_os_ht$Genus_spp, 
                                  levels = dens_sp_order$Genus_spp)

dens_sp_os_ht = dens_sp_os_ht %>% 
  mutate(Habitat_Type = str_replace_all(Habitat_Type, "Relief", "Reef"))

dens_sp_os_ht = dens_sp_os_ht %>% 
  mutate(dend_group = paste(Orientation, os, Habitat_Type))

cluster_df <- data.frame(
  dend_group = c("Parallel Inshore High Ecotone", "Perpendicular East High Ecotone", "Parallel Offshore High Ecotone", 
                 "Perpendicular West High Ecotone", "Parallel Inshore High Reef", "Perpendicular East High Reef", 
                 "Parallel Offshore High Reef", "Perpendicular West High Reef", "Parallel Inshore Low Ecotone", 
                 "Perpendicular East Low Ecotone", "Parallel Offshore Low Ecotone", "Perpendicular West Low Ecotone", 
                 "Parallel Inshore Low Reef", "Perpendicular East Low Reef", "Parallel Offshore Low Reef", 
                 "Perpendicular West Low Reef", "Parallel Inshore Medium Ecotone", "Perpendicular East Medium Ecotone", 
                 "Parallel Offshore Medium Ecotone", "Perpendicular West Medium Ecotone", "Parallel Inshore Medium Reef", 
                 "Perpendicular East Medium Reef", "Parallel Offshore Medium Reef", "Perpendicular West Medium Reef"),
  cluster_group = c(1, 2, 3, 4, 4, 3, 3, 1, 5, 3, 3, 4, 5, 1, 1, 4, 5, 4, 1, 3, 5, 1, 3, 3)
)


dens_sp_os_ht <- dens_sp_os_ht %>%
  left_join(cluster_df, by = "dend_group")

dens_sp_os_ht = dens_sp_os_ht %>% 
  mutate(cluster_ordered = (case_when(startsWith(as.character(cluster_group), "1") ~ "4",
                                      startsWith(as.character(cluster_group), "2") ~ "1",
                                      startsWith(as.character(cluster_group), "3") ~ "2",
                                      startsWith(as.character(cluster_group), "4") ~ "3",
                                      startsWith(as.character(cluster_group), "5") ~ "5")))


dens_sp_clust_heatmap <- dens_sp_os_ht %>% 
  group_by(cluster_ordered, Genus_spp) %>% 
  summarise(mean_clust_dens = mean(mean_dens), 
            min_clust_dens = min(mean_dens),
            max_clust_dens = max(mean_dens),
            count_tt = n())

group_color_lab <- c("black","coral2","springgreen4", "steelblue3","purple4")

heatmap_sp_clust<-dens_sp_clust_heatmap %>%
  ggplot(aes(x=cluster_ordered, y = Genus_spp)) +
  geom_tile(aes(fill = mean_clust_dens ), position = "identity", colour = "black") +
  scale_fill_gradientn(trans= "log1p", 
                       colors = c("white","red","dark blue"),
                       #colours = wes_palette("Zissou1", 100, type = "continuous"),
                       limits=c(0, 400), 
                       breaks = c(0, 1, 3, 6, 12, 25, 50, 100, 200, 400),
                       name= expression(paste("Density\n(No./100",m^{2},")"))) + #name= expression(density~(No.~100m,^{-2}))) +
  theme_bw() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 18)) + 
  theme(axis.text.y = element_text(face = "bold", color = "black"), axis.text.x = element_text(face = "bold", color = group_color_lab)) +
  #xlab('Depth group (m)') +
  #scale_x_discrete(limits = rev(levels(dens_sp_clust_heatmap$cluster))) +
  ylab(NULL) +
  xlab(NULL) +
  
  geom_text(aes(label = paste(round(mean_clust_dens, 1), "\n", 
                              paste("(", 
                                    round(min_clust_dens, 1), 
                                    ", ", 
                                    round(max_clust_dens, 1), 
                                    ")", sep = ""))), 
            size = 2.2, 
            fontface = "bold") +
  scale_y_discrete(labels = c("Rock Wrasse","Olive Rockfish","Pile Perch","Opaleye","Barred Sand Bass","Rainbow Seaperch", "Black Perch","California Sheephead","Kelp Bass","Señorita","Blacksmith"))


heatmap_sp_clust

ggsave(heatmap_sp_clust, file="figures/heatmap.png", width=6, height=6, dpi=600) 



#### SPP dens across haibtat types
# sp dens by module
dens_sp_mod <- dat_fish_t %>% 
  group_by(Genus_spp, Module) %>%
  summarise(median_dens = median(dens_100m2), 
            mean_dens = mean(dens_100m2),
            sd_dens = sd(dens_100m2),
            max_dens = max(dens_100m2),
            min_dens = min(dens_100m2)) %>% 
  filter(Genus_spp %in% focal_spp)


###CHANGE TO MODULE
dens_sp_ht_ft <- flextable(dens_sp_ht,
                           col_keys = c("Genus_spp", "Habitat_Type", "median_dens",
                                        "mean_dens", "sd_dens", "max_dens", "min_dens")) %>% 
  add_header_row(colwidths = c(1, 1, 5), values = c("Species Name", "Habitat Type", "Focal fish density (No./100 m2)")) %>% 
  set_header_labels(Genus_spp = "Species Name",
                    Habitat_Type = "Habitat Type",
                    median_dens = "Median",
                    mean_dens = "Mean",
                    sd_dens = "Standard Deviation",
                    max_dens = "Max",
                    min_dens = "Min") %>% 
  colformat_double(digits = 1) %>% 
  theme_box() %>% 
  align(align = "center") %>% 
  align(part = "header", align = "center") %>% 
  compose(j = "Genus_spp",
          value = as_paragraph(as_i(Genus_spp))) %>% 
  merge_v(part = "header")

dens_sp_ht_ft
#### Module NMDS ----
dat_fish_ht_mod_18 <- dat_fish_t %>% 
  group_by(Module, Genus_spp) %>% 
  summarise(dens_100m2 = mean(dens_100m2)) %>% 
  ungroup() %>% 
  left_join(
    distinct(select(transect_var, !c("Module_Side", "Habitat_Type", "Sub_Mod", "transect", "Zone")))
  )

wide_fish_ht_mod_18 <- dat_fish_ht_mod_18 %>% 
  mutate(dens_100m2_sqrt = dens_100m2^0.5) %>% 
  filter(Genus_spp %in% focal_spp) %>% 
  pivot_wider(id_cols = Module, names_from = Genus_spp, values_from = dens_100m2_sqrt)

names(wide_fish_ht_mod_18) <- str_replace_all(names(wide_fish_ht_mod_18), c(" " = "_"))



comm_fish_ht_mod_18 <- wide_fish_ht_mod_18 %>%
  column_to_rownames(var = "Module") %>%
  select(Chromis_punctipinnis:Semicossyphus_pulcher)


NMDS_comm_fish_ht_mod_18 <- metaMDS(comm_fish_ht_mod_18, 
                                    trymax = 200,
                                    distance = "bray",
                                    autotransform = F)


tibble_comm_fish_ht_mod_18 <- as_tibble(scores(NMDS_comm_fish_ht_mod_18$points), 
                                        rownames = ("Module"))


wide_fish_ht_mod_18 <- wide_fish_ht_mod_18 %>% 
  left_join(tibble_comm_fish_ht_mod_18)




wide_fish_ht_mod_18 <- wide_fish_ht_mod_18 %>% 
  left_join(
    distinct(select(transect_var, !c("Module_Side", "Habitat_Type", "Sub_Mod", "transect", "Zone")))
  )

wide_fish_ht_mod_18 <- wide_fish_ht_mod_18 %>% 
  mutate(construction_group = (case_when(startsWith(as.character(construction_group), "1") ~ "CP 1",
                                         startsWith(as.character(construction_group), "2") ~ "CP 2")))

wide_fish_ht_mod_18 <- wide_fish_ht_mod_18 %>% 
  mutate(mid_high_hgt_m = (case_when(startsWith(mid_high_hgt_m, "3") ~ "3 m",
                                     startsWith(mid_high_hgt_m, "4") ~ "4 m"))) 

plot_wide_fish_ht_mod_18 <- ggplot(wide_fish_ht_mod_18,
                                   aes(MDS1, MDS2)) +
  geom_text(aes(label = Module, vjust = 2, hjust = .2), show.legend = F) +
  geom_point(aes(shape = as_factor(construction_group), color = mid_high_hgt_m), size = 4) +
  theme_classic() +
  scale_colour_manual(values = c("#FF62BC", "#39B600")) +
  scale_shape_manual(values = c(15,16)) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid")) +
  labs(shape = "Construction Phase", color = "Maximum Vertical Relief")


plot_wide_fish_ht_mod_18


hull_ht_mod_18 <- wide_fish_ht_mod_18 %>% 
  group_by(construction_group) %>% 
  slice(chull(MDS1, MDS2))

plot_wide_fish_ht_mod_18_hulls <- plot_wide_fish_ht_mod_18 +
  geom_polygon(data = hull_ht_mod_18,
               aes(x = MDS1, y = MDS2, 
                   fill = as_factor(construction_group),
                   group = as_factor(construction_group)), alpha = 0.2) +
  scale_fill_manual(values = c("#F8766D", "#00B0F6")) +
  geom_text(x = 0.5, y = 0.3, label = c(paste("2D Stress:" ,round(NMDS_comm_fish_ht_mod_18$stress,2), sep = " ")), color = "black") +
  guides(shape = guide_legend(title = "Construction Phase"),
         fill = guide_legend(title = "Construction Phase")) +
  theme(legend.position = "top")


plot_wide_fish_ht_mod_18_hulls



ggsave("figures/module_NMDS.png", plot_wide_fish_ht_mod_18_hulls,
       width = 6, height = 8, dpi = 600)


#### Module Cluster Analysis ----
wide_fish_ht_mod_18 <- wide_fish_ht_mod_18 %>% 
  mutate(dend_lab = paste(Module, construction_group, mid_high_hgt_m))

wide_fish_ht_mod_18 <- wide_fish_ht_mod_18 %>% 
  mutate(dend_lab = str_replace(dend_lab, "CP 1 3 m", "(CP 1, 3 m)"),
         dend_lab = str_replace(dend_lab, "CP 2 3 m", "(CP 2, 3 m)"),
         dend_lab = str_replace(dend_lab, "CP 1 4 m", "(CP 1, 4 m)"),
         dend_lab = str_replace(dend_lab, "CP 2 4 m", "(CP 2, 4 m)"))


comm_fish_ht_mod_18 <- wide_fish_ht_mod_18 %>%
  column_to_rownames(var = "dend_lab") %>%
  select(Chromis_punctipinnis:Semicossyphus_pulcher)

#Create a distance matrix based on the community assemblages
dis.comm_fish_ht_mod_18 <- vegdist(comm_fish_ht_mod_18)

#Create a cluster dendrogram
clust.comm_fish_ht_mod_18 <- hclust(dis.comm_fish_ht_mod_18, "average")


#Add color labeles and branches based on the NMDS groups
mod_18_dendro <- color_labels(clust.comm_fish_ht_mod_18, col = "black", k = 1)

mod_18_dendro <- color_branches(mod_18_dendro, col = "black", k = 1) %>% 
  set("labels_cex", 1)
gg_mod_18_dend <- as.ggdend(mod_18_dendro)

plot_gg_mod_18_dend <- ggplot(gg_mod_18_dend, horiz = T, offset_labels = -0.01)
plot_gg_mod_18_dend <- plot_gg_mod_18_dend +
  theme_classic() +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(), axis.text.x = element_text(size = 16), axis.title.x = element_text(size = 16)) +
  labs(y = "Bray-Curtis Dissimilarity") 


plot_gg_mod_18_dend


ggsave("figures/module_dendrogram.png", plot_gg_mod_18_dend,
       width = 10, height = 10, dpi = 600)

#### Module PERMANOVA ----

dis.comm_fish_ht_mod_18 <-vegdist(comm_fish_ht_mod_18)



ado.fish_ht_mod_18 <- adonis2(dis.comm_fish_ht_mod_18 ~  mid_high_hgt_m + construction_group, by = "margin", wide_fish_ht_mod_18)



ado_mod_18_table <- as.data.frame(ado.fish_ht_mod_18)
ado_mod_18_table <- ado_mod_18_table %>% 
  rownames_to_column() %>% 
  mutate(rowname = str_replace(rowname, "mid_high_hgt_m", "Maximum Vertical Relief"),
         rowname = str_replace(rowname, "construction_group", "Construction Group"))

set_flextable_defaults(font.family = "Arial", font.size = 9, digits = 2, padding = 0.1)

ado_mod_18_ft <- flextable(ado_mod_18_table,
                           col_keys = c("rowname", "Df"
                                        , "SumOfSqs", "R2", "F", "Pr(>F)")) %>% 
  set_header_labels(rowname = "Module Factors",
                    Df = "Df",
                    SumOfSqs = "Sum sq", 
                    R2 = "R2") %>% 
  align(align = "center") %>%
  align(part = "header", align = "center") %>%
  colformat_double(digits = 2) 



ado_mod_18_ft

write_csv(ado_mod_18_table, "tables/ado_mod_18_table.csv")
# save_as_docx(ado_mod_18_ft, path = "ado_mod_18_ft.docx")




#### Submodule NMDS  ----
dat_fish_os_ht <- dat_fish_t %>% 
  group_by(Habitat_Type, os, Genus_spp) %>% 
  summarise(dens_100m2 = mean(dens_100m2)) %>% 
  ungroup()


wide_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(dens_100m2_sqrt = dens_100m2^0.5) %>% 
  filter(Genus_spp %in% focal_spp) %>% 
  pivot_wider(id_cols = Habitat_Type:os, names_from = Genus_spp, values_from = dens_100m2_sqrt) %>% 
  mutate(os_ht = paste(os, Habitat_Type))



#Need the column names to not have spaces
names(wide_fish_os_ht) <- str_replace_all(names(wide_fish_os_ht), c(" " = "_"))


comm_fish_os_ht <- wide_fish_os_ht %>%
  column_to_rownames(var = "os_ht") %>%
  select(Chromis_punctipinnis:Semicossyphus_pulcher)



NMDS_comm_fish_os_ht <- metaMDS(comm_fish_os_ht, 
                                trymax = 200,
                                distance = "bray",
                                autotransform = F)

NMDS_comm_fish_os_ht

tibble_comm_fish_os_ht <- as_tibble(scores(NMDS_comm_fish_os_ht$points), 
                                    rownames = ("os_ht"))

wide_fish_os_ht <- wide_fish_os_ht %>% 
  left_join(tibble_comm_fish_os_ht)

wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(Habitat_Type = as.character(Habitat_Type))

wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(Zone = case_when(startsWith(Habitat_Type, "High Ecotone") ~ "Ecotone",
                          startsWith(Habitat_Type, "Medium Ecotone") ~ "Ecotone",
                          startsWith(Habitat_Type, "Low Ecotone") ~ "Ecotone",
                          startsWith(Habitat_Type, "High Relief") ~ "Midline",
                          startsWith(Habitat_Type, "Medium Relief") ~ "Midline",
                          startsWith(Habitat_Type, "Low Relief") ~ "Midline"))

### ADD CLUSTER ANALYSIS - WHICH DEFINES THESE GROUPS
wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(current =  case_when(startsWith(os, "West") ~ "Up-current",
                              startsWith(os, "East") ~ "Down-current"))

wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(Orientation =  case_when(startsWith(os, "West Par") ~ "Parallel",
                                  startsWith(os, "West Per") ~ "Perpendicular",
                                  startsWith(os, "East Par") ~ "Parallel",
                                  startsWith(os, "East Per") ~ "Perpendicular"))


wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(os_lab = paste(Orientation, current))

wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(os_lab = str_replace(os_lab, "Parallel Up-current", "Par. Offshore"),
         os_lab = str_replace(os_lab, "Parallel Down-current", "Par. Inshore"),
         os_lab = str_replace(os_lab, "Perpendicular Up-current", "Perp. West"),
         os_lab = str_replace(os_lab, "Perpendicular Down-current", "Perp. East"))



wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(Habitat_Type = str_replace_all(Habitat_Type, "Relief", "Reef"))

wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(dend_group = paste(os_lab, Habitat_Type))

cluster_df <- data.frame(
  dend_group = c("Par. Inshore High Ecotone", "Perp. East High Ecotone", "Par. Offshore High Ecotone", 
                 "Perp. West High Ecotone", "Par. Inshore High Reef", "Perp. East High Reef", 
                 "Par. Offshore High Reef", "Perp. West High Reef", "Par. Inshore Low Ecotone", 
                 "Perp. East Low Ecotone", "Par. Offshore Low Ecotone", "Perp. West Low Ecotone", 
                 "Par. Inshore Low Reef", "Perp. East Low Reef", "Par. Offshore Low Reef", 
                 "Perp. West Low Reef", "Par. Inshore Medium Ecotone", "Perp. East Medium Ecotone", 
                 "Par. Offshore Medium Ecotone", "Perp. West Medium Ecotone", "Par. Inshore Medium Reef", 
                 "Perp. East Medium Reef", "Par. Offshore Medium Reef", "Perp. West Medium Reef"),
  cluster_group = c(1, 2, 3, 4, 4, 3, 3, 1, 5, 3, 3, 4, 5, 1, 1, 4, 5, 4, 1, 3, 5, 1, 3, 3)
)

wide_fish_os_ht <- wide_fish_os_ht %>%
  left_join(cluster_df, by = "dend_group")

wide_fish_os_ht = wide_fish_os_ht %>% 
  mutate(cluster_ordered = (case_when(startsWith(as.character(cluster_group), "1") ~ "4",
                                      startsWith(as.character(cluster_group), "2") ~ "1",
                                      startsWith(as.character(cluster_group), "3") ~ "2",
                                      startsWith(as.character(cluster_group), "4") ~ "3",
                                      startsWith(as.character(cluster_group), "5") ~ "5")))



wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(Habitat_Type = factor(Habitat_Type, levels = c("High Reef",
                                                        "Medium Reef",
                                                        "Low Reef",
                                                        "High Ecotone",
                                                        "Medium Ecotone",
                                                        "Low Ecotone")))

plot_wide_fish_os_ht <- ggplot(wide_fish_os_ht,
                               aes(-MDS1, -MDS2)) +
  geom_text(aes(label = os_lab), vjust = 3, hjust = .4, size = 5) +
  geom_point(aes(color = as.factor(cluster_ordered), shape = Habitat_Type),size = 7, stroke = 3) +
  scale_shape_manual(values = c(15, 16, 17, 0, 1, 2)) +
  scale_color_manual(values = c("black", "coral2","springgreen4", "steelblue3","purple4")) +
  theme_classic() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = NA, color = "black", size = 1, linetype = "solid")) +
  theme(plot.margin = margin(1,2,1,1, "cm")) +
  theme(text = element_text(size = 16)) +
  guides(shape = guide_legend(title = "Habitat Type"),
         color = "none")

plot_wide_fish_os_ht = plot_wide_fish_os_ht +
  theme(legend.key = element_rect(fill = "white", colour = NA))
plot_wide_fish_os_ht =plot_wide_fish_os_ht +
  guides(shape = guide_legend(override.aes = list(stroke = 1)))
plot_wide_fish_os_ht

hull_os_ht <- wide_fish_os_ht %>%
  group_by(cluster_ordered) %>%
  slice(chull(-MDS1, -MDS2))


plot_wide_fish_os_ht_hulls <- plot_wide_fish_os_ht +
  geom_polygon(data = hull_os_ht, aes(x = -MDS1, y = -MDS2, group = cluster_ordered, fill = cluster_ordered),alpha = 0.3)

plot_wide_fish_os_ht_hulls

hull_os_ht <- wide_fish_os_ht %>% 
  group_by(cluster_ordered) %>% 
  slice(chull(-MDS1, -MDS2))



plot_wide_fish_os_ht_hulls <- plot_wide_fish_os_ht +
  geom_polygon(data = hull_os_ht, aes(x = -MDS1, y = -MDS2, group = as.factor(cluster_ordered), fill = as.factor(cluster_ordered)),alpha = 0.3) + 
  scale_fill_manual(values = c("black","coral2","springgreen4", "steelblue3","purple4")) +
  geom_text(x = -0.58, y = 0.24, label = c(paste("2D Stress:" ,round(NMDS_comm_fish_os_ht$stress,2), sep = " ")), color = "black", size = 8) +
  # guides(fill = guide_legend(title = "Cluster Group"))
  guides(fill = guide_legend(
    title = "Cluster Group",
    override.aes = list(alpha = 1)  # Set alpha to 1 to remove transparency in legend
  ),
  shape = guide_legend(
    title = "Habitat Type",
    nrow = 3, 
    ncol = 2,
    override.aes = list(stroke = 1)
  ))



plot_wide_fish_os_ht_hulls



site_scores <- wide_fish_os_ht %>%
  select(Habitat_Type, os, Zone, MDS1, MDS2, cluster_ordered, os_ht) %>% 
  column_to_rownames(var = "os_ht")

os_ht_spp_fit <- envfit(NMDS_comm_fish_os_ht, comm_fish_os_ht, permutations = 999)

spp_scrs <- as_tibble(scores(os_ht_spp_fit, display = "vectors"), rownames = "Species") %>% 
  mutate(pval = os_ht_spp_fit$vectors$pvals) %>% 
  filter(pval <= 0.05 )

spp_scrs <- spp_scrs %>% 
  mutate(common_name = str_replace_all(Species, "_", " "))

spp_scrs <- spp_scrs %>% 
  mutate(common_name =  case_when(startsWith(common_name, "Ch") ~ "Blacksmith",
                                  startsWith(common_name, "Emb") ~ "Black Perch",
                                  startsWith(common_name, "Gir") ~ "Opaleye",
                                  startsWith(common_name, "Oxy") ~ "Senorita",
                                  common_name == "Paralabrax nebulifer" ~ "Barred Sand Bass",
                                  common_name == "Paralabrax clathratus" ~ "Kelp Bass",
                                  common_name == "Halichoeres semicinctus" ~ "Rock Wrasse",
                                  common_name == "Damalichthys vacca" ~ "Pile Perch",
                                  startsWith(common_name, "Seb") ~ "Olive Rockfish",
                                  startsWith(common_name, "Sem") ~ "CA Sheephead"))

plot_NMDS_os_ht_spp_vect <- plot_wide_fish_os_ht_hulls +
  geom_segment(data = spp_scrs, aes(x = 0, xend = -NMDS1*.275, y = 0, yend = -NMDS2*.27, shape = NULL),
               arrow = arrow(length = unit(.25, "cm")),
               color = "grey10", lwd = 0.3) +
  geom_text(data = spp_scrs, aes(x = -NMDS1*.27, y = -NMDS2*.27, label = common_name, shape = NULL), fontface = "bold", color = "black", size = 7) +
  theme(legend.position = c(0.01, .93),
        legend.justification = c(0, 1),   
        legend.background = element_rect(color = "black", fill = "white", size = 0.5),
        legend.margin = margin(5, 5, 5, 5), legend.direction = "horizontal")



# plot_NMDS_os_ht_spp_vect
reverse_y_plot_NMDS_os_ht_spp_vect = plot_NMDS_os_ht_spp_vect + scale_y_reverse()

reverse_y_plot_NMDS_os_ht_spp_vect
ggsave("figures/submodule_NMDS.png", reverse_y_plot_NMDS_os_ht_spp_vect,
       width = 16, height = 12, dpi = 600)
# ggsave("figures/submodule_assemblage_sqrt.png", plot_NMDS_os_ht_spp_vect,
#        width = 16, height = 12, dpi = 600)

spp_scrs <- spp_scrs %>% 
  arrange(pval)

spp_scrs

spp_scrs_ft <- flextable(spp_scrs,
                         col_keys = c("Species", "pval")) %>% 
  set_header_labels(Species = "NMDS1",
                    P = "pval") 

#spp_scrs_ft



#### Submodule Cluster Analysis ----

wide_fish_os_ht = wide_fish_os_ht %>% 
  mutate(cluster_group = (case_when(startsWith(as.character(cluster_group), "1") ~ "Group 3",
                                    startsWith(as.character(cluster_group), "2") ~ "Group 5",
                                    startsWith(as.character(cluster_group), "3") ~ "Group 1",
                                    startsWith(as.character(cluster_group), "4") ~ "Group 2",
                                    startsWith(as.character(cluster_group), "5") ~ "Group 4")))

# Define colors for each category in 'cluster_2'
group_colors <- c("Group 1" = "coral2",
                  "Group 2" = "springgreen4",
                  "Group 3" = "steelblue3",
                  "Group 4" = "purple4",
                  "Group 5" = "black")

# Define shapes for each level in 'Module'
module_shapes <- c("Module 1" = 15,
                   "Module 2" = 16,
                   "Module 3" = 17,
                   "Module 4" = 0,
                   "Module 5" = 1,
                   "Module 6" = 2)


comm_fish_os_ht <- wide_fish_os_ht %>%
  column_to_rownames(var = "dend_group") %>%
  select(Chromis_punctipinnis:Semicossyphus_pulcher)

dis.comm_fish_os_ht <- vegdist(comm_fish_os_ht)

# Create a cluster dendrogram
clust.comm_fish_os_ht <- hclust(dis.comm_fish_os_ht, "average")

# Convert to a dendrogram object
os_ht_dendro <- as.dendrogram(clust.comm_fish_os_ht)

# Extract the order of the labels in the dendrogram
dendro_labels <- labels(os_ht_dendro)

# Map the reordered labels to their corresponding colors based on 'cluster_2'
label_color_mapping <- sapply(dendro_labels, function(label) {
  group <- wide_fish_os_ht[wide_fish_os_ht$dend_group == label, "cluster_group"][[1]]
  group_colors[group]
})

# Map the reordered labels to their corresponding shapes based on 'Module'
label_shape_mapping <- sapply(dendro_labels, function(label) {
  module <- wide_fish_os_ht[wide_fish_os_ht$dend_group == label, "Habitat_Type"][[1]]
  module_shapes[module]
})

# Modify the plotting pipeline with the correct order of colors and shapes
os_ht_dendro <- os_ht_dendro %>%
  set("leaves_pch", label_shape_mapping) %>%  # node point type based on module
  set("leaves_cex", 5) %>%  # node point size
  set("leaves_col", label_color_mapping)  # node point color

# Plot the dendrogram again
# plot(os_ht_dendro)

# order.dendrogram(os_ht_dendro)

# new_order = c(2,  7, 10, 20,3, 11,  6, 23, 24,  16,  5, 12,  4, 18,  8, 15, 22 , 1 ,14 ,19, 13,  9, 17, 21)

# # Define the new order
new_order <- c(2, 8,10,18,15,22,16,5,12,7,6,23,24,20,3,11,4,13,14,19,1,9,17,21)
# 
# Rotate the dendrogram
dend_rotated <- rotate(os_ht_dendro, new_order)
# 
# # Plot the rotated dendrogram
plot(dend_rotated)
# order.dendrogram(dend_rotated)
# 
rotation2 = c(2, 7,  6, 23, 24,      9, 5, 12, 18,  4, 16,        1, 14, 19, 15, 22, 8 , 13, 17, 21,   3, 11, 10, 20         )

dend_rotated2 <- rotate(dend_rotated, rotation2)
plot(dend_rotated2)
order.dendrogram(dend_rotated2)

rotate3 = c(  8, 22, 15,  1, 14, 19,  4, 18, 12,  5, 16,13,  9, 21, 17)
dend_rotated3 <- rotate(dend_rotated2, rotate3)
plot(dend_rotated3)
order.dendrogram(dend_rotated3)

rotate4 = c(13,  9, 21, 17,   8, 22, 15, 19, 14,  1, 16,  5, 12,  4, 18, 6, 24, 23, 11,  3, 10,20,  7,  2)
dend_rotated4 <- rotate(dend_rotated3, rotate4)
plot(dend_rotated4)
order.dendrogram(dend_rotated4)

rotate5 = c(2, 11,  3, 10, 20,  6, 24, 23,  7,18,  4,  5, 12, 16, 19, 14,  1, 15, 22,  8, 13, 17, 21,  9)
dend_rotated5 <- rotate(dend_rotated4, rotate5)
plot(dend_rotated5)
order.dendrogram(dend_rotated5)


# Convert to ggplot object
gg_os_ht_dend <- as.ggdend(dend_rotated2)

# Plot using ggplot2
plot_gg_os_ht_dend <- ggplot(gg_os_ht_dend, horiz = TRUE, offset_labels = -0.01) +
  theme_classic() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "none", axis.line.y = element_blank(), axis.title.x = element_text(vjust = -1.2, hjust = 0.5)) +
  theme(text = element_text(size = 24)) +
  scale_y_reverse(breaks = c(0.3, 0.2, 0.1, 0), expand = c(0, .1, 0, .1)) + 
  labs(y = "Bray-Curtis Dissimilarity")

# Plot the ggplot object
print(plot_gg_os_ht_dend)

# Save the plot
ggsave("figures/submodule_dendrogram.png", plot_gg_os_ht_dend,
       width = 18, height = 7.5, dpi = 600)


# Convert to ggplot object
gg_os_ht_dend5 <- as.ggdend(dend_rotated5)

# Plot using ggplot2
plot_gg_os_ht_dend5 <- ggplot(gg_os_ht_dend5, horiz = TRUE, offset_labels = -0.01) +
  theme_classic() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "none", axis.line.y = element_blank(), axis.title.x = element_text(vjust = -1.2, hjust = 0.5)) +
  theme(text = element_text(size = 24)) +
  scale_y_reverse(breaks = c(0.3, 0.2, 0.1, 0), expand = c(0, .1, 0, .1)) + 
  labs(y = "Bray-Curtis Dissimilarity")

# Plot the ggplot object
print(plot_gg_os_ht_dend5)

# Save the plot
ggsave("figures/submodule_dendrogram_clusters.png", plot_gg_os_ht_dend5,
       width = 18, height = 7.5, dpi = 600)


# Determine the optimal number of clusters using the silhouette method
sil_width <- numeric(10)
for (i in 2:10) {
  clusters <- cutree(clust.comm_fish_os_ht, k = i)
  sil <- silhouette(clusters, dist(comm_fish_os_ht))
  sil_width[i] <- mean(sil[, 3])
}

# Plot the silhouette width to determine the optimal number of clusters
plot(1:10, sil_width, type = "b", xlab = "Number of Clusters", ylab = "Average Silhouette Width")
abline(v = which.max(sil_width), col = "red", lty = 2)
title(main = "Silhouette Method for Optimal Number of Clusters")


# define some clusters
mycl <- cutree(os_ht_dendro, h=0.23)
mycl
as_tibble(mycl)

cluster_df <- data.frame(dend_group = names(mycl), cluster_group = mycl)

# # Join the cluster information with the wide_fish_os_ht data frame
# wide_fish_os_ht <- wide_fish_os_ht %>%
#   left_join(cluster_df, by = "dend_group")

# os_ht_dendro$height


#### SPP dens across haibtat types
# sp dens by module
dens_sp_mod <- dat_fish_t %>% 
  group_by(Genus_spp, Module) %>%
  summarise(median_dens = median(dens_100m2), 
            mean_dens = mean(dens_100m2),
            sd_dens = sd(dens_100m2),
            max_dens = max(dens_100m2),
            min_dens = min(dens_100m2)) %>% 
  filter(Genus_spp %in% focal_spp)


###CHANGE TO MODULE
dens_sp_ht_ft <- flextable(dens_sp_ht,
                           col_keys = c("Genus_spp", "Habitat_Type", "median_dens",
                                        "mean_dens", "sd_dens", "max_dens", "min_dens")) %>% 
  add_header_row(colwidths = c(1, 1, 5), values = c("Species Name", "Habitat Type", "Focal fish density (No./100 m2)")) %>% 
  set_header_labels(Genus_spp = "Species Name",
                    Habitat_Type = "Habitat Type",
                    median_dens = "Median",
                    mean_dens = "Mean",
                    sd_dens = "Standard Deviation",
                    max_dens = "Max",
                    min_dens = "Min") %>% 
  colformat_double(digits = 1) %>% 
  theme_box() %>% 
  align(align = "center") %>% 
  align(part = "header", align = "center") %>% 
  compose(j = "Genus_spp",
          value = as_paragraph(as_i(Genus_spp))) %>% 
  merge_v(part = "header")

dens_sp_ht_ft

#### Submodule PERMANOVA  ----
dis.comm_fish_os_ht <- vegdist(comm_fish_os_ht)

adonis2(dis.comm_fish_os_ht ~ cluster_ordered, wide_fish_os_ht)

ado2.fish_os_ht_clus <-adonis2(dis.comm_fish_os_ht ~ Habitat_Type + os + cluster_ordered, by = "margin", wide_fish_os_ht)

ado2.fish_os_ht_clus

### UPDATE BELOW
ado2.fish_os_ht_clus_table <- as.data.frame(ado2.fish_os_ht_clus)
ado2.fish_os_ht_clus_table <- ado2.fish_os_ht_clus_table %>% 
  rownames_to_column() %>% 
  mutate(rowname = str_replace(rowname, "Habitat_Type", "Habitat Type"),
         rowname = str_replace(rowname, "os", "Orientation & Side"),
         rowname = str_replace(rowname, "cluster_ordered", "Cluster"))

set_flextable_defaults(font.family = "Arial", font.size = 9, digits = 2, padding = 0.1)

ado_os_ht_ft <- flextable(ado2.fish_os_ht_clus_table,
                          col_keys = c("rowname", "Df"
                                       , "SumOfSqs", "R2", "F", "Pr(>F)")) %>% 
  set_header_labels(rowname = "Transect Characteristics",
                    Df = "Df",
                    SumOfSqs = "Sum sq", 
                    R2 = "R2") %>% 
  colformat_double(digits = 2) %>% 
  colformat_double(j = "Df", digits = 0) %>% 
  colformat_double(j = "Pr(>F)", digits = 3)



ado_os_ht_ft
write_csv(ado2.fish_os_ht_clus_table, "tables/ado2.fish_os_ht_clus_table.csv")
# save_as_docx(ado_os_ht_ft, path = "ado_os_ht_ft.docx")


#### Community Composition Stacked Bar Plots ----
#start with the mean density per species per transect type 744 points (31 spp 24 transect types)
dat_fish_os_ht <- dat_fish_os_ht %>%
  mutate(os_ht = paste(os, Habitat_Type))


dat_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(Zone = case_when(startsWith(Habitat_Type, "High Ecotone") ~ "Ecotone",
                          startsWith(Habitat_Type, "Medium Ecotone") ~ "Ecotone",
                          startsWith(Habitat_Type, "Low Ecotone") ~ "Ecotone",
                          startsWith(Habitat_Type, "High Relief") ~ "Midline",
                          startsWith(Habitat_Type, "Medium Relief") ~ "Midline",
                          startsWith(Habitat_Type, "Low Relief") ~ "Midline"))

### ADD CLUSTER ANALYSIS - WHICH DEFINES THESE GROUPS
dat_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(current =  case_when(startsWith(os, "West") ~ "Up-current",
                              startsWith(os, "East") ~ "Down-current"))

dat_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(Orientation =  case_when(startsWith(os, "West Par") ~ "Parallel",
                                  startsWith(os, "West Per") ~ "Perpendicular",
                                  startsWith(os, "East Par") ~ "Parallel",
                                  startsWith(os, "East Per") ~ "Perpendicular"))


dat_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(os_lab = paste(Orientation, current))

dat_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(os_lab = str_replace(os_lab, "Parallel Up-current", "Parallel Offshore"),
         os_lab = str_replace(os_lab, "Parallel Down-current", "Parallel Inshore"),
         os_lab = str_replace(os_lab, "Perpendicular Up-current", "Perpendicular West"),
         os_lab = str_replace(os_lab, "Perpendicular Down-current", "Perpendicular East"))



dat_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(Habitat_Type = str_replace_all(Habitat_Type, "Relief", "Reef"))

dat_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(dend_group = paste(os_lab, Habitat_Type))

cluster_df <- data.frame(
  dend_group = c("Parallel Inshore High Ecotone", "Perpendicular East High Ecotone", "Parallel Offshore High Ecotone", 
                 "Perpendicular West High Ecotone", "Parallel Inshore High Reef", "Perpendicular East High Reef", 
                 "Parallel Offshore High Reef", "Perpendicular West High Reef", "Parallel Inshore Low Ecotone", 
                 "Perpendicular East Low Ecotone", "Parallel Offshore Low Ecotone", "Perpendicular West Low Ecotone", 
                 "Parallel Inshore Low Reef", "Perpendicular East Low Reef", "Parallel Offshore Low Reef", 
                 "Perpendicular West Low Reef", "Parallel Inshore Medium Ecotone", "Perpendicular East Medium Ecotone", 
                 "Parallel Offshore Medium Ecotone", "Perpendicular West Medium Ecotone", "Parallel Inshore Medium Reef", 
                 "Perpendicular East Medium Reef", "Parallel Offshore Medium Reef", "Perpendicular West Medium Reef"),
  cluster_group = c(1, 2, 3, 4, 4, 3, 3, 1, 5, 3, 3, 4, 5, 1, 1, 4, 5, 4, 1, 3, 5, 1, 3, 3)
)

dat_fish_os_ht <- dat_fish_os_ht %>%
  left_join(cluster_df, by = "dend_group")

# dat_fish_os_ht = dat_fish_os_ht %>% 
#   mutate(cluster_ordered = (case_when(startsWith(as.character(cluster_group), "1") ~ "4",
#                                       startsWith(as.character(cluster_group), "2") ~ "1",
#                                       startsWith(as.character(cluster_group), "3") ~ "2",
#                                       startsWith(as.character(cluster_group), "4") ~ "3",
#                                       startsWith(as.character(cluster_group), "5") ~ "5")))

clust_col <- c("black",
               "coral2","coral2","coral2","coral2","coral2","coral2","coral2","coral2",
               "springgreen4","springgreen4","springgreen4","springgreen4","springgreen4",
               "steelblue3","steelblue3","steelblue3","steelblue3","steelblue3","steelblue3",
               "purple4","purple4","purple4","purple4")



plot_dens_total_os_ht <- dat_fish_os_ht %>%
  mutate(dend_group = fct_relevel(dend_group,
                                  
                                  "Perpendicular East High Ecotone",
                                  
                                  "Parallel Offshore Low Ecotone",
                                  "Parallel Offshore High Ecotone",
                                  "Perpendicular East Low Ecotone",
                                  "Perpendicular West Medium Ecotone",
                                  "Perpendicular East High Reef",
                                  "Perpendicular West Medium Reef",
                                  "Parallel Offshore Medium Reef",
                                  "Parallel Offshore High Reef",
                                  
                                  "Perpendicular West High Ecotone",
                                  "Perpendicular East Medium Ecotone",
                                  "Perpendicular West Low Ecotone", 
                                  "Parallel Inshore High Reef",
                                  "Perpendicular West Low Reef",
                                  
                                  "Perpendicular West High Reef",
                                  "Perpendicular East Medium Reef",
                                  "Parallel Offshore Low Reef",
                                  "Parallel Inshore High Ecotone",
                                  "Perpendicular East Low Reef",
                                  "Parallel Offshore Medium Ecotone",
                                  
                                  
                                  
                                  "Parallel Inshore Low Reef",
                                  "Parallel Inshore Low Ecotone",
                                  "Parallel Inshore Medium Reef",
                                  "Parallel Inshore Medium Ecotone"
                                  )) %>%
  filter(Genus_spp %in% focal_spp) %>%
  group_by(dend_group, Genus_spp) %>%
  summarize(total_dens = sum(dens_100m2), .groups = 'drop') %>%
  ggplot(aes(x = dend_group, y = total_dens, fill = Genus_spp)) +
  geom_bar(stat = "identity", colour = "black") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Reef Design Features", y = expression(paste("Mean Density (No./100",m^{2},")"))) +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  guides(fill = guide_legend(title = "Focal Fish Species")) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_brewer(palette = "Set3", labels = c("Blacksmith",
                                                 "Señorita",
                                                 "Kelp Bass",
                                                 "California Sheephead",
                                                 "Black Perch",
                                                 "Rainbow Seaperch",
                                                 "Barred Sand Bass",
                                                 "Opaleye",
                                                 "Pile Perch",
                                                 "Rock Wrasse",
                                                 "Olive Rockfish")) +
  theme(
    legend.position = c(.857, .797),
    legend.direction = "vertical", 
    legend.background = element_rect(fill = "white", color = "black"), 
    legend.text = element_text(size = 16),
    axis.text.y = element_text(color = clust_col)  # Set y-axis (flipped x-axis) label colors
  )

plot_dens_total_os_ht

ggsave("figures/fish_density_total.png", plot_dens_total_os_ht,
       width = 12, height = 8, dpi = 600)

plot_prop_dens_total_os_ht <- dat_fish_os_ht %>%
  mutate(dend_group = fct_relevel(dend_group,
                                  
                                  "Perpendicular East High Ecotone",
                                  
                                  "Parallel Offshore Low Ecotone",
                                  "Parallel Offshore High Ecotone",
                                  "Perpendicular East Low Ecotone",
                                  "Perpendicular West Medium Ecotone",
                                  "Perpendicular East High Reef",
                                  "Perpendicular West Medium Reef",
                                  "Parallel Offshore Medium Reef",
                                  "Parallel Offshore High Reef",
                                  
                                  "Perpendicular West High Ecotone",
                                  "Perpendicular East Medium Ecotone",
                                  "Perpendicular West Low Ecotone", 
                                  "Parallel Inshore High Reef",
                                  "Perpendicular West Low Reef",
                                  
                                  "Perpendicular West High Reef",
                                  "Perpendicular East Medium Reef",
                                  "Parallel Offshore Low Reef",
                                  "Parallel Inshore High Ecotone",
                                  "Perpendicular East Low Reef",
                                  "Parallel Offshore Medium Ecotone",
                                  
                                  
                                  
                                  "Parallel Inshore Low Reef",
                                  "Parallel Inshore Low Ecotone",
                                  "Parallel Inshore Medium Reef",
                                  "Parallel Inshore Medium Ecotone"
  )) %>%
  filter(Genus_spp %in% focal_spp) %>%
  group_by(dend_group, Genus_spp) %>%
  summarize(total_dens = sum(dens_100m2), .groups = 'drop') %>%
  group_by(dend_group) %>%
  mutate(prop_dens = total_dens / sum(total_dens)) %>%
  ungroup() %>%
  ggplot(aes(x = dend_group, y = prop_dens, fill = Genus_spp)) +
  geom_bar(stat = "identity", colour = "black") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Reef Design Features", y = "Proportion of Total Density") +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  guides(fill = guide_legend(title = "Focal Fish Species")) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), labels = scales::percent) +
  scale_fill_brewer(palette = "Set3", labels = c("Blacksmith",
                                                 "Señorita",
                                                 "Kelp Bass",
                                                 "California Sheephead",
                                                 "Black Perch",
                                                 "Rainbow Seaperch",
                                                 "Barred Sand Bass",
                                                 "Opaleye",
                                                 "Pile Perch",
                                                 "Rock Wrasse",
                                                 "Olive Rockfish")) +
  theme(
    legend.position = c(.857, .797),
    legend.direction = "vertical", 
    legend.background = element_rect(fill = "white", color = "black"), 
    legend.text = element_text(size = 16),
    axis.text.y = element_text(color = clust_col)  # Set y-axis (flipped x-axis) label colors
  )

plot_prop_dens_total_os_ht

ggsave("figures/fish_density_proprtional_stack.png", plot_prop_dens_total_os_ht,
       width = 12, height = 8, dpi = 600)

#### Species Specific Habitat Use ----

dat_fish_spp <- dat_fish_t %>% 
  filter(Genus_spp %in% focal_spp)

dat_fish_spp = dat_fish_spp %>% 
  mutate(os_ht = paste(Module_Side, Orientation, Habitat_Type, sep = " "))

dat_fish_spp <- dat_fish_spp %>% 
  mutate(Zone = case_when(startsWith(Habitat_Type, "High Ecotone") ~ "Ecotone",
                          startsWith(Habitat_Type, "Medium Ecotone") ~ "Ecotone",
                          startsWith(Habitat_Type, "Low Ecotone") ~ "Ecotone",
                          startsWith(Habitat_Type, "High Relief") ~ "Midline",
                          startsWith(Habitat_Type, "Medium Relief") ~ "Midline",
                          startsWith(Habitat_Type, "Low Relief") ~ "Midline"))


dat_fish_spp <- dat_fish_spp %>% 
  mutate(Orientation =  case_when(startsWith(os, "West Par") ~ "Parallel",
                                  startsWith(os, "West Per") ~ "Perpendicular",
                                  startsWith(os, "East Par") ~ "Parallel",
                                  startsWith(os, "East Per") ~ "Perpendicular"))


dat_fish_spp <- dat_fish_spp %>% 
  mutate(os_lab = paste(Orientation, current))

dat_fish_spp <- dat_fish_spp %>% 
  mutate(os_lab = str_replace(os_lab, "Parallel Up-current", "Parallel Offshore"),
         os_lab = str_replace(os_lab, "Parallel Down-current", "Parallel Inshore"),
         os_lab = str_replace(os_lab, "Perpendicular Up-current", "Perpendicular West"),
         os_lab = str_replace(os_lab, "Perpendicular Down-current", "Perpendicular East"))



dat_fish_spp <- dat_fish_spp %>% 
  mutate(Habitat_Type = str_replace_all(Habitat_Type, "Relief", "Reef"))

dat_fish_spp <- dat_fish_spp %>% 
  mutate(dend_group = paste(os_lab, Habitat_Type))





# cluster_df <- data.frame(
#   dend_group = c("Parallel Inshore High Ecotone", "Perpendicular East High Ecotone", "Parallel Offshore High Ecotone", 
#                  "Perpendicular West High Ecotone", "Parallel Inshore High Reef", "Perpendicular East High Reef", 
#                  "Parallel Offshore High Reef", "Perpendicular West High Reef", "Parallel Inshore Low Ecotone", 
#                  "Perpendicular East Low Ecotone", "Parallel Offshore Low Ecotone", "Perpendicular West Low Ecotone", 
#                  "Parallel Inshore Low Reef", "Perpendicular East Low Reef", "Parallel Offshore Low Reef", 
#                  "Perpendicular West Low Reef", "Parallel Inshore Medium Ecotone", "Perpendicular East Medium Ecotone", 
#                  "Parallel Offshore Medium Ecotone", "Perpendicular West Medium Ecotone", "Parallel Inshore Medium Reef", 
#                  "Perpendicular East Medium Reef", "Parallel Offshore Medium Reef", "Perpendicular West Medium Reef"),
#   cluster_group = c(1, 2, 3, 4, 4, 3, 3, 1, 5, 3, 3, 4, 5, 1, 1, 4, 5, 4, 1, 3, 5, 1, 3, 3)
# )

# dat_fish_spp <- dat_fish_spp %>%
#   left_join(cluster_df, by = "dend_group")


# dat_fish_spp = dat_fish_spp %>%
#   mutate(cluster_ordered = (case_when(startsWith(as.character(cluster_group), "1") ~ "4",
#                                       startsWith(as.character(cluster_group), "2") ~ "1",
#                                       startsWith(as.character(cluster_group), "3") ~ "2",
#                                       startsWith(as.character(cluster_group), "4") ~ "3",
#                                       startsWith(as.character(cluster_group), "5") ~ "5")))

# dat_fish_spp <- dat_fish_spp %>% 
#   mutate(transect = paste(Module, Module_Side, Habitat_Type),
#          dens_100m2_sqrt = dens_100m2^0.5) %>% 
#   left_join(transect_var)
# 
# dat_fish_spp <- dat_fish_spp %>% 
#   mutate(construction_group = as.character(construction_group))
# 
# species = unique(dat_fish_spp$Genus_spp)
# species_plots = list()
# 
# # Filter for focal species and create os_ht column
# dat_fish_spp <- dat_fish_t %>%
#   filter(Genus_spp %in% focal_spp) %>%
#   mutate(os_ht = paste(Module_Side, Orientation, Habitat_Type, sep = " "))
# 
# # Add cluster information
# dat_fish_spp <- dat_fish_spp %>%
#   mutate(cluster = case_when(
#     startsWith(os_ht, "West Perpendicular High Relief") ~ "Group 5",
#     startsWith(os_ht, "West Perpendicular High Ecotone") ~ "Group 3",
#     startsWith(os_ht, "West Perpendicular Low Relief") ~ "Group 2",
#     startsWith(os_ht, "West Perpendicular Low Ecotone") ~ "Group 2",
#     startsWith(os_ht, "West Perpendicular Medium Relief") ~ "Group 1",
#     startsWith(os_ht, "West Perpendicular Medium Ecotone") ~ "Group 1",
#     startsWith(os_ht, "East Perpendicular High Relief") ~ "Group 1",
#     startsWith(os_ht, "East Perpendicular High Ecotone") ~ "Group 6",
#     startsWith(os_ht, "East Perpendicular Low Relief") ~ "Group 3",
#     startsWith(os_ht, "East Perpendicular Low Ecotone") ~ "Group 2",
#     startsWith(os_ht, "East Perpendicular Medium Relief") ~ "Group 2",
#     startsWith(os_ht, "East Perpendicular Medium Ecotone") ~ "Group 2",
#     startsWith(os_ht, "West Parallel High Relief") ~ "Group 1",
#     startsWith(os_ht, "West Parallel High Ecotone") ~ "Group 1",
#     startsWith(os_ht, "West Parallel Low Relief") ~ "Group 2",
#     startsWith(os_ht, "West Parallel Low Ecotone") ~ "Group 3",
#     startsWith(os_ht, "West Parallel Medium Relief") ~ "Group 1",
#     startsWith(os_ht, "West Parallel Medium Ecotone") ~ "Group 3",
#     startsWith(os_ht, "East Parallel High Relief") ~ "Group 2",
#     startsWith(os_ht, "East Parallel High Ecotone") ~ "Group 4",
#     startsWith(os_ht, "East Parallel Low Relief") ~ "Group 3",
#     startsWith(os_ht, "East Parallel Low Ecotone") ~ "Group 4",
#     startsWith(os_ht, "East Parallel Medium Relief") ~ "Group 4",
#     startsWith(os_ht, "East Parallel Medium Ecotone") ~ "Group 4"
#   ))

# # Create factor for Habitat_Type
# dat_fish_spp <- dat_fish_spp %>%
#   mutate(Habitat_Type = factor(Habitat_Type, levels = c(
#     "High Relief", "Medium Relief", "Low Relief", 
#     "High Ecotone", "Medium Ecotone", "Low Ecotone"
#   )))
# 
# # Create transect and dens_100m2_sqrt columns
# dat_fish_spp <- dat_fish_spp %>%
#   mutate(transect = paste(Module, Module_Side, Habitat_Type),
#          dens_100m2_sqrt = dens_100m2^0.5) %>%
#   left_join(transect_var)
# 
# # Ensure construction_group is a character
# dat_fish_spp <- dat_fish_spp %>%
#   mutate(construction_group = as.character(construction_group))

# Get unique species
species <- unique(dat_fish_spp$Genus_spp)
species_plots <- list()

group_color_lab <- c("Group 1" = "black", "Group 2" = "coral2", 
                     "Group 3" = "springgreen4", "Group 4" = "steelblue3", 
                     "Group 5" = "purple4")

for(species_ in species){
  species_plots[[species_]] <- dat_fish_spp %>%
    filter(Genus_spp == species_) %>%
    mutate(dend_group = fct_relevel(dend_group,
                                    
                                    "Perpendicular East High Ecotone",
                                    
                                    "Parallel Offshore Low Ecotone",
                                    "Parallel Offshore High Ecotone",
                                    "Perpendicular East Low Ecotone",
                                    "Perpendicular West Medium Ecotone",
                                    "Perpendicular East High Reef",
                                    "Perpendicular West Medium Reef",
                                    "Parallel Offshore Medium Reef",
                                    "Parallel Offshore High Reef",
                                    
                                    "Perpendicular West High Ecotone",
                                    "Perpendicular East Medium Ecotone",
                                    "Perpendicular West Low Ecotone", 
                                    "Parallel Inshore High Reef",
                                    "Perpendicular West Low Reef",
                                    
                                    "Perpendicular West High Reef",
                                    "Perpendicular East Medium Reef",
                                    "Parallel Offshore Low Reef",
                                    "Parallel Inshore High Ecotone",
                                    "Perpendicular East Low Reef",
                                    "Parallel Offshore Medium Ecotone",
                                    
                                    
                                    
                                    "Parallel Inshore Low Reef",
                                    "Parallel Inshore Low Ecotone",
                                    "Parallel Inshore Medium Reef",
                                    "Parallel Inshore Medium Ecotone"
    )) %>%
    mutate(Habitat_Type = factor(Habitat_Type, levels = c("High Reef",
                                                          "Medium Reef",
                                                          "Low Reef",
                                                          "High Ecotone",
                                                          "Medium Ecotone",
                                                          "Low Ecotone"))) %>% 
    ggplot(aes(x = dend_group, y = dens_100m2, color = cluster, shape = Habitat_Type)) +
    scale_shape_manual(values = c(15, 16, 17, 0, 1, 2)) +
    geom_crossbar(stat = "summary", fun.data = mean_cl_boot,
                  fill = "gray", alpha = 0.5, width = 0.4) +
    geom_point(position = position_jitter()) +
    theme_classic() +
    ggtitle(species_) +
    scale_color_manual(values = group_color_lab) +
    guides(shape = guide_legend(title = "Habitat Type", nrow = 3, 
                                ncol = 2,), color = guide_legend(title = "Cluster Group")) +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 9,color = clust_col),legend.position = c(0.65, 0.85), legend.direction = "horizontal", legend.box = "vertical") +
    labs(x = "Reef Design Features", y = expression(paste("Fish Density (No./100", m^2, ")")))
  
  print(species_plots[[species_]])
  
  ggsave(paste0("figures/species_plots/spp_density_plot_", species_, ".png"), species_plots[[species_]],
         width = 9, height = 8, dpi = 600)
}

#### Size Class Analysis ----
dat_fish_l <-dat_fish_l %>%
  mutate(Habitat_Type = str_replace_all(Habitat_Type, "Ecotone_High", "High Ecotone"),
         Habitat_Type = str_replace_all(Habitat_Type, "Ecotone_Medium", "Medium Ecotone"),
         Habitat_Type = str_replace_all(Habitat_Type, "Ecotone_Low", "Low Ecotone"),
         Habitat_Type = str_replace_all(Habitat_Type, "Mid_High", "High Relief"),
         Habitat_Type = str_replace_all(Habitat_Type, "Mid_Medium", "Medium Relief"),
         Habitat_Type = str_replace_all(Habitat_Type, "Mid_Low", "Low Relief"))

dat_fish_l <- dat_fish_l %>%
  mutate(current =  case_when(startsWith(os, "West") ~ "Up-current",
                              startsWith(os, "East") ~ "Down-current"))

dat_fish_l <- dat_fish_l %>%
  mutate(os_ht = paste(Module_Side, Orientation, Habitat_Type))


dat_fish_l <- dat_fish_l %>%
  mutate(cluster = (case_when(startsWith(os_ht, "West Perpendicular High Relief") ~ "Group 5",
                              startsWith(os_ht, "West Perpendicular High Ecotone") ~ "Group 3",
                              startsWith(os_ht, "West Perpendicular Low Relief") ~ "Group 2",
                              startsWith(os_ht, "West Perpendicular Low Ecotone") ~ "Group 2",
                              startsWith(os_ht, "West Perpendicular Medium Relief") ~ "Group 1",
                              startsWith(os_ht, "West Perpendicular Medium Ecotone") ~ "Group 1",
                              startsWith(os_ht, "East Perpendicular High Relief") ~ "Group 1",
                              startsWith(os_ht, "East Perpendicular High Ecotone") ~ "Group 6",
                              startsWith(os_ht, "East Perpendicular Low Relief") ~ "Group 3",
                              startsWith(os_ht, "East Perpendicular Low Ecotone") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Medium Relief") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Medium Ecotone") ~ "Group 2",
                              startsWith(os_ht, "West Parallel High Relief") ~ "Group 1",
                              startsWith(os_ht, "West Parallel High Ecotone") ~ "Group 1",
                              startsWith(os_ht, "West Parallel Low Relief") ~ "Group 2",
                              startsWith(os_ht, "West Parallel Low Ecotone") ~ "Group 3",
                              startsWith(os_ht, "West Parallel Medium Relief") ~ "Group 1",
                              startsWith(os_ht, "West Parallel Medium Ecotone") ~ "Group 3",
                              startsWith(os_ht, "East Parallel High Relief") ~ "Group 2",
                              startsWith(os_ht, "East Parallel High Ecotone") ~ "Group 4",
                              startsWith(os_ht, "East Parallel Low Relief") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Low Ecotone") ~ "Group 4",
                              startsWith(os_ht, "East Parallel Medium Relief") ~ "Group 4",
                              startsWith(os_ht, "East Parallel Medium Ecotone") ~ "Group 4")))

dat_fish_l <-dat_fish_l %>%
  filter(Genus_spp %in% focal_spp)



# chromis_size <- dat_fish_l %>%
#   filter(Genus_spp == "Chromis punctipinnis",
#          Length < 260) %>%
#   group_by(cluster) %>%
#   ggplot(aes(x = Length, fill = cluster)) +
#   geom_histogram(position = "identity", alpha = 0.3, bins = 30) +
#   theme_classic() +
#   ggtitle("Chomis punctipinnis") +
#   scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2"))


# chromis_size

# #
#  for(species_ in species){
#    species_plots[[species_]] <- dat_fish_l %>%
#      filter(Genus_spp == species_) %>%
#    group_by(cluster) %>%
#    ggplot(aes(x = Length, fill = cluster)) +
#    geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
#    theme_classic() +
#      ggtitle(species_) +
#    scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
#    guides(color = guide_legend(title = "Cluster Group")) +
#    labs(x = "Total Length (mm)", y = "Observations") +
#      facet_grid(.~cluster, rows = vars(cluster), cols = vars(Length))
#       scale_x_discrete(expand = c(0,0)) +
#      scale_y_continuous(expand = c(0,0))
#  print(species_plots[[species_]])}
# 
# #Change y axis label (proportional to transect numbers)
# 
# chromis_size_hist <- dat_fish_l %>%
#   # mutate(Habitat_Type = fct_relevel(Habitat_Type, levels = c(
#   #   "High Relief",
#   #   "Medium Relief",
#   #   "Low Relief",
#   #   "High Ecotone",
#   #   "Medium Ecotone",
#   #   "Low Ecotone"))) %>% 
#   filter(Genus_spp == "Chromis punctipinnis") %>%
#   ggplot(aes(x = Length, fill = cluster)) + 
#   geom_histogram(binwidth = 5, position = "stack", alpha = 0.7) +
#   scale_fill_manual(values = c("coral2","springgreen4", "steelblue3","purple4", "black","khaki4")) +
#   theme_classic() +
#   guides(fill = guide_legend(title = "Cluster Group")) +
#   labs(x = "Total Length (mm)", y = "Chromis Abundance") +
#   theme(legend.position = "top", legend.direction = "horizontal") +
#   facet_grid(rows = vars(Habitat_Type))
# # , scales = "free_y")
# 
# 
# chromis_size_hist
# 
# 
# # ggsave("figures/chromis_size_hist.png", chromis_size_hist,
# #        width = 9, height = 10, dpi = 600)
# 
# kelp_bass_size_hist <- dat_fish_l %>%
#   # mutate(Habitat_Type = fct_relevel(Habitat_Type, levels = c(
#   #   "High Relief",
#   #   "Medium Relief",
#   #   "Low Relief",
#   #   "High Ecotone",
#   #   "Medium Ecotone",
#   #   "Low Ecotone"))) %>% 
#   filter(Genus_spp == "Paralabrax clathratus") %>%
#   ggplot(aes(x = Length, fill = cluster)) +
#   geom_histogram(binwidth = 25, position = "stack", alpha = 0.7) +
#   scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
#   theme_classic() +
#   guides(fill = guide_legend(title = "Cluster Group")) +
#   labs(x = "Total Length (mm)", y = "Kelp Bass Abundance") +
#   theme(legend.position = "top", legend.direction = "horizontal") +
#   facet_grid(rows = vars(Habitat_Type))
# # , scales = "free_y")
# 
# kelp_bass_size_hist
# 
# # ggsave("figures/kelp_bass_size_hist.png", kelp_bass_size_hist,
# #        width = 9, height = 10, dpi = 600)
# 
# 
# sheephead_size_hist <- dat_fish_l %>%
#   # mutate(Habitat_Type = fct_relevel(Habitat_Type, levels = c(
#   #   "High Relief",
#   #   "Medium Relief",
#   #   "Low Relief",
#   #   "High Ecotone",
#   #   "Medium Ecotone",
#   #   "Low Ecotone"))) %>% 
#   filter(Genus_spp == "Semicossyphus pulcher") %>%
#   ggplot(aes(x = Length, fill = cluster)) +
#   geom_histogram(position = "stack", alpha = 0.7, binwidth = 20) +
#   scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
#   theme_classic() +
#   guides(fill = guide_legend(title = "Cluster Group")) +
#   labs(x = "Total Length (mm)", y = "Sheephead Abundance") +
#   theme(legend.position = "top", legend.direction = "horizontal") +
#   facet_grid(rows = vars(Habitat_Type))
# # , scales = "free_y")
# 
# 
# sheephead_size_hist


# ggsave("figures/sheephead_size_hist.png", sheephead_size_hist,
#        width = 9, height = 10, dpi = 600)
### making kernal density curves


#### Reef Use by Size Class ----
# Define the colors for the habitat types
relief_colors <- c("High Relief" = "black", "Medium Relief" = "gray", "Low Relief" = "white")

# Get unique species from the dataset
unique_species <- unique(dat_fish_l$Genus_spp)

# Initialize an empty list to store the plots
species_density_plots <- list()

# Loop over each species to create density plots
for(species_ in unique_species){
  species_density_plots[[species_]] <- dat_fish_l %>%
    mutate(Habitat_Type = factor(Habitat_Type, levels = c("High Relief",
                                                          "Medium Relief",
                                                          "Low Relief",
                                                          "High Ecotone",
                                                          "Medium Ecotone",
                                                          "Low Ecotone"))) %>% 
    # Filter data for the current species and desired habitat types
    filter(Genus_spp == species_ & Habitat_Type %in% c("High Relief", "Medium Relief", "Low Relief")) %>%
    ggplot(aes(x = Length, fill = Habitat_Type)) +
    geom_density(position = "fill", alpha = 0.8) +
    scale_fill_manual(values = relief_colors) +
    theme_classic() +
    guides(fill = guide_legend(title = "Habitat Type")) +
    labs(x = bquote(italic(.(species_)) ~ " Total Length (mm)"), y = "Proportional Density") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20), 
          legend.position = "none", legend.direction = "vertical", legend.text = element_text(size = 16))
  
  # Print the plot for each species
  print(species_density_plots[[species_]])
  
  # Save the plot to a file
  ggsave(paste0("figures/size_density_plots/density_plot_", species_, ".png"), species_density_plots[[species_]],
         width = 9, height = 8, dpi = 600)
}

