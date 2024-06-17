#CREATION DATE 3 MARCH 2022

#AUTHORs: Brenda Calderon, becalderon@cpp.edu ; Jeremy Claisse, jtclaisse@cpp.edu

#PURPOSE
#Analyze Garibaldi gut contents
#Analyze correlation between diet and life history

# Loading Packages --------------------------------------------------------
library(RColorBrewer)
library(flextable)
library(lubridate)
library(ggrepel)
library(ggpubr)
library(gridExtra)
library(vegan)
library(ape)
library(tidyverse)
library(ggdendro)
library(dendextend)
library(sigclust)
library(broom)
library(emmeans)
library(lme4)
library(cowplot)
library(ggpattern)

# Reading Data Files ------------------------------------------------------
#read (Brenda's) island + some mainland gut contents data
Island_Gut_Contents <- read_csv("Island_Gut_Contents.csv")
Island_Gut_Contents <- Island_Gut_Contents 
# Island_Gut_Contents_count <-Island_Gut_Contents |> 
#   distinct(CN)

#read (Jacob's) mainland gut contents data
Mainland_Gut_Contents <- read_csv("Gut_contents_Nov_15_2020.csv")
Mainland_Gut_Contents <- Mainland_Gut_Contents 
# Mainland_Gut_Contents_count <- Mainland_Gut_Contents |>
#   distinct(CN)

#combine gut contents data
Gut_Contents <- bind_rows(Island_Gut_Contents, Mainland_Gut_Contents)

#remove CN's that do not have gut contents data
Gut_Contents <- Gut_Contents |> 
  filter(Dry_Weight_g != "NA")

#mutate to change gut content names. use mutate(data=recode(data, old=new) function 
#to change factor level names (like "Algae_Red","Red Algae")
#also recoded multiple levels into Crustacean
Gut_Contents <- Gut_Contents |> 
  mutate(Gut_Content=recode(Gut_Content, 
                            'Algae_Red' ="Fleshy Red Algae", 
                            'Algae_Brown'="Brown Algae", 
                            'Algae_Green'="Green Algae", 
                            'Coralline' = "Coralline Red Algae",
                            'Brittle_Star'="Brittle Stars", 
                            'Egg_Garibaldi'="Garibaldi Eggs", 
                            'Bryozoan_Encrusted'="Encrusting Bryozoans",
                            'Bryozoan'="Branching Bryozoans",
                            'Isopod'="Crustaceans", 
                            'Copepod'="Crustaceans",
                            'Polychaeta' = "Polychaete Worms",
                            'Anemone' = "Anemones",
                            'Mollusk' = "Mollusks",
                            'Crustacean' = "Crustaceans",
                            'Gorgonian' = "Gorgonians",
                            'Tunicate' = "Tunicates",
                            'Hydroids' = "Hydroids",
                            'Sponge' = "Sponge"))

#summarize to combine the crustacean categories, now equals 16 diet categories
Gut_Contents <- Gut_Contents |> 
  group_by(CN, Gut_Content) |> 
  summarize(Dry_Weight_g = sum(Dry_Weight_g), 
            Wet_Weight_g = sum(Wet_Weight_g))

Gut_Contents |> 
  distinct(CN) 
# row_check<- Gut_Contents |> 
#   group_by(CN) |> 
#   summarise(row_count = n())
# ## 937 obs = 585 island + 352 mainland
#Note: some of the CNs in the island file are from mainland (extra LA)

##it matches with row_check (937)
# Gut_Contents |> 
# distinct(CN)
# 
##16 distinct gut contents - originally 18 but iso and cope are now crust = 16 distinct categories
# Gut_Contents |> 
#   distinct(Gut_Content)

#creating gut content data set with major Animal/Algae categories
Major_Gut_Contents <- Gut_Contents |> 
  mutate(Gut_Content=recode(Gut_Content, 
                            'Anemones' ="Animal",
                            'Encrusting Bryozoans' ="Animal", 
                            'Brittle Stars'="Animal", 
                            'Brown Algae'="Algae", 
                            'Branching Bryozoans'="Animal", 
                            'Coralline Red Algae'="Algae", 
                            'Crustaceans'="Animal", 
                            'Garibaldi Eggs'="Animal", 
                            'Gorgonians'="Animal",
                            'Green Algae'="Algae", 
                            'Hydroids'="Animal",
                            'Mollusks' = "Animal",
                            'Polychaete Worms' = "Animal",
                            'Fleshy Red Algae' = "Algae",
                            'Sponge' = "Animal",
                            'Tunicates' = "Animal"))

#summarize to combine the Animal/Algae dry & wet weights
Major_Gut_Contents <- Major_Gut_Contents |> 
  group_by(CN, Gut_Content) |> 
  summarize(Dry_Weight_g = sum(Dry_Weight_g), 
            Wet_Weight_g = sum(Wet_Weight_g))

#read island stomach metrics data (remove notes column to bind)
Island_Stomach_Metrics <- read_csv("Island_Stomach_Metrics.csv")
Island_Stomach_Metrics <- Island_Stomach_Metrics
#show unique values in notes
Island_Stomach_Metrics |> 
  distinct(Notes) |> 
  print(n = Inf)

#create vector of descriptions in notes to be removed
to_remove <- c("empty stomach",
               "disintegrated sample",
               "contents just mush",
               "broken stomach 1/2 gone",
               "missing stomach",
               "too tiny",
               "full gut length without stomach",
               "all mush",
               "1/2 stomach missing",
               "missing contents",
               "stomach contents = goo",
               "full gut-length without stomach",
               "stomach half gone")

#to reference which were removed 
Island_Stomach_Metrics_removed <- Island_Stomach_Metrics |> 
  filter(Notes %in% to_remove)

#filter out any rows that Notes matches to_remove
Island_Stomach_Metrics <- Island_Stomach_Metrics |> 
  filter(!Notes %in% to_remove)

#read mainland stomach metrics data
Mainland_Stomach_Metrics <- read_csv("Stomach_Metrics_Nov_14_2020.csv")
Mainland_Stomach_Metrics <- Mainland_Stomach_Metrics

#combine stomach metrics data
Stomach_Metrics <- bind_rows(Island_Stomach_Metrics, Mainland_Stomach_Metrics)
Stomach_Metrics |> 
  distinct(CN) |> 
  count()

#read in life history data
Life_History <- read_csv("HYPRUB_Life_History_26_Mar_2021.csv") |> ## NOTE: manually changed Day for SCAI fish from 20-24 to 20 (causing read in as character)
  mutate(CN = factor(CN))
glimpse(Life_History)

# TO-DO replace missing wt_g body weight w/ formula from Chelsea's paper

#Sex column in Life_History already includes macro ID'd sex of gonad Chelsea did during dissection + histology determined sex that Jacob did at CPP
Life_History |> 
  count(Sex)
# For our purposes NA = U
Life_History <- Life_History |> 
  mutate(Sex = replace_na(Sex, "U"))

Life_History |> 
  count(Sex)

#read in sites data (Chelsea's version w/ North & South PV separate)
Sites <- read_csv("sites.csv")
glimpse(Sites)

#bind life history and sites data joining by "Site" 
Life_History <- Life_History |> 
  left_join(Sites, by = c("Site")) |> 
  mutate(IM_RT = as.factor(paste(Island_Mainland, ReefType)))
glimpse(Life_History)

#joining gut contents and stomach data
##first filter out CN's w/ notes indicating removal above
Dat_Dt_St <- Gut_Contents |>
  filter(!CN %in% Island_Stomach_Metrics_removed$CN) |>
  left_join(Stomach_Metrics, by = "CN") |> 
  mutate(CN = factor(CN))

glimpse(Dat_Dt_St)

#adding life history
Dat <- Dat_Dt_St |> 
  left_join(Life_History, by = "CN")
glimpse(Dat)

#joining major gut contents with stomach data
#filtering out CNs w/notes indicating removal above
Dat_Dt_St_Major <- Major_Gut_Contents |>
  filter(!CN %in% Island_Stomach_Metrics_removed$CN) |> 
  left_join(Stomach_Metrics, by = "CN") |> 
  mutate(CN = factor(CN))

glimpse(Dat_Dt_St_Major)

#adding life history
Dat_Major <- Dat_Dt_St_Major |> 
  left_join(Life_History, by = "CN")
glimpse(Dat_Major)

#read in predicted parameters
LH_parameters <- read_csv("Garibaldi_Predicted_Parameters_2022-11-15.csv")

#read in predicted parameters for all ages
Predicted_Parameters <- read_csv("Garibaldi_Predicted_Parameters_All_Ages.csv")

##these CN's did not have wt_g in raw data
dat_NA_check <- Dat |> 
  filter(CN %in% c(142, 219, 227, 296))
dat_NA_check$CN
#only 219 and 227 are in the data
#TO DO can estimate 219's wt_g using length-weight relationship formula in Chelsea's paper

#removing the N. Baja California sample
Dat <- Dat |> 
  filter(!CN == "599") |> 
  filter(Wt_g != "NA") # filter out fish w/ no Wt_g

#check for NAs
dat_NA_check_dwg <- Dat |> 
  filter(is.na(Dry_Weight_g))

#filtering out fish of unknown sex
Dat <- Dat |> 
  filter(Sex != "U")

#filtering out N. Baja and fish without weights
Dat_Major <- Dat_Major |> 
  filter(!CN == "599") |> 
  filter(Wt_g != "NA") # filter out fish w/ no Wt_g

dat_NA_check_dwg <- Dat_Major |> 
  filter(is.na(Dry_Weight_g))

#filtering out fish of unknown sex
Dat_Major <- Dat_Major |> 
  filter(Sex != "U")

#merge dmy columns into one "date" column
Dat <- Dat |> 
  unite("Date", Day:Year, sep= "-", remove = F) |>
  mutate(Date = dmy(Date))
glimpse(Dat)

Dat_Major <- Dat_Major |> 
  unite("Date", Day:Year, sep= "-", remove = F) |>
  mutate(Date = dmy(Date))
glimpse(Dat_Major)

# Calculations ------------------------------------------------------------
#add columns for sum of weights, columns for proportions, conversion columns, and Fulton's K column
#Diet contents divided by Wt_g of the fish 
Dat <- Dat |>
  group_by(CN) |> 
  mutate(sum_Dry_Weight_g = sum(Dry_Weight_g), #total dry weight for each fish's gut contents 
         sum_Wet_Weight_g = sum(Wet_Weight_g), #total wet weight for each fish's gut contents 
         prop_Dry_Weight_g = Dry_Weight_g/sum(Dry_Weight_g), #Proportion of gut_contents for each fish dry weight
         prop_Wet_Weight_g = Wet_Weight_g/sum(Wet_Weight_g), #Proportion of gut_contents for each fish wet weight
         Dry_Weight_mg = ((1000)*(Dry_Weight_g)), #convert gut_content dry weight from mg to g
         Wet_Weight_mg = ((1000)*(Wet_Weight_g)), #convert gut_content wet weight from mg to g
         prop_DW_body_wt = (Dry_Weight_g/Wt_g), 
         prop_WW_body_wt = (Wet_Weight_g/Wt_g),
         prop_DW_mg_body_wt_g = (Dry_Weight_mg/Wt_g), #this is DBI
         prop_WW_mg_body_wt_g = (Wet_Weight_mg/Wt_g)) |>
  # Fulton's K: 100W⁄ L^3, where W is the weight of the fish, and L is the length (usually total length). A scaling factor is usually applied to bring the factor close to 1
  mutate(Fultons_K = (Wt_g * 10000)/(TL_mm^3)) |> 
  ungroup()

#Diet contents divided by Wt_g of the fish 
Dat_Major <- Dat_Major |>
  group_by(CN) |> 
  mutate(sum_Dry_Weight_g = sum(Dry_Weight_g), #total dry weight for each fish's gut contents 
         sum_Wet_Weight_g = sum(Wet_Weight_g), #total wet weight for each fish's gut contents 
         prop_Dry_Weight_g = Dry_Weight_g/sum(Dry_Weight_g), #Proportion of gut_contents for each fish dry weight
         prop_Wet_Weight_g = Wet_Weight_g/sum(Wet_Weight_g), #Proportion of gut_contents for each fish wet weight
         Dry_Weight_mg = ((1000)*(Dry_Weight_g)), #convert gut_content dry weight from mg to g
         Wet_Weight_mg = ((1000)*(Wet_Weight_g)), #convert gut_content wet weight from mg to g
         prop_DW_body_wt = (Dry_Weight_g/Wt_g), 
         prop_WW_body_wt = (Wet_Weight_g/Wt_g),
         prop_DW_mg_body_wt_g = (Dry_Weight_mg/Wt_g), ## this is DBI
         prop_WW_mg_body_wt_g = (Wet_Weight_mg/Wt_g)) |>
  # Fulton's K: 100W⁄ L^3, where W is the weight of the fish, and L is the length (usually total length). A scaling factor is usually applied to bring the factor close to 1
  mutate(Fultons_K = (Wt_g * 10000)/(TL_mm^3)) |> 
  ungroup()

#Find sum for dry weight mg by gut content, arranged in descending order
Sum_DW_Order_mg <- Dat |>
  group_by(Gut_Content) |> 
  summarize(Sum_GC = sum(Dry_Weight_mg)) |>
  arrange(desc(Sum_GC))

#define order for plotting/display of Gut_Content in Dat & Dat_AISCRI tables
Dat <- Dat |> 
  mutate(Gut_Content = factor(Gut_Content, levels = Sum_DW_Order_mg$Gut_Content))
  
glimpse(Dat)

# Dat_AISCRI object creation ----------------------------------------------
#creating version of Dat data set that combines AI + SCRI
Dat_AISCRI <- Dat |> 
  mutate(Region = recode(Region,
                         "Anacapa Island" = "Anacapa & Santa Cruz Islands",
                         "Santa Cruz Island" = "Anacapa & Santa Cruz Islands")) 

Dat_Major_AISCRI <- Dat_Major |> 
  mutate(Region = recode(Region,
                         "Anacapa Island" = "Anacapa & Santa Cruz Islands",
                         "Santa Cruz Island" = "Anacapa & Santa Cruz Islands")) 

#Average + sqrt across CNs by Reg_code, Reef Type for each Gut_Content
Dat_Major_AISCRI_R_RT_GC  <- Dat_Major_AISCRI |>
  group_by(Region, ReefType, Sex, Gut_Content, IM_RT) |> 
  filter(Sex !="U") |> 
  summarize(mean_prop_DW_mg_body_wt_g = (mean(prop_DW_mg_body_wt_g)),
            RegionSST = mean(RegionSST)) |>
  mutate(sqrt_mean_prop_DW_mg_body_wt_g = mean_prop_DW_mg_body_wt_g^0.5) #sqrt transformed  

#create community data using pivot_wider (Region, Reef Type)
Wide_Dat_Major_AISCRI_R_RT_GC <- Dat_Major_AISCRI_R_RT_GC |>
  mutate(Variables_Label = paste(Region, ReefType, Sex)) |>
  pivot_wider(id_cols = c(Variables_Label, IM_RT, RegionSST),
              names_from = Gut_Content , 
              values_from = sqrt_mean_prop_DW_mg_body_wt_g)

## make "community data" table format to go into vegan package functions
Comm_Wide_Dat_Major_AISCRI_R_RT_GC <- Wide_Dat_Major_AISCRI_R_RT_GC |> 
  column_to_rownames(var = "Variables_Label") |>
  select(-IM_RT, -RegionSST) #remove the columns to get into comm format 

# Condition Factor --------------------------------------------------------
#examine condition factor
## FIRST remove repeated rows for each CN (gut category), distinct(CN)
FK_plots <- Dat |> 
  filter(Sex != "U") |> 
  group_by(Region, ReefType, Sex, Fultons_K, TL_mm, Wt_g, IM_RT) |>
  distinct(CN) |> 
  ungroup()

FK_plots <- FK_plots |> 
  mutate(Variables_Label = paste(Region, ReefType)) |> 
  mutate(Location_Code = recode(Variables_Label,
                       'Anacapa Island Natural' = "AI",
                       'North Palos Verdes Artificial' = "NPV-A",
                       'North Palos Verdes Natural' = "NPV-N",
                       'North San Diego Artificial' = "NSD-A",
                       'North San Diego Natural' = "NSD-N",
                       'Orange County Artificial' = "OC-A",
                       'Orange County Natural' = "OC-N",
                       'San Clemente Island Natural' = "SCLI",
                       'Santa Barbara Island Natural' = "SBI",
                       'Santa Catalina Island Natural' = "SCI",
                       'Santa Cruz Island Natural' = "SCRI",
                       'South Palos Verdes Artificial' = "SPV-A",
                       'South Palos Verdes Natural' = "SPV-N",
                       'South San Diego Artificial' = "SSD-A",
                       'South San Diego Natural' = "SSD-N"))
       
cond_cols <- c("Artificial" = "blue4", 
               "Natural" = "forestgreen")
IM_RT_cols <- c("Island Natural" = "lightseagreen", 
                "Mainland Natural" = "forestgreen", 
                "Mainland Artificial" = "blue4")

## LMM Condition Factor ----------------------------------------------------

#### renaming locations into groups
FK_plots <- FK_plots |> 
  mutate(Variables_Label = paste(Region, ReefType)) |> 
  mutate(Group = recode(Variables_Label,
                        'Anacapa Island Natural' = "Group 1",
                        'Santa Cruz Island Natural' = "Group 1",
                        'North Palos Verdes Artificial' = "Group 2",
                        'South Palos Verdes Artificial' = "Group 2",
                        'Orange County Artificial' = "Group 2",
                        'North San Diego Artificial' = "Group 2",
                        'South San Diego Artificial' = "Group 2",
                        'Santa Barbara Island Natural' = "SBI",
                        'Santa Catalina Island Natural' = "Group 3",
                        'San Clemente Island Natural' = "Group 3",
                        'North Palos Verdes Natural' = "Group 4",
                        'South Palos Verdes Natural' = "Group 4",
                        'Orange County Natural' = "Group 4",
                        'South San Diego Natural' = "Group 4",
                        'North San Diego Natural' = "NSD"))

#creating vector of order for groups
Group_order <- c("Group 1",
                 "Group 2",
                 "SBI",
                 "Group 3",
                 "Group 4",
                 "NSD")

FK_plots <- FK_plots |> 
  mutate(Group = factor(Group, levels = Group_order))

FK_plots |> 
  group_by(Group) |> 
  summarise(FK_sd = sd(Fultons_K, na.rm = T))
# equal variance assumption ok, largest sd only about 2x smallest

# Fit LMM Linear Mixed model
# Fixed factors = Group, Sex
# Random factor = location
LMM_FK_G_S_rLC <- lmer(Fultons_K ~ Group + Sex +
                         (1 | Location_Code),
                       data = FK_plots)
LMM_FK_G_S_rLC
#AICcmodavg::AICc(LMM_FK_G_S_rLC)

LMM_FK_G_S_rLC_coef <- cbind(coef(summary(LMM_FK_G_S_rLC)),
                             confint(LMM_FK_G_S_rLC)[-c(1:2),]) #
LMM_FK_G_S_rLC_coef

# Marginal means and 95% CIs for each group (averaged across sexes)
LMM_FK_G_S_rLC_emm <- emmeans(LMM_FK_G_S_rLC, specs = c("Group"))
LMM_FK_G_S_rLC_emm

#add 2nd appendix table for w/ means and 95% CIs for each sex within each cluster group
sex_results_Table<- emmeans(LMM_FK_G_S_rLC, specs = c("Sex", "Group"))
sex_results_Table

##broom package extends function tidy() to clean up model outputs
sex_results_Table.tibble <- tidy(sex_results_Table)

#use flex table to output emmeans results table in word
FT_sex_results_Table <- flextable(sex_results_Table.tibble)
FT_sex_results_Table <- set_header_labels(FT_sex_results_Table,
                                   estimate = "Mean",
                                   df = "Df",
                                   statistic = "F",
                                   p.value = "Pr(>F)")
FT_sex_results_Table <- set_table_properties(FT_sex_results_Table, 
                                      layout = "autofit")
FT_sex_results_Table <- theme_box(FT_sex_results_Table)
FT_sex_results_Table <- align(FT_sex_results_Table, 
                       part = "header", 
                       align = "center")
FT_sex_results_Table <- bold(FT_sex_results_Table, ~ p.value <0.05, ~ p.value, 
                      bold = TRUE)
FT_sex_results_Table

#save flextable as a word doc
save_as_docx(FT_sex_results_Table, 
             path = "FT_sex_results_Table.docx")

# Pre-planned contrasts (pairwise mean differences we care about)
# LMM_FK_G_S_rLC_emm # print in console to get row numbers for each group
# set the mean as the row number from the emmeans table
Group_1 <- c(1,0,0,0,0,0)
Group_2 <- c(0,1,0,0,0,0)
SBI <-     c(0,0,1,0,0,0)
Group_3 <- c(0,0,0,1,0,0)
Group_4 <- c(0,0,0,0,1,0)
NSD <-     c(0,0,0,0,0,1)

LMM_FK_G_S_rLC_planned <- contrast(LMM_FK_G_S_rLC_emm,
                                   method = list(
                                     "G2 - G4" = c(Group_2 - Group_4),
                                     "G2 - G3" = c(Group_2 - Group_3),
                                     "G1 - G4" = c(Group_1 - Group_4),
                                     "G1 - G3" = c(Group_1 - Group_3),                              
                                     "G1 - G2" = c(Group_1 - Group_2),
                                     "G4 - G3" = c(Group_4 - Group_3)),
                                   adjust = "none") |> 
  summary(infer = TRUE)

LMM_FK_G_S_rLC_planned

LMM_FK_G_S_rLC_emm.tibble <- as_tibble(LMM_FK_G_S_rLC_emm)

FK_contrasts_1_3 <- LMM_FK_G_S_rLC_planned |> 
  filter(contrast %in% c("G1 - G3")) |> 
  mutate(perc_diff = (estimate / 0.275)*100,
         lower_diff = (lower.CL / 0.251)*100,
         upper_diff = (upper.CL / 0.299)*100)

FK_contrasts_1_4 <- LMM_FK_G_S_rLC_planned |> 
  filter(contrast %in% c("G1 - G4")) |> 
  mutate(perc_diff = (estimate / 0.300)*100,
         lower_diff = (lower.CL / 0.283)*100,
         upper_diff = (upper.CL / 0.318)*100)

FK_contrasts_2_3 <- LMM_FK_G_S_rLC_planned |> 
  filter(contrast %in% c("G2 - G3")) |> 
  mutate(perc_diff = (estimate / 0.275)*100,
         lower_diff = (lower.CL / 0.251)*100,
         upper_diff = (upper.CL / 0.299)*100)

FK_contrasts_2_4 <- LMM_FK_G_S_rLC_planned |> 
  filter(contrast %in% c("G2 - G4")) |> 
  mutate(perc_diff = (estimate / 0.300)*100,
         lower_diff = (lower.CL / 0.283)*100,
         upper_diff = (upper.CL / 0.318)*100)

# PLOT BY GROUP w/ s w/ CI's plus pairwise contrasts differences in top plot
# could include filtered out groups in plot? w/ emmeans CI?#FK bar plot faceted by reef type and sex

# note in ggplot below we had to turn the emmeans object into a tibble (data.frame) so it would work as data in ggplot
#as_tibble(lm_len_supp_emm)

# points w/ emmeans 95% CI (LMM estimated marginal means for each group)
plot_LMM_FK_G_S_rLC_emm <- FK_plots |>   
  ggplot(aes(x = Group, y = Fultons_K, color = Group)) +
  geom_jitter(width = 0.1, alpha = 0.2) +
  #group colors: 1 Cold Islands, 2 Mainland ARs, 3 Warm Islands, 4 Mainland NRs
  scale_colour_manual(values = c("lightseagreen", "blue4", "lightseagreen","lightseagreen", "forestgreen", "forestgreen")) +
  scale_x_discrete(labels = c("Cool Islands", "Mainland ARs", 
                              "SBI NR", "Warm Islands", 
                              "Mainland NRs", "NSD NR")) +
  ylab("Fulton's K Condition Factor") +
  ylim(0.15, 0.53) +
  xlab("Cluster Group") +
  #use emm object as 2nd data table to plot 95% CIs
  geom_crossbar(data = as_tibble(LMM_FK_G_S_rLC_emm), 
                aes(y = emmean,
                    ymin = lower.CL,
                    ymax = upper.CL), 
                width = 0.33,
                linewidth = 0.8,
                color =  "black") +
  theme_classic() +
  theme(legend.position="none", 
        axis.text.x = element_text(angle = 45, hjust = 1))

plot_LMM_FK_G_S_rLC_emm

## pairwise contrast effects plot
# note in ggplot below we had to turn the contrast object into a tibble (data.frame) so it would work as data in ggplot
plot_LMM_FK_G_S_rLC_pairs <- as_tibble(LMM_FK_G_S_rLC_planned) |> 
  ggplot(aes(y=factor(contrast, level = c('G4 - G3', 'G2 - G4','G2 - G3','G1 - G4', 'G1 - G3', 'G1 - G2')), 
             x = estimate, 
             label = round((p.value),3))) +
  geom_pointrange(aes(xmin = lower.CL,
                      xmax = upper.CL
  )) +
  geom_vline(xintercept = 0, linetype = 2) +
  xlab("Pairwise Effects") +
  ylab("") +
  scale_y_discrete(labels = c("Warm Isls. - Main. NRs", "Main. ARs - Main. NRs", 
                              "Main. ARs - Warm Isls.", "Cool Isls. - Main. NRs", 
                              "Cool Isls. - Warm Isls.", "Cool Isls. - Main. ARs")) +
  scale_x_continuous(position = "top") +
  theme_classic() +
  theme(plot.margin = margin(0.3,1.2,0.3,0.3, "cm")) +
  geom_text(vjust = 0, nudge_y = 0.2, size = 3)

plot_LMM_FK_G_S_rLC_pairs

#combined emmeans boxplot with pairwise effects plot
results_plot_LMM_FK_G_S_rLC <- plot_grid(plot_LMM_FK_G_S_rLC_pairs, 
                                         plot_LMM_FK_G_S_rLC_emm, 
                                         nrow = 2,
                                         align = "v",
                                         axis = "rl",
                                         rel_heights = c(0.4, 1))
# rel_heights argument specifies relative size of two plots (i.e., first plot is 0.4 as tall as the second plot) 
results_plot_LMM_FK_G_S_rLC

# ggsave("results_plot_LMM_FK_G_S_rLC.png", results_plot_LMM_FK_G_S_rLC,
#        height = 8, 
#        width = 5, 
#        dpi = 600)

## END LMM
# ## Other condition factor plots (exploratory) ------------------------------
# #FK bar plot faceted by reef type and sex
# ggplot(FK_plots, aes(x = Fultons_K)) +
#   geom_histogram() +
#   facet_grid(rows = vars(ReefType), 
#              cols = vars(Sex), 
#              scales="free_y")
# 
# #linear model for condition factor by TL
# FK_lineplot <- ggplot(FK_plots, aes(x = TL_mm, 
#                                     y = Fultons_K, 
#                                     color = IM_RT)) +
#   geom_point(alpha = 0.5, size = 1) +
#   geom_smooth(method = "lm") +
#   labs(x = "Total Fish Length (mm)", 
#        y = "Fulton's K Condition Factor",
#        col = "Reef Type") +
#   theme_classic() +
#   theme(legend.position = c(0.86, 0.85)) +
#   scale_color_manual(values = IM_RT_cols)
# 
# FK_lineplot
# ## Very cool! Higher condition in Artificial reefs
# summary(lm(Fultons_K ~ ReefType * TL_mm, data = FK_plots))
# 
# ggsave("FK_lineplot.png", FK_lineplot,
#        width = 6,
#        height = 4, 
#        dpi = 600)
# 
# #boxplot
# ggplot(FK_plots, aes(x = ReefType, y = Fultons_K, color = ReefType)) +
#   geom_boxplot() +
#   geom_jitter(alpha = 0.5)
# 
# ggplot(FK_plots, aes(x = ReefType, y = Fultons_K, color = ReefType)) +
#   geom_jitter(alpha = 0.5) +
#   geom_crossbar(stat = "summary", fun.data = mean_cl_boot)
# 
# summary(lm(Fultons_K ~ ReefType, data = FK_plots))
# #6% of variation in FK explained by RT
# confint(lm(Fultons_K ~ ReefType, data = FK_plots))
# 
# FK_plot_IM_RT <- ggplot(FK_plots, aes(x = IM_RT, y = Fultons_K, color = IM_RT)) +
#   geom_jitter(alpha = 0.5) +
#   geom_crossbar(stat = "summary", fun.data = mean_cl_boot) +
#   scale_color_manual(values = IM_RT_cols) +
#   ylab("Fulton's K Condition Factor") +
#   scale_x_discrete(limits = c("Island Natural", "Mainland Natural", "Mainland Artificial")) +
#   theme_classic() +
#   theme(legend.position = 'none',
#         axis.title.x = element_blank())
# 
# FK_plot_IM_RT
# 
# confint(lm(Fultons_K ~ IM_RT, data = FK_plots))
# 
# FK_plots %>%
#   group_by(IM_RT) %>%
#   summarise(mean_FK = mean(Fultons_K, na.rm = TRUE),
#             sd_FK = sd(Fultons_K, na.rm = TRUE),
#             n_FK = n()) %>%
#   mutate(SE_FK = sd_FK / sqrt(n_FK),
#          lower_95CI_t_dis = mean_FK - qt(1 - (0.05 / 2), n_FK - 1) * SE_FK,
#          upper_95CI_t_dis = mean_FK + qt(1 - (0.05 / 2), n_FK - 1) * SE_FK)
# 
# ggsave("FK_plot_IM_RT.png", FK_plot_IM_RT,
#        height = 4, 
#        width = 7,
#        dpi = 600)
# 
# FK_plot_loc <- ggplot(FK_plots, aes(x = Location_Code, 
#                                     y = Fultons_K, 
#                                     color = IM_RT)) +
#   geom_jitter(alpha = 0.5) +
#   geom_crossbar(stat = "summary", 
#                 fun.data = mean_cl_boot, 
#                 width = 0.5) +
#   scale_color_manual(values = IM_RT_cols) +
#   labs(color = "Reef Type") +
#   ylab("Fulton's K Condition Factor") +
#   scale_x_discrete(limits = c("AI", "SCRI", "SBI", 
#                               "SCI", "SCLI", "NPV-N", 
#                               "SPV-N", "OC-N", "NSD-N", 
#                               "SSD-N", "NPV-A", "SPV-A", 
#                               "OC-A", "NSD-A", "SSD-A" )) +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
#   facet_wrap(~Sex)
# 
# FK_plot_loc
# 
# ggsave("FK_plot_loc.png", FK_plot_loc,
#        height = 4,
#        width = 7, 
#        dpi = 600)
# 
# #comparing FK by sex
# FK_S_plot <- ggplot(FK_plots, aes(x = Sex, 
#                                   y = Fultons_K, 
#                                   color = IM_RT)) +
#   geom_jitter(alpha = 0.5) +
#   geom_crossbar(stat = "summary", 
#                 fun.data = mean_cl_boot, 
#                 width = 0.5) +
#   scale_color_manual(values = IM_RT_cols) +
#   labs(color = "Reef Type") +
#   ylab("Fulton's K Condition Factor") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# 
# FK_S_plot
# 
# #checking outlier CNs
# ggplot(FK_plots, aes(x = ReefType, y = Fultons_K, color = ReefType)) +
#   geom_jitter(alpha = 0.5) +
#   geom_crossbar(stat = "summary", fun.data = mean_cl_boot) +
#   geom_text(aes(label=ifelse(Fultons_K > 0.5,as.character(CN),'')))
# 
# #FK outliers
# FK_outliers <- Life_History$CN[c(679, 715, 721, 870)]
# 
# #to reference which were removed 
# Life_History_FK_outliers <- Life_History |> 
#   filter(CN %in% FK_outliers)
# 
# Life_History_FK_outliers
# 
# #plotting condition factor with outliers removed
# ggplot(FK_plots, aes(x = TL_mm, y = Wt_g, color = ReefType, shape = Sex)) +
#   geom_point() +
#   geom_smooth()
# 
# #does the pattern persist when outliers are removed?
# FK_no_outliers <- FK_plots |> 
#   filter(!CN %in% FK_outliers)
# 
# #linear model for condition factor by TL with outliers removed
# ggplot(FK_no_outliers, aes(x = TL_mm, y = Fultons_K, color = ReefType)) +
#   geom_point(alpha = 0.5) +
#   geom_smooth(method = "lm") + 
#   labs(x = "Total Fish Length (mm)", y = "Fulton's K Condition Factor") +
#   theme_classic()
# 
# #some overlap of CI with smaller fish, but overall still see artificial having higher condition factor
# summary(lm(Fultons_K ~ ReefType, data = FK_no_outliers))
# #5% of variation in FK explained by RT
# confint(lm(Fultons_K ~ ReefType, data = FK_no_outliers))
# 

# Fish Counts Tables -------------------------------------------------------
## With Dat ----------------------------------------------------------------

#Number of fish by region, reeftype, sex, latitude (arranged in descending order of latitude) for Dat
Summary_CN_R_RT_S <- Dat |> 
  filter(Sex != "U") |> 
  group_by(Region, ReefType, Sex, Region_Latitude) |> 
  distinct(CN) |> 
  summarise(count = (count=n())) |> 
  arrange(desc(Region_Latitude)) 

#Min and max TL for fish per region, reeftype, sex
Summary_CN_R_RT_S_TL <- Dat |> 
  filter(Sex != "U") |> 
  filter(TL_mm != "NA") |> 
  group_by(Region, ReefType, Sex, Region_Latitude) |> 
  summarise(min_TL_mm = min(TL_mm), max_TL_mm = max(TL_mm))

#Joining two tables above
Summary_CN_R_RT_S <- Summary_CN_R_RT_S |> 
  left_join(Summary_CN_R_RT_S_TL) |> 
  mutate(Group = paste(Region, ReefType)) |> 
  mutate(Abbreviation = recode(Region,
                               'Anacapa Island' = "AI",
                               'North Palos Verdes' = "NPV",
                               'North San Diego' = "NSD",
                               'Orange County' = "OC",
                               'San Clemente Island' = "SCLI",
                               'Santa Barbara Island' = "SBI",
                               'Santa Catalina Island' = "SCI",
                               'Santa Cruz Island' = "SCRI",
                               'South Palos Verdes' = "SPV",
                               'South San Diego' = "SSD"))
# Renaming cluster groups
Summary_CN_R_RT_S <- Summary_CN_R_RT_S |> 
  mutate(Cluster_Group = recode(Group,
                                'Santa Cruz Island Natural' = "Cool Islands",
                                'Anacapa Island Natural' = "Cool Islands",
                                'North Palos Verdes Artificial' = "Mainland Artificial Reefs (ARs)",
                                'North Palos Verdes Natural' = "Mainland Natural Reefs (NRs)",
                                'South Palos Verdes Artificial' = "Mainland Artificial Reefs (ARs)",
                                'South Palos Verdes Natural' = "Mainland Natural Reefs (NRs)",
                                'Orange County Artificial' = "Mainland Artificial Reefs (ARs)",
                                'Orange County Natural' = "Mainland Natural Reefs (NRs)",
                                'Santa Barbara Island Natural' = "SBI",
                                'Santa Catalina Island Natural' = "Warm Islands",
                                'North San Diego Artificial' = "Mainland Artificial Reefs (ARs)",
                                'North San Diego Natural' = "NSD NR",
                                'San Clemente Island Natural' = "Warm Islands",
                                'South San Diego Artificial' = "Mainland Artificial Reefs (ARs)",
                                'South San Diego Natural' = "Mainland Natural Reefs (NRs)")) |> 
  mutate(Location = paste(Region, " (", Abbreviation, ")", sep = ""))

FT_sum_cts_long <- flextable(Summary_CN_R_RT_S)

#table using flextable function
Summary_CN_R_RT_S_Wide <- Summary_CN_R_RT_S |> 
  pivot_wider(id_cols = c(Location, ReefType, Cluster_Group, min_TL_mm, max_TL_mm), names_from = Sex, values_from = count)

FT_Summ_CN_R_TF_S <- flextable(Summary_CN_R_RT_S_Wide)
FT_Summ_CN_R_TF_S <- set_header_labels(FT_Summ_CN_R_TF_S, 
                                       ReefType = "Reef Type",
                                       Cluster_Group = "Diet Similarity Cluster Groups",
                                       min_TL_mm = "Min. TL (mm)",
                                       max_TL_mm = "Max. TL (mm)",
                                       F = "Female",
                                       M = "Male")

FT_Summ_CN_R_TF_S <- align(FT_Summ_CN_R_TF_S, 
                       part = "header", 
                       align = "left")
FT_Summ_CN_R_TF_S <- set_table_properties(FT_Summ_CN_R_TF_S, 
                                      layout = "autofit")

FT_Summ_CN_R_TF_S <- set_table_properties(FT_Summ_CN_R_TF_S, layout = "autofit")
FT_Summ_CN_R_TF_S <- theme_box(FT_Summ_CN_R_TF_S)
FT_Summ_CN_R_TF_S <- merge_v(FT_Summ_CN_R_TF_S, j = ~ Location)
# FT_Summ_CN_R_TF_S <- add_footer_lines(FT_Summ_CN_R_TF_S, "Number of female and male fish per location and reef type.") #adds figure caption on the footer
# FT_Summ_CN_R_TF_S <- color(FT_Summ_CN_R_TF_S, part = "footer", color = "#666666") #text color of footer caption
# brdr <- fp_border_default(color = "darkorange3", width = 1.5)
# FT_Summ_CN_R_TF_S <- border_outer(FT_Summ_CN_R_TF_S, border = brdr)
FT_Summ_CN_R_TF_S

#save flextable as a word doc
save_as_docx(FT_Summ_CN_R_TF_S, path = "FT_Summ_CN_R_TF_S.docx")

## With Dat_AISCRI ----------------------------------------------------------------

#Number of fish by Reg, RT, S, Reg_Lat (arranged in descending order of latitude) for Dat_AISCRI
Summary_CN_R_RT_S_2 <- Dat_AISCRI |> 
  filter(Sex != "U") |> 
  group_by(Region, ReefType, Sex, Region_Latitude) |> 
  distinct(CN) |> 
  summarise(count = (count=n())) |> 
  arrange(desc(Region_Latitude)) 

Summary_CN_R_RT_S_2 <- Summary_CN_R_RT_S_2 |> 
  summarise(count = sum(count))

#table using flextable function
Summary_CN_R_RT_S_Wide_2 <- Summary_CN_R_RT_S_2 |> 
  pivot_wider(id_cols = c(Region, ReefType), names_from = Sex, values_from = count)

FT_Summ_CN_R_TF_S_2 <- flextable(Summary_CN_R_RT_S_Wide_2)
FT_Summ_CN_R_TF_S_2 <- set_header_labels(FT_Summ_CN_R_TF_S_2, 
                                       Region = "Location",
                                       ReefType = "Reef Type",
                                       F = "Female",
                                       M = "Male")
FT_Summ_CN_R_TF_S_2 <- set_table_properties(FT_Summ_CN_R_TF_S_2, layout = "autofit")
FT_Summ_CN_R_TF_S_2 <- theme_box(FT_Summ_CN_R_TF_S_2)
FT_Summ_CN_R_TF_S_2 <- merge_v(FT_Summ_CN_R_TF_S_2, j = ~ Region)

FT_Summ_CN_R_TF_S_2

# Line Plots --------------------------------------------------------------
#Comparing fish total content category sum_Dry_Weight by body weight to evaluate if dividing by body weight is effective in accounting for fish heftyness 

#assigning colors to each sex
cols <- c("F" = "darkorange", "M" = "deepskyblue3")

Dat_CN <- Dat |>
  filter(Sex != "U") |>   
  group_by(CN, Region, ReefType, Sex, Wt_g) |> 
  summarize(sum_Dry_Weight_mg = sum(Dry_Weight_mg)) |> 
  mutate(prop_SDW_mg_body_wt_g = sum_Dry_Weight_mg/Wt_g) |> 
  ungroup()

Sum_DW_mg_Body_Wt_g.plot <- Dat_CN |>  
  ggplot(aes(x = Wt_g, y = sum_Dry_Weight_mg, color = Sex)) +
  geom_point() +
  geom_text(x = 900, y = 1370, label = "A", color = "black", size = 4.5) + #adding figure letter label on top right
  scale_color_manual(values = cols) +
  geom_smooth(method = lm, se = F) +
  xlab(NULL) +
  ylab("Stomach Content Dry Weight (mg)") + 
  theme_classic() +
  theme(legend.position = c(0.12, 0.85))

Sum_DW_mg_Body_Wt_g.plot

#data with only males (for lm)
Dat_CN_M <- Dat_CN |> 
  filter(Sex == "M")

#data with only females (for lm)
Dat_CN_F <- Dat_CN |> 
  filter(Sex == "F")

# Fit linear model for males
lm_SDW_Wt_M <- lm(sum_Dry_Weight_mg ~ Wt_g, data = Dat_CN_M)
# extra model summary in tidy format w/ 95% CIs of slope
parms_M <- tidy(summary(lm_SDW_Wt_M), conf.int = TRUE) |> 
  mutate(Sex = "M")
# Fit linear model for females
lm_SDW_Wt_F <- lm(sum_Dry_Weight_mg ~ Wt_g, data = Dat_CN_F)
# extra model summary in tidy format w/ 95% CIs of slope
parms_F <- tidy(summary(lm_SDW_Wt_F), conf.int = TRUE) |> 
  mutate(Sex = "F")

# combine tables, filter for just slope parameters (don't need y-intercepts)
parms_table <- bind_rows(parms_M, parms_F) |> 
  filter(term == "Wt_g")

parms_table

#line plot of proportion of total dry gut contents weight / body weight
Prop_Sum_DW_mg_Body_Wt_g.plot <- Dat_CN |> 
  ggplot(aes(x = Wt_g, y = prop_SDW_mg_body_wt_g, color = Sex)) +
  geom_point() +
  geom_text(x = 900, y = 3.1, label = "B", color = "black", size = 4.5) + #adding figure letter label on top right
  scale_color_manual(values = cols) +
  geom_smooth(method = lm, se = F) +
  xlab("Body Weight (g)") +
  #ylab("Proportion of Dry Gut Contents Weight per Fish Body Weight") +
  ylab("Stomach Content Dry Weight (mg) /\n Body Weight (g)") +
  theme_classic() +
  theme(legend.position = 'none') 

Prop_Sum_DW_mg_Body_Wt_g.plot

# Fit linear model for males
lm_SDW.Wt_Wt_M <- lm(prop_SDW_mg_body_wt_g ~ Wt_g, data = Dat_CN_M)
# extra model summary in tidy format w/ 95% CIs of slope
SDW.Wt_Wt_parms_M <- tidy(summary(lm_SDW.Wt_Wt_M), conf.int = TRUE) |> 
  mutate(Sex = "M")
# Fit linear model for females
lm_SDW.Wt_Wt_F <- lm(prop_SDW_mg_body_wt_g ~ Wt_g, data = Dat_CN_F)
# extra model summary in tidy format w/ 95% CIs of slope
SDW.Wt_Wt_parms_F <- tidy(summary(lm_SDW.Wt_Wt_F), conf.int = TRUE) |> 
  mutate(Sex = "F")
# combine tables, filter for just slope parameters (don't need y-intercepts)
SDW.Wt_Wt_parms_table <- bind_rows(SDW.Wt_Wt_parms_M, SDW.Wt_Wt_parms_F) |> 
  filter(term == "Wt_g")

SDW.Wt_Wt_parms_table

# combine two plots above
plot.DW_mg_Body_Wt_g_compare.final <- plot_grid(Sum_DW_mg_Body_Wt_g.plot,
                                                Prop_Sum_DW_mg_Body_Wt_g.plot, 
                                                nrow = 2,
                                                align = "v",
                                                axis = "rl")
plot.DW_mg_Body_Wt_g_compare.final

# ggsave("plot.DW_mg_Body_Wt_g_compare.final.png", plot.DW_mg_Body_Wt_g_compare.final,
#        width = 10,
#        height = 7, 
#        dpi = 600)

# Box Plot ----------------------------------------------------------------

#Box plot of proportional dry weight sum by site and reef type (are fish on AR or Natural just eating more)?
Dat_CN |>
  ggplot(aes(x = paste(Region, ReefType), y = prop_SDW_mg_body_wt_g, color = ReefType)) +
  geom_boxplot()

# NMDS by R, RT, S --------------------------------------------------------

# Average + sq-rt across CNs by Reg_code, Reef Type and Sex for each Gut_Content
Dat_R_RT_S_GC <- Dat |>
  group_by(Region, ReefType, Sex, Gut_Content, IM_RT) |> 
  summarize(mean_prop_DW_mg_body_wt_g = (mean(prop_DW_mg_body_wt_g)),
            RegionSST = mean(RegionSST)) |>
  mutate(sqrt_mean_prop_DW_mg_body_wt_g = mean_prop_DW_mg_body_wt_g^0.5) |>  #sqrt transformed 
  filter(Sex != "U")

#put data in wide format using pivot_wider (Region, Reef Type, Sex)
Gut_Wide_R_RT_S <- Dat_R_RT_S_GC |>
  mutate(Variables_Label = paste(Region, ReefType, Sex)) |>
  pivot_wider(id_cols = c(Variables_Label, Region, ReefType, Sex, IM_RT, RegionSST),
              names_from = Gut_Content , 
              values_from = sqrt_mean_prop_DW_mg_body_wt_g)

#make "community data" table format to go into vegan package functions
Comm_Gut_Wide_R_RT_S <- Gut_Wide_R_RT_S |> 
  column_to_rownames(var = "Variables_Label") |> 
  select(-Region, -ReefType, -Sex, -IM_RT, -RegionSST)  #remove the columns to get into comm format 

#create nMDS
Rc_RT_S_NMDS <- metaMDS(Comm_Gut_Wide_R_RT_S, 
                              distance = "bray", #using bray-curtis dissimilarity 
                              autotransform = FALSE, trymax=2000)

#extract nMDS stress for plotting
plot_mds_stress <-round(Rc_RT_S_NMDS$stress, 2)

#create tibble with nMDS scores
tibble.Rc_RT_S_NMDS <- as_tibble(scores(Rc_RT_S_NMDS$points), 
                                      rownames = "Variables_Label")

#add variables and values for plotting
tibble.Rc_RT_S_NMDS<- tibble.Rc_RT_S_NMDS |> 
  left_join(Gut_Wide_R_RT_S)

tibble.Rc_RT_S_NMDS |> 
  mutate()

#creating vector of order for Sex
tibble.Rc_RT_S_NMDS$Sex <- factor(tibble.Rc_RT_S_NMDS$Sex, 
                                         levels = c("M", "F"))

#recoding the Location_Code into location abbreviations (there's probably a shorter way to do this)
tibble.Rc_RT_S_NMDS <- tibble.Rc_RT_S_NMDS |> 
  mutate(Location_Code = recode(Variables_Label,
                                'Anacapa Island Natural F' = "AI",
                                'Anacapa Island Natural M' = "AI",
                                'North Palos Verdes Artificial F' = "NPV",
                                'North Palos Verdes Artificial M' = "NPV",
                                'North Palos Verdes Natural F' = "NPV",
                                'North Palos Verdes Natural M' = "NPV",
                                'North San Diego Artificial F' = "NSD",
                                'North San Diego Artificial M' = "NSD",
                                'North San Diego Natural F' = "NSD",
                                'North San Diego Natural M' = "NSD",
                                'Orange County Artificial F' = "OC",
                                'Orange County Artificial M' = "OC",
                                'Orange County Natural F' = "OC",
                                'Orange County Natural M' = "OC",
                                'San Clemente Island Natural F' = "SCLI",
                                'San Clemente Island Natural M' = "SCLI",
                                'Santa Barbara Island Natural F' = "SBI",
                                'Santa Barbara Island Natural M' = "SBI",
                                'Santa Catalina Island Natural F' = "SCI",
                                'Santa Catalina Island Natural M' = "SCI",
                                'Santa Cruz Island Natural F' = "SCRI",
                                'Santa Cruz Island Natural M' = "SCRI",
                                'South Palos Verdes Artificial F' = "SPV",
                                'South Palos Verdes Artificial M' = "SPV",
                                'South Palos Verdes Natural F' = "SPV",
                                'South Palos Verdes Natural M' = "SPV",
                                'South San Diego Artificial F' = "SSD",
                                'South San Diego Artificial M' = "SSD",
                                'South San Diego Natural F' = "SSD",
                                'South San Diego Natural M' = "SSD"))

  plot.Rc_RT_S_NMDS <- ggplot(tibble.Rc_RT_S_NMDS, 
                                 aes(MDS1, (MDS2*-1),#(NMDS2*-1) because somewhere in code axis got flipped
                                     color = IM_RT,
                                     shape = Sex)) + #sizing points by temp
  geom_point() +
  # stat_ellipse() +
  scale_shape_manual(values=c(15,17)) +
  geom_text_repel(aes(label = Location_Code), 
            hjust = 0.3, 
            nudge_x = 0.03, 
            min.segment.length = 1,
            size = 4) + #text size of location labels
    theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  scale_color_manual(name = "Reef Type", 
                       values = c("Island Natural" = "lightseagreen", 
                                  "Mainland Natural" = "forestgreen", 
                                  "Mainland Artificial" = "blue4")) +
  theme(legend.position = c(0.9, 0.7),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11)) +
  geom_text(aes(y = 0.5, x = 0.7, 
                label = paste("2D Stress:", plot_mds_stress)), 
            vjust = "inward", 
            hjust = "inward", 
            color = "black") 
# for nMDS plots it is also customary to have 2D stress in upper right corner (ideally this should be less than 0.2, and not near 0)

print(plot.Rc_RT_S_NMDS)

# ggsave("plot.Rc_RT_S_NMDS.png", plot.Rc_RT_S_NMDS,
#        width = 9,
#        height = 7, 
#        dpi = 600)

#"Species" Scores
Comm_Gut_Wide_R_RT_S.spp.fit <- envfit(Rc_RT_S_NMDS, 
                                       Comm_Gut_Wide_R_RT_S, 
                                       permutations = 999) # this fits species vectors

Spp_Comm_Gut_Wide_R_RT_S.scrs <- as_tibble(scores(Comm_Gut_Wide_R_RT_S.spp.fit, 
                                                  display = "vectors"), 
                                           rownames = "Gut_Content") |> #save species intrinsic values into dataframe
  mutate(pval = Comm_Gut_Wide_R_RT_S.spp.fit$vectors$pvals) |> #add pvalues to dataframe so you can select species which are significant
  filter(pval <= 0.05) |>
# make species arrow vectors shorter
  mutate(NMDS1 = NMDS1 * 0.7,
         NMDS2 = NMDS2 * 0.7)

#Plot "species" vectors on top of nMDS
plot.Rc_RT_S_NMDS.final <- plot.Rc_RT_S_NMDS +
  geom_segment(data = Spp_Comm_Gut_Wide_R_RT_S.scrs, 
               aes(x = 0, 
                   xend = NMDS1, 
                   y = 0, 
                   yend = (NMDS2*-1), 
                   shape = NULL, 
                   fill =  NULL), # need fill Null because specified in mds above
               arrow = arrow(length = unit(0.2, "cm")), #size of the arrow head
               colour = "grey10", lwd=0.3) + #add vector arrows of significant species
  geom_text_repel(data = Spp_Comm_Gut_Wide_R_RT_S.scrs, 
                  aes(x=NMDS1, 
                      y=(NMDS2*-1), 
                      label = Gut_Content, 
                      shape = NULL, 
                      color = NULL, 
                      fill =  NULL), # need color Null because specified in mds above
                      cex = 4, #text size of the arrow labels
                  direction = "y", 
                  segment.size = 0.25) #add labels for species, use ggrepel

print(plot.Rc_RT_S_NMDS.final)

# #save plot as png file
# ggsave("plot.Rc_RT_S_NMDS.final.png", plot.Rc_RT_S_NMDS.final,
#        width = 9,
#        height = 7, 
#        dpi = 600)

# PERMANOVAs --------------------------------------------------------------

## Region + Reef Type + Sex ####
PMV_R_RT_S <- adonis2(Comm_Gut_Wide_R_RT_S ~ Region + ReefType + Sex, #  
                      data = Gut_Wide_R_RT_S, 
                      by = "margin", #because data unbalanced
                      permutations = 999, 
                      method = "bray")

PMV_R_RT_S

## broom package extends function tidy() to clean up model outputs
PMV_R_RT_S.tibble <- tidy(PMV_R_RT_S)

PMV_R_RT_S.tibble <- PMV_R_RT_S.tibble |> 
  mutate_at(vars(SumOfSqs, R2, statistic), round, digits = 2)

# use flex table to output PERMANOVA results table in word
FT_PMV_R_RT_S <- flextable(PMV_R_RT_S.tibble)
FT_PMV_R_RT_S <- theme_vanilla(FT_PMV_R_RT_S)
FT_PMV_R_RT_S <- set_header_labels(FT_PMV_R_RT_S,
                                   term = NA,
                                   df = "Df",
                                   SumOfSqs = "SS",
                                   R2 = "R^2",
                                   statistic = "F",
                                   p.value = "Pr(>F)")
FT_PMV_R_RT_S <- align(FT_PMV_R_RT_S, 
                       part = "header", 
                       align = "center")
FT_PMV_R_RT_S <- bold(FT_PMV_R_RT_S, ~ p.value <0.05, ~ p.value, 
                      bold = TRUE)
FT_PMV_R_RT_S <- set_table_properties(FT_PMV_R_RT_S, 
                                      layout = "autofit")

FT_PMV_R_RT_S

#save flextable as a word doc
save_as_docx(FT_PMV_R_RT_S, 
             path = "FT_PMV_R_RT_S.docx")

##!FINAL: RegionSST + Island/Mainland ReefTypes + Sex ####
#PERMANOVA replacing Region (location) with temperature 

PMV_SST_IMRT_S <- adonis2(Comm_Gut_Wide_R_RT_S ~ RegionSST + IM_RT + Sex, #  
                        data = Gut_Wide_R_RT_S, 
                        by = "margin", #because data unbalanced
                        permutations = 999, 
                        method = "bray")

PMV_SST_IMRT_S

## broom package extends function tidy() to clean up model outputs
PMV_SST_IMRT_S.tibble <- tidy(PMV_SST_IMRT_S)

PMV_SST_IMRT_S.tibble <- PMV_SST_IMRT_S.tibble |> 
  mutate_at(vars(SumOfSqs, R2, statistic), round, digits = 2)

PMV_SST_IMRT_S.tibble

# use flex table to output PERMANOVA results table in word
FT_PMV_SST_IMRT_S <- flextable(PMV_SST_IMRT_S.tibble)
FT_PMV_SST_IMRT_S <- theme_vanilla(FT_PMV_SST_IMRT_S)
FT_PMV_SST_IMRT_S <- set_header_labels(FT_PMV_SST_IMRT_S,
                                   term = NA,
                                   df = "Df",
                                   SumOfSqs = "SS",
                                   R2 = "R^2",
                                   statistic = "F",
                                   p.value = "Pr(>F)")
FT_PMV_SST_IMRT_S <- align(FT_PMV_SST_IMRT_S, 
                       part = "header", 
                       align = "center")
FT_PMV_SST_IMRT_S <- bold(FT_PMV_SST_IMRT_S, ~ p.value <0.05, ~ p.value, 
                      bold = TRUE)
FT_PMV_SST_IMRT_S <- set_table_properties(FT_PMV_SST_IMRT_S, 
                                      layout = "autofit")
FT_PMV_SST_IMRT_S

#save flextable as a word doc
save_as_docx(FT_PMV_SST_IMRT_S, 
             path = "FT_PMV_SST_IMRT_S.docx")

##RegionSST * Island/Mainland ReefTypes + Sex ####
#PERMANOVA replacing Region (location) with temperature 

PMV_SST_RT_S.int <- adonis2(Comm_Gut_Wide_R_RT_S ~ RegionSST * IM_RT + Sex, #  
                        data = Gut_Wide_R_RT_S, 
                        by = "margin", #because data unbalanced
                        permutations = 999, 
                        method = "bray")

PMV_SST_RT_S.int

## broom package extends function tidy() to clean up model outputs
PMV_SST_RT_S.int.tibble <- tidy(PMV_SST_RT_S.int)

PMV_SST_RT_S.int.tibble <- PMV_SST_RT_S.int.tibble |> 
  mutate_at(vars(SumOfSqs, R2, statistic), round, digits = 2)

PMV_SST_RT_S.int.tibble

# use flex table to output PERMANOVA results table in word
FT_PMV_SST_RT_S.int <- flextable(PMV_SST_RT_S.int.tibble)
FT_PMV_SST_RT_S.int <- theme_vanilla(FT_PMV_SST_RT_S.int)
FT_PMV_SST_RT_S.int <- set_header_labels(FT_PMV_SST_RT_S.int,
                                     term = NA,
                                     df = "Df",
                                     SumOfSqs = "SS",
                                     R2 = "R^2",
                                     statistic = "F",
                                     p.value = "Pr(>F)")
FT_PMV_SST_RT_S.int <- align(FT_PMV_SST_RT_S.int, 
                         part = "header", 
                         align = "center")
FT_PMV_SST_RT_S.int <- bold(FT_PMV_SST_RT_S.int, ~ p.value <0.05, ~ p.value, 
                        bold = TRUE)
FT_PMV_SST_RT_S.int <- set_table_properties(FT_PMV_SST_RT_S.int, 
                                        layout = "autofit")
FT_PMV_SST_RT_S.int

#save flextable as a word doc
save_as_docx(FT_PMV_SST_RT_S.int, 
             path = "FT_PMV_R_RT_S.int.docx")

##RegionSST + ReefTypes + Sex ####
#PERMANOVA replacing Region (location) with temperature 

PMV_SST_RT_S <- adonis2(Comm_Gut_Wide_R_RT_S ~ RegionSST + ReefType + Sex, #  
                        data = Gut_Wide_R_RT_S, 
                        by = "margin", #because data unbalanced
                        permutations = 999, 
                        method = "bray")

PMV_SST_RT_S

## broom package extends function tidy() to clean up model outputs
PMV_SST_RT_S.tibble <- tidy(PMV_SST_RT_S)

PMV_SST_RT_S.tibble <- PMV_SST_RT_S.tibble |> 
  mutate_at(vars(SumOfSqs, R2, statistic), round, digits = 2)

PMV_SST_RT_S.tibble

# use flex table to output PERMANOVA results table in word
FT_PMV_SST_RT_S <- flextable(PMV_SST_RT_S.tibble)
FT_PMV_SST_RT_S <- theme_vanilla(FT_PMV_SST_RT_S)
FT_PMV_SST_RT_S <- set_header_labels(FT_PMV_SST_RT_S,
                                     term = NA,
                                     df = "Df",
                                     SumOfSqs = "SS",
                                     R2 = "R^2",
                                     statistic = "F",
                                     p.value = "Pr(>F)")
FT_PMV_SST_RT_S <- align(FT_PMV_SST_RT_S, 
                         part = "header", 
                         align = "center")
FT_PMV_SST_RT_S <- bold(FT_PMV_SST_RT_S, ~ p.value <0.05, ~ p.value, 
                        bold = TRUE)
FT_PMV_SST_RT_S <- set_table_properties(FT_PMV_SST_RT_S, 
                                        layout = "autofit")
FT_PMV_SST_RT_S
#save flextable as a word doc
save_as_docx(FT_PMV_SST_RT_S, 
             path = "FT_PMV_R_RT_S.docx")

# Creating Color Palette for Diet Items --------------------------------------------------
#assign colors to my_cols object
my_cols <- c("darkolivegreen3", "lightpink2", "darkgoldenrod4", "red3", "beige", "orchid3", "cadetblue3", "orangered", "orange", "mediumpurple3", "azure3", "peru", "darkseagreen", "sandybrown", "salmon", "blanchedalmond")
my_cols

# Stacked Bar Plots -------------------------------------------------------
## DBI by CN ####
DBI_CN_dat <- Dat |>
  group_by(CN, Gut_Content) |>
  filter(Sex != "U") |> #Remove Sex = U 
  mutate(DBI = (mean(prop_DW_mg_body_wt_g))) |> 
  mutate(Major_Gut_Content = recode(Gut_Content, 
                              'Anemones' ="Animal",
                              'Encrusting Bryozoans' ="Animal", 
                              'Brittle Stars'="Animal", 
                              'Brown Algae'="Algae", 
                              'Branching Bryozoans'="Animal", 
                              'Coralline Red Algae'="Algae", 
                              'Crustaceans'="Animal", 
                              'Garibaldi Eggs'="Animal", 
                              'Gorgonians'="Animal",
                              'Green Algae'="Algae", 
                              'Hydroids'="Animal",
                              'Mollusks' = "Animal",
                              'Polychaete Worms' = "Animal",
                              'Fleshy Red Algae' = "Algae",
                              'Sponge' = "Animal",
                              'Tunicates' = "Animal")) 
# 
# SBI <- DBI_CN_dat |> 
#   filter(Region == "Santa Barbara Island")
# SBI |> 
#   ggplot(aes(x = Gut_Content, y = DBI)) +
#   geom_point() +
#   geom_text(aes(label=ifelse(DBI > 0.00025,as.character(CN),'')))
#CN 959 with a lot of crustacean but removing it and rerunning nmds did not change relative postion

# DBI by gut content category 
DBI_R_RT_S_GC <- DBI_CN_dat |> 
  group_by(Gut_Content,Region, ReefType, Sex) |> 
  filter(Sex != "U") |> 
  mutate(DBI_Category = mean(DBI))

# Find overall proportions of food items
DBI_R_RT_S_GC.order <- DBI_R_RT_S_GC |>
  group_by(Gut_Content) |> 
  summarize(mean_DBI = mean(DBI_Category)) |> 
  arrange(Gut_Content) %>%
  mutate(Gut_Content = factor(Gut_Content, levels=c("Green Algae",
                                                    "Coralline Red Algae",
                                                    "Brown Algae",
                                                    "Fleshy Red Algae", 
                                                    "Tunicates",
                                                    "Gorgonians",
                                                    "Brittle Stars",
                                                    "Crustaceans",
                                                    "Garibaldi Eggs",
                                                    "Mollusks",
                                                    "Branching Bryozoans",
                                                    "Hydroids",
                                                    "Encrusting Bryozoans",
                                                    "Sponge",
                                                    "Anemones",
                                                    "Polychaete Worms"))) %>% 
  arrange(Gut_Content)

# define order for plotting/display of Gut_Content
DBI_R_RT_S_GC$Gut_Content <- factor(DBI_R_RT_S_GC$Gut_Content, 
                                          levels = DBI_R_RT_S_GC.order$Gut_Content)

#create community data using pivot_wider 
Comm_DBI_R_RT_S_GC <- DBI_R_RT_S_GC |>
  mutate(Variables_Label = paste(Region, ReefType, Sex),
         Group = recode(Variables_Label,
                        'Anacapa Island Natural F' = "Cool Islands",
                        'Anacapa Island Natural M' = "Cool Islands",
                        'Santa Cruz Island Natural F' = "Cool Islands",
                        'Santa Cruz Island Natural M' = "Cool Islands",
                        'North Palos Verdes Artificial F' = "Mainland ARs",
                        'North Palos Verdes Artificial M' = "Mainland ARs",
                        'South Palos Verdes Artificial F' = "Mainland ARs",
                        'South Palos Verdes Artificial M' = "Mainland ARs",
                        'Orange County Artificial F' = "Mainland ARs",
                        'Orange County Artificial M' = "Mainland ARs",
                        'North San Diego Artificial F' = "Mainland ARs",
                        'North San Diego Artificial M' = "Mainland ARs",
                        'South San Diego Artificial F' = "Mainland ARs",
                        'South San Diego Artificial M' = "Mainland ARs",
                        'Santa Barbara Island Natural F' = "SBI",
                        'Santa Barbara Island Natural M' = "SBI",
                        'Santa Catalina Island Natural F' = "Warm Islands",
                        'Santa Catalina Island Natural M' = "Warm Islands",
                        'San Clemente Island Natural F' = "Warm Islands",
                        'San Clemente Island Natural M' = "Warm Islands",
                        'North Palos Verdes Natural F' = "Mainland NRs",
                        'North Palos Verdes Natural M' = "Mainland NRs",
                        'South Palos Verdes Natural F' = "Mainland NRs",
                        'South Palos Verdes Natural M' = "Mainland NRs",
                        'Orange County Natural F' = "Mainland NRs",
                        'Orange County Natural M' = "Mainland NRs",
                        'South San Diego Natural F' = "Mainland NRs",
                        'South San Diego Natural M' = "Mainland NRs",
                        'North San Diego Natural F' = "NSD",
                        'North San Diego Natural M' = "NSD"))

mean_DBI_Category_dat <- Comm_DBI_R_RT_S_GC |> 
  group_by(Variables_Label, Gut_Content, Major_Gut_Content, Group) |> 
  summarise(mean_DBI_Category = mean(DBI_Category))

mean_DBI_Category_dat |> 
  distinct(Variables_Label) |> 
  print(n = Inf)

Variables_Label_order <- c("Anacapa Island Natural F",
                           "Anacapa Island Natural M",   
                           "Santa Cruz Island Natural F",    
                           "Santa Cruz Island Natural M", 
                           "Santa Barbara Island Natural F", 
                           "Santa Barbara Island Natural M", 
                           "Santa Catalina Island Natural F",
                           "Santa Catalina Island Natural M",
                           "San Clemente Island Natural F",   
                           "San Clemente Island Natural M",
                           "North Palos Verdes Natural F",  
                           "North Palos Verdes Natural M",
                           "South Palos Verdes Natural F",   
                           "South Palos Verdes Natural M", 
                           "Orange County Natural F",        
                           "Orange County Natural M",  
                           "North San Diego Natural F",      
                           "North San Diego Natural M",  
                           "South San Diego Natural F",     
                           "South San Diego Natural M",
                           "North Palos Verdes Artificial F",
                           "North Palos Verdes Artificial M",
                           "South Palos Verdes Artificial F",
                           "South Palos Verdes Artificial M",
                           "Orange County Artificial F",     
                           "Orange County Artificial M",
                           "North San Diego Artificial F",   
                           "North San Diego Artificial M",
                           "South San Diego Artificial F",   
                           "South San Diego Artificial M")

#creating vector of order so variables label from north to south 
mean_DBI_Category_dat$Variables_Label = factor(mean_DBI_Category_dat$Variables_Label, 
                                               levels = rev(unique(Variables_Label_order)))

#ordering locations for plotting based on cluster groups
Loc_order <- c("Anacapa Island Natural F",
               "Anacapa Island Natural M",   
               "Santa Cruz Island Natural F",    
               "Santa Cruz Island Natural M",
               "North Palos Verdes Artificial F",
               "North Palos Verdes Artificial M",
               "South Palos Verdes Artificial F",
               "South Palos Verdes Artificial M",
               "Orange County Artificial F",     
               "Orange County Artificial M",
               "North San Diego Artificial F",   
               "North San Diego Artificial M",
               "South San Diego Artificial F",   
               "South San Diego Artificial M",
               "Santa Barbara Island Natural F", 
               "Santa Barbara Island Natural M", 
               "Santa Catalina Island Natural F",
               "Santa Catalina Island Natural M",
               "San Clemente Island Natural F",   
               "San Clemente Island Natural M",
               "North Palos Verdes Natural F",  
               "North Palos Verdes Natural M",
               "South Palos Verdes Natural F",   
               "South Palos Verdes Natural M", 
               "Orange County Natural F",        
               "Orange County Natural M",  
               "South San Diego Natural F",     
               "South San Diego Natural M",
               "North San Diego Natural F",      
               "North San Diego Natural M")

mean_DBI_Category_dat$Variables_Label = factor(mean_DBI_Category_dat$Variables_Label, levels = rev(unique(Loc_order)))

Group_orders <- c("NSD",
                  "Mainland NRs",
                  "Warm Islands",
                  "SBI",
                  "Mainland ARs",
                  "Cool Islands")

mean_DBI_Category_dat$Group = factor(mean_DBI_Category_dat$Group, levels = rev(unique(Group_orders)))

#Barplot with all of the variables from the NMDS (Region, ReefType, and Sex)
barplot_RC_RT_S_SC <- mean_DBI_Category_dat |>
  ggplot( aes(x = Variables_Label, 
              y = mean_DBI_Category, 
              fill = Gut_Content, 
              pattern = Major_Gut_Content)) +
#geom_bar_pattern replaces geom_bar - part of the ggpattern package needed to add diagonal lines to algae categories
  geom_bar_pattern(stat="identity", 
                   position = "fill", 
                   width = .75, 
                   colour = "black",
                   pattern_color = 'black', 
                   pattern_fill = 'black', 
                   pattern_density = 0.1) +   
  facet_grid(Group ~., scales = "free", space = "free") +
  scale_pattern_manual(values = c(Algae = "stripe", Animal = "none"),
                       guide = "none") +
  guides(fill = guide_legend(reverse = T, 
                             override.aes = list(pattern = c("none", "none","none", "none", "none", "none","none", "none","none", "none", "none", "none", "stripe", "stripe", "stripe", "stripe")))) +  # change order of GC in legend + add stripes
  coord_flip() +
  labs(x = " ", 
       y = "Average DBI Proportion in Diet") +
  theme_bw() +
  theme(axis.text = element_text(size = 13),
        legend.text = element_text(size = 13),
        axis.title = element_text(size = 15)) +
  theme(legend.title = element_blank()) +
  theme(strip.text = element_text(size = 11),
        strip.background = element_rect(color = "black", fill = "white")) +
  scale_fill_manual(values = (my_cols))

barplot_RC_RT_S_SC

# ggsave("barplot_RC_RT_S_SC.png", barplot_RC_RT_S_SC,
#        width = 10,
#        height = 9, 
#        dpi = 600)

## DBI by CN major categories####
DBI_CN_dat_major <- Dat_Major |>
  group_by(CN, Gut_Content) |>
  filter(Sex != "U") |> #Remove Sex = U 
  mutate(DBI = (mean(prop_DW_mg_body_wt_g)))

# DBI by gut content category 
DBI_R_RT_S_GC_major <- DBI_CN_dat_major |> 
  group_by(Gut_Content,Region, ReefType, Sex) |> 
  filter(Sex != "U") |> 
  mutate(DBI_Category = mean(DBI))

# Find overall proportions of food items
DBI_R_RT_S_GC_major.order <- DBI_R_RT_S_GC_major |>
  group_by(Gut_Content) |> 
  summarize(mean_DBI = mean(DBI_Category)) |>
  arrange(mean_DBI)

# define order for plotting/display of Gut_Content
DBI_R_RT_S_GC_major$Gut_Content <- factor(DBI_R_RT_S_GC_major$Gut_Content, 
                                    levels = DBI_R_RT_S_GC_major.order$Gut_Content)

#create community data using pivot_wider 
Comm_DBI_R_RT_S_GC_major <- DBI_R_RT_S_GC_major |>
  mutate(Variables_Label = paste(Region, ReefType, Sex),
         Group = recode(Variables_Label,
                        'Anacapa Island Natural F' = "Cool Islands",
                        'Anacapa Island Natural M' = "Cool Islands",
                        'Santa Cruz Island Natural F' = "Cool Islands",
                        'Santa Cruz Island Natural M' = "Cool Islands",
                        'North Palos Verdes Artificial F' = "Mainland ARs",
                        'North Palos Verdes Artificial M' = "Mainland ARs",
                        'South Palos Verdes Artificial F' = "Mainland ARs",
                        'South Palos Verdes Artificial M' = "Mainland ARs",
                        'Orange County Artificial F' = "Mainland ARs",
                        'Orange County Artificial M' = "Mainland ARs",
                        'North San Diego Artificial F' = "Mainland ARs",
                        'North San Diego Artificial M' = "Mainland ARs",
                        'South San Diego Artificial F' = "Mainland ARs",
                        'South San Diego Artificial M' = "Mainland ARs",
                        'Santa Barbara Island Natural F' = "SBI",
                        'Santa Barbara Island Natural M' = "SBI",
                        'Santa Catalina Island Natural F' = "Warm Islands",
                        'Santa Catalina Island Natural M' = "Warm Islands",
                        'San Clemente Island Natural F' = "Warm Islands",
                        'San Clemente Island Natural M' = "Warm Islands",
                        'North Palos Verdes Natural F' = "Mainland NRs",
                        'North Palos Verdes Natural M' = "Mainland NRs",
                        'South Palos Verdes Natural F' = "Mainland NRs",
                        'South Palos Verdes Natural M' = "Mainland NRs",
                        'Orange County Natural F' = "Mainland NRs",
                        'Orange County Natural M' = "Mainland NRs",
                        'South San Diego Natural F' = "Mainland NRs",
                        'South San Diego Natural M' = "Mainland NRs",
                        'North San Diego Natural F' = "NSD",
                        'North San Diego Natural M' = "NSD"))

mean_DBI_Category_dat_major <- Comm_DBI_R_RT_S_GC_major |> 
  group_by(Variables_Label, Gut_Content, Group) |> 
  summarise(mean_DBI_Category = mean(DBI_Category))

mean_DBI_Category_dat_major |> 
  distinct(Variables_Label) |> 
  print(n = Inf)

Variables_Label_order <- c("Anacapa Island Natural F",
                           "Anacapa Island Natural M",   
                           "Santa Cruz Island Natural F",    
                           "Santa Cruz Island Natural M", 
                           "Santa Barbara Island Natural F", 
                           "Santa Barbara Island Natural M", 
                           "Santa Catalina Island Natural F",
                           "Santa Catalina Island Natural M",
                           "San Clemente Island Natural F",   
                           "San Clemente Island Natural M",
                           "North Palos Verdes Natural F",  
                           "North Palos Verdes Natural M",
                           "South Palos Verdes Natural F",   
                           "South Palos Verdes Natural M", 
                           "Orange County Natural F",        
                           "Orange County Natural M",  
                           "North San Diego Natural F",      
                           "North San Diego Natural M",  
                           "South San Diego Natural F",     
                           "South San Diego Natural M",
                           "North Palos Verdes Artificial F",
                           "North Palos Verdes Artificial M",
                           "South Palos Verdes Artificial F",
                           "South Palos Verdes Artificial M",
                           "Orange County Artificial F",     
                           "Orange County Artificial M",
                           "North San Diego Artificial F",   
                           "North San Diego Artificial M",
                           "South San Diego Artificial F",   
                           "South San Diego Artificial M")

#creating vector of order so variables label from north to south 
mean_DBI_Category_dat_major$Variables_Label = factor(mean_DBI_Category_dat_major$Variables_Label, 
                                                     levels = rev(unique(Variables_Label_order)))

#ordering locations for plotting based on cluster groups
Loc_order <- c("Anacapa Island Natural F",
               "Anacapa Island Natural M",   
               "Santa Cruz Island Natural F",    
               "Santa Cruz Island Natural M",
               "North Palos Verdes Artificial F",
               "North Palos Verdes Artificial M",
               "South Palos Verdes Artificial F",
               "South Palos Verdes Artificial M",
               "Orange County Artificial F",     
               "Orange County Artificial M",
               "North San Diego Artificial F",   
               "North San Diego Artificial M",
               "South San Diego Artificial F",   
               "South San Diego Artificial M",
               "Santa Barbara Island Natural F", 
               "Santa Barbara Island Natural M", 
               "Santa Catalina Island Natural F",
               "Santa Catalina Island Natural M",
               "San Clemente Island Natural F",   
               "San Clemente Island Natural M",
               "North Palos Verdes Natural F",  
               "North Palos Verdes Natural M",
               "South Palos Verdes Natural F",   
               "South Palos Verdes Natural M", 
               "Orange County Natural F",        
               "Orange County Natural M",  
               "South San Diego Natural F",     
               "South San Diego Natural M",
               "North San Diego Natural F",      
               "North San Diego Natural M")

mean_DBI_Category_dat_major$Variables_Label = factor(mean_DBI_Category_dat_major$Variables_Label, levels = rev(unique(Loc_order)))

Group_orders <- c("NSD",
                  "Mainland NRs",
                  "Warm Islands",
                  "SBI",
                  "Mainland ARs",
                  "Cool Islands")

mean_DBI_Category_dat_major$Group = factor(mean_DBI_Category_dat_major$Group, levels = rev(unique(Group_orders)))

#Barplot with all of the variables from the NMDS (Region, ReefType, and Sex)
barplot_RC_RT_S_SC_major <- mean_DBI_Category_dat_major |>
  ggplot( aes(x = Variables_Label, 
              y = mean_DBI_Category, 
              fill = Gut_Content)) +
  geom_bar(stat = "identity", 
           colour = "black", 
           position = "fill", 
           width = 0.75) +   #show_guide=FALSE
  guides(fill = guide_legend(reverse = T)) +  # change order of GC in legend 
  facet_grid(Group ~., scales = "free", space = "free") +
  coord_flip() +
  labs(x = " ", 
       y = "Average Consolidated DBI Proportion in Diet") +
  theme_bw() +
  theme(axis.text = element_text(size = 13),
        legend.text = element_text(size = 13),
        axis.title = element_text(size = 15)) +
  theme(legend.title = element_blank()) +
  theme(strip.text = element_text(size = 11),
        strip.background = element_rect(color = "black", fill = "white")) +
  scale_fill_manual(values=c("Animal"="cadetblue",
                             "Algae"="chartreuse4"))
barplot_RC_RT_S_SC_major

# ggsave("barplot_RC_RT_S_SC_major.png", barplot_RC_RT_S_SC_major,
#        width = 10,
#        height = 9, 
#        dpi = 600)

## DBI by diet groups ####
group_barplot <- DBI_R_RT_S_GC |> 
  mutate(Variables_Label = paste(Region, ReefType)) |> 
  mutate(Group = recode(Variables_Label,
                        'Anacapa Island Natural' = "Group 1",
                        'Santa Cruz Island Natural' = "Group 1",
                        'North Palos Verdes Artificial' = "Group 2",
                        'South Palos Verdes Artificial' = "Group 2",
                        'Orange County Artificial' = "Group 2",
                        'North San Diego Artificial' = "Group 2",
                        'South San Diego Artificial' = "Group 2",
                        'Santa Barbara Island Natural' = "SBI",
                        'Santa Catalina Island Natural' = "Group 3",
                        'San Clemente Island Natural' = "Group 3",
                        'North Palos Verdes Natural' = "Group 4",
                        'South Palos Verdes Natural' = "Group 4",
                        'Orange County Natural' = "Group 4",
                        'South San Diego Natural' = "Group 4",
                        'North San Diego Natural' = "NSD"),
         Major_Gut_Content = recode(Gut_Content, 
                                    'Anemones' ="Animal",
                                    'Encrusting Bryozoans' ="Animal", 
                                    'Brittle Stars'="Animal", 
                                    'Brown Algae'="Algae", 
                                    'Branching Bryozoans'="Animal", 
                                    'Coralline Red Algae'="Algae", 
                                    'Crustaceans'="Animal", 
                                    'Garibaldi Eggs'="Animal", 
                                    'Gorgonians'="Animal",
                                    'Green Algae'="Algae", 
                                    'Hydroids'="Animal",
                                    'Mollusks' = "Animal",
                                    'Polychaete Worms' = "Animal",
                                    'Fleshy Red Algae' = "Algae",
                                    'Sponge' = "Animal",
                                    'Tunicates' = "Animal"))

group_barplot <- group_barplot |> 
  group_by(Group, Gut_Content, Major_Gut_Content) |> 
  summarise(mean_DBI_Category = mean(DBI_Category))

#creating vector of order for groups
Group_order <- c("Group 1",
                 "Group 2",
                 "SBI",
                 "Group 3",
                 "Group 4",
                 "NSD")

group_barplot$Group = factor(group_barplot$Group, levels = rev(unique(Group_order)))

group_barplot_plot <- group_barplot |> 
  ggplot(aes(x = Group,
             y = mean_DBI_Category, 
             fill = Gut_Content, 
             pattern = Major_Gut_Content)) +
  geom_bar_pattern(stat="identity", 
           position = "fill", 
           width = .7, 
           colour = "black",
           pattern_color = 'black', 
           pattern_fill = 'black', 
           pattern_density = 0.1,
           pattern_spacing = 0.025) +   #show_guide=FALSE
  scale_pattern_manual(values = c(Algae = "stripe", Animal = "none"),
                       guide = "none") +
  guides(fill = guide_legend(reverse = T, 
                             override.aes = list(pattern = c("none", "none","none", "none", "none", "none","none", "none","none", "none", "none", "none", "stripe", "stripe", "stripe", "stripe")))) +  # change order of GC in legend 
  coord_flip() +
  theme_bw() +
  labs(x = " ", 
       y = "Average DBI Proportion in Diet") +
  scale_x_discrete(labels=c("N. San Diego NR","Mainland NRs", 
                            "Warm Islands","Santa Barbara Island", 
                            "Mainland ARs", "Cool Islands")) +
  theme(legend.title = element_blank()) +
  theme(axis.text = element_text(size = 13),
        legend.text = element_text(size = 13),
        axis.title = element_text(size = 15)) +
  scale_fill_manual(values = (my_cols)) 

group_barplot_plot

# ggsave("group_barplot_plot.png", group_barplot_plot,
#        height = 5,
#        width = 7,
#        dpi = 600)

## Individual proportions of each category ####
Barplot_all <- DBI_R_RT_S_GC |> 
  mutate(Rg_RT = paste(Island_Mainland, ReefType)) |> 
  group_by(Gut_Content) |> 
  summarize(mean_DBI_Category = ((mean(DBI_Category))))

Barplot_all <- Barplot_all |> 
  mutate(Prop = mean_DBI_Category / sum(mean_DBI_Category),
         Major_Gut_Content = recode(Gut_Content, 
                                    'Anemones' ="Animal",
                                    'Encrusting Bryozoans' ="Animal", 
                                    'Brittle Stars'="Animal", 
                                    'Brown Algae'="Algae", 
                                    'Branching Bryozoans'="Animal", 
                                    'Coralline Red Algae'="Algae", 
                                    'Crustaceans'="Animal", 
                                    'Garibaldi Eggs'="Animal", 
                                    'Gorgonians'="Animal",
                                    'Green Algae'="Algae", 
                                    'Hydroids'="Animal",
                                    'Mollusks' = "Animal",
                                    'Polychaete Worms' = "Animal",
                                    'Fleshy Red Algae' = "Algae",
                                    'Sponge' = "Animal",
                                    'Tunicates' = "Animal"))

Barplot_all.order <- c("Tunicates", "Green Algae", "Gorgonians",
                       "Brittle Stars",  "Crustaceans", "Coralline Red Algae",
                       "Garibaldi Eggs", "Brown Algae", "Mollusks", 
                       "Branching Bryozoans", "Hydroids", "Encrusting Bryozoans",
                       "Sponge", "Anemones", "Polychaete Worms", "Fleshy Red Algae")

Barplot_all_plot <- Barplot_all |>   
  ggplot(aes(x = Gut_Content, 
             y = Prop, 
             fill = Gut_Content,
             pattern = Major_Gut_Content)) +
  geom_bar_pattern(stat="identity", 
           position = "dodge", 
           width = .7, 
           colour = "black",
           pattern_color = 'black', 
           pattern_fill = 'black', 
           pattern_density = 0.1,
           pattern_spacing = 0.025) +   #show_guide=FALSE
  scale_pattern_manual(values = c(Algae = "stripe", Animal = "none"),
                       guide = "none") +
  coord_flip() +
  theme_bw() +
  labs(x = " ", 
       y = "Average DBI Proportion in Diet") +
  scale_x_discrete(limits = Barplot_all.order) +
  theme(legend.position = 'none',
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 13),
        axis.title = element_text(size = 15)) +
  scale_fill_manual(values = (my_cols)) 

print(Barplot_all_plot)

# ggsave("Barplot_all+plot.png", Barplot_all_plot,
#        width = 7,
#        height = 5, 
#        dpi = 600)

#Dendrogram --------------------------------------------------------

# Life history parameters table 
LH <- LH_parameters |>
  mutate(Variables_Label = paste(Region, ReefType, Sex)) |>
  mutate(Island_Mainland = recode(Region,
                                  "Anacapa & Santa Cruz Islands"= 'Island',
                                  "Santa Barbara Island"= 'Island',
                                  "Santa Catalina Island"= 'Island',
                                  "San Clemente Island" = 'Island',
                                  "North Palos Verdes"= 'Mainland',
                                    "South Palos Verdes"= 'Mainland',
                                    "Orange County"= 'Mainland',
                                    "North San Diego"= 'Mainland',
                                    "South San Diego" = 'Mainland')) |>
  mutate(IM_RT = paste(Island_Mainland, ReefType))

# Calculate mean condition factor (K) by
Mean_Fultons_K <- Dat |>
  select(CN, Region, ReefType, Sex, Fultons_K) |>
  distinct() |>
  mutate(Region = recode(Region,
                         "Anacapa Island" = "Anacapa & Santa Cruz Islands",
                         "Santa Cruz Island" = "Anacapa & Santa Cruz Islands")) |>
  group_by(Region, ReefType, Sex) |>
  filter(Sex != "U") |> #Remove Sex = U
  summarise(mean_Fultons_K = (mean(Fultons_K,
                                   na.rm=T))) |>
    ungroup()

# Guts by sex 
#Create a distance matrix based on the community assemblages
dis.Comm_Gut_Wide_R_Rt_S <- vegdist(Comm_Gut_Wide_R_RT_S, method = "bray")

#Create a cluster dendrogram
clust.Comm_Gut_Wide_R_Rt_S <- hclust(dis.Comm_Gut_Wide_R_Rt_S, "average")

## !FINAL: dendrogram ####
dend <- as.dendrogram(clust.Comm_Gut_Wide_R_Rt_S)
par(mar=c(5,1,1,12))

plot(rotate(dend, c(1:15, 20, 24, 26:27, 25, 29:30, 28, 23:21, 16:17, 18:19)), 
     horiz = T,
     xlab = "Dissimilarity",
     xlim = c(0.4, 0))

# Color the branches based on height
dend_colored <- dend %>% color_branches(k = 5)  # You can adjust the number of colors (k) as needed

# Plot the colored dendrogram
plot(rotate(dend_colored, c(1:15, 20, 24, 26:27, 25, 29:30, 28, 23:21, 16:17, 18:19)), 
     horiz = T,
     xlab = "Dissimilarity",
     xlim = c(0.4, 0))

# PCoA --------------------------------------------------------------------

# Make only M and only F tables, then do PCoA for each
# extract Axis 1 as x-axis, plot vs. y-axis each life history parameter

# create gut contents community data using pivot_wider for only males or females (Region, ReefType)
# Average + sqrt across CNs by Reg_code, ReefType and Sex for each Gut_Content
Dat_AISCRI_R_RT_S_GC <- Dat_AISCRI |>
  group_by(Region, ReefType, Sex, Gut_Content, IM_RT) |>
  summarize(mean_prop_DW_mg_body_wt_g = (mean(prop_DW_mg_body_wt_g)),
            RegionSST = mean(RegionSST)) |>
  mutate(sqrt_mean_prop_DW_mg_body_wt_g = mean_prop_DW_mg_body_wt_g^0.5) |>  #sqrt transformed
  filter(Sex != "U")

# Gut_Wide_AISCRI_R_RT_S <- Dat_AISCRI_R_RT_S_GC |>
#   mutate(Variables_Label = paste(Region, ReefType, Sex)) |>
#   pivot_wider(id_cols = c(Variables_Label, Region, ReefType, IM_RT, RegionSST),
#               names_from = Gut_Content, 
#               values_from = sqrt_mean_prop_DW_mg_body_wt_g)
# 
# Comm_Gut_Wide_AISCRI_R_RT_S <- Gut_Wide_AISCRI_R_RT_S|> 
#   column_to_rownames(var = "Variables_Label") |>
#   select(-Region, -ReefType, -IM_RT, -RegionSST) 

#males
Gut_Wide_AISCRI_R_RT_M <- Dat_AISCRI_R_RT_S_GC |>
  filter(Sex == "M") |> 
  mutate(Variables_Label = paste(Region, ReefType)) |>
  pivot_wider(id_cols = c(Variables_Label, Region, ReefType, IM_RT, RegionSST),
              names_from = Gut_Content, 
              values_from = sqrt_mean_prop_DW_mg_body_wt_g)

# make "community data" table format to go into vegan package functions - guts, only males
Comm_Gut_Wide_AISCRI_R_RT_M <- Gut_Wide_AISCRI_R_RT_M |> 
  column_to_rownames(var = "Variables_Label") |>
  select(-Region, -ReefType, -IM_RT, -RegionSST)  #remove the columns to get into comm format 


Gut_Wide_AISCRI_R_RT_F <- Dat_AISCRI_R_RT_S_GC |>
  filter(Sex == "F") |> 
  mutate(Variables_Label = paste(Region, ReefType)) |>
  pivot_wider(id_cols = c(Variables_Label, Region, ReefType, IM_RT, RegionSST),
              names_from = Gut_Content , 
              values_from = sqrt_mean_prop_DW_mg_body_wt_g)

# make "community data" table format to go into vegan package functions - guts, only females
Comm_Gut_Wide_AISCRI_R_RT_F <- Gut_Wide_AISCRI_R_RT_F|> 
  column_to_rownames(var = "Variables_Label") |>
  select(-Region, -ReefType, -IM_RT, -RegionSST) 

#LH for sexes combined
LH_parameters_all <- LH_parameters |> 
  mutate(Variables_Label = paste(Region, ReefType)) |> 
  mutate(Island_Mainland = recode(Region,
                                  "Anacapa & Santa Cruz Islands"= 'Island', 
                                  "Santa Barbara Island"= 'Island', 
                                  "Santa Catalina Island"= 'Island', 
                                  "San Clemente Island" = 'Island',
                                  "North Palos Verdes"= 'Mainland', 
                                  "South Palos Verdes"= 'Mainland', 
                                  "Orange County"= 'Mainland', 
                                  "North San Diego"= 'Mainland', 
                                  "South San Diego" = 'Mainland')) |> 
  mutate(IM_RT = paste(Island_Mainland, ReefType))

LH_parameters_all <- LH_parameters_all |> 
  left_join(Mean_Fultons_K)

LH_parameters_all <- LH_parameters_all |> 
  mutate(Location_Code = recode(Variables_Label,
                              'Anacapa & Santa Cruz Islands Natural' = "AISCRI",
                              'North Palos Verdes Artificial' = "NPV",
                              'North Palos Verdes Natural' = "NPV",
                              'North San Diego Artificial' = "NSD",
                              'North San Diego Natural' = "NSD",
                              'Orange County Artificial' = "OC",
                              'Orange County Natural' = "OC",
                              'San Clemente Island Natural' = "SCLI",
                              'Santa Barbara Island Natural' = "SBI",
                              'Santa Catalina Island Natural' = "SCI",
                              'South Palos Verdes Artificial' = "SPV",
                              'South Palos Verdes Natural' = "SPV",
                              'South San Diego Artificial' = "SSD",
                              'South San Diego Natural' = "SSD"))

# LH for malees and females only, separately
# males - preparing data
LH_M <- LH_parameters |> 
  filter(Sex == "M") |> 
  mutate(Variables_Label = paste(Region, ReefType)) |> 
  mutate(Island_Mainland = recode(Region,
                                  "Anacapa & Santa Cruz Islands"= 'Island', 
                                  "Santa Barbara Island"= 'Island', 
                                  "Santa Catalina Island"= 'Island', 
                                  "San Clemente Island" = 'Island',
                                  "North Palos Verdes"= 'Mainland', 
                                  "South Palos Verdes"= 'Mainland', 
                                  "Orange County"= 'Mainland', 
                                  "North San Diego"= 'Mainland', 
                                  "South San Diego" = 'Mainland')) |> 
  mutate(IM_RT = paste(Island_Mainland, ReefType))

LH_M <- LH_M |> 
  left_join(Mean_Fultons_K)

LH_M <- LH_M |> 
  mutate(Location_Code = recode(Variables_Label,
                              'Anacapa & Santa Cruz Islands Natural' = "AISCRI",
                              'North Palos Verdes Artificial' = "NPV",
                              'North Palos Verdes Natural' = "NPV",
                              'North San Diego Artificial' = "NSD",
                              'North San Diego Natural' = "NSD",
                              'Orange County Artificial' = "OC",
                              'Orange County Natural' = "OC",
                              'San Clemente Island Natural' = "SCLI",
                              'Santa Barbara Island Natural' = "SBI",
                              'Santa Catalina Island Natural' = "SCI",
                              'South Palos Verdes Artificial' = "SPV",
                              'South Palos Verdes Natural' = "SPV",
                              'South San Diego Artificial' = "SSD",
                              'South San Diego Natural' = "SSD"))

# females - preparing data
LH_F <- LH_parameters |> 
  filter(Sex == "F") |> 
  mutate(Variables_Label = paste(Region, ReefType)) |> 
  mutate(Island_Mainland = recode(Region,
                                  "Anacapa & Santa Cruz Islands"= 'Island', 
                                  "Santa Barbara Island"= 'Island', 
                                  "Santa Catalina Island"= 'Island', 
                                  "San Clemente Island" = 'Island',
                                  "North Palos Verdes"= 'Mainland', 
                                  "South Palos Verdes"= 'Mainland', 
                                  "Orange County"= 'Mainland', 
                                  "North San Diego"= 'Mainland', 
                                  "South San Diego" = 'Mainland')) |> 
  mutate(IM_RT = paste(Island_Mainland, ReefType))

LH_F <- LH_F |> 
  left_join(Mean_Fultons_K)

LH_F <- LH_F |> 
  mutate(Location_Code = recode(Variables_Label,
                              'Anacapa & Santa Cruz Islands Natural' = "AISCRI",
                              'North Palos Verdes Artificial' = "NPV",
                              'North Palos Verdes Natural' = "NPV",
                              'North San Diego Artificial' = "NSD",
                              'North San Diego Natural' = "NSD",
                              'Orange County Artificial' = "OC",
                              'Orange County Natural' = "OC",
                              'San Clemente Island Natural' = "SCLI",
                              'Santa Barbara Island Natural' = "SBI",
                              'Santa Catalina Island Natural' = "SCI",
                              'South Palos Verdes Artificial' = "SPV",
                              'South Palos Verdes Natural' = "SPV",
                              'South San Diego Artificial' = "SSD",
                              'South San Diego Natural' = "SSD"))


# M pcoa using function in ape package
pcoa_input_M <- vegdist(Comm_Gut_Wide_AISCRI_R_RT_M, "bray")
pcoa_M <- pcoa(pcoa_input_M)
biplot(pcoa_M, Comm_Gut_Wide_AISCRI_R_RT_M, 
       dir.axis1 = -1)

tibble.pcoa_M <- as_tibble(scores(pcoa_M$vectors), 
                           rownames = "Variables_Label")

#add variables and values to use in regression plots 
tibble.pcoa_M <- tibble.pcoa_M |> 
  left_join(LH_M)

tibble.pcoa_M <- tibble.pcoa_M %>% 
  mutate(Axis.1_flipped = (Axis.1*-1))

# F pcoa using function in ape package
pcoa_input_F <- vegdist(Comm_Gut_Wide_AISCRI_R_RT_F, "bray")
pcoa_F <- pcoa(pcoa_input_F)
biplot(pcoa_F, Comm_Gut_Wide_AISCRI_R_RT_F)

tibble.pcoa_F <- as_tibble(scores(pcoa_F$vectors), 
                           rownames = "Variables_Label")

#add variables and values to use in regression plots
tibble.pcoa_F <- tibble.pcoa_F |> 
  left_join(LH_F)

tibble.pcoa_F <- tibble.pcoa_F |> 
  mutate(Axis.1_flipped = Axis.1*-1)

# Regression plots with PCoA1 and temp --------------------------------------------------------
#temp vs pcoa females
SST_F_plot <- ggplot(tibble.pcoa_F, aes(Region_SST, Axis.1_flipped, 
                                      color = IM_RT)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = F, 
              aes(group = 1), 
              color = "black") +
  geom_text_repel(aes(label = Location_Code)) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  scale_color_manual(values = c("Island Natural" = "lightseagreen", 
                                "Mainland Natural" = "forestgreen", 
                                "Mainland Artificial" = "blue4"))

SST_F_plot
# Fit linear model for females temp
lm_SST_F <- lm(Axis.1_flipped ~ Region_SST, data = tibble.pcoa_F)
# extra model summary in tidy format w/ 95% CIs of slope
SST_parms_F <- tidy(summary(lm_SST_F), conf.int = TRUE) |> 
  mutate(Sex = "F") |> 
  mutate(Parm = "SST")

#temp vs pcoa males
SST_M_plot <- ggplot(tibble.pcoa_M, aes(Region_SST, Axis.1,
                                        color = IM_RT)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = F, 
              aes(group = 1), 
              color = "black") +
  geom_text_repel(aes(label = Location_Code)) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  scale_color_manual(values = c("Island Natural" = "lightseagreen", 
                                "Mainland Natural" = "forestgreen", 
                                "Mainland Artificial" = "blue4"))

SST_M_plot
# Fit linear model for females temp
lm_SST_M <- lm(Axis.1 ~ Region_SST, data = tibble.pcoa_M)
# extra model summary in tidy format w/ 95% CIs of slope
SST_parms_M <- tidy(summary(lm_SST_M), conf.int = TRUE) |> 
  mutate(Sex = "M") |> 
  mutate(Parm = "SST")

SST_pcoa_table <- bind_rows(SST_parms_F, SST_parms_M) |> 
  filter(term != "(Intercept)")

SST_pcoa_table

# Regression plots with PCoA1 --------------------------------------------------------
## F: k vs pcoa ####
k_F_plot <- ggplot(tibble.pcoa_F, aes(Axis.1_flipped, k, 
                                      color = IM_RT)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = F, 
              aes(group = 1), 
              color = "black") +
  geom_text_repel(aes(label = Location_Code)) +
  geom_text(x=0.2, y=0.28, label="B", color="black", size=4.5) +
  ylim(c(0.10,0.28)) +
  xlim(c(-0.27 ,0.2)) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  scale_color_manual(values = c("Island Natural" = "lightseagreen", 
                                "Mainland Natural" = "forestgreen", 
                                "Mainland Artificial" = "blue4"))

k_F_plot

# Fit linear model for females k
lm_k_F <- lm(k ~ Axis.1_flipped, data = tibble.pcoa_F)
# extra model summary in tidy format w/ 95% CIs of slope
k_parms_F <- tidy(summary(lm_k_F), conf.int = TRUE) |> 
  mutate(Sex = "F") |> 
  mutate(Parm = "k")

## F: Linf vs pcoa1 ####
Linf_F_plot <- ggplot(tibble.pcoa_F, aes(Axis.1_flipped, linf, 
                                         color = IM_RT)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = F, 
              aes(group = 1), 
              color = "black") +
  geom_text(x=0.2, y=300, label="C", color="black", size=4.5) +
  geom_text_repel(aes(label = Location_Code)) +
  ylab("Linf") +
  ylim(c(230,302)) +
  xlim(c(-0.27 ,0.2)) +
  labs(y = expression(L[infinity])) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  scale_color_manual(name = "Reef Type", 
                     values = c("Island Natural" = "lightseagreen", 
                                "Mainland Natural" = "forestgreen", 
                                "Mainland Artificial" = "blue4"))

Linf_F_plot

# Fit linear model for females Linf
lm_Linf_F <- lm(linf ~ Axis.1_flipped, data = tibble.pcoa_F)
# extra model summary in tidy format w/ 95% CIs of slope
Linf_parms_F <- tidy(summary(lm_Linf_F), conf.int = TRUE) |> 
  mutate(Sex = "F") |> 
  mutate(Parm = "Linf")

## F: FK vs pcoa1 ####
FK_F_plot <- ggplot(tibble.pcoa_F, aes(Axis.1_flipped, mean_Fultons_K, 
                                       color = IM_RT)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = F, 
              aes(group = 1), 
              color = "black") +
  geom_text_repel(aes(label = Location_Code)) +
  geom_text(x=0.2, y=0.42, label="A", color="black", size=4.5) +
  ylab("Mean Fulton's K") +
  ylim(c(0.25, 0.42)) +
  xlim(c(-0.27, 0.2)) +
  theme_classic() +
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  scale_color_manual(values = c("Island Natural" = "lightseagreen", 
                                "Mainland Natural" = "forestgreen", 
                                "Mainland Artificial" = "blue4"))

FK_F_plot

# Fit linear model for females FK
lm_FK_F <- lm(mean_Fultons_K ~ Axis.1_flipped, data = tibble.pcoa_F)
# extra model summary in tidy format w/ 95% CIs of slope
FK_parms_F <- tidy(summary(lm_FK_F), conf.int = TRUE) |> 
  mutate(Sex = "F")|> 
  mutate(Parm = "FK")

summary(lm(mean_Fultons_K ~ Axis.1_flipped, data = tibble.pcoa_F))

## F: PTL10 vs pcoa1 ####
Predicted_Parameters_10_F <- Predicted_Parameters |> 
  filter(Age_yr == 10,
         Sex == "F") |> 
  mutate(Variables_Label = paste(Region, ReefType))

tibble.pcoa_F_Pred_10 <- tibble.pcoa_F |> 
  left_join(Predicted_Parameters_10_F)

PTL_F_10_plot <- ggplot(tibble.pcoa_F_Pred_10, aes(Axis.1_flipped, Pred_TL, 
                                                 color = IM_RT)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = F, 
              aes(group = 1), 
              color = "black") +
  geom_text(x=0.2, y=270, label="D", color="black", size=4.5) +
  geom_text_repel(aes(label = Location_Code)) +
  ylim(c(190, 270)) +
  ylab("Pred. TL at Age 10") +
  xlab("PCoA 1") +
  xlim(c(-0.27 ,0.2)) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("Island Natural" = "lightseagreen", 
                                "Mainland Natural" = "forestgreen", 
                                "Mainland Artificial" = "blue4"))
PTL_F_10_plot

# Fit linear model for females PTL
lm_PTL_F <- lm(Pred_TL ~ Axis.1_flipped, data = tibble.pcoa_F_Pred_10)
# extra model summary in tidy format w/ 95% CIs of slope
PTL_parms_F <- tidy(summary(lm_PTL_F), conf.int = TRUE) |> 
  mutate(Sex = "F") |> 
  mutate(Parm = "PTL10")

## F: combined regression plots ####
F_regressions_plot <- plot_grid(FK_F_plot,
                                k_F_plot, 
                                Linf_F_plot, 
                                PTL_F_10_plot,
                                ncol = 1, 
                                align = "v",
                                axis = "rl")
F_regressions_plot

## M: k vs pcoa1 ####
k_M_plot <- ggplot(tibble.pcoa_M, aes(Axis.1, k, 
                                      color = IM_RT)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = F, 
              aes(group=1), 
              color = "black") +
  geom_text(x=0.21, y=0.28, label="F", color="black", size=4.5) +
  geom_text_repel(aes(label = Location_Code)) +
  ylim(c(0.10,0.28)) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  scale_color_manual(values = c("Island Natural" = "lightseagreen", 
                                "Mainland Natural" = "forestgreen", 
                                "Mainland Artificial" = "blue4"))
k_M_plot

# Fit linear model for males k
lm_k_M <- lm(k ~ Axis.1, data = tibble.pcoa_M)
# extra model summary in tidy format w/ 95% CIs of slope
k_parms_M <- tidy(summary(lm_k_M), conf.int = TRUE) |> 
  mutate(Sex = "M") |> 
  mutate(Parm = "k")

## M: Linf vs pcoa1 ####
Linf_M_plot <- ggplot(tibble.pcoa_M, aes(Axis.1, linf, 
                                         color = IM_RT)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = F, 
              aes(group = 1), 
              color = "black") +
  geom_text(x=0.21, y=302, label="G", color="black", size=4.5) +
  geom_text_repel(aes(label = Location_Code)) +
  ylab("Linf") +
  ylim(c(230,302)) +
  labs(y = expression(L[infinity])) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), 
        legend.position = "none") +
  scale_color_manual(name = "Reef Type", 
                     values = c("Island Natural" = "lightseagreen", 
                                "Mainland Natural" = "forestgreen", 
                                "Mainland Artificial" = "blue4"))
Linf_M_plot

# Fit linear model for males Linf
lm_Linf_M <- lm(linf ~ Axis.1, data = tibble.pcoa_M)
# extra model summary in tidy format w/ 95% CIs of slope
Linf_parms_M <- tidy(summary(lm_Linf_M), conf.int = TRUE) |> 
  mutate(Sex = "M") |> 
  mutate(Parm = "Linf")

## M: FK vs pcoa1 ####
FK_M <- ggplot(tibble.pcoa_M, aes(Axis.1, mean_Fultons_K, 
                                  color = IM_RT)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = F, 
              aes(group = 1), 
              color = "black") +
  geom_text(x=0.21, y=0.42, label="E", color="black", size=4.5) +
  geom_text_repel(aes(label = Location_Code)) +
  ylab("Mean Fulton's K") +
  ylim(c(0.25, 0.42)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_color_manual(values = c("Island Natural" = "lightseagreen", 
                                "Mainland Natural" = "forestgreen", 
                                "Mainland Artificial" = "blue4"))

FK_M

# Fit linear model for males FK
lm_FK_M <- lm(mean_Fultons_K ~ Axis.1, data = tibble.pcoa_M)
# extra model summary in tidy format w/ 95% CIs of slope
FK_parms_M <- tidy(summary(lm_FK_M), conf.int = TRUE) |> 
  mutate(Sex = "M") |> 
  mutate(Parm = "FK")


## M: PTL10 vs pcoa1 ####
Predicted_Parameters_10_M <- Predicted_Parameters |> 
  filter(Age_yr == 10,
         Sex == "M") |> 
  mutate(Variables_Label = paste(Region, ReefType))

tibble.pcoa_M_Pred_10 <- tibble.pcoa_M |> 
  left_join(Predicted_Parameters_10_M)

PTL_M_10_plot <- ggplot(tibble.pcoa_M_Pred_10, aes(Axis.1, Pred_TL, 
                                                   color = IM_RT)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = F, 
              aes(group = 1), 
              color = "black") +
  geom_text(x=0.21, y=270, label="H", color="black", size=4.5) +
  geom_text_repel(aes(label = Location_Code)) +
  ylim(c(190, 270)) +
  ylab("Pred. TL at Age 10") +
  xlab("PCoA 1") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("Island Natural" = "lightseagreen", 
                                "Mainland Natural" = "forestgreen", 
                                "Mainland Artificial" = "blue4"))

PTL_M_10_plot

# Fit linear model for males FK
lm_PTL_M <- lm(Pred_TL ~ Axis.1, data = tibble.pcoa_M_Pred_10)
# extra model summary in tidy format w/ 95% CIs of slope
PTL_parms_M <- tidy(summary(lm_PTL_M), conf.int = TRUE) |> 
  mutate(Sex = "M") |> 
  mutate(Parm = "PTL10")

## M: combined regression plots ####
M_regressions_plot <- plot_grid(FK_M,
                                k_M_plot, 
                                Linf_M_plot,
                                PTL_M_10_plot,
                                ncol=1,
                                align = "v",
                                axis = "rl")
M_regressions_plot

## Final combined regression plots with pcoa1 ####
F.M_regression_plot <- plot_grid(F_regressions_plot, 
                                 M_regressions_plot, 
                                 ncol = 2, 
                                 align = "h")
F.M_regression_plot
# ggsave("F.M_regression_plot.png", F.M_regression_plot,
#        width = 8, 
#        height = 10, 
#        dpi = 600)

# combined coef tables, filter for just slope parameters (don't need y-intercepts)
LH_pcoa_parms_table <- bind_rows(k_parms_F, Linf_parms_F, FK_parms_F, PTL_parms_F, 
                            k_parms_M, Linf_parms_M, FK_parms_M, PTL_parms_M) |> 
  filter(term != "(Intercept)")

LH_pcoa_parms_table

FM_pcoa_LH_parms <- flextable(LH_pcoa_parms_table)
FM_pcoa_LH_parms <- theme_vanilla(FM_pcoa_LH_parms)
FM_pcoa_LH_parms <- set_header_labels(FM_pcoa_LH_parms,
                                   term = NA,
                                   estimate = "Slope",
                                   statistic = "F",
                                   p.value = "Pr(>F)")
FM_pcoa_LH_parms <- align(FM_pcoa_LH_parms, 
                     part = "header", 
                     align = "center")
FM_pcoa_LH_parms <- bold(FM_pcoa_LH_parms, ~ p.value <0.05, ~ p.value, 
                    bold = TRUE)
FM_pcoa_LH_parms <- set_table_properties(FM_pcoa_LH_parms, 
                                    layout = "autofit")

FM_pcoa_LH_parms

save_as_docx(FM_pcoa_LH_parms, path = "FM_pcoa_LH_parms.docx")

# Regression plots with Animal ####
# filtering for females
Dat_Major_F <- Dat_Major |> 
  mutate(Region = recode(Region,
                         "Anacapa Island" = "Anacapa & Santa Cruz Islands",
                         "Santa Cruz Island" = "Anacapa & Santa Cruz Islands")) |> 
  filter(Sex == "F") |> 
  mutate(Variables_Label = paste(Region, ReefType))

Dat_Major_F <- Dat_Major_F |> 
  group_by(Gut_Content, Variables_Label, IM_RT) |> 
  summarise(mean_prop_DW_mg_body_wt_g = mean(prop_DW_mg_body_wt_g))

Dat_Major_LH_F <- Dat_Major_F |> 
  filter(Gut_Content == "Animal") |> 
  left_join(LH_F)

# filtering for males
Dat_Major_M <- Dat_Major |> 
  mutate(Region = recode(Region,
                         "Anacapa Island" = "Anacapa & Santa Cruz Islands",
                         "Santa Cruz Island" = "Anacapa & Santa Cruz Islands")) |> 
  filter(Sex == "M") |> 
  mutate(Variables_Label = paste(Region, ReefType))

Dat_Major_M <- Dat_Major_M |> 
  group_by(Gut_Content, Variables_Label, IM_RT) |> 
  summarise(mean_prop_DW_mg_body_wt_g = mean(prop_DW_mg_body_wt_g))

Dat_Major_LH_M <- Dat_Major_M |> 
  filter(Gut_Content == "Animal") |> 
  left_join(LH_M)

### F: k vs animal ####
k_F_major_plot <- ggplot(Dat_Major_LH_F, aes(mean_prop_DW_mg_body_wt_g, k, 
                                             color = IM_RT)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = F, 
              aes(group = 1), 
              color = "black") +
  geom_text(x=0.83, y=0.27, label="A", color="black", size=4.5) +
  geom_text_repel(aes(label = Location_Code)) +
  ylab("k") +
  ylim(c(0.1, 0.27)) +
  theme_classic() +
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_color_manual(values = c("Island Natural" = "lightseagreen", 
                                "Mainland Natural" = "forestgreen", 
                                "Mainland Artificial" = "blue4"))
k_F_major_plot

# Fit linear model for females k by animal weight
lm_k_anwt_F <- lm(k ~ mean_prop_DW_mg_body_wt_g, data = Dat_Major_LH_F)
# extra model summary in tidy format w/ 95% CIs of slope
k_anwt_parms_F <- tidy(summary(lm_k_anwt_F), conf.int = TRUE) |> 
  mutate(Sex = "F") |> 
  mutate(Parm = "k")

### F: Linf vs animal ####
Linf_F_major_plot <- ggplot(Dat_Major_LH_F, aes(mean_prop_DW_mg_body_wt_g, linf, 
                                                color = IM_RT)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = F, 
              aes(group = 1), 
              color = "black") +
  geom_text(x=0.83, y=302, label="B", color="black", size=4.5) +
  geom_text_repel(aes(label = Location_Code)) +
  ylab("Linf") +
  ylim(c(230, 302)) +
  labs(y = expression(L[infinity])) +
  xlab("Mean Animal Dry Weight (g)") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_color_manual(values = c("Island Natural" = "lightseagreen", 
                                "Mainland Natural" = "forestgreen", 
                                "Mainland Artificial" = "blue4"))
Linf_F_major_plot

# Fit linear model for females linf by animal weight
lm_Linf_anwt_F <- lm(linf ~ mean_prop_DW_mg_body_wt_g, data = Dat_Major_LH_F)
# extra model summary in tidy format w/ 95% CIs of slope
Linf_anwt_parms_F <- tidy(summary(lm_Linf_anwt_F), conf.int = TRUE) |> 
  mutate(Sex = "F") |> 
  mutate(Parm = "Linf")

### F: FK vs animal ####
FK_F_major_plot <- ggplot(Dat_Major_LH_F, aes(mean_prop_DW_mg_body_wt_g, mean_Fultons_K, 
                                              color = IM_RT)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = F, 
              aes(group = 1), 
              color = "black") +
  geom_text(x=0.83, y=0.40, label="D", color="black", size=4.5) +
  geom_text_repel(aes(label = Location_Code)) +
  ylab("Mean Fulton's K") +
  ylim(c(0.25, 0.40)) +
  xlab("Average Animal DBI Proportion") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("Island Natural" = "lightseagreen", 
                                "Mainland Natural" = "forestgreen", 
                                "Mainland Artificial" = "blue4"))
FK_F_major_plot

# Fit linear model for females FK by animal weight
lm_FK_anwt_F <- lm(mean_Fultons_K ~ mean_prop_DW_mg_body_wt_g, data = Dat_Major_LH_F)
# extra model summary in tidy format w/ 95% CIs of slope
FK_anwt_parms_F <- tidy(summary(lm_FK_anwt_F), conf.int = TRUE) |> 
  mutate(Sex = "F") |> 
  mutate(Parm = "FK")

### F: PTL10 vs animal ####
Dat_Major_LH_F.predTL <- Dat_Major_F |> 
  filter(Gut_Content == "Animal") |> 
  left_join(Predicted_Parameters_10_F) |> 
  mutate(Location_Code = recode(Variables_Label,
                                'Anacapa & Santa Cruz Islands Natural' = "AISCRI",
                                'North Palos Verdes Artificial' = "NPV",
                                'North Palos Verdes Natural' = "NPV",
                                'North San Diego Artificial' = "NSD",
                                'North San Diego Natural' = "NSD",
                                'Orange County Artificial' = "OC",
                                'Orange County Natural' = "OC",
                                'San Clemente Island Natural' = "SCLI",
                                'Santa Barbara Island Natural' = "SBI",
                                'Santa Catalina Island Natural' = "SCI",
                                'South Palos Verdes Artificial' = "SPV",
                                'South Palos Verdes Natural' = "SPV",
                                'South San Diego Artificial' = "SSD",
                                'South San Diego Natural' = "SSD"))

predTL_F_major_plot <- ggplot(Dat_Major_LH_F.predTL, aes(mean_prop_DW_mg_body_wt_g, Pred_TL, 
                                                         color = IM_RT)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = F, 
              aes(group = 1), 
              color = "black") +
  geom_text(x=0.83, y=268, label="C", color="black", size=4.5) +
  geom_text_repel(aes(label = Location_Code)) +
  ylab("Pred. Size at Age 10") +
  ylim(c(190, 268)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_color_manual(values = c("Island Natural" = "lightseagreen", 
                                "Mainland Natural" = "forestgreen", 
                                "Mainland Artificial" = "blue4"))
predTL_F_major_plot

# Fit linear model for females PTL10 by animal weight
lm_PTL_anwt_F <- lm(Pred_TL ~ mean_prop_DW_mg_body_wt_g, data = Dat_Major_LH_F.predTL)
# extra model summary in tidy format w/ 95% CIs of slope
PTL_anwt_parms_F <- tidy(summary(lm_PTL_anwt_F), conf.int = TRUE) |> 
  mutate(Sex = "F") |> 
  mutate(Parm = "PTL10")

F_major_regressions_plot <- plot_grid(k_F_major_plot, 
                                      Linf_F_major_plot, 
                                      predTL_F_major_plot,
                                      FK_F_major_plot, 
                                      ncol = 1,
                                      align = "v",
                                      axis = "rl")
F_major_regressions_plot

### M: k vs animal ####
k_M_major_plot <- ggplot(Dat_Major_LH_M, aes(mean_prop_DW_mg_body_wt_g, k, 
                                             color = IM_RT)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = F, 
              aes(group = 1), 
              color = "black") +
  geom_text(x=0.75, y=0.27, label="E", color="black", size=4.5) +
  geom_text_repel(aes(label = Location_Code)) +
  ylab("k") +
  ylim(c(0.1, 0.27)) +
  theme_classic() +
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_color_manual(values = c("Island Natural" = "lightseagreen", 
                                "Mainland Natural" = "forestgreen", 
                                "Mainland Artificial" = "blue4"))
k_M_major_plot

# Fit linear model for males k by animal weight
lm_k_anwt_M <- lm(k ~ mean_prop_DW_mg_body_wt_g, data = Dat_Major_LH_M)
# extra model summary in tidy format w/ 95% CIs of slope
k_anwt_parms_M <- tidy(summary(lm_k_anwt_M), conf.int = TRUE) |> 
  mutate(Sex = "M") |> 
  mutate(Parm = "k")

### M: Linf vs animal ####
Linf_M_major_plot <- ggplot(Dat_Major_LH_M, aes(mean_prop_DW_mg_body_wt_g, linf, 
                                                color = IM_RT)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = F, 
              aes(group = 1), 
              color = "black") +
  geom_text(x=0.75, y=302, label="F", color="black", size=4.5) +
  geom_text_repel(aes(label = Location_Code)) +
  ylab("Linf") +
  ylim(c(230, 302)) +
  labs(y = expression(L[infinity])) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_color_manual(values = c("Island Natural" = "lightseagreen", 
                                "Mainland Natural" = "forestgreen", 
                                "Mainland Artificial" = "blue4"))
Linf_M_major_plot

# Fit linear model for males k by animal weight
lm_Linf_anwt_M <- lm(linf ~ mean_prop_DW_mg_body_wt_g, data = Dat_Major_LH_M)
# extra model summary in tidy format w/ 95% CIs of slope
Linf_anwt_parms_M <- tidy(summary(lm_Linf_anwt_M), conf.int = TRUE) |> 
  mutate(Sex = "M") |> 
  mutate(Parm = "Linf")

### M: FK vs animal ####
FK_M_major_plot <- ggplot(Dat_Major_LH_M, aes(mean_prop_DW_mg_body_wt_g, mean_Fultons_K, 
                                              color = IM_RT)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = F, 
              aes(group = 1), 
              color = "black") +
  geom_text(x=0.75, y=0.40, label="H", color="black", size=4.5) +
  geom_text_repel(aes(label = Location_Code)) +
  ylab("Mean Fulton's K") +
  ylim(c(0.25, 0.40)) +
  xlab("Average Animal DBI Proportion") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("Island Natural" = "lightseagreen", 
                                "Mainland Natural" = "forestgreen", 
                                "Mainland Artificial" = "blue4"))
FK_M_major_plot

# Fit linear model for males k by animal weight
lm_FK_anwt_M <- lm(mean_Fultons_K ~ mean_prop_DW_mg_body_wt_g, data = Dat_Major_LH_M)
# extra model summary in tidy format w/ 95% CIs of slope
FK_anwt_parms_M <- tidy(summary(lm_FK_anwt_M), conf.int = TRUE) |> 
  mutate(Sex = "M") |> 
  mutate(Parm = "FK")

### M: PTL10 vs animal ####
Dat_Major_LH_M.predTL <- Dat_Major_M |> 
  filter(Gut_Content == "Animal") |> 
  left_join(Predicted_Parameters_10_M) |> 
  mutate(Location_Code = recode(Variables_Label,
                                'Anacapa & Santa Cruz Islands Natural' = "AISCRI",
                                'North Palos Verdes Artificial' = "NPV",
                                'North Palos Verdes Natural' = "NPV",
                                'North San Diego Artificial' = "NSD",
                                'North San Diego Natural' = "NSD",
                                'Orange County Artificial' = "OC",
                                'Orange County Natural' = "OC",
                                'San Clemente Island Natural' = "SCLI",
                                'Santa Barbara Island Natural' = "SBI",
                                'Santa Catalina Island Natural' = "SCI",
                                'South Palos Verdes Artificial' = "SPV",
                                'South Palos Verdes Natural' = "SPV",
                                'South San Diego Artificial' = "SSD",
                                'South San Diego Natural' = "SSD"))

predTL_M_major_plot <- ggplot(Dat_Major_LH_M.predTL, aes(mean_prop_DW_mg_body_wt_g, Pred_TL, 
                                                         color = IM_RT)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = F, 
              aes(group = 1), 
              color = "black") +
  geom_text(x=0.75, y=268, label="G", color="black", size=4.5) +
  geom_text_repel(aes(label = Location_Code)) +
  ylim(c(190, 268)) +
  ylab("Pred. Size at Age 10") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_color_manual(values = c("Island Natural" = "lightseagreen", 
                                "Mainland Natural" = "forestgreen", 
                                "Mainland Artificial" = "blue4"))
predTL_M_major_plot

# Fit linear model for males PTL10 by animal weight
lm_PTL_anwt_M <- lm(Pred_TL ~ mean_prop_DW_mg_body_wt_g, data = Dat_Major_LH_M.predTL)
# extra model summary in tidy format w/ 95% CIs of slope
PTL_anwt_parms_M <- tidy(summary(lm_PTL_anwt_M), conf.int = TRUE) |> 
  mutate(Sex = "M") |> 
  mutate(Parm = "PTL10")

M_major_regressions_plot <- plot_grid(k_M_major_plot, 
                                      Linf_M_major_plot, 
                                      predTL_M_major_plot,
                                      FK_M_major_plot, 
                                      ncol = 1,
                                      align = "v",
                                      axis = "rl")
M_major_regressions_plot

## Final combined regression plots with animal ####
FM_major_regressions_plot <- plot_grid(F_major_regressions_plot, 
                                       M_major_regressions_plot, 
                                       nrow = 1, 
                                       align = "h")
FM_major_regressions_plot

# ggsave("FM_major_regressions_plot.png", FM_major_regressions_plot,
#        height = 10, 
#        width = 8, 
#        dpi = 600)

# combined coef tables, filter for just slope parameters (don't need y-intercepts)
LH_anwt_parms_table <- bind_rows(k_anwt_parms_F, Linf_anwt_parms_F, FK_anwt_parms_F, PTL_anwt_parms_F, 
                                 k_anwt_parms_M, Linf_anwt_parms_M, FK_anwt_parms_M, PTL_anwt_parms_M) |> 
  filter(term != "(Intercept)")

LH_anwt_parms_table

FM_anwt_LH_parms <- flextable(LH_anwt_parms_table)
FM_anwt_LH_parms <- theme_vanilla(FM_anwt_LH_parms)
FM_anwt_LH_parms <- set_header_labels(FM_anwt_LH_parms,
                                      term = NA,
                                      estimate = "Slope",
                                      statistic = "F",
                                      p.value = "Pr(>F)")
FM_anwt_LH_parms <- align(FM_anwt_LH_parms, 
                          part = "header", 
                          align = "center")
FM_anwt_LH_parms <- bold(FM_anwt_LH_parms, ~ p.value <0.05, ~ p.value, 
                         bold = TRUE)
FM_anwt_LH_parms <- set_table_properties(FM_anwt_LH_parms, 
                                         layout = "autofit")

FM_anwt_LH_parms

save_as_docx(FM_anwt_LH_parms, path = "FM_anwt_LH_parms.docx")

# Regression plots with temperature ####
Dat_Major_Alg_SST <- Dat_Major |> 
  group_by(Region, ReefType, Island_Mainland, IM_RT) |> 
  filter(Gut_Content == "Algae") |> 
  summarise(mean_prop_DW_mg_body_wt_g = mean(prop_DW_mg_body_wt_g),
            RegionSST = mean(RegionSST)) |> 
  mutate(Variables_Label = paste(Region, ReefType))

Dat_Major_Alg_SST <- Dat_Major_Alg_SST |> 
  group_by(Variables_Label, IM_RT) |> 
  mutate(Location_Code = recode(Variables_Label,
                                'Anacapa Island Natural' = "AI",
                                'Santa Cruz Island Natural' = "SCRI",
                                'North Palos Verdes Artificial' = "NPV",
                                'North Palos Verdes Natural' = "NPV",
                                'North San Diego Artificial' = "NSD",
                                'North San Diego Natural' = "NSD",
                                'Orange County Artificial' = "OC",
                                'Orange County Natural' = "OC",
                                'San Clemente Island Natural' = "SCLI",
                                'Santa Barbara Island Natural' = "SBI",
                                'Santa Catalina Island Natural' = "SCI",
                                'South Palos Verdes Artificial' = "SPV",
                                'South Palos Verdes Natural' = "SPV",
                                'South San Diego Artificial' = "SSD",
                                'South San Diego Natural' = "SSD"))



Alg_SST_plot <- ggplot(Dat_Major_Alg_SST, aes(RegionSST, mean_prop_DW_mg_body_wt_g, 
                                                         color = ReefType)) +
  geom_point(size = 3.5) +
  geom_text_repel(aes(label = Location_Code)) +
  geom_smooth(method = lm, 
              se = F, 
              aes(group = 1), 
              color = "black") +
  geom_text(x=0.75, y=268, label="G", color="black", size=4.5) +
  scale_color_manual(values = c("Natural" = "forestgreen", 
                                "Artificial" = "blue4")) +
  ylim(0,1) +
  ylab("Average Algae DBI Proportion in Diet") +
  xlab("Sea Surface Temperature (F)") +
  theme_classic() +
  theme(legend.text = element_text(size = 13),
        legend.title = element_text(size = 15), 
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15))

Alg_SST_plot

# ggsave("Alg_SST_plot.png", Alg_SST_plot,
#        height = 10, 
#        width = 8, 
#        dpi = 600)

# Fit linear model for males PTL10 by animal weight
lm_Alg_SST <- lm(mean_prop_DW_mg_body_wt_g ~ RegionSST * IM_RT, data = Dat_Major_Alg_SST)
# extra model summary in tidy format w/ 95% CIs of slope
lm_Alg_SST_parms <- tidy(summary(lm_Alg_SST), conf.int = TRUE) 
lm_Alg_SST_parms

# Appendix Tables ---------------------------------------------------------
# DBI proportion of each content category by location, reef type, & sex
#actual one
prop_table <- Dat |> 
  group_by(Region, ReefType, Sex, Gut_Content) |> 
  summarise(mean_DBI = mean(prop_DW_mg_body_wt_g))
  
prop_table<- prop_table |> 
  group_by(Region, ReefType, Sex) |> 
  mutate(sum_DBI = sum(mean_DBI))

prop_table<- prop_table |> 
  group_by(Region, ReefType, Sex) |> 
  mutate(prop_DBI = mean_DBI/sum_DBI)

prop_table <- prop_table |> 
  pivot_wider(id_cols = c(Region, ReefType, Sex),
            names_from = Gut_Content , 
            values_from = prop_DBI) |> 
  mutate(across(where(is.numeric), round, 2))

prop_table <- as_tibble(prop_table)

FT_prop_table <- flextable(prop_table)

FT_prop_table <- set_header_labels(FT_prop_table, 
                                        Region = "Location",
                                        ReefType = "Reef Type")

FT_prop_table <- align(FT_prop_table, 
                            part = "header", 
                            align = "left")
FT_prop_table <- set_table_properties(FT_prop_table, 
                                           layout = "autofit")

FT_prop_table <- set_table_properties(FT_prop_table, layout = "autofit")
FT_prop_table <- theme_box(FT_prop_table)
FT_prop_table <- merge_v(FT_prop_table, j = ~Region)

FT_prop_table
save_as_docx(FT_prop_table, path = "FT_prop_table.docx")

# number of stomachs processed by location, reef type, sex, site long/lat, & collection date
Summary_CN_R_RT_S_Site_Date <- Dat |> 
  filter(Sex != "U") |> 
  group_by(Region, ReefType, Sex, Site, Region_Longitude, Region_Latitude, Date) |> 
  distinct(CN) |> 
  summarise(count = (count=n())) |> 
  arrange(desc(Region_Latitude)) 

Summary_CN_R_RT_S_Site_Date$Date <- format(as.Date(Summary_CN_R_RT_S_Site_Date$Date), "%d/%m/%Y")

FT_Summ_CN_R_RT_S_Site_Date <- flextable(Summary_CN_R_RT_S_Site_Date)
FT_Summ_CN_R_RT_S_Site_Date <- set_header_labels(FT_Summ_CN_R_RT_S_Site_Date, 
                                                 Region = "Location", 
                                                 ReefType = "Reef Type",
                                                 Region_Longitude = "Longitude",
                                                 Region_Latitude = "Latitude",
                                                 Date = "Collection Date")

FT_Summ_CN_R_RT_S_Site_Date <- set_table_properties(FT_Summ_CN_R_RT_S_Site_Date, layout = "autofit")
FT_Summ_CN_R_RT_S_Site_Date <- theme_box(FT_Summ_CN_R_RT_S_Site_Date)
FT_Summ_CN_R_RT_S_Site_Date <- merge_v(FT_Summ_CN_R_RT_S_Site_Date, j = ~ Region)

FT_Summ_CN_R_RT_S_Site_Date
save_as_docx(FT_Summ_CN_R_RT_S_Site_Date, path = "FT_Summ_CN_R_RT_S_Site_Date.docx")

# life history metrics
tibble.metrics_F <- tibble.pcoa_F_Pred_10 |> 
  mutate(Sex = "F")

tibble.metrics_M <- tibble.pcoa_M_Pred_10 |> 
  mutate(Sex = "M")

LH_metrics_table <- tibble.metrics_F |> 
  bind_rows(tibble.metrics_M)

LH_metrics_table <- LH_metrics_table |> 
  select(Region, ReefType, Sex, linf, k, mean_Fultons_K, Pred_TL)

LH_metrics_table <- as_tibble(LH_metrics_table)

FT_LH_metrics_table <- flextable(LH_metrics_table)
FT_LH_metrics_table <- set_header_labels(FT_LH_metrics_table, 
                                         Region = "Location", 
                                         ReefType = "Reef Type",
                                         linf = "L∞",
                                         mean_Fultons_K = "Funlton's K",
                                         Pred_TL = "PTL 10")

FT_LH_metrics_table <- set_table_properties(FT_LH_metrics_table, layout = "autofit")
FT_LH_metrics_table <- theme_box(FT_LH_metrics_table)
FT_LH_metrics_table <- merge_v(FT_LH_metrics_table, j = ~ Region)

FT_LH_metrics_table
save_as_docx(FT_LH_metrics_table, path = "FT_LH_metrics_table.docx")

# RDA Models ---------------------------------------------------------------------
#RDA model with both males and females included
##in our case, "species" or response variable is LH (Y) and "env" or independent variable is gut contents (X)
LH_comm.rda <- LH_comm %>% 
  select(linf, k, mean_Fultons_K)

rda_R_RT_S <- rda(LH_comm.rda~., Comm_Gut_Wide_AISCRI_R_RT_S)

#results
summary(rda_R_RT_S)

##constrained proportion: variance of Y explained by X (78%)
  #the included gut content categories explain 78% of the variation observed in life history patterns
##unconstained Proportion: unexplained variance in Y (22%)

#test the significance of the RDA
anova.cca(rda_R_RT_S, step = 1000)

#test the significance of each variable
anova.cca(rda_R_RT_S, step = 1000, by = "term")

#adj R2 of the full model
RsquareAdj(rda_R_RT_S)
##gut contents explain 46% of variance in life history(adj R2)
##adj R2 corrected for number of explanatory variables

# try with LH standardized?
LH.rda.std <- decostand(LH_comm.rda, method = "standardize")
rda.std <- rda(LH.rda.std~., Comm_Gut_Wide_AISCRI_R_RT_S)
rda_F.std <- rda(LH_F.rda.std ~  Red_Algae + Polychaeta + Anemone + Sponge + BE_Algae + Hydroids + Bryozoan + Mollusk + Brown_Algae, data = Comm_Gut_Wide_AISCRI_R_RT_F)
summary(rda.std)
RsquareAdj(rda.std)
plot(rda.std)

#graphRDA 
plot(rda_R_RT_S)
#longer arrows indicate that prey category strongly drives the variation in the community matrix 
#arrows pointing in opposite directions have a negative relationship
#arrows pointing in the same direction have a positive relationship 

## two types of RDA scaling
##scaling 1: shows similarities between objects in the response matrix, such that distance among objects reflect their similarities
ordiplot(rda_R_RT_S, scaling = 1, type = "text")
##regions that are closer together have more similar communities
##life history patterns that are closer together occupy more sites in common

##scaling 2: shows the effects of the explanatory variables, such that the angles between variables reflect their correlation
ordiplot(rda_R_RT_S, scaling = 2, type = "text")
##longer arrows mean that variable strongly drives the variation in the community matrix
##arrows pointing in opposite directions have a negative relationship 
##arrows pointing in the same direction have a positive relationship

#### RDA model separated males/females ####
## !!! Filtering by male or female cuts the amount of objects in half, creating an issue: If the number of explanatory variables is equal to or greater than the number of objects in your data set, the analysis is not constrained. That is, the matrix of response variables will be completely 'explained' by the matrix of explanatory variables.
 #https://sites.google.com/site/mb3gustame/constrained-analyses/rda

# females only 

LH_F.rda <- LH_F |> 
  column_to_rownames(var = "Variables_Label") |> 
  select(linf, k, mean_Fultons_K)

# running rda 
names(Comm_Gut_Wide_AISCRI_R_RT_F) <- str_replace_all(names(Comm_Gut_Wide_AISCRI_R_RT_F), " ", "_")

rda_F <- rda(LH_F.rda ~  Red_Algae + Polychaeta + Anemone + Sponge + BE_Algae + Hydroids + Bryozoan + Mollusk + Brown_Algae, data = Comm_Gut_Wide_AISCRI_R_RT_F)

#results
summary(rda_F)

##constrained proportion: variance of Y explained by X (95%)
#the included gut content categories explain % of the variation observed in life history patterns
##unconstained Proportion: unexplained variance in Y (5%)

#test the significance of the RDA
anova.cca(rda_F, step = 1000)

#test the significance of each variable
anova.cca(rda_F, step = 1000, by = "term")

#adj R2 of the full model
RsquareAdj(rda_F)
##gut contents explain 85% of variance in life history(adj R2)
##adj R2 corrected for number of explanatory variables
plot(rda_F)

# try with LH standardized?
LH_F.rda.std <- decostand(LH_F.rda, method = "standardize")

rda_F.std <- rda(LH_F.rda.std ~  Red_Algae + Polychaeta + Anemone + Sponge + BE_Algae + Hydroids + Bryozoan + Mollusk + Brown_Algae, data = Comm_Gut_Wide_AISCRI_R_RT_F)

summary(rda_F.std)

RsquareAdj(rda_F.std)

plot(rda_F.std)

#LH with range stand method
# LH_F.rda.stdrange <- decostand(LH_F.rda, method = "range")
# 
# rda_F.stdrange <- rda(LH_F.rda.stdrange ~  Red_Algae + Polychaeta + Anemone + Sponge + BE_Algae + Hydroids + Bryozoan + Mollusk + Brown_Algae, data = Comm_Gut_Wide_AISCRI_R_RT_F)
# summary(rda_F.stdrange)
# RsquareAdj(rda_F.stdrange)
# plot(rda_F.stdrange)

# males only
LH_M.rda <- LH_M |> 
  column_to_rownames(var = "Variables_Label") |> 
  select(linf, k, mean_Fultons_K)

# running rda 
names(Comm_Gut_Wide_AISCRI_R_RT_M) <- str_replace_all(names(Comm_Gut_Wide_AISCRI_R_RT_M), " ", "_")

# standardizing life history data
LH_M.rda.std <- decostand(LH_M.rda, method = "standardize")

#running rda
rda_M.std <- rda(LH_M.rda.std ~  Red_Algae + Polychaeta + Anemone + Sponge + BE_Algae + Hydroids + Bryozoan + Mollusk + Brown_Algae, data = Comm_Gut_Wide_AISCRI_R_RT_M)

summary(rda_M.std)

RsquareAdj(rda_M.std)

plot(rda_M.std)

# db-RDA ------------------------------------------------------------------
##02/2023 decided not to use dbrda - the db-RDA would work if our response was diet comp because that’s the data we would use BC on, but since we want to look at the effect of diet on LH, we should be okay to use RDA and do analysis based on euclidean distances

#Legendre & Anderson 1999 OGs
#Steps in the procedure include: 
##(1) calculating a matrix of distances among replicates using a distance measure of choice (e.g., Bray-Curtis); 
##(2) determining the principal coordinates (including a correction for negative eigenvalues, if necessary), which preserve these distances; 
##(3) creating a matrix of dummy variables corresponding to the design of the experiment (i.e., individual terms in a linear model); 
##(4) analyzing the relationship between the principal coordinates (species data) and the dummy variables (model) using RDA; and 
##(5) implementing a test by per- mutation for particular statistics corresponding to the particular terms in the model
# 
# dbRDA <- capscale( ~  +  + ,
#                   data = Comm_Gut_Wide_AISCRI_R_RT_S, dist = "bray")

#Following this example: https://fukamilab.github.io/BIO202/06-B-constrained-ordination.html
#inputting the gut comm data with the LH comm data through the capscale function

# dbRDA2 <- capscale(LH_comm2 ~ Animal + Algae,
#                    data = Comm_Wide_Dat_Major_AISCRI_R_RT_GC, dist = "bray")
# dbRDA2
# 
# #plot RDA
# plot(dbRDA2)
# 
# summary(dbRDA2)
# screeplot(dbRDA2)

# ##Example from ?capscale  
# data(varespec)
# data(varechem)
# vare.cap <- capscale(varespec ~ N + P + K + Condition(Al), varechem,
#                      dist="bray")
# vare.cap
# plot(vare.cap)
# anova(vare.cap)
