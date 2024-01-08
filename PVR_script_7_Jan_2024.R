#### Final Thesis Script ----
#### Author: James W. Sturges
#### Project Title: Assessing Community Structure and Fine-Scale Fish Habitat Use Patterns on the Palos Verdes Reef Restoration Project
#### Last Updated: July 12 2022

#### Required Packages ----
library(here)
library(Hmisc)
library(RColorBrewer)
library(randomcoloR)
library(vegan)
library(tidyverse)
library(flextable) 
library(ggrepel)
#library(clustsig) no longer supported
library(officer)
library(wesanderson)
library(dendextend)
library(ggdendro)


here()


#### Importing Cleaned Data ----
#This imports our data set from the raw EventMeasure files
#We have already cleaned these data to remove transect indicator measurements
source("dat_EMObs_Cleaning.R")

set_flextable_defaults(font.family = "Arial", font.size = 9, digits = 1, padding = 0.1)


#### mean distance per habitat type flextable ----

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


ht_dist_ft

write_csv(ht_dist_st, "tables/ht_dist_st.csv")

# save_as_docx(ht_dist_ft, path = "ht_dist_ft.docx")


#### swim time per transect east v west comparison ----

time_comp_plot <- time_comp_plot +
  theme_classic() +
  labs(x = "Time of Upcurrent Transect (s)", y = "Time of Downcurrent Transect (s)") +
  guides(color = guide_legend(title = "Transect Type"),
         guide_legend(override.aes = aes(label = ""))) +
  theme(legend.position = "top") +
  theme(legend.text = element_text(color = "black")) 






#time_comp_plot

# ggsave("figures/time_comp_plot.png", time_comp_plot,
#        width = 6, height = 5, dpi = 600)



#### create new variables for Zone and current flow ----

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

#### Creating dat_fish_t ----
# 31 species observed over 216 transects
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
  mutate(compass_heading = (case_when(startsWith(os_ht, "West Perpendicular Mid_High") ~ 341,
                                      startsWith(os_ht, "West Perpendicular Ecotone_High") ~ 341,
                                      startsWith(os_ht, "West Perpendicular Mid_Low") ~ 297,
                                      startsWith(os_ht, "West Perpendicular Ecotone_Low") ~ 297,
                                      startsWith(os_ht, "West Perpendicular Mid_Medium") ~ 257,
                                      startsWith(os_ht, "West Perpendicular Ecotone_Medium") ~ 257,
                                      startsWith(os_ht, "East Perpendicular Mid_High") ~ 167,
                                      startsWith(os_ht, "East Perpendicular Ecotone_High") ~ 167,
                                      startsWith(os_ht, "East Perpendicular Mid_Low") ~ 115,
                                      startsWith(os_ht, "East Perpendicular Ecotone_Low") ~ 115,
                                      startsWith(os_ht, "East Perpendicular Mid_Medium") ~ 72,
                                      startsWith(os_ht, "East Perpendicular Ecotone_Medium") ~ 72,
                                      startsWith(os_ht, "West Parallel Mid_High") ~ 254,
                                      startsWith(os_ht, "West Parallel Ecotone_High") ~ 254,
                                      startsWith(os_ht, "West Parallel Mid_Low") ~ 200,
                                      startsWith(os_ht, "West Parallel Ecotone_Low") ~ 200,
                                      startsWith(os_ht, "West Parallel Mid_Medium") ~ 164,
                                      startsWith(os_ht, "West Parallel Ecotone_Medium") ~ 164,
                                      startsWith(os_ht, "East Parallel Mid_High") ~ 78,
                                      startsWith(os_ht, "East Parallel Ecotone_High") ~ 78,
                                      startsWith(os_ht, "East Parallel Mid_Low") ~ 18,
                                      startsWith(os_ht, "East Parallel Ecotone_Low") ~ 18,
                                      startsWith(os_ht, "East Parallel Mid_Medium") ~ 347,
                                      startsWith(os_ht, "East Parallel Ecotone_Medium") ~ 347)))

dat_fish_t <- dat_fish_t %>% 
  mutate(cluster = (case_when(startsWith(os_ht, "West Perpendicular Mid_High") ~ "Group 1",
                              startsWith(os_ht, "West Perpendicular Ecotone_High") ~ "Group 4",
                              startsWith(os_ht, "West Perpendicular Mid_Low") ~ "Group 5",
                              startsWith(os_ht, "West Perpendicular Ecotone_Low") ~ "Group 5",
                              startsWith(os_ht, "West Perpendicular Mid_Medium") ~ "Group 6",
                              startsWith(os_ht, "West Perpendicular Ecotone_Medium") ~ "Group 6",
                              startsWith(os_ht, "East Perpendicular Mid_High") ~ "Group 6",
                              startsWith(os_ht, "East Perpendicular Ecotone_High") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Mid_Low") ~ "Group 4",
                              startsWith(os_ht, "East Perpendicular Ecotone_Low") ~ "Group 5",
                              startsWith(os_ht, "East Perpendicular Mid_Medium") ~ "Group 5",
                              startsWith(os_ht, "East Perpendicular Ecotone_Medium") ~ "Group 5",
                              startsWith(os_ht, "West Parallel Mid_High") ~ "Group 6",
                              startsWith(os_ht, "West Parallel Ecotone_High") ~ "Group 6",
                              startsWith(os_ht, "West Parallel Mid_Low") ~ "Group 5",
                              startsWith(os_ht, "West Parallel Ecotone_Low") ~ "Group 4",
                              startsWith(os_ht, "West Parallel Mid_Medium") ~ "Group 6",
                              startsWith(os_ht, "West Parallel Ecotone_Medium") ~ "Group 4",
                              startsWith(os_ht, "East Parallel Mid_High") ~ "Group 5",
                              startsWith(os_ht, "East Parallel Ecotone_High") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Mid_Low") ~ "Group 4",
                              startsWith(os_ht, "East Parallel Ecotone_Low") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Mid_Medium") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Ecotone_Medium") ~ "Group 3")))


#### Summary Tables ----

# List species observed in study and total abundance indentified 
spp_ID <- dat_fish_2m_wide %>% 
  group_by(Genus_spp, Code) %>% 
  count(Genus_spp, sort = T) %>% 
  arrange(desc(n))


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
#### Focal Species summary table ----
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

# focal_spp_st <- focal_spp_st %>% 
#   left_join(fish_images)



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

# focal_spp_ft <- flextable(focal_spp_st,
#                           col_keys = c("Genus_spp", "Code",
#                                        "n", "max_length", "min_length", "Ecotone_High","Ecotone_Medium","Ecotone_Low", "Mid_High","Mid_Medium","Mid_Low")) %>% 
#   add_header_row(colwidths = c(2,1,2,6), values = c("Species Name", "Total Observations", "Length (mm)", "Total Density Across Habitat Types")) %>% 
# set_header_labels(Genus_spp = "Species Name",
#                     Code = "Species Name",
#                     n = "Total Observations", 
#                     max_length = "Max",
#                     min_length = "Min",
#                     Ecotone_High = "EH",
#                     Ecotone_Medium = "EM",
#                     Ecotone_Low = "EL",
#                     Mid_High = "MH",
#                     Mid_Medium = "MM",
#                     Mid_Low = "ML") %>% 
#   # add_footer_lines("Focal Species Summary Table") %>% 
#   #  bg(i = ~ Ecotone_High > 5000,
#   #        j = c("Ecotone_High"),
#   #        bg = "green") %>% 
#   #  bg(i = ~ Mid_High > 306 & Mid_High < 307,
#   #     j = c("Mid_High"),
#   #     bg = "green") %>% 
#   #  bg(i = ~ Ecotone_High > 646 & Ecotone_High < 647,
#   #     j = c("Ecotone_High"),
#   #     bg = "green") %>% 
#   #  bg(i = ~ Mid_Medium > 226 & Mid_Medium < 227,
#   #     j = c("Mid_Medium"),
#   #     bg = "green") %>% 
#   #  bg(i = ~ Ecotone_Low > 106 & Ecotone_Low < 107,
#   #     j = c("Ecotone_Low"),
#   #     bg = "green") %>% 
#   #  bg(i = ~ Mid_Medium > 113 & Mid_Medium < 114,
#   #     j = c("Mid_Medium"),
#   #     bg = "green") %>% 
#   #  bg(i = ~ Ecotone_High > 84 & Ecotone_High < 85,
#   #     j = c("Ecotone_High"),
#   #     bg = "green") %>% 
#   #  bg(i = ~ Mid_Medium > 84 & Mid_Medium < 86,
#   #     j = c("Mid_Medium"),
#   #     bg = "green") %>% 
#   #  bg(i = ~ Mid_High > 56 & Mid_High < 57,
#   #     j = c("Mid_High"),
#   #     bg = "green") %>% 
#   #  bg(i = ~ Ecotone_High > 53 & Ecotone_High < 54,
#   #     j = c("Ecotone_High"),
#   #     bg = "green") %>% 
#   #  bg(i = ~ Ecotone_High > 40 & Ecotone_High < 41,
#   #     j = c("Ecotone_High"),
#   #     bg = "green") %>% 
#    colformat_double(digits = 1) %>%
#    theme_box() %>%
#    align(align = "center") %>%
#    align(part = "header", align = "center") %>% 
#  compose(j = "Genus_spp",
#            value = as_paragraph(as_i(Genus_spp))) %>% 
#   #  bg(part = "header", j = c("Mid_High", "Mid_Medium", "Mid_Low"), bg = "azure4") %>% 
#   #  bg(part = "header", j = c("Ecotone_High", "Ecotone_Medium", "Ecotone_Low"), bg = "blanchedalmond") %>% 
#   # bg(i = ~ Mid_Low > 1194 & Mid_Low < 1195,
#   #    j = c("Mid_Low"),
#   #    bg = "red") %>% 
#   # bg(i = ~ Ecotone_Medium > 156 & Ecotone_Medium < 157,
#   #    j = c("Ecotone_Medium"),
#   #    bg = "red") %>% 
#   # bg(i = ~ Ecotone_Low > 84 & Ecotone_Low < 85,
#   #    j = c("Ecotone_Low"),
#   #    bg = "red") %>% 
#   # bg(i = ~ Mid_Low > 71 & Mid_Low < 72,
#   #    j = c("Mid_Low"),
#   #    bg = "red") %>% 
#   # bg(i = ~ Mid_Low > 70 & Mid_Low < 71,
#   #    j = c("Mid_Low"),
#   #    bg = "red") %>% 
#   # bg(i = ~ Ecotone_High > 37 & Ecotone_High < 38,
#   #    j = c("Ecotone_High"),
#   #    bg = "red") %>% 
#   # bg(i = ~ Mid_High > 26 & Mid_High < 27,
#   #    j = c("Mid_High"),
#   #    bg = "red") %>% 
#   # bg(i = ~ Ecotone_Low > 12 & Ecotone_Low < 13,
#   #    j = c("Ecotone_Low"),
#   #    bg = "red") %>% 
#   # bg(i = ~ Ecotone_Medium > 12 & Ecotone_Medium < 13,
#   #    j = c("Ecotone_Medium"),
#   #    bg = "red") %>% 
#   # bg(i = ~ Ecotone_Medium > 21 & Ecotone_Medium < 22,
#   #    j = c("Ecotone_Medium"),
#   #    bg = "red") %>% 
#   # bg(i = ~ Ecotone_High > 21 & Ecotone_High < 22,
#   #    j = c("Ecotone_High"),
#   #    bg = "red") %>% 
#   # bg(i = ~ Mid_High > 13 & Mid_High < 14,
#   #    j = c("Mid_High"),
#   #    bg = "red") %>% 
#   # bg(i = ~ Mid_High > 4 & Mid_High < 5,
#   #    j = c("Mid_High"),
#   #    bg = "red") %>% 
#    merge_v(part = "header")




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
  mutate(cluster = (case_when(startsWith(os_ht, "West Perpendicular Mid_High") ~ "Group 1",
                              startsWith(os_ht, "West Perpendicular Ecotone_High") ~ "Group 4",
                              startsWith(os_ht, "West Perpendicular Mid_Low") ~ "Group 5",
                              startsWith(os_ht, "West Perpendicular Ecotone_Low") ~ "Group 5",
                              startsWith(os_ht, "West Perpendicular Mid_Medium") ~ "Group 6",
                              startsWith(os_ht, "West Perpendicular Ecotone_Medium") ~ "Group 6",
                              startsWith(os_ht, "East Perpendicular Mid_High") ~ "Group 6",
                              startsWith(os_ht, "East Perpendicular Ecotone_High") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Mid_Low") ~ "Group 4",
                              startsWith(os_ht, "East Perpendicular Ecotone_Low") ~ "Group 5",
                              startsWith(os_ht, "East Perpendicular Mid_Medium") ~ "Group 5",
                              startsWith(os_ht, "East Perpendicular Ecotone_Medium") ~ "Group 5",
                              startsWith(os_ht, "West Parallel Mid_High") ~ "Group 6",
                              startsWith(os_ht, "West Parallel Ecotone_High") ~ "Group 6",
                              startsWith(os_ht, "West Parallel Mid_Low") ~ "Group 5",
                              startsWith(os_ht, "West Parallel Ecotone_Low") ~ "Group 4",
                              startsWith(os_ht, "West Parallel Mid_Medium") ~ "Group 6",
                              startsWith(os_ht, "West Parallel Ecotone_Medium") ~ "Group 4",
                              startsWith(os_ht, "East Parallel Mid_High") ~ "Group 5",
                              startsWith(os_ht, "East Parallel Ecotone_High") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Mid_Low") ~ "Group 4",
                              startsWith(os_ht, "East Parallel Ecotone_Low") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Mid_Medium") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Ecotone_Medium") ~ "Group 3")))


spp_rich_t <- spp_rich_t %>% 
  mutate(current =  case_when(startsWith(os, "West") ~ "Up-current",
                              startsWith(os, "East") ~ "Down-current"))
spp_rich_t <- spp_rich_t %>%
  mutate(cluster_2 = cluster)

spp_rich_t <- spp_rich_t %>%
  mutate(cluster_2 = str_replace_all(cluster_2, "Group 1", "Perpendicular Up-current High Relief"),
         cluster_2 = str_replace_all(cluster_2, "Group 2", "Perpendicular Down-current High Ecotone"),
         cluster_2 = str_replace_all(cluster_2, "Group 3", "Inshore Parallel"),
         cluster_2 = str_replace_all(cluster_2, "Group 4", "Low Relief & Ecotones"),
         cluster_2 = str_replace_all(cluster_2, "Group 5", "Intermediate"),
         cluster_2 = str_replace_all(cluster_2, "Group 6", "Offshore High Relief"))

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
  bg(j = "cluster", i = ~ cluster == "Group 1", bg = "gray", part = "body") %>% 
  bg(j = "cluster", i = ~ cluster == "Group 2", bg = "khaki4", part = "body") %>% 
  bg(j = "cluster", i = ~ cluster == "Group 3", bg = "purple4", part = "body") %>% 
  bg(j = "cluster", i = ~ cluster == "Group 4", bg = "steelblue3", part = "body") %>% 
  bg(j = "cluster", i = ~ cluster == "Group 5", bg = "springgreen4", part = "body") %>% 
  bg(j = "cluster", i = ~ cluster == "Group 6", bg = "coral2", part = "body") %>% 
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
  # theme_box() %>%
  align(align = "center") %>%
  align(part = "header", align = "center") %>%
  # bg(j = "cluster", i = ~ cluster == "Group 1", bg = "gray", part = "body") %>% 
  # bg(j = "cluster", i = ~ cluster == "Group 2", bg = "khaki4", part = "body") %>% 
  # bg(j = "cluster", i = ~ cluster == "Group 3", bg = "purple4", part = "body") %>% 
  # bg(j = "cluster", i = ~ cluster == "Group 4", bg = "steelblue3", part = "body") %>% 
  # bg(j = "cluster", i = ~ cluster == "Group 5", bg = "springgreen4", part = "body") %>% 
  # bg(j = "cluster", i = ~ cluster == "Group 6", bg = "coral2", part = "body") %>% 
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

### TO DO - LOOK AT PAPERS for what columns included typically (mean, median sd, min max etc???)
## THEN FORMAT WHAT IS CALCUALTED and how columns are ordered below

# species level (across all transects)
dat_fish_t <- dat_fish_t %>% 
  mutate(current =  case_when(startsWith(os, "West") ~ "Up-current",
                              startsWith(os, "East") ~ "Down-current"))
dat_fish_t <- dat_fish_t %>%
  mutate(cluster_2 = cluster)

dat_fish_t <- dat_fish_t %>%
  mutate(cluster_2 = str_replace_all(cluster_2, "Group 1", "Perpendicular Inshore High Relief"),
         cluster_2 = str_replace_all(cluster_2, "Group 2", "Perpendicular Offshore High Ecotone"),
         cluster_2 = str_replace_all(cluster_2, "Group 3", "Sheltered Inshore Parallel"),
         cluster_2 = str_replace_all(cluster_2, "Group 4", "Low Relief & Ecotones"),
         cluster_2 = str_replace_all(cluster_2, "Group 5", "Intermediate"),
         cluster_2 = str_replace_all(cluster_2, "Group 6", "Offshore High & Medium Relief"))

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
  mutate(cluster = (case_when(startsWith(os_ht, "West Perpendicular Mid_High") ~ "Group 1",
                              startsWith(os_ht, "West Perpendicular Ecotone_High") ~ "Group 4",
                              startsWith(os_ht, "West Perpendicular Mid_Low") ~ "Group 5",
                              startsWith(os_ht, "West Perpendicular Ecotone_Low") ~ "Group 5",
                              startsWith(os_ht, "West Perpendicular Mid_Medium") ~ "Group 6",
                              startsWith(os_ht, "West Perpendicular Ecotone_Medium") ~ "Group 6",
                              startsWith(os_ht, "East Perpendicular Mid_High") ~ "Group 6",
                              startsWith(os_ht, "East Perpendicular Ecotone_High") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Mid_Low") ~ "Group 4",
                              startsWith(os_ht, "East Perpendicular Ecotone_Low") ~ "Group 5",
                              startsWith(os_ht, "East Perpendicular Mid_Medium") ~ "Group 5",
                              startsWith(os_ht, "East Perpendicular Ecotone_Medium") ~ "Group 5",
                              startsWith(os_ht, "West Parallel Mid_High") ~ "Group 6",
                              startsWith(os_ht, "West Parallel Ecotone_High") ~ "Group 6",
                              startsWith(os_ht, "West Parallel Mid_Low") ~ "Group 5",
                              startsWith(os_ht, "West Parallel Ecotone_Low") ~ "Group 4",
                              startsWith(os_ht, "West Parallel Mid_Medium") ~ "Group 6",
                              startsWith(os_ht, "West Parallel Ecotone_Medium") ~ "Group 4",
                              startsWith(os_ht, "East Parallel Mid_High") ~ "Group 5",
                              startsWith(os_ht, "East Parallel Ecotone_High") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Mid_Low") ~ "Group 4",
                              startsWith(os_ht, "East Parallel Ecotone_Low") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Mid_Medium") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Ecotone_Medium") ~ "Group 3")))




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


####  Heatmap ----
dens_sp_order <- dens_sp_os_ht %>% 
  group_by(Genus_spp) %>% 
  summarise(mean_dens = mean(mean_dens)) %>% 
  arrange(mean_dens)

dens_sp_os_ht$Genus_spp <- factor(dens_sp_os_ht$Genus_spp, 
                                  levels = dens_sp_order$Genus_spp)

dens_sp_clust_heatmap <- dens_sp_os_ht %>% 
  group_by(cluster, Genus_spp) %>% 
  summarise(mean_clust_dens = mean(mean_dens), 
            min_clust_dens = min(mean_dens),
            max_clust_dens = max(mean_dens),
            count_tt = n())

group_color_lab <- c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")

heatmap_sp_clust<-dens_sp_clust_heatmap %>%
  ggplot(aes(x=cluster, y = Genus_spp)) +
  geom_tile(aes(fill = mean_clust_dens ), position = "identity", colour = "black") +
  scale_fill_gradientn(trans= "log1p", 
                       colors = c("white","red","dark blue"),
                       #colours = wes_palette("Zissou1", 100, type = "continuous"),
                       limits=c(0, 400), 
                       breaks = c(0, 1, 3, 6, 12, 25, 50, 100, 200, 400),
                       name= expression(paste("Density\n(No./100",m^{2},")"))) + #name= expression(density~(No.~100m,^{-2}))) +
  theme_bw() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 18)) + 
  theme(axis.text.y = element_text(face = "bold"), axis.text.x = element_text(color = group_color_lab)) +
  #xlab('Depth group (m)') +
  #scale_x_discrete(limits = rev(levels(dens_sp_clust_heatmap$cluster))) +
  ylab(NULL) +
  xlab(NULL) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) +
  #scale_x_discrete(labels=c("rig midwater"="Mid", "rig base"="Base", "rig shellmound"="Shell", "natural reef"="Nat")) +
  geom_text(aes(label=paste(round(mean_clust_dens,1),"\n",
                            #paste("(", round(sd.density_m2*100,1), ")", sep=""),"\n",   # with sd
                            paste("(",
                                  round(min_clust_dens,1),
                                  ", ",
                                  round(max_clust_dens,1),
                                  ")", sep = "")   # with range
                            #,"\n",paste("tt =", count_tt )
  )) , size=5.5) +
  scale_y_discrete(labels = c("Rock Wrasse","Olive Rockfish","Pile Perch","Opaleye","Barred Sand Bass","Rainbow Seaperch", "Black Perch","California Sheephead","Kelp Bass","Señorita","Blacksmith"))


heatmap_sp_clust

ggsave(heatmap_sp_clust, file="figures/heatmap.png", width=6, height=6, dpi=600) 



#####
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

#dens_sp_ht_ft

### TO DO - ADD LIST OF tables prior to J going on vacation
# Both biological and sampling/module tables

# Possibly include in fish tables
# - density (min, mean, median, max, sd), frequency of occurrence on transects (proportion of transects species was observed)

# species richness (Overall vs. # of focal species) - which tables will this go in (Overall vs. # of focal species)?

#### TO DO - Transect time/length analysis to adjust densities etc...

# And include "sampling intensity adjustment plot" - to justify using time to estimate transect lengths on mids
# and used to justify ecotone lengths




#### Community Composition Analysis ----

# TO DO - which other point designations should be done? Create list:
# For sure:
# by Module
# by os_ht

# Maybe???
# (probably not by transect - didn't converge - too much variability among individual transects?)

# (NOT: os_ht + module height - was too few transects per point)

# Community grouped by habitat type, orientation, and side
# Creates 24 unique locations across the reef
# We think Module Orientation influences sediment and nutrient patterns



### LOOK INTO - can you rotate this 180 degrees? so offshore/onshore groupings are similar to map?

#### Community Composition Section ----

#### Module Level Community Analysis ----
dat_fish_ht_mod_18 <- dat_fish_t %>% 
  group_by(Module, Genus_spp) %>% 
  summarise(dens_100m2 = mean(dens_100m2)) %>% 
  ungroup() %>% 
  left_join(
    distinct(select(transect_var, !c("Module_Side", "Habitat_Type", "Sub_Mod", "transect", "Zone")))
  )

wide_fish_ht_mod_18 <- dat_fish_ht_mod_18 %>% 
  mutate(dens_100m2_4rt = dens_100m2^0.25) %>% 
  filter(Genus_spp %in% focal_spp) %>% 
  pivot_wider(id_cols = Module, names_from = Genus_spp, values_from = dens_100m2_4rt)

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
  geom_text(x = 0.28, y = 0.22, label = c(paste("2D Stress:" ,round(NMDS_comm_fish_ht_mod_18$stress,2), sep = " ")), color = "black") +
  guides(shape = guide_legend(title = "Construction Phase"),
         fill = guide_legend(title = "Construction Phase")) +
  theme(legend.position = "top")


plot_wide_fish_ht_mod_18_hulls



ggsave("figures/module_assemblage.png", plot_wide_fish_ht_mod_18_hulls,
       width = 6, height = 8, dpi = 600)


#### Cluster Analysis for mod 18 ----
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
  labs(y = "Dissimilarity") 


plot_gg_mod_18_dend


ggsave("figures/module_dendrogram.png", plot_gg_mod_18_dend,
       width = 10, height = 10, dpi = 600)


# Determining number of significant clusters
# clustsig$simprof no longer actively maintained
# #simprof.comm_fish_ht_mod_18<-simprof(comm_fish_ht_mod_18,
#                                      num.expected=1000,
#                                      num.simulated=999,
#                                      method.distance="braycurtis",
#                                      alpha=0.05,
#                                      warn.braycurtis=F)

## TO DO - try to figure out if this is running correctly (really 1 sig group? etc)

# simprof.comm_fish_ht_mod_18
# summary(simprof.comm_fish_ht_mod_18)


# nMDS w/ construction date and/or 3_4 (likely to be able to show no apparent/strong patterns)


dis.comm_fish_ht_mod_18 <-vegdist(comm_fish_ht_mod_18)

### !!! Note that because design is unblanced, order of terms matters.
# ado.fish_ht_mod_18 <-adonis(dis.comm_fish_ht_mod_18 ~  mid_high_hgt_m + Orientation + construction_group, wide_fish_ht_mod_18)
# 
# ado.fish_ht_mod_18

#ado.fish_ht_mod_18 <- adonis2(dis.comm_fish_ht_mod_18 ~  mid_high_hgt_m + Orientation + construction_group, by = "margin", wide_fish_ht_mod_18)

ado.fish_ht_mod_18 <- adonis2(dis.comm_fish_ht_mod_18 ~  mid_high_hgt_m + construction_group, by = "margin", wide_fish_ht_mod_18)

# issue with how the permutations run because of overlap module level variables and possibly habitat type being nested in Module

#ado.fish_ht_mod_18


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




#### Community Composition OS + HT ----
dat_fish_os_ht <- dat_fish_t %>% 
  group_by(Habitat_Type, os, Genus_spp) %>% 
  summarise(dens_100m2 = mean(dens_100m2)) %>% 
  ungroup()


wide_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(dens_100m2_4rt = dens_100m2^0.25) %>% 
  filter(Genus_spp %in% focal_spp) %>% 
  pivot_wider(id_cols = Habitat_Type:os, names_from = Genus_spp, values_from = dens_100m2_4rt) %>% 
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
  mutate(cluster = (case_when(startsWith(os_ht, "West Perpendicular High Relief") ~ "Group 1",
                              startsWith(os_ht, "West Perpendicular High Ecotone") ~ "Group 4",
                              startsWith(os_ht, "West Perpendicular Low Relief") ~ "Group 5",
                              startsWith(os_ht, "West Perpendicular Low Ecotone") ~ "Group 5",
                              startsWith(os_ht, "West Perpendicular Medium Relief") ~ "Group 6",
                              startsWith(os_ht, "West Perpendicular Medium Ecotone") ~ "Group 6",
                              startsWith(os_ht, "East Perpendicular High Relief") ~ "Group 6",
                              startsWith(os_ht, "East Perpendicular High Ecotone") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Low Relief") ~ "Group 4",
                              startsWith(os_ht, "East Perpendicular Low Ecotone") ~ "Group 5",
                              startsWith(os_ht, "East Perpendicular Medium Relief") ~ "Group 5",
                              startsWith(os_ht, "East Perpendicular Medium Ecotone") ~ "Group 5",
                              startsWith(os_ht, "West Parallel High Relief") ~ "Group 6",
                              startsWith(os_ht, "West Parallel High Ecotone") ~ "Group 6",
                              startsWith(os_ht, "West Parallel Low Relief") ~ "Group 5",
                              startsWith(os_ht, "West Parallel Low Ecotone") ~ "Group 4",
                              startsWith(os_ht, "West Parallel Medium Relief") ~ "Group 6",
                              startsWith(os_ht, "West Parallel Medium Ecotone") ~ "Group 4",
                              startsWith(os_ht, "East Parallel High Relief") ~ "Group 5",
                              startsWith(os_ht, "East Parallel High Ecotone") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Low Relief") ~ "Group 4",
                              startsWith(os_ht, "East Parallel Low Ecotone") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Medium Relief") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Medium Ecotone") ~ "Group 3")))



wide_fish_os_ht <- wide_fish_os_ht %>%
  mutate(Habitat_Type_2 = str_replace_all(Habitat_Type, "_", " "))


wide_fish_os_ht <- wide_fish_os_ht %>%
  mutate(cluster_2 = cluster)

wide_fish_os_ht <- wide_fish_os_ht %>%
  mutate(cluster_2 = str_replace_all(cluster_2, "Group 1", "Group 1: Perpendicular Inshore High Relief"),
         cluster_2 = str_replace_all(cluster_2, "Group 2", "Group 2: Perpendicular Offshore High Ecotone"),
         cluster_2 = str_replace_all(cluster_2, "Group 3", "Group 3: Sheltered Inshore Parallels"),
         cluster_2 = str_replace_all(cluster_2, "Group 4", "Group 4: Low Relief & Ecotones"),
         cluster_2 = str_replace_all(cluster_2, "Group 5", "Group 5: Intermediate"),
         cluster_2 = str_replace_all(cluster_2, "Group 6", "Group 6: Offshore High & Medium Relief"))


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
  mutate(Module = factor(Habitat_Type_2, levels = c("High Relief",
                                            "Medium Relief",
                                            "Low Relief",
                                            "High Ecotone",
                                            "Medium Ecotone",
                                            "Low Ecotone")))


plot_wide_fish_os_ht <- ggplot(wide_fish_os_ht,
                               aes(-MDS1, -MDS2)) +
  geom_text(aes(label = os_lab), vjust = 2, hjust = .55, size = 4) +
  geom_point(aes(color = cluster_2, shape = Module),size = 6) +
  scale_shape_manual(values = c(15, 16, 17, 0, 1, 2)) +
  scale_color_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
  theme_classic() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = NA, color = "black", size = 1, linetype = "solid")) +
  theme(plot.margin = margin(1,2,1,1, "cm")) +
  theme(text = element_text(size = 16)) +
  guides(shape = guide_legend(title = "Habitat Type"),
         color = "none") 

#plot_wide_fish_os_ht
# hull_os_ht <- wide_fish_os_ht %>% 
#   group_by(Zone) %>% 
#   slice(chull(-NMDS1, -NMDS2))
# 
# 
# 
# plot_wide_fish_os_ht_hulls <- plot_wide_fish_os_ht +
#   geom_polygon(data = hull_os_ht, aes(x = -NMDS1, y = -NMDS2, group = Zone, fill = Zone),alpha = 0.3)
# 
# plot_wide_fish_os_ht_hulls

hull_os_ht <- wide_fish_os_ht %>% 
  group_by(cluster) %>% 
  slice(chull(-MDS1, -MDS2))



plot_wide_fish_os_ht_hulls <- plot_wide_fish_os_ht +
  geom_polygon(data = hull_os_ht, aes(x = -MDS1, y = -MDS2, group = cluster_2, fill = cluster_2),alpha = 0.3) + 
  scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
  geom_text(x = 0.21, y = 0.17, label = c(paste("2D Stress:" ,round(NMDS_comm_fish_os_ht$stress,2), sep = " ")), color = "black", size = 8) +
  # guides(fill = guide_legend(title = "Cluster Group"))
guides(fill = guide_legend(
  title = "Cluster Group",
  override.aes = list(alpha = 1)  # Set alpha to 1 to remove transparency in legend
))


plot_wide_fish_os_ht_hulls



site_scores <- wide_fish_os_ht %>%
  select(Habitat_Type, os, Zone, MDS1, MDS2, cluster, os_ht) %>% 
  column_to_rownames(var = "os_ht")

os_ht_spp_fit <- envfit(NMDS_comm_fish_os_ht, comm_fish_os_ht, permutations = 999)

spp_scrs <- as_tibble(scores(os_ht_spp_fit, display = "vectors"), rownames = "Species") %>% 
  mutate(pval = os_ht_spp_fit$vectors$pvals) %>% 
  filter(pval <= 0.01 )

spp_scrs <- spp_scrs %>% 
  mutate(common_name = str_replace_all(Species, "_", " "))

spp_scrs <- spp_scrs %>% 
  mutate(common_name =  case_when(startsWith(common_name, "Ch") ~ "Blacksmith",
                                  startsWith(common_name, "Emb") ~ "Black Perch",
                                  startsWith(common_name, "Gir") ~ "Opaleye",
                                  startsWith(common_name, "Oxy") ~ "Senorita",
                                  startsWith(common_name, "Para") ~ "Barred Sand Bass",
                                  startsWith(common_name, "Seb") ~ "Rock Wrasse",
                                  startsWith(common_name, "Sem") ~ "CA Sheephead"))

plot_NMDS_os_ht_spp_vect <- plot_wide_fish_os_ht_hulls +
  geom_segment(data = spp_scrs, aes(x = 0, xend = -NMDS1*.275, y = 0, yend = -NMDS2*.27, shape = NULL),
               arrow = arrow(length = unit(.25, "cm")),
               color = "grey10", lwd = 0.3) +
  geom_text(data = spp_scrs, aes(x = -NMDS1*.27, y = -NMDS2*.27, label = common_name, shape = NULL), fontface = "bold", color = "black", size = 6) +
  theme(legend.position = "top", legend.direction = "vertical")



plot_NMDS_os_ht_spp_vect



ggsave("figures/submodule_assemblage.png", plot_NMDS_os_ht_spp_vect,
       width = 12, height = 12, dpi = 600)

spp_scrs <- spp_scrs %>% 
  arrange(pval)

spp_scrs

spp_scrs_ft <- flextable(spp_scrs,
                         col_keys = c("Species", "pval")) %>% 
  set_header_labels(Species = "NMDS1",
                    P = "pval") 

#spp_scrs_ft



#### Cluster Analysis for OS_HT ----
wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(dend_lab = paste(Orientation, current, Habitat_Type))


wide_fish_os_ht <- wide_fish_os_ht %>%
  mutate(dend_lab = str_replace(dend_lab, "Parallel Up-current High Ecotone", "Par. Offshore High Ecotone"),
         dend_lab = str_replace(dend_lab, "Parallel Up-current High Relief", "Par. Offshore High Relief"),
         dend_lab = str_replace(dend_lab, "Parallel Up-current Medium Ecotone", "Par. Offshore Medium Ecotone"),
         dend_lab = str_replace(dend_lab, "Parallel Up-current Medium Relief", "Par. Offshore Medium Relief"),
         dend_lab = str_replace(dend_lab, "Parallel Up-current Low Ecotone", "Par. Offshore Low Ecotone"),
         dend_lab = str_replace(dend_lab, "Parallel Up-current Low Relief", "Par. Offshore Low Relief"),
         dend_lab = str_replace(dend_lab, "Parallel Down-current High Ecotone", "Par. Inshore High Ecotone"),
         dend_lab = str_replace(dend_lab, "Parallel Down-current High Relief", "Par. Inshore High Relief"),
         dend_lab = str_replace(dend_lab, "Parallel Down-current Medium Ecotone", "Par. Inshore Medium Ecotone"),
         dend_lab = str_replace(dend_lab, "Parallel Down-current Medium Relief", "Par. Inshore Medium Relief"),
         dend_lab = str_replace(dend_lab, "Parallel Down-current Low Ecotone", "Par. Inshore Low Ecotone"),
         dend_lab = str_replace(dend_lab, "Parallel Down-current Low Relief", "Par. Inshore Low Relief"),
         dend_lab = str_replace(dend_lab, "Perpendicular Up-current High Ecotone", "Perp. West High Ecotone"),
         dend_lab = str_replace(dend_lab, "Perpendicular Up-current High Relief", "Perp. West High Relief"),
         dend_lab = str_replace(dend_lab, "Perpendicular Up-current Medium Ecotone", "Perp. West Medium Ecotone"),
         dend_lab = str_replace(dend_lab, "Perpendicular Up-current Medium Relief", "Perp. West Medium Relief"),
         dend_lab = str_replace(dend_lab, "Perpendicular Up-current Low Ecotone", "Perp. West Low Ecotone"),
         dend_lab = str_replace(dend_lab, "Perpendicular Up-current Low Relief", "Perp. West Low Relief"),
         dend_lab = str_replace(dend_lab, "Perpendicular Down-current High Ecotone", "Perp. East High Ecotone"),
         dend_lab = str_replace(dend_lab, "Perpendicular Down-current High Relief", "Perp. East High Relief"),
         dend_lab = str_replace(dend_lab, "Perpendicular Down-current Medium Ecotone", "Perp. East Medium Ecotone"),
         dend_lab = str_replace(dend_lab, "Perpendicular Down-current Medium Relief", "Perp. East Medium Relief"),
         dend_lab = str_replace(dend_lab, "Perpendicular Down-current Low Ecotone", "Perp. East Low Ecotone"),
         dend_lab = str_replace(dend_lab, "Perpendicular Down-current Low Relief", "Perp. East Low Relief"))
#          






comm_fish_os_ht <- wide_fish_os_ht %>%
  column_to_rownames(var = "dend_lab") %>%
  select(Chromis_punctipinnis:Semicossyphus_pulcher)



#Create a distance matrix based on the community assemblages
dis.comm_fish_os_ht <- vegdist(comm_fish_os_ht)

#Create a cluster dendrogram
clust.comm_fish_os_ht <- hclust(dis.comm_fish_os_ht, "average")

#Add color labeles and branches based on the NMDS groups
os_ht_dendro <- color_labels(clust.comm_fish_os_ht, col = c("khaki4", "black", "purple4", "steelblue3", "springgreen4", "coral2"), k = 6)

os_ht_dendro <- color_branches(os_ht_dendro, col = c("khaki4", "black", "purple4", "steelblue3", "springgreen4", "coral2"), k = 6)
# %>% 
#   set("labels_cex", 1.8) %>% 
#   set("leaves_pch", c(15,15,16,16,16,16,17,17,17,17,17,18,18,18,18,18,18,18,19,19,19,19,19,19)) %>% 
#   set("leaves_col", c("black", "khaki4","purple4","purple4","purple4","purple4","steelblue3","steelblue3","steelblue3","steelblue3","steelblue3","springgreen4","springgreen4","springgreen4","springgreen4","springgreen4","springgreen4","springgreen4","coral2","coral2","coral2","coral2","coral2","coral2"))




os_ht_dendro <- os_ht_dendro %>% 
  rotate(24:1) 


os_ht_dendro

gg_os_ht_dend <- as.ggdend(os_ht_dendro)
plot_gg_os_ht_dend <- ggplot(gg_os_ht_dend, horiz = T, offset_labels = -0.01)
plot_gg_os_ht_dend <- plot_gg_os_ht_dend +
  theme_classic() +
  theme(plot.margin = margin(1,1,1,1, "cm")) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(), axis.title.x = element_text(vjust = - 1.2, hjust = .5)) +
  theme(text = element_text(size = 24)) +
  scale_y_reverse(breaks = c(0.3,0.2,0.1,0), expand=c(0,.1,0,.1)) + 
  labs(y = "Dissimilarity")




plot_gg_os_ht_dend

ggsave("figures/submodule_dendrogram.png", plot_gg_os_ht_dend,
       width = 13, height = 7.5, dpi = 600)

# Determining number of significant clusters
# No longer supported looking for alternative
#simprof.comm_fish_os_ht <- simprof(comm_fish_os_ht,
                                   # num.expected=1000,
                                   # num.simulated=999,
                                   # method.distance="braycurtis",
                                   # alpha=0.05,
                                   # warn.braycurtis=F)


# simprof.comm_fish_os_ht
# summary(simprof.comm_fish_os_ht)









#### Adonis OS_HT ----
dis.comm_fish_os_ht <- vegdist(comm_fish_os_ht)

# adonis2(dis.comm_fish_os_ht ~ Habitat_Type + os, wide_fish_os_ht)
# adonis2(dis.comm_fish_os_ht ~ os + Habitat_Type, wide_fish_os_ht)
# adonis2(dis.comm_fish_os_ht ~ Habitat_Type + os, by = "margin", wide_fish_os_ht)
# 
# ado2.fish_os_ht <-adonis2(dis.comm_fish_os_ht ~ Habitat_Type + os, by = "margin", wide_fish_os_ht)
# ado2.fish_os_ht

# adonis2(dis.comm_fish_os_ht ~ Habitat_Type + os + cluster,  wide_fish_os_ht)
# adonis2(dis.comm_fish_os_ht ~ cluster + os + Habitat_Type,  wide_fish_os_ht)
# adonis2(dis.comm_fish_os_ht ~ os + cluster, by = "margin", wide_fish_os_ht)
# adonis2(dis.comm_fish_os_ht ~ Habitat_Type + os + cluster, by = "margin", wide_fish_os_ht)
# 
# adonis2(dis.comm_fish_os_ht ~ Habitat_Type, wide_fish_os_ht)
adonis2(dis.comm_fish_os_ht ~ cluster, wide_fish_os_ht)

ado2.fish_os_ht_clus <-adonis2(dis.comm_fish_os_ht ~ Habitat_Type + os + cluster, by = "margin", wide_fish_os_ht)

ado2.fish_os_ht_clus

### UPDATE BELOW
ado2.fish_os_ht_clus_table <- as.data.frame(ado2.fish_os_ht_clus)
ado2.fish_os_ht_clus_table <- ado2.fish_os_ht_clus_table %>% 
  rownames_to_column() %>% 
  mutate(rowname = str_replace(rowname, "Habitat_Type", "Habitat Type"),
         rowname = str_replace(rowname, "os", "Orientation & Side"),
         rowname = str_replace(rowname, "cluster", "Cluster"))

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
  mutate(cluster = (case_when(startsWith(os_ht, "West Perpendicular High Relief") ~ "Group 1",
                              startsWith(os_ht, "West Perpendicular High Ecotone") ~ "Group 4",
                              startsWith(os_ht, "West Perpendicular Low Relief") ~ "Group 5",
                              startsWith(os_ht, "West Perpendicular Low Ecotone") ~ "Group 5",
                              startsWith(os_ht, "West Perpendicular Medium Relief") ~ "Group 6",
                              startsWith(os_ht, "West Perpendicular Medium Ecotone") ~ "Group 6",
                              startsWith(os_ht, "East Perpendicular High Relief") ~ "Group 6",
                              startsWith(os_ht, "East Perpendicular High Ecotone") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Low Relief") ~ "Group 4",
                              startsWith(os_ht, "East Perpendicular Low Ecotone") ~ "Group 5",
                              startsWith(os_ht, "East Perpendicular Medium Relief") ~ "Group 5",
                              startsWith(os_ht, "East Perpendicular Medium Ecotone") ~ "Group 5",
                              startsWith(os_ht, "West Parallel High Relief") ~ "Group 6",
                              startsWith(os_ht, "West Parallel High Ecotone") ~ "Group 6",
                              startsWith(os_ht, "West Parallel Low Relief") ~ "Group 5",
                              startsWith(os_ht, "West Parallel Low Ecotone") ~ "Group 4",
                              startsWith(os_ht, "West Parallel Medium Relief") ~ "Group 6",
                              startsWith(os_ht, "West Parallel Medium Ecotone") ~ "Group 4",
                              startsWith(os_ht, "East Parallel High Relief") ~ "Group 5",
                              startsWith(os_ht, "East Parallel High Ecotone") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Low Relief") ~ "Group 4",
                              startsWith(os_ht, "East Parallel Low Ecotone") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Medium Relief") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Medium Ecotone") ~ "Group 3")))




# Stacked bar plot with total densities observed

common_name_vect <- c("Blacksmith",
                      "Kelp Bass",
                      "Senorita",
                      "California Sheephead",
                      "Black Perch",
                      "Rainbow Seaperch",
                      "Barred Sand Bass",
                      "Opaleye",
                      "Pile Perch",
                      "Olive Rockfish",
                      "Rock Wrasse")

dat_fish_os_ht <- dat_fish_os_ht %>%
  mutate(cluster_2 = cluster)

dat_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(current =  case_when(startsWith(os, "West") ~ "Up-current",
                              startsWith(os, "East") ~ "Down-current"))

dat_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(Orientation =  case_when(startsWith(os, "West Par") ~ "Parallel",
                                  startsWith(os, "West Per") ~ "Perpendicular",
                                  startsWith(os, "East Par") ~ "Parallel",
                                  startsWith(os, "East Per") ~ "Perpendicular"))

dat_fish_os_ht <- dat_fish_os_ht %>%
  mutate(cluster_2 = str_replace_all(cluster_2, "Group 1", "Group 1: Perpendicular Up-current High Relief"),
         cluster_2 = str_replace_all(cluster_2, "Group 2", "Group 2: Perpendicular Down-current High Ecotone"),
         cluster_2 = str_replace_all(cluster_2, "Group 3", "Group 3: Sheltered Inshore Parallels"),
         cluster_2 = str_replace_all(cluster_2, "Group 4", "Group 4: Low Relief & Ecotones"),
         cluster_2 = str_replace_all(cluster_2, "Group 5", "Group 5: Intermediate"),
         cluster_2 = str_replace_all(cluster_2, "Group 6", "Group 6: Offshore Exposed High & Medium Relief"))

dat_fish_os_ht <- dat_fish_os_ht %>%
  mutate(stack_lab = paste(Orientation, current, Habitat_Type))

dat_fish_os_ht <- dat_fish_os_ht %>%
  mutate(stack_lab = str_replace(stack_lab, "Parallel Up-current High Ecotone", "Par. Offshore High Ecotone"),
         stack_lab = str_replace(stack_lab, "Parallel Up-current High Relief", "Par. Offshore High Relief"),
         stack_lab = str_replace(stack_lab, "Parallel Up-current Medium Ecotone", "Par. Offshore Medium Ecotone"),
         stack_lab = str_replace(stack_lab, "Parallel Up-current Medium Relief", "Par. Offshore Medium Relief"),
         stack_lab = str_replace(stack_lab, "Parallel Up-current Low Ecotone", "Par. Offshore Low Ecotone"),
         stack_lab = str_replace(stack_lab, "Parallel Up-current Low Relief", "Par. Offshore Low Relief"),
         stack_lab = str_replace(stack_lab, "Parallel Down-current High Ecotone", "Par. Inshore High Ecotone"),
         stack_lab = str_replace(stack_lab, "Parallel Down-current High Relief", "Par. Inshore High Relief"),
         stack_lab = str_replace(stack_lab, "Parallel Down-current Medium Ecotone", "Par. Inshore Medium Ecotone"),
         stack_lab = str_replace(stack_lab, "Parallel Down-current Medium Relief", "Par. Inshore Medium Relief"),
         stack_lab = str_replace(stack_lab, "Parallel Down-current Low Ecotone", "Par. Inshore Low Ecotone"),
         stack_lab = str_replace(stack_lab, "Parallel Down-current Low Relief", "Par. Inshore Low Relief"),
         stack_lab = str_replace(stack_lab, "Perpendicular Up-current High Ecotone", "Perp. West High Ecotone"),
         stack_lab = str_replace(stack_lab, "Perpendicular Up-current High Relief", "Perp. West High Relief"),
         stack_lab = str_replace(stack_lab, "Perpendicular Up-current Medium Ecotone", "Perp. West Medium Ecotone"),
         stack_lab = str_replace(stack_lab, "Perpendicular Up-current Medium Relief", "Perp. West Medium Relief"),
         stack_lab = str_replace(stack_lab, "Perpendicular Up-current Low Ecotone", "Perp. West Low Ecotone"),
         stack_lab = str_replace(stack_lab, "Perpendicular Up-current Low Relief", "Perp. West Low Relief"),
         stack_lab = str_replace(stack_lab, "Perpendicular Down-current High Ecotone", "Perp. East High Ecotone"),
         stack_lab = str_replace(stack_lab, "Perpendicular Down-current High Relief", "Perp. East High Relief"),
         stack_lab = str_replace(stack_lab, "Perpendicular Down-current Medium Ecotone", "Perp. East Medium Ecotone"),
         stack_lab = str_replace(stack_lab, "Perpendicular Down-current Medium Relief", "Perp. East Medium Relief"),
         stack_lab = str_replace(stack_lab, "Perpendicular Down-current Low Ecotone", "Perp. East Low Ecotone"),
         stack_lab = str_replace(stack_lab, "Perpendicular Down-current Low Relief", "Perp. East Low Relief"))

clust_col <- c("khaki4", "black", "purple4","purple4","purple4","purple4", "steelblue3","steelblue3","steelblue3","steelblue3","steelblue3", "springgreen4","springgreen4","springgreen4","springgreen4","springgreen4","springgreen4","springgreen4", "coral2", "coral2", "coral2", "coral2", "coral2", "coral2")


clust_col_2 <- c("coral2","coral2","coral2","coral2","coral2","coral2","springgreen4","springgreen4","springgreen4","springgreen4","springgreen4","springgreen4","springgreen4","steelblue3","steelblue3","steelblue3","steelblue3","steelblue3","purple4","purple4","purple4","purple4","khaki4","black")

dat_fish_os_ht <- dat_fish_os_ht %>%
  mutate(group_lab = paste(cluster, stack_lab))








# os_ht_order <- c("Group 1 Perpendicular Up-current High Relief",
#                  "Group 2 Perpendicular Down-current High Ecotone",
#                  "Group 3 Parallel Down-current High Ecotone",
#                  "Group 3 Parallel Down-current Low Ecotone",
#                  "Group 3 Parallel Down-current Medium Ecotone",
#                  "Group 3 Parallel Down-current Medium Relief",
#                  "Group 4 Perpendicular Up-current High Ecotone",
#                  "Group 4 Parallel Down-current Low Relief",
#                  "Group 4 Parallel Up-current Medium Ecotone",
#                  "Group 4 Perpendicular Down-current Low Relief",
#                  "Group 4 Parallel Up-current Low Ecotone",
#                  "Group 5 Perpendicular Down-current Low Ecotone",
#                  "Group 5 Perpendicular Down-current Medium Ecotone",
#                  "Group 5 Parallel Up-current Low Relief",
#                  "Group 5 Perpendicular Down-current Medium Relief",
#                  "Group 5 Perpendicular Up-current Low Relief",
#                  "Group 5 Parallel Down-current High Relief",
#                  "Group 5 Perpendicular Up-current Low Ecotone",
#                  "Group 6 Parallel Up-current High Relief",
#                  "Group 6 Perpendicular Down-current High Relief",
#                  "Group 6 Parallel Up-current Medium Relief",
#                  "Group 6 Perpendicular Up-current Medium Relief",
#                  "Group 6 Parallel Up-current High Ecotone",
#                  "Group 6 Perpendicular Up-current Medium Ecotone")



# plot_dens_total_os_ht <- dat_fish_os_ht %>% 
#   mutate(stack_lab = fct_relevel(stack_lab, levels = c(
#     "Perp. West Medium Ecotone",
#     "Par. Offshore High Ecotone",
#     "Perp. West Medium Relief",
#     "Par. Offshore Medium Relief",
#     "Perp. East High Relief",
#     "Par. Offshore High Relief",
#     
#     "Perp. West Low Ecotone",
#     "Perp. East Low Ecotone",
#     "Perp. East Medium Ecotone",
#     "Perp. West Low Relief",
#     "Par. Offshore Low Relief",
#     "Perp. East Medium Relief",
#     "Par. Inshore High Relief",
#     
#     "Par. Offshore Low Ecotone",
#     "Par. Offshore Medium Ecotone",
#     "Perp. West High Ecotone",
#     "Par. Inshore Low Relief",
#     "Perp. East Low Relief",
#     
#     "Par. Inshore Low Ecotone",
#     "Par. Inshore Medium Ecotone",
#     "Par. Inshore High Ecotone",
#     "Par. Inshore Medium Relief",
#     
#     "Perp. East High Ecotone",
#     
#     "Perp. West High Relief"))) %>% 
#   filter(Genus_spp %in% focal_spp) %>%
#   group_by(stack_lab, Genus_spp) %>% 
#   summarize(total_dens = sum(dens_100m2)) %>% 
#   ggplot(aes(x = stack_lab, y = total_dens, fill = Genus_spp)) +
#   geom_bar(stat = "identity", colour = "black") +
#   coord_flip() + 
#   theme_classic() +
#   theme(legend.position = "top") +
#   guides(fill = guide_legend(reverse= TRUE)) + 
#   labs(x = "Transect Type", y = expression(paste("Mean Density (No./100",m^{2},")"))) +
#   theme(legend.text = element_text(size = 12, face = "bold"), axis.text.y = element_text(color = clust_col_2)) +
#   guides(fill = guide_legend(title = "Focal Fish Species")) +
#   scale_x_discrete(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   scale_fill_brewer(palette = "Set3", labels = c("Blacksmith", "Pile Perch", "Black Perch", "Opaleye", "Rock Wrasse", "Rainbow Seaperch", "Senorita", "Kelp Bass", "Barred Sand Bass", "Olive Rockfish", "California Sheephead")) +
#   theme(legend.position = c(.7,.5), legend.direction = "vertical", legend.background = element_rect(fill = "white", color = "black"), legend.text = element_text(size = 16))
# 
# 
# 
# plot_dens_total_os_ht


# ggsave("plot_dens_total_os_ht.png", plot_dens_total_os_ht,
#        width = 8, height = 8, dpi = 600)


#proportional stacked bar plot with densities set to be percentages 
# plot_dens_prop_os_ht <- dat_fish_os_ht %>% 
#   mutate(stack_lab = fct_relevel(stack_lab, levels = c(
#     "Perp. UC Medium Ecotone",
#     "Par. UC High Ecotone",
#     "Perp. UC Medium Relief",
#     "Par. UC Medium Relief",
#     "Perp. DC High Relief",
#     "Par. UC High Relief",
#     
#     "Perp. UC Low Ecotone",
#     "Perp. DC Low Ecotone",
#     "Perp. DC Medium Ecotone",
#     "Perp. UC Low Relief",
#     "Par. UC Low Relief",
#     "Perp. DC Medium Relief",
#     "Par. DC High Relief",
#     
#     "Par. UC Low Ecotone",
#     "Par. UC Medium Ecotone",
#     "Perp. UC High Ecotone",
#     "Par. DC Low Relief",
#     "Perp. DC Low Relief",
#     
#     "Par. DC Low Ecotone",
#     "Par. DC Medium Ecotone",
#     "Par. DC High Ecotone",
#     "Par. DC Medium Relief",
#     
#     "Perp. DC High Ecotone",
#     
#     "Perp. UC High Relief"))) %>% 
#   filter(Genus_spp %in% focal_spp) %>%
#   group_by(stack_lab, Genus_spp) %>% 
#   summarize(total_dens = sum(dens_100m2)) %>% 
#   ggplot(aes(x = stack_lab, y = total_dens, fill = Genus_spp)) +
#   geom_bar(stat = "identity", colour = "black", position = "fill") +
#   theme_classic() + 
#   coord_flip() +
#   theme(legend.position = "top") +
#   guides(fill = guide_legend(reverse= TRUE)) + 
#   labs(x = "Transect Type", y = expression(paste("Proportional Density (Percent/100",m^{2},")"))) +
#   theme(legend.text = element_text(size = 12, face = "bold"), axis.text.y = element_text(color = clust_col_2)) +
#   guides(fill = guide_legend(title = "Focal Species")) +
#   scale_x_discrete(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   scale_fill_brewer(palette = "Set3", labels = c("Blacksmith", "Pile Perch", "Black Perch", "Opaleye", "Rock Wrasse", "Rainbow Seaperch", "Senorita", "Kelp Bass", "Barred Sand Bass", "Olive Rockfish", "California Sheephead")) 
# 
# plot_dens_prop_os_ht
# 
# 
# ggsave("plot_dens_prop_os_ht.png", plot_dens_prop_os_ht,
#        width = 12, height = 8, dpi = 600)
# 



#### species specific for loops ----

dat_fish_spp <- dat_fish_t %>% 
  filter(Genus_spp %in% focal_spp)

dat_fish_spp = dat_fish_spp %>% 
  mutate(Habitat_Type = factor(Habitat_Type, levels = c("High Relief",
                                                        "Medium Relief",
                                                        "Low Relief",
                                                        "High Ecotone",
                                                        "Medium Ecotone",
                                                        "Low Ecotone")))

dat_fish_spp <- dat_fish_spp %>% 
  mutate(transect = paste(Module, Module_Side, Habitat_Type),
         dens_100m2_4rt = dens_100m2^0.25) %>% 
  left_join(transect_var)

dat_fish_spp <- dat_fish_spp %>% 
  mutate(construction_group = as.character(construction_group))

species = unique(dat_fish_spp$Genus_spp)
species_plots = list()

dat_fish_spp <- dat_fish_spp %>%
  mutate(cluster_2 = cluster)

dat_fish_spp <- dat_fish_spp %>% 
  mutate(os_ht = str_replace_all(os_ht, "Ecotone_High", "High Ecotone"),
         os_ht = str_replace_all(os_ht, "Ecotone_Medium", "Medium Ecotone"),
         os_ht = str_replace_all(os_ht, "Ecotone_Low", "Low Ecotone"),
         os_ht = str_replace_all(os_ht, "Mid_High", "High Relief"),
         os_ht = str_replace_all(os_ht, "Mid_Medium", "Medium Relief"),
         os_ht = str_replace_all(os_ht, "Mid_Low", "Low Relief"))

dat_fish_spp <- dat_fish_spp %>%
  mutate(cluster_2 = str_replace_all(cluster_2, "Group 1", "Group 1: Perpendicular Inshore High Relief"),
         cluster_2 = str_replace_all(cluster_2, "Group 2", "Group 2: Perpendicular Offshore High Ecotone"),
         cluster_2 = str_replace_all(cluster_2, "Group 3", "Group 3: Sheltered Inshore Parallels"),
         cluster_2 = str_replace_all(cluster_2, "Group 4", "Group 4: Low Relief & Ecotones"),
         cluster_2 = str_replace_all(cluster_2, "Group 5", "Group 5: Intermediate"),
         cluster_2 = str_replace_all(cluster_2, "Group 6", "Group 6: Offshore High & Medium Relief"))

wide_fish_os_ht <- wide_fish_os_ht %>%
  mutate(cluster_2 = str_replace_all(cluster_2, "Group 1", "Group 1: Perpendicular Inshore High Relief"),
         cluster_2 = str_replace_all(cluster_2, "Group 2", "Group 2: Perpendicular Offshore High Ecotone Ecotone"),
         cluster_2 = str_replace_all(cluster_2, "Group 3", "Group 3: Sheltered Inshore Parallels"),
         cluster_2 = str_replace_all(cluster_2, "Group 4", "Group 4: Low Relief & Ecotones"),
         cluster_2 = str_replace_all(cluster_2, "Group 5", "Group 5: Intermediate"),
         cluster_2 = str_replace_all(cluster_2, "Group 6", "Group 6: Offshore High & Medium Relief"))


wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(current =  case_when(startsWith(os, "West") ~ "Up-current",
                              startsWith(os, "East") ~ "Down-current"))

wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(Orientation =  case_when(startsWith(os, "West Par") ~ "Parallel",
                                  startsWith(os, "West Per") ~ "Perpendicular",
                                  startsWith(os, "East Par") ~ "Parallel",
                                  startsWith(os, "East Per") ~ "Perpendicular"))

dat_fish_spp <- dat_fish_spp %>% 
  mutate(t_type = paste(cluster, Orientation, current, Habitat_Type))

dat_fish_spp <- dat_fish_spp %>% 
  mutate(current =  case_when(startsWith(Module_Side, "West") ~ "Up-current",
                              startsWith(Module_Side, "East") ~ "Down-current"))
### TO DO - create os_ht_order 

# dat_fish_spp <- dat_fish_spp %>% 
#   arrange(cluster)

dat_fish_spp <- dat_fish_spp %>%
  mutate(stack_lab = paste(Orientation, current, Habitat_Type))

dat_fish_spp <- dat_fish_spp %>%
  mutate(stack_lab = str_replace(stack_lab, "Parallel Up-current High Ecotone", "Par. Offshore High Ecotone"),
         stack_lab = str_replace(stack_lab, "Parallel Up-current High Relief", "Par. Offshore High Relief"),
         stack_lab = str_replace(stack_lab, "Parallel Up-current Medium Ecotone", "Par. Offshore Medium Ecotone"),
         stack_lab = str_replace(stack_lab, "Parallel Up-current Medium Relief", "Par. Offshore Medium Relief"),
         stack_lab = str_replace(stack_lab, "Parallel Up-current Low Ecotone", "Par. Offshore Low Ecotone"),
         stack_lab = str_replace(stack_lab, "Parallel Up-current Low Relief", "Par. Offshore Low Relief"),
         stack_lab = str_replace(stack_lab, "Parallel Down-current High Ecotone", "Par. Inshore High Ecotone"),
         stack_lab = str_replace(stack_lab, "Parallel Down-current High Relief", "Par. Inshore High Relief"),
         stack_lab = str_replace(stack_lab, "Parallel Down-current Medium Ecotone", "Par. Inshore Medium Ecotone"),
         stack_lab = str_replace(stack_lab, "Parallel Down-current Medium Relief", "Par. Inshore Medium Relief"),
         stack_lab = str_replace(stack_lab, "Parallel Down-current Low Ecotone", "Par. Inshore Low Ecotone"),
         stack_lab = str_replace(stack_lab, "Parallel Down-current Low Relief", "Par. Inshore Low Relief"),
         stack_lab = str_replace(stack_lab, "Perpendicular Up-current High Ecotone", "Perp. West High Ecotone"),
         stack_lab = str_replace(stack_lab, "Perpendicular Up-current High Relief", "Perp. West High Relief"),
         stack_lab = str_replace(stack_lab, "Perpendicular Up-current Medium Ecotone", "Perp. West Medium Ecotone"),
         stack_lab = str_replace(stack_lab, "Perpendicular Up-current Medium Relief", "Perp. West Medium Relief"),
         stack_lab = str_replace(stack_lab, "Perpendicular Up-current Low Ecotone", "Perp. West Low Ecotone"),
         stack_lab = str_replace(stack_lab, "Perpendicular Up-current Low Relief", "Perp. West Low Relief"),
         stack_lab = str_replace(stack_lab, "Perpendicular Down-current High Ecotone", "Perp. East High Ecotone"),
         stack_lab = str_replace(stack_lab, "Perpendicular Down-current High Relief", "Perp. East High Relief"),
         stack_lab = str_replace(stack_lab, "Perpendicular Down-current Medium Ecotone", "Perp. East Medium Ecotone"),
         stack_lab = str_replace(stack_lab, "Perpendicular Down-current Medium Relief", "Perp. East Medium Relief"),
         stack_lab = str_replace(stack_lab, "Perpendicular Down-current Low Ecotone", "Perp. East Low Ecotone"),
         stack_lab = str_replace(stack_lab, "Perpendicular Down-current Low Relief", "Perp. East Low Relief"))


# common_names <- as.list(c("CA Sheephead", "Pile Perch", "Black Perch", "Opaleye","Rock Wrasse", "Rainbow Seaperch", "Señorita", "Kelp Bass", "Barred Sand Bass", "Olive Rockfish", "CA Sheephead"))

dat_fish_spp = dat_fish_spp %>% 
  mutate(cluster_2 = str_replace_all(cluster_2, "Group 1", "Group 1: Perpendicular West High Relief"),
         cluster_2 = str_replace_all(cluster_2, "Group 2", "Group 2: Perpendicular East High Ecotone"),
         cluster_2 = str_replace_all(cluster_2, "Group 3", "Group 3: Sheltered Inshore Parallels"),
         cluster_2 = str_replace_all(cluster_2, "Group 4", "Group 4: Low Relief & Ecotones"),
         cluster_2 = str_replace_all(cluster_2, "Group 5", "Group 5: Intermediate Density"),
         cluster_2 = str_replace_all(cluster_2, "Group 6", "Group 6: High Density"))


dat_fish_spp = dat_fish_spp %>% 
  mutate(stack_lab = factor(stack_lab, levels = c("Perp. West High Relief",
                                                        "Perp. East High Ecotone","Par. Inshore Medium Relief","Par. Inshore High Ecotone","Par. Inshore Medium Ecotone","Par. Inshore Low Ecotone","Perp. East Low Relief","Par. Inshore Low Relief", "Perp. West High Ecotone","Par. Offshore Medium Ecotone","Par. Offshore Low Ecotone", "Par. Inshore High Relief","Perp. East Medium Relief","Par. Offshore Low Relief","Perp. West Low Relief","Perp. East Medium Ecotone","Perp. East Low Ecotone","Perp. West Low Ecotone","Par. Offshore High Relief","Perp. East High Relief","Par. Offshore Medium Relief","Perp. West Medium Relief","Par. Offshore High Ecotone","Perp. West Medium Ecotone")))

dat_fish_spp = dat_fish_spp %>% 
  mutate(cluster_2 = str_replace_all(cluster, "Group 1", "CG 1: Perpendicular West High Relief"),
         cluster_2 = str_replace_all(cluster, "Group 2", "CG 2: Perpendicular East High Ecotone"),
         cluster_2 = str_replace_all(cluster, "Group 3", "CG 3: Sheltered Inshore Parallels"),
         cluster_2 = str_replace_all(cluster, "Group 4", "CG 4: Low Relief & Ecotones"),
         cluster_2 = str_replace_all(cluster, "Group 5", "CG 5: Intermediate Density"),
         cluster_2 = str_replace_all(cluster, "Group 6", "CG 6: High Density"))

dat_fish_spp = dat_fish_spp %>% 
  mutate(cluster_2 = str_replace_all(cluster_2, "Group 1", "CG 1: Perpendicular West High Relief"),
         cluster_2 = str_replace_all(cluster_2, "Group 2", "CG 2: Perpendicular East High Ecotone"),
         cluster_2 = str_replace_all(cluster_2, "Group 3", "CG 3: Sheltered Inshore Parallels"),
         cluster_2 = str_replace_all(cluster_2, "Group 4", "CG 4: Low Relief & Ecotones"),
         cluster_2 = str_replace_all(cluster_2, "Group 5", "CG 5: Intermediate Density"),
         cluster_2 = str_replace_all(cluster_2, "Group 6", "CG 6: High Density"))


dat_fish_spp <- dat_fish_spp %>% 
  mutate(Habitat_Type = factor(Habitat_Type, levels = c("High Relief",
                                                    "Medium Relief",
                                                    "Low Relief",
                                                    "High Ecotone",
                                                    "Medium Ecotone",
                                                    "Low Ecotone")))
# densities reported in OS_HT groups
for(species_ in species){
  #species_ <- "Chromis punctipinnis"# for testing
  species_plots[[species_]] <- dat_fish_spp %>%
    mutate(Module = factor(Habitat_Type, levels = c("High Relief",
                                                    "Medium Relief",
                                                    "Low Relief",
                                                    "High Ecotone",
                                                    "Medium Ecotone",
                                                    "Low Ecotone"))) %>% 
    # mutate(Habitat_Type = factor(stack_lab, levels = c(
    #   "High Relief",
    #   "Medium Relief",
    #   "Low Relief",
    #   "High Ecotone",
    #   "Medium Ecotone",
    #   "Low Ecotone")))
    filter(Genus_spp == species_) %>%
    ggplot(aes(x = stack_lab, y = dens_100m2, color = cluster_2, shape = Habitat_Type)) +
    scale_shape_manual(values = c(15, 16, 17, 0, 1, 2)) +
    geom_crossbar(stat = "summary", fun.data = mean_cl_boot,
                  #position = position_dodge(),
                  fill = "gray",
                  alpha = 0.5,
                  width = 0.4) +
    geom_point(position = position_jitter()) +
    theme_classic() +
    # ggtitle(bquote(italic(.(species))))+
    ggtitle(species_) +
    scale_color_manual(values = c("khaki4", "black", "purple4", "steelblue3", "springgreen4", "coral2")) +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 9)) +
    theme(plot.margin = margin(1,1,1,1, "cm")) +
    #theme(axis.text.x = element_text(color = clust_col)) +
    guides(shape = guide_legend(title = "Habitat Type"), color = guide_legend(title = "Cluster Group")) +
    theme(legend.position = c(0.6,.8), legend.direction = "vertical", legend.box = "horizontal", axis.text.x = element_text(color = clust_col)) +
    labs(x = "Cluster Groups of 24 Combinations of Reef Metrics,", y = expression(paste("Mean Focal Fish Density (No./100",m^{2},")", sep = "")))

  print(species_plots[[species_]])

  ggsave(paste("figures/spp_density_plot_", species_, ".png", sep=""), species_plots[[species_]],
         width = 9, height = 8, dpi = 600)
}
# 
# 
# # for (species_ in species) {
# #   png(paste("1st_spp_plot_", species_, ".png", sep=""), width = 800, height = 800, res = 120)
# #   print(species_plots[[species_]])
# #   dev.off()
# # }
# 
# 
# ###fix jitter with position jitter dodge
# dat_fish_spp <-  dat_fish_spp %>% 
#   mutate(ht_o = paste(Habitat_Type, Orientation))


# for(species_ in species){
#   species_plots[[species_]] <- dat_fish_spp %>%
#     mutate(ht_o = fct_relevel(ht_o, levels = c(
#       "High Relief Perpendicular",
#       "High Relief Parallel",
#       "Medium Relief Perpendicular",
#       "Medium Relief Parallel",
#       "Low Relief Perpendicular",
#       "Low Relief Parallel",
#       "High Ecotone Perpendicular",
#       "High Ecotone Parallel",
#       "Medium Ecotone Perpendicular",
#       "Medium Ecotone Parallel",
#       "Low Ecotone Perpendicular",
#       "Low Ecotone Parallel",
#       current = fct_relevel(current, levels = c(
#         "Up-current",
#         "Down-current"))))) %>% 
#     filter(Genus_spp == species_) %>% 
#     group_by(Orientation, current, Habitat_Type) %>% 
#     ggplot(aes(x = ht_o, y = dens_100m2, color = cluster_2, shape = current)) +
#     scale_shape_manual(values = c(15, 0)) +
#     geom_crossbar(stat = "summary", fun.data = mean_cl_boot,
#                   position = position_dodge(),
#                   fill = "gray",
#                   alpha = 0.5,
#                   width = 0.7) +
#     geom_point(position = position_jitterdodge(dodge.width = .2)) +
#     theme_classic() +
#     ggtitle(species_) +
#     scale_color_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
#     theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
#     theme(plot.margin = margin(1,1,1,1, "cm")) +
#     #theme(axis.text.x = element_text(color = clust_col)) +
#     guides(color = guide_legend(title = "Cluster Group"), shape = guide_legend(title = "Current Position")) +
#     theme(legend.position = "top") +
#     labs(x = "Habitat Type & Orientation", y = expression(paste("Fish Density (No./100",m^{2},")", sep = ""))) 
#   print(species_plots[[species_]])}
# 
# 
# for (species_ in species) {
#   png(paste("2nd_spp_plot_", species_, ".png", sep=""), width = 1600, height = 1600, res = 120)
#   print(species_plots[[species_]])
#   dev.off()
# }

#### Size distribution analysis
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
  mutate(cluster = (case_when(startsWith(os_ht, "West Perpendicular High Relief") ~ "Group 1",
                              startsWith(os_ht, "West Perpendicular High Ecotone") ~ "Group 4",
                              startsWith(os_ht, "West Perpendicular Low Relief") ~ "Group 5",
                              startsWith(os_ht, "West Perpendicular Low Ecotone") ~ "Group 5",
                              startsWith(os_ht, "West Perpendicular Medium Relief") ~ "Group 6",
                              startsWith(os_ht, "West Perpendicular Medium Ecotone") ~ "Group 6",
                              startsWith(os_ht, "East Perpendicular High Relief") ~ "Group 6",
                              startsWith(os_ht, "East Perpendicular High Ecotone") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Low Relief") ~ "Group 4",
                              startsWith(os_ht, "East Perpendicular Low Ecotone") ~ "Group 5",
                              startsWith(os_ht, "East Perpendicular Medium Relief") ~ "Group 5",
                              startsWith(os_ht, "East Perpendicular Medium Ecotone") ~ "Group 5",
                              startsWith(os_ht, "West Parallel High Relief") ~ "Group 6",
                              startsWith(os_ht, "West Parallel High Ecotone") ~ "Group 6",
                              startsWith(os_ht, "West Parallel Low Relief") ~ "Group 5",
                              startsWith(os_ht, "West Parallel Low Ecotone") ~ "Group 4",
                              startsWith(os_ht, "West Parallel Medium Relief") ~ "Group 6",
                              startsWith(os_ht, "West Parallel Medium Ecotone") ~ "Group 4",
                              startsWith(os_ht, "East Parallel High Relief") ~ "Group 5",
                              startsWith(os_ht, "East Parallel High Ecotone") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Low Relief") ~ "Group 4",
                              startsWith(os_ht, "East Parallel Low Ecotone") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Medium Relief") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Medium Ecotone") ~ "Group 3")))

dat_fish_l <- dat_fish_l %>%
  mutate(cluster_2 = cluster)

dat_fish_l <- dat_fish_l %>%
  mutate(cluster_2 = str_replace_all(cluster_2, "Group 1", "Group 1: Perpendicular Inshore High Relief"),
         cluster_2 = str_replace_all(cluster_2, "Group 2", "Group 2: Perpendicular Offshore High Ecotone"),
         cluster_2 = str_replace_all(cluster_2, "Group 3", "Group 3: Sheltered Inshore Parallels"),
         cluster_2 = str_replace_all(cluster_2, "Group 4", "Group 4: Low Relief & Ecotones"),
         cluster_2 = str_replace_all(cluster_2, "Group 5", "Group 5: Intermediate"),
         cluster_2 = str_replace_all(cluster_2, "Group 6", "Group 6: Offshore High & Medium Relief"))

dat_fish_l <- dat_fish_l %>% 
  mutate(t_type = paste(cluster,Orientation, current, Habitat_Type))



chromis_size <- dat_fish_l %>%
  filter(Genus_spp == "Chromis punctipinnis",
         Length < 260) %>%
  group_by(cluster) %>%
  ggplot(aes(x = Length, fill = cluster)) +
  geom_histogram(position = "identity", alpha = 0.3, bins = 30) +
  theme_classic() +
  ggtitle("Chomis punctipinnis") +
  scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2"))


# chromis_size

# 
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

#Change y axis label (proportional to transect numbers)

chromis_size_hist <- dat_fish_l %>%
  # mutate(Habitat_Type = fct_relevel(Habitat_Type, levels = c(
  #   "High Relief",
  #   "Medium Relief",
  #   "Low Relief",
  #   "High Ecotone",
  #   "Medium Ecotone",
  #   "Low Ecotone"))) %>% 
  filter(Genus_spp == "Chromis punctipinnis") %>%
  ggplot(aes(x = Length, fill = cluster_2)) + 
  geom_histogram(binwidth = 5, position = "stack", alpha = 0.7) +
  scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Cluster Group")) +
  labs(x = "Total Length (mm)", y = "Chromis Abundance") +
  theme(legend.position = "top", legend.direction = "horizontal") +
  facet_grid(rows = vars(Habitat_Type))
# , scales = "free_y")


chromis_size_hist


# ggsave("figures/chromis_size_hist.png", chromis_size_hist,
#        width = 9, height = 10, dpi = 600)

kelp_bass_size_hist <- dat_fish_l %>%
  # mutate(Habitat_Type = fct_relevel(Habitat_Type, levels = c(
  #   "High Relief",
  #   "Medium Relief",
  #   "Low Relief",
  #   "High Ecotone",
  #   "Medium Ecotone",
  #   "Low Ecotone"))) %>% 
  filter(Genus_spp == "Paralabrax clathratus") %>%
  ggplot(aes(x = Length, fill = cluster_2)) +
  geom_histogram(binwidth = 25, position = "stack", alpha = 0.7) +
  scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Cluster Group")) +
  labs(x = "Total Length (mm)", y = "Kelp Bass Abundance") +
  theme(legend.position = "top", legend.direction = "horizontal") +
  facet_grid(rows = vars(Habitat_Type))
# , scales = "free_y")

kelp_bass_size_hist

# ggsave("figures/kelp_bass_size_hist.png", kelp_bass_size_hist,
#        width = 9, height = 10, dpi = 600)


sheephead_size_hist <- dat_fish_l %>%
  # mutate(Habitat_Type = fct_relevel(Habitat_Type, levels = c(
  #   "High Relief",
  #   "Medium Relief",
  #   "Low Relief",
  #   "High Ecotone",
  #   "Medium Ecotone",
  #   "Low Ecotone"))) %>% 
  filter(Genus_spp == "Semicossyphus pulcher") %>%
  ggplot(aes(x = Length, fill = cluster_2)) +
  geom_histogram(position = "stack", alpha = 0.7, binwidth = 20) +
  scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Cluster Group")) +
  labs(x = "Total Length (mm)", y = "Sheephead Abundance") +
  theme(legend.position = "top", legend.direction = "horizontal") +
  facet_grid(rows = vars(Habitat_Type))
# , scales = "free_y")


sheephead_size_hist


# ggsave("figures/sheephead_size_hist.png", sheephead_size_hist,
#        width = 9, height = 10, dpi = 600)
### making kernal density curves

#No stacked positions
chromis_dens_curve <- dat_fish_l %>% 
  filter(Genus_spp == "Chromis punctipinnis") %>% 
  ggplot(aes(x = Length, fill = cluster_2)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Cluster Group")) +
  labs(x = "Blacksmith Total Length (mm)", y = "Denisty") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20), legend.position = "none")

chromis_dens_curve

ggsave("figures/density_curve_chromis.png", chromis_dens_curve,
       width = 10, height = 6, dpi = 600)


kelp_bass_dens_curve <- dat_fish_l %>%  
  filter(Genus_spp == "Paralabrax clathratus") %>% 
  ggplot(aes(x = Length, fill = cluster_2)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Cluster Group")) +
  labs(x = "Kelp Bass Total Length (mm)", y = "Denisty")+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20), legend.position = "none")

ggsave("figures/density_curve_kelp_bass.png", kelp_bass_dens_curve,
       width = 12, height = 9, dpi = 600)


sheephead_dens_curve <- dat_fish_l %>% 
  filter(Genus_spp == "Semicossyphus pulcher") %>% 
  ggplot(aes(x = Length, fill = cluster_2)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Cluster Group")) +
  labs(x = "Sheephead Total Length (mm)", y = "Denisty")+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20), legend.position = "none")

ggsave("figures/density_curve_sheephead.png", sheephead_dens_curve,
       width = 12, height = 9, dpi = 600)

#### stacked positions

chromis_stacked <- dat_fish_l %>% 
  filter(Genus_spp == "Chromis punctipinnis") %>% 
  ggplot(aes(x = Length, fill = cluster_2)) +
  geom_density(position = "stack", alpha = 0.5) +
  scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Cluster Group")) +
  labs(x = "Blacksmith Total Length (mm)", y = "Denisty")+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20), legend.position = "none")

# ggsave("figures/chromis_stacked.png", chromis_stacked,
#        width = 12, height = 9, dpi = 600)


kelp_bass_stacked <- dat_fish_l %>% 
  filter(Genus_spp == "Paralabrax clathratus") %>% 
  ggplot(aes(x = Length, fill = cluster_2)) +
  geom_density(position = "stack", alpha = 0.5) +
  scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Cluster Group")) +
  labs(x = "Kelp Bass Total Length (mm)", y = "Denisty")+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20), legend.position = "none") 

# ggsave("figures/kelp_bass_stacked.png", kelp_bass_stacked,
#        width = 12, height = 9, dpi = 600)

sheephead_stacked <- dat_fish_l %>% 
  filter(Genus_spp == "Semicossyphus pulcher") %>% 
  ggplot(aes(x = Length, fill = cluster_2)) +
  geom_density(position = "stack", alpha = 0.5) +
  scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Cluster Group")) +
  labs(x = "Sheephead Total Length (mm)", y = "Denisty")+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20), legend.position = "none")

# ggsave("figures/sheephead_stacked.png", sheephead_stacked,
#        width = 12, height = 9, dpi = 600)

#### Fill option 

chromis_filled <- dat_fish_l %>% 
  filter(Genus_spp == "Chromis punctipinnis") %>% 
  ggplot(aes(x = Length, fill = cluster_2)) +
  geom_density(position = "fill", alpha = 0.5) +
  scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Cluster Group")) +
  labs(x = "Blacksmith Total Length (mm)", y = "Denisty")+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20), legend.position = "none")


# ggsave("figures/chromis_filled.png", chromis_filled,
#        width = 12, height = 9, dpi = 600)

kelp_bass_filled <- dat_fish_l %>% 
  filter(Genus_spp == "Paralabrax clathratus") %>% 
  ggplot(aes(x = Length, fill = cluster_2)) +
  geom_density(position = "fill", alpha = 0.5) +
  scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Cluster Group")) +
  labs(x = "Kelp Bass Total Length (mm)", y = "Denisty")+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20), legend.position = "none")


# ggsave("figures/kelp_bass_filled.png", kelp_bass_filled,
#        width = 12, height = 9, dpi = 600)

sheephead_filled <- dat_fish_l %>% 
  filter(Genus_spp == "Semicossyphus pulcher") %>% 
  ggplot(aes(x = Length, fill = cluster_2)) +
  geom_density(position = "fill", alpha = 0.5) +
  scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Cluster Group")) +
  labs(x = "Sheephead Total Length (mm)", y = "Denisty")+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20))

# ggsave("figures/sheephead_filled.png", sheephead_filled,
#        width = 12, height = 9, dpi = 600)



####Trying filled with habitat types -

chromis_filled_ht <- dat_fish_l %>% 
  # mutate(Habitat_Type = fct_relevel(Habitat_Type, levels = c(
  #   "High Relief",
  #   "Medium Relief",
  #   "Low Relief",
  #   "High Ecotone",
  #   "Medium Ecotone",
  #   "Low Ecotone"))) %>% 
  filter(Genus_spp == "Chromis punctipinnis") %>% 
  ggplot(aes(x = Length, fill = Habitat_Type)) +
  geom_density(position = "fill", alpha = 0.8) +
  scale_fill_manual(values = c("grey34", "grey69", "lightgrey", "sienna4", "sandybrown", "peachpuff1")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Habitat Type")) +
  labs(x = "Blacksmith Total Length (mm)", y = "Denisty")+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20), legend.position = "none", legend.direction = "vertical", legend.text = element_text(size = 16))

chromis_filled_ht

# ggsave("figures/chromis_filled_ht.png", chromis_filled_ht,
#        width = 12, height = 9, dpi = 600)

kelp_bass_filled_ht <- dat_fish_l %>% 
  # mutate(Habitat_Type = fct_relevel(Habitat_Type, levels = c(
  #   "High Relief",
  #   "Medium Relief",
  #   "Low Relief",
  #   "High Ecotone",
  #   "Medium Ecotone",
  #   "Low Ecotone"))) %>% 
  filter(Genus_spp == "Paralabrax clathratus") %>% 
  ggplot(aes(x = Length, fill = Habitat_Type)) +
  geom_density(position = "fill", alpha = 0.8) +
  scale_fill_manual(values = c("grey34", "grey69", "lightgrey", "sienna4", "sandybrown", "peachpuff1")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Habitat Type")) +
  labs(x = "Kelp Bass Total Length (mm)", y = "Denisty")+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20), legend.position = "none", legend.direction = "vertical", legend.text = element_text(size = 16))

kelp_bass_filled_ht

# ggsave("figures/kelp_bass_filled_ht.png", kelp_bass_filled_ht,
#        width = 12, height = 9, dpi = 600)

sheephead_filled_ht <- dat_fish_l %>% 
  # mutate(Habitat_Type = fct_relevel(Habitat_Type, levels = c(
  #   "High Relief",
  #   "Medium Relief",
  #   "Low Relief",
  #   "High Ecotone",
  #   "Medium Ecotone",
  #   "Low Ecotone"))) %>% 
  filter(Genus_spp == "Semicossyphus pulcher") %>% 
  ggplot(aes(x = Length, fill = Habitat_Type)) +
  geom_density(position = "fill", alpha = 0.8) +
  scale_fill_manual(values = c("grey34", "grey69", "lightgrey", "sienna4", "sandybrown", "peachpuff1")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Habitat Type")) +
  labs(x = "Sheephead Total Length (mm)", y = "Denisty")+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20), legend.position = "none", legend.direction = "vertical", legend.text = element_text(size = 16))

sheephead_filled_ht

# ggsave("figures/sheephead_filled_ht.png", sheephead_filled_ht,
#        width = 12, height = 9, dpi = 600)


