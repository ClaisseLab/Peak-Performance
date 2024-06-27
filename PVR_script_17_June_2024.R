#### Project Information ----
#### Project Title: Peak Performance: vertical relief influences fish density and assemblage structure on heterogenous restoration reef
#### Authors: James W. Sturges & Jeremy T. Claisse
#### Last Updated: Jun3 20 2024

#### Required Packages ----
library(here)
library(Hmisc)
library(RColorBrewer)
library(randomcoloR)
library(vegan)
library(tidyverse)
library(flextable) 
library(ggrepel)
library(officer)
library(wesanderson)
library(dendextend)
library(ggdendro)


here()


#### Importing Cleaned Data ----
# This imports our data set from the raw EventMeasure observation files
# details on data cleaning can be found in the dat.EMObs_Cleaning.R
source("dat_EMObs_Cleaning.R")

set_flextable_defaults(font.family = "Arial", font.size = 9, digits = 1, padding = 0.1)



#### Scaling for Sampling Effort ----
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


ht_dist_ft

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






time_comp_plot

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

#### Creating dat_fish_t ----

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
                              startsWith(os_ht, "West Parallel Mid_High") ~ "Group 3",
                              startsWith(os_ht, "West Parallel Ecotone_High") ~ "Group 3",
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
dat_fish_t <- dat_fish_t %>%
  mutate(cluster_2 = cluster)

dat_fish_t <- dat_fish_t %>%
  mutate(cluster_2 = str_replace_all(cluster_2, "Group 6", "Perpendicular Inshore High Relief"),
         cluster_2 = str_replace_all(cluster_2, "Group 5", "Perpendicular Offshore High Ecotone"),
         cluster_2 = str_replace_all(cluster_2, "Group 1", "Sheltered Inshore Parallel"),
         cluster_2 = str_replace_all(cluster_2, "Group 2", "Low Relief & Ecotones"),
         cluster_2 = str_replace_all(cluster_2, "Group 3", "Intermediate"),
         cluster_2 = str_replace_all(cluster_2, "Group 4", "Offshore High & Medium Relief"))

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

dens_sp_os_ht <- dens_sp_os_ht %>% 
  mutate(cluster = (case_when(startsWith(cluster, "Group 1") ~ "Group 4",
                    startsWith(cluster, "Group 2") ~ "Group 3",
                    startsWith(cluster, "Group 3") ~ "Group 2",
                    startsWith(cluster, "Group 4") ~ "Group 1",
                    startsWith(cluster, "Group 5") ~ "Group 6",
                    startsWith(cluster, "Group 6") ~ "Group 5")))
                              
dens_sp_clust_heatmap <- dens_sp_os_ht %>% 
  group_by(cluster, Genus_spp) %>% 
  summarise(mean_clust_dens = mean(mean_dens), 
            min_clust_dens = min(mean_dens),
            max_clust_dens = max(mean_dens),
            count_tt = n())

group_color_lab <- c("coral2","springgreen4", "steelblue3","purple4" ,"black","khaki4")

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
#### Module Assemblage NMDS ----
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


#### Module Assemblage Cluster Analysis ----
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




#### Sub-module Assemblage NMDS  ----
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



wide_fish_os_ht <- wide_fish_os_ht %>%
  mutate(Habitat_Type_2 = str_replace_all(Habitat_Type, "_", " "))


wide_fish_os_ht <- wide_fish_os_ht %>%
  mutate(cluster_2 = cluster)


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
  mutate(Module = factor(Habitat_Type_2))
         
wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(Module = str_replace(Module, "High Relief", "High Reef"),
         Module = str_replace(Module, "Medium Relief", "Medium Reef"),
         Module = str_replace(Module, "Low Relief", "Low Reef"))

wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(Module = factor(Module, levels = c("High Reef",
                                                    "Medium Reef",
                                                    "Low Reef",
                                                    "High Ecotone",
                                                    "Medium Ecotone",
                                                    "Low Ecotone")))

plot_wide_fish_os_ht <- ggplot(wide_fish_os_ht,
                               aes(-MDS1, -MDS2)) +
  geom_text(aes(label = os_lab), vjust = 3, hjust = .4, size = 5) +
  geom_point(aes(color = cluster_2, shape = Module),size = 7, stroke = 3) +
  scale_shape_manual(values = c(15, 16, 17, 0, 1, 2)) +
  scale_color_manual(values = c("coral2","springgreen4", "steelblue3","purple4" ,"black","khaki4")) +
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
#plot_wide_fish_os_ht
# hull_os_ht <- wide_fish_os_ht %>% 
#   group_by(Zone) %>% 
#   slice(chull(-NMDS1, -NMDS2))
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
  scale_fill_manual(values = c("coral2","springgreen4", "steelblue3","purple4" ,"black","khaki4")) +
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
  geom_text(data = spp_scrs, aes(x = -NMDS1*.27, y = -NMDS2*.27, label = common_name, shape = NULL), fontface = "bold", color = "black", size = 7) +
  theme(legend.position = c(0.01, .99),
        legend.justification = c(0, 1),   
        legend.background = element_rect(color = "black", fill = "white", size = 0.5),
        legend.margin = margin(5, 5, 5, 5), legend.direction = "vertical")



plot_NMDS_os_ht_spp_vect

new_plot_NMDS_os_ht_spp_vect = plot_NMDS_os_ht_spp_vect + scale_y_reverse()

new_plot_NMDS_os_ht_spp_vect
ggsave("figures/new_submodule_assemblage.png", new_plot_NMDS_os_ht_spp_vect,
       width = 16, height = 12, dpi = 600)
ggsave("figures/submodule_assemblage.png", plot_NMDS_os_ht_spp_vect,
       width = 16, height = 12, dpi = 600)
ggsave("figures/submodule_assemblage.pdf", plot_NMDS_os_ht_spp_vect,
       width = 16, height = 12, dpi = 600)
spp_scrs <- spp_scrs %>% 
  arrange(pval)

spp_scrs

spp_scrs_ft <- flextable(spp_scrs,
                         col_keys = c("Species", "pval")) %>% 
  set_header_labels(Species = "NMDS1",
                    P = "pval") 

#spp_scrs_ft



#### Cluster Analysis for OS_HT ----

#### DENDO EDITS ----

# Method 1 Dynamic Tree

library(dynamicTreeCut)
library(ggdendro)
library(ggplot2)
library(vegan)

wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(dend_lab = paste(os_lab, Module, sep = " "))



comm_fish_os_ht <- wide_fish_os_ht %>%
  column_to_rownames(var = "dend_lab") %>%
  select(Chromis_punctipinnis:Semicossyphus_pulcher)

# Create acomm_fish_os_ht# Create a distance matrix based on the community assemblages
dis.comm_fish_os_ht <- vegdist(comm_fish_os_ht)

# Create a cluster dendrogram
clust.comm_fish_os_ht <- hclust(dis.comm_fish_os_ht, "average")

# Dynamic tree cut to find clusters
dynamic_clusters <- cutreeDynamic(clust.comm_fish_os_ht, distM = as.matrix(dis.comm_fish_os_ht), deepSplit = 2, pamRespectsDendro = FALSE)

# Convert hclust object to dendrogram object
dendro <- as.dendrogram(clust.comm_fish_os_ht)

# Extract dendrogram data for ggplot2
dendro_data <- dendro_data(dendro)

# Plot the dendrogram with dynamic clusters highlighted
ggplot() +
  geom_segment(data = segment(dendro_data), aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_text(data = label(dendro_data), aes(x = x, y = y, label = label, angle = 45)) +
  # scale_color_manual(values = rainbow(length(unique(dynamic_clusters)))) +
  theme_minimal() +
  labs(title = "Cluster Dendrogram with Dynamic Clusters", x = "Samples", y = "Height") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.title = element_blank())


# Method 2
library(cluster)

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

# Use K = 6 for clustering
K <- 6

# Cut the dendrogram at K clusters
clusters <- cutree(clust.comm_fish_os_ht, k = K)

# Convert hclust object to dendrogram object again for plotting
dendro <- as.dendrogram(clust.comm_fish_os_ht)

# Extract dendrogram data for ggplot2
dendro_data <- dendro_data(dendro)

# Plot the dendrogram with K clusters highlighted
ggplot() +
  geom_segment(data = segment(dendro_data), aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_text(data = label(dendro_data), aes(x = x, y = y, label = label), hjust = 1, angle = 90, size = 3) +
  # scale_color_manual(values = rainbow(K)) +
  theme_minimal() +
  labs(title = paste("Cluster Dendrogram with", K, "Clusters"), x = "Samples", y = "Height") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.title = element_blank())












# wide_fish_os_ht <- wide_fish_os_ht %>% 
#   mutate(dend_lab = paste(Orientation, current, Habitat_Type))
# 
# 
# wide_fish_os_ht <- wide_fish_os_ht %>%
#   mutate(dend_lab = str_replace(dend_lab, "Parallel Up-current High Ecotone", "Par. Offshore High Ecotone"),
#          dend_lab = str_replace(dend_lab, "Parallel Up-current High Relief", "Par. Offshore High Relief"),
#          dend_lab = str_replace(dend_lab, "Parallel Up-current Medium Ecotone", "Par. Offshore Medium Ecotone"),
#          dend_lab = str_replace(dend_lab, "Parallel Up-current Medium Relief", "Par. Offshore Medium Relief"),
#          dend_lab = str_replace(dend_lab, "Parallel Up-current Low Ecotone", "Par. Offshore Low Ecotone"),
#          dend_lab = str_replace(dend_lab, "Parallel Up-current Low Relief", "Par. Offshore Low Relief"),
#          dend_lab = str_replace(dend_lab, "Parallel Down-current High Ecotone", "Par. Inshore High Ecotone"),
#          dend_lab = str_replace(dend_lab, "Parallel Down-current High Relief", "Par. Inshore High Relief"),
#          dend_lab = str_replace(dend_lab, "Parallel Down-current Medium Ecotone", "Par. Inshore Medium Ecotone"),
#          dend_lab = str_replace(dend_lab, "Parallel Down-current Medium Relief", "Par. Inshore Medium Relief"),
#          dend_lab = str_replace(dend_lab, "Parallel Down-current Low Ecotone", "Par. Inshore Low Ecotone"),
#          dend_lab = str_replace(dend_lab, "Parallel Down-current Low Relief", "Par. Inshore Low Relief"),
#          dend_lab = str_replace(dend_lab, "Perpendicular Up-current High Ecotone", "Perp. West High Ecotone"),
#          dend_lab = str_replace(dend_lab, "Perpendicular Up-current High Relief", "Perp. West High Relief"),
#          dend_lab = str_replace(dend_lab, "Perpendicular Up-current Medium Ecotone", "Perp. West Medium Ecotone"),
#          dend_lab = str_replace(dend_lab, "Perpendicular Up-current Medium Relief", "Perp. West Medium Relief"),
#          dend_lab = str_replace(dend_lab, "Perpendicular Up-current Low Ecotone", "Perp. West Low Ecotone"),
#          dend_lab = str_replace(dend_lab, "Perpendicular Up-current Low Relief", "Perp. West Low Relief"),
#          dend_lab = str_replace(dend_lab, "Perpendicular Down-current High Ecotone", "Perp. East High Ecotone"),
#          dend_lab = str_replace(dend_lab, "Perpendicular Down-current High Relief", "Perp. East High Relief"),
#          dend_lab = str_replace(dend_lab, "Perpendicular Down-current Medium Ecotone", "Perp. East Medium Ecotone"),
#          dend_lab = str_replace(dend_lab, "Perpendicular Down-current Medium Relief", "Perp. East Medium Relief"),
#          dend_lab = str_replace(dend_lab, "Perpendicular Down-current Low Ecotone", "Perp. East Low Ecotone"),
#          dend_lab = str_replace(dend_lab, "Perpendicular Down-current Low Relief", "Perp. East Low Relief"))
# 
# 
# comm_fish_os_ht <- wide_fish_os_ht %>%
#   column_to_rownames(var = "dend_lab") %>%
#   select(Chromis_punctipinnis:Semicossyphus_pulcher)

#Create a distance matrix based on the community assemblages
dis.comm_fish_os_ht <- vegdist(comm_fish_os_ht)

#Create a cluster dendrogram
clust.comm_fish_os_ht <- hclust(dis.comm_fish_os_ht, "average")

#Add color labeles and branches based on the NMDS groups
os_ht_dendro <- color_labels(clust.comm_fish_os_ht, col = c("black","black","black", "black", "black", "black"), k = 6)

os_ht_dendro <- color_branches(os_ht_dendro, col = c("black","black","black", "black", "black", "black"), k = 6)

 # os_ht_dendro <- os_ht_dendro %>%
 #   rotate(24:2)
 
 # dend <- as.dendrogram(clust.Comm_Gut_Wide_R_Rt_S)
 # par(mar=c(5,1,1,12))
 # 
 # plot(rotate(dend, c(1:15, 20, 24, 26:27, 25, 29:30, 28, 23:21, 16:17, 18:19)), 
 #      horiz = T,
 #      xlab = "Dissimilarity",
 #      xlim = c(0.4, 0))

gg_os_ht_dend <- as.ggdend(os_ht_dendro)
plot_gg_os_ht_dend <- ggplot(gg_os_ht_dend, horiz = T, offset_labels = -0.01)
plot_gg_os_ht_dend <- plot_gg_os_ht_dend +
  theme_classic() +
  theme(plot.margin = margin(1,1,1,1, "cm")) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "none", axis.line.y = element_blank(), axis.title.x = element_text(vjust = - 1.2, hjust = .5)) +
  theme(text = element_text(size = 24)) +
  scale_y_reverse(breaks = c(0.3,0.2,0.1,0), expand=c(0,.1,0,.1)) + 
  labs(y = "Bray-Curtis Dissimilarity")



plot_gg_os_ht_dend

ggsave("figures/submodule_dendrogram.png", plot_gg_os_ht_dend,
       width = 15, height = 7.5, dpi = 600)

ggsave("figures/submodule_dendrogram.pdf", plot_gg_os_ht_dend,
       width = 15, height = 7.5, dpi = 600)









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





clust_col <- c("coral2","coral2","coral2","coral2","coral2","coral2","springgreen4","springgreen4","springgreen4","springgreen4","springgreen4","springgreen4","springgreen4","steelblue3","steelblue3","steelblue3","steelblue3","steelblue3","purple4","purple4","purple4","purple4","black","khaki4")



plot_dens_total_os_ht <- dat_fish_os_ht %>%
  mutate(os_ht = fct_relevel(os_ht,
                             "East Perpendicular High Relief",
                             "West Parallel High Relief",
                             "West Parallel Medium Relief",
                             "West Perpendicular Medium Relief",
                             "West Parallel High Ecotone",
                             "West Perpendicular Medium Ecotone",
                             
                             "East Parallel High Relief",
                             "East Perpendicular Medium Relief",
                             "West Perpendicular Low Relief",
                             "West Parallel Low Relief",
                             "East Perpendicular Medium Ecotone",
                             "East Perpendicular Low Ecotone",
                             "West Perpendicular Low Ecotone",
                             
                             
                             "East Parallel Low Relief",
                             "East Perpendicular Low Relief",
                             "West Perpendicular High Ecotone",
                             "West Parallel Medium Ecotone",
                             "West Parallel Low Ecotone",
                             
                             "East Parallel Medium Relief",
                             "East Parallel High Ecotone",
                             "East Parallel Medium Ecotone",
                             "East Parallel Low Ecotone",
                             
                             
                             "West Perpendicular High Relief",
                             
                             "East Perpendicular High Ecotone"
  )) %>%
  filter(Genus_spp %in% focal_spp) %>%
  group_by(os_ht, Genus_spp) %>%
  summarize(total_dens = sum(dens_100m2), .groups = 'drop') %>%
  ggplot(aes(x = os_ht, y = total_dens, fill = Genus_spp)) +
  geom_bar(stat = "identity", colour = "black") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Transect Type", y = expression(paste("Mean Density (No./100",m^{2},")"))) +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  guides(fill = guide_legend(title = "Focal Fish Species")) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_brewer(palette = "Set3", labels = c("Blacksmith", "Pile Perch", "Black Perch", "Opaleye", "Rock Wrasse", "Rainbow Seaperch", "Senorita", "Kelp Bass", "Barred Sand Bass", "Olive Rockfish", "California Sheephead")) +
  theme(
    legend.position = c(.7, .5), 
    legend.direction = "vertical", 
    legend.background = element_rect(fill = "white", color = "black"), 
    legend.text = element_text(size = 16),
    axis.text.y = element_text(color = clust_col)  # Set y-axis (flipped x-axis) label colors
  )

plot_dens_total_os_ht

dat_fish_os_ht <- dat_fish_os_ht %>%
  filter(Genus_spp %in% focal_spp) %>%
  group_by(os_ht, Genus_spp) %>%
  summarize(total_dens = sum(dens_100m2), .groups = 'drop') %>%
  group_by(os_ht) %>%
  mutate(proportion = total_dens / sum(total_dens))

# Create the proportional density plot
plot_dens_proportional_os_ht <- ggplot(dat_fish_os_ht, aes(x = os_ht, y = proportion, fill = Genus_spp)) +
  geom_bar(stat = "identity", colour = "black") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Transect Type", y = "Proportion") +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  guides(fill = guide_legend(title = "Focal Fish Species")) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set3", labels = c("Blacksmith", "Pile Perch", "Black Perch", "Opaleye", "Rock Wrasse", "Rainbow Seaperch", "Senorita", "Kelp Bass", "Barred Sand Bass", "Olive Rockfish", "California Sheephead")) +
  theme(
    legend.position = c(.7, .5), 
    legend.direction = "vertical", 
    legend.background = element_rect(fill = "white", color = "black"), 
    legend.text = element_text(size = 16),
    axis.text.y = element_text(color = clust_col)  # Set y-axis (flipped x-axis) label colors
  )

plot_dens_proportional_os_ht

#### species specific for loops ----

dat_fish_spp <- dat_fish_t %>% 
  filter(Genus_spp %in% focal_spp)

dat_fish_spp = dat_fish_spp %>% 
mutate(os_ht = paste(Module_Side, Orientation, Habitat_Type, sep = " "))

dat_fish_spp <- dat_fish_spp %>%
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

# Filter for focal species and create os_ht column
dat_fish_spp <- dat_fish_t %>%
  filter(Genus_spp %in% focal_spp) %>%
  mutate(os_ht = paste(Module_Side, Orientation, Habitat_Type, sep = " "))

# Add cluster information
dat_fish_spp <- dat_fish_spp %>%
  mutate(cluster = case_when(
    startsWith(os_ht, "West Perpendicular High Relief") ~ "Group 5",
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
    startsWith(os_ht, "East Parallel Medium Ecotone") ~ "Group 4"
  ))

# Create factor for Habitat_Type
dat_fish_spp <- dat_fish_spp %>%
  mutate(Habitat_Type = factor(Habitat_Type, levels = c(
    "High Relief", "Medium Relief", "Low Relief", 
    "High Ecotone", "Medium Ecotone", "Low Ecotone"
  )))

# Create transect and dens_100m2_4rt columns
dat_fish_spp <- dat_fish_spp %>%
  mutate(transect = paste(Module, Module_Side, Habitat_Type),
         dens_100m2_4rt = dens_100m2^0.25) %>%
  left_join(transect_var)

# Ensure construction_group is a character
dat_fish_spp <- dat_fish_spp %>%
  mutate(construction_group = as.character(construction_group))

# Get unique species
species <- unique(dat_fish_spp$Genus_spp)
species_plots <- list()

group_color_lab <- c("Group 1" = "coral2", "Group 2" = "springgreen4", 
                     "Group 3" = "steelblue3", "Group 4" = "purple4", 
                     "Group 5" = "black", "Group 6" = "khaki4")

for(species_ in species){
  species_plots[[species_]] <- dat_fish_spp %>%
    filter(Genus_spp == species_) %>%
    mutate(os_ht = fct_relevel(os_ht,
                               "East Perpendicular High Relief",
                               "West Parallel High Relief",
                               "West Parallel Medium Relief",
                               "West Perpendicular Medium Relief",
                               "West Parallel High Ecotone",
                               "West Perpendicular Medium Ecotone",
                               "East Parallel High Relief",
                               "East Perpendicular Medium Relief",
                               "West Perpendicular Low Relief",
                               "West Parallel Low Relief",
                               "East Perpendicular Medium Ecotone",
                               "East Perpendicular Low Ecotone",
                               "West Perpendicular Low Ecotone",
                               "East Parallel Low Relief",
                               "East Perpendicular Low Relief",
                               "West Perpendicular High Ecotone",
                               "West Parallel Medium Ecotone",
                               "West Parallel Low Ecotone",
                               "East Parallel Medium Relief",
                               "East Parallel High Ecotone",
                               "East Parallel Medium Ecotone",
                               "East Parallel Low Ecotone",
                               "West Perpendicular High Relief",
                               "East Perpendicular High Ecotone"
    )) %>%
    mutate(Habitat_Type = factor(Habitat_Type, levels = c("Low Relief",
                                                          "Medium Relief",
                                                          "High Relief",
                                                          "High Ecotone",
                                                          "Medium Ecotone",
                                                          "Low Ecotone"))) %>% 
    ggplot(aes(x = os_ht, y = dens_100m2, color = cluster, shape = Habitat_Type)) +
    scale_shape_manual(values = c(15, 16, 17, 0, 1, 2)) +
    geom_crossbar(stat = "summary", fun.data = mean_cl_boot,
                  fill = "gray", alpha = 0.5, width = 0.4) +
    geom_point(position = position_jitter()) +
    theme_classic() +
    ggtitle(species_) +
    scale_color_manual(values = group_color_lab) +
    guides(shape = guide_legend(title = "Habitat Type"), color = guide_legend(title = "Cluster Group")) +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 9,color = clust_col),legend.position = c(0.65, 0.85), legend.direction = "horizontal", legend.box = "vertical") +
    labs(x = "Unique Reef Feature Combinations", y = expression(paste("Fish Density (No./100", m^2, ")")))
  
  print(species_plots[[species_]])
  
  ggsave(paste0("figures/spp_density_plot_", species_, ".png"), species_plots[[species_]],
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


#### Trying filled with habitat types ----
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
  ggsave(paste0("figures/density_plots/density_plot_", species_, ".png"), species_density_plots[[species_]],
         width = 9, height = 8, dpi = 600)
}
#### No stacked positions ----
# chromis_dens_curve <- dat_fish_l %>% 
#   filter(Genus_spp == "Chromis punctipinnis") %>% 
#   ggplot(aes(x = Length, fill = cluster)) +
#   geom_density(alpha = 0.5) +
#   scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
#   theme_classic() +
#   guides(fill = guide_legend(title = "Cluster Group")) +
#   labs(x = "Blacksmith Total Length (mm)", y = "Denisty") +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20), legend.position = "none")
# 
# chromis_dens_curve
# 
# ggsave("figures/density_curve_chromis.png", chromis_dens_curve,
#        width = 10, height = 6, dpi = 600)
# 
# 
# kelp_bass_dens_curve <- dat_fish_l %>%  
#   filter(Genus_spp == "Paralabrax clathratus") %>% 
#   ggplot(aes(x = Length, fill = cluster)) +
#   geom_density(alpha = 0.5) +
#   scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
#   theme_classic() +
#   guides(fill = guide_legend(title = "Cluster Group")) +
#   labs(x = "Kelp Bass Total Length (mm)", y = "Denisty")+
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20), legend.position = "none")
# 
# ggsave("figures/density_curve_kelp_bass.png", kelp_bass_dens_curve,
#        width = 12, height = 9, dpi = 600)
# 
# 
# sheephead_dens_curve <- dat_fish_l %>% 
#   filter(Genus_spp == "Semicossyphus pulcher") %>% 
#   ggplot(aes(x = Length, fill = cluster)) +
#   geom_density(alpha = 0.5) +
#   scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
#   theme_classic() +
#   guides(fill = guide_legend(title = "Cluster Group")) +
#   labs(x = "Sheephead Total Length (mm)", y = "Denisty")+
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20), legend.position = "none")
# 
# ggsave("figures/density_curve_sheephead.png", sheephead_dens_curve,
#        width = 12, height = 9, dpi = 600)
# 
# #### stacked positions
# 
# chromis_stacked <- dat_fish_l %>% 
#   filter(Genus_spp == "Chromis punctipinnis") %>% 
#   ggplot(aes(x = Length, fill = cluster_2)) +
#   geom_density(position = "stack", alpha = 0.5) +
#   scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
#   theme_classic() +
#   guides(fill = guide_legend(title = "Cluster Group")) +
#   labs(x = "Blacksmith Total Length (mm)", y = "Denisty")+
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20), legend.position = "none")
# 
# # ggsave("figures/chromis_stacked.png", chromis_stacked,
# #        width = 12, height = 9, dpi = 600)
# 
# 
# kelp_bass_stacked <- dat_fish_l %>% 
#   filter(Genus_spp == "Paralabrax clathratus") %>% 
#   ggplot(aes(x = Length, fill = cluster_2)) +
#   geom_density(position = "stack", alpha = 0.5) +
#   scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
#   theme_classic() +
#   guides(fill = guide_legend(title = "Cluster Group")) +
#   labs(x = "Kelp Bass Total Length (mm)", y = "Denisty")+
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0))+
#   theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20), legend.position = "none") 
# 
# # ggsave("figures/kelp_bass_stacked.png", kelp_bass_stacked,
# #        width = 12, height = 9, dpi = 600)
# 
# sheephead_stacked <- dat_fish_l %>% 
#   filter(Genus_spp == "Semicossyphus pulcher") %>% 
#   ggplot(aes(x = Length, fill = cluster_2)) +
#   geom_density(position = "stack", alpha = 0.5) +
#   scale_fill_manual(values = c("black", "khaki4", "purple4", "steelblue3", "springgreen4", "coral2")) +
#   theme_classic() +
#   guides(fill = guide_legend(title = "Cluster Group")) +
#   labs(x = "Sheephead Total Length (mm)", y = "Denisty")+
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20), legend.position = "none")
# 
# # ggsave("figures/sheephead_stacked.png", sheephead_stacked,
# #        width = 12, height = 9, dpi = 600)
