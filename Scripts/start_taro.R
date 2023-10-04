# Startup script to get going on pacnvegetation package.

#install.packages("devtools")
#devtools::install_github("jakegross808/pacn-veg-package")

# Or do a fork from github
# Helpful Inventory and Monitoring Site for Data Science:
# https://doimspp.sharepoint.com/:u:/r/sites/nps-nrss-imdiv/data-science-hub/SitePages/Home.aspx?csf=1&web=1&e=QVM7qR

library(pacnvegetation)
library(tidyverse)
#library(magrittr)

# if need to install packages while on network:
# options(download.file.method = "wininet") #This approach still works, but see new, better approach here:
# https://doimspp.sharepoint.com/sites/nps-nrss-imdiv/data-science-hub/SitePages/Programming-for-Science.aspx#can-t-install-or-update-packages

#--- 1. Read latest csv folder cache ----

# replace with local path to folder:
pacnveg_cache_path <- "C:/Users/tkatayama/OneDrive - DOI/Desktop/Pacn/R_WritePACNVeg"
#pacnveg_cache_path <- "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/R_WritePACNVeg"


# Read
path_file_info <- file.info(list.files(pacnveg_cache_path, full.names = T))
latest_folder <- rownames(path_file_info)[which.max(path_file_info$mtime)]

LoadPACNVeg(data_path = latest_folder,
            data_source = "file")

# Warning message about parsing issues will display - I need to update a section of package
# Message can be ignored.

# All Focal Terrestrial Plant Community (FTPC) and
# Established Invasive Plant Species (EIPS) are now loaded in R session

#Use FilterPACNVeg() to see the different datasets
names(FilterPACNVeg())

#Then get dataset:
FilterPACNVeg(data_name = "SmWoody")

# I need help creating visuals for the following FTPC (community plot) datasets:
# LgTrees
# Presence
# SmWoody <- small woody. Including trees that are small
# Canopy
# Debris

# Using the LgTrees and SmWoody datasets (and one selected species within one
# selected sampling frame),
# I would like to recreate something like the example plot
# in the shared folder called "Tree_sizeclass_example.png" this visual
# works well with 4 plots (as in the figure) but I'm not sure how it would
# hold up with 10+ plots - it might just get to busy?

# Below is some example functions that generate tables and figures for
# the understory dataset:

# Understory ----

# starting dataset example using "Olaa" Sampling Frame:
raw_understory <- pacnvegetation::FilterPACNVeg(data_name = "Understory", sample_frame = "Olaa")

raw_understory <- FilterPACNVeg(data_name = "Understory", sample_frame = "Olaa")
# This is raw data showing species hit at each point along a cover line
# There are 300 pts per plot (less in coastal strand plant community),
# and two strata per point (0-1 m) and (1-2 m)

# A function called "summarize_understory()" was created to help summarize
# the point-level data into plot-level summaries which are more graph-ready tables:
# Data can be grouped by "None", "Nativity", "Life_Form", or "Species,
?pacnvegetation::summarize_understory

# For example

# All plant understory cover:
under_hits <- summarize_understory(plant_grouping = "None",
                                   sample_frame = "Olaa",
                                   combine_strata = TRUE)

under_hits %>%
  ggplot(aes(x = Year, y = Cover)) +
  geom_boxplot()

# Grouped by Native vs Non-native Cover:
under_native <- summarize_understory(plant_grouping = "Nativity",
                                     sample_frame = "Olaa",
                                     combine_strata = TRUE)

under_native %>%
  filter(Nativity != "Unknown") %>% # remove plants not identified to species level
  ggplot(aes(x = Year, y = Cover, fill = Nativity)) +
  geom_boxplot()

# Grouped by Lifeform + Nativity:
under_lifeform <- summarize_understory(plant_grouping = "Life_Form",
                                       sample_frame = "Olaa",
                                       combine_strata = TRUE)

under_lifeform %>%
  filter(Nativity != "Unknown") %>% # remove plants not identified to species level
  ggplot(aes(x = Year, y = Cover, fill = Nativity)) +
  facet_wrap(vars(Life_Form), scales = "free") +
  geom_boxplot()

# Split into Species Cover:
under_spp <- summarize_understory(plant_grouping = "Species",
                                  sample_frame = "Olaa",
                                  combine_strata = TRUE)

# Select top 10 most common species (highest max cover in a plot):
common_species <- under_spp %>%
  arrange(-Cover) %>%
  distinct(Scientific_Name) %>%
  drop_na() %>%
  slice(1:10) %>%
  pull()

under_spp %>%
  filter(Scientific_Name %in% common_species) %>% # Top 10
  filter(Plot_Type == "Fixed") %>%
  ggplot(aes(x = Year, y = Cover, fill = Nativity)) +
  geom_boxplot() +
  facet_wrap(vars(Scientific_Name), scales = "free")


 # Good to experiment with different visuals

# Ultimately a visual is decided upon for either QA/QC, Exploratory Data Analysis,
# or communication with park managers (sometimes these overlap)

# For example the following visualization using bar graphs was made into a function
# for specific use in park manager briefs.
# I like the box plots with medians highlighted,
# but it seemed like the managers I talked to so far
# liked the easy readability of the bar charts with means.

# This funtion "v_cover_bar_stats()" uses the function "summarize_understory()"
# to first produce a datatable and then graph it with  standardized colors,
# and listing of plot number used to calculate the mean at the bottom of the
# figure.
pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                  sample_frame = "Olaa",
                  combine_strata = TRUE,
                  species_filter = common_species,
                  measurement = "Cover")

# The Metrosideros polymorpha understory data shows a disturbing trend -
# as it is the most important native tree species
# in Hawaii's forest (park managers have been notified of this preliminary result).
# Need to check if seedling, small tree, and large tree data show similar trends.

# Take closer look at M. polymorpha cover for just the fixed plots
v_cover_bar_stats(plant_grouping = "Species",
                  sample_frame = "Olaa",
                  combine_strata = TRUE,
                  species_filter = "Metrosideros polymorpha",
                  plot_type = "Fixed")

# Look at paired change between the fixed plots
v_cover_bar_stats(plant_grouping = "Species",
                  sample_frame = "Olaa",
                  combine_strata = TRUE,
                  species_filter = "Metrosideros polymorpha",
                  paired_change = TRUE,
                  measurement = "Chg_Prior")

# Significant decrease in M. polymorpha in the understory between 2015-2021.


# Luckily it is the opposite trend in Kahuku, a different location (sampling frame)
# within Hawaii Volcanoes National Park
v_cover_bar_stats(plant_grouping = "Species",
                  sample_frame = "Kahuku",
                  combine_strata = TRUE,
                  species_filter = "Metrosideros polymorpha",
                  plot_type = "Fixed")

# ...and somewhat consistent in 'Mauna Loa' sampling frame (HAVO):
v_cover_bar_stats(plant_grouping = "Species",
                  sample_frame = "Mauna Loa",
                  combine_strata = TRUE,
                  species_filter = "Metrosideros polymorpha",
                  plot_type = "Fixed")


# This is same function with plant_grouping set to "Nativity" for
# high-level summary graph used in .Rmarkdown park-manager report:
v_cover_bar_stats(plant_grouping = "Nativity",
                  sample_frame = "Olaa",
                  measurement = "Cover")

# Or, can use same function with different specific 'plot_number" arguement
# to look at low-level, detailed change within one plot:
v_cover_bar_stats(plant_grouping = "Species",
                  sample_frame = "Olaa",
                  paired_change = TRUE,
                  combine_strata = TRUE,
                  measurement = "Chg_Prior",
                  plot_number = 1)

# Example of building up a visualization----

# Compare native to non-native understory by plot:

# Dataset
nat_v_non <- pacnvegetation::UnderNativityCover(combine_strata = TRUE,
                   #paired_change = FALSE,
                   #crosstalk = FALSE,
                   sample_frame = "Olaa",
                   cycle = 3)

# Base R
plot(x = nat_v_non$Native_Cover_Total_pct,
     y = nat_v_non$NonNative_Cover_Total_pct,
     xlab = "Total Native Cover",
     ylab = "Total Non-Native Cover")

# ggplot (within function)
pacnvegetation::UnderNativityCover.plot.nat_v_non(combine_strata = TRUE,
                                                  sample_frame = "Olaa",
                                                  cycle = 3)

# using "plotly" and "crosstalk" packages (within function)
grp1 <- "cov_total"

plt1 <- UnderNativityCover.plot.nat_v_non(
  sample_frame = "Olaa",
  year_filter = TRUE,
  paired_change = FALSE,
  combine_strata = TRUE,
  crosstalk = TRUE,
  crosstalk_group = grp1,
  interactive = TRUE)
plt1

# using "plotly", "crosstalk", and "leaflet" packages (within function)
grp1 <- "cov_total"

plt1 <- UnderNativityCover.plot.nat_v_non(
  sample_frame = "Olaa",
  year_filter = TRUE,
  paired_change = FALSE,
  combine_strata = TRUE,
  crosstalk = TRUE,
  crosstalk_group = grp1,
  interactive = TRUE)

map1 <- MapCoverTotal2(crosstalk = TRUE,
                       crosstalk_group = grp1,
                       sample_frame = "Olaa")

crosstalk::bscols(widths = c(6, NA), plt1, map1)


# Most of the pacnvegetation function used above can be found in the
# github project "R" folder - within "EDA_understory.R" or "spatial.R"
# some helper functions are in "utils.R" and "viz_utils.R"

