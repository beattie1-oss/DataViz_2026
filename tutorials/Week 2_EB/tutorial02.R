###############################
# Data Viz  
# Tutorial 2:  Ellen Beattie
# Tidyverse & Manipulating Data        
###############################  

# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
    basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
    package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
    package.list <- setdiff(package.list, basic.packages)
    if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
    }
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg,  dependencies = TRUE)
    sapply(pkg,  require,  character.only = TRUE)
    }

# Load any necessary packages
lapply(c("tidyverse", "ggplot2", "ggridges", "tradestatistics"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

theme_ellen <- function(...) {
  theme_minimal(base_size = 12, base_family = "serif") + 
    theme(
      #Backgrounds 
      # Grid
      panel.background = element_rect(fill = "white", colour = NA), 
      panel.border = element_rect(fill = NA, colour = "grey20"), 
      panel.grid = element_line(colour = "grey92"), 
      panel.grid.minor = element_line(size = rel(0.5)), 
      
      #Axis
      axis.title = element_text(family = "serif", size = 14), #title font
      axis.line.x = element_line(size = 0.4), #axis lines
      axis.line.y = element_line(size = 0.4),
      axis.text = element_text(colour = "black", size = 10), #axis text change
      axis.ticks.x = element_line(colour = "black", size = 0.75),
      
      #Legend
      legend.title = element_text(family = 'serif', size = 14, colour = 'black'), #legend
      legend.text =  element_text(family = 'serif', size = 12, colour = 'black'),# font change
      legend.key.height = unit(1.5, "lines"),#spacing
      legend.key.width = unit(1.5, "lines"), 
      legend.spacing.y = unit(1, "lines"),
      legend.background = element_rect( 
        fill = "white",
        colour = "grey30", #make box aound legend to align it
        size = 0.5,
        linetype = "solid")
    )
} 

#################
### In class demo
#################

# Import dataset and load library
SAFI <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/DataViz_2026/refs/heads/main/datasets/SAFI.csv")		

# All households in village "God"
SAFI_god <- filter(SAFI, village == "God") #assignment filter all villages equal GOd

# Households with more than 6 members
large_households <- filter(SAFI, no_membrs > 6)

# Households in "God" or "Ruaca" with more than 4 members
god_ruaca_large <- filter(SAFI, village %in% c("God", "Ruaca") & no_membrs > 4) #stack assigments

# Keep only identification and location variables
id_loc <- SAFI |>
  select(key_ID, village)

# Move village to the first position
village_first <- SAFI |>
  select(village, everything()) #village as first column, then everything, more to do with arrangement

# Drop columns related to survey timing
no_timing <- SAFI |>
  select(-starts_with("interview_date")) #any column startig with gets dropped

# Households ordered by size (ascending)
SAFI_by_size <- SAFI |>
  arrange(no_membrs)

# Households ordered by size (descending) within village
SAFI_by_village_size <- SAFI |>
  arrange(village, desc(no_membrs)) #

# Add a logical indicator for "large" households
SAFI <- SAFI |>
  mutate(large_hh = no_membrs >= 6) #add column

# Years of living in village in decades
SAFI <- SAFI |>
  mutate(living_decades = years_liv / 10)

# Categorize household size
SAFI <- SAFI |>
  mutate(
    hh_size_cat = case_when( #vectorise if else
      no_membrs <= 3            ~ "small",
      no_membrs >= 4 & no_membrs <= 7 ~ "medium",
      no_membrs > 7             ~ "large"
      )
  )

?case_when

# Large households in Ruaca with selected columns
ruaca_large_small_df <- SAFI |>
  filter(village == "Ruaca", no_membrs > 6) |>
  mutate(
    large_hh = TRUE, #gets appended to data set
    members_per_room = no_membrs / rooms #new column
  ) |>
  select(village, no_membrs, rooms, members_per_room, large_hh) #then select columns want to display

# Mean household size by village
village_hh_summary <- SAFI |>
  group_by(village) |>
  summarize(
    mean_members = mean(no_membrs, na.rm = TRUE),
    median_members = median(no_membrs, na.rm = TRUE),
    n_households = n(),
    .groups = "drop"
  )

# Mean years in village by roof type
wall_living_summary <- SAFI |>
  count(wall_type, village, name = "n") |>
  group_by(village) |>
  mutate(prop = n / sum(n))

pdf("wall_type_village.pdf")
ggplot(wall_living_summary,
       aes(x = village, y = prop, fill = wall_type)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "\nVillage",
    y = "Proportion of households\n",
    fill = "Wall type",
    title = "Wall material proportions by village"
  ) 
dev.off()

pdf("hist_bin30.pdf")
ggplot(SAFI, aes(x = no_membrs)) +
  geom_histogram(color = "white", fill = "#E16462") + #automatically selects 30 binwidth
  labs(x = "\nNumber of household members", 
       y = "Count\n") +
  theme_ellen()
dev.off()

pdf("hist_bin1.pdf")
ggplot(SAFI, aes(x = no_membrs)) +
  geom_histogram(binwidth = 1, color = "white", fill = "#E16462") + #setting it to 1
  labs(x = "\nNumber of household members", 
       y = "Count\n")+
  theme_ellen()
dev.off()

pdf("hist_group1.pdf")
ggplot(SAFI, aes(x = no_membrs, fill = memb_assoc)) +
  geom_histogram(position="dodge") +
  labs(x = "\nNumber of household members", 
       y = "Count\n")
dev.off()

pdf("hist_group2.pdf")
ggplot(SAFI, aes(x = no_membrs, fill = memb_assoc)) +
  geom_histogram(position="dodge") +
  facet_wrap(vars(village)) + #sharp looking bars
  labs(x = "\nNumber of household members", 
       y = "Count\n")
dev.off()

pdf("density.pdf")
ggplot(SAFI, aes(x = no_membrs, fill = village)) +
  geom_density(alpha = 0.5, color = NA, adjust = 1) +
  labs(x = "\nNumber of household members",
       y = "Density\n",
       fill = "Village")
dev.off()

pdf("ridge_density.pdf")
ggplot(SAFI, aes(x = no_membrs, y = village, fill = village)) +
  geom_density_ridges(alpha = 0.7, color = "white", scale = 1.2) +
  labs(x = "\nNumber of household members",
       y = "Village\n",
       fill = "Village") 
dev.off()

pdf("boxplot.pdf")
ggplot(SAFI, aes(x = village, y = no_membrs, fill = village)) +
  geom_boxplot(alpha = 0.7, color = "#5ABD51", coef = 3, width = 0.4) +
  labs(fill = "Village", x = "\nVillage", y = "Household members\n")+
  theme_ellen()
dev.off()

pdf("violin.pdf")
ggplot(SAFI, aes(x = village, y = no_membrs, fill = village)) +
  geom_violin(alpha = 0.7, width = 0.8, draw_quantiles = c(0.25, 0.5, 0.75)) + #add marks in
  geom_jitter(width = 0.15, alpha = 0.6, size = 0.8) +
  labs(fill = "Village", x = "\nVillage", y = "Household members\n") +
  theme_ellen()
dev.off()

##############
### Group work
##############

# Download US trade data with Ireland for 2018-2020 
# table: yrpc
# reporters: USA
us_trade <- ots_create_tidy_data(
  years = 2018:2020,
  reporters = "USA",
  table = "yrpc"
)
head(us_trade)

### Data Manipulation

# Filter US imports from China (2018-2020),
from_china <- us_trade %>%
  filter(partner_name == "China") %>%
  year %in% 2018:2020 #sequence for integers

# create a variable for log import values, and 
us_trade <- us_trade %>% mutate(log_import = log(trade_value_usd_imp))
# WHy? Makes distribution more interpretable, normalised looking graph

# categorize commodities as:
# (1) Primary Goods (Agriculture & Raw Materials)
#   "Live animals; animal products", "Vegetable products", "Animal, vegetable or microbial fats and oils...", "Mineral products", 
#   "Raw hides and skins, leather, furskins...", "Wood and articles of wood...", "Natural or cultured pearls, precious..."
# (2) Intermediate Goods (Manufacturing Inputs)
#   "Products of the chemical or allied industries", "Plastics and articles thereof; rubber...", "Pulp of wood... paper and paperboard...", 
#   "Textiles and textile articles", "Articles of stone, plaster, cement...", "Base metals and articles of base metal"
# (3) Final Goods (Consumer & Capital Goods)
#   "Prepared foodstuffs; beverages...", "Footwear, headgear, umbrellas...", "Machinery and mechanical appliances...", "Works of art, collectors' pieces...",
#   "Vehicles, aircraft, vessels...","Optical, photographic... medical instruments", "Arms and ammunition", "Miscellaneous manufactured articles"
us_trade <- us_trade %>% 
  mutate(goods_type = case_when( #vectorise if else
    section_name %in% c(
      "Live animals; animal products", "Vegetable products", "Animal, vegetable or microbial fats and oils...", "Mineral products" ,"Raw hides and skins, leather, furskins...", "Wood and articles of wood...", 
      "Natural or cultured pearls, precious...") ~ "Primary Goods",
    section_name %in% c(
      "Products of the chemical or allied industries", "Plastics and articles thereof; rubber...", "Pulp of wood... paper and paperboard...", "Textiles and textile articles", "Articles of stone, plaster, cement...",
      "Base metals and articles of base metal") ~ "Intermediate Goods",
    section_name %in% c(
      "Prepared foodstuffs; beverages...", "Footwear, headgear, umbrellas...", "Machinery and mechanical appliances...", "Works of art, collectors' pieces...", "Vehicles, aircraft, vessels...","Optical, photographic... medical instruments", "Arms and ammunition",
      "Miscellaneous manufactured articles") ~ "Final Goods",
      
      TRUE ~ "Other"
    )
  )
head(us_trade)
  


# For 2020 US imports:
us_imp_2020 <- us_trade %>%
  filter(year == 2020) %>%
  group_by(partner_name) %>%
  summarise(
    total_imports = sum(trade_value_usd_imp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_imports)) %>%
  slice_head(n=5) #only top 5 in new dataset

us_imp_2020

# What are the top 5 import partners by total value
# What are the average import value by 2-digit HS code for top 3 partners
average_import_hs2 <- us_trade %>%
  filter(
    year == 2020,
    partner_name %in% us_imp_2020$partner_name[1:3]
  ) %>%
  group_by(partner_name, chapter_code) %>%
  summarize(
    avg_import = mean(trade_value_usd_imp, na.rm=TRUE),
    .groups = "drop"
  )

# What is the growth rate 2018→2020 by partner (requires pivot_wider)
growth_rate_partner <- us_trade %>%
  filter(year %in% c(2018, 2020)) %>%
  group_by(partner_name, year) %>% 
  summarize(
    total_imports = sum(trade_value_usd_imp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = total_imports
  ) %>%
  mutate(
    growth_rate = (`2020` - `2018`) / `2018`
  )

# Let's investiage electronics growth
# Filter chapter_code = 85 (Machinery and mechanical appliances...) imports
# Calculate year-over-year growth
# Rank partners by 2020 value AND growth rate
# Select & arrange top 10
electronics <- us_trade %>% 
  filter(
    chapter_code == "85",
    trade_value_usd_imp >0
  )

electronic_growth <-   electronics %>%
  group_by(partner_name, year) %>% 
  summarize(
    total_imports = sum(trade_value_usd_imp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(partner_name, year) %>%
  group_by(partner_name) %>%
  mutate(
    yoy_growth_rate = (total_imports - lag(total_imports)) / lag(total_imports)
  )
# lag, takes the previous value

#Rank parterns by 202 value and growth rate
electronics_rank <- electronic_growth %>%
  filter(year == 2020) %>%
  group_by(partner_name) %>%
  arrange(desc(total_imports), desc(yoy_growth_rank))

### Visualizations

# Visualize 2020 US import values:
# Histogram with default bins vs binwidth = 1e9, boundary = 0
ggplot(data = us_trade %>% filter(year == 2020), aes(x = trade_value_usd_imp)) +
  geom_histogram(binwidth = 1e9, boundary = 0)
# Log-scale version of best histogram
ggplot(data = us_trade %>% filter(year == 2020), aes(x = log_import)) +
  geom_histogram()

# Density plot overlay (import vs export)
us_trade <- us_trade %>%
  mutate(
    log_export = log(trade_value_usd_exp)
  )
us_long <- us_trade %>%
  filter(year == 2020) %>%
  pivot_longer(
    cols = c(log_import, log_export),
    names_to = "flow",
    values_to = "value"
  )

ggplot(data = us_long, aes(x = value, fill = flow)) +
  geom_density(alpha = 0.5, colour = NA, adjust = 1)

# Who are the top partners? Create:
# Stacked bar: Top 5 partners' share of total 2020 imports
ggplot(to)
# Dodged bars: Import vs Export comparison for top 5 partners

# Distribution exploration:
# Boxplot: Import value distribution by top 5 partners
# Violin + jitter: Same data with individual shipments
# Ridge plot: Partner distributions stacked vertically

# Extra questions:
# Which partner shows most growth variability 2018-2020?
# What binwidth best reveals the import value distribution?
# Which visualization best communicates "China dominates electronics"?