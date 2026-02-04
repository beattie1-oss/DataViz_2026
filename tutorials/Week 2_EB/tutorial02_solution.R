###############################
# Data Viz  
# Tutorial 2:  
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
lapply(c("tidyverse", "ggplot2", "ggridges", "tradestatistics", "dplyr"),  pkgTest)


# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#################
### In class demo
#################

# Import dataset and load library
SAFI <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/DataViz_2026/refs/heads/main/datasets/SAFI.csv")		

str(SAFI)

# All households in village "God"
SAFI_god <- filter(SAFI, village == "God")

# Households with more than 6 members
large_households <- filter(SAFI, no_membrs > 6)

# Households in "God" or "Ruaca" with more than 4 members
god_ruaca_large <- filter(SAFI, village %in% c("God", "Ruaca") & no_membrs > 4)

# Keep only identification and location variables
id_loc <- SAFI |>
  select(key_ID, village)

# Move village to the first position
village_first <- SAFI |>
  select(village, everything())

# Drop columns related to survey timing
no_timing_n <- SAFI |>
  select(-starts_with("intervie"))

# Households ordered by size (ascending)
SAFI_by_size <- SAFI |>
  arrange(no_membrs)

# Households ordered by size (descending) within village
SAFI_by_village_size <- SAFI |>
  arrange(village, desc(no_membrs))

# Add a logical indicator for "large" households
SAFI <- SAFI |>
  mutate(large_hh = no_membrs >= 6)

# Years of living in village in decades
SAFI <- SAFI |>
  mutate(living_decades = years_liv / 10)

# Categorize household size
SAFI <- SAFI |>
  mutate(
    hh_size_cat = case_when(
      no_membrs <= 3            ~ "small",
      no_membrs >= 4 & no_membrs <= 7 ~ "medium",
      no_membrs > 7             ~ "large"
      )
  )

# Large households in Ruaca with selected columns
ruaca_large_small_df <- SAFI |>
  filter(village == "Ruaca", no_membrs > 6) |>
  mutate(
    large_hh = TRUE,
    members_per_room = no_membrs / rooms
  ) |>
  select(village, no_membrs, rooms, members_per_room, large_hh)

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
  geom_histogram(color = "white", fill = "#E16462") +
  labs(x = "\nNumber of household members", 
       y = "Count\n")
dev.off()

pdf("hist_bin1.pdf")
ggplot(SAFI, aes(x = no_membrs)) +
  geom_histogram(binwidth = 1, color = "white", fill = "#E16462") +
  labs(x = "\nNumber of household members", 
       y = "Count\n")
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
  facet_wrap(vars(village)) +
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
  labs(fill = "Village", x = "\nVillage", y = "Household members\n")
dev.off()

pdf("violin.pdf")
ggplot(SAFI, aes(x = village, y = no_membrs, fill = village)) +
  geom_violin(alpha = 0.7, width = 0.8, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 0.8) +
  labs(fill = "Village", x = "\nVillage", y = "Household members\n")
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

### Data Manipulation

# Filter US imports from China (2018-2020),
# create a variable for log import values, 

us_imports_chn <- us_trade %>%
  filter(
    partner_iso == "CHN",
    year %in% 2018:2020
  )

us_imports_chn <- us_imports_chn %>%
  mutate(
    log_import = log(trade_value_usd_imp)
  )

us_trade <- us_trade %>%
  mutate(
    log_import = log(trade_value_usd_imp)
  )
# Why do the transformation?
#Trade values are extremely skewed; Logs make distributions interpretable


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
  mutate(
    goods_type = case_when(
      section_name %in% c(
        "Live animals; animal products",
        "Vegetable products",
        "Animal, vegetable or microbial fats and oils...",
        "Mineral products",
        "Raw hides and skins, leather, furskins...",
        "Wood and articles of wood...",
        "Natural or cultured pearls, precious..."
      ) ~ "Primary Goods",
      
      section_name %in% c(
        "Products of the chemical or allied industries",
        "Plastics and articles thereof; rubber...",
        "Pulp of wood or of other fibrous cellulosic material; paper and paperboard...",
        "Textiles and textile articles",
        "Articles of stone, plaster, cement...",
        "Base metals and articles of base metal"
      ) ~ "Intermediate Goods",
      
      section_name %in% c(
        "Prepared foodstuffs; beverages, spirits and vinegar...",
        "Footwear, headgear, umbrellas...",
        "Machinery and mechanical appliances; electrical equipment...",
        "Works of art, collectors' pieces and antiques",
        "Vehicles, aircraft, vessels...",
        "Optical, photographic, cinematographic, measuring, checking, precision, medical instruments",
        "Arms and ammunition; parts and accessories thereof",
        "Miscellaneous manufactured articles"
      ) ~ "Final Goods",
      
      TRUE ~ "Other"
    )
  )

unique(us_trade$goods_type)

# For 2020 US imports:
# What are the top 5 import partners by total value
top5_partners_2020 <- us_trade %>%
  filter(year == 2020) %>%
  group_by(partner_name) %>%
  summarize(
    total_imports = sum(trade_value_usd_imp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_imports)) %>%
  slice_head(n = 5)

# What are the average import value by 2-digit HS code for top 3 partners
## What it means?: For the three biggest US import partners, 
##what is the typical import size in each broad product category?
## What is an HS code? HS = Harmonized System
## It’s an international product classification system.
## The number of digits tells you how detailed the product category is (2-digit: broad; 4-digit: sub-category; 6-digit: detailed product)

### Prepare HS2 code
us_trade <- us_trade %>%
  mutate(
    hs2 = substr(commodity_code, 1, 2)
  )

### Compute averages
avg_import_hs2 <- us_trade %>%
  filter(
    year == 2020,
    partner_name %in% top5_partners_2020$partner_name[1:3]
  ) %>%
  group_by(partner_name, hs2) %>%
  summarize(
    avg_import = mean(trade_value_usd_imp, na.rm = TRUE),
    .groups = "drop"
  )

#log_import should be considered?

# What is the growth rate 2018→2020 by partner (requires pivot_wider)
## What it means?: For each trading partner, how did total US imports change 
##between 2018 and 2020, expressed as a percentage?

growth_partner <- us_trade %>%
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


# Let's investigate electronics growth
# Filter chapter_code = 85 (Machinery and mechanical appliances...) imports
electronics <- us_trade %>%
  filter(
    chapter_code == "85",
    trade_value_usd_imp > 0
  )

# Calculate year-over-year growth
electronics_growth <- electronics %>%
  group_by(partner_name, year) %>%
  summarize(
    total_imports = sum(trade_value_usd_imp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(partner_name, year) %>%
  group_by(partner_name) %>%
  mutate(
    yoy_growth = (total_imports - lag(total_imports)) / lag(total_imports)
  )

#is.grouped_df(electronics_growth)

# Rank partners by 2020 value AND growth rate
electronics_rank <- electronics_growth %>%
  filter(year == 2020)  %>%
  group_by(partner_name) %>%
  arrange(desc(total_imports), desc(yoy_growth)) 

# Select & arrange top 10
electronics_rank_10 <- electronics_growth %>%
  filter(year == 2020) %>%
  ungroup() %>%
  arrange(desc(total_imports), desc(yoy_growth)) %>%
  slice_head(n = 10)


### Visualizations

# Visualize 2020 US import values:
# Histogram with default bins vs binwidth = 1e9, boundary = 0
ggplot(us_trade %>% filter(year == 2020),
       aes(trade_value_usd_imp)) +
  geom_histogram() +
  labs(title = "Default histogram (30 bins)")

ggplot(us_trade %>% filter(year == 2020),
       aes(trade_value_usd_imp)) +
  geom_histogram(binwidth = 1e9, boundary = 0) +
  labs(title = "Histogram with binwidth = 1e9")

# Log-scale version of best histogram
ggplot(us_trade %>% filter(year == 2020),
       aes(log_import)) +
  geom_histogram() 
# or use: + scale_x_log10()

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

ggplot(us_long, aes(x = value, fill = flow)) +
  geom_density(alpha = 0.5, color = NA, adjust = 1) +
  labs(x = "\nLog value of trade",
       y = "Density\n",
       title = "Density plot overlay (import vs export)",
       fill = "Trade flow")

# Who are the top partners? Create:
# Stacked bar: Top 5 partners' share of total 2020 imports
ggplot(top5_partners_2020,
       aes(x = "", y = total_imports, fill = partner_name)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "2020",
    y = "Proportion of total imports\n",
    fill = "Partner",
    title = "Top 5 partners' share of total 2020 imports"
  ) 

# Dodged bars: Import vs Export comparison for top 5 partners

#top5_flows <- us_long %>%
  #filter(
   # year == 2020,
  #  partner_name %in% top5_partners_2020$partner_name
  #)

top5_flows_summary <- us_long %>%
  filter(
    year == 2020,
    partner_name %in% top5_partners_2020$partner_name
  ) %>%
  group_by(partner_name, flow) %>%
  summarize(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(top5_flows_summary,
       aes(x = partner_name, y = value, fill = flow)) +
  geom_col(position = "dodge") +
  labs(
    x = "Partner",
    y = "Log trade value",
    fill = "Trade flow",
    title = "Import vs Export comparison for top 5 partners"
  )

# Distribution exploration:
# Boxplot: Import value distribution by top 5 partners
us_trade %>%
  filter(
    partner_name %in% top5_partners_2020$partner_name
  ) %>%
  ggplot(aes(x = partner_name, y = log_import)) +
  geom_boxplot(alpha = 0.7, coef = 1.5, width = 0.4) +
  labs(x = "\nPartner", y = "Log import\n")

# Violin + jitter: Same data with individual shipments
us_trade %>%
  filter(
    partner_name %in% top5_partners_2020$partner_name
  ) %>%
  ggplot(aes(x = partner_name, y = log_import)) +
  geom_violin(alpha = 0.7, width = 0.8, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(width = 0.15, alpha = 0.2, size = 0.1) +
  labs(x = "\nPartner", y = "Log import\n")

# Ridge plot: Partner distributions stacked vertically
us_trade %>%
  filter(
    partner_name %in% top5_partners_2020$partner_name
  ) %>%
  ggplot(aes(x = log_import, y = partner_name, fill = partner_name)) +
  geom_density_ridges(alpha = 0.7, color = "white ", scale = 1.5) +
  labs(x = "\nLog import", y = "Partner\n", fill = "Partner")

# Extra questions:
# Which partner shows most growth variability 2018-2020?
##Partner with largest SD of YoY growth across years.
growth_variability <- electronics_growth %>%
  filter(!is.na(yoy_growth)) %>%      # remove first year (NA growth)
  group_by(partner_name) %>%
  summarize(
    sd_growth = sd(yoy_growth, na.rm = TRUE),
    mean_growth = mean(yoy_growth, na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(sd_growth))

# Partner with most variability
growth_variability %>% slice_head(n = 1)

# What binwidth best reveals the import value distribution?
##One that: Reveals skew; Avoids spike noise; 
##Usually log-scale + wide bins
ggplot(us_trade %>% filter(year == 2020),
       aes(log_import)) +
  geom_histogram() 
# or use: + scale_x_log10()

# Which visualization best communicates "China dominates electronics"?
##Stacked bar of HS85 by partner

electronics_2020 <- electronics %>%
  filter(year == 2020,
         partner_name %in% top5_partners_2020$partner_name) %>%
  group_by(partner_name) %>%
  summarize(
    total_imports = sum(trade_value_usd_imp, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(electronics_2020,
       aes(x = "", y = total_imports, fill = partner_name)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "",
    y = "Share of HS85 imports",
    fill = "Partner",
    title = "Partner share of US electronics imports (2020)"
  )


ggplot(electronics %>%
         filter(year == 2020,
                partner_name %in% top5_partners_2020$partner_name),
       aes(x = log_import,
           y = partner_name,
           fill = partner_name)) +
  geom_density_ridges(scale = 1.2, alpha = 0.7) +
  labs(
    x = "Import value (log scale)",
    y = "Partner",
    fill = "Partner",
    title = "Distribution of electronics import values by partner"
  )
