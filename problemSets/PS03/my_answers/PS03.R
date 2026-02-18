###### Data Visualisation PS03 ######
# Ellen Beattie
#### Set up ####

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
lapply(c("tidyverse", "ggplot2", "readr", "readxl", "showtext", "purrr", "ggridges", "ggdist", "xtable", "ggrepel", "extrafont", "WDI", "ggtext", "ggdist", "forecats", "patchwork"),  pkgTest)
loadfonts(device = "win")
# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

scale_fill_eb <- function(...) {
  scale_fill_manual(
    values = c(
      "#fd3838","#ff8c02", "#7cb04b",  "#7792ff",  "#b960da", "#da5799","#ff7257","#c83131", "#e17421", "#399e72", "#5e72c5",  "#9b5ab2",  "#a72264","#ff7257", "#ffb050", "#a4eac2", "#b6c3e8", "#e3aaf8","#ed90af","#fd3838","#ff8c02", "#7cb04b",  "#7792ff",  "#b960da"
    ),
    ...
  )
}
scale_colour_eb <- function(...) {
  scale_color_manual(
    values = c(
      "#fd3838","#ff8c02", "#7cb04b",  "#7792ff",  "#b960da", "#da5799","#ff7257", "#ffb050", "#a4eac2", "#b6c3e8", "#e3aaf8",  "#ed90af","#c83131", "#e17421", "#399e72", "#5e72c5",  "#9b5ab2",  "#a72264"
    ),
    ...
  )
}

#### Data Manipulation ####

# DM 1: Load Data #
ces2015 <- read_csv("~/GitHub/DataViz_2026/datasets/CES2015.csv") 
ces2015 <- ces2015 %>% filter(discard == "Good quality")


# DM 2:  Filter by answered question
unique(ces2015$p_voted) #unique answers incl NA/ "1000"
table(ces2015$p_voted) #see rough split to make sure its right
ces2015 <- ces2015 %>% 
  filter(p_voted %in% c("Yes", "No")) #filter only to those who responded yes or no





# DM 3:Create Age Variable
str(ces2015$age) #character need to convert integer
table(ces2015$age) #check for missing values / potentially miscoded entries (i.e 1000)

ces2015 <- ces2015 %>%
  filter(!age %in% c("refused", "don't know", "1000")) %>% #remove nonvalid yobs
  mutate(
    age = as.integer(age), #convert to numeric
    age = 2015 - age, #years old as of 2015
    cat_age = cut( #create categorical variable
          age,
          breaks = c(0, 25, 35, 45, 55, 65, 75, 150), #range breaks
          labels= c("18-25", "26-35", "36-45", "46-55","56-65", "65-75", "<75") 
        )
    )
        
table(ces2015$age) #check age range looks accurate
table(ces2015$cat_age) #view categories


####  Data Visualisation #### 86
# DV 1: Turnout rate by age group
turnout_agegroup<- ces2015 %>%
  filter(!is.na(cat_age)) %>% #filter na age categories
  group_by(cat_age) %>% #group within age category to
  summarise(turnout = mean(p_voted == "Yes", na.rm = TRUE)) #turnout rate as yes voted vs no (filtered earlier)

plot1 <- ggplot(data = turnout_agegroup, aes(x = cat_age, y = turnout, fill = cat_age)) +
  geom_col(alpha = 0.6, width = 0.85) +
  scale_y_continuous(
    limits = c(0,1),
    breaks = seq(0, 1, by  = 0.2),
    expand = c(0,0),
    sec.axis = sec_axis(~. * 100,  #add second axis for fun transform to percentage
                        labels = function(x) paste0(x, "%")) #add % label
  ) +
  scale_fill_eb() + 
  theme_classic(base_size = 12, base_family = "Arial") +
  labs(x = "Age Group", y = "Turnout rate",
       title = "2015 Canada Election Turnout Rate by Age Group", caption = "2015 Canadian Electoral Survey") +
  guides(fill = "none") +
  theme(
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13)
  )
ggsave("PS03_p1.pdf", plot1, device = cairo_pdf)

# DV 2: Density Plot of ideology by party
table(is.na(ces2015$p_selfplace)) #number missing values for selfplace
unique(ces2015$p_selfplace) #can see NA and 1000 out of range
unique(ces2015$vote_for) #party intend to vote for, see spellings/missing values
table(ces2015$vote_for) #baseline to check for after


ideo_by_party <- ces2015 %>%
  mutate(
    p_selfplace = as.integer(p_selfplace), #turn to integer
    vote_for = as.factor(vote_for) #transform to factor
  ) %>%
  filter(!is.na(p_selfplace), p_selfplace != "1000") %>% #remove na and 1000 values
  filter(vote_for %in% c("Liberal", "ndp", "Green Party", "Conservatives", "Bloc Quebecois")) %>%
  mutate(vote_for = recode(factor(vote_for), "ndp" = "NDP"))

table(ideo_by_party$vote_for) #check looks correct and parties
table(ideo_by_party$p_selfplace)
unique(ideo_by_party$vote_for)

#Order by mean left-right
ideo_by_party$vote_for <- reorder(ideo_by_party$vote_for, ideo_by_party$p_selfplace, FUN = mean)


plot2 <- ggplot(ideo_by_party, aes(x= p_selfplace, fill= vote_for)) +
  geom_density(alpha = 0.7, bw = 0.4) +  #scale height of factors
  geom_vline(xintercept = 5, linetype = "dashed") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  facet_wrap(vars(vote_for), nrow = 5)+
  coord_cartesian(xlim = c(0,10)) +
  scale_x_continuous(limits = c(0,10),
                     breaks = 0:10,
                     expand = c(0,0)) +
  scale_fill_manual(values = c( #manually set colours to hexcodes extracted from wiki page
    "Liberal" = "#EA6D6A", #correct party colours for clarity
    "NDP" = "#F4A460",
    "Green Party" = "#99C955",
    "Conservatives" = "#6495ED",
    "Bloc Quebecois" = "#87CEFA")) +
  labs(x = "Left Wing                                  Center                                 Right Wing",
       title = "Left-Right Self-Placement of Individuals Indending to Vote for the Main Parties\n",
       caption = "2015 Canadian Election Survey political orientation score of intended party voters") +
  guides(fill = "none") +
  theme_classic(base_family = "CMU Sans Serif") +
  theme(
    axis.text.y = element_blank(), #rmv y axis
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.title.x = element_text(size = 13, face = "bold"),
    strip.background = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    strip.text.x = element_text(size = 12, hjust = 0.5),
    panel.spacing = unit(1, "cm") 
  )
ggsave("PS03_p2.pdf", plot2, device = cairo_pdf)


# DV3:  Hisogram Counts turnout by income facet province 172
table(ces2015$income_full) #see range of factors
table(ces2015$province) #see codes used for shorthand of provinces
ces2015 <- ces2015 %>%
  mutate(province = as.factor(province)) %>% #turn into factor
  mutate(province = recode_factor(province,#change based on codebook
                                  bc = "British Columbia",
                                  nb = "New Brunswick",
                                  ns = "Nova Scotia",
                                  nwt = "Northwest Territories",
                                  Nfld = "Newfoundland and \n Labrador",
                                  pei = "Prince Edward Island",
                                  Sask = "Saskatchewan")) %>%
  mutate(income_full = as.factor(income_full)) %>% #turn income into factor
  filter(!income_full %in% c(".r", ".d")) %>% droplevels() #remove the missing categories
           
table(ces2015$income_full) 
ces2015$income_full <- relevel(ces2015$income_full,
                              ref = c("less than $29,999")) #specify levels of factor lowest
                  #to highest


ces2015<- ces2015 %>% #reorder for facet placement by size of province
  mutate(province = fct_reorder(province, province, .fun = length, .desc = TRUE)) 


turnout_income <- ces2015 %>%  #create turnout by income variable
  group_by(province, income_full) %>% #grouped by province
  filter(!is.na(income_full)) %>% #remove na
  summarise(turnout = sum(p_voted == "Yes", na.rm = TRUE))  
  
class(turnout_income$province)  

plot3 <- ggplot(turnout_income, aes(x = income_full, y = turnout, fill = income_full)) +
  geom_col(alpha = 0.75)+
  facet_wrap(vars(province), ncol = 5) +
  scale_y_continuous(breaks = seq(0, 400, by = 50), expand = c(0,5)) +
  theme_classic(base_family = "CMU Sans Serif")+
  labs(y = "Turnout Count", fill = "Income Level",
       title = "Canadian Province Turnout Counts by Income Level", 
       caption = "Canadian Electoral Survey 2015. Total number of respondants who said 'Yes' when asked if they voted") +
  scale_fill_eb() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_line(colour = "black"), 
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 6.5, colour = "white", face = "bold"),
    strip.background = element_rect(fill = "navy", colour = "navy"), #blue box white text
    panel.border = element_rect(color = "navy", fill = NA, linewidth = 0.4),
    panel.grid.major.y = element_line(colour = "#e6e9ff", linewidth =  0.2), #very light grid
    panel.grid.major.x = element_line(colour = "#e6e9ff", linewidth =  0.2),
    legend.background = element_rect(fill = "white",colour = "grey30", size = 0.5, linetype = "solid")
  ) +
  guides(fill = guide_legend(nrow = 2))
ggsave("PS03_p3.pdf", plot3, device = cairo_pdf)

# DV 4: Create customisable theme
theme_ellen <- function(...) {
  theme_classic(base_size = 12, base_family = "CMU Sans Serif") + 
    theme(
      # Titles
      plot.title = element_text(face = "bold", size = rel(1.3)),
      plot.subtitle = element_text(face = "plain", size = rel(1.1), color = "grey70"),
      plot.caption = element_text(face = "italic", size = rel(0.7), 
                                  color = "grey70", hjust = 0),
      #Backgrounds 
      # Grid
      panel.background = element_rect(fill = "white", colour = NA), 
      panel.border = element_blank(), 
      panel.grid = element_blank(), 
      panel.grid.minor = element_blank(), 
      
      #Axis
      axis.title = element_text(family = "CMU Sans Serif", size = 12), #title font
      axis.line.x = element_line(linewidth = 0.4), #axis lines
      axis.line.y = element_line(linewidth = 0.4),
      axis.text = element_text(colour = "black", size = 10), #axis text change
      axis.ticks.x = element_line(colour = "black", linewidth = 0.75),
      
      #Facets
      strip.background = element_blank(),
      strip.text = element_text(size = 11),
      
      #Legend
      legend.title = element_text(family = 'CMU Sans Serif', size = 12, colour = 'black'), #legend
      legend.text =  element_text(family = 'CMU Sans Serif', size = 10, colour = 'black'),# font change
      legend.key.height = unit(1, "lines"),#spacing
      legend.key.width = unit(1, "lines"), 
      legend.spacing.y = unit(1, "lines"),
      legend.background = element_rect( 
        fill = NA,
        colour = "grey60", #make box around legend to align it
        size = 0.5,
        linetype = "solid")
    )
} 

#Apply to previos
plot4 <- plot1 + theme_ellen() + #ironically done most of this in first plt
  coord_flip() +
  labs(title = "Turnout rate increased with Age in the 2015 Canadian Election",
       subtitle = "Percentage of respondants proporting to have voted in the 2015 election by age group.",
       caption = "Source: Canadian Election Study 2015 Post-Election Poll. \n Poor quality participants and those with missing or irregular responses were removed.") +
  theme(
     axis.text.x = element_blank(),
     axis.line.x = element_blank(),
     axis.title.x = element_blank(),
     axis.ticks.x = element_blank()
   ) +
  scale_fill_manual(values = c("#fdd735", "#ffb14e", "#fa8775", "#ea5f94", "#cd34b5", "#9d02d7", "#0d0dca")) +
  scale_y_continuous(limits = c(0,1.3), expand = c(0,0)) +
  geom_text(
    aes(label = scales::percent(turnout, accuracy = 1)),
    hjust = -0.1) +
  annotate(geom = "segment", x = 1, xend = 7, y = .99, yend = 1.1, colour = "darkblue", 
           arrow = arrow(angle = 15, length = unit(0.5, "lines"))) +
  annotate(geom = "text", x = 3, y = 1.04, label = "Turnout rate \n increases gradually with age", hjust = 0, colour = "darkblue", size = 4)
  
ggsave("PS03_p4.pdf", plot4, device = cairo_pdf)
