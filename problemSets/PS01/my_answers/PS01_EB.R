###### Data Visualisation PS01 ######

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
lapply(c("tidyverse", "ggplot2", "readr", "readxl", "purrr", "ggridges"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#### Data Manipulation ####

## Load Data to Global environment ##
excel_sheets('mep_info_26Jul11.xls') #look at the different sheets in xls
EPs <- c("EP1", "EP2", "EP3", "EP4", "EP5") #names of sheets we care to load
MEP_info <-  lapply( #apply to multiple
  EPs, 
  function(i) { #function to combine each year from respective sheets
    readxl::read_excel('mep_info_26Jul11.xls', sheet = i) %>% #each i is in EPs
      mutate(
        `NOM-D1`= as.numeric(`NOM-D1`), #ensure specified as numeric info
        `NOM-D2` = as.numeric(`NOM-D2`)
      ) %>%
      mutate(EP = i) #add name
    }
)
names(MEP_info) <- EPs #name the elements as years
MEP_df <- bind_rows(MEP_info) #transofrm to dataframe


#Read in RCV
RCV1 <- read_delim("rcv_ep1.txt", delim = ",", locale = locale(encoding = "UTF-8"))
RCV2 <- read_delim("rcv_ep2.txt", delim = ",", locale = locale(encoding = "UTF-8"))
RCV3 <- read_delim("rcv_ep3.txt", delim = ",", locale = locale(encoding = "UTF-8"))
RCV4 <- read_delim("rcv_ep4.txt", delim = ",", locale = locale(encoding = "UTF-8"))
RCV5 <- read_delim("rcv_ep5.txt", delim = ",", locale = locale(encoding = "UTF-8"))


## Tidy Votes into long format ## 
RCV1 <- RCV1 %>% pivot_longer(cols = -(1:5), names_to = "Votes", values_to = "Outcome")
RCV2 <- RCV2 %>% pivot_longer(cols = -(1:5), names_to = "Votes", values_to = "Outcome")
RCV3 <- RCV3 %>% pivot_longer(cols = -(1:5), names_to = "Votes", values_to = "Outcome")
RCV4 <- RCV4 %>% pivot_longer(cols = -(1:5), names_to = "Votes", values_to = "Outcome")
RCV5 <- RCV5 %>% pivot_longer(cols = -(1:5), names_to = "Votes", values_to = "Outcome")

##  MEP Decision Categories ##
decision125 <- c( #from the data link
  "0" = "Absent",
  "1" = "Yes",
  "2" = "No",
  "3" = "Abstain",
  "4" = "Present but no vote",
  "5" = "Not an MEP")

decision34 <- c( #from the data link the codes for EP 3/4
  "0" = "Absent or Not an MEP",
  "1" = "Yes",
  "2" = "No",
  "3" = "Abstain",
  "4" = "Present but no vote")


RCV1 <- RCV1 %>% mutate(Outcome_Category =  decision125[as.character(Outcome)])
RCV2 <- RCV2 %>% mutate(Outcome_Category =  decision125[as.character(Outcome)])
RCV3 <- RCV3 %>% mutate(Outcome_Category =  decision34[as.character(Outcome)])
RCV4 <- RCV4 %>% mutate(Outcome_Category =  decision34[as.character(Outcome)])
RCV5 <- RCV5 %>% mutate(Outcome_Category =  decision125[as.character(Outcome)])
#creates new categorical variable that codes the outcome referencing decision code 

#Summary EP 1 of Counts across all votes
RCV1 %>% 
  group_by(Outcome_Category) %>% #different groups to count
  summarise(count = n()) %>% #count of each subgroup
  arrange(desc(count)) #descending order to see popularity

## Contruct new combined dataset ##
#All EP's
RCV_all <- bind_rows(
  RCV1 %>% mutate(EP = "EP1"),#create new variable with codes for EP session
  RCV2 %>% mutate(EP = "EP2"),
  RCV3 %>% mutate(EP = "EP3"),
  RCV4 %>% mutate(EP = "EP4"),
  RCV5 %>% mutate(EP = "EP5"))

# Rename ID to match and join data
MEP_df <- MEP_df %>% rename('MEPID' =`MEP id`) #make them identical to combine

RCV_com <- RCV_all %>% 
  left_join(MEP_df %>% select(EP, `MEPID`, `NOM-D1`,`NOM-D2`), #merge these variables
            by = c("EP", "MEPID")) #common key is ID and EP to mesh them together

#Check for NA's
print(RCV_com %>%
  summarise(across(everything(), ~sum(is.na(.)))))

#See the starting rows
RCV_com %>% summarise(n_rows = nrow(.))

#Remove any rows with na()
RCV_com <- RCV_com %>%
  tidyr::drop_na()

#Check new entries
RCV_com %>% summarise(n_rows = nrow(.))


##  Comparison of EP Groups in EP1 ##
EP1_grouped <- RCV_com %>%  #make a group just of EP1
  filter(EP == "EP1") %>% #work with first parliment only
  group_by(EPG)
  
EP1_grouped %>%
  summarise(count = n()) %>%  #count the total number of votes/decisions in each group 
  arrange(desc(count)) #see most populated group

# Yes rate #
EP1_grouped %>%
  summarise( #create a Yes_rate variable summimg up outcome categories (as grouped EPG)
    Yes_Rate = sum(Outcome_Category == "Yes") / sum(Outcome_Category %in% c("Yes", "No", "Abstain"))
    )

# Abstain Rate #
EP1_grouped %>%
  summarise( #repeat for abstain rate
    Abstain_Rate = sum(Outcome_Category == "Abstain") / n() # divall categories 
    )


# 150 EP1 Mean preference along dimensions NOM-D1 and NOM-D2 #
EP1_grouped %>%
  filter(Outcome_Category == "Yes") %>% #isolate just within those with yes outcome
  summarise(
    mean_D1 = mean(`NOM-D1`), #average dimension at mean for each EP group
    mean_D2 = mean(`NOM-D2`)
  )

EP1_grouped %>%
  filter(Outcome_Category == "No") %>% #repeat for no
  summarise(
    mean_D1 = mean(`NOM-D1`),
    mean_D2 = mean(`NOM-D2`)
    )

EP1_grouped %>%
  filter(Outcome_Category == "Abstain") %>% #and abstain
  summarise(
    mean_D1 = mean(`NOM-D1`),
    mean_D2 = mean(`NOM-D2`)
    )


#### Data Visualisation ####

##---------- Plot 1 ----------## 175

MEPs <- RCV_com %>% #make a dataframe for just the MEP's (not longform votes)
  group_by(MEPID, EP, EPG) %>% #capture id, group and parliment
  summarise(
    D1 = first(`NOM-D1`),  #capture the first value of D1
    D2 = first(`NOM-D2`)
  ) %>%
  ungroup() #ungroup to explore as whole set

pdf("PS01_Fig1.pdf")
ggplot(MEPs, aes(y = EPG, x = D1 , fill = EPG)) + 
  # https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
  geom_density_ridges( #to see multiple density plots in one
    color = "black", #line colour
    alpha = 0.5, #transparency
    scale = 1.5,
    rel_min_height = 0.005,#hight of distribution
    linewidth = 0.4 #size of line
  ) +
  scale_x_continuous(limits = c(-1, 1), #start and end at dimension max/min
                     breaks = seq(-1 ,1 ,0.25), #sequence for x axis
                     minor_breaks = NULL, #reduce some of clustered grid
                     expand = c(0,0)) + #stop grid going beyond limits
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.3) + #add line at zero
  geom_vline(xintercept = 1, color = "black", size = 0.5) + #add axis at +1
  theme_minimal() +
  labs(x = "Nominate Dimension 1", y = "EP Group", fill = "EP Group") + #labels
  theme(
    axis.title = element_text(family = "serif", size = 14), #title font
    axis.line.x = element_line(size = 0.4), #axis lines
    axis.line.y = element_line(size = 0.4),
    axis.text = element_text(colour = "black", size = 10), #axis text change
    axis.ticks.x = element_line(colour = "black", size = 0.75),
    panel.grid.major.y = element_line(colour = "grey40"), #change horiz grid to stronger
    panel.grid.major.x = element_line(colour = "grey90"), #lighter vertical gridlines
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
dev.off()


##---------- Plot 2 ----------## 225

MEP_ind <- MEPs %>%  #isolate MEP individuals (so MEP's in multiple EP's appear once
  group_by(MEPID, EPG) %>% #one id per mep
  summarise(D1 = mean(D1),D2 = mean(D2)) %>% #summarise their dimension as mean (if many)
  ungroup()

pdf("PS01_Fig2.pdf", width = 10, height = 8)
ggplot(MEP_ind, aes(x = D1, y = D2, color = EPG))+ #plot each individual MEP, colour by EPG
  geom_point(alpha = 0.5, size = 1.5, na.rm = TRUE) + #add points to make scatter
  geom_hline(yintercept = 0, color = "grey40", size = 0.3) + #add grid axis at 0,0
  geom_vline(xintercept = 0,color = "grey40", size = 0.3) +
  geom_vline(xintercept = 1,color = "grey40", size = 0.3) +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1, 0.5), expand = c(0,0)) + #make scale fit -1 to +1
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1, 0.5), expand = c(0,0)) + #same again
  labs(x = "Nominate Dimension 1",y = "Nominate Dimension 2", colour = "EP Group") +
  theme_minimal()+
  theme(
    panel.border = element_rect(colour = "grey60", fill = NA, size = 0.5),#add grey border
    axis.title = element_text(family = "serif", size = 14), #title font
    axis.text = element_text(colour = "black", size = 11), #axis text change
    axis.ticks.x = element_line(colour = "black", size = 0.75),
    legend.title = element_text(family = 'serif', size = 13, colour = 'black'), #legend
    legend.text =  element_text(family = 'serif', size = 12, colour = 'black'),# font change
    legend.position = "right", #position right
    legend.margin = margin(10,10,10,10), #add space around
    legend.background = element_rect(
      fill = "white",
      colour = "grey50", #make box aound legend to align it
      size = 0.5,
      linetype = "solid")
  )
dev.off()
  
  
##---------- Plot 3 ----------## 260
table(RCV_com$NP) 

Yes_rate <- RCV_com %>% #make a Yes rate 
    group_by(MEPID, EPG) %>% #grouping by id and EP group 
    summarise(total_votes = n(),#list of group as votes in long form
              yes_votes = sum(`Outcome_Category` == "Yes"), #sum within them of yes
              yes_rate = yes_votes / total_votes #yes rate
              )

pdf("PS01_Fig3.pdf")
ggplot(Yes_rate, aes(x=EPG, y = yes_rate, fill = EPG)) +#boxplot by group, yes rate on y
  geom_boxplot(alpha = 0.6) +
  labs(x = "EP Group",y = "Proportion Voting 'Yes'", fill = "EP Group" ) +
  scale_y_continuous(limits = c(0,0.65), breaks = seq(0, 0.65, 0.05), expand = c(0,0)) + #fitting boundaries
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "grey60", fill = NA, size = 0.5),#box round
    axis.title = element_text(family = "serif", size = 14), #title font
    axis.text = element_text(colour = "black", size = 11), #axis text change
    axis.ticks.x = element_line(colour = "black", size = 0.75),
    legend.title = element_text(family = 'serif', size = 13, colour = 'black'), #legend
    legend.text =  element_text(family = 'serif', size = 12, colour = 'black'),# font change
    legend.position = "right",
    legend.margin = margin(10,10,10,10),
    legend.background = element_rect(
      fill = "white",
      colour = "grey50", #make box around legend to align it
      size = 0.5,
      linetype = "solid")
  )
dev.off()



  ##---------- Plot 4 ----------## 250
length(unique(RCV_com$NP)) #163 parties

NP_Family <- readxl::read_excel('mep_info_26Jul11.xls', sheet = 'Codes-National Parties') %>% #
  select('Code', 'Party Family') %>% #extract info on party family
  rename(NP = `Code`, #code of party
         party_family = `Party Family`)
unique(NP_Family$party_family) #11 'National Party families'

RCV_com <- RCV_com %>% 
  left_join(NP_Family, by = "NP") #add back to the dataframe, matching NP code

Yes_rate_NP_EP <- RCV_com %>% #yes rate for each party for each year
  group_by(MEPID, NP, party_family, EP) %>% #grouping
  summarise(total_votes = n(),
            yes_votes = sum(`Outcome_Category` == "Yes"),
            yes_rate_NP_EP = yes_votes / total_votes)

Yes_rate_NP_family <- Yes_rate_NP_EP %>% #grouping by family of parties 
  group_by(EP, party_family) %>% #get mean rate of national parties within
  summarise(mean_yes_rate_NP_EP = mean(yes_rate_NP_EP, .groups = "drop"))

pdf("PS01_Fig4.pdf")
ggplot() + #initialise empty plot (as doing both points and bar)
  geom_col(data = Yes_rate_NP_family ,#use family level data for bar 
           aes(x = factor(EP), y = mean_yes_rate_NP_EP, fill = party_family), #colour family
           color = "black", width = 0.6, alpha = 0.6) + 
  geom_point(data = Yes_rate_NP_EP, #for points use average party rate for each year
              aes(x = factor(EP), y = yes_rate_NP_EP),  #plug in from above
             size = 0.4, alpha = 0.5) +
  facet_wrap(~ party_family, ncol = 3) + #wrap to get subplot for each party_family, w 3 cols
  scale_y_continuous(limits = c(0, 0.65)) + #restrict limits to see patterm more clearly
  labs(x = "European Parliament", y = "Proportion of 'Yes' Votes", fill = "Party Family") +
  theme_minimal()+
  theme(
    panel.border = element_rect(colour = "grey60", fill = NA, size = 0.5),
    axis.title = element_text(family = "serif", size = 14), #title font
    axis.text = element_text(colour = "black", size = 10), #axis text change
    axis.ticks.x = element_line(colour = "black", size = 0.75),
    strip.text = element_text(family = 'serif', size = 11, colour = 'black'),
    legend.title = element_text(family = 'serif', size = 13, colour = 'black'), #legend
    legend.text =  element_text(family = 'serif', size = 12, colour = 'black'),# font change
    legend.position = "right",
    legend.margin = margin(10,10,10,10),
    legend.background = element_rect(
      fill = "white",
      colour = "grey50", #make box around legend to align it
      size = 0.5,
      linetype = "solid")
  )
dev.off()


##---------- Plot 5 ----------## 340
#Data separate
Yes_rate_EPG <- RCV_com %>% #now grouping by group per EP to see time prog
  group_by(EP, EPG) %>% #splitting data
  summarise( #same again
    total_votes = n(),
    yes_votes = sum(`Outcome_Category` == "Yes"), 
    yes_rate = yes_votes / total_votes
  )

pdf("PS01_Fig5.pdf")
ggplot(Yes_rate_EPG, aes(x=EP, y = yes_rate, colour = EPG, group = EPG))+ 
  geom_line(size = 1, alpha = 0.7) + #line mapping each group through EP's
  geom_point(size = 2) + #adding points on mean yes rate
  scale_y_continuous(limits = c(0, 0.525), breaks = seq(0, 0.65, 0.05), expand = c(0,0)) +
  labs(x = "European Parliament", y = "Average 'Yes' Votes", colour = "EP Group") +
  theme_minimal()+
  theme(
    panel.border = element_rect(colour = "grey60", fill = NA, size = 0.5),
    axis.title = element_text(family = "serif", size = 14), #title font
    axis.text = element_text(colour = "black", size = 10), #axis text change
    axis.ticks.x = element_line(colour = "black", size = 0.75),
    strip.text = element_text(family = 'serif', size = 11, colour = 'black'),
    legend.title = element_text(family = 'serif', size = 13, colour = 'black'), #legend
    legend.text =  element_text(family = 'serif', size = 12, colour = 'black'),# font change
    legend.position = "right",
    legend.margin = margin(10,10,10,10),
    legend.background = element_rect(
      fill = "white",
      colour = "grey50", #make box aorund legend to align it
      size = 0.5,
      linetype = "solid")
  )
dev.off()



