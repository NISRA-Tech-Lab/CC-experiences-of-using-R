################################################################################

# Install datapasta
install.packages("datapasta")

# Click on Addins to see if 'Paste as data.frame' option is available

################################################################################
# Copying and pasting data

#install.packages("clipr")
#install.packages("tidyverse")
library(tidyverse)
library(clipr)

# Link - https://www.nisra.gov.uk/publications/index-services-ios-statistical-bulletin-and-tables-quarter-4-2023
# Navigate to Table_1_1
# Add 'Instance' to A3
# Copy A3:C79
# Paste as dataframe onto "IOS" line below
# Save as 'IOS' object

IOS <- 

new <- IOS %>%
  mutate(Sum = NI.Services + UK.Services)

write_clip(new)

# You can also remove dataframes from the Global Environment when you are finished
# with them to remove 'clutter'

rm(IOS)

################################################################################
# Previous Instances

Instance <- "Q4 2023"
Quarter <- as.numeric(substr(Instance, 2, 2))
Year <- as.numeric(substr(Instance, 4,8))

Previous_Year <- Year - 1

Previous_Quarter <- ifelse(Quarter > 1,
                           Quarter -1,
                           4)

Previous_Quarter_Year <- ifelse(Quarter > 1,
                                Year,
                                Year -1)

Previous_Instance <- paste("Q", Previous_Quarter," ", Previous_Quarter_Year, sep = "")

Four_Instances_Ago <- paste("Q", Quarter," ", Year - 1, sep = "")

# We could remove all the components if we wanted
rm(Previous_Quarter, Previous_Quarter_Year, Previous_Year, Quarter, Year)

################################################################################
# Below is a dummy dataframe that I have created which resembles data we would
# work with in ELMS

################################################################################
## Set working directory to the location of this script
this.dir <-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)

data <-read.csv("Dummy.csv")

#install.packages("tidyverse")
library(tidyverse)

str(data) # this line of code shows you how the data is structured

# Basic functionality in the tidyverse

# Filter
filter <- data %>%
  filter(Instance == 2019) %>%
  filter(FormType == "B")
filter

## Different filter options
# != not equal to
# < less than
# <= less than or equal to
# > greater than
# >= greater than or equal to
# == equal to
# %in% filter multiples

filter2 <- data %>%
  filter(FormType == "B" | FormType == "C")
filter2

forms <-c("B", "C")

filter3 <- data %>%
  filter(FormType %in% forms)
filter3

# Select
select <- data %>%
  select(RURef)
select

drop <- data %>%
  select(-FormType, -SIC)
drop

range <- data %>%
  select(Instance:FormType)
range

range2 <- data %>%
  select(Instance:FormType, SIC)
range2

# Mutate
mutate <- data %>%
  mutate(Grossed_TO = TRNOVR_GF * TOT_TURNOVER)
mutate

mutate2 <- data %>%
  mutate(divided_TO = TRNOVR_GF / 100, # new column
         Instance = paste("Year", Instance, sep =" ")) # mutate existing column
mutate2

# Unite
unite1 <- data %>%
  unite(`SIC Strata`, SIC, `T.O_STRATA`, sep = " ", remove = FALSE)
unite1

unite2 <- data %>%
  unite(`SIC Strata`, SIC, `T.O_STRATA`, sep = " ", remove = TRUE)
unite2

# Joining it all together
all_together <- data %>%
  filter(Instance == 2019 | Instance == 2018,
         FormType == "B") %>%
  unite(`SIC Strata`, SIC, `T.O_STRATA`, sep = " ", remove = TRUE) %>%
  mutate(Grossed_TO = TRNOVR_GF * TOT_TURNOVER) %>%
  select(`SIC Strata`, Instance, Grossed_TO)
all_together

# Group by and summarise
sum <- data %>%
  group_by(Instance) %>%
  summarise("Annual Turnover" = sum(TOT_TURNOVER))
sum

# Group by and summarise
mean <- data %>%
  group_by(Instance) %>%
  summarise("Average Turnover" = mean(TOT_TURNOVER))
mean

# Write csv
write.csv(mean, "mean.csv")

# Pivot wider
wide <- sum %>%
  pivot_wider(names_from = Instance,
              values_from = "Annual Turnover")
wide

# Join
FormTypes <- data.frame(
  stringsAsFactors = FALSE,
  FormType = c("A", "B", "C", "D"),
  FormTypeDescription = c("Income","Expenditure","Savings",
                          "Investment"))
FormTypes

str(FormTypes) # Check structure

join <- data %>%
  left_join(FormTypes, data, by = "FormType")
join

# Row bind
new_row <- c("2024", "499654259", "Receipted", "D", "231", "5432", "2", "23", "100+")
new_row

new_data <- rbind(data, new_row)
new_data

# Conditional Column
conditional_column <- data %>%
  mutate(Survey = ifelse(SIC < 50,
                         "Production",
                         "Services"))
conditional_column

multiple_conditions <- data %>%
  mutate(FormTypeDescription = ifelse(FormType == "A",
                                      "Income",
                                      ifelse(FormStatus == "B",
                                             "Expenditure",
                                             ifelse(FormTypes == "C",
                                                    "Savings", 
                                                    "Investment"))))
multiple_conditions

# Create static bar chart
ggplot(sum, aes(x = Instance, 
                y = `Annual Turnover`)) + 
  geom_bar(stat = "identity") 

ggplot(sum, aes(x = Instance, 
                y = `Annual Turnover`)) + 
  geom_bar(stat = "identity",
           fill = "darkolivegreen3") +
  coord_flip()

# Full list of colours
colors()

# Try keying various colours below
"paleturquoise"
"orangered1"   
"goldenrod4" 
"#00205B"
"#3878c5"
"#CEDC20"

# Create dynamic bar chart
library(plotly)

sum %>%
  plot_ly(
    x = ~Instance,
    y = ~`Annual Turnover`,
    type = "bar")

sum %>%
  plot_ly(
    x = ~Instance,
    y = ~`Annual Turnover`,
    type = "bar",
    color = I("red")) 

sum %>%
  plot_ly(
    x = ~Instance,
    y = ~`Annual Turnover`,
    type = "bar",
    color = I("green")) %>%
  layout(title = "Turnover", 
         xaxis = list(title = "Survey Instance"),
         yaxis = list(title = "Annual Turnover (£)"))

sum %>%
  plot_ly(
    x = ~Instance,
    y = ~`Annual Turnover`,
    type = "bar",
    color = I("#00205B")) %>%
  layout(title = "Turnover", plot_bgcolor = "#CEDC20")

# For information on more bar charts see - https://plotly.com/r/bar-charts/