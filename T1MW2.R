# Tidyverse - wranglings data in a tidy format
# Indentifying problems to important R learning
# First step is understand each variable and observation actuallly mean
# Second step is look at the untidy data with two problems often occuring - One variable across mulitple columns and one observation is scattered across multiple rows
# Using Pivot to fix the data pivot_longer() lengthens the data while pivot_wider() widens it
#Grouping function facet_wrap()
#%>% acts as a reader from right to left filter the data and summarizes

# Make sure packages are loaded
library(tidyverse)
library(palmerpenguins)

# The messy metadata provided by your colleague
island_metadata <- tibble(
  island_name = c(" biscoe", "Dream ", "Torgersen"),
  station_install = c("15/01/2003", "22-03-2004", "05/11/2001"),
  latitude = c(-64.81, -64.73, -64.76)
)

print(island_metadata)


library(stringr)
library(dplyr)
# Exercise 1
clean_metadata <- island_metadata %>% 
  mutate(island_name = str_to_title(island_name),
         station_install = ymd("2003/01/15", "2004/03/22", "2001/11/05"))

#Exercise 2 


penguins_spatial<- left_join(clean_metadata, penguins, join_by,(island == island_name)
                             penguins_spatial <- left_join(clean_metadata, penguins, by = c("island_name" = "island")



#Exercise 3            
# separate lines need to help debug code
Max_body_mass <- penguins %>%
  drop_na(body_mass_g)  %>%
  group_by(species,island) %>%
  summarise(
    max_body_mass = max(body_mass_g),
  )%>%
    pivot_wider(
      names_from = island, 
      values_from = max_body_mass
      )

print(Max_body_mass)
# A tibble: 3 × 4
# Groups:   species [3]
#species   Torgersen Biscoe Dream
#<fct>         <int>  <int> <int>
  #1 Adelie         4700   4775  4650
#2 Gentoo           NA   6300    NA
#3 Chinstrap        NA     NA  4800
## The NA is due to the penguin species not occuring on those islands

#Helps look for packages
find("%>%")


# Keystone exercise 

list.files("data")
#Loading in the data
EU_metdata <- read_csv("data/estuary_metadata.csv")
En_sonde <- read_csv("data/estuary_sonde_data.csv")
Species_dict <- read_csv("data/species_dictionary.csv")
E_catch <- read_csv("data/estuary_catch_log.xlsx")
library(readxl)

df <- read_excel("data/estuary_catch_log.xlsx")

E_metdata <- tibble(
  site_name = c("Upper_Ross, Mid_Ross, Low_Ross, Ross_Mouth")
)
print(E_metdata)

read_csv("data/estuary_metadata.csv")
library(dplyr)
library(stringr)
#Changing names to the appropiate style
C_Edata <- Clean_EUdata %>%
  mutate(
    site_name = c("Upper Ross", "Mid Ross", "Lower Ross", "Ross Mouth")
      )
  
unique(C_Edata$site_name)


library(readxl)

excel_sheets("data/estuary_catch_log.xlsx")
lower <- read_excel("data/estuary_catch_log.xlsx", sheet = "Lower_ross")
upper <- read_excel("data/estuary_catch_log.xlsx", sheet = "Upper Ross")
mid <- read_excel("data/estuary_catch_log.xlsx", sheet = 2)
mouth <- read_excel("data/estuary_catch_log.xlsx", sheet = 3)

library(purrr)
library(dplyr)

library(readxl)
library(purrr)
library(dplyr)

# Import all sheets
fish <- map_dfr(
  excel_sheets("data/estuary_catch_log.xlsx"),
  ~ read_excel("data/estuary_catch_log.xlsx", sheet = .x)
)
fish <- bind_rows (lower, mid, mouth,upper)
fish <- fish %>%
  mutate(
    site_name = c("Upper Ross", "Mid Ross", "Lower Ross", "Ross Mouth")
  )

library(dplyr)


fish$species <- trimws(tolower(fish$species))
                             
fish$species <- recode(
              fish$species,
                  "barramundi" = "Lates calcarifer",
                  "mangrove jack" = "Lutjanus argentimaculatus",
                  "bream" = "Acanthopagrus australis",
                  "flathead" = "Platycephalus fuscus"
                             )

fish <- fish |> select(-scientific_name)

fish <- fish |> select(-barramundi)
fish <- fish |> select(-bream)
fish <- fish |> select(-lon.y)
fish <- fish |> select(-`mangrove jack`)


library(dplyr)
#Combine two datasets together 
fish <- fish %>%
  left_join(metadata,
            by = c("site" = "site_name"))


  # Changing the name so that standised between datasets
  Son <- E_sonde %>%
    mutate(site = case_when(
      site %in% c("mid_ross", "Mid_ross", "MID_ROSS", "Mid_Ross") ~ "Mid Ross",
      site %in% c("lower_ross", "Lower_ross", "LOWER ROSS")      ~ "Lower Ross",
      site %in% c("upper_ross", "Upper_ross", "Upper_Ross")      ~ "Upper Ross",
      site %in% c("ross_mouth", "Ross_mouth", "ROSS MOUTH")      ~ "Ross Mouth",
      TRUE ~ site # leaves any other site name unchanged
    ))
#Gives a list of the different sites
unique(E_sonde$site)



fish <- fish %>%
  mutate(site = case_match(
    site,
    "Upper Ross" ~ "Upper Ross",
    " Mid_ross"   ~ "Mid Ross",
    "Lower_ross" ~ "Lower Ross",
    "ROSS MOUTH" ~ "Ross Mouth",
    .default = site 
  ))
unique(fish$site)
#Connecting turbid/ salinity data into a master dataset
Master <- fish %>%
  left_join(Son,
            by = c("site"))
#Getting the data set dates in order
library(lubridate)
date_1 <-ymd("2026-05-01")
date_2 <dmy("")


#Sorting out the sensor error codes

library(dplyr)

fixed_dataset <- E_sonde %>%
  mutate(turbidity = na_if(turbidity, -999))

print(fixed_dataset)

EU_sonde <- En_sonde %>%
turbidity = c(6.4, 5.8, 7.1, 6.9, 5.5, 6.2, 7.0, 6.8, 5.9, 6.3)
 
#Inspecting the data 
names(En_sonde)
str(En_sonde)
names(fish)
str(fish)

#Removing the errors
En_sonde$turbidity[En_sonde$turbidity == -999] <- NA

# Standardizing the site names to match the metadata
S<- En_sonde %>%
  mutate(site = case_when(
    site %in% c("mid_ross", "Mid_ross", "MID_ROSS", "Mid_Ross") ~ "Mid Ross",
    site %in% c("lower_ross", "Lower_ross", "LOWER ROSS")      ~ "Lower Ross",
    site %in% c("upper_ross", "Upper_ross", "Upper_Ross")      ~ "Upper Ross",
    site %in% c("ross_mouth", "Ross_mouth", "ROSS MOUTH")      ~ "Ross Mouth",
    TRUE ~ site # leaves any other site name unchanged
  ))

#Checks how many variables
nrow(E_sonde)
dim(En_sonde)

meta <- fish %>%
  left_join(S,
            by = c("site", "date"))
table(fish$site)
table(S$site)

#Creating another column for dates
S$date <- as.Date(S$timestamp ,
                  format = "%d/%m/%Y %H:%M")
names(fish)
names(S)
head(S)

library(dplyr)

S_daily <- S %>%
  group_by(site, date) %>%
  summarise(
    mean_turbidity = mean(turbidity, na.rm = TRUE),
    mean_temp = mean(temperature, na.rm = TRUE),
    mean_salinity = mean(salinity, na.rm = TRUE),
    .groups = "drop"
  )
head(S_daily)

dim(S_daily)


Final <- left_join(fish,S_daily,
                   by = c("site", "date"))


library(dplyr)
library(tidyr)
  #Zero catch framework                           
Final_catch <- Final %>% 
        complete(
        nesting(site, date, lat, lon, zone, mean_turbidity, mean_temp),
                species,
                fill = list(count = 0)
                               )   

#Summary table displaying mean count/ mean salinity depending on zone with sd included 
summary_table <- Final_catch %>%
  group_by(species, zone) %>%
  summarise(
   mean_count = sum(count, na.rm = TRUE),
   sd_count = sd(count, na.rm = TRUE),
   mean_sal = mean(mean_salinity, na.rm = TRUE),
  sd_sal = sd(mean_salinity, na.rm = TRUE),
    .groups = "drop"
  )
print(summary_table)


Final%>%
  ggplot(data=Final, mapping = aes(x=species, y= mean_salinity, fill=zone)) +
  geom_boxplot(width= 0.5) +
  labs(title="Salinity by Species and Zone",
       x="Species",
       y="Mean Salinity") +
  theme_minimal(base_size =14)
  




