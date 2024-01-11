library(ggplot2)
library(dplyr)
library(raster)
library(sf)
library(lubridate)
library(mapview)



RJ = sf::st_read("C:/Users/pedro/Documents/PROslide_RIO/DATA/StudyArea.shp")

rioslides = sf::st_read("C:/Users/pedro/Documents/PROslide_RIO/DATA/landslides_2023.shp")
summary(as.factor(rioslides$tipologia1))
#0    1    2    3    4    5    6    7    8    9   10   11 
#59 1659   32   25    1  323   55  449   12   71  155  157 

rioslides <- rioslides %>%
  dplyr::filter(tipologia1 == 1 | tipologia2 == 1 | tipologia3 == 1 | tipologia4 == 1)

summary(rioslides$data)
summary(as.factor(rioslides$anolaudo))
#2009 2010 2011 2012 2013 2014 2015 2016 2017 
#2  909  162   80  196  111   78  123    3 
# Create a new column to determine if the date is NA or not
rioslides$has_date <- ifelse(is.na(rioslides$data), "No Date", "Has Date")
summary(as.factor(rioslides$has_date))
#Has Date  No Date 
#701      963


mapview::mapview(rioslides, zcol = "has_date")+ mapview(RJ)


# Plot
ggplot() +
  geom_sf(data = RJ, fill = NA) +
  geom_sf(data = rioslides, aes(color = has_date), size= .1) +
  theme_minimal() +
  labs(title = "Landslides in RJ", 
       subtitle = "Points colored based on presence of date") +
  theme(
    axis.text = element_blank(),  # Remove axis text
    axis.title = element_blank(), # Remove axis title
    axis.ticks = element_blank(), # Remove axis ticks
    legend.position = "bottom"   # Place legend at the bottom
  )


rioslides <- rioslides[!is.na(rioslides$data), ]

summary(as.factor(rioslides$anolaudo))
#2009 2010 2011 2012 2013 2014 2015 2016 
#2  553   39   15   44   33   11    4 
# Extract month from the date column
rioslides$month <- factor(lubridate::month(rioslides$data, label = TRUE),
                          levels = month.abb)  # Order by the real months of the year
rioslides$year <- factor(lubridate::year(rioslides$data)) 

# Plot
ggplot(data = rioslides, aes(x = year)) +
  geom_bar(aes(y = after_stat(count))) +  # Updated notation
  geom_text(aes(y = after_stat(count), label = after_stat(count)), stat = "count", vjust = -0.5) +  # Updated notation
  theme_minimal() +
  labs(title = "Number of Landslides by year",
       caption = "Graph based on the 1101 landslides which have date associated. The 1897 landslides without date field were excluded",
       x = "Month",
       y = "Number of Landslides")


# Plot
ggplot(data = rioslides, aes(x = month)) +
  geom_bar(aes(y = after_stat(count))) +  # Updated notation
  geom_text(aes(y = after_stat(count), label = after_stat(count)), stat = "count", vjust = -0.5) +  # Updated notation
  theme_minimal() +
  labs(title = "Number of Landslides by Month",
       caption = "Graph based on the 701 landslides which have date associated. Landslides without date field are not represented.",
       x = "Month",
       y = "Number of Landslides")

gg=rioslides %>%
  mutate(year = as.numeric(as.character(year))) %>% # Convert 'year' from factor to numeric
  filter(year <= 2017) %>%
  mutate(year = factor(year, levels = sort(unique(year)))) %>% # Convert 'year' to ordered factor
  ggplot( aes(x = month, fill = year)) +
  geom_bar(aes(y = after_stat(count))) +
  geom_text(
    aes(y = after_stat(count), label = ifelse(after_stat(count) > 30, after_stat(count), "")),
    stat = "count", size=5
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2", name = "Year") + # or any other palette of your choice
  labs(title = "Number of Landslides by Month",
       #subtitle = "Colored by Year, Labels for Counts Above 30",
       caption = "Graph based on the 701 landslides which have date associated. Landslides without date field are not represented.",
       x = "Month",
       y = "Number of Landslides")+
  theme(legend.position = "bottom",
    text = element_text(size = 20), # Increase base size for all text elements
    plot.title = element_text(size = 20), # Optionally set a larger size for title
    plot.caption = element_text(size = 10), # Optionally set a different size for caption
    axis.title = element_text(size = 20), # Optionally set a larger size for axis titles
    legend.title = element_text(size = 20) # Optionally set a larger size for legend title
  ) +
  coord_flip() # Flip the axes


png("E:/PROslide_RIO/PRESENTATIONS/plot_year_month.png", width = 750, height = 1150)
print(gg)
dev.off()

