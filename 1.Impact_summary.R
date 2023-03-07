# The "tigris" library is read from the dashboard script

source("countyTest.R")

#read ca dataset
cali <- counties("California", cb = TRUE)

# change the name of the 6th column to "County" for easy merging.
colnames(cali)[6] <- "County"

# Read in CMF data and filter for awardee data
awardees <- read.csv("awardees.csv", stringsAsFactors = FALSE)

# Group by county and tiers to be grouped at the end
County_summary <- awardees %>% 
  filter(Remove != "Yes") %>% 
  group_by(County, Tier.Level) %>% 
  summarize(n = n(),
            awardAmount = sum(Amount.Awarded),
            Total.Awarded.Amount = scales::dollar(sum(Amount.Awarded)))

#group by county and discipline
county_discipline <- awardees %>% 
  filter(Remove != "Yes") %>% 
  group_by(County, Discipline, Tier.Level) %>% 
  summarize(n = n(),
            funds = sum(Amount.Awarded))

#create a vector of unique counties
counties <- unique(awardees$County)

# get the for loop ready
county_text <- NULL

for (i in counties) {
  
  d2 <- filter(county_discipline, County == i)
  county_text[i] <- countyText(d2)
}

#create new data frame
df <- data.frame(County = counties, mytext = county_text)

# merge calimap with new dataset
awardee_map <-  left_join(cali, df, by = "County")

# merge the county summary data with awardee map data
awardee_map <- left_join(awardee_map, County_summary, by = "County")

# for counties that do not have awardees, provide some text
awardee_map$mytext[is.na(awardee_map$mytext)] <- "County has no awardees"

rm(county_discipline, County_summary, d2, df)
rm(list = c("counties", "county_text", "i", "countyText"))

