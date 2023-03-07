
source("grad_text_function.R")

#read in data on grads. this data is from CMF application. Update this to accommodate graduate data
grads <- read.csv("Grad_data.csv", stringsAsFactors = FALSE)

#subset for grads that practice more that 50%
Grad_cali <- filter(grads, State == "California" & County != "Out of State" & Speciality == "Yes")
rm(grads)

#Group by county to get overall county data
grads_county <- Grad_cali %>% 
  group_by(County) %>% 
  summarize(n = n())

#group grads by county and discipline to get discipline level data
grads_grouped <- Grad_cali %>%
  group_by(County, Specialty) %>% 
  summarize(n = n())

#get totals by discipline
fm_grads <- sum(grads_grouped$n[grads_grouped$Specialty == "Family Medicine"])
im_grads <- sum(grads_grouped$n[grads_grouped$Specialty == "Internal Medicine"])
em_grads <- sum(grads_grouped$n[grads_grouped$Specialty == "Emergency Medicine"])
ob_grads <- sum(grads_grouped$n[grads_grouped$Specialty == "Obstetrics and Gynecology"])
peds_grads <- sum(grads_grouped$n[grads_grouped$Specialty == "Pediatrics"])
cb_grads <- sum(grads_grouped$n[grads_grouped$Specialty == "Combined Program"])

rm(Grad_cali)

# create table for grad text
counties <- unique(grads_county$County)

gradtexts <- NULL


for (i in counties) {
  
  d2 <- filter(grads_grouped, County == i)
  
  gradtexts[i] <- grad_text(d2)
  
}

df <- data.frame(County = counties, mytext = gradtexts)

#remove any unnecessary data
rm(list = c("d2", "i", "gradtexts", "counties", "grads_grouped"))

# read in cali map
cali <- counties("California", cb = TRUE)

colnames(cali)[6] <- "County"

# merge cali map with total_set

grads_cali <- left_join(cali, df, by = "County")

grads_cali <- left_join(grads_cali, grads_county, by = "County")

rm(list = c("df", "cali", "grads_county", "grad_text"))

#join the county map with grad discipline data
grads_cali$mytext[is.na(grads_cali$mytext)] <- "County has no graduates here"
#grads_cali$n[is.na(grads_cali$n)] <- 0
