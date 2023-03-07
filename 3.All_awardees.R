#Prepare data for all cycles and all awardees

#group by discipline to get overall discipline level data
awardees_by_discipline <- awardees %>% 
  filter(Remove != "Yes") %>% 
  group_by(Discipline) %>% 
  summarize(Total = n())


#group by disicpline and cycle year
residents_disicipline <- awardees %>% 
  filter(Remove != "Yes") %>% 
  group_by(Discipline) %>% 
  summarize(Total.Residents = sum(Total.Positions.Awarded))


#total amount by discipline
amount_discipline <- awardees %>% 
  filter(Remove != "Yes") %>% 
  group_by(Discipline) %>% 
  summarise(funds = sum(Amount.Awarded),
            Amount = scales::dollar(sum(Amount.Awarded)))

# positions by program type
residents_pt <- awardees %>% 
  filter(Remove != "Yes") %>% 
  group_by(Discipline) %>% 
  summarize(New = sum(New),
            Existing = sum(Existing),
            Expanding = sum(Expanding)) %>% 
  pivot_longer(cols = 2:4,
               names_to = "Program.Type", 
               values_to = "Total")

new <- sum(residents_pt$Total[residents_pt$Program.Type ==  "New"])
exp <- sum(residents_pt$Total[residents_pt$Program.Type ==  "Expanding"])
Exist <- sum(residents_pt$Total[residents_pt$Program.Type ==  "Existing"])

all.Discipline <- data.frame(Discipline = c("All Disciplines", "All Disciplines", "All Disciplines"),
                             Program.Type = c("New", "Existing", "Expanding"),
                             Total = c(252, 581, 133))


residents_pt <- rbind(residents_pt, all.Discipline)

rm(new, exp, Exist, all.Discipline)
