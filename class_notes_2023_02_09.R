#EBores
#2023-02-09


library("tidyverse")
library("palmerpenguins")
head(iris)

tidyverse_packages()
head(penguins)
summary(penguins)
glimpse(penguins)
class(penguins)

mean(penguins$bill_depth_mm, na.rm=TRUE) #na.rm=TRUE remove NAs


#filter by species

head(penguins)
gentoo= filter(penguins, species=="Gentoo")
head(gentoo)
summary(gentoo)

gentoo_ladies=filter(penguins, species=="Gentoo", sex=="female")
summary(gentoo_ladies)

gentoo_ladies=penguins%>%
  filter(species=="Gentoo")%>%
  filter(sex=="female")
summary(gentoo_ladies)


mean_ladies_mass=penguins %>%
  filter(sex=="female") %>%
  summarize(mean_mass_g= mean(body_mass_g))

mean_ladies_mass= mean(penguins$body_mass_g[penguins$sex=="female"], na.rm=TRUE)
mean_ladies_mass

species_sex_mass=penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species, sex) %>%
  summarize(mean_mass_g=mean(body_mass_g))

write_csv(x=species_sex_mass, file="data/species_sex_mass.csv")

species_count=penguins %>%
  #filter(!is.na(sex)) %>%
  group_by(species) %>%
  summarize(count=n())

penguins_for_america=penguins %>%
  mutate(body_mass_lb= body_mass_g* 0.0022)

head(penguins_for_america)

penguins%>%
  distinct(island)

penguins %>%
  select(-bill_length_mm, -bill_depth_mm) #everything but exclude these 2 columns (use a -)

penguins %>%
  arrange(desc(body_mass_g)) #desc= arrange in descending in order vs ascending

#exercise 1.2 mean bill length in inches of Adelie penguins on Dream Island or Biscoe island. What is std dev?
# mean larger or small than the mean bill length of adelie penguins found on Torgersen
#0.039 inches per mm



penguins%>%
  filter(species=="Adelie",
         island %in% c("Biscoe", "Dream"),
         !is.na(bill_length_mm)) %>%
 mutate(bill_length_inches= bill_length_mm*0.039) %>% #0.039 inches/mm
  summarize(mean_bill_length_inches=mean(bill_length_inches),
            sd_bill_length_inches=sd(bill_length_inches))

penguins%>%
  filter(species=="Adelie",
         island %in% c("Torgersen"),
         !is.na(bill_length_mm)) %>%
  mutate(bill_length_inches= bill_length_mm*0.039) %>% #0.039 inches/mm
  summarize(mean_bill_length_inches=mean(bill_length_inches),
            sd_bill_length_inches=sd(bill_length_inches))

#mean smaller than Torgersen penguins











