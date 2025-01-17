##EMB
##2023-02-21


library(palmerpenguins)
library(tidyverse)
library(rstatix)

head(penguins)

ggplot(data=penguins)+
  geom_histogram(aes(x=body_mass_g, fill=species))

#one-sample t-test
gentoo=penguins %>%
  filter(species=="Gentoo")
head(gentoo)

ggplot(data=gentoo)+
  geom_histogram(aes(x=body_mass_g))

ggplot(data=gentoo)+
  stat_qq(aes(sample=body_mass_g))

#theoretical/perfect normal distribution. Would give you a perfect 1 to 1 line if your data plotted
# has a good distribution

gentoo %>%
  summarize(mean_body_mass_g= mean(body_mass_g, na.rm=TRUE),
            sd_body_mass_g=sd(body_mass_g, na.rm=TRUE))

# run t test:
t.test(gentoo$body_mass_g, mu=5500) #mu= literature derived value, 5500 literature value of mass
# p value above 0.5 means mean hypothesis is true, off by 10%

t_test_results= gentoo %>%
  t_test(body_mass_g ~ 1, mu=5500) #1= 1 sample t-test

#two sample t-test

data_for_t_test= penguins %>%
  filter(species %in% c("Gentoo", "Adelie"),
         !is.na(body_mass_g)) %>%
  select(species, body_mass_g) %>%
  droplevels()


head(data_for_t_test)
summary(data_for_t_test)

data_for_t_test %>%
  group_by(species) %>%
  summarize(mean=mean(body_mass_g),
            sd=sd(body_mass_g))

ggplot(data=data_for_t_test)+
  stat_qq(aes(sample=body_mass_g))+
  facet_wrap(~species, scales="free")

#check equality of variance assumption
data_for_t_test %>%
  levene_test(body_mass_g ~species)

#if p-value is small (0.05) then use a different t-test , if higher then accept 
#the null hypothesis 

t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species)
#p-value is very very small, rejecting null hypothesis that the distributions
#have a similar mean and are accepting the alt, that the distributions have a 
#different mean

#is bill length similar to bill depth (correlations)

ggplot(data=gentoo)+
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm))

ggplot(data=gentoo)+
  stat_qq(aes(sample=bill_length_mm))

ggplot(data=gentoo)+
  stat_qq(aes(sample=bill_depth_mm))

cor(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm, use="complete.obs")
cor.test(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm, use="complete.obs")

gentoo %>%
  cor_test(bill_length_mm, bill_depth_mm)

head(gentoo)

cor(gentoo[ ,c(3:6)], use="complete.obs")

library(GGally)

penguins %>%
  select(species,bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)%>%
  GGally::ggpairs(aes(color=species))




