##EMB
##2023-02-23


library(palmerpenguins)
library(tidyverse)
library(GGally)
library(broom)

head(penguins)

penguins%>%
  select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  GGally::ggpairs(aes(color=species))

penguins%>%
  select(bill_depth_mm, bill_length_mm)%>%
  ggpairs()

#linear model

lm_1= lm(bill_depth_mm ~ bill_length_mm, data=penguins)
class(lm_1)
summary(lm_1)

#r2= 0.05= means only predicting about %5 of variation in bill depth (not good)

ggplot(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm))+
  geom_point()+
  geom_smooth(method="lm")

plot(lm_1)

gentoo=penguins%>%
  filter(species=="Gentoo")
gentoo%>%
  select(bill_length_mm, bill_depth_mm)%>%
  ggpairs()

lm_2= lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
summary(lm_2)
plot(lm_2)

ggplot(data=gentoo, aes(x=bill_length_mm, y=bill_depth_mm), color="black")+
  geom_point()+
  geom_smooth(method="lm")+ theme_bw()
#+geom_point(data=penguins %>%filter(species=="Adelie"), aes(x=bill_length_mm, y=bill_depth_mm), color="red")

#plot each species lm separately
ggplot(data=penguins)+
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species))+
  geom_smooth(aes(x=bill_length_mm, y=bill_depth_mm, color=species), method="lm")+
  geom_smooth(aes(x=bill_length_mm, y=bill_depth_mm), color="black", method="lm")+
  theme_bw()
#simpsons paradox

#exercise 5.1
#gentoo bill depth as function of flipper length


gentoo=penguins%>%
  filter(species=="Gentoo")
gentoo%>%
  select(bill_depth_mm, flipper_length_mm)%>%
  ggpairs()

lm_3= lm(bill_depth_mm ~ flipper_length_mm, data=gentoo)
summary(lm_3)
plot(lm_3)

ggplot(data=gentoo, aes( x=bill_depth_mm, y=flipper_length_mm), color="black")+
  geom_point()+
  geom_smooth(method="lm")+ theme_bw()

#Flipper length as the R2 value is greater than bill length (0.4951 vs 0.405)





