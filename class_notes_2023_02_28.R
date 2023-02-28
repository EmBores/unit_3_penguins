##EMB
##2023-02-28


library(palmerpenguins)
library(tidyverse)
library(ggiraph)
library(ggiraphExtra)

head(penguins)


penguins_lm_3= penguins %>%
  filter(!is.na(bill_depth_mm),
         !is.na(bill_length_mm),
         !is.na(species))

head(penguins_lm_3)

#build model

lm_3=lm(bill_depth_mm ~ bill_length_mm + species, data=penguins_lm_3)
summary(lm_3)
coef(lm_3)[1]
anova(lm_3)
my_results=broom::tidy(lm_3, conf.int=TRUE, conf.level=0.95)%>%
  mutate_if(is.numeric, round, 2)
my_results

#visualize model
ggPredict(lm_3, se=TRUE, interactive=TRUE)

lm_3_predictions=predict(lm_3, interval="confidence",level=0.95)
head(lm_3_predictions)
head(penguins_lm_3)

penguins_lm_3_predict=cbind(penguins_lm_3, lm_3_predictions)
head(penguins_lm_3_predict)

ggplot(data=penguins_lm_3_predict, aes(x=bill_length_mm, y=bill_depth_mm, color=species))+
  geom_ribbon(aes(ymin=lwr,ymax=upr, fill=species, color=NULL), alpha=0.5)+
  geom_point()+
  geom_line(aes(y=fit))
  
# generate new data
newdata_bill_length_mm=seq(min(penguins_lm_3$bill_length_mm), 
                           max(penguins_lm_3$bill_length_mm),
                           by=0.1)
newdata= expand.grid(bill_length_mm=newdata_bill_length_mm, 
                     species=unique(penguins_lm_3$species))

newdata_predict_lm_3= cbind(newdata, predict(lm_3, newdata=newdata, interval="confidence"))
head(newdata_predict_lm_3)

ggplot()+
  geom_point(data=penguins_lm_3, aes(x=bill_length_mm, y=bill_depth_mm, color=species))+
  geom_ribbon(data=newdata_predict_lm_3, aes(ymin=lwr, ymax=upr, x=bill_length_mm, fill=species),
              alpha=0.5)+
  geom_line(data=newdata_predict_lm_3, aes(y=fit, x=bill_length_mm, color=species))


#tidyverse way of generating predictions

lm_3_predict=lm_3 %>%
  broom::augment(data=penguins_lm_3, se_fit=TRUE, interval="confidence")
glimpse(lm_3_predict)

ggplot()+
  geom_point(data=penguins_lm_3, aes(x=bill_length_mm, y=bill_depth_mm, color=species))+
  geom_ribbon(data=lm_3_predict, aes(ymin=.lower, ymax=.upper, x=bill_length_mm, fill=species),
              alpha=0.5)+
  geom_line(data=lm_3_predict, aes(y=.fitted, x=bill_length_mm, color=species))


