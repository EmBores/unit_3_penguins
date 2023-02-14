#EMB
#2023-02-14

library(tidyverse)
library(palmerpenguins)
head(penguins)
find("filter")
stats::filter()
dplyr::select()  #calling the package where you want the function filter from


my_plot=ggplot(data=penguins) +
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species,
                 shape=sex))+ geom_smooth(aes(x=flipper_length_mm, y=body_mass_g))+
  xlab("Flipper lenght(mm)")+ ylab("Body mass (g)")+ ggtitle("penguins are cute")+theme_bw()

ggsave(my_plot, filename="figures/flipper_v_mass.png", width=7, height=5, units="in", dpi=300)

head(penguins)

penguin_ts=penguins %>%
  group_by(year, species) %>%
  summarize(num_penguins=n())


ggplot(penguin_ts)+
  geom_line(aes(x=year, y=num_penguins, color=species))

ggplot(data=penguins)+
  geom_histogram(aes(x=flipper_length_mm, fill=species, color=species), position="identity", alpha=0.5)+
  scale_fill_manual(values=c("darkorange", "darkorchid", "cyan4"))

ggplot(data=penguins)+
  geom_boxplot(aes(y=flipper_length_mm, x=species))+
  geom_jitter(aes(y=flipper_length_mm, x=species, color=species), width=0.2)+ theme_bw()

ggplot(data=penguins)+
  geom_bar(aes(x=sex, fill=species)) +
  facet_wrap(~species, nrow=3) + theme_bw()


ggplot(data=penguins)+
  geom_bar(aes(x=island, fill=species))+
  facet_wrap(~species,nrow=3)+
  coord_flip()





