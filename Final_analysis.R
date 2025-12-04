
library(tidyverse)
library(easystats)
library(gganimate)
library(readxl)

smt1 = read_xlsx('resource/SMTdata.xlsx', sheet = 1)

smt2 = read_xlsx('resource/SMTdata.xlsx', sheet = 2)

smt3 = read_xlsx('resource/SMTdata.xlsx', sheet = 3)

smt4 = read_xlsx('resource/SMTdata.xlsx', sheet = 4)

smt5 = read_xlsx('resource/SMTdata.xlsx', sheet = 5)

view(smt1__)

#clean 

vars = c("CC", "OA", "Mineral", "Lipids", "SP", "TNC", "Lignin",
                  "TSC", "NO3", "Protein", "N", "P", "SLA", "C", "N/P", "C/N")

smt1_ = smt1 %>%
  mutate(across(all_of(vars), ~ as.numeric(.x)))

smt1__ = smt1_[-1, ]

#longer

smt1_long = smt1__ %>%
  pivot_longer(
    cols = c(CC, OA, Mineral, Lipids, SP, TNC, Lignin, TSC, NO3, Protein,
             N, P, SLA, C, `N/P`, `C/N`),
    names_to = "Variable",
    values_to = "Value"
  )

view(smt1_long)

#base mean

smt1_long_mean = smt1_long %>%
  group_by(Species, Rate, Variable) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")

#base 

smt1_long_mean %>% 
  filter(Variable %in% c('Lignin','Lipids','OA','Protein','SLA','SP','TNC','TSC')) %>% 
  ggplot(aes(x = Rate,
             y = Value, 
             group = Species, 
             color = Species)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Variable) +
  scale_x_continuous(breaks = c(0, 25, 50)) +
  labs(x = 'Kg of Nitrogen')

#base 2

smt1_long_mean %>% 
  filter(Variable %in% c('C/N','Mineral','N','N/P')) %>% 
  ggplot(aes(x = Rate,
             y = Value, 
             group = Species, 
             color = Species)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Variable) +
  scale_x_continuous(breaks = c(0, 25, 50)) +
  labs(x = 'Kg of Nitrogen')

#base 3

smt1_long_mean %>% 
  filter(Variable %in% c('C','CC','NO3','P')) %>% 
  ggplot(aes(x = Rate,
             y = Value, 
             group = Species, 
             color = Species)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Variable) +
  scale_x_continuous(breaks = c(0, 25, 50)) +
  labs(x = 'Kg of Nitrogen')

#TSC1 mean

smt1__mean = smt1__ %>%
  group_by(Species, Rate, Approach) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

#TSC1

smt1__mean %>% 
  ggplot(aes(x = Rate,
             y = TSC,
             color = Species)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Approach) +
  scale_x_continuous(breaks = c(0, 25, 50))


#TSC2

smt1__ %>% 
  filter(Approach == 'UAN') %>% 
  group_by(block) %>% 
  ggplot(aes(x = Rate,
             y = TSC,
             color = Species,
             group = block)) +
  geom_point() +
  geom_path() +
  facet_wrap(~ Species) +
  scale_x_continuous(breaks = c(0, 25, 50))


?theme


names(smt3$Species)
str(smt3$Species)






