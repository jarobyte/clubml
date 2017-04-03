library(splines)
library(tidyverse)
library(modelr)

x = c(-1000, -701,-600,-500,-400,-299,100, 1000, 3000)                        
y = c(-1000,-901,-800,-500,-200,-99,100, 1000, 3000)

sim <- tibble(x, y)

mod <- lm(y ~ ns(x, 8), data = sim)

grid <- sim %>% 
  data_grid(x = seq_range(x, by = 1)) %>% 
  gather_predictions(mod, .pred = "y")

ggplot(sim, aes(x, y)) + 
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~ model)

write_csv(grid, "splineData.csv")