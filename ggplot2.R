library(tidyverse)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(stroke = 3) +
  geom_smooth(se = FALSE, size = 2)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(stroke = 3) +
  geom_smooth(mapping = aes(group = drv), se = FALSE, size = 2)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_point(stroke = 3) +
  geom_smooth(mapping = aes(group = drv), se = FALSE, size = 2)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv), stroke = 3) +
  geom_smooth(mapping = aes(linetype = drv), se = FALSE, size = 2)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(fill = drv), stroke = 3, shape = 21, color = "white", size = 5)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 10, color = "white") +
  geom_point(aes(colour = drv), stroke = 3)

?geom_point
