### Juleplot
set.seed("123")
sne <- data.frame("x" = 1:500,
                  "y" =  rnorm(500, sd = 1.3),
                  "z" = sample(1:10, size = 500, replace = TRUE))
juletest <- ggplot(sne, aes(x = x, y = y)) +
  geom_point(show.legend = FALSE, color ="lightsteelblue1", shape = 8, size = 3)

juletest + transition_time(z)


# Set scene

plot(1:500, rnorm(500, sd = 1.3), col = "lightsteelblue1", pch = 8, cex = 3, xaxt = "n", xlab = "", yaxt = "n", ylab = "")
points(1:1000, rnorm(1000, mean = -4, sd = 0.15), pch = 15, cex = 3, col = "lightsteelblue1")

for(i in 1:10){
  points(x = 20, y = 3.4, pch = 24, cex = i, col = "goldenrod1")
  points(x = 20, y = 3.4, pch = 25, cex = i, col = "goldenrod1")
}

# lav hus
points(x = 90, y = -3, pch = 15, cex = 25, col = "tan3")
points(x = 45, y = -1.2, pch = 15, cex = 3, col = "chocolate4")
points(x = 45, y = -1, pch = 15, cex = 3, col = "chocolate4")
points(x = 45, y = -0.8, pch = 15, cex = 3, col = "chocolate4")
points(x = 90, y = -0.6, pch = 17, cex = 20, col = "brown")
points(x = 80, y = -3.5, pch = 15, cex = 5, col = "burlywood4")
points(x = 80, y = -3, pch = 15, cex = 5, col = "burlywood4")
points(x = 84, y = -3.3, pch = 16)
points(x = 110, y = -2.5, pch = 15, cex = 7, col = "white")
points(x = 110, y = -2.5, pch = 12, cex = 7)

# Lav røg til skorsten
points(x = 45, y = -0.3, pch = 16, cex = 1.5, col = "gray")
points(x = 45, y = -0.2, pch = 16, cex = 1.5, col = "gray")
points(x = 45, y = -0.1, pch = 16, cex = 1.5, col = "gray")
points(x = 44, y = 0, pch = 16, cex = 1.5, col = "gray")
points(x = 43, y = 0.1, pch = 16, cex = 1.5, col = "gray")
points(x = 45, y = 0.2, pch = 16, cex = 1.5, col = "gray")


# Lav juletræ
segments(x0 = 400, y0 = -3.6, x1 = 400, y1 = -2.32, lwd = 30, col = "burlywood4")
points(x = 400, y = 0.5, pch = 17, cex = 20, col = "forestgreen")
points(x = 400, y = -0.5, pch = 17, cex = 23, col = "forestgreen")
points(x = 400, y = -1.5, pch = 17, cex = 26, col = "forestgreen")
points(x = 400, y = 2.5, pch = 25, cex = 5, col = "goldenrod1", bg = "goldenrod1")
points(x = 400, y = 2.5, pch = 24, cex = 5, col = "goldenrod1", bg = "goldenrod1")

from_a <- -2.6
to_a <- -1.5
from_b <- 335
to_b <- 455
a <- seq(from = from_a, to = to_a, by = (to_a - from_a)/20)
b <- seq(from = from_b, to = to_b, by = (to_b - from_b)/20)
for(i in 1:length(a)){
  points(x = b[[i]], y = a[[i]], pch = 20, cex = 2, col= "darkred", bg = "darkred")
}

from_a <- -1.2
to_a <- -0.2
from_b <- 348
to_b <- 448
a <- seq(from = from_a, to = to_a, by = (to_a - from_a)/15)
b <- seq(from = from_b, to = to_b, by = (to_b - from_b)/15)
for(i in 1:length(a)){
  points(x = b[[i]], y = a[[i]], pch = 20, cex = 2, col= "darkred", bg = "darkred")
}

from_a <- 0.2
to_a <- 0.8
from_b <- 360
to_b <- 425
a <- seq(from = from_a, to = to_a, by = (to_a - from_a)/10)
b <- seq(from = from_b, to = to_b, by = (to_b - from_b)/10)
for(i in 1:length(a)){
  points(x = b[[i]], y = a[[i]], pch = 20, cex = 2, col= "darkred", bg = "darkred")
}


points(x = 370, y = -1.8, pch = 16, cex = 4, col = "red1")
points(x = 430, y = -1.2, pch = 16, cex = 4, col = "red1")
points(x = 400, y = 0, pch = 16, cex = 4, col = "red1")


# Lav snemand
points(x = 250, y = -3, pch = 21, cex = 12, col = "gray", bg="white")
points(x = 250, y = -1.8, pch = 21, cex = 8, col = "gray", bg = "white")
points(x = 250, y = -0.95, pch = 21, cex = 6, col = "gray", bg = "white")
segments(x0 = 240, y0 = -0.65, x1 = 260, y1 = -0.65, lwd = 7, col = "black")
points(x = 250, y = -0.45, pch = 15, cex = 4, col = "black")
segments(x0 = 260, y0 = -0.95, x1 = 267, y1 = -0.95, lwd = 7, col = "orange")
points(x = 250, y = -0.92, pch = 16, cex = 1, col = "black")

segments(x0 = 233, y0 = -1.8, x1 = 213, y1 = -0.90, lwd = 5, col = "black")
segments(x0 = 265, y0 = -1.8, x1 = 280, y1 = -1, lwd = 5, col = "black")



library(ggplot2)
library(gganimate)
theme_set(theme_bw())
library(gapminder)
head(gapminder)

p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p

p + transition_time(year) +
  labs(title = "Year: {frame_time}")

