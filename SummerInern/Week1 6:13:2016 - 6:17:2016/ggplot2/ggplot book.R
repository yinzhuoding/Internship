library(ggplot2)
ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()
ggplot(mpg, aes(displ,hwy, col = class)) + geom_point()
ggplot(mpg, aes(displ,hwy, shape = drv)) + geom_point()

ggplot(mpg, aes(displ,hwy)) + 
  geom_point() +
  facet_wrap(~cyl)

#### geom_smooth() ####
ggplot(mpg, aes(displ,hwy)) + 
  geom_point() +
  geom_smooth()

library(mgcv)
ggplot(mpg, aes(displ,hwy)) + 
  geom_point() +
  geom_smooth(method = "gam", formula = y~s(x))

ggplot(mpg, aes(displ,hwy)) + 
  geom_point() +
  geom_smooth(method = "lm")

library(MASS)
ggplot(mpg, aes(displ,hwy)) + 
  geom_point() +
  geom_smooth(method = "lm")

#### Boxplot and jittered points
ggplot(mpg, aes(drv, hwy)) + geom_point()
ggplot(mpg, aes(drv, hwy)) + geom_jitter()
ggplot(mpg, aes(drv, hwy)) + geom_boxplot()
ggplot(mpg, aes(drv, hwy)) + geom_violin()

#### Histograms and frequency polygons
ggplot(mpg, aes(displ, col = drv)) + 
  geom_freqpoly(binwidth = 0.5)
ggplot(mpg, aes(displ, fill = drv)) + 
  geom_histogram(binwidth = 0.5)
ggplot(mpg, aes(displ, fill = drv)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~drv, ncol = 1)

#### Bar charts
ggplot(mpg, aes(manufacturer)) + geom_bar()

#### Line and path plot
ggplot(economics, aes(unemploy/pop, uempmed)) + 
  geom_path(col = "grey50") +
  geom_point(aes(col = date))


ggplot(mpg, aes(cty,hwy)) + geom_point()

#### Labels
df = data.frame(x = 1, y = 1:3, family = c("sans", "serif", "mono"))
ggplot(df, aes(x,y)) +
  geom_text(aes(label = family, family = family))

df = data.frame(x = 1, y = 1:3, face = c("plain", "bold", "italic"))
ggplot(df, aes(x,y)) +
  geom_text(aes(label = face, fontface = face))

#### Annotations
ggplot(economics, aes(date, unemploy)) + geom_line()
presidential = subset(presidential, start > economics$date[1])
ggplot(economics) +
  geom_rect(aes(xmin = start, xmax = end, fill = party),
            ymin = -Inf, ymax =Inf, alpha = 0.2,
            data = presidential) +
  geom_vline(aes(xintercept = as.numeric(start)),
             data = presidential,
             col = "grey50",
             alpha = 0.5) +
  geom_text(aes(x = start, y = 2500, label = name),
            data = presidential,
            size = 3, vjust = 0, hjust = 0, nudge_x = 50) +
  geom_line(aes(date, unemploy)) +
  scale_fill_manual(values = c("blue", "red"))

#### Collective geoms
library(nlme)
Oxboys = Oxboys
ggplot(Oxboys, aes(age, height, group = Subject)) +
  geom_point() +
  geom_line()
ggplot(Oxboys, aes(age, height)) +
  geom_point() +
  geom_line()
ggplot(Oxboys, aes(age, height)) +
  geom_line(aes(group = Subject)) +
  geom_smooth(method = "lm", size = 2, se  = FALSE)
ggplot(Oxboys, aes(Occasion, height)) + 
  geom_boxplot() +
  geom_line(aes(group = Subject), col = "blue", alpha = 0.5)

#### Surface plots
# Contours
ggplot(faithfuld, aes(eruptions, waiting)) +
  geom_contour(aes(z = density, col = ..level..))
# Tiles
ggplot(faithfuld, aes(eruptions, waiting)) +
  geom_raster(aes(fill = density))
# Bubble plots work better with fewer observations
small = faithfuld[seq(1, nrow(faithfuld), by = 10), ]
ggplot(small, aes(eruptions, waiting)) +
  geom_point(aes(size = density), alpha = 1/3) +
  scale_size_area()

#### Revealing uncertainty
y = c(18,11,16)
df = data.frame(x = 1:3, y = y, se = c(1.2, 0.5, 1.0))
base = ggplot(df, aes(x, y, ymin = y - se, ymax = y + se))
base + geom_crossbar()
base + geom_pointrange()
base + geom_smooth(stat = "identity")
base + geom_errorbar()
base + geom_linerange()
base + geom_ribbon()

#### Weighted data
ggplot(midwest, aes(percwhite, percbelowpoverty))