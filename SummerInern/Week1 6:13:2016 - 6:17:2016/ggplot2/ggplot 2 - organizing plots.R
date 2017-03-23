### ggplot2 learning - organizing the plots
## Data comes from the same dataset
# Faceting
# Reference: http://blog.csdn.net/tanzuozhev/article/details/51112177
library(reshape2)
head(tips)
library(ggplot2)
p1 = ggplot(tips, aes(x = total_bill, y = tip/total_bill)) +
  geom_point(shape = 1)
p1
## facet_grid()
# vertically 
p1 + facet_grid(sex ~ .)
# horizontally
p1 + facet_grid(. ~ sex)
# vertical for sex and horizontal for day
p1 + facet_grid(sex ~ day)

## facet_wrap()
p1 + facet_wrap( ~ day,ncol = 2)
# Change the labels
labels = c(Female = "Women", Male = "Men")
p1 + facet_grid(. ~ sex, labeller = labeller(sex = labels))

# Setting the limits
hp = ggplot(tips, aes(x = total_bill)) + 
  geom_histogram(binwidth = 2, col = "blue")
hp + facet_grid(sex ~ smoker)
# free_y
hp + facet_grid(sex ~ smoker, scales = "free_y")
hp + facet_grid(sex ~ smoker, scales = "free", space = "free")

## Data comes from different dataset
# Method 1: 
library(ggplot2)
x = 1:20
fun1 = function(x) {
  3 + 2 * log(x)
}
fun2 = function(x) {
  3 - 2 * log(x)
}
a = ggplot(NULL, aes(x = x)) + 
  stat_function(fun = fun1, n = 20, shape = 1, size = 3, geom = c("point")) +
  geom_line(stat = "function", fun = fun1) #+
  #theme_bw() + 
  #theme(panel.grid = element_blank())
b = ggplot(NULL, aes(x = x)) + 
  stat_function(fun = fun2, n = 20, shape = 1, size = 3, geom = c("point")) +
  geom_line(stat = "function", fun = fun2) +
  theme_bw() #+ 
  #theme(panel.grid = element_blank())
library(grid)
grid.newpage() 
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 2))) 
print(a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1)) 
print(b, vp = viewport(layout.pos.row = 1, layout.pos.col = 2)) 

# Method 2:
library(gridExtra)
grid.arrange(a,b,ncol = 2)
#############
ggplot(aes(x = 1:4, y = 1:4)) + geom_point
