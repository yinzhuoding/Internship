frames = 10
for(i in 1:frames) {
  if(i < 10) {
    name = paste('000',i,'plot.png',sep = '')
  }
  if(i >= 10) {
    name = paste('00',i,'plot.png',sep = '')
  }
  x = seq(0,i,1)
  f.3 = dbinom(x, size = i, prob = 0.3)
  f.7 = dbinom(x, size = i, prob = 0,7)
  png(name)
  plot(x,f.3,type = 'h', xlim = c(0,frames), ylim = c(0,0.7), col = 'red')
  lines(x,f.7, type = 'h', co = 'blue')
  dev.off()
}
###################
## using animation package
draw.curve = function(cutoff) {
  a = ggplot(data = NULL, aes(x = seq(1,10,1), y = cutoff)) +
    geom_point() +
    scale_y_continuous(limits = c(0,20))
  print(a)
}
draw.curve(3)
trace.animate = function() {
  for(i in 1:15)
    draw.curve(i)
}
saveGIF(trace.animate(),interval = 0.2, movie.name = "trace2.gif")

#####################
## using imagemagick
png(file = "example.png", width = 200, height = 200)
for(i in c(10:1, "GO!")) {
  plot.new()
  text(.5, .5, i, cex = 6)
}
dev.off()
system("convert -delay 20 *.png example1.gif")