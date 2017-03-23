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
