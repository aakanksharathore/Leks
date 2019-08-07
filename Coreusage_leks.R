##Code to plot core usage areas of blackbuck on the leks

#install.packages(c()'adehabitatHR', 'sp'))
library(adehabitatHR)    #loads sp package
## Loading data
fname <- file.choose()
dat = read.csv(fname, header=TRUE)

ls=as.data.frame(table(dat$ID))
x=ls[which(ls$Freq<6),"Var1"]
dat=dat[!(dat$ID %in% x),]

dat$x=(dat$xmin+dat$xmax)/2
dat$y=(dat$ymin+dat$ymax)/2



coordinates(dat) <- dat[, c('x', 'y')]
mcp_est100 <- mcp(dat[, "ID"], percent = 100)
mcp_est90 <- mcp(dat[, "ID"], percent = 90)
mcp_est50 <- mcp(dat[, "ID"], percent = 50)
mcp_est20 <- mcp(dat[, "ID"], percent = 20)

library(OpenImageR) # load the ReadImages package
im <- readImage(file.choose()) # open the jpeg image and read it into an object 'j'
#imageShow(im)




par(mar = c(0,0,0,0)) # set zero margins on all 4 sides
plot(x=NULL, y=NULL, asp = 1,col=factor(dat$ID), xlim=c(min(dat$x),max(dat$x)),ylim=c(min(dat$y),max(dat$y)))

rasterImage(im, xleft = min(dat$x), ybottom = min(dat$y), xright = max(dat$x), ytop = max(dat$y)) # plot jpeg

points(dat$x, dat$y, asp = 1,col=factor(dat$ID), xlim=c(min(dat$x),max(dat$x)),ylim=c(min(dat$y),max(dat$y)))

points(dat$x,dat$y, cex = .75, pch = 19, col = 'orange')
cl=1:length(mcp_est50$id)
plot(mcp_est50, col =  cl,add=TRUE)
