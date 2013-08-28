#this is a really crude zoom function for use in plotting shapefiles or rasters
#it is meant to be run on an object that is already in your plot window

#it can be run multiple times to zoom in as far as you want.
#to zoom all the way out, make your first click to the right of your second.

#once you've zoomed in as far as you want, you can add other layers to the plot by plotting them normally with add=T

simple.zoom<-function(x,y)
{
	cat("click the bottom left and upper right corners of the zoom area")
	b.left<-locator(1)
	u.right<-locator(1)
	xmin<-b.left$x
	ymin<-b.left$y
	xmax<-u.right$x
	ymax<-u.right$y
	if(xmax<xmin|ymax<ymin)
		plot(x,y)
	else
		plot(x,y,xlim=c(xmin,xmax),ylim=c(ymin,ymax))
}