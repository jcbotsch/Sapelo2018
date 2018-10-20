library(lme4)
library(lmerTest)
library(car)
library(jpeg)

dir <- "./IRPhotos_Cropped"
files <- list.files(dir, pattern=".jpg")

w <- readJPEG(paste0(dir,"/",files[1]))
xleft = 1
xright = dim(w)[2]
ybottom = 1
ytop = dim(w)[1]

nphotos <- length(files)
x <- data.frame(file=rep(NA, nphotos), NDVI_1=NA, NDVI_2=NA)
counter <- 0

for(i in 1:nphotos){
	counter <- counter + 1
	filename <- files[i]
	
	x$file[counter] <- filename

	w <- readJPEG(paste0(dir,"/",filename))
	xleft = 1
	xright = dim(w)[2]
	ybottom = 1
	ytop = dim(w)[1]
	w <- w[ybottom:ytop, xleft:xright,]
	
	NDVI_1 <- (w[,,1] - .5*(w[,,2] + w[,,3]))/(w[,,1] + .5*(w[,,2] + w[,,3]))
	x$NDVI_1[counter] <- mean(NDVI_1)

	NDVI_2 <- (w[,,1] - w[,,3])/(w[,,1] + w[,,3])
	x$NDVI_2[counter] <- mean(NDVI_2)
	
	if(T){
		par(mfrow=c(1,3))
		plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "", main=files[i])
		rasterImage(w, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)
		
		plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "", main=round(x$NDVI_1[counter], digits=3))
		rasterImage((NDVI_1-min(NDVI_1,na.rm=T))/max(NDVI_1-min(NDVI_1,na.rm=T)), xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)
		
		plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "", main=round(x$NDVI_2[counter], digits=3))
		rasterImage((NDVI_2-min(NDVI_2,na.rm=T))/max(NDVI_2-min(NDVI_2,na.rm=T)), xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)
	}
}	

write.table(x, "Fert plot NDVI 10Aug18.csv", sep=",", row.names=F)