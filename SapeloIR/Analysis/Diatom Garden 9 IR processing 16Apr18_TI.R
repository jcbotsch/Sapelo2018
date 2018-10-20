library(jpeg)
library(tiff)

#################################################
# 14Apr18 Individual photos (M3 missing)
#################################################
foldername = "H"
basedir <- getwd()
dir <- paste0("/Users/arives/Desktop/DG9 photos 14Apr18/", foldername)
files <- list.files(dir, pattern=".JPG")

# find cropping region
filename=files[1]

d <- readJPEG(paste0(dir,"/",filename))
xright = dim(d)[2]
xleft = 1
ytop = dim(d)[1]
ybottom = 1

plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(d, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)


x <- data.frame(file=rep(NA, 72), height=NA, NDVI=NA, NDVI_NA=NA)
counter <- 0
for(foldername in c("H", "M", "S")){
	dir <- paste0("/Users/arives/Desktop/DG9_photos_14Apr18/", foldername)
	files <- list.files(dir, pattern=".JPG")
	nphotos <- length(files)
	for(i in 1:nphotos){
		counter <- counter + 1
		filename <- files[i]
		
		x$file[counter] <- filename
		x$height[counter] <- foldername
	
		d <- readJPEG(paste0(dir,"/",filename))
		xright = dim(d)[2]
		xleft = 1
		ytop = dim(d)[1]
		ybottom = 1
		
		# plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
		# rasterImage(d, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)
		
		# plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
		# rasterImage(d, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)
		
		# for (j in 1:3) d[, , j] <- (d[, , j] - min(d[, , j]))/(max(d[, , j]) - min(d[, , j]))
		
		NDVI <- (d[, , 1] - .5*(d[,,2] + d[,,3]))/(d[, , 1] + .5*(d[,,2] + d[,,3]))
		#NDVI <-  (d[, , 1] - d[,,2])/(d[, , 1] + d[,,2])
		# for(i in 1:100) NDVI[NDVI==min(NDVI,na.rm=T)] <- NA
		
		# NDVI <- (NDVI-min(NDVI,na.rm=T))
		# NDVI <- NDVI/max(NDVI,na.rm=T)
		# NDVI[NDVI<.1] <- NA
		# NDVI[NDVI>.6] <- NA
		
		# crop d
		xleft = 0
		xright = 2300
		ybottom = 1
		ytop = dim(d)[1]
		
		d <- d[ybottom:ytop, xleft:xright,]
		NDVI <- NDVI[ybottom:ytop, xleft:xright]
	
#		NDVI2 <- NDVI
#		NDVI2 <- NDVI^.5
		NDVI.thresh <- NDVI
		s <- sort(NDVI.thresh)
		thresh <- s[round(.5*length(s))]
		NDVI.thresh[NDVI.thresh < thresh] <- 0

		x$NDVI[counter] <- mean(NDVI.thresh)
		x$NDVI_NA[counter] <- mean(NDVI.thresh[NDVI.thresh>0])
		show(x$NDVI[counter])
		
		if(T){
			par(mfrow=c(2,2))
			plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
			rasterImage(d, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)
			
			plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
			rasterImage((NDVI-min(NDVI,na.rm=T))/max(NDVI-min(NDVI,na.rm=T)), xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)
			
			# plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
			# rasterImage(NDVI2, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)
			
			plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "", main=round(c(x$NDVI[counter], x$NDVI_NA[counter]), digits=3))
			rasterImage((NDVI.thresh-min(NDVI.thresh,na.rm=T))/max(NDVI.thresh-min(NDVI.thresh,na.rm=T)), xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)
			
			hist(NDVI, breaks=100)
		}
	}	
}
write.table(x, "DG9 IR 14Apr18.csv", sep=",", row.names=F)

############################
# analyzing .DNG files

foldername = "H"
basedir <- getwd()
dir <- paste0("/Users/arives/Desktop/DG9_photos_14Apr18/", foldername)
files <- list.files(dir, pattern=".tif")

# find cropping region
filename=files[1]

d <- readTIFF(paste0(dir,"/",filename))
xright = 2300
xleft = 1
ytop = dim(d)[1]-400
ybottom = 400

plot(c(xleft, xright), c(ybottom, ytop), type = "n", xlab = "", ylab = "")
rasterImage(d[ybottom:ytop,xleft:xright,], xleft = xleft, ybottom = ybottom, xright = xright, ytop = ytop, interpolate = F)


x <- data.frame(file=rep(NA, 72), height=NA, NDVI=NA, NDVI_NA=NA)
counter <- 0
for(foldername in c("H", "M", "S")){
	dir <- paste0("/Users/arives/Desktop/DG9_photos_14Apr18/", foldername)
	files <- list.files(dir, pattern=".tif")
	nphotos <- length(files)
	for(i in 1:nphotos){
		counter <- counter + 1
		filename <- files[i]
		
		x$file[counter] <- filename
		x$height[counter] <- foldername
	
		d <- readTIFF(paste0(dir,"/",filename))	
		
		xright = 2300
		xleft = 1
		ytop = dim(d)[1]-400
		ybottom = 400
		
		d <- d[ybottom:ytop,xleft:xright,]
	
		NDVI <- (d[, , 1] - .5*(d[,,2] + d[,,3]))/(d[, , 1] + .5*(d[,,2] + d[,,3]))
		NDVI <- .5*(NDVI + 1)
		NDVI[is.na(NDVI)] <- 0
		
		NDVI.thresh <- NDVI
		s <- sort(NDVI.thresh)
		#thresh <- s[round(.5*length(s))]
		thresh <- .49
		NDVI.thresh[NDVI.thresh < thresh] <- 0

		x$NDVI[counter] <- mean(NDVI.thresh)
		x$NDVI_NA[counter] <- mean(NDVI.thresh[NDVI.thresh>0])
		show(x$NDVI[counter])
		
		if(T){
			par(mfrow=c(2,2))
			plot(c(0, xright-xleft), c(0, ytop-ybottom), type = "n", xlab = "", ylab = "")
			rasterImage(d, xleft = 1, ybottom = 1, xright = xright-xleft, ytop = ytop-ybottom, interpolate = F)
			
			plot(c(0, xright-xleft), c(0, ytop-ybottom), type = "n", xlab = "", ylab = "")
			rasterImage(NDVI, xleft = 1, ybottom = 1, xright = xright-xleft, ytop = ytop-ybottom, interpolate = F)
			
			# plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
			# rasterImage(NDVI2, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)
			
			plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "", main=round(c(x$NDVI[counter], x$NDVI_NA[counter]), digits=3))
			rasterImage(NDVI.thresh, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)
			
			hist(NDVI, breaks=100)
		}
	}	
}
write.table(x, "DG9 IR DNG 14Apr18.csv", sep=",", row.names=F)


#################################################
# Compare IR and chl
#################################################

x.jpg <- read.csv("DG9 IR 14Apr18.csv", header=T)
x.dng <- read.csv("DG9 IR DNG 14Apr18.csv", header=T)
x.jpg <- x.jpg[1:71,]
x.dng <- x.dng[1:71,]
x.jpg$number <- c(1:32, 1:2, 4:32, 9,16,30,17,5,28,8,3)
x.dng$number <- c(1:32, 1:2, 4:32, 9,16,30,17,5,28,8,3)

x.jpg <- rbind(x.jpg[1:34,],c(NA, "M", NA, NA),x.jpg[35:71,])
x.dng<- rbind(x.dng[1:34,],c(NA, "M", NA, NA),x.dng[35:71,])

d <- read.table("Garden 9 chl data 14Apr18.csv", header=T, sep=",")

pdf("IR jpg vs chl 16Apr18.pdf", width=6, height=6)
par(mfcol=c(2,2))
z <- lm(x.jpg$NDVI ~ d$uncorchl_ugl)
R2 <- 1-var(z$residuals)/var(z$fitted.values + z$residuals)
plot(x.jpg$NDVI ~ d$uncorchl_ugl, main=paste("R2 =", round(R2, digits=2)))

z <- lm(x.jpg$NDVI ~ d$chl_ugl)
R2 <- 1-var(z$residuals)/var(z$fitted.values + z$residuals)
plot(x.jpg$NDVI ~ d$chl_ugl, main=paste("R2 =", round(R2, digits=2)))

z <- lm(x.jpg$NDVI_NA ~ d$uncorchl_ugl)
R2 <- 1-var(z$residuals)/var(z$fitted.values + z$residuals)
plot(x.jpg$NDVI_NA ~ d$uncorchl_ugl, main=paste("R2 =", round(R2, digits=2)))

z <- lm(x.jpg$NDVI_NA ~ d$chl_ugl)
R2 <- 1-var(z$residuals)/var(z$fitted.values + z$residuals)
plot(x.jpg$NDVI_NA ~ d$chl_ugl, main=paste("R2 =", round(R2, digits=2)))
dev.off()

pdf("IR dng vs chl 16Apr18.pdf", width=6, height=6)
par(mfcol=c(2,2))
z <- lm(x.dng$NDVI ~ d$uncorchl_ugl)
R2 <- 1-var(z$residuals)/var(z$fitted.values + z$residuals)
plot(x.dng$NDVI ~ d$uncorchl_ugl, main=paste("R2 =", round(R2, digits=2)))

z <- lm(x.dng$NDVI ~ d$chl_ugl)
R2 <- 1-var(z$residuals)/var(z$fitted.values + z$residuals)
plot(x.dng$NDVI ~ d$chl_ugl, main=paste("R2 =", round(R2, digits=2)))

z <- lm(x.dng$NDVI_NA ~ d$uncorchl_ugl)
R2 <- 1-var(z$residuals)/var(z$fitted.values + z$residuals)
plot(x.dng$NDVI_NA ~ d$uncorchl_ugl, main=paste("R2 =", round(R2, digits=2)))

z <- lm(x.dng$NDVI_NA ~ d$chl_ugl)
R2 <- 1-var(z$residuals)/var(z$fitted.values + z$residuals)
plot(x.dng$NDVI_NA ~ d$chl_ugl, main=paste("R2 =", round(R2, digits=2)))
dev.off()


par(mfrow=c(1,2))
plot(x.jpg$NDVI ~ x.dng$NDVI)
plot(x.jpg$NDVI_NA ~ x.dng$NDVI_NA)

#################################################
# 14Apr18 Single photo
#################################################
foldername = "All"
basedir <- getwd()
dir <- paste0("/Users/arives/Desktop/DG9_photos_14Apr18/", foldername)
files <- list.files(dir, pattern=".JPG")

# find cropping region
filename=files[14]

d <- readJPEG(paste0(dir,"/",filename))
xright = 3470
xleft = 640
ytop = dim(d)[1]-20
ybottom = 40

d <- d[ybottom:ytop, xleft:xright,]

par(mfrow=c(2,2))
plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(d, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

NDVI <- (d[, , 1] - .5*(d[,,2] + d[,,3]))/(d[, , 1] + .5*(d[,,2] + d[,,3]))
#NDVI <-  (d[, , 1] - d[,,2])/(d[, , 1] + d[,,2])
NDVI <- (NDVI-min(NDVI,na.rm=T))
NDVI <- NDVI/max(NDVI,na.rm=T)

plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(NDVI, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

hist(NDVI,breaks=100)

NDVI.thresh <- NDVI
s <- sort(NDVI.thresh)
thresh <- s[round(.4*length(s))]
NDVI.thresh[NDVI.thresh < thresh] <- 0
plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(NDVI.thresh, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

deltax <- dim(NDVI)[2]/9
deltay <- dim(NDVI)[1]/8


height <- c(rep("H", times=8),rep("M", times=8),rep("H", times=8),rep("M", times=8),rep("S", times=8),rep("H", times=8),rep("M", times=8),rep("H", times=8),rep("M", times=8))
number <- c(1:8,1:8,9:16,9:16,c(9,16,30,17,5,28,8,3),17:24,17:24,25:32,25:32)
x <- data.frame(number=number, height=height, NDVI=NA, NDVI_NA=NA)
counter <- 0
for(j in 1:9) for(i in 1:8){
	counter <- counter+1
	xleft = (j-1)*deltax
	xright = j*deltax
	ybottom = (i-1)*deltay
	ytop = i*deltay
	NDVI_disc <- NDVI.thresh[ybottom:ytop, xleft:xright]
	plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
	rasterImage(NDVI_disc, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)
	x$NDVI[counter] <- mean(NDVI_disc, na.rm=T)
	x$NDVI_NA[counter] <- mean(NDVI_disc[NDVI_disc>0], na.rm=T)
}

x

write.table(x, "DG9 IR full 14Apr18.csv", sep=",", row.names=F)

#############
# copare with individual photos jpg
x <- read.csv("DG9 IR full 14Apr18.csv", header=T)
x <- x[order(x$height),]

x.jpg <- read.csv("DG9 IR 14Apr18.csv", header=T)
x.jpg <- x.jpg[1:71,]
x.jpg$number <- c(1:32, 1:2, 4:32, 9,16,30,17,5,28,8,3)
x.jpg <- rbind(x.jpg[1:34,],c(NA, "M", NA, NA),x.jpg[35:71,])

pdf("IR jpg separate vs. full 16Apr18.pdf", width=6, height=6)
par(mfcol=c(2,2))
z <- lm(x.jpg$NDVI ~ x$NDVI)
R2 <- 1-var(z$residuals)/var(z$fitted.values + z$residuals)
plot(x.jpg$NDVI ~ x$NDVI, main=paste("R2 =", round(R2, digits=2)))

z <- lm(x.jpg$NDVI ~ x$NDVI_NA)
R2 <- 1-var(z$residuals)/var(z$fitted.values + z$residuals)
plot(x.jpg$NDVI ~ x$NDVI_NA, main=paste("R2 =", round(R2, digits=2)))

z <- lm(x.jpg$NDVI_NA ~ x$NDVI)
R2 <- 1-var(z$residuals)/var(z$fitted.values + z$residuals)
plot(x.jpg$NDVI_NA ~ x$NDVI, main=paste("R2 =", round(R2, digits=2)))

z <- lm(x.jpg$NDVI_NA ~ x$NDVI_NA)
R2 <- 1-var(z$residuals)/var(z$fitted.values + z$residuals)
plot(x.jpg$NDVI_NA ~ x$NDVI_NA, main=paste("R2 =", round(R2, digits=2)))

dev.off()




#################################################
# 14Apr18 Single photo - looking at different colors (some look more green) RGB
#################################################
foldername = "All"
basedir <- getwd()
dir <- paste0("/Users/arives/Desktop/DG9 photos 14Apr18/", foldername)
files <- list.files(dir, pattern=".JPG")

# find cropping region
filename=files[14]

d <- readJPEG(paste0(dir,"/",filename))
xright = 3470
xleft = 640
ytop = dim(d)[1]-20
ybottom = 40

d <- d[ybottom:ytop, xleft:xright,]

par(mfrow=c(2,2))
plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(d, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

NDVI <-  (d[, , 1] - d[,,3])/(d[, , 1] + d[,,3])
NDVI <- (NDVI-min(NDVI,na.rm=T))
NDVI <- NDVI/max(NDVI,na.rm=T)

plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(NDVI, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

x <- (d[, , 1] - .5*(d[,,2] + d[,,3]))/(d[, , 1] + .5*(d[,,2] + d[,,3]))
x <- x-min(x)
x <- x/max(x)
plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(x, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)


x <- d[, , 2]/(d[, , 2] + d[,,3])
x <- x
x <- x-min(x)
x <- x/max(x)
plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(x, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

#################################################
# core image
#################################################
# IMG_5381

dir <- "/Users/arives/Desktop/DG9_photos_14Apr18/"

d <- readJPEG(paste0(dir, "IMG_5381.JPG"))

d <- readJPEG(paste0(dir, "IMG_5390.JPG"))

xright = dim(d)[2]
xleft = 1
ytop = dim(d)[1]
ybottom = 1

plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(d, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

# crop d
xleft = 1800
xright = 3400
ybottom = 500
ytop = 1500

d <- d[ybottom:ytop, xleft:xright,]
par(mfrow=c(1,2))
plot(c(0, xright-xleft), c(0, ytop-ybottom), type = "n", xlab = "", ylab = "")
rasterImage(d, xleft = 1, ybottom = 1, xright = xright-xleft, ytop = ytop-ybottom, interpolate = F)

NDVI <- (d[, , 1] - .5*(d[,,2] + d[,,3]))/(d[, , 1] + .5*(d[,,2] + d[,,3]))
NDVI <- NDVI - min(NDVI)
NDVI <- NDVI/max(NDVI)
plot(c(0, xright-xleft), c(0, ytop-ybottom), type = "n", xlab = "", ylab = "")
rasterImage(NDVI, xleft = 1, ybottom = 1, xright = xright-xleft, ytop = ytop-ybottom, interpolate = F)




#################################################
# 12Apr18
#################################################
foldername = "H tubes"
basedir <- getwd()
dir <- paste0("/Users/arives/Desktop/DG9 photos 12Apr18/", foldername)
setwd(dir)
files <- list.files(dir, pattern=".JPG")

# find cropping region
filename=files[1]

d <- readJPEG(filename)
xright = dim(d)[2]
xleft = 1
ytop = dim(d)[1]
ybottom = 1

plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(d, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

# crop d
xleft = 2000
xright = 4000
ybottom = 1
ytop = dim(d)[1]

d <- d[ybottom:ytop, xleft:xright,]
plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(d, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

# for (j in 1:3) d[, , j] <- (d[, , j] - min(d[, , j]))/(max(d[, , j]) - min(d[, , j]))

NDVI <- (d[, , 1] - .5*(d[,,2] + d[,,3]))/(d[, , 1] + .5*(d[,,2] + d[,,3]))
#NDVI <-  (d[, , 1] - d[,,2])/(d[, , 1] + d[,,2])
# for(i in 1:100) NDVI[NDVI==min(NDVI,na.rm=T)] <- NA
NDVI <- (NDVI-min(NDVI,na.rm=T))
NDVI <- NDVI/max(NDVI,na.rm=T)
hist(NDVI,breaks=100)
NDVI[NDVI<.1] <- NA
NDVI[NDVI>.6] <- NA
NDVI <- (NDVI-min(NDVI,na.rm=T))
NDVI <- NDVI/max(NDVI,na.rm=T)

par(mfrow=c(2,2))
plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(d, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(NDVI, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(NDVI^2, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

# hist(NDVI^2, breaks=100)
NDVI2.thresh <- NDVI^2
thresh <- sort(NDVI2.thresh)[round(.75*length(NDVI))]
s <- sort(NDVI2.thresh)
thresh <- s[round(.75*length(s))]
NDVI2.thresh[NDVI2.thresh < thresh] <- 0
# hist(NDVI2.thresh,breaks=100)

plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(NDVI2.thresh, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

###################################################
x <- data.frame(file=rep(NA, 72), height=NA, NDVI=NA, NDVI_NA=NA)
counter <- 0
for(foldername in c("H", "M", "S")){
	dir <- paste0("/Users/arives/Desktop/DG9 photos 12Apr18/", foldername)
	files <- list.files(dir, pattern=".JPG")
	nphotos <- length(files)
	for(i in 1:nphotos){
		counter <- counter + 1
		filename <- files[i]
		
		x$file[counter] <- filename
		x$height[counter] <- foldername
	
		d <- readJPEG(paste0(dir,"/",filename))
		xright = dim(d)[2]
		xleft = 1
		ytop = dim(d)[1]
		ybottom = 1
		
		# plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
		# rasterImage(d, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)
		
		# plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
		# rasterImage(d, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)
		
		# for (j in 1:3) d[, , j] <- (d[, , j] - min(d[, , j]))/(max(d[, , j]) - min(d[, , j]))
		
		NDVI <- (d[, , 1] - .5*(d[,,2] + d[,,3]))/(d[, , 1] + .5*(d[,,2] + d[,,3]))
		#NDVI <-  (d[, , 1] - d[,,2])/(d[, , 1] + d[,,2])
		# for(i in 1:100) NDVI[NDVI==min(NDVI,na.rm=T)] <- NA
		
		# NDVI <- (NDVI-min(NDVI,na.rm=T))
		# NDVI <- NDVI/max(NDVI,na.rm=T)
		# NDVI[NDVI<.1] <- NA
		# NDVI[NDVI>.6] <- NA
		
		# crop d
		xleft = 2000
		xright = 4000
		ybottom = 1
		ytop = dim(d)[1]
		
		d <- d[ybottom:ytop, xleft:xright,]
		NDVI <- NDVI[ybottom:ytop, xleft:xright]
	
#		NDVI2 <- NDVI
#		NDVI2 <- NDVI^.5
		NDVI.thresh <- NDVI
		s <- sort(NDVI.thresh)
		thresh <- s[round(.5*length(s))]
		NDVI.thresh[NDVI.thresh < thresh] <- 0

		x$NDVI[counter] <- mean(NDVI.thresh)
		x$NDVI_NA[counter] <- mean(NDVI.thresh[NDVI.thresh>0])
		show(x$NDVI[counter])
		
		if(T){
			par(mfrow=c(2,2))
			plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
			rasterImage(d, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)
			
			plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
			rasterImage((NDVI-min(NDVI,na.rm=T))/max(NDVI-min(NDVI,na.rm=T)), xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)
			
			# plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
			# rasterImage(NDVI2, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)
			
			plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "", main=round(c(x$NDVI[counter],x$NDVI_NA[counter]), digits=3))
			rasterImage((NDVI.thresh-min(NDVI.thresh,na.rm=T))/max(NDVI.thresh-min(NDVI.thresh,na.rm=T)), xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)
			
			hist(NDVI, breaks=100)
		}
	}	
}
write.table(x, "DG9 IR 12Apr18.csv", sep=",")





#################################################
# find cropping region

# sunlight
filename="IMG_5484.jpg"
filename="CRW_5484.DNG"

# microscope halogen
filename="IMG_5485.jpg"

d <- readJPEG(filename)
# d <- image_read(filename)

# crop d
xleft = 1000
xright = 3000
ybottom = 1
ytop = dim(d)[1]

d <- d[ybottom:ytop, xleft:xright,]
for (j in 1:3) d[, , j] <- (d[, , j] - min(d[, , j]))/(max(d[, , j]) - min(d[, , j]))

xright = xright-xleft+1
xleft = 1
ytop = ytop-ybottom+1
ybottom = 1

NDVI <- (d[, , 1] - .5*(d[,,2] + d[,,3]))/(d[, , 1] + .5*(d[,,2] + d[,,3]))
#NDVI <-  (d[, , 1] - d[,,2])/(d[, , 1] + d[,,2])
# for(i in 1:100) NDVI[NDVI==min(NDVI,na.rm=T)] <- NA
NDVI <- (NDVI-min(NDVI,na.rm=T))
NDVI <- NDVI/max(NDVI,na.rm=T)
hist(NDVI,breaks=100)
NDVI[NDVI<.1] <- NA
NDVI[NDVI>.6] <- NA
NDVI <- (NDVI-min(NDVI,na.rm=T))
NDVI <- NDVI/max(NDVI,na.rm=T)

par(mfrow=c(2,2))
plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(d, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(NDVI, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(NDVI^2, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

NDVI2.thresh <- NDVI^2
thresh <- sort(NDVI2.thresh)[round(.75*length(NDVI))]
s <- sort(NDVI2.thresh)
thresh <- s[round(.75*length(s))]
NDVI2.thresh[NDVI2.thresh < thresh] <- 0
#hist(NDVI2.thresh,breaks=100)

plot(c(0, xright), c(0, ytop), type = "n", xlab = "", ylab = "")
rasterImage(NDVI2.thresh, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

lowbit <- which(NDVI==min(NDVI,na.rm=T), arr.ind = T)
NDVI[(lowbit[1]-5):(lowbit[1]+5),(lowbit[2]-5):(lowbit[2]+5)]

highbit <- which(NDVI==max(NDVI,na.rm=T), arr.ind = T)
NDVI[(highbit[1]-5):(highbit[1]+5),(highbit[2]-5):(highbit[2]+5)]
























	par(mfrow=c(1,1))
	plot(c(0, 4000), c(0, 3000), type = "n", xlab = "", ylab = "")
	rasterImage(d, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

	i.image <- 1

	h <- 550
	k <- 2250
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

# find circles
if (T) {
	h.list <- array(0, dim = 24)
	k.list <- array(0, dim = 24)
	r.list <- array(0, dim = 24)

	par(mfrow=c(1,1))
	plot(c(0, 4000), c(0, 3000), type = "n", xlab = "", ylab = "")
	rasterImage(d, xleft = 1, ybottom = 1, xright = xright, ytop = ytop, interpolate = F)

	i.image <- 1

	h <- 550
	k <- 2250
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 2

	h <- 1150
	k <- 2250
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 3

	h <- 1750
	k <- 2250
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 4

	h <- 2290
	k <- 2250
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 5

	h <- 2900
	k <- 2250
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 6

	h <- 3450
	k <- 2250
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 7

	h <- 550
	k <- 1650
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 8

	h <- 1150
	k <- 1650
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 9

	h <- 1750
	k <- 1650
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 10

	h <- 2290
	k <- 1650
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 11

	h <- 2900
	k <- 1650
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 12

	h <- 3450
	k <- 1650
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r
	
	i.image <- 13

	h <- 550
	k <- 1100
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 14

	h <- 1200
	k <- 1100
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 15

	h <- 1750
	k <- 1100
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 16

	h <- 2290
	k <- 1100
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 17

	h <- 2900
	k <- 1100
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 18

	h <- 3450
	k <- 1100
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r
	
	i.image <- 19

	h <- 580
	k <- 550
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 20

	h <- 1150
	k <- 550
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 21

	h <- 1750
	k <- 550
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 22

	h <- 2290
	k <- 500
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 23

	h <- 2900
	k <- 500
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r

	i.image <- 24

	h <- 3500
	k <- 550
	r = 270

	xs = seq(from = h - r, to = h + r) #specify x coordinates
	lines(x = xs, y = k + sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #plot circle (top)
	lines(x = xs, y = k - sqrt(-h^2 + r^2 + 2 * h * xs - xs^2), lwd = 3) #(bottom)

	h.list[i.image] <- h
	k.list[i.image] <- k
	r.list[i.image] <- r}
}
#################################################
par(mfrow=c(1,1))
plot(c(0, 4000), c(0, 3000), type = "n", xlab = "", ylab = "")
rasterImage(d, xleft = 1, ybottom = 1, xright = 4000, ytop = 3000, interpolate = F)

mean.NDVI <- array(0, dim=24)
for (i.image in 1:24) {
	
	##Define and check circle
	h = h.list[i.image]
	k = k.list[i.image]
	r = r.list[i.image]

	xs = seq(from = h - r, to = h + r) #specify x coordinates

	# NDVI 
	sum.NDVI <- 0
	n.NDVI <- 0
	for (i in 1:length(xs)) {
		xx = xs[i] #loop over xs values
		ymax = 3000 - round(k - sqrt(-h^2 + r^2 + 2 * h * xx - xx^2), 0) #lower edge of circle at x
		ymin = 3000 - round(k + sqrt(-h^2 + r^2 + 2 * h * xx - xx^2), 0) #upper edge of circle at x
		ys = seq(from = ymin, to = ymax, by = 1) #sequence of y values at x
		sum.NDVI <- sum.NDVI + sum(NDVI[ys, xx],na.rm=T)
		n.NDVI <- n.NDVI + (ymax - ymin + 1)
	}

	mean.NDVI[i.image] <- sum.NDVI/n.NDVI
	
	#plot NDVI 
	if (F) {
		sum.NDVI <- 0
		n.NDVI <- 0
		dd <- NDVI 
		dd[,1:(min(xs) - 1)] <- 1
		dd[,(max(xs) + 1):4000] <- 1
		for (i in 1:length(xs)) {
			xx = xs[i] #loop over xs values
			ymax = 3000 - round(k - sqrt(-h^2 + r^2 + 2 * h * xx - xx^2), 0)
			ymin = 3000 - round(k + sqrt(-h^2 + r^2 + 2 * h * xx - xx^2), 0)
			# ymax = 3000 - (k-r)
			# ymin = 3000 - (k+r)
			dd[1:ymin,xx] <- 1
			dd[ymax:3000,xx] <- 1
			ys = seq(from = ymin, to = ymax, by = 1) #sequence of y values at x
			sum.NDVI <- sum.NDVI + sum(dd[ys, xx])
			n.NDVI <- n.NDVI + (ymax - ymin + 1)
		}
		
		pdf(file=paste0(substr(image.list[i.image], start=1, stop=8),"_NDVI.pdf"), width=4, height=5)

		plot(c(1000, 3000), c(500, 2500), type = "n", xlab = "", ylab = "", main=paste(image.list[i.image], .001*round(1000*mean.NDVI[i.image])))
		rasterImage((dd + 1)/2, xleft = 1, ybottom = 1, xright = 4000, ytop = 3000, interpolate = F)
		axis(side=1, labels=F, tick=F, lty=0)
		axis(side=2, labels=F, tick=F, lty=0)
		axis(side=3, labels=F, tick=F, lty=0)
		axis(side=4, labels=F, tick=F, lty=0)
		
		dev.off()
	}
}
mean.NDVI
quartz()
hist(mean.NDVI)

write.table(mean.NDVI, file=paste0("NDVI_", filename,".csv"), sep=",")