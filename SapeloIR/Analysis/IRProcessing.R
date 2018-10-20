library(jpeg)



#====Set up directories to read in photos====
foldername = "20Oct18" #enter folder where photos are housed
basedir = getwd() #set base working directory
dir = paste0("./Photos/", foldername) 
files = list.files(dir) 

xleft = 1
xright = dim(w)[2]
ybottom = 1
ytop = dim(w)[1]


nphotos = length(files)
x = data.frame(file=rep(NA, nphotos), date=NA, NDVI=NA, NDVI_min=NA, NDVI_max=NA)
counter=0

for(i in 1:nphotos){
		counter = counter + 1
		filename = files[i]
		
		x$file[counter] = filename
		x$date[counter] = foldername
	
		d = readJPEG(paste0(dir,"/",filename))
		xright = dim(d)[2]
		xleft = 1
		ytop = dim(d)[1]
		ybottom = 1
		
		#calculate NDVI
		#here we calculate ENDVI which is ((NIR+Green)-2*(blue))/((NIR+Green)+2blue)

		ENDVI = ((d[, , 1] + d[,,2]) - 2*d[,,3])/((d[, , 1] + d[,,2]) + 2*d[,,3])
    

		x$NDVI[counter] = mean(ENDVI)
		x$NDVI_min[counter] = min(ENDVI)
		x$NDVI_max[counter] = max(ENDVI)

	
}

write.table(x, paste0("./Data/NDVI_",foldername,".csv"), sep=",", row.names=F)
