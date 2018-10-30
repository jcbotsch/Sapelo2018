#====Load Packages====
library(jpeg)



#====Set up directories to read in photos====
foldername = "26Oct18" #enter folder where photos are housed. Mine are dates.
basedir = getwd() #set base working directory
dir = paste0("./Photos/", foldername) 
files = list.files(dir) #get the name for each file



#get the number of photos
nphotos = length(files)

#create data frame
x = data.frame(file=rep(NA, nphotos), date=NA, NDVI=NA, NDVI_min=NA, NDVI_max=NA) #didn't include Tony's

#create 'counter' for each row
counter=0

#for loop to apply this to each photo
for(i in 1:nphotos){
		counter = counter + 1 #advance the counter
		filename = files[i] #get file name for the ith file
		
		x$file[counter] = filename #indicate file name
		x$date[counter] = foldername #fills date column with the folder name
	
		d = readJPEG(paste0(dir,"/",filename)) #read in photo
		
		#get dimensions of photo
		xright = dim(d)[2] 
		xleft = 1
		ytop = dim(d)[1]
		ybottom = 1
		
		#calculate NDVI
		#here we calculate ENDVI which is ((NIR+Green)-2*(blue))/((NIR+Green)+2blue)

		ENDVI = ((d[, , 1] + d[,,2]) - 2*d[,,3])/((d[, , 1] + d[,,2]) + 2*d[,,3])
    
		#Tony Versions
		# NDVI_1 <- (d[,,1] - .5*(d[,,2] + d[,,3]))/(d[,,1] + .5*(d[,,2] + d[,,3]))
		# x$NDVI_1[counter] <- mean(NDVI_1)
		# 
		# NDVI_2 <- (d[,,1] - d[,,3])/(d[,,1] + d[,,3])
		# x$NDVI_2[counter] <- mean(NDVI_2)
    
		#fill in the NDVI columns
		x$NDVI[counter] = mean(ENDVI)
		x$NDVI_min[counter] = min(ENDVI)
		x$NDVI_max[counter] = max(ENDVI)
		# x$NDVI_1[counter] = mean(NDVI_1)
		# x$NDVI_2[counter] = mean(NDVI_2)

	
}

#write table with the output
write.table(x, paste0("./Data/NDVI_",foldername,".csv"), sep=",", row.names=F)
