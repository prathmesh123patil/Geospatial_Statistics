# Name: Prathamesh Patil
# Roll No: 213350001
#Sub: GNR mini project

setwd("C:\\Users\\hp\\Desktop\\GNR Mini project")

#load packages 
library(readxl)
library(ggplot2)
library(sp)
library(gstat)
library(fields)
library(spatstat)   #point pattern analysis

#read data
data= read_excel("C:\\Users\\hp\\Desktop\\GNR Mini project\\lat_long_yearly_2019rainfall.xlsx")

#convert data into dataframe
df1=data.frame(data)

df1
# mean,median, mode, standard deviation
mean(df1$Rainfall)

median(df1$Rainfall)
mode(df1$Rainfall)
sd(df1$Rainfall)
var(df1$Rainfall)


#convert data into vector format
data_vec= unlist(data)
View(data_vec)
data_vec = unlist(data[,-1:-2])
View(data_vec)
data_vec = matrix(data_vec,ncol=1)


#plot histogram
hist(data_vec,freq = FALSE, xlab = "annual rainfall", ylab = "relative frequency",col = "orange")
hist(data_vec, col = "green", xlab = "annual rainfall", ylab = "Frequency")


#boxplot
boxplot(data_vec, col = "red")

#calculate Interquartile range
quantile(df1$Rainfall)

#convert data into dataframe
data_df = data.frame(name=c("rainfall"),value=data_vec)

#violin plot using ggplot
p=ggplot(data_df, aes(x=name,y=value))
p+geom_violin(width=1, col = "blue")+geom_boxplot(width = 0.15, col= "red")


df1=data.frame(data)

#assign the column names
colnames(df1) = c("E","N","rainfall")
View(df1)

#bubble plot
coordinates(df1)=~E+N
bubble(df1,"rainfall")

#visualize the data
plot(df1$E,df1$N,pch=20,cex=1.5,col = "orange",xlab="Easting",ylab="northing")
grid()
#text(df1$E,df1$N,round(df1$rainfall),adj = c(0.5,0.5))

#fit linear regression considering E and N as input
model.1 = lm(rainfall~E+N,data = df1)
summary(model.1)

#linear regression using E as input
model.2 = lm(rainfall~E,data = df1)
summary(model.2)

#linear regression using N as input
model.3 = lm(rainfall~N,data = df1)
summary(model.3)

# construct the interpolation grid
range(data$X)
range(data$Y)

seq.E= seq(range(df1$E)[1],range(df1$E)[2],by=0.25)
seq.N= seq(range(df1$N)[1],range(df1$N)[2],by=0.25)

grid = expand.grid(E=seq.E,N=seq.N)

plot(grid$E,grid$N)


#predict using linear regression model
predict.1 = predict.lm(model.1,newdata = grid)


#visualize predict.1
require(sp)

#convert grid info into spatial object
coordinates(grid)= c("E","N")
sp.grid = SpatialPointsDataFrame(coords = coordinates(grid),data = as.data.frame(predict.1))
gridded(sp.grid)=TRUE

spplot(sp.grid,main="rainfall pattern",xlab="Easting",ylab= "Northing")


#plot variogram cloud
vc= variogram((df1$rainfall)~1,locations = df1, cloud = T)
head(vc)
plot(vc)


#plot experimental variogram
exv= variogram((df1$rainfall)~1,locations = df1, cloud = F)
plot(exv,plot.numbers=T)
head(exv)

v=variogram(rainfall~1,df1,locations = df1)
plot(v,plot.nunbers=T)

#show different variogram models
show.vgms()
#plot exv with cutoff
exv_cutoff= variogram((df1$rainfall)~1,locations = df1, cloud = F,cutoff = 1.8)
plot(exv_cutoff,plot.numbers=T)

show.vgms(models=c("Exp","Sph","Pen","Pow"),range = 1.6, sill = 1200000, nugget = 220000,max = 1.8)

#fit spherical variogram model
vm1=vgm(model = "Sph",nugget = 220000, range = 1.6, sill = 1200000)

#get error estimate
vmf=fit.variogram(exv_cutoff,vm1,fit.method = 2)
vmf$range


#fit exponential variogram model
vm2=vgm(model = "Exp",nugget = 220000, range = 1.6, sill = 1200000)

#get error estimate
vmf=fit.variogram(exv_cutoff,vm2,fit.method = 2)
vmf$range



#fit pentaspherical variogram model
vm3=vgm(model = "Pen",nugget = 220000, range = 1.6, sill = 1200000)

#get error estimate
vmf=fit.variogram(exv_cutoff,vm3,fit.method = 2)
vmf$range


#fit power function variogram model
vm4=vgm(model = "Pow",nugget = 220000, range = 1.6, sill = 1200000)

#get error estimate
vmf=fit.variogram(exv_cutoff,vm4,fit.method = 2)
vmf$range


##detecting anisoptropy
plot(df1,asp=1,cex=4*df1$rainfall/max(df1$rainfall),pch=1,col= "blue")

plot(variogram(rainfall~1,df1,locations = df1,alpha=c(30,60,90,120)),plot.numbers=T,pch=20,col="orange")


##kriging
coordinates(grid)= c("E","N")
sp.grid = SpatialPointsDataFrame(coords = coordinates(grid),data = as.data.frame(predict.1))
gridded(sp.grid)=TRUE

#ordinary kriging
ok=krige(rainfall~1,locations = df1,newdata=sp.grid,model=vmf)
spplot(ok,"var1.pred",asp = 1)















