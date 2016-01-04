RIP_NorthBR_ref=subset(RIP_RS_final, ECO_LVL_3NAME=='Northern Basin and Range')
SED_NorthBR_ref=subset(SED_RS_final, ECO_LVL_3NAME=='Northern Basin and Range')

#To create different graphs change the variables below. 
#To create different graphs change the variables below. 
refcompare=SED_NorthBR_ref
refcompare$variable=refcompare$PCT_SAFN
dataset=AquametCheck
dataset$variable=dataset$PCT_SAFN_CHECK

######################instrucutions
#need two different dataframes
# "dataset" is the managed dataset (adataframe), and "variable" is the column with the managed data of interest (no NA's)
# "refcompare" is reference dataset (adataframe ) and "variable" is the column with the reference data of interest (no NA's)
# yscalar changes the height of the y axis! But this is multiplied so it isn't an exact match to the y axis
# xlim and xlim2 are the x axis width, if you have outliers then you may want to adjust this
# may have to adjust "best condition" and "worse condition" in the code below depending on if high or low values are good condition.
##################################################
#code starts here################################
yscalar= 0.55 # this is the scalar to adjust the height of the y axis
#max1=max(refcompare$variable)
#max2=max(dataset$variable)
#max1=quantile(refcompare$variable,1.99) # use quantiles if you have outliers
#max2=quantile(dataset$variable,1.99)
#min1=min(refcompare$variable)
#min2=min(dataset$variable)
#min1=quantile(refcompare$variable,0.0001)
#min2=quantile(dataset$variable,0.0001)
#breaks=10
#xlim=min(min1,min2)
#xlim2=max(max1,max2)

breaks=10
xlim=0
xlim2=100

histref1=refcompare$variable

#hist1=function(){ # this is if you want to make a function to output to workd
par(mai=c(1.3,1.3,1.3,0.42))

histman=hist(((dataset$variable)/1),ylab="Frequency",freq=FALSE,col="gray",main="  ",cex.axis=.9,cex.lab=1,xlab=" ",breaks=breaks,xlim=c(xlim,xlim2))
histmanmax=max(histman$density)
histmanmax2=histmanmax + histmanmax*yscalar
histman=hist(((dataset$variable)/1),ylab="Frequency",freq=FALSE,col="gray",main="Percent Fine Sediment Results",cex.axis=1.,cex.lab=1.3,xlab=" ",xlim=c(xlim,xlim2),breaks=breaks,ylim=c(0,histmanmax2))

labels=c("Best Condition <-------------------------> Worst Condition")
axis(1,labels=c(labels),at=xlim + (xlim2 - xlim)/2,lwd=0,line=1.2,cex.axis=1.2,tick=FALSE)
# axis(1,labels=c(" Percent of Range"),at=xlim + (xlim2 - xlim)/2  ,cex.axis=1,line=2)
lines(density(histref1),col="steelblue4",lwd=4)

legend("topleft", c("Test Sites"), cex=1, 
       bty="n", fill=c("gray"),x.intersp=1.3)
legend("topleft", c("Reference Sites"), cex=1, 
       lty=c(1),merge=T,bty="n",col="steelblue4",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(28,0.0242), c("Poor Threshold"), cex=1, 
      lty=c(1),merge=T,bty="n",col="red",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(28,0.02425), c("Fair Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="yellow",lwd=4,x.intersp=1,y.intersp=1)

box(lty = '11', col = 'black')
box("outer",col="black")

ablineclip(v=76.43, col="red", lwd=3,y2=0.0175)
ablineclip(v=54.29, col="yellow",lwd=3,y2=0.0175)
#abline(h=0.0177, col="blue",lwd=3,)
#abline(h=0.0212, col="blue",lwd=3,)



#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
SED_NorthBR_ref=subset(SED_RS_final, ECO_LVL_3NAME=='Northern Basin and Range')

#To create different graphs change the variables below. 
refcompare=SED_NorthBR_ref
refcompare$variable=refcompare$LINCIS_H
refcompare=data.frame(refcompare$variable)
refcompare=na.omit(refcompare)
refcompare$variable=refcompare$refcompare.variable
dataset=AquametCheck
dataset$variable=dataset$LINCIS_H_CHECK

yscalar= 0.55 # this is the scalar to adjust the height of the y axis
breaks=10
xlim=-1.0
xlim2=.5

histref1=refcompare$variable
par(mai=c(1.3,1.3,1.3,0.42))
histman=hist(((dataset$variable)/1),ylab="Frequency",freq=FALSE,col="gray",main="  ",cex.axis=.9,cex.lab=1,xlab=" ",breaks=breaks,xlim=c(xlim,xlim2))
histmanmax=max(histman$density)
histmanmax2=histmanmax + histmanmax*yscalar
histman=hist(((dataset$variable)/1),ylab="Frequency",freq=FALSE,col="gray",main="Floodplain Connectivity Results",cex.axis=1.,cex.lab=1.3,xlab=" ",xlim=c(xlim,xlim2),breaks=breaks,ylim=c(0,histmanmax2))

labels=c("Best Condition <-------------------------> Worst Condition")
axis(1,labels=c(labels),at=xlim + (xlim2 - xlim)/2,lwd=0,line=1.2,cex.axis=1.2,tick=FALSE)
lines(density(histref1),col="steelblue4",lwd=4)
legend("topleft", c("Test Sites"), cex=1, 
       bty="n", fill=c("gray"),x.intersp=1.3)
legend("topleft", c("Reference Sites"), cex=1, 
       lty=c(1),merge=T,bty="n",col="steelblue4",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(-.6,3.85), c("Poor Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="red",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(-.6,3.21), c("Fair Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="yellow",lwd=4,x.intersp=1,y.intersp=1)

box(lty = '11', col = 'black')
box("outer",col="black")

ablineclip(v=0.223, col="red", lwd=3,y2=2.5)
ablineclip(v=0.036, col="yellow",lwd=3,y2=2.5)
#abline(h=3.2, col="blue",lwd=3,)
#abline(h=2.88, col="blue",lwd=3,)



#####################################################################################
#Eco10
#####################################################################################
SED_XE_NORTH_ref=subset(SED_RS_final, ECO10=='XE-NORTH')


#To create different graphs change the variables below. 
refcompare=SED_XE_NORTH_ref
refcompare$variable=refcompare$LINCIS_H
refcompare=data.frame(refcompare$variable)
refcompare=na.omit(refcompare)
refcompare$variable=refcompare$refcompare.variable
dataset=AquametCheck
dataset$variable=dataset$LINCIS_H_CHECK

yscalar= 0.55 # this is the scalar to adjust the height of the y axis
breaks=10
xlim=-1.0
xlim2=.5

histref1=refcompare$variable
par(mai=c(1.3,1.3,1.3,0.42))
histman=hist(((dataset$variable)/1),ylab="Frequency",freq=FALSE,col="gray",main="  ",cex.axis=.9,cex.lab=1,xlab=" ",breaks=breaks,xlim=c(xlim,xlim2))
histmanmax=max(histman$density)
histmanmax2=histmanmax + histmanmax*yscalar
histman=hist(((dataset$variable)/1),ylab="Frequency",freq=FALSE,col="gray",main="Floodplain Connectivity Results (Hybrid Ecoregions)",cex.axis=1.,cex.lab=1.3,xlab=" ",xlim=c(xlim,xlim2),breaks=breaks,ylim=c(0,histmanmax2))

labels=c("Best Condition <-------------------------> Worst Condition")
axis(1,labels=c(labels),at=xlim + (xlim2 - xlim)/2,lwd=0,line=1.2,cex.axis=1.2,tick=FALSE)
lines(density(histref1),col="steelblue4",lwd=4)
legend("topleft", c("Test Sites"), cex=1, 
       bty="n", fill=c("gray"),x.intersp=1.3)
legend("topleft", c("Reference Sites"), cex=1, 
       lty=c(1),merge=T,bty="n",col="steelblue4",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(-.6,3.85), c("Poor Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="red",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(-.6,3.21), c("Fair Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="yellow",lwd=4,x.intersp=1,y.intersp=1)

box(lty = '11', col = 'black')
box("outer",col="black")

ablineclip(v=0.0609, col="red", lwd=3,y2=2.5)
ablineclip(v=0.2668, col="yellow",lwd=3,y2=2.5)
#abline(h=3.2, col="blue",lwd=3,)
#abline(h=2.88, col="blue",lwd=3,)




#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
RIP_NorthBR_ref=subset(RIP_RS_final, ECO_LVL_3NAME=='Northern Basin and Range')

#To create different graphs change the variables below. 
refcompare=RIP_NorthBR_ref
refcompare$variable=refcompare$XCMG
dataset=AquametCheck
dataset$variable=dataset$XCMG_CHECK

yscalar= 0.55 # this is the scalar to adjust the height of the y axis
breaks=10
xlim=0
xlim2=2

histref1=refcompare$variable
par(mai=c(1.3,1.3,1.3,0.42))
histman=hist(((dataset$variable)/1),ylab="Frequency",freq=FALSE,col="gray",main="  ",cex.axis=.9,cex.lab=1,xlab=" ",breaks=breaks,xlim=c(xlim,xlim2))
histmanmax=max(histman$density)
histmanmax2=histmanmax + histmanmax*yscalar
histman=hist(((dataset$variable)/1),ylab="Frequency",freq=FALSE,col="gray",main="Riparian Habitat Complexity Results",cex.axis=1.,cex.lab=1.3,xlab=" ",xlim=c(xlim,xlim2),breaks=breaks,ylim=c(0,histmanmax2))

labels=c("Worst Condition <-------------------------> Best Condition")
axis(1,labels=c(labels),at=xlim + (xlim2 - xlim)/2,lwd=0,line=1.2,cex.axis=1.2,tick=FALSE)
lines(density(histref1),col="steelblue4",lwd=4)
legend("topleft", c("Test Sites"), cex=1, 
       bty="n", fill=c("gray"),x.intersp=1.3)
legend("topleft", c("Reference Sites"), cex=1, 
       lty=c(1),merge=T,bty="n",col="steelblue4",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(.6,2.42), c("Poor Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="red",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(.6,2.43), c("Fair Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="yellow",lwd=4,x.intersp=1,y.intersp=1)

box(lty = '11', col = 'black')
box("outer",col="black")

ablineclip(v=0.579, col="red", lwd=3,y2=1.7)
ablineclip(v=0.997, col="yellow",lwd=3,y2=1.7)
#abline(h=1.755, col="blue",lwd=3,)
#abline(h=2.09, col="blue",lwd=3,)


#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
SED_NorthBR_ref=subset(SED_RS_final, ECO_LVL_3NAME=='Northern Basin and Range')

#To create different graphs change the variables below. 
refcompare=SED_NorthBR_ref
refcompare$variable=refcompare$XFC_NAT
dataset=AquametCheck
dataset$variable=dataset$XFC_NAT_CHECK

yscalar= 0.25 # this is the scalar to adjust the height of the y axis
breaks=10
xlim=0
xlim2=1.2

histref1=refcompare$variable
par(mai=c(1.3,1.3,1.3,0.42))
histman=hist(((dataset$variable)/1),ylab="Frequency",freq=FALSE,col="gray",main="  ",cex.axis=.9,cex.lab=1,xlab=" ",breaks=breaks,xlim=c(xlim,xlim2))
histmanmax=max(histman$density)
histmanmax2=histmanmax + histmanmax*yscalar
histman=hist(((dataset$variable)/1),ylab="Frequency",freq=FALSE,col="gray",main="Instream Habitat Complexity Results",cex.axis=1.,cex.lab=1.3,xlab=" ",xlim=c(xlim,xlim2),breaks=breaks,ylim=c(0,histmanmax2))

labels=c("Worst Condition <-------------------------> Best Condition")
axis(1,labels=c(labels),at=xlim + (xlim2 - xlim)/2,lwd=0,line=1.2,cex.axis=1.2,tick=FALSE)
lines(density(histref1),col="steelblue4",lwd=4)
legend("topleft", c("Test Sites"), cex=1, 
       bty="n", fill=c("gray"),x.intersp=1.3)
legend("topleft", c("Reference Sites"), cex=1, 
       lty=c(1),merge=T,bty="n",col="steelblue4",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(.4,3.105), c("Poor Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="red",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(.4,2.59), c("Fair Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="yellow",lwd=4,x.intersp=1,y.intersp=1)

box(lty = '11', col = 'black')
box("outer",col="black")

ablineclip(v=0.0718, col="red", lwd=3,y2=2.25)
ablineclip(v=0.2688, col="yellow",lwd=3,y2=2.25)
#abline(h=2.59, col="blue",lwd=3,)
#abline(h=2.33, col="blue",lwd=3,)


#####################################################################################
#Eco 10
#####################################################################################
SED_XE_NORTH_ref=subset(SED_RS_final, ECO10=='XE-NORTH')
#To create different graphs change the variables below. 
refcompare=SED_XE_NORTH_ref
refcompare$variable=refcompare$XFC_NAT
dataset=AquametCheck
dataset$variable=dataset$XFC_NAT_CHECK

yscalar= 0.25 # this is the scalar to adjust the height of the y axis
breaks=10
xlim=0
xlim2=1.2

histref1=refcompare$variable
par(mai=c(1.3,1.3,1.3,0.42))
histman=hist(((dataset$variable)/1),ylab="Frequency",freq=FALSE,col="gray",main="  ",cex.axis=.9,cex.lab=1,xlab=" ",breaks=breaks,xlim=c(xlim,xlim2))
histmanmax=max(histman$density)
histmanmax2=histmanmax + histmanmax*yscalar
histman=hist(((dataset$variable)/1),ylab="Frequency",freq=FALSE,col="gray",main="Instream Habitat Complexity Results (Hybrid Ecoregions)",cex.axis=1.,cex.lab=1.3,xlab=" ",xlim=c(xlim,xlim2),breaks=breaks,ylim=c(0,histmanmax2))

labels=c("Worst Condition <-------------------------> Best Condition")
axis(1,labels=c(labels),at=xlim + (xlim2 - xlim)/2,lwd=0,line=1.2,cex.axis=1.2,tick=FALSE)
lines(density(histref1),col="steelblue4",lwd=4)
legend("topleft", c("Test Sites"), cex=1, 
       bty="n", fill=c("gray"),x.intersp=1.3)
legend("topleft", c("Reference Sites"), cex=1, 
       lty=c(1),merge=T,bty="n",col="steelblue4",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(.4,3.105), c("Poor Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="red",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(.4,2.59), c("Fair Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="yellow",lwd=4,x.intersp=1,y.intersp=1)

box(lty = '11', col = 'black')
box("outer",col="black")

ablineclip(v=0.0727, col="red", lwd=3,y2=2.25)
ablineclip(v=0.2205, col="yellow",lwd=3,y2=2.25)
#abline(h=2.59, col="blue",lwd=3,)
#abline(h=2.33, col="blue",lwd=3,)






#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
SED_NorthBR_ref=subset(SED_RS_final, ECO_LVL_3NAME=='Northern Basin and Range')

#To create different graphs change the variables below. 
refcompare=SED_NorthBR_ref
refcompare$variable=refcompare$XCDENMID
dataset=AquametCheck
dataset$variable=dataset$xcdenmid_CHECK

yscalar= .3 # this is the scalar to adjust the height of the y axis
breaks=10
xlim=0
xlim2=100

histref1=refcompare$variable
par(mai=c(1.3,1.3,1.3,0.42))
histman=hist(((dataset$variable)/1),ylab="Frequency",freq=FALSE,col="gray",main="  ",cex.axis=.9,cex.lab=1,xlab=" ",breaks=breaks,xlim=c(xlim,xlim2))
histmanmax=max(histman$density)
histmanmax2=histmanmax + histmanmax*yscalar
histman=hist(((dataset$variable)/1),ylab="Frequency",freq=FALSE,col="gray",main="Midstream Riparian Cover Results",cex.axis=1.,cex.lab=1.3,xlab=" ",xlim=c(xlim,xlim2),breaks=breaks,ylim=c(0,histmanmax2))

labels=c("Worst Condition <-------------------------> Best Condition")
axis(1,labels=c(labels),at=xlim + (xlim2 - xlim)/2,lwd=0,line=1.2,cex.axis=1.2,tick=FALSE)
lines(density(histref1),col="steelblue4",lwd=4)
legend("topleft", c("Test Sites"), cex=1, 
       bty="n", fill=c("gray"),x.intersp=1.3)
legend("topleft", c("Reference Sites"), cex=1, 
       lty=c(1),merge=T,bty="n",col="steelblue4",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(30,.0425), c("Poor Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="red",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(30,.0426), c("Fair Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="yellow",lwd=4,x.intersp=1,y.intersp=1)

box(lty = '11', col = 'black')
box("outer",col="black")

ablineclip(v=0.9291, col="red", lwd=3,y2=.033)
ablineclip(v=20.555, col="yellow",lwd=3,y2=.033)
#abline(h=0.0388, col="blue",lwd=3,)
#abline(h=.035, col="blue",lwd=3,)

#####################################################################################
#Eco10
#####################################################################################
SED_XE_NORTH_ref=subset(SED_RS_final, ECO10=='XE-NORTH')

#To create different graphs change the variables below. 
refcompare=SED_XE_NORTH_ref
refcompare$variable=refcompare$XCDENMID
dataset=AquametCheck
dataset$variable=dataset$xcdenmid_CHECK

yscalar= .3 # this is the scalar to adjust the height of the y axis
breaks=10
xlim=0
xlim2=100

histref1=refcompare$variable
par(mai=c(1.3,1.3,1.3,0.42))
histman=hist(((dataset$variable)/1),ylab="Frequency",freq=FALSE,col="gray",main="  ",cex.axis=.9,cex.lab=1,xlab=" ",breaks=breaks,xlim=c(xlim,xlim2))
histmanmax=max(histman$density)
histmanmax2=histmanmax + histmanmax*yscalar
histman=hist(((dataset$variable)/1),ylab="Frequency",freq=FALSE,col="gray",main="Midstream Riparian Cover Results (Hybrid Ecoregions)",cex.axis=1.,cex.lab=1.3,xlab=" ",xlim=c(xlim,xlim2),breaks=breaks,ylim=c(0,histmanmax2))

labels=c("Worst Condition <-------------------------> Best Condition")
axis(1,labels=c(labels),at=xlim + (xlim2 - xlim)/2,lwd=0,line=1.2,cex.axis=1.2,tick=FALSE)
lines(density(histref1),col="steelblue4",lwd=4)
legend("topleft", c("Test Sites"), cex=1, 
       bty="n", fill=c("gray"),x.intersp=1.3)
legend("topleft", c("Reference Sites"), cex=1, 
       lty=c(1),merge=T,bty="n",col="steelblue4",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(30,.0425), c("Poor Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="red",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(30,.0426), c("Fair Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="yellow",lwd=4,x.intersp=1,y.intersp=1)

box(lty = '11', col = 'black')
box("outer",col="black")

ablineclip(v=0.267, col="red", lwd=3,y2=.033)
ablineclip(v=15.642, col="yellow",lwd=3,y2=.033)
#abline(h=0.0388, col="blue",lwd=3,)
#abline(h=.035, col="blue",lwd=3,)


#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################


#To create different graphs change the variables below. 
refcompare=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\BugModels\\EnvirOutliers\\NV_MMI_RreferenceScore.csv')
refcompare$variable=refcompare$x
dataset=read.csv('\\\\share1.bluezone.usu.edu\\miller\\buglab\\Research Projects\\BLM_WRSA_Stream_Surveys\\Results and Reports\\NorCal_2013\\Analysis\\BugModels\\All_BugModel_Results.csv')
dataset$variable=dataset$NV_MMI

yscalar= 0.65 # this is the scalar to adjust the height of the y axis
breaks=10 #Changes histogram "bins" 
xlim=10
xlim2=80

histref1=refcompare$variable
par(mai=c(1.3,1.3,1.3,0.42))
histman=hist(((dataset$variable)/1),ylab="Frequency",freq=FALSE,col="gray",main="  ",cex.axis=.9,cex.lab=1,xlab=" ",breaks=breaks,xlim=c(xlim,xlim2))
histmanmax=max(histman$density)
histmanmax2=histmanmax + histmanmax*yscalar
histman=hist(((dataset$variable)/1),ylab="Frequency",freq=FALSE,col="gray",main="Macroinvertebrate MMI Results",cex.axis=1.,cex.lab=1.3,xlab=" ",xlim=c(xlim,xlim2),breaks=breaks,ylim=c(0,histmanmax2))

labels=c("MMI score")
axis(1,labels=c(labels),at=xlim + (xlim2 - xlim)/2,lwd=0,line=1.2,cex.axis=1.2,tick=FALSE)
lines(density(histref1),col="steelblue4",lwd=4)
legend("topleft", c("Test Sites"), cex=1, 
       bty="n", fill=c("gray"),x.intersp=1.3)
legend("topleft", c("Reference Sites"), cex=1, 
       lty=c(1),merge=T,bty="n",col="steelblue4",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(7.2,.053), c("Degraded Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="red",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(7.2,.053), c("Undetermined Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="yellow",lwd=4,x.intersp=1,y.intersp=1)

box(lty = '11', col = 'black')
box("outer",col="black")

ablineclip(v=44.6, col="red", lwd=3,y1=0, y2=.063)
ablineclip(v=47.0, col="yellow",lwd=3,y1=0, y2=.063)
#abline(h=1.755, col="blue",lwd=3,)
#abline(h=2.09, col="blue",lwd=3,)












#####################################################################################
#####################################################################################
#Trying to split the graphs
#####################################################################################
#####################################################################################
RIP_NorthBR_ref=subset(RIP_RS_final, ECO_LVL_3NAME=='Northern Basin and Range')

#To create different graphs change the variables below. 
refcompare=RIP_NorthBR_ref
refcompare$variable=refcompare$XCMG
dataset=AquametCheck
dataset$variable=dataset$XCMG_CHECK

histref1=refcompare$variable
plot(-1,-1, xlim=c(0,2), ylim=c(0,2.35), ylab="Frequency",main="Riparian Habitat Complexity Results",cex.axis=1.,cex.lab=1.3,xlab='Worst Condition <-------------------------> Best Condition', breaks=10)
lines(density(histref1),col="steelblue4",lwd=4)

box(lty = '11', col = 'black')
box("outer",col="black")

legend("topleft", c("Reference Sites"), cex=1, 
       lty=c(1),merge=T,bty="n",col="steelblue4",lwd=4,x.intersp=1,y.intersp=4)

ablineclip(v=0.579, col="red", lwd=3,y2=1.7)
ablineclip(v=0.997, col="yellow",lwd=3,y2=1.7)

legend(xy.coords(.6,2.42), c("Poor Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="red",lwd=4,x.intersp=1,y.intersp=4)
legend(xy.coords(.6,2.43), c("Fair Threshold"), cex=1, 
       lty=c(1),merge=T,bty="n",col="yellow",lwd=4,x.intersp=1,y.intersp=1)






