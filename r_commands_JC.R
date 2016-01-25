help()
? 
#gives help for any function

source()
Sink()
# runs code from external source or stores output in an external source

objects() # calls all objects in workspaces
rm() # removes all objects listed from workspace
rm(list=ls()) # removes all objects in workspace
ls() # lists all objects in workspace

crt + L # clears console

read.table() # sep=  "\t" specifies tabs as seperator and gives NA fdr missing data
read.csv() # reads csv files
str() # strucure of data

x<- 0:10 # assigns number 0 through 10 to x
x1<-seq(10) # counts from 1 to 10
X4<-seq(30,0, by =-3) # counts down by 3
X5<-c(5,4,2) # concatenate
x6<-scan() # hit return after entering each number and return twice to stop
is.na(x)

?datasets # package called datasets
data() # see list of all built in datasets

margin.table()#get marginal frequencies from original table
plot() # makes appropriate plor for data
barplot(x) # needs summary of data but can control plot more than plot
par(oma=c(1,1,1,1)) # sets outside margins
par(mar= c(1,1,1)) # sets plot margins
\n new line and space
pie() # pie chart
hist() #histogram
boxplot() # boxplot
boxplot.stats()# numbers that go into box plot
summary() summary stats
psych # package that has
prop.table() #proportions
round() # rounds
scale() # M=0, SD=1
ggplot(vegLengths, aes(length, fill = veg)) + geom_density(alpha = 0.2)

par(mfrow=c(2,2))
plot(Year,Max7day,type="n",bty='n',xlab="",ylab="Max 7-day mean discharge (cfs)")
lines(Year,Max7day)
box(bty="l",lwd=2)
plot(Year,hgt6000,type="n",bty='n',xlab="",ylab="Total hours with discharge >6000 cfs")
lines(Year,hgt6000)
box(bty="l",lwd=2)
plot(Year,mean_dcv,type="n",bty='n',xlab="",ylab="Mean daily CV")
lines(Year,mean_dcv)
box(bty="l",lwd=2)
plot(Year,ds_6000,type="n",bty='n',xlab="",ylab="Number of days since discharge >6000 cfs")
lines(Year,ds_6000)
box(bty="l",lwd=2)


prop.test( 100, 200)
t.test(x, mu=4)
t.test(x,y, var.equal=T)
chisq.test(prop.table(eyes)) # expects equal distribution add (, p=c(3,4,5))
wilcox.test(x,y) #mann whitney
cor()#correlation matrix for data frame
cor.test(x,y) # pearson correlation for a pair
rcorr(as.matrix(dataframe))


#normality tests
ks.test(x,y)

& # boolean and
| #boolean OR
  
merge(dataframe1, dataframe2, by=c(), all=T)
aggregate(width ~ species) #~ function of
aggregate(cbind(petal.width,length)) #cbind column bind rbind row bind


getAnywhere()# gets background code for function
install.packages()
%s # place holder

length(which(v<7))# counts values in v that are less than 7 and excludes NAs

# make boxplots for every column in boxplotdata-LOOP
#Par makes your graphic have 2 rows of 6 boxplots
par(mfrow=c(2,6))
for (i in 1:length(boxplotdata)) {
  boxplot(boxplotdata[,i], main=names(boxplotdata[i]))
}
