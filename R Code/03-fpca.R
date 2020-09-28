
 # Ainhoa Elena Leger
 # University of Padova

 # This .R file contains the analyses of mortality data from Human Mortality Database (HMD) for males of 32 countries in the 1960-2010 period.
 # A Functional Principal Component Analysis is performed with the aim to synthetize the variability of the curves. The effects of the harmonics 
 # as well as the first principal subspace give an understanding of the evolution of mortality curves, while the reconstruction of smoothed curves
 # turns out useful for the evaluation of the appropriate number of principal components to consider.
 # The analysis are showed for males, but can repeated in the same way for females.

 # The following codes were executed with R version 3.5.2.
 # I used the R package fda of Ramsay et al. (2011) for the functional representation of data and the Functional Principal Component Analysis.


 #----------------------------------------------------#
 #	FUNCTIONAL PRINCIPAL COMPONENT ANALYSIS		#
 #----------------------------------------------------#

 library(fda)

 # We can conduct a fPCA through
 PCA.countries = pca.fd(new.fd,nharm=6)

 # Effect of the harmonics with respect to the mean function 
 #------------------------------------------------------------

 # Setting for the underlying grid
 h = c(0,0.005,0.01,0.015,0.02,0.025,0.03,0.035,0.04)
 v = c(0,20,40,60,80,100)

 # Obtain the mean function
 m.countries = mean.fd(new.fd)

 # Effect of the first harmonic
 par(par(mfrow=c(1,2)),mar = c(2.5,4.5,3,0.5))
 plot(PCA.countries,harm=1)
 abline(h=h, v=v, lty=2, col="lightgray")

 # And the second harmonic
 plot(PCA.countries,harm=2)
 abline(h=h, v=v, lty=2, col="lightgray")

 # Principal subspace for a subset of countries
 #------------------------------------------------

 # Selecting 7 countries in intervals of 10 years and their last available year 
 # Czech Republic, Denmark, France, Japan, Russia, Sweden, USA
 # look for the right countries and years
 fd.names <- new.fd$fdnames$reps
 # select them
 k <- 10
 select5<-c(seq(410,468,k),468, seq(585,643,k),643, seq(820,877,k),877,
		seq(1109,1167,k),1167, seq(1574,1628,k),1628,
		seq(1687,1745,k),1745, seq(1800,1857,k),1857)
 # check them
 fd.names[select5]

 # Create a colour variable for cluster membership
 col="black"
 col[res5$class[select5]==1]="gold"
 col[res5$class[select5]==2]="cyan"
 col[res5$class[select5]==3]="cornflowerblue"
 col[res5$class[select5]==4]="powderblue"
 col[res5$class[select5]==5]="orange"

 # Principal subspace 
 par(par(mfrow=c(1,1)),mar = c(4.5,4.5,1,1))
 plot(PCA.countries$scores[select5,1],PCA.countries$scores[select5,2],
	col=col,pch=20,cex=1,xlim=c(-0.087,0.064),ylim=c(-0.05,0.026),
	xlab="FPCA - First component",ylab="FPCA - Second component")
 abline(h=0); abline(v=0)
 text(PCA.countries$scores[select5,1],PCA.countries$scores[select5,2],pos=3,
	as.character(fd.names)[select5],col=col,cex=1.1,font=2)

 # Evolution lines 
 ptin <- c(1,8,15,22,29,36,43,50)
 for(i in ptin){
   x<-PCA.countries$scores[select5[i:(i+6)],1]
   f_x<-PCA.countries$scores[select5[i:(i+6)],2]
   lines(x,f_x,xlim=range(x),ylim=range(f_x),col="black")
   }

 # Legend
 legend("topright",c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5"),
	pch=20,cex=0.9,bty="n",col=c("powderblue","cyan","cornflowerblue","yellow","orange"))
 legend("topleft",c("II"),bty="n")
 legend("bottomleft",c("III"),bty="n")
 legend("bottomright",c("IV"),bty="n")

 # Principal subspace for all countries 
 #----------------------------------------

 # Length of time period for each country
 len.period <- rep(NA, 32)
 countries <- c("AUS","AUT","BEL","BGR","BLR","CAN","CHE","CZE","DEUTE","DEUTW",
	"DNK","ESP","EST","FIN","FRA","GBR","HUN","IRL","ITA","JPN","LTU","LVA",
	"NLD","NOR","NZL","POL","PRT","RUS","SVK","SWE","UKR","USA")
 for(i in 1:32){ len.period[i] <- sum(str_count(fd.names, countries[i])) }
 # Starting and ending values of time period for each country
 start <- cumsum(c(1,len.period))[-33]
 end <- cumsum(len.period)
 # Selecting 7 time points for every country (10 years interval + last year available)
 k <- 10
 select5 <- rep(NA,224)
 ind <- seq(1,length(fd.names),7)
 for(i in 1:32){ select5[ind[i]:(ind[i+1]-1)] <- c(seq(start[i],end[i],k), end[i]) }

 # Create a colour variable for cluster membership
 col="black"
 col[res5$class[select5]==1]="gold"
 col[res5$class[select5]==2]="cyan"
 col[res5$class[select5]==3]="cornflowerblue"
 col[res5$class[select5]==4]="powderblue"
 col[res5$class[select5]==5]="orange"

 # First principal subspace
 par(par(mfrow=c(1,1)),mar = c(4.5,4.5,1,1))
 plot(PCA.countries$scores[select5,1],PCA.countries$scores[select5,2],
	col=col,pch=20,cex=1,xlim=c(-0.089,0.066),ylim=c(-0.051,0.033),
	xlab="FPCA - First component",ylab="FPCA - Second component")
 abline(h=0); abline(v=0)
 text(PCA.countries$scores[select5,1],PCA.countries$scores[select5,2],pos=3,
	as.character(fd.names)[select5],col=col,cex=1.1)

 # Legend
 legend("topright",c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5"),
	pch=20,cex=0.9,bty="n",col=c("powderblue","cyan","cornflowerblue","yellow","orange"))
 legend("topleft",c("II"),bty="n")
 legend("bottomleft",c("III"),bty="n")
 legend("bottomright",c("IV"),bty="n")

 # Reconstruction of curves through functional principal components
 #-------------------------------------------------------------------

 # Scores and harmonics
 scores = PCA.countries$scores
 PCs = PCA.countries$harmonics

 # France 2017
  x=877

 # Mean and smoothed curve
 par(mfrow=c(1,1),mar = c(4.5,4.5,0.5,0.5))
 plot(m.countries,ylim=c(0,0.043),xlab="Age",ylab="Proportion of deaths",
	col="black",cex.lab=1.1,cex.axis=1.1,lwd=2)
 lines(new.fd[x],col="blue",lwd=2)
 h = c(0,0.01,0.02,0.03,0.04,0.05)
 v = c(20,40,60,80,100)
 abline(h=h, v=v, lty=2, col="lightgray")

 # Firstly, just the mean + PC1
 ex.r1 = m.countries + scores[x,1]*PCs[1]
 lines(ex.r1,col="green",lwd=2)
 # Try adding the second PC
 ex.r2 = m.countries + scores[x,1]*PCs[1] + scores[x,2]*PCs[2]
 lines(ex.r2,col="gold",lwd=2)
 # Try using 6 PC
 ex.r6 = m.countries + scores[x,1]*PCs[1] + scores[x,2]*PCs[2] + 
		scores[x,3]*PCs[3] + scores[x,4]*PCs[4] + 
		scores[x,5]*PCs[5] + scores[x,6]*PCs[6]
 lines(ex.r6,col="red",lty=6,lwd=2)

 legend("topleft",c("Curve of France in 2017","Mean curve",
	"Mean curve + PC1","Mean curve + PC1 + PC2",
	"Mean curve + PC1 + PC2 + ...+ PC6"),pch=20,cex=0.9,bty="n",
	col=c("blue","black","green","gold","red"))

 # Comparisons between curves
 #--------------------------------

 # Select Sweden 2010 and France 2017
 x=1737 # Sweden
 y=877 # France

 # Plot smoothed curves
 par(mfrow=c(1,1),mar = c(4.5,4.5,0.5,0.5))
 plot(new.fd[x],ylim=c(0,0.043),xlab="Age",ylab="Proportion of deaths",
	col="black",cex.lab=1.1,cex.axis=1.1,lwd=2)
 lines(new.fd[y],col="red",lwd=2)
 h = c(0,0.01,0.02,0.03,0.04,0.05)
 v = c(20,40,60,80,100)
 abline(h=h, v=v, lty=2, col="lightgray")
 legend("topleft",c("Curve of Sweden in 2010","Curve of France in 2017"),
	pch=20,cex=0.9,bty="n",col=c("black","red"))

 # Select Czech 1990 and Russia 1990
 x=440 # Czech
 y=1604 # Russia

 # Plot smoothed curves
 par(mfrow=c(1,1),mar = c(4.5,4.5,0.5,0.5))
 plot(new.fd[x],col="black",xlab="Age",ylab="Proportion of deaths",
	col="black",cex.lab=1.1,cex.axis=1.1,lwd=2)
 lines(new.fd[y],col="red",lwd=2)
 h = c(0,0.01,0.02,0.03,0.04,0.05)
 v = c(20,40,60,80,100)
 abline(h=h, v=v, lty=2, col="lightgray")
 legend("topleft",c("Curve of Czech in 1990","Curve of Russia in 1990"),
	pch=20,cex=0.9,bty="n",col=c("black","red"))




