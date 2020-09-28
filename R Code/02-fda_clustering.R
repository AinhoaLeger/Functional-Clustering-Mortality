
 # Ainhoa Elena Leger
 # University of Padova

 # This .R file contains the analyses of mortality data from Human Mortality Database (HMD) for 32 countries in the 1960-2018 period.
 # The program needs the functional data object resulting from the smoothing of the curves.
 # The three methods of Functional Clustering are carried out, reflecting the three main approaches for functional clustering:
 # (1) two-stages approach on the coefficients of basis expansion of the curves,
 # (2) model-based approach with the FLM model, 
 # (3) distance-based approach through semimetric based on FPCA.
 # The analysis are showed for males, but can repeated in the same way for females.

 # The following codes were executed with R version 3.5.2.
 # I used the R package funHDDC of Bouveyron and Jacques (2014) to perform model-based method, 
 # and the R package fda.usc of De la Fuente and Febrero-Bande (2011) for distance-based approach.


 #----------------------------------------------------#
 #	FUNCTIONAL CLUSTER ANALYSIS		#
 #----------------------------------------------------#

 library(fda)

 # Settings for image plot
 countries <- c("AUS","AUT","BEL","BGR","BLR","CAN","CHE","CZE","DEUTE","DEUTW","DNK","ESP","EST","FIN","FRA","GBR",
	"HUN","IRL","ITA","JPN","LTU","LVA","NLD","NOR","NZL","POL","PRT","RUS","SVK","SWE","UKR","USA")
 period <- 1960:2018
 list <- c("DNK","FIN","NOR","SWE","NLD","AUT","BEL","CHE","DEUTE","DEUTW","FRA","GBR","IRL","ESP","ITA","PRT",
		"BGR","CZE","HUN","POL","SVK","BLR","EST","LTU","LVA","RUS","UKR","CAN","USA","AUS","NZL","JPN")

 # Settings for curves in clusters
 library(stringr)
 h = c(0.01,0.02,0.03,0.04,0.05)
 v = c(20,40,60,80,100)

 # Model-based cluster analysis 
 #--------------------------------------------------------
 # In a first step the curves are classified in k=5 clusters with the model-based approach
 # Not all the countries have available data until 2018, so we add those years at the end of the period (value=0)
 # We save in a matrix the results and plot it through the function myImagePlot

 library(funHDDC)

 # Model AkjBQkDk
 set.seed(5555)
 res5 = funHDDC(new.fd,K=5,model="AkjBQkDk",init="kmeans")
 #      model K threshold complexity        BIC
 # 1 AKJBQKDK 5       0.2        493 -22,057.17
 ##### Takes approximately 15 minutes #####

 # Create a vector containing cluster membership
 mb.vec <- res5$class
 # Length of time period for each country
 len.period <- rep(NA, 32)
 fd.names <- new.fd$fdnames$reps
 for(i in 1:32){ len.period[i] <- sum(str_count(fd.names, countries[i])) }
 # Starting and ending values of time period for each country
 start <- cumsum(c(1,len.period))[-33]
 end <- cumsum(len.period)
 # Matrix containing cluster membership for every country and year
 M.mb <- matrix(0,32,59)
 for(i in 1:32){ M.mb[i,1:len.period[i]] <- mb.vec[start[i]:end[i]] }
 rownames(M.mb) <- countries
 colnames(M.mb) <- period

 # Image Plot
 # Change order of countries
 M.mb2 <- M.mb[list, ]
 # Change order of clusters to have the desired order of colors
 colors <- c("powderblue","cyan","cornflowerblue","yellow","orange")
 M.mb3 <- matrix(0,32,59) 
 M.mb3[M.mb2 == 1] <- 4
 M.mb3[M.mb2 == 2] <- 2
 M.mb3[M.mb2 == 3] <- 3
 M.mb3[M.mb2 == 4] <- 1
 M.mb3[M.mb2 == 5] <- 5
 rownames(M.mb3) <- list
 colnames(M.mb3) <- period
 source("myImagePlot.R")
 myImagePlot(M.mb3,colors)

 # Model-based cluster analysis - curves in clusters
 #--------------------------------------------------------

 # rename classification object to comply with "selectyears" code
 GF <- res5$class
 # order of clusters in the cycle
 order <- c(5,1,3,2,4)
 # define colors (following order of clusters in the cycle)
 col <- c("yellow","cyan","cornflowerblue","powderblue","orange")

 par(mfrow=c(1,2), mar=c(4.5,4.5,0.5,0.5))
 # plot curves in clusters
 plot(new.fd[1],ylim=c(0,0.047),col="white",xlab="Age",ylab="Proportion of deaths")
 abline(h=h, v=v, lty=2, col="lightgray")
 for(k in order){ 
	source("selectyears.R")
	lines(new.fd[which(res5$class==k)],col=col[k])
	}
 legend(3,0.047,c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5"),
	pch=20,bty="n",col=c("powderblue","cyan","cornflowerblue","yellow","orange"))

 # plot mean curves in clusters
 plot(new.fd[1],ylim=c(0,0.047),col="white",xlab="Age",ylab="Proportion of deaths",main="")
 abline(h=h, v=v, lty=2, col="lightgray")
 for(k in order){
	select<-fd(new.fd$coefs[,which(res5$class==k)], new.fd$basis)
	lines(mean.fd(select),lty=1,lwd=3,col=col[k])
	}
 legend(3,0.047,c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5"),
	pch=20,bty="n",col=c("powderblue","cyan","cornflowerblue","yellow","orange"))

 # Two-stages cluster analysis 
 #-----------------------------------------------------

 # Cluster analysis on spline coefficients
 coef.countries <- t(new.fd$coefs)
 coef.dist <- dist(coef.countries,method="euclidean") 
 countries.clust <- hclust(coef.dist,method="ward.D")
 coef.clust <- cutree(countries.clust,k=5)

 # The matrix of group membership can be created as before from the results
 # of the cluster analysis and then used in the function myImagePlot 
 # The curves in clusters can be also plotted as shown above

 # Distance-based cluster analysis
 #--------------------------------------------------------

 library(fda.usc)

 # fda.usc requires a functional object of type fdata
 age.countries <- 0:110
 agerng.countries = range(age.countries)
 knots.countries <- c(seq(0,2,0.25),seq(5,49.5,5),seq(50,89.5,2),seq(90,109.5,5),110)
 norder.countries = 4
 nbasis.countries = length(knots.countries) + norder.countries - 2
 basis.countries = create.bspline.basis(agerng.countries,nbasis.countries,norder.countries,knots.countries)
 new <- Data2fd(0:110,y=dx.countries,basisobj=basis.countries)
 new.fdata <- fdata(new,argvals=knots.countries,rangeval=c(0,110))

 # Cluster analysis with semimetric.pca
 sem.pca <- semimetric.pca(new.fdata,new.fdata,q=6)
 countries.clust <- hclust(as.dist(sem.pca),method="ward.D")
 pca.clust <- cutree(countries.clust,k=5)

 # The matrix of group membership can be created as before from the results
 # of the cluster analysis and then used in the function myImagePlot 
 # The curves in clusters can be also plotted as shown above



