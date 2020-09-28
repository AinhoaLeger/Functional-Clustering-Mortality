
 # Ainhoa Elena Leger
 # University of Padova

 # This .R file contains the code for the functional representation of mortality data from Human Mortality Database (HMD). 
 # The program reads the lifetables extracted from HMD for males of 32 countries in the period 1960-2018 and obtain 
 # functional data with a B-spline approximation. 
 # The procedure is showed for males, but can repeated in the same way for females.

 # The following codes were executed with R version 3.5.2.
 # I used the R package fda of Ramsay et al. (2011) for functional representation data analysis.


 #----------------------------------------------------#
 #       	  HMD DATA  	                  #
 #----------------------------------------------------#

 # Save lifetables for selected countries from HMD
 lf.AUS <- read.table("AUS.txt",header=TRUE)
 # ... same for all 32 countries

 # Extract and save deaths counts dx over age interval [0,110] for every country and year in period 1960-2018
 # Calculate proportion of deaths
 fun.dx <- function(lf.country){
  yfin <- max(lf.country$Year)
  indx<-c(1960:yfin)
  nfin <- length(unique(lf.country$Year[lf.country$Year>=1960]))
  result=matrix(NA,111,nfin)
   for(i in 1:nfin){
	result[,i]<-lf.country[lf.country$Year==indx[i],
	]$dx/sum(lf.country[lf.country$Year==indx[i],]$dx)
	}
  colnames(result)<-c(1960:yfin)
  row.names(result)<-c(0:110)
  return(result)
  }

 dx.AUS <- fun.dx(lf.AUS)
 # ... same for all 32 countries

 # Save distributions of deaths for every country in a matrix
 dx.countries <- cbind(dx.AUS,dx.AUT,dx.BEL,dx.BGR,dx.BLR,dx.CAN,dx.CHE,
	dx.CZE,dx.DEUTE,dx.DEUTW,dx.DNK,dx.ESP,dx.EST,dx.FIN,dx.FRA,dx.GBR,
	dx.HUN,dx.IRL,dx.ITA,dx.JPN,dx.LTU,dx.LVA,dx.NLD,dx.NOR,dx.NZL,
	dx.POL,dx.PRT,dx.RUS,dx.SVK,dx.SWE,dx.UKR,dx.USA)

 # Rename column by country and year
 colnames(dx.countries) <- 
  c(sprintf("AUS%d", 1960:max(lf.AUS$Year)), ...)


 #----------------------------------------------------#
 #	CONSTRUCTION OF FUNCTIONAL DATA OBJECT          #
 #----------------------------------------------------#

 library(fda)

 # Define a basis system of natural cubic splines with a sequence of 31 knots
 age.countries <- 0:110
 agerng.countries = range(age.countries)
 knots.countries <- c(seq(0,2,0.25),seq(5,49.5,5),seq(50,89.5,5),seq(90,109.5,5),110)
 length(knots.countries)
 norder.countries = 4
 nbasis.countries = length(knots.countries) + norder.countries - 2
 basis.countries = create.bspline.basis(agerng.countries,nbasis.countries,norder.countries,knots.countries)

 # Define smoothing functions
 curv.Lfd.countries = int2Lfd(2)
 lambda = 100
 curv.fdPar.countries = fdPar(basis.countries,curv.Lfd.countries,lambda)

 # Let's define a sequence of lamdba values
 lambdas = 10^seq(-3,1,by=0.1)

 # Select smoothing parameter through Generalized Cross Validation (GCV) criterion
 mean.gcv = matrix(0,dim(dx.countries)[2],length(lambdas))
 for(icount in 1:dim(dx.countries)[2]){
	for(ilam in 1:length(lambdas)){
	  curv.fdPari = curv.fdPar.countries
	  curv.fdPari$lambda = lambdas[ilam]
		Smoothi = smooth.basis(age.countries,dx.countries[ ,icount],curv.fdPari)
	  mean.gcv[icount,ilam] = mean(Smoothi$gcv)
	  }
	}
 ##### Takes approximately 15 minutes #####
 
 # Let's select the lowest of these and smooth
 gcv<-matrix(0,1,dim(dx.countries)[2])
 for(i in 1:dim(dx.countries)[2]){
	best = which.min(mean.gcv[i,])
	gcv[1,i] = lambdas[best]
	}
 row.names(gcv) <- c("Lambda")
 colnames(gcv) <- colnames(dx.countries)

 # Functional object
 bs.coef <- matrix(0,33,dim(dx.countries)[2])
 for(i in 1:dim(dx.countries)[2]){
	curv.fdPar.countries$lambda = gcv[i]
	Smooth.countries = smooth.basis(age.countries,dx.countries[,i],curv.fdPar.countries)
	bs.coef[,i] <- Smooth.countries$fd$coefs
	}
 row.names(bs.coef) <- c(sprintf("bspl4.%d", 1:33))
 colnames(bs.coef) <- colnames(dx.countries)

 new.fd = fd(bs.coef,Smooth.countries$fd$basis)
 new.fd$fdnames$reps <- colnames(dx.countries)





