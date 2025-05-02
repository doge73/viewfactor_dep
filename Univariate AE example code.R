library(readxl)
library(OpenMx)
library(psych); library(polycor)
mxOption(NULL, "Default optimizer", "SLSQP")

#code for imput data


##univariate twin modeling
vars      <- c('variable of interest' )
nv        <- 1                         # number of variables
ntv       <- nv*2                      # number of total variables
selVars   <- paste(vars,c(rep(1,nv),rep(2,nv)),sep="")
covVars <-c('age1', 'age2') # remember to replace to your own age variable name 

mzData  <- as.data.frame(subset(twinpair, ZYG==1, c(selVars, covVars)))
dzData  <- as.data.frame(subset(twinpair, ZYG==2, c(selVars, covVars)))

meanG <- mxMatrix( type="Full", nrow=1, ncol=2, free=TRUE, values=4, labels="mean", name="meanG")

# create matrices and algebra for covariates 
defAge    <- mxMatrix( type="Full", nrow=1, ncol=2, free=FALSE, labels=c('data.age1', 'data.age2'), name="defAge" ) # remember to replace to your own age variable name 
pathB     <- mxMatrix( type="Full", nrow=1, ncol=1, free=TRUE, values=.01, label=c("beta1"), name="b" )
means2   <- mxAlgebra( expression= meanG + (b%*%defAge), name="means2" )

# Create Matrices for Variance Components
covA      <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=TRUE, values=10, label="VA11", name="VA") 
covC      <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=TRUE, values=6, label="VC11", name="VC")
covE      <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=TRUE, values=10, label="VE11", name="VE")

# Create Algebra for expected Variance/Covariance Matrices in MZ & DZ twins
covP      <- mxAlgebra( expression= VA+VC+VE, name="V" )
covMZ     <- mxAlgebra( expression= VA+VC, name="cMZ" )
covDZ     <- mxAlgebra( expression= 0.5%x%VA+ VC, name="cDZ" )
expCovMZ  <- mxAlgebra( expression= rbind( cbind(V, cMZ), cbind(t(cMZ), V)), name="expCovMZ" )
expCovDZ  <- mxAlgebra( expression= rbind( cbind(V, cDZ), cbind(t(cDZ), V)), name="expCovDZ" )

SA <- mxAlgebra(VA/V, name='SA')
SC <- mxAlgebra(VC/V, name='SC')
SE <- mxAlgebra(VE/V, name='SE')

# Create Data Objects for Multiple Groups
dataMZ    <- mxData( observed=mzData, type="raw" )
dataDZ    <- mxData( observed=dzData, type="raw" )

# Create Expectation Objects for Multiple Groups
expMZ     <- mxExpectationNormal( covariance="expCovMZ", means="means2", dimnames=selVars )
expDZ     <- mxExpectationNormal( covariance="expCovDZ", means="means2", dimnames=selVars )
funML     <- mxFitFunctionML()

# Create Model Objects for Multiple Groups
pars      <- list( meanG, defAge, pathB, means2,covA, covC, covE, covP, SA, SC, SE)
modelMZ   <- mxModel( pars, covMZ, expCovMZ, dataMZ, expMZ, funML, name="MZ" , mxCI(c("SA", "SC", "SE")))
modelDZ   <- mxModel( pars, covDZ, expCovDZ, dataDZ, expDZ, funML, name="DZ" )
multi     <- mxFitFunctionMultigroup( c("MZ","DZ") )

# Build Model with Confidence Intervals: ACE model
modelACE  <- mxModel( "oneACEvc", modelMZ, modelDZ, multi)
fitACE    <- mxRun( modelACE, intervals=T  )
sumACE    <- summary( fitACE, verbose=T  )


# Run AE model
modelAE   <- mxModel( fitACE, name="oneAEvc" )
modelAE   <- omxSetParameters( modelAE, labels="VC11", free=FALSE, values=0 )
fitAE     <- mxRun( modelAE, intervals=T )
sumAE     <- summary(fitAE)

# Run CE model
modelCE   <- mxModel( fitACE, name="oneCEvc" )
modelCE   <- omxSetParameters( modelCE, labels="VA11", free=FALSE, values=0 )
fitCE     <- mxRun( modelCE, intervals=T )
sumCE     <- summary(fitCE)

# Run E model
modelE    <- mxModel( fitAE, name="oneEvc" )
modelE    <- omxSetParameters( modelE, labels="VA11", free=FALSE, values=0 )
fitE      <- mxRun( modelE, intervals=T )
sumE     <- summary(fitE)


#Model performance comparison
mxCompare(fitACE, nested <- c(fitAE, fitCE, fitE))

#ACE model summary
sumACE 

#AE model summary
sumAE 

#CE model summary
sumCE 

#E model summary
sumE 