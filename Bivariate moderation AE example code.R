library(readxl)
library(OpenMx)
library(umx)
library(psych)
library(ggplot2)
mxOption(NULL, "Default optimizer", "SLSQP")
set.seed(31920251)

#code for imput data

twinpair<-umx_residualize(c("dep", "visibility factor"), covs = "age", suffixes = c("1", "2"), data=twinpair)
#use the residual for adjusting covariates

vars<-"dep"
mods<-"visibility factor"
nv        <- 1                         # number of variables
ntv       <- nv*2                      # number of total variables
selDVs   <- paste(vars,c(rep(1,nv),rep(2,nv)),sep="") ##outcome
selDefs   <-paste(mods,c(rep(1,nv),rep(2,nv)),sep="") ##moderator

mzData  <- as.data.frame(subset(twinpair, ZYG==1, c(selDVs, selDefs)))
dzData  <- as.data.frame(subset(twinpair, ZYG==2, c(selDVs, selDefs)))

selVars = c(selDefs[1], selDVs[1], selDefs[2], selDVs[2]) #MN

dzData = dzData[ , selVars]
mzData = mzData[ , selVars]
mzData = xmu_data_missing(mzData, selVars = selDefs, dropMissingDef=dropMissingDef, hint="mzData")
dzData = xmu_data_missing(dzData, selVars = selDefs, dropMissingDef=dropMissingDef, hint="dzData")


model = mxModel("GxEbiv", 
                mxModel("top",
                        umxMatrix("a11"   , "Lower", nrow = 1, ncol = 1, free = TRUE, values = .6), 
                        umxMatrix("c11"   , "Lower", nrow = 1, ncol = 1, free = FALSE, values = 0), 
                        umxMatrix("e11"   , "Lower", nrow = 1, ncol = 1, free = TRUE, values = .6),
                        umxMatrix("a21"   , "Lower", nrow = 1, ncol = 1, free = TRUE, values = .6), 
                        umxMatrix("c21"   , "Lower", nrow = 1, ncol = 1, free = FALSE, values = 0),
                        umxMatrix("e21"   , "Lower", nrow = 1, ncol = 1, free = TRUE, values = .6),
                        umxMatrix("a22"   , "Lower", nrow = 1, ncol = 1, free = TRUE, values = .6),
                        umxMatrix("c22"   , "Lower", nrow = 1, ncol = 1, free = FALSE, values = 0),
                        umxMatrix("e22"   , "Lower", nrow = 1, ncol = 1, free = TRUE, values = .6),
                        umxMatrix("aBeta1", "Lower", nrow = 1, ncol = 1, free = TRUE, values = .0), 
                        umxMatrix("cBeta1", "Lower", nrow = 1, ncol = 1, free = FALSE, values = 0),
                        umxMatrix("eBeta1", "Lower", nrow = 1, ncol = 1, free = TRUE, values = .0),	
                        umxMatrix("aBeta2", "Lower", nrow = 1, ncol = 1, free = TRUE, values = .0),
                        umxMatrix("cBeta2", "Lower", nrow = 1, ncol = 1, free = FALSE, values = 0),
                        umxMatrix("eBeta2", "Lower", nrow = 1, ncol = 1, free = TRUE, values = .0),
                        # Assemble Cholesky decomposition for twin 1 and twin 2 
                        umxMatrix("PsAmz",   "Symm", nrow = 4, ncol = 4, free = FALSE, values = c(1,  0, 1.0, 0, 1, 0,  1.0, 1, 0, 1)), 
                        umxMatrix("PsAdz",   "Symm", nrow = 4, ncol = 4, free = FALSE, values = c(1,  0, 0.5, 0, 1, 0,  0.5, 1, 0, 1)), 
                        umxMatrix("PsC",     "Symm", nrow = 4, ncol = 4, free = FALSE, values = c(1,  0, 1.0, 0, 1, 0,  1.0, 1, 0, 1)), 
                        umxMatrix("expMean", "Full", nrow = 1, ncol = 4, free = TRUE,  values = 0, labels = c("m_mod", "m_trait", "m_mod", "m_trait"))
                ),
                mxModel("MZ", 
                        # Matrices generated to hold A and E computed Variance Components
                        # This is a Cholesky decomposition of A for twin 1 and twin 2 (mz and dz) 
                        # note that mod1 appears in the top left part (twin 1) and mod2 in the bottom right part (twin 2)
                        # Definition variables to create moderated paths (M -> T)
                        umxMatrix("mod1", "Full", nrow = 1, ncol = 1, free = FALSE, labels = paste0("data.", selDefs[1])), 
                        umxMatrix("mod2", "Full", nrow = 1, ncol = 1, free = FALSE, labels = paste0("data.", selDefs[2])), 
                        mxAlgebra(name = "chA", 
                                  rbind(
                                    cbind(top.a11,                         0,                               0,       0),
                                    cbind(top.a21 + (mod1 %x% top.aBeta1), top.a22 + (mod1 %x% top.aBeta2), 0,       0), 
                                    cbind(0,                               0,                               top.a11, 0), 
                                    cbind(0,                               0,                               top.a21 + (mod2 %x% top.aBeta1), top.a22 + (mod2 %x% top.aBeta2)))
                        ), 
                        mxAlgebra(name = "chC", 
                                  rbind(
                                    cbind(top.c11,                                                       0,                               0,                              0), 
                                    cbind(top.c21 + (mod1 %x% top.cBeta1), top.c22 + (mod1 %x% top.cBeta2),                               0,                              0), 
                                    cbind(0,                                                             0,                         top.c11,                              0), 
                                    cbind(0,                                                             0, top.c21 + (mod2 %x% top.cBeta1), top.c22 + (mod2 %x% top.cBeta2)))
                        ), 
                        mxAlgebra(name = "chE", 
                                  rbind(
                                    cbind(                        top.e11,                               0, 0, 0), 
                                    cbind(top.e21 + (mod1 %x% top.eBeta1), top.e22 + (mod1 %x% top.eBeta2), 0, 0), 
                                    cbind(                              0,                               0, top.e11, 0), 
                                    cbind(                              0,                               0, top.e21 + (mod2 %x% top.eBeta1), top.e22 + (mod2 %x% top.eBeta2)))
                        ), 
                        mxAlgebra(name = "Amz", chA %&% top.PsAmz), 
                        mxAlgebra(name = "C",   chC %&% top.PsC), 
                        mxAlgebra(name = "E",   chE %*% t(chE)), 
                        mxAlgebra(name = "expCovMZ", Amz + C + E), 
                        mxData(mzData, type = "raw"), 
                        mxExpectationNormal("expCovMZ", means = "top.expMean", dimnames = selVars), mxFitFunctionML()
                ),
                mxModel("DZ", 
                        umxMatrix("mod1", "Full", nrow = 1, ncol = 1, free = FALSE, labels = paste0("data.", selDefs[1])), # "data.defmod1"
                        umxMatrix("mod2", "Full", nrow = 1, ncol = 1, free = FALSE, labels = paste0("data.", selDefs[2])), # "data.defmod2"
                        mxAlgebra(name = "chA", 
                                  rbind(
                                    cbind(top.a11,                         0,                               0,       0),
                                    cbind(top.a21 + (mod1 %x% top.aBeta1), top.a22 + (mod1 %x% top.aBeta2), 0,       0), 
                                    cbind(0,                               0,                               top.a11, 0), 
                                    cbind(0,                               0,                               top.a21 + (mod2 %x% top.aBeta1), top.a22 + (mod2 %x% top.aBeta2)))
                        ), 
                        mxAlgebra(name = "chC", 
                                  rbind(
                                    cbind(top.c11,                                                       0,                               0,                              0), 
                                    cbind(top.c21 + (mod1 %x% top.cBeta1), top.c22 + (mod1 %x% top.cBeta2),                               0,                              0), 
                                    cbind(0,                                                             0,                         top.c11,                              0), 
                                    cbind(0,                                                             0, top.c21 + (mod2 %x% top.cBeta1), top.c22 + (mod2 %x% top.cBeta2)))
                        ), 
                        mxAlgebra(name = "chE", 
                                  rbind(
                                    cbind(                        top.e11,                               0, 0, 0), 
                                    cbind(top.e21 + (mod1 %x% top.eBeta1), top.e22 + (mod1 %x% top.eBeta2), 0, 0), 
                                    cbind(                              0,                               0, top.e11, 0), 
                                    cbind(                              0,                               0, top.e21 + (mod2 %x% top.eBeta1), top.e22 + (mod2 %x% top.eBeta2)))
                        ), 
                        mxAlgebra(name = "Adz", chA %&% top.PsAdz ), 
                        mxAlgebra(name = "C",   chC %&% top.PsC   ), 
                        mxAlgebra(name = "E",   chE %*% t(chE)    ), 
                        mxAlgebra(name = "expCovDZ", Adz + C + E  ), 
                        mxData(dzData, type = "raw"), 
                        mxExpectationNormal("expCovDZ", means = "top.expMean", dimnames = selVars), mxFitFunctionML()
                ),
                mxFitFunctionMultigroup(c("MZ", "DZ"))
)


model = omxSetParameters(model, labels = c("a11_r1c1",  "e11_r1c1","a22_r1c1", "e22_r1c1"), lbound=0)
model = as(model, "MxModelGxEbiv")
model = xmu_safe_run_summary(model)
summary(model)

result_ci<- summary(umxConfint(model, parm = "all", run = TRUE,optimizer = c("CSOLNP")), verbose=T )[["CI"]]

mzData2  = model$MZ$data$observed
dzData2  = model$DZ$data$observed
selDefs2 = names(mzData2)[3:4]

mzdef1 = as.vector(mzData2[, selDefs2[1]])
mzdef2 = as.vector(mzData2[, selDefs2[2]])
dzdef1 = as.vector(dzData2[, selDefs2[1]])
dzdef2 = as.vector(dzData2[, selDefs2[2]])
allValuesOfDefVar= c(mzdef1, mzdef2, dzdef1, dzdef2)
defVarValues = sort(unique(allValuesOfDefVar))

a11 = model$top.a11$values
a21 = model$top.a21$values
a22 = model$top.a22$values
Ba1 = model$top.aBeta1$values
Ba2 = model$top.aBeta2$values

e11 = model$top.e11$values
e21 = model$top.e21$values
e22 = model$top.e22$values
Be1 = model$top.eBeta1$values
Be2 = model$top.eBeta2$values	

Va  = (c(a21 + a22) + (defVarValues * c(Ba1 + Ba2)))^2
Ve  = (c(e21 + e22) + (defVarValues * c(Be1 + Be2)))^2
Vt  = Va + Ve

out    = as.matrix(cbind(Va, Ve, Vt))
outStd = as.matrix(cbind(Va/Vt, Ve/Vt))

tmp= data.frame(rbind(
  cbind(a11, NA , e11,  NA, Ba1, Be1),
  cbind(a21, a22, e21, e22, Ba2,  Be2))
)
names(tmp) = c("a1", "a2", "e1", "e2", "a_betas",  "e_betas")
umx_print(tmp, digits=2)

out<-data.frame(out)
outStd<-data.frame(outStd)
defVarValues<-data.frame(defVarValues)
plot.data_unstandardized<-cbind(defVarValues, out)
plot.data_standardized<-cbind(defVarValues, outStd)


