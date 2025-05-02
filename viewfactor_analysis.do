
//----data cleaning is hided due to privacy----//
//---------------------------------------------//


//----descriptive analysis----//
foreach var of varlist edu work marriage livingstatus PA_freq alcohol_freq smoking substance{
tab `var' 
}

foreach var of varlist edu work marriage livingstatus PA_freq alcohol_freq smoking substance{
tab `var' if SEX==1
}

foreach var of varlist edu work marriage livingstatus PA_freq alcohol_freq smoking substance{
tab `var' if SEX==2
}

foreach var of varlist edu work  livingstatus PA_freq alcohol_freq smoking substance{
tab `var' SEX, chi
}

foreach var of varlist  age dep deprivation_a {
summ `var'
 }
 
foreach var of varlist  age dep deprivation_a {
summ `var' if SEX==1
 }

foreach var of varlist  age dep deprivation_a {
summ `var' if SEX==2
 }

oneway age SEX
oneway dep SEX
oneway deprivation_a SEX

oneway sky SEX
oneway tree SEX
oneway building SEX

pnorm dep
pnorm sky
pnorm tree
pnorm building
//standardized normal probability (P-P) plot for normalizatION
//----------------------//

//----imputation for covariate----//
foreach var of varlist edu work alcohol_freq PA_freq smoking substance marriage livingstatus{
	summ `var',detail
	replace `var'=r(p50) if missing(`var')
}
//----------------------//

//----linear regression with sex stratification----//
reg dep sky i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a, cluster(family_nb)
reg dep tree  i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a, cluster(family_nb)
reg dep building  i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a, cluster(family_nb)

reg dep sky   age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==1, cluster(family_nb)
reg dep tree  age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==1, cluster(family_nb)
reg dep building   age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==1, cluster(family_nb)

reg dep sky   age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==2, cluster(family_nb)
reg dep tree  age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==2, cluster(family_nb)
reg dep building   age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==2, cluster(family_nb)


reg dep sky , cluster(family_nb)
reg dep tree ,cluster(family_nb)
reg dep building  , cluster(family_nb)


reg dep sky if SEX==1, cluster(family_nb)
reg dep tree if SEX==1,cluster(family_nb)
reg dep building  if SEX==1, cluster(family_nb)


reg dep sky if SEX==2, cluster(family_nb)
reg dep tree if SEX==2,cluster(family_nb)
reg dep building  if SEX==2, cluster(family_nb)
//----------------------//

//----within-pair linear regression with sex stratification----//
xtset family_nb

xtreg dep sky i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a,fe 
xtreg dep tree i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a,fe 
xtreg dep building i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a,fe 

xtreg dep sky i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==1,fe 
xtreg dep tree i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==1,fe 
xtreg dep building i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==1,fe 

xtreg dep sky i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==2,fe 
xtreg dep tree i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==2,fe 
xtreg dep building i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==2,fe 

xtreg dep sky,fe 
xtreg dep tree ,fe 
xtreg dep building ,fe 

xtreg dep sky if SEX==1,fe 
xtreg dep tree if SEX==1 ,fe 
xtreg dep building if SEX==1 ,fe 

xtreg dep sky if SEX==2,fe 
xtreg dep tree if SEX==2,fe 
xtreg dep building if SEX==2,fe 

//for MZ individual twins only
xtreg dep sky i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if ZYG==1,fe 
xtreg dep tree i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if ZYG==1,fe 
xtreg dep building i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if ZYG==1 ,fe 

xtreg dep sky  age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==1 & ZYG==1 ,fe 
xtreg dep tree age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==1 & ZYG==1 ,fe 
xtreg dep building age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==1 & ZYG==1 ,fe 

xtreg dep sky  age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==2 & ZYG==1,fe 
xtreg dep tree  age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==2 & ZYG==1,fe 
xtreg dep building  age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==2 & ZYG==1,fe 
//----------------------//


//----sensitivity analysis for comparison exposrues----//
replace  meantcd= meantcd/100
reg dep  meantcd i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a, cluster(family_nb)
reg dep  meantcd  age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==1, cluster(family_nb)
reg dep  meantcd  age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==2, cluster(family_nb)

replace built=built/100
reg dep  built i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a, cluster(family_nb)
reg dep  built  age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==1, cluster(family_nb)
reg dep  built  age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if SEX==2, cluster(family_nb)
//----------------------//


//code for exporting

//----sensitivity by stratification of ykr urban level----//
//code for importing data

reg dep c.sky##i.city_ykr i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a, cluster(family_nb)
reg dep c.tree##i.city_ykr  i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a, cluster(family_nb)
reg dep c.building##i.city_ykr  i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a, cluster(family_nb)


reg dep sky i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if city_ykr==1, cluster(family_nb)
reg dep tree  i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a  if city_ykr==1, cluster(family_nb)
reg dep building  i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a  if city_ykr==1, cluster(family_nb)


reg dep sky i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if city_ykr==0, cluster(family_nb)
reg dep tree  i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a  if city_ykr==0, cluster(family_nb)
reg dep building  i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a  if city_ykr==0, cluster(family_nb)


reg dep sky i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if city_ykr==1 & SEX==1, cluster(family_nb)
reg dep tree  i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a  if city_ykr==1  & SEX==1, cluster(family_nb)
reg dep building  i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a  if city_ykr==1  & SEX==1, cluster(family_nb)

reg dep sky i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a if city_ykr==0 & SEX==1, cluster(family_nb)
reg dep tree  i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a  if city_ykr==0  & SEX==1, cluster(family_nb)
reg dep building  i.SEX age i.edu i.work i.livingstatus i.smoking i.substance i.alcohol_freq i.PA_freq deprivation_a  if city_ykr==0  & SEX==1, cluster(family_nb)

//----------------------//



//----rGE by PRS----//
//code for importing data


lasso linear dep (pc1 pc2 pc3 pc4 pc5 pc6 pc7 pc8 pc9 pc10 SEX age) score_scz score_bmi2018 score_ukbb20127_raw score_ukbb2090 score_wb score_mdd score_ins score_ext score_ad score_alcdep score_si2022 score_ea, folds(10)  cluster(family_nb)
//generate multi-prs score
lassogof 
etable 
estimates 
predict multi_prs, xb

reg sky multi_prs sex
reg tree multi_prs sex 
reg building multi_prs sex

reg sky multi_prs if SEX==1
reg tree multi_prs if SEX==1
reg building multi_prs if SEX==1

reg sky multi_prs if SEX==2
reg tree multi_prs if SEX==2
reg building multi_prs if SEX==2
//----------------------//


//----intra-pair correlation----//
recode ZYG 0=2
loneway sky family_nb if ZYG ==1
loneway sky family_nb if ZYG ==2

loneway tree family_nb if ZYG ==1
loneway tree family_nb if ZYG ==2

loneway building family_nb if ZYG ==1
loneway building family_nb if ZYG ==2

loneway dep family_nb if ZYG ==1
loneway dep family_nb if ZYG ==2
//----------------------//

//----data processing for twin modelling in R----//
bysort family_nb: gen a =runiform()
bysort family_nb: egen b = max(a)
bysort family_nb: gen j=1 if b==a
bysort family_nb: replace j=2 if b!=a
bysort family_nb: egen c = max(j)
drop if c ==1


keep  family_nb age SEX ZYG j dep sky tree building 
reshape wide dep sky tree building age SEX, i(family_nb) j(j)

gen sexzyg =.
replace sexzyg=1 if SEX1==1 & ZYG==1 //MMZ
replace sexzyg=2 if SEX1==2 & ZYG==1 //FMZ
gen sexindex= SEX1+SEX2
replace sexzyg=3 if sexindex==2 & ZYG==2 //MDZ 
replace sexzyg=4 if sexindex==4 & ZYG==2 //FDZ 
replace sexzyg=5 if sexindex==3 & ZYG==2 //OSDZ

drop sexindex
label define sexzyg 1 "MMZ" 2 "FMZ" 3 "MDZ" 4 "FDZ" 5 "OSDZ"
label value  sexzyg sexzyg
//code for exporting
//----------------------//