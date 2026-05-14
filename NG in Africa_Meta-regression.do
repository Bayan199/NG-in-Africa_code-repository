**# import
import excel "C:\Users\baa4012\Box\gonorreha in Africa\files for extraction\Bayan's files\Updated Search June 2025\Data Analysis\NG in Africa-Meta regression.xlsx", sheet("uro") firstrow

**Labelling**

**# Population classification for urogenital 
label define health 1 "General population" 2 "Intermediate risk population" 3 "Female sex workers (FSWs)" 4 "Men who have sex with men (MSM)" 7 "Symptomatic women" 8 "Symptomatic men" 9 "Symptomatic mixed sexes" 10 "Infertility clinic attendees" 11 "Women with adverse pregnancy or birth outcomes" 12 "STI clinic attendees" 13 "HIV-positive individuals and individuals in HIV-discordant couples" 14 "Sexual contacts of persons with NG/CT" 15 "Patients with confirmed or suspected sexually transmitted infections and related infections" 16 "Other populations"
label value health health
tab health 


**# Population classification for anorectal 
label define health 1 "General population" 2 "Intermediate risk population" 3 "Female sex workers (FSWs)" 4 "Men who have sex with men (MSM)" 8 "Symptomatic men" 13 "HIV-positive individuals and individuals in HIV-discordant couples"
label value health health
tab health 


*********************************************************************************
**# Assay
label define assay_coded 1 "NAAT/PCR" 2 "Culture" 3 "Gram stain/Microscopy" 5 "Rapid test" 8 "ELISA/EIA" 11 "Mixed" 12 "Unclear"
label value assay_coded assay_coded
tab assay_coded

**********************************************************************************
**# gender

label define Gender 1"Female" 2"Male" 3"Mixed"
label value Gender Gender
tab Gender
************************************************************************************
**#Publication year
label define Year_Pub 1"<=2005" 2"2006-2015" 3">2015"
label value Year_Pub Year_Pub
tab Year_Pub

**#data collection year
label define Code 1"<2000" 2"2000-2009" 3">=2010"
label value Code Code
tab Code
***********************************************************************************
**# SAmple size

label define precision 1"<200" 2">200"
label value precision precision 
tab precision
*********************************************************************************
**# probability

label define probability 1"probability" 2"non-probability"
label value probability probability
tab probability
*********************************************************************************
**# Response rate

label define response 1">=80" 2"<80%" 3"Unclear" 
label value response response
tab response
*********************************************************************************
label define response_quality 1">80%" 2"otherwise"
label value response_quality response_quality
tab response_quality
*********************************************************************************

**# Age_groups  ≤ 1 ≥
label define age_coded1 1 "<20" 2 "20-29" 3 "30-39" 4 "40-49" 5 "≥50" 6 "Mixed" 
label value age_coded1 age_coded1
tab age_coded1
******************************************************************************


**# Age_groups  ≤ 2 ≥
label define age_coded2 1 "≤24" 2 "24-35" 3 "35-45" 4 "≥45" 5 "Mixed" 
label value age_coded2 age_coded2
tab age_coded2
******************************************************************************


**# Age_updated **Anorectal**

label define age_coded 1 "18-34" 2 "Mixed" 
label value age_coded age_coded
tab age_coded



**# Age_updated **Oropharyngeal**

label define age_coded 1 "15-34" 2 "Mixed" 
label value age_coded age_coded
tab age_coded

********************************************************************************
**# Regions

label define subreg 1 "Southern Africa" 2 "Eastern Africa" 3 "Western Africa" 4 "Northern Africa" 5 "Central Africa" 6 "Mixed regions"
label value subreg subreg
tab subreg


*********************************************************************************************
***generate the summarised measures by groups: mean, median, range, and overall sample size
******************************************************************************************


table health, c(count n sum n)
table subreg, c(count n sum n)
table Gender, c(count n sum n)
table age_coded1, c(count n sum n)
table age_coded2, c(count n sum n)
table assay_coded, c(count n sum n)
table precision, c(count n sum n)
table probability, c(count n sum n)
table response, c(count n sum n)
table Year_Pub, c(count n sum n)
table Code, c(count n sum n)
table age_coded, c(count n sum n)

*********************************************************************************************
***************Computing the Risk Ratio metaregression for the data
*********************************************************************************************

*******STEP 1: create the log trasformation of the prevalence for the meta regression
gen trans=log((event+0.01)/(n+0.01)) if event~=n
replace trans=log((event)/(n+0.01)) if event==n
gen SEtrans=sqrt((1/(event+0.01))-(1/(n+0.01))) if event~=n
replace SEtrans=sqrt((1/event)-(1/(n+0.01))) if event==n


*******STEP 2: Univariable metaregression
xi: metareg trans i.health, wsse (SEtrans) eform
xi: metareg trans i.age_coded1, wsse (SEtrans) eform 
xi: metareg trans i.age_coded2, wsse (SEtrans) eform 
xi: metareg trans i.Gender, wsse (SEtrans) eform
xi: metareg trans i.subreg, wsse (SEtrans) eform
xi: metareg trans i.assay_coded, wsse (SEtrans) eform
xi: metareg trans precision, wsse (SEtrans) eform
xi: metareg trans probability, wsse (SEtrans) eform
xi: metareg trans i.response, wsse (SEtrans) eform

xi: metareg trans i.Code, wsse (SEtrans) eform 
xi: metareg trans imputed, wsse (SEtrans) eform

xi: metareg trans i.Year_Pub, wsse (SEtrans) eform 
xi: metareg trans year, wsse (SEtrans) eform


xi: metareg trans i.age_coded, wsse (SEtrans) eform 


*******STEP 3: Multivariable metaregression (add only variables found significan in the univariate analysis p<=0.1)

***urogenital:

For Year of data collection:

xi: metareg trans i.health i.age_coded1 i.Gender i.subreg i.assay_coded i.precision i.probability i.response i.Code, wsse (SEtrans) eform 
xi: metareg trans i.health i.age_coded1 i.Gender i.subreg i.assay_coded i.precision i.probability i.response imputed, wsse (SEtrans) eform 

xi: metareg trans i.health i.age_coded2 i.Gender i.subreg i.assay_coded i.precision i.probability i.response i.Code, wsse (SEtrans) eform 
xi: metareg trans i.health i.age_coded2 i.Gender i.subreg i.assay_coded i.precision i.probability i.response imputed, wsse (SEtrans) eform 




For Year of publication:


xi: metareg trans i.health i.age_coded1 i.Gender i.subreg i.assay_coded i.precision i.probability i.response i.Year_Pub, wsse (SEtrans) eform 
xi: metareg trans i.health i.age_coded1 i.Gender i.subreg i.assay_coded i.precision i.probability i.response year, wsse (SEtrans) eform 

xi: metareg trans i.health i.age_coded2 i.Gender i.subreg i.assay_coded i.precision i.probability i.response i.Year_Pub, wsse (SEtrans) eform 
xi: metareg trans i.health i.age_coded2 i.Gender i.subreg i.assay_coded i.precision i.probability i.response year, wsse (SEtrans) eform 



***Anorectal

To make MSM as the comparator: char health[omit] 4

For Year of data collection:

xi: metareg trans i.health i.age_coded1 i.Gender i.subreg i.assay_coded i.precision i.Code, wsse (SEtrans) eform 
xi: metareg trans i.health i.age_coded1 i.Gender i.subreg i.assay_coded i.precision imputed, wsse (SEtrans) eform 

xi: metareg trans i.health i.Gender i.subreg i.assay_coded i.precision i.Code, wsse (SEtrans) eform 
xi: metareg trans i.health i.Gender i.subreg i.assay_coded i.precision imputed, wsse (SEtrans) eform 

xi: metareg trans i.health i.age_coded i.Gender i.subreg i.assay_coded i.precision i.Code, wsse (SEtrans) eform 
xi: metareg trans i.health i.age_coded i.Gender i.subreg i.assay_coded i.precision imputed, wsse (SEtrans) eform 


For Year of publication:

xi: metareg trans i.health i.age_coded1 i.Gender i.subreg i.assay_coded i.precision i.Year_Pub, wsse (SEtrans) eform 
xi: metareg trans i.health i.age_coded1 i.Gender i.subreg i.assay_coded i.precision year, wsse (SEtrans) eform 

xi: metareg trans i.health i.Gender i.subreg i.assay_coded i.precision i.Year_Pub, wsse (SEtrans) eform 
xi: metareg trans i.health i.Gender i.subreg i.assay_coded i.precision year, wsse (SEtrans) eform 



xi: metareg trans i.health i.age_coded i.Gender i.subreg i.assay_coded i.precision i.Year_Pub, wsse (SEtrans) eform 
xi: metareg trans i.health i.age_coded i.Gender i.subreg i.assay_coded i.precision year, wsse (SEtrans) eform 





***oropharygeal



To make MSM as the comparator: char health[omit] 4

For Year of data collection:

xi: metareg trans i.health i.age_coded i.Code, wsse (SEtrans) eform 
xi: metareg trans i.health i.age_coded imputed, wsse (SEtrans) eform 




For Year of publication:

xi: metareg trans i.health i.age_coded i.Year_Pub, wsse (SEtrans) eform 
xi: metareg trans i.health i.age_coded year, wsse (SEtrans) eform 





*********************************************************************************************
***tabulated the variable
*********************************************************************************************

tab health
tab age-coded
tab Gender
tab state_coded
tab mode
tab assay
tab precision
tab probability
tab response
tab year_coded
tab imputed_coded 


***creating dummy variables
generate pop1=health
recode pop1 (1=1) (2=0)(3=0) (4=0)(5=0) (6=0) (7=0) (8=0) (9=0) (10=0) (11=0) (12=0) (13=0) 
generate pop2=health
recode pop2 (1=0) (2=1)(3=0) (4=0)(5=0) (6=0) (7=0) (8=0) (9=0) (10=0) (11=0) (12=0) (13=0) 
generate pop3=health
recode pop3 (1=0) (2=0)(3=1) (4=0)(5=0) (6=0) (7=0) (8=0) (9=0) (10=0) (11=0) (12=0) (13=0) 
generate pop4=health
recode pop4 (1=0) (2=0)(3=0) (4=1)(5=0) (6=0) (7=0) (8=0) (9=0) (10=0) (11=0) (12=0) (13=0) 
generate pop5=health
recode pop5 (1=0) (2=0)(3=0) (4=0)(5=1) (6=0) (7=0) (8=0) (9=0) (10=0) (11=0) (12=0) (13=0) 
generate pop6=health
recode pop6 (1=0) (2=0)(3=0) (4=0)(5=0) (6=1) (7=0) (8=0) (9=0) (10=0) (11=0) (12=0) (13=0) 
generate pop7=health
recode pop7 (1=0) (2=0)(3=0) (4=0)(5=0) (6=0) (7=1) (8=0) (9=0) (10=0) (11=0) (12=0) (13=0) 
generate pop8=health
recode pop8 (1=0) (2=0)(3=0) (4=0)(5=0) (6=0) (7=0) (8=1) (9=0) (10=0) (11=0) (12=0) (13=0) 
generate pop9=health
recode pop9 (1=0) (2=0)(3=0) (4=0)(5=0) (6=0) (7=0) (8=0) (9=1) (10=0) (11=0) (12=0) (13=0) 
generate pop10=health
recode pop10 (1=0) (2=0)(3=0) (4=0)(5=0) (6=0) (7=0) (8=0) (9=0) (10=1) (11=0) (12=0) (13=0) 
generate pop11=health
recode pop11 (1=0) (2=0)(3=0) (4=0)(5=0) (6=0) (7=0) (8=0) (9=0) (10=0) (11=1) (12=0) (13=0) 
generate pop12=health
recode pop12 (1=0) (2=0)(3=0) (4=0)(5=0) (6=0) (7=0) (8=0) (9=0) (10=0) (11=0) (12=1) (13=0) 
generate pop13=health
recode pop13 (1=0) (2=0)(3=0) (4=0)(5=0) (6=0) (7=0) (8=0) (9=0) (10=0) (11=0) (12=0) (13=1)



generate range1=age_1
recode range1 (1=1) (2=0) (3=0) (6=0)
generate range2=age_1
recode range2 (1=0) (2=1) (3=0) (6=0)
generate range3=age_1
recode range3 (1=0) (2=0) (3=1) (6=0)
generate range4=age_1
recode range4 (1=0) (2=0) (3=0) (6=1)


generate gender1=Gender
recode gender1 (1=1) (2=0) (3=0)
generate gender2=Gender
recode gender2 (1=0) (2=1) (3=0)
generate gender3=Gender
recode gender3 (1=0) (2=0) (3=3)


generate mode1=mode
recode mode1(1=1) (2=0) (3=0)
generate mode2=mode
recode mode2 (1=0) (2=1) (3=0)
generate mode3=mode
recode mode3 (1=0) (2=0) (3=1)

generate region1=Subregion
recode region1 (1=1) (2=0) (3=0) (4=0) (5=0)
generate region2=Subregion
recode region2 (1=0) (2=1) (3=0) (4=0) (5=0)
generate region3=Subregion
recode region3 (1=0) (2=0) (3=1) (4=0) (5=0)
generate region4=Subregion
recode region4 (1=0) (2=0) (3=0) (4=1) (5=0)
generate region5=Subregion
recode region5 (1=0) (2=0) (3=0) (4=0) (5=1)


generate income1=income
recode income1 (3=1) (4=0) (5=0)
generate income2=income
recode income2 (3=0) (4=1) (5=0)
generate income3=income
recode income3 (3=0) (4=0) (5=1)

generate precision1=precision
recode precision1 (1=1) (2=0) 
generate precision2=precision
recode precision2 (1=0) (2=1) 


generate probability1=probability
recode probability1 (1=1) (2=0)
generate probability2=probability
recode probability2 (1=0) (2=1)


generate response1=response
recode response1 (1=1) (2=0) (3=0)
generate response2=response
recode response2 (1=0) (2=1) (3=0)
generate response3=response
recode response3 (1=0) (2=0) (3=1)


generate collection1=imputed1
recode collection1 (1=1) (2=0) (3=0)
generate collection2=imputed1
recode collection2 (1=0) (2=1) (3=0)
generate collection3=imputed1
recode collection3 (1=0) (2=0) (3=1)


generate year1=Year_Pub
recode year1 (1=1) (2=0) (3=0)
generate year2=Year_Pub
recode year2 (1=0) (2=1) (3=0)
generate year3=Year_Pub
recode year3 (1=0) (2=0) (3=1)


***********************************************************************************************************
Temporal Trend
***********************************************************************************************************
twoway ///
(scatter prevalence year, mcolor(gs6) msymbol(o)) ///
(lfit prevalence year, lcolor(navy) lwidth(medthick)), ///
legend(order(1 "Prevalence" 2 "Linear time trend in prevalence") ///
       size(small) ///
       position(12) ring(0) ///
       cols(2) ///
       region(lcolor(gs8) lwidth(vthin) fcolor(none))) ///
ytitle("Prevalence") ///
xtitle("Publication year") ///
xlabel(1960(10)2025, grid) ///
ylabel(0(10)100, format(%9.0f) grid) ///
graphregion(margin(0 10 0 10) color(white))
******************************************************
twoway ///
(scatter prevalence imputed, mcolor(gs6) msymbol(o)) ///
(lfit prevalence imputed, lcolor(navy) lwidth(medthick)), ///
legend(order(1 "Prevalence" 2 "Linear time trend in prevalence") ///
       size(small) ///
       position(12) ring(0) ///
       cols(2) ///
       region(lcolor(gs8) lwidth(vthin) fcolor(none))) ///
ytitle("Prevalence") ///
xtitle("Year of data collection") ///
xlabel(1960(10)2025, grid) ///
ylabel(0(10)100, format(%9.0f) grid) ///
graphregion(margin(0 10 0 10) color(white))
**************************************************************************
**# Central
**************************************************************************
twoway ///
(scatter prevalence imputed if subreg==5, mcolor(gs6) msymbol(o)) ///
(lfit prevalence imputed if subreg==5, lcolor(navy) lwidth(medthick)), ///
legend(order(1 "Prevalence" 2 "Linear time trend in prevalence") ///
       size(small) ///
       position(12) ring(0) ///
       cols(2) ///
       region(lcolor(gs8) lwidth(vthin) fcolor(none))) ///
ytitle("Prevalence") ///
xtitle("Year of data collection") ///
xlabel(1970(10)2025, grid) ///
ylabel(0(10)100, format(%9.0f) grid) ///
graphregion(margin(0 10 0 10) color(white))
**************************************************************************
**# eastern
**************************************************************************
twoway ///
(scatter prevalence imputed if subreg==2, mcolor(gs6) msymbol(o)) ///
(lfit prevalence imputed if subreg==2, lcolor(navy) lwidth(medthick)), ///
legend(order(1 "Prevalence" 2 "Linear time trend in prevalence") ///
       size(small) ///
       position(12) ring(0) ///
       cols(2) ///
       region(lcolor(gs8) lwidth(vthin) fcolor(none))) ///
ytitle("Prevalence") ///
xtitle("Year of data collection") ///
xlabel(1960(10)2025, grid) ///
ylabel(0(10)100, format(%9.0f) grid) ///
graphregion(margin(0 10 0 10) color(white))


**************************************************************************
**# southern
**************************************************************************
twoway ///
(scatter prevalence imputed if subreg==1, mcolor(gs6) msymbol(o)) ///
(lfit prevalence imputed if subreg==1, lcolor(navy) lwidth(medthick)), ///
legend(order(1 "Prevalence" 2 "Linear time trend in prevalence") ///
       size(small) ///
       position(12) ring(0) ///
       cols(2) ///
       region(lcolor(gs8) lwidth(vthin) fcolor(none))) ///
ytitle("Prevalence") ///
xtitle("Year of data collection") ///
xlabel(1960(10)2025, grid) ///
ylabel(0(10)100, format(%9.0f) grid) ///
graphregion(margin(0 10 0 10) color(white))

**************************************************************************
**# western
**************************************************************************
twoway ///
(scatter prevalence imputed if subreg==3, mcolor(gs6) msymbol(o)) ///
(lfit prevalence imputed if subreg==3, lcolor(navy) lwidth(medthick)), ///
legend(order(1 "Prevalence" 2 "Linear time trend in prevalence") ///
       size(small) ///
       position(12) ring(0) ///
       cols(2) ///
       region(lcolor(gs8) lwidth(vthin) fcolor(none))) ///
ytitle("Prevalence") ///
xtitle("Year of data collection") ///
xlabel(1960(10)2025, grid) ///
ylabel(0(10)100, format(%9.0f) grid) ///
graphregion(margin(0 10 0 10) color(white))
