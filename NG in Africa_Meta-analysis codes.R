

library(XLConnect)
library(readxl)
library(metadat)
library(meta)
library(metaviz)
library(metasens) 

***folder directory
setwd("C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis") 


#####data directory

#  assay code
# NAAT/PCR	1
# Culture	2
# Gram stain/microscopy	3
# Rapid test	5
# Serological tests	6
# ELISA/EIA 8
# Mixed	11
# Unclear	12


#  population code:
# General population	1
# Intermediate risk population	2
# FSWs	3
# MSM	4
# Male sex workers	5
# transgender and WSW	6
# Symptomatic women	7
# Symptomatic men	8
# Symptomatic mixed sexes	9
# Infertility clinic attendees	10
# Women with adverse pregnancy or birth outcomes (e.g., miscarriage, ectopic pregnancy, stillbirth, preterm delivery, SGA, or similar)	11
# STI clinic attendees	12
# HIV-positive individuals and individuals in HIV-discordant couples	13
# Sexual contact of persons with neisseria gonorrhoeae/chlamydia trachomatis	14
# Patients with confirmed or suspected sexually transmitted infections and related infections	15
# Other populations	16



*********************************************************************************************************************************************************
  #  Meta-analysis for urogenital
*********************************************************************************************************************************************************
  
  
  urogenital<- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet="uro")
attach(urogenital)
**************************************************************************
  

####population type#####
##1##General Population
attach(urogenital)
health<-subset(urogenital,urogenital$health==1)
detach(urogenital)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 


## Urogenital-General population DOI plot (No sub group analysis)
metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Urogenital_General population.tif",
    width=4500, height=45000, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(0, 100), 
       xlab.pos = 20,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()






##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 
##meta-analysis by culture
attach(health)
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by gram stain
attach(health)
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by rapid test
attach(health)
assay<-subset(health, assay_coded==5)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay)

##meta-analysis by ELISA/EIA
attach(health)
assay<-subset(health, assay_coded==8)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay)

##meta-analysis by Mixed
attach(health)
assay<-subset(health, assay_coded==11)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)

##meta-analysis by unclear
attach(health)
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)

*******************************************************************
####population type#######2##Intermediate Risk
  attach(urogenital)
  health<-subset(urogenital,urogenital$health==2)
detach(urogenital)
attach(health)
summary(prevalence)

## Urogenital-Intermediate Risk DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Urogenital_Intermediate risk population.tif",
    width=4500, height=7500, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(0, 100), 
       xlab.pos = 20,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()




##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 
##meta-analysis by culture
attach(health)
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by gram stain
attach(health)
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by rapid test
attach(health)
assay<-subset(health, assay_coded==5)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay)

##meta-analysis by unclear
attach(health)
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)

*******************************************************************
  
####population type#######3##FSWs
attach(urogenital)
health<-subset(urogenital,urogenital$health==3)
detach(urogenital)
attach(health)
summary(prevalence)

## Urogenital-FSWs DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Urogenital_FSWs.tif",
    width=5000, height=13000, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(0, 100), 
       xlab.pos = 20,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()



##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 
##meta-analysis by culture
attach(health)
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by gram stain
attach(health)
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by rapid test
attach(health)
assay<-subset(health, assay_coded==5)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay)


##meta-analysis by Mixed
attach(health)
assay<-subset(health, assay_coded==11)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)

##meta-analysis by unclear
attach(health)
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)

******************************************************************************
####population type#######4 MSMs
attach(urogenital)
health<-subset(urogenital,urogenital$health==4)
detach(urogenital)
attach(health)
summary(prevalence)


## Urogenital-MSM DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Urogenital_MSM.tif",
    width=5000, height=4000, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(0, 30), 
       xlab.pos = 1,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()






##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


*****************************************************************************


####population type#####5 SYMPTOMATIC WOMEN##
attach(urogenital)
health<-subset(urogenital,urogenital$health==7)
detach(urogenital)
attach(health)
summary(prevalence)


## Urogenital-Symptomatic women DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Urogenital_Symptomatic women.tif",
    width=5000, height=15000, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(0, 100), 
       xlab.pos = 20,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()







##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 
##meta-analysis by culture
attach(health)
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by gram stain
attach(health)
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by rapid test
attach(health)
assay<-subset(health, assay_coded==5)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay)


##meta-analysis by Mixed
attach(health)
assay<-subset(health, assay_coded==11)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)

##meta-analysis by unclear
attach(health)
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)

*****************************************************************************
  
  
  ####population type##### Symptomatic men##
  attach(urogenital)
health<-subset(urogenital,urogenital$health==8)
detach(urogenital)
attach(health)
summary(prevalence)


## Urogenital-Symptomatic men DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Urogenital_Symptomatic men.tif",
    width=4000, height=16500, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(0, 100), 
       xlab.pos = 20,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()






##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 
##meta-analysis by culture
attach(health)
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by gram stain
attach(health)
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 



##meta-analysis by rapid test
attach(health)
assay<-subset(health, assay_coded==5)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay)


##meta-analysis by Mixed
attach(health)
assay<-subset(health, assay_coded==11)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)

##meta-analysis by unclear
attach(health)
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
*****************************************************************************
  
  
  ####population type##### Symptomatic mixed sexes##
  attach(urogenital)
health<-subset(urogenital,urogenital$health==9)
detach(urogenital)
attach(health)
summary(prevalence)



## Urogenital-Symptomatic mixed sexes DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Urogenital_Symptomatic mixed sexes.tif",
    width=3500, height=4500, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(0, 100), 
       xlab.pos = 20,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()




##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 
##meta-analysis by culture
attach(health)
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by gram stain
attach(health)
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 



##meta-analysis by Mixed
attach(health)
assay<-subset(health, assay_coded==11)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)

##meta-analysis by unclear
attach(health)
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)

*****************************************************************************
  
  
  ####population type#####Infertility clinic attendees ##
  attach(urogenital)
health<-subset(urogenital,urogenital$health==10)
detach(urogenital)
attach(health)
summary(prevalence)



## Urogenital-Infertility clinic attendees DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Urogenital_Infertility clinic attendees.tif",
    width=3000, height=2000, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(0, 30), 
       xlab.pos = 1,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()








##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 
##meta-analysis by culture
attach(health)
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by gram stain
attach(health)
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by unclear
attach(health)
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)*****************************************************************************
  
  
####Women with adverse pregnancy or birth outcomes ##
  attach(urogenital)
health<-subset(urogenital,urogenital$health==11)
detach(urogenital)
attach(health)
summary(prevalence)




## Urogenital-Women with adverse pregnancy or birth outcomes DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Urogenital_Women with adverse pregnancy or birth outcomes.tif",
    width=3000, height=2000, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(0, 50), 
       xlab.pos = 5,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()







##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 
##meta-analysis by culture
attach(health)
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by gram stain
attach(health)
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

*****************************************************************************
  
  
  ####STI clinic attendees##### ##
  attach(urogenital)
health<-subset(urogenital,urogenital$health==12)
detach(urogenital)
attach(health)
summary(prevalence)



## Urogenital-STI clinic attendees DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Urogenital_STI clinic attendees.tif",
    width=4000, height=5500, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(0, 80), 
       xlab.pos = 10,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()






##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 
##meta-analysis by culture
attach(health)
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by gram stain
attach(health)
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by ELISA/EIA
attach(health)
assay<-subset(health, assay_coded==8)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay)

##meta-analysis by Mixed
attach(health)
assay<-subset(health, assay_coded==11)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)

##meta-analysis by unclear
attach(health)
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)


****************************************************************************************
  
  
  ####HIV-positive individuals and individuals in HIV-discordant couples##### ##
  attach(urogenital)
health<-subset(urogenital,urogenital$health==13)
detach(urogenital)
attach(health)
summary(prevalence)



## Urogenital-HIV-positive individuals and individuals in HIV-discordant couples DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Urogenital_HIV-positive individuals and individuals in HIV-discordant couples.tif",
    width=3500, height=6000, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(0, 50), 
       xlab.pos = 10,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()





##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 
##meta-analysis by culture
attach(health)
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by gram stain
attach(health)
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by unclear
attach(health)
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)

**********************************************************************************
  
  
  ####Sexual contact of persons with neisseria gonorrhoeae/chlamydia trachomatis##### ##
  attach(urogenital)
health<-subset(urogenital,urogenital$health==14)
detach(urogenital)
attach(health)
summary(prevalence)




## Urogenital-Sexual contact of persons with neisseria gonorrhoeae/chlamydia trachomatis DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Urogenital_Sexual contact of persons with NG or CT.tif",
    width=3500, height=2000, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(0, 80), 
       xlab.pos = 10,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()










##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 
##meta-analysis by culture
attach(health)
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by gram stain
attach(health)
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 
************************************************************************************
  
  
  ####population type##### Patients with confirmed or suspected sexually transmitted infections and related infections##
  attach(urogenital)
health<-subset(urogenital,urogenital$health==15)
detach(urogenital)
attach(health)
summary(prevalence)





## Urogenital-Patients with confirmed or suspected sexually transmitted infections and related infections DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Urogenital_Patients with confirmed or suspected sexually transmitted infections and related infections.tif",
    width=3500, height=3000, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(0, 100), 
       xlab.pos = 20,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()







##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 
##meta-analysis by culture
attach(health)
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by gram stain
attach(health)
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by Mixed
attach(health)
assay<-subset(health, assay_coded==11)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)

##meta-analysis by unclear
attach(health)
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)


*****************************************************************************
  *******************************************************************
  
  ####population type##### Other populations##
  attach(urogenital)
health<-subset(urogenital,urogenital$health==16)
detach(urogenital)
attach(health)
summary(prevalence)




## Urogenital-Other populations DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Urogenital_Other populations.tif",
    width=3500, height=5500, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(0, 100), 
       xlab.pos = 20,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()










##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 
##meta-analysis by culture
attach(health)
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by gram stain
attach(health)
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by ELISA/EIA
attach(health)
assay<-subset(health, assay_coded==8)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay)

##meta-analysis by unclear
attach(health)
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)












*********************************************************************************************************************************************************
  #  Meta-analysis for ANORECTAL
*********************************************************************************************************************************************************
  
  setwd("C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Forest Plots/Anorectal") 
  
  anorectal<- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet="ano")
attach(anorectal)
#####data directory
#  assay code
#    - NAAT/PCR = 1
#    - culture = 2
#    - Gram stain/microscopy =  3
#    - Unclear = 	12


#  population code:
#     -	General populations  = 1
#     -	Intermediate risk populations = 2
#     -	Female sex workers  = 3    
#     - MSM = 4
#     -Symptomatic men =	8
#     -HIV-positive individuals and individuals in HIV-discordant couples=	13



********************************************************************************
  **********************************************************************
  
    
####population type##### General populations##
attach(anorectal)
health<-subset(anorectal,anorectal$health==1)
detach(anorectal)
attach(health)
summary(prevalence)





## Anorectal-General population DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Anorectal-General population.tif",
    width=3500, height=1500, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(0, 20), 
       xlab.pos = 1,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()




##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 
##meta-analysis by culture
attach(health)
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 



********************************************************************************
  **********************************************************************
  
  
  ####population type##### Intermediate risk populations##
  attach(anorectal)
health<-subset(anorectal,anorectal$health==2)
detach(anorectal)
attach(health)
summary(prevalence)






## Anorectal-Intermediate risk populations DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Anorectal-Intermediate risk populations.tif",
    width=3500, height=1500, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(-20, 20), 
       xlab.pos = 1,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()








##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 
 

********************************************************************************
  **********************************************************************
  
  
  ####population type##### FSWs##
  attach(anorectal)
health<-subset(anorectal,anorectal$health==3)
detach(anorectal)
attach(health)
summary(prevalence)




## Anorectal-FSWs DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Anorectal-FSWs.tif",
    width=3500, height=1500, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(-20, 20), 
       xlab.pos = 1,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()










##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 



********************************************************************************
  **********************************************************************
  
  
  ####population type##### MSM##
  attach(anorectal)
health<-subset(anorectal,anorectal$health==4)
detach(anorectal)
attach(health)
summary(prevalence)








## Anorectal-MSM DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Anorectal-MSM.tif",
    width=3500, height=3500, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(0, 70), 
       xlab.pos = 1,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()









##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by unclear
attach(health)
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)





********************************************************************************
  **********************************************************************
  
  
  ####population type##### Symptomatic men##
  attach(anorectal)
health<-subset(anorectal,anorectal$health==8)
detach(anorectal)
attach(health)
summary(prevalence)







## Anorectal-Symptomatic men DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Anorectal-Symptomatic men.tif",
    width=3500, height=1500, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(-20, 70), 
       xlab.pos = 10,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()







##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by gram stain
attach(health)
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by unclear
attach(health)
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)





********************************************************************************
  **********************************************************************
  
  
  ####population type##### Individuals living with HIV and individuals in HIV-discordant couples  ##
  attach(anorectal)
health<-subset(anorectal,anorectal$health==13)
detach(anorectal)
attach(health)
summary(prevalence)





## Anorectal-Individuals living with HIV and individuals in HIV-discordant couples DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Anorectal-Individuals living with HIV and individuals in HIV-discordant couples.tif",
    width=3500, height=1500, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(0, 70), 
       xlab.pos = 10,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()









##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 
##meta-analysis by culture
attach(health)
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 







*********************************************************************************************************************************************************
  #  Meta-analysis for Oropharyngeal 
*********************************************************************************************************************************************************
 
   setwd("C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Forest Plots/Oropharyngeal") 
  
oro<- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet="oro")
attach(oro)
 
  
#####data directory
#  assay code
#    - NAAT/PCR = 1
#    - culture = 2


#  population code:
#     -	General populations  = 1
#     - MSM = 4
#     - Symptomatic men =	8
#     - HIV-positive individuals and individuals in HIV-discordant couples=	13



********************************************************************************
  **********************************************************************
  
  ####population type##### General populations##
  attach(oro)
health<-subset(oro,oro$health==1)
detach(oro)
attach(health)
summary(prevalence)




## Oropharyngeal-General population DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Oropharyngeal-General population.tif",
    width=3500, height=1500, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(-20, 30), 
       xlab.pos = 1,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()






##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 
##meta-analysis by culture
attach(health)
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 



********************************************************************************
  **********************************************************************
  
  ####population type##### MSM##
  attach(oro)
health<-subset(oro,oro$health==4)
detach(oro)
attach(health)
summary(prevalence)




## Oropharyngeal-MSM DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Oropharyngeal-MSM.tif",
    width=3500, height=1500, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(-20, 40), 
       xlab.pos = 1,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()








##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


********************************************************************************
  **********************************************************************
  
  ####population type##### Symptomatic men##
  attach(oro)
health<-subset(oro,oro$health==8)
detach(oro)
attach(health)
summary(prevalence)


## Oropharyngeal-Symptomatic men DOI plot (No sub group analysis)

metahealth<-metaprop(health$event, health$n, health$Name, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

png(file="C:/Users/baa4012/Box/gonorreha in Africa/files for extraction/Bayan's files/Updated Search June 2025/Data Analysis/Oropharyngeal-Symptomatic men.tif",
    width=3500, height=1500, res=300)

forest(metahealth, 
       comb.fixed = TRUE, 
       comb.random = TRUE, 
       overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = metahealth$Assay, 
       print.byvar = metahealth$print.byvar,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.subgroup = "black",        
       xlab = "Gonorrhea prevalence (95% CI)",
       xlim = c(-20, 40), 
       xlab.pos = 1,
       label.left = metahealth$Title,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Author, Year", "Gonorrhea positive", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE, 
       print.Q = TRUE, 
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)

dev.off()









##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by culture
attach(health)
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 






********************************************************************************
  **********************************************************************
  
  ####population type##### Individuals living with HIV and individuals in HIV-discordant couples##
  attach(oro)
health<-subset(oro,oro$health==13)
detach(oro)
attach(health)
summary(prevalence)





## Oropharyngeal-Individuals living with HIV and individuals in HIV-discordant couples DOI plot (No sub group analysis)

##Not possible to conduct since it's only one study.
      







##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 





 


********************************************************************************************************************************************************
  #  Meta-analysis for seroprevalence
********************************************************************************************************************************************************
  sera<- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet="sera")
attach(sera)

summary(prevalence)

metasera<-metaprop(sera$event, sera$n, sera$Name, byvar=sera$Pop_classification, sm="PFT", comb.random=TRUE, prediction=TRUE, title="sera")
print(metasera)


#####data directory
#  population code:
#   General population	1
#   Intermediate risk population	2
#   FSWs	3
#   Symptomatic women	7
#   Symptomatic men	8
#   Infertility clinic attendees	10
#   Women with adverse pregnancy or birth outcomes (e.g., miscarriage, ectopic pregnancy, stillbirth, preterm delivery, SGA, or similar)	11
#   HIV-positive individuals and individuals in HIV-discordant couples	13
#   Patients with confirmed or suspected sexually transmitted infections and related infections	15
#   Other populations	16

#####data directory

#  assay code
#   culture = 2
#   Serological tests	6
#   ELISA/EIA	8
#   Unclear	12


********************************************************************************
  *********************************************************************
  
  

####population type##### General populations##
attach(sera)
health<-subset(sera,sera$health==1)
detach(sera)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by Culture
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 



##meta-analysis by ELISA/EIA
assay<-subset(health, assay_coded==8)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by Serological tests
assay<-subset(health, assay_coded==6)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 






********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### Intermediate risk populations ##
  attach(sera)
health<-subset(sera,sera$health==2)
detach(sera)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by Unclear assays
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


********************************************************************************
  *********************************************************************
  


####population type##### FSWs##
attach(sera)
health<-subset(sera,sera$health==3)
detach(sera)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by Unclear assays
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 






********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### Symptomatic women ##
  attach(sera)
health<-subset(sera,sera$health==7)
detach(sera)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 



##meta-analysis by ELISA/EIA
assay<-subset(health, assay_coded==8)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by Serological tests
assay<-subset(health, assay_coded==6)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 







********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### Symptomatic men ##
  attach(sera)
health<-subset(sera,sera$health==8)
detach(sera)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 


##meta-analysis by Serological tests
assay<-subset(health, assay_coded==6)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 







********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### Infertility clinic attendees ##
  attach(sera)
health<-subset(sera,sera$health==10)
detach(sera)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 



##meta-analysis by ELISA/EIA
assay<-subset(health, assay_coded==8)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by Serological tests
assay<-subset(health, assay_coded==6)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 













********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### Women with adverse pregnancy or birth outcomes ##
  attach(sera)
health<-subset(sera,sera$health==11)
detach(sera)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 



##meta-analysis by ELISA/EIA
assay<-subset(health, assay_coded==8)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by Serological tests
assay<-subset(health, assay_coded==6)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 




********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### Individuals living with HIV and individuals in HIV-discordant couples ##
  attach(sera)
health<-subset(sera,sera$health==13)
detach(sera)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 



##meta-analysis by Serological tests
assay<-subset(health, assay_coded==6)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 








********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### Patients with confirmed or suspected sexually transmitted infections and related infections##
  attach(sera)
health<-subset(sera,sera$health==15)
detach(sera)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 



##meta-analysis by ELISA/EIA
assay<-subset(health, assay_coded==8)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 







********************************************************************************
  *********************************************************************
  
  

####population type##### Other populations##
attach(sera)
health<-subset(sera,sera$health==16)
detach(sera)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 



##meta-analysis by Serological tests
assay<-subset(health, assay_coded==6)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


















*********************************************************************************************************************************************************
  #  Meta-analysis for MIXED/UNCLEAR
*********************************************************************************************************************************************************
  mixed<- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet="mixed")
attach(mixed)



#####data directory
#  population code:
#   General population	1
#   Intermediate risk population	2
#   FSWs	3
#   MSM   4
#   Symptomatic women 	7
#   Symptomatic men	  8
#   Symptomatic mixed sexes   9
#   Women with adverse pregnancy or birth outcomes (e.g., miscarriage, ectopic pregnancy, stillbirth, preterm delivery, SGA, or similar)	11
#   STI clinic attendees  12
#   HIV-positive individuals and individuals in HIV-discordant couples	13
#   Sexual contacts of persons infected with NG/CT    14
#   Patients with confirmed or suspected sexually transmitted infections and related infections	15
#   Other populations 	16

#####data directory

#  assay code
# NAAT/PCR	1
# Culture	2
# Gram stain/microscopy	3
# Rapid test	5
# Serological tests	6
# ELISA/EIA 8
# Mixed	11
# Unclear	12



********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### General populations##
  attach(mixed)
health<-subset(mixed,mixed$health==1)
detach(mixed)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 


##meta-analysis by NAAT/PCR
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 



##meta-analysis by Culture
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 



##meta-analysis by Gram stain
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by Mixed
assay<-subset(health, assay_coded==11)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 




##meta-analysis by Unclear
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 






********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### Intermediate risk populations##
  attach(mixed)
health<-subset(mixed,mixed$health==2)
detach(mixed)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 


##meta-analysis by NAAT/PCR
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 





********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### FSWs##
  attach(mixed)
health<-subset(mixed,mixed$health==3)
detach(mixed)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 


##meta-analysis by NAAT/PCR
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by Unclear
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 








********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### MSM ##
  attach(mixed)
health<-subset(mixed,mixed$health==4)
detach(mixed)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 


##meta-analysis by NAAT/PCR
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by Gram stain
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by Unclear
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 







********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### Symptomatic women ##
  attach(mixed)
health<-subset(mixed,mixed$health==7)
detach(mixed)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 


##meta-analysis by Culture
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by Mixed
assay<-subset(health, assay_coded==11)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by Unclear
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 










********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### Symptomatic men ##
  attach(mixed)
health<-subset(mixed,mixed$health==8)
detach(mixed)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 


##meta-analysis by Culture
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by Gram stain
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by Unclear
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 








********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### Symptomatic mixed sexes ##
  attach(mixed)
health<-subset(mixed,mixed$health==9)
detach(mixed)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 


##meta-analysis by Culture
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by Gram stain
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by Unclear
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 






********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### Women with adverse pregnancy or birth outcomes ##
  attach(mixed)
health<-subset(mixed,mixed$health==11)
detach(mixed)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 


##meta-analysis by Culture
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by Unclear
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 









********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### STI clinic attendees ##
  attach(mixed)
health<-subset(mixed,mixed$health==12)
detach(mixed)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 


##meta-analysis by Culture
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by Gram stain
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by Unclear
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 










********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### Individuals living with HIV and individuals in HIV-discordant couples##
  attach(mixed)
health<-subset(mixed,mixed$health==13)
detach(mixed)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 


##meta-analysis by NAAT/PCR
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by Unclear
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 







********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### Sexual contacts of persons infected with NG/CT ##
  attach(mixed)
health<-subset(mixed,mixed$health==14)
detach(mixed)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 


##meta-analysis by Unclear
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 








********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### Patients with confirmed or suspected sexually transmitted infections and related infections  ##
  attach(mixed)
health<-subset(mixed,mixed$health==15)
detach(mixed)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 



##meta-analysis by Culture
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


##meta-analysis by Unclear
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 










********************************************************************************
  *********************************************************************
  
  
  
  ####population type##### Other populations #
  attach(mixed)
health<-subset(mixed,mixed$health==16)
detach(mixed)
attach(health)
summary(prevalence)

##overall meta-analysis with subgroups by assay
metahealth<-metaprop(health$event, health$n, health$Name, subgroup = health$assay_type, sm="PFT", random=TRUE, prediction=TRUE, title="healthy")
print(metahealth)
doiplot(metahealth) 

##meta-analysis by NAAT/PCR
assay<-subset(health, assay_coded==1)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by Culture
assay<-subset(health, assay_coded==2)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by Gram stain
assay<-subset(health, assay_coded==3)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by Mixed
assay<-subset(health, assay_coded==11)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 

##meta-analysis by Unclear
assay<-subset(health, assay_coded==12)
detach(health)
attach(assay)
summary(prevalence)
metaassay<-metaprop(assay$event, assay$n, assay$Name, sm="PFT", random=TRUE, prediction=TRUE, title="assay")
print(metaassay)
doiplot(metaassay) 


**************************************************************************************
  *****************************************************************************
  New Sub-populations 
**************************************************************************
  
  
  ##1##General Populations (excluding pregnant women)
  # Load packages
  library(readxl)
install.packages("meta")
library(meta)

# Read data
urogenital <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "uro")


# filter General low risk population
Type <- subset(urogenital, pop_type == "General: low risk")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "General non pregnant"
)

print(metatype)

****************************
  # Read data
  Anorectal <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "ano")


# filter General low risk population
Type <- subset(Anorectal, pop_type == "General: low risk")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "General non pregnant"
)

print(metatype)
*********************************
  # Read data
  Oro <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "oro")


# filter General low risk population
Type <- subset(Oro, pop_type == "General: low risk")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "General non pregnant"
)

print(metatype)
******************************
  # Read data
  Sera <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "sera")


# filter General low risk population
Type <- subset(Sera, pop_type == "General: low risk")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "General non pregnant"
)

print(metatype)

*******************************
  # Read data
  Mixed <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "mixed")


# filter General low risk population
Type <- subset(Mixed, pop_type == "General: low risk")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "General non pregnant"
)

print(metatype)
*******************************************************************************************************************
  ##2##Antenatal clinic attendees (pregnant women)
  # Load packages
  library(readxl)
install.packages("meta")
library(meta)

# Read data
urogenital <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "uro")


# filter Antenatal clinic attendees (pregnant women)
Type <- subset(urogenital, pop_type == "General: pregnant")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "Antenatal clinic attendees (pregnant women)"
)

print(metatype)

****************************
  # Read data
  Anorectal <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "ano")


# filter Antenatal clinic attendees (pregnant women)
Type <- subset(Anorectal, pop_type == "General: pregnant")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "Antenatal clinic attendees (pregnant women)"
)

print(metatype)
*********************************
  # Read data
  Oro <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "oro")


# filter Antenatal clinic attendees (pregnant women)
Type <- subset(Oro, pop_type == "General: pregnant")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "Antenatal clinic attendees (pregnant women)"
)

print(metatype)
******************************
  # Read data
  Sera <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "sera")


# filter Antenatal clinic attendees (pregnant women)
Type <- subset(Sera, pop_type == "General: pregnant")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "Antenatal clinic attendees (pregnant women)"
)

print(metatype)

*******************************
  # Read data
  Mixed <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "mixed")


# filter Antenatal clinic attendees (pregnant women)
Type <- subset(Mixed, pop_type == "General: pregnant")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "Antenatal clinic attendees (pregnant women)"
)

print(metatype)

*******************************************************************************************************************
  ##3##Intermediate: clients of SWs
  # Load packages
  library(readxl)
install.packages("meta")
library(meta)

# Read data
urogenital <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "uro")


# filter Intermediate: clients of SWs
Type <- subset(urogenital, pop_type == "Intermediate: clients of SWs")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "Intermediate: clients of SWs"
)

print(metatype)

****************************
  # Read data
  Anorectal <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "ano")


# filter Intermediate: clients of SWs
Type <- subset(Anorectal, pop_type == "Intermediate: clients of SWs")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "Intermediate: clients of SWs"
)

print(metatype)



*******************************************************************************************************************
  ##3##Intermediate: soldiers/military
  # Load packages
  library(readxl)
install.packages("meta")
library(meta)

# Read data
urogenital <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "uro")


# filter Intermediate: soldiers/military
Type <- subset(urogenital, pop_type == "Intermediate: soldiers/military")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "Intermediate: soldiers/military"
)

print(metatype)

******************************
  # Read data
  Sera <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "sera")


# filter Intermediate: soldiers/military
Type <- subset(Sera, pop_type == "Intermediate: soldiers/military")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "Intermediate: soldiers/military"
)

print(metatype)

*******************************
  # Read data
  Mixed <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "mixed")


# filter Intermediate: soldiers/military
Type <- subset(Mixed, pop_type == "Intermediate: soldiers/military")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "Intermediate: soldiers/military"
)

print(metatype)



*******************************************************************************************************************
  ##4##Intermediate: soldiers/military
  # Load packages
  library(readxl)
install.packages("meta")
library(meta)

# Read data
urogenital <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "uro")


# filter Intermediate: truck/taxi drivers
Type <- subset(urogenital, pop_type == "Intermediate: truck/taxi drivers")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "Intermediate: truck/taxi drivers"
)

print(metatype)



*******************************************************************************************************************
  ##5##Intermediate: soldiers/military
  # Load packages
  library(readxl)
install.packages("meta")
library(meta)

# Read data
urogenital <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "uro")


# filter Intermediate: miners / fishermen/ plantation workers
Type <- subset(urogenital, pop_type == "Intermediate: miners / fishermen/ plantation workers")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "Intermediate: miners / fishermen/ plantation workers"
)

print(metatype)




*******************************************************************************************************************
  ##6##Intermediate: PWID
  # Load packages
  library(readxl)
install.packages("meta")
library(meta)

# Read data
urogenital <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "uro")


# filter Intermediate: PWID
Type <- subset(urogenital, pop_type == "Intermediate: PWID")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "Intermediate: PWID"
)

print(metatype)

****************************
  # Read data
  Anorectal <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "ano")


# filter Intermediate: PWID
Type <- subset(Anorectal, pop_type == "Intermediate: PWID")


summary(Type$prevalence)


metatype <- metaprop(
  event = Type$event,
  n = Type$n,
  studlab = Type$Name,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "Intermediate: PWID"
)

print(metatype)








**************************************************************************************************
  ******************************************************************************
  Analysis based on sampling method
************************************************************************
  
  #### 1 #### Geneal population 
  **************************
  ##### Uro ######

library(readxl)
library(meta)

urogenital <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "uro")
pop_class <- subset(urogenital, Pop_classification == "General population")

class_prob <- subset(pop_class, QA_Sampling == "Probability")
class_nonprob <- subset(pop_class, QA_Sampling == "Non-probability")

summary(class_prob$prevalence)

meta_prob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_prob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "General population - Probability sampling"
)

summary(class_nonprob$prevalence)
meta_nonprob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_nonprob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "General population - Non-probability sampling"
)

print(meta_prob)
print(meta_nonprob)


##### Ano ######

library(readxl)
library(meta)

anorectal <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "ano")
pop_class <- subset(anorectal, Pop_classification == "General population")

class_nonprob <- subset(pop_class, QA_Sampling == "Non-probability")

summary(class_nonprob$prevalence)
meta_nonprob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_nonprob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "General population - Non-probability sampling"
)

print(meta_nonprob)


##### Oro ######

library(readxl)
library(meta)

oropharyngeal <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "oro")
pop_class <- subset(oropharyngeal, Pop_classification == "General population")

class_nonprob <- subset(pop_class, QA_Sampling == "Non-probability")


summary(class_nonprob$prevalence)
meta_nonprob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_nonprob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "General population - Non-probability sampling"
)


print(meta_nonprob)


##### sera ######

library(readxl)
library(meta)

sera <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "sera")
pop_class <- subset(sera, Pop_classification == "General population")

class_prob <- subset(pop_class, QA_Sampling == "Probability")
class_nonprob <- subset(pop_class, QA_Sampling == "Non-probability")

summary(class_prob$prevalence)

meta_prob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_prob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "General population - Probability sampling"
)

summary(class_nonprob$prevalence)
meta_nonprob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_nonprob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "General population - Non-probability sampling"
)

print(meta_prob)
print(meta_nonprob)



##### mixed ######

library(readxl)
library(meta)

mixed <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "mixed")
pop_class <- subset(mixed, Pop_classification == "General population")

class_prob <- subset(pop_class, QA_Sampling == "Probability")
class_nonprob <- subset(pop_class, QA_Sampling == "Non-probability")

summary(class_prob$prevalence)

meta_prob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_prob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "General population - Probability sampling"
)

summary(class_nonprob$prevalence)
meta_nonprob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_nonprob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "General population - Non-probability sampling"
)

print(meta_prob)
print(meta_nonprob)







#### 2 #### FSWs
**************************
  ##### Uro ######

library(readxl)
library(meta)

urogenital <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "uro")
pop_class <- subset(urogenital, Pop_classification == "FSWs")

class_prob <- subset(pop_class, QA_Sampling == "Probability")
class_nonprob <- subset(pop_class, QA_Sampling == "Non-probability")

summary(class_prob$prevalence)

meta_prob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_prob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "FSWs - Probability sampling"
)

summary(class_nonprob$prevalence)
meta_nonprob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_nonprob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "FSWs - Non-probability sampling"
)

print(meta_prob)
print(meta_nonprob)


##### Ano ######

library(readxl)
library(meta)

ano <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "ano")
pop_class <- subset(ano, Pop_classification == "FSWs")

class_prob <- subset(pop_class, QA_Sampling == "Probability")
class_nonprob <- subset(pop_class, QA_Sampling == "Non-probability")

summary(class_prob$prevalence)

meta_prob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_prob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "FSWs - Probability sampling"
)

summary(class_nonprob$prevalence)
meta_nonprob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_nonprob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "FSWs - Non-probability sampling"
)

print(meta_prob)
print(meta_nonprob)


##### sera ######

library(readxl)
library(meta)

sera <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "sera")
pop_class <- subset(sera, Pop_classification == "FSWs")

class_nonprob <- subset(pop_class, QA_Sampling == "Non-probability")


summary(class_nonprob$prevalence)
meta_nonprob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_nonprob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "FSWs - Non-probability sampling"
)

print(meta_nonprob)


##### mixed ######

library(readxl)
library(meta)

mixed <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "mixed")
pop_class <- subset(mixed, Pop_classification == "FSWs")

class_prob <- subset(pop_class, QA_Sampling == "Probability")
class_nonprob <- subset(pop_class, QA_Sampling == "Non-probability")

summary(class_prob$prevalence)

meta_prob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_prob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "FSWs - Probability sampling"
)

summary(class_nonprob$prevalence)
meta_nonprob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_nonprob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "FSWs - Non-probability sampling"
)

print(meta_prob)
print(meta_nonprob)












#### 3 #### MSM 
**************************
  ##### Uro ######

library(readxl)
library(meta)

urogenital <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "uro")
pop_class <- subset(urogenital, Pop_classification == "MSM")

class_prob <- subset(pop_class, QA_Sampling == "Probability")
class_nonprob <- subset(pop_class, QA_Sampling == "Non-probability")

summary(class_prob$prevalence)

meta_prob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_prob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "MSM - Probability sampling"
)

summary(class_nonprob$prevalence)
meta_nonprob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_nonprob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "MSM - Non-probability sampling"
)

print(meta_prob)
print(meta_nonprob)


##### Ano ######

ano <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "ano")
pop_class <- subset(ano, Pop_classification == "MSM")

class_prob <- subset(pop_class, QA_Sampling == "Probability")
class_nonprob <- subset(pop_class, QA_Sampling == "Non-probability")

summary(class_prob$prevalence)

meta_prob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_prob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "MSM - Probability sampling"
)

summary(class_nonprob$prevalence)
meta_nonprob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_nonprob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "MSM - Non-probability sampling"
)

print(meta_prob)
print(meta_nonprob)



##### Oro ######

library(readxl)
library(meta)


oro <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "oro")
pop_class <- subset(oro, Pop_classification == "MSM")

class_prob <- subset(pop_class, QA_Sampling == "Probability")
class_nonprob <- subset(pop_class, QA_Sampling == "Non-probability")

summary(class_prob$prevalence)

meta_prob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_prob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "MSM - Probability sampling"
)

summary(class_nonprob$prevalence)
meta_nonprob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_nonprob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "MSM - Non-probability sampling"
)

print(meta_prob)
print(meta_nonprob)






##### mixed ######

library(readxl)
library(meta)

mixed <- read_excel("NG in Africa-Final Data Cleaning-Bayan.xlsx", sheet = "mixed")
pop_class <- subset(mixed, Pop_classification == "MSM")

class_prob <- subset(pop_class, QA_Sampling == "Probability")
class_nonprob <- subset(pop_class, QA_Sampling == "Non-probability")

summary(class_prob$prevalence)

meta_prob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_prob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "MSM - Probability sampling"
)

summary(class_nonprob$prevalence)
meta_nonprob <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = class_nonprob,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "MSM - Non-probability sampling"
)

print(meta_prob)
print(meta_nonprob)








************************************************************************************************************************************************
  
  
  
  library(readxl)
library(meta)


mixed <- read_excel("General population_mixed specimen_non-probability sampling.xlsx")

summary(mixed$prevalence)
meta_mixed <- metaprop(
  event = event,
  n = n,
  studlab = Name,
  data = mixed,
  sm = "PFT",
  random = TRUE,
  prediction = TRUE,
  title = "General population - Non-probability sampling"
)

print(meta_mixed)






**************************************************************************************
  ********************************************************************************
  Geographic Distribution of Prevalence Measures
***************************************************************
  
  install.packages(c("sf", "rnaturalearth", "rnaturalearthdata",
                     "ggplot2", "dplyr", "readxl"))
install.packages("ggrepel")
remotes::install_github("ropensci/rnaturalearthhires")


library(sf)
library(ggplot2)
library(dplyr)
library(readxl)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthhires)
library(ragg)


# 1. Read Excel file
data <- read_excel(file.choose(), sheet = 1)

# 2. Create prevalence count categories
data <- data %>%
  mutate(
    count_cat = cut(
      prevalence,
      breaks = c(-Inf, 0, 5, 15, 40, 80, 150, Inf),
      labels = c(
        "No measures", "1–5 measures", "6–15 measures",
        "16–40 measures", "41–80 measures",
        "81–150 measures", ">150 measures"
      ),
      right = TRUE
    )
  )

# Use high-resolution Natural Earth boundaries
world <- ne_countries(scale = "large", returnclass = "sf")

africa <- world %>%
  filter(continent == "Africa",
         admin != "Bir Tawil") %>%
  st_make_valid()

map_data <- africa %>%
  left_join(data, by = c("admin" = "country")) %>%
  mutate(
    prevalence = ifelse(is.na(prevalence), 0, prevalence),
    count_cat = ifelse(is.na(count_cat), "No measures", as.character(count_cat)),
    count_cat = factor(
      count_cat,
      levels = c(
        "No measures", "1–5 measures", "6–15 measures",
        "16–40 measures", "41–80 measures",
        "81–150 measures", ">150 measures"
      )
    )
  )

label_data <- map_data %>%
  st_point_on_surface() %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2],
    label = admin
  ) %>%
  st_drop_geometry()

# Manually improve crowded labels
label_data <- label_data %>%
  mutate(
    x = case_when(
      label == "Cabo Verde" ~ -26,
      label == "The Gambia" ~ -18.5,
      label == "Guinea-Bissau" ~ -18.5,
      label == "Sierra Leone" ~ -17.5,
      label == "Liberia" ~ -16.5,
      label == "Togo" ~ 1.5,
      label == "Benin" ~ 3.8,
      label == "Equatorial Guinea" ~ 7.5,
      label == "São Tomé and Principe" ~ 7.0,
      label == "Rwanda" ~ 34.5,
      label == "Burundi" ~ 34.5,
      label == "Malawi" ~ 36.5,
      label == "Comoros" ~ 47.5,
      label == "Mauritius" ~ 59,
      label == "Seychelles" ~ 56,
      TRUE ~ x
    ),
    y = case_when(
      label == "Cabo Verde" ~ 16.0,
      label == "The Gambia" ~ 13.5,
      label == "Guinea-Bissau" ~ 11.6,
      label == "Sierra Leone" ~ 8.8,
      label == "Liberia" ~ 6.5,
      label == "Togo" ~ 6.2,
      label == "Benin" ~ 6.7,
      label == "Equatorial Guinea" ~ 1.5,
      label == "São Tomé and Principe" ~ -1.5,
      label == "Rwanda" ~ -1.4,
      label == "Burundi" ~ -3.5,
      label == "Malawi" ~ -13.5,
      label == "Comoros" ~ -11.5,
      label == "Mauritius" ~ -20.0,
      label == "Seychelles" ~ -4.5,
      TRUE ~ y
    )
  )

p <- ggplot(map_data) +
  geom_sf(
    aes(fill = count_cat),
    color = "grey55",
    linewidth = 0.25
  ) +
  geom_text_repel(
    data = label_data,
    aes(x = x, y = y, label = label),
    size = 3,
    color = "black",
    fontface = "bold",
    max.overlaps = Inf,
    min.segment.length = 0,
    segment.color = "grey55",
    box.padding = 0.25,
    point.padding = 0.15
  ) +
  coord_sf(
    xlim = c(-27, 61),
    ylim = c(-36, 38),
    expand = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "No measures" = "#D9D9D9",
      "1–5 measures" = "#FDD49E",
      "6–15 measures" = "#C7E9C0",
      "16–40 measures" = "#41B6C4",
      "41–80 measures" = "#2C7FB8",
      "81–150 measures" = "#7A5195",
      ">150 measures" = "#6A006A"
    ),
    name = "Overall count of prevalence measures (n)",
    drop = FALSE
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.18, 0.20),
    legend.background = element_rect(color = "black", fill = "white"),
    legend.margin = margin(t = 10, r = 10, b = 10, l = 10),
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    plot.caption = element_text(size = 8, hjust = 0)
  ) +
  labs(
    caption = paste0(
      "Abbreviations: NG, Neisseria gonorrhoeae.\n",
      "Source: Authors’ analysis using Natural Earth public-domain country boundary data."
    )
  )

p

ggsave(
  "S1_Fig_Geographic_distribution_NG_prevalence_300dpi.tiff",
  plot = p,
  device = ragg::agg_tiff,
  width = 10,
  height = 7,
  units = "in",
  dpi = 300,
  compression = "lzw",
  background = "white"
)
