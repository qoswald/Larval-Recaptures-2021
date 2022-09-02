####################################################################################################################################################################################################

#Rscript to the paper: "Population monitoring of fire salamanders (Salamandra salamandra) with a new photo-recognition software "
#authors: P. Oswald, B. Tunnat & B. A. Caspers
#created: 27-04-2022
#last modified: 

####################################################################################################################################################################################################

# 1. LARVAL RECAPTURE ANALYSES TO OBTAIN RECAPTURE RATES, SURVIVAL, ESTIMATED POPULATION SIZE

# needed packages
library(R2ucare) # to test for goodness of fit (underlying assumptions)
library(dplyr) # for tidy data
library(magrittr) # for pipes
library(RMark)
MarkPath="D:/UniBieProgramme/MARK" # set path to were the Mark pogram is saved on your computer

########## ponds
##### KoVK 
# based on weekly averages
# upload and manipulate data
KoVK=read.delim("KoVK.txt",colClass=c("character","character"))
KoVKmut <- mutate(KoVK, across(c(ID), as.factor))
str(KoVKmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
KoVK.matrix <- KoVKmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(KoVKmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(KoVK.matrix, rep(1,nrow(KoVKmut)))
# if p>0.05 no evidence for lack of fit
# process data
KoVK.proc <- process.data(KoVKmut, model = "POPAN")
# Smake design data (from processed data)
KoVK.dd <- make.design.data(KoVK.proc)
fit.KoVK.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = KoVK.proc, ddl = KoVK.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
KoVK.models <- fit.KoVK.model()
KoVK.models
summary(KoVK.models[[2]], se=TRUE)
KoVK.models[[2]]$results$derived

##### SG
# based on weekly averages
# upload and manipulate data
SG=read.delim("SG.txt",colClass=c("character","character"))
SGmut <- mutate(SG, across(c(ID), as.factor))
str(SGmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
SG.matrix <- SGmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(SGmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(SG.matrix, rep(1,nrow(SGmut)))
# if p>0.05 no evidence for lack of fit
# process data
SG.proc <- process.data(SGmut, model = "POPAN")
# Smake design data (from processed data)
SG.dd <- make.design.data(SG.proc)
fit.SG.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = SG.proc, ddl = SG.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
SG.models <- fit.SG.model()
SG.models
summary(SG.models[[2]], se=TRUE)
SG.models[[2]]$results$derived

##### TG
# based on weekly averages
# upload and manipulate data
TG=read.delim("TG.txt",colClass=c("character","character"))
TGmut <- mutate(TG, across(c(ID), as.factor))
str(TGmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
TG.matrix <- TGmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(TGmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(TG.matrix, rep(1,nrow(TGmut)))
# if p>0.05 no evidence for lack of fit
# process data
TG.proc <- process.data(TGmut, model = "POPAN")
# Smake design data (from processed data)
TG.dd <- make.design.data(TG.proc)
fit.TG.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = TG.proc, ddl = TG.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
TG.models <- fit.TG.model()
TG.models
summary(TG.models[[2]], se=TRUE)
TG.models[[2]]$results$derived

##### TT
# based on weekly averages
# upload and manipulate data
TT=read.delim("TT.txt",colClass=c("character","character"))
TTmut <- mutate(TT, across(c(ID), as.factor))
str(TTmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
TT.matrix <- TTmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(TTmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(TT.matrix, rep(1,nrow(TTmut)))
# if p>0.05 no evidence for lack of fit
# process data
TT.proc <- process.data(TTmut, model = "POPAN")
# Smake design data (from processed data)
TT.dd <- make.design.data(TT.proc)
fit.TT.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = TT.proc, ddl = TT.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
TT.models <- fit.TT.model()
TT.models
summary(TT.models[[2]], se=TRUE)
TT.models[[2]]$results$derived

#########+ streams
##### KB
# based on weekly averages
# upload and manipulate data
KB=read.delim("KB.txt",colClass=c("character","character"))
KBmut <- mutate(KB, across(c(ID), as.factor))
str(KBmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
KB.matrix <- KBmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(KBmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(KB.matrix, rep(1,nrow(KBmut)))
# if p>0.05 no evidence for lack of fit
# process data
KB.proc <- process.data(KBmut, model = "POPAN")
# Smake design data (from processed data)
KB.dd <- make.design.data(KB.proc)
fit.KB.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = KB.proc, ddl = KB.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
KB.models <- fit.KB.model()
KB.models
summary(KB.models[[2]], se=TRUE)
KB.models[[2]]$results$derived

##### KoB
# based on weekly averages
# upload and manipulate data
KoB=read.delim("KoB.txt",colClass=c("character","character"))
KoBmut <- mutate(KoB, across(c(ID), as.factor))
str(KoBmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
KoB.matrix <- KoBmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(KoBmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(KoB.matrix, rep(1,nrow(KoBmut)))
# if p>0.05 no evidence for lack of fit
# process data
KoB.proc <- process.data(KoBmut, model = "POPAN")
# Smake design data (from processed data)
KoB.dd <- make.design.data(KoB.proc)
fit.KoB.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = KoB.proc, ddl = KoB.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
KoB.models <- fit.KoB.model()
KoB.models
summary(KoB.models[[2]], se=TRUE)
KoB.models[[2]]$results$derived

##### MB
# based on weekly averages
# upload and manipulate data
MB=read.delim("MB.txt",colClass=c("character","character"))
MBmut <- mutate(MB, across(c(ID), as.factor))
str(MBmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
MB.matrix <- MBmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(MBmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(MB.matrix, rep(1,nrow(MBmut)))
# if p>0.05 no evidence for lack of fit
# process data
MB.proc <- process.data(MBmut, model = "POPAN")
# Smake design data (from processed data)
MB.dd <- make.design.data(MB.proc)
fit.MB.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = MB.proc, ddl = MB.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
MB.models <- fit.MB.model()
MB.models
summary(MB.models[[2]], se=TRUE)
MB.models[[2]]$results$derived

##### VB
# based on weekly averages
# upload and manipulate data
VB=read.delim("VB.txt",colClass=c("character","character"))
VBmut <- mutate(VB, across(c(ID), as.factor))
str(VBmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
VB.matrix <- VBmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(VBmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(VB.matrix, rep(1,nrow(VBmut)))
# if p>0.05 no evidence for lack of fit
# process data
VB.proc <- process.data(VBmut, model = "POPAN")
# Smake design data (from processed data)
VB.dd <- make.design.data(VB.proc)
fit.VB.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = VB.proc, ddl = VB.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
VB.models <- fit.VB.model()
VB.models
summary(VB.models[[2]], se=TRUE)
VB.models[[2]]$results$derived


####################################################################################################################################################################################################

# 2. TEMPERATURE REGIMES IN PONDS AND STREAMS 
## import rawdata
Sys.setlocale("LC_TIME", "English") # this helps R to read the right date format from your raw data, if formatted as yyyy-mm-dd
library(readxl)
overall. <- read_excel("2021_Meandata-Mastertable.xlsx", na="NA",
                       col_types = c("text", "text", "numeric", "date","date", "text", "text", "numeric", 
                                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric"))

## set categories
library(dplyr)
overall.dat <- mutate(overall., across(c(sample.site:year, session:observer), as.factor))
str(overall.dat)

## prepare and inspect data for models
water.sub<- subset(overall.dat, sample.site!="KoB") # exclude KoB, because it's not really a stream nor a pond
str(water.sub)
hist(water.sub$n.total)

## check for normal distribution
plot(water.sub$habitat, water.sub$water.temp)
hist(water.sub$water.temp)
shapiro.test(water.sub$water.temp) # if > 0.05, i.e. NOT significant -> good, normal distribution
var.test(water.sub$water.temp ~ water.sub$habitat) # if > 0.05, i.e. NOT significant -> good, assumption of homogeneity fulfilled

## if data is not normally distributed, try to transform data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_water <- arcsinh_x(water.sub$water.temp))
(boxcox_water <- boxcox(water.sub$water.temp))
(centerscale_water <- center_scale(water.sub$water.temp))
(orderNorm_water <- orderNorm(water.sub$water.temp))
(yeojohnson_water <- yeojohnson(water.sub$water.temp))
(sqrt_water <- sqrt(water.sub$water.temp))
(log_water <- log(water.sub$water.temp))

# then have a look at the histograms of the different transformations for first impression
par(mfrow = c(2,4)) # display all histograms in one window
hist(water.sub$water.temp)
MASS::truehist(arcsinh_water$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(boxcox_water$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(centerscale_water$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(orderNorm_water$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(yeojohnson_water$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(sqrt_water, main = "squareroot transformation", nbins = 12)
MASS::truehist(log_water, main = "log transformation", nbins = 12)

# let R suggest best transformation
bn.water<-bestNormalize(water.sub$water.temp, out_of_sample = FALSE) # out_of_sample = do not perform repeated cross-validation; otherwise you'll get different results every time you run this code
bn.water 

## create an object for the best transformation using the following expression (e.g.) any.name <- orderNorm$x.t  # (i.e result of best normalisation)
(orderNorm_water <- orderNorm(water.sub$water.temp))
on.water <- orderNorm_water$x.t

## add the new object as column to your data frame
water.transformed <- cbind(water.sub, on.water)
hist(water.transformed$on.water)

## test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(water.transformed$on.water)
var.test(water.transformed$on.water ~ water.transformed$habitat)

# run model and investigate model output
library(lme4)
library(lmerTest)
library(performance)
modwat<-lmer(on.water ~ habitat + (1|sample.site/year) + (1|session/year), data=water.transformed)
modwat1<-lmer(on.water ~ sample.site + (1|session/year), data=water.transformed)
summary(modwat)
summary(modwat1)
AIC(modwat,modwat1) # modwat most supported
compare_performance(modwat, modwat1,rank=T) # modwat most supported

# check model
check_model(modwat)

# plot as boxplot
library(plyr)
library(ggpubr) 
count(water.sub, "habitat")

setwd("D:/Plots/Mark_recapture")
png("watertemp.png", height=150, width=200, units="mm", res=300);print(watertemp) #save plot on local harddrive
watertemp<-
  ggplot(data=water.sub, aes(habitat,water.temp), fill=habitat)+
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  stat_summary(fun=mean, shape=8, show.legend=FALSE) +
  scale_x_discrete(labels=c("Pond (N=88)","Stream (N=66)"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Water temperature (°C)")+
  scale_y_continuous(breaks=seq(0,25,5), limits = c(0, 25))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
dev.off()


####################################################################################################################################################################################################

# 3. NUMBER OF LARVAE

## import rawdata
Sys.setlocale("LC_TIME", "English") # this helps R to read the right date format from your raw data, if formatted as yyyy-mm-dd
library(readxl)
overall. <- read_excel("2021_Meandata-Mastertable.xlsx", na="NA",
                       col_types = c("text", "text", "numeric", "date","date", "text", "text", "numeric", 
                                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric"))

## set categories
library(dplyr)
overall.dat <- mutate(overall., across(c(sample.site:year, session:observer), as.factor))
str(overall.dat)

## prepare and inspect data for models
count.sub<- subset(overall.dat, sample.site!="KoB") # exclude KoB, because it's not really a stream nor a pond
str(count.sub)
hist(count.sub$n.total)

## find best transformation for data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_count <- arcsinh_x(count.sub$n.total))
(boxcox_count <- boxcox(count.sub$n.total))
(centerscale_count <- center_scale(count.sub$n.total))
(orderNorm_count <- orderNorm(count.sub$n.total))
(yeojohnson_count <- yeojohnson(count.sub$n.total))
(sqrt_count <- sqrt(count.sub$n.total))
(log_count <- log(count.sub$n.total))
# then have a look at the histograms of the different transformations for first impression
par(mfrow = c(2,4)) # display all histograms in one window
hist(count.sub$n.total)
MASS::truehist(arcsinh_count$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(boxcox_count$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(centerscale_count$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(orderNorm_count$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(yeojohnson_count$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(sqrt_count, main = "squareroot transformation", nbins = 12)
MASS::truehist(log_count, main = "log transformation", nbins = 12)
# let R recommend the most suitable transformation method
bn.count<-bestNormalize(count.sub$n.total, out_of_sample = FALSE) 
bn.count 

## create an object for the best transformation
on.count <- orderNorm_count$x.t # ordernorm transformation

## add the new object as column to your data frame
count.transformed <- cbind(count.sub, on.count)
str(count.transformed)
hist(count.transformed$on.count)

# plot number of larvae as boxplot just for inspection
library(plyr)
library(ggpubr) 
count(count.transformed, "habitat")
hist(count.transformed$n.total)
shapiro.test(count.transformed$n.total) # non-normal
var.test(count.transformed$n.total ~ count.transformed$habitat) #okay
# since assumptions not fulfilled use non-parametric test to compare means -> Wilcoxon

setwd("D:/Work/Work/Plots/Mark_recapture")
png("countboxplot.png", height=150, width=200, units="mm", res=300);print(countplot)
countplot<-
  ggplot(data=count.transformed, aes(habitat,n.total), fill=habitat)+
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21)+
  facet_wrap(~year)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  stat_summary(fun=mean, shape=8, show.legend=FALSE) +
  scale_x_discrete(labels=c("Pond (N=57)","Stream (N=61)"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Number of larvae")+
  scale_y_continuous(breaks=seq(0,100,20), limits = c(0, 100))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
dev.off()


## Plot graphs for each sample site
library(ggplot2)

count.KB<-subset(overall.dat, sample.site=="KB")
str(count.KB)
p1<-ggplot(count.KB, aes(x=date.simplified, y=n.total, group=year, colour=year, shape=year)) +
  ggtitle("Stream KB")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_color_manual(values=c("steelblue4", "darkseagreen4","goldenrod"))+
  scale_shape_manual(values=c(16,4,5))+
  scale_y_continuous(breaks=seq(0,90,15), limits = c(0, 90))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Number of larvae (count)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

count.VB<-subset(overall.dat, sample.site=="VB")
p2<-ggplot(count.VB, aes(x=date.simplified, y=n.total, group=year, colour=year, shape=year)) +
  ggtitle("Stream VB")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_color_manual(values=c("steelblue4", "darkseagreen4","goldenrod"))+
  scale_shape_manual(values=c(16,4,5))+
  scale_y_continuous(breaks=seq(0,90,15), limits = c(0, 90))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Number of larvae (count)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

count.MB<-subset(overall.dat, sample.site=="MB")
p3<-ggplot(count.MB, aes(x=date.simplified, y=n.total, group=year, colour=year, shape=year)) +
  ggtitle("Stream KoB")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_color_manual(values=c("steelblue4", "darkseagreen4", "goldenrod"))+
  scale_shape_manual(values=c(16,4,5))+
  scale_y_continuous(breaks=seq(0,90,15), limits = c(0, 90))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Number of larvae (count)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

count.KoB<-subset(overall.dat, sample.site=="KoB")
p4<-ggplot(count.KoB, aes(x=date.simplified, y=n.total, group=year, colour=year, shape=year)) +
  ggtitle("Stream MB")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("steelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(0,90,15), limits = c(0, 90))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Number of larvae (count)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

count.KoVK<-subset(overall.dat, sample.site=="KoVK")
p5<-ggplot(count.KoVK, aes(x=date.simplified, y=n.total, group=year, colour=year, shape=year)) +
  ggtitle("Pond KoVK")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("steelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(0,90,15), limits = c(0, 90))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Number of larvae (count)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

count.TG<-subset(overall.dat, sample.site=="TG")
p6<-ggplot(count.TG, aes(x=date.simplified, y=n.total, group=year, colour=year, shape=year)) +
  ggtitle("Pond TG")+geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("steelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(0,90,15), limits = c(0, 90))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Number of larvae (count)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

count.SG<-subset(overall.dat, sample.site=="SG")
p7<-ggplot(count.SG, aes(x=date.simplified, y=n.total, group=year, colour=year, shape=year)) +
  ggtitle("Pond SG")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("steelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(0,90,15), limits = c(0, 90))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Number of larvae (count)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

count.TT<-subset(overall.dat, sample.site=="TT")
p8<-ggplot(count.TT, aes(x=date.simplified, y=n.total, group=year, colour=year, shape=year)) +
  ggtitle("Pond TT")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("steelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(0,90,15), limits = c(0, 90))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Number of larvae (count)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="bottom",
        legend.title =element_text(family="Arial", size=12, color="black"),
        legend.text = element_text(family="Arial", size=12, color="black"),
        axis.title.x=element_blank(),)


# extract legend for multiplot 
get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

legend1<- get_only_legend(p8)


# arrange margins of plot 
figi<-p1+
  theme(plot.margin = unit(c(0.55,0,0.7,0.7), "cm"),
        plot.title=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

figj<-p2+
  theme(plot.margin = unit(c(0.55,0.25,0.7,0.45), "cm"),
        plot.title=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())

figk<-p3+
  theme(plot.margin = unit(c(0.55,0.3,0.7,0.25), "cm"),
        plot.title=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())

figl<-p4+
  theme(plot.margin = unit(c(0.55,0.75,0.7,0.2), "cm"),
        plot.title=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())

figm<-p5+
  theme(plot.margin = unit(c(0.2,0,1.2,0.7), "cm"),
        axis.title.y=element_blank(),
        plot.title=element_blank(),
        axis.text.x=element_blank())

fign<-p6+
  theme(plot.margin = unit(c(0.2,0.25,1.2,0.45), "cm"),
        plot.title=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())

figo<-p7+
  theme(plot.margin = unit(c(0.2,0.3,1.2,0.25), "cm"),
        plot.title=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())

figp<-p8+
  theme(plot.margin = unit(c(0.2,0.75,1.2,0.2), "cm"),
        plot.title=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        legend.position = "none")


# create multiplot
library(gridExtra)
library(grid)
setwd("D:/Plots/Mark_recapture")
Multiplot1<-grid.arrange(figi, figj, figk, figl, figm, fign, figo, figp, ncol=4)
png("multiplot-count.png", height=150, width=200, units="mm", res=300);print(Multiplot1)
Multiplot1a<-grid.arrange(Multiplot1, legend1, ncol=1, heights = c(10, 1))
grid.text("Stream KB", x = unit(0.16, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Stream VB", x = unit(0.375, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Stream MB", x = unit(0.63, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("(Stream) KoB", x = unit(0.86, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Pond KoVK", x = unit(0.16, "npc"), y = unit(0.53, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Pond TG", x = unit(0.375, "npc"), y = unit(0.53, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Pond SG", x = unit(0.63, "npc"), y = unit(0.53, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Pond TT", x = unit(0.86, "npc"), y = unit(0.53, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.084, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.123, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.161, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.201, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.245, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.294, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.34, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.385, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.431, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.4805, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.5349, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.5819, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.631, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.677, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.729, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.782, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.824, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.867, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.909, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.957, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.084, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.123, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.161, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.201, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.245, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.294, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.34, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.385, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.431, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.4805, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.5349, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.5819, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.631, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.677, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.729, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.782, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.824, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.867, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.909, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.957, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Month", x = unit(0.51, "npc"), y = unit(0.11, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Observed number of larvae", x = unit(0.013, "npc"), y = unit(0.35, "npc"), rot=90, gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Observed number of larvae", x = unit(0.013, "npc"), y = unit(0.79, "npc"), rot=90, gp=gpar(fontfamily="Arial", fontsize=12))
dev.off()


####################################################################################################################################################################################################

# 4. MEAN LARVAL SIZES

# import rawdata
Sys.setlocale("LC_TIME", "English") # this helps R to read the right date format from your raw data, if formatted as yyyy-mm-dd
library(readxl)
overall. <- read_excel("2021_Meandata-Mastertable.xlsx", na="NA",
                       col_types = c("text", "text", "numeric", "date","date", "text", "text", "numeric", 
                                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric"))

## set categories
library(dplyr)
overall.dat <- mutate(overall., across(c(sample.site:year, session:observer), as.factor))
str(overall.dat)

# prepare and inspect data
mean.sub<-subset(overall.dat, sample.site!="KoB")
# no need to remove "ssfs", because this was already considered in the raw-data, mean size was calculated without "ssfs"
str(mean.sub)
hist(mean.sub$mean.size)

## find best transformation for data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_mean <- arcsinh_x(mean.sub$mean.size))
(boxcox_mean <- boxcox(mean.sub$mean.size))
(centerscale_mean <- center_scale(mean.sub$mean.size))
(orderNorm_mean <- orderNorm(mean.sub$mean.size))
(yeojohnson_mean <- yeojohnson(mean.sub$mean.size))
(sqrt_mean <- sqrt(mean.sub$mean.size))
(log_mean <- log(mean.sub$mean.size))

# then have a look at the histograms of the different transformations for first impression
par(mfrow = c(2,4)) # display all histograms in one window
hist(mean.sub$mean.size)
MASS::truehist(arcsinh_mean$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(boxcox_mean$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(centerscale_mean$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(orderNorm_mean$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(yeojohnson_mean$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(sqrt_mean, main = "squareroot transformation", nbins = 12)
MASS::truehist(log_mean, main = "log transformation", nbins = 12)
# let R recommend the most suitable transformation method
bn.mean<-bestNormalize(mean.sub$mean.size, out_of_sample = FALSE) 
bn.mean 

## create an object for the best transformation
on.mean <- orderNorm_mean$x.t # ordernorm transformation

## add the new object as column to your data frame
mean.transformed <- cbind(mean.sub, on.mean)
str(mean.transformed)
hist(mean.transformed$on.mean)

## test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(mean.transformed$on.mean) 
var.test(mean.transformed$on.mean ~ mean.transformed$habitat) 

# set up models
library(lme4)
library(lmerTest)
library(performance)

msize0<-lm(on.mean~habitat, data=mean.transformed)
msize1<-lmer(on.mean~habitat+(1|sample.site)+(1|session/year), data=mean.transformed)
msize2<-lmer(on.mean~habitat+ water.temp + (1|sample.site)+(1|session/year), data=mean.transformed)
summary(msize0)
summary(msize1)
summary(msize2)

# check models
AIC(msize0,msize1, msize2) # msize 2 has lowest AIC
check_model(msize2) 
compare_performance(msize0,msize1, msize2, rank = T) # msize2 best performance score

# plot mean larval size as boxplot over all years and all sample sites
library(plyr)
library(ggpubr) 
count(mean.sub, "habitat")
hist(mean.sub$mean.size)
shapiro.test(mean.sub$mean.size) # non-normal
var.test(mean.sub$mean.size ~ mean.sub$habitat) #okay

setwd("D:/Plots/Mark_recapture")
png("sizeplot.png", height=150, width=200, units="mm", res=300);print(sizeplot)
sizeplot<-
  ggplot(data=mean.sub, aes(habitat,mean.size), fill=habitat)+
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  annotate("text", x = 1.5, y =5, label = "n.s.")+
  stat_summary(fun=mean, shape=8, show.legend=FALSE) +
  scale_x_discrete(labels=c("Pond (N=88)","Stream (N=66)"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Mean larval size (cm)")+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
dev.off()


## plot graphs for each sample site
mean.KB<-subset(overall.dat, sample.site=="KB")
ps1<-ggplot(mean.KB, aes(x=date.simplified, y=mean.size, group=year, colour=year,shape=year)) +
  ggtitle("Stream KB")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Mean larval size (cm)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

mean.VB<-subset(overall.dat, sample.site=="VB")
ps2<-ggplot(mean.VB, aes(x=date.simplified, y=mean.size, group=year, colour=year, shape=year)) +
  ggtitle("Stream VB")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Mean larval size (cm)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

mean.MB<-subset(overall.dat, sample.site=="MB")
ps3<-ggplot(mean.MB, aes(x=date.simplified, y=mean.size, group=year, colour=year, shape=year)) +
  ggtitle("Stream MB")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Mean larval size (cm)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

mean.KoB<-subset(overall.dat, sample.site=="KoB")
ps4<-ggplot(mean.KoB, aes(x=date.simplified, y=mean.size, group=year, colour=year, shape=year)) +
  ggtitle("Stream KoB")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4", "goldenrod"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Mean larval size (cm)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

mean.KoVK<-subset(overall.dat, sample.site=="KoVK")
ps5<-ggplot(mean.KoVK, aes(x=date.simplified, y=mean.size, group=year, colour=year, shape=year)) +
  ggtitle("Pond KoVK")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Mean larval size (cm)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

mean.TG<-subset(overall.dat, sample.site=="TG")
ps6<-ggplot(mean.TG, aes(x=date.simplified, y=mean.size, group=year, colour=year, shape=year)) +
  ggtitle("Pond TG")+geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Mean larval size (cm)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

mean.SG<-subset(overall.dat, sample.site=="SG")
ps7<-ggplot(mean.SG, aes(x=date.simplified, y=mean.size, group=year, colour=year, shape=year)) +
  ggtitle("Pond SG")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Mean larval size (cm)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

mean.TT<-subset(overall.dat, sample.site=="TT")
ps8<-ggplot(mean.TT, aes(x=date.simplified, y=mean.size, group=year, colour=year, shape=year)) +
  ggtitle("Pond TT")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Mean larval size (cm)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="bottom",
        legend.title =element_text(family="Arial", size=12, color="black"),
        legend.text = element_text(family="Arial", size=12, color="black"),
        axis.title.x=element_blank(),)

# extract legend for multiplot
legend2 <- get_only_legend(ps8)


# arrange margins of plots 
fps1<-ps1+
  theme(plot.margin = unit(c(0.55,0,0.7,0.7), "cm"),
        plot.title=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

fps2<-ps2+
  theme(plot.margin = unit(c(0.55,0.25,0.7,0.45), "cm"),
        plot.title=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())

fps3<-ps3+
  theme(plot.margin = unit(c(0.55,0.3,0.7,0.25), "cm"),
        plot.title=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())

fps4<-ps4+
  theme(plot.margin = unit(c(0.55,0.75,0.7,0.2), "cm"),
        plot.title=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())

fps5<-ps5+
  theme(plot.margin = unit(c(0.2,0,1.2,0.7), "cm"),
        axis.title.y=element_blank(),
        plot.title=element_blank(),
        axis.text.x=element_blank())

fps6<-ps6+
  theme(plot.margin = unit(c(0.2,0.25,1.2,0.45), "cm"),
        plot.title=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())

fps7<-ps7+
  theme(plot.margin = unit(c(0.2,0.3,1.2,0.25), "cm"),
        plot.title=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())

fps8<-ps8+
  theme(plot.margin = unit(c(0.2,0.75,1.2,0.2), "cm"),
        plot.title=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        legend.position = "none")

# create multiplot
library(gridExtra)
library(grid)
setwd("D:/Plots/Mark_recapture")
Multiplot2<-grid.arrange(fps1, fps2, fps3, fps4, fps5, fps6, fps7, fps8, ncol=4)
png("multiplot-meansize.png", height=150, width=200, units="mm", res=300);print(Multiplot2)
Multiplot2a<-grid.arrange(Multiplot2, legend2, ncol=1, heights = c(10, 1))
grid.text("Stream KB", x = unit(0.155, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Stream VB", x = unit(0.375, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Stream MB", x = unit(0.63, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("(Stream) KoB", x = unit(0.86, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Pond KoVK", x = unit(0.155, "npc"), y = unit(0.53, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Pond TG", x = unit(0.375, "npc"), y = unit(0.53, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Pond SG", x = unit(0.63, "npc"), y = unit(0.53, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Pond TT", x = unit(0.86, "npc"), y = unit(0.53, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.0725, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.1145, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.156, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.198, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.244, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.294, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.34, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.385, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.431, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.4805, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.5349, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.5819, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.631, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.677, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.729, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.782, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.824, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.867, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.909, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.957, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.0725, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.1145, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.156, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.198, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.244, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.294, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.34, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.385, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.431, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.4805, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.5349, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.5819, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.631, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.677, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.729, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.782, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.824, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.867, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.909, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.957, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Month", x = unit(0.51, "npc"), y = unit(0.11, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Snout-to-tail length (cm)", x = unit(0.013, "npc"), y = unit(0.35, "npc"), rot=90, gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Snout-to-tail length (cm)", x = unit(0.013, "npc"), y = unit(0.79, "npc"), rot=90, gp=gpar(fontfamily="Arial", fontsize=12))
dev.off()


# create another plot for the mean sizes in ponds and streams per year
library(ggplot2)

library(tidyverse)
df.mean1<-aggregate(x=mean.sub$mean.size,na.rm=TRUE,
                    by=list(mean.sub$habitat,mean.sub$session, mean.sub$year),
                    FUN=mean)

# rename columns
df.mean<-df.mean1 %>% 
  rename(
    habitat = Group.1,
    session = Group.2,
    year =Group.3,
    mean.size = x
  )
str(df.mean)


setwd("D:/Plots/Mark_recapture")
png("meansizeperyear.png", height=150, width=200, units="mm", res=300);print(meanyear)
meanyear<-
  ggplot(data=df.mean, aes(x=factor(session,level = c('1', '2', '3',"4","5", "6","7","8", "9","10","11","12")), 
                           y=mean.size, group=habitat, colour=habitat, shape=habitat)) +
  geom_line(aes(color=habitat), size=0.5)+
  geom_point(aes(color=habitat))+
  facet_wrap(~year)+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("steelblue4", "lightsteelblue3"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  scale_fill_discrete(name = "habitat")+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Mean larval size (cm)", x="Monitoring event")+
  theme(axis.text.y=element_text(family="Arial", size=12, color="black"),
        axis.title.y=element_text(family="Arial", size=12, color="black"),
        axis.text.x=element_text(family="Arial", size=12, color="black"),
        axis.title.x=element_text(family="Arial", size=12, color="black"),
        legend.position = "bottom",
        legend.title =element_text(family="Arial", size=12, color="black"),
        legend.text = element_text(family="Arial", size=12, color="black"),)
dev.off()



####################################################################################################################################################################################################

# 5. INDIVIDUAL GROWTH 
# import rawdata
library(readxl)
ind.dat <- read_excel("Ind-sizes.xlsx", na="NA",
                       col_types = c("text", "text", "text","numeric","date","numeric","text","numeric",
                                     "numeric","numeric","numeric","text")) 

## set categories
library(dplyr)
ind.size <- mutate(ind.dat, across(c(ID:sample.site, observer, note), as.factor))
str(ind.size)
    
# prepare and inspect data
ind.size1<-subset(ind.size, sample.site!="KoB")
ind.sub<-subset(ind.size1, note!="ssf")
hist(ind.sub$daily.growth)

## find best transformation for data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_ind <- arcsinh_x(ind.sub$daily.growth))
(boxcox_ind <- boxcox(ind.sub$daily.growth))
(centerscale_ind <- center_scale(ind.sub$daily.growth))
(orderNorm_ind <- orderNorm(ind.sub$daily.growth))
(yeojohnson_ind <- yeojohnson(ind.sub$daily.growth))
(sqrt_ind <- sqrt(ind.sub$daily.growth))
(log_ind <- log(ind.sub$daily.growth))

# then have a look at the histograms of the different transformations for first impression
par(mfrow = c(2,4)) # display all histograms in one window
hist(ind.sub$daily.growth)
MASS::truehist(arcsinh_ind$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(boxcox_ind$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(centerscale_ind$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(orderNorm_ind$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(yeojohnson_ind$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(sqrt_ind, main = "squareroot transformation", nbins = 12)
MASS::truehist(log_ind, main = "log transformation", nbins = 12)

# let R recommend the most suitable transformation method
bn.ind<-bestNormalize(ind.sub$daily.growth, out_of_sample = FALSE) 
bn.ind 

## create an object for the best transformation
on.ind <- orderNorm_ind$x.t # ordernorm transformation

## add the new object as column to your data frame
ind.transformed <- cbind(ind.sub, on.ind)
str(ind.transformed)
hist(ind.transformed$on.ind)

## test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(ind.transformed$on.ind) 
var.test(ind.transformed$on.ind ~ ind.transformed$habitat) 

# set up models
library(lme4)
library(lmerTest)
library(performance)

mind0<-lmer(on.ind~habitat+(1|ID)+(1|timespan),data=ind.transformed)
mind1<-lmer(on.ind~habitat+water.temp+(1|ID)+(1|timespan),data=ind.transformed)
mind2<-lmer(on.ind~habitat+(1|ID)+(1|timespan),data=ind.transformed)
mind3<-lmer(on.ind~habitat+water.temp+(1|ID)+(1|timespan)+(1|sample.site),data=ind.transformed)
mind4<-lmer(on.ind~habitat+(1|ID)+(1|timespan)+(1|sample.site),data=ind.transformed)
mind5<-lmer(on.ind~habitat+water.temp+(1|ID)+(1|timespan)+(1|sample.site)+(1|session),data=ind.transformed)
mind6<-lmer(on.ind~habitat+(1|ID)+(1|timespan)+(1|sample.site)+(1|session),data=ind.transformed)

# warning boundary fit is singular, it might mean that the variances of the random effects are near zero, that is not a problem for the models
# you can check the variances with glmmTMB
# if it's not about the variances there might be another/bigger problem
library(glmmTMB)
glmmTMB(on.ind~habitat+water.temp+(1|ID)+(1|timespan),data=ind.transformed)

AIC(mind0,mind1,mind2,mind3,mind4,mind5,mind6) # mind1 best supported
compare_performance(mind0,mind1,mind2,mind3,mind4,mind5,mind6, rank = T) # mind1 and mind6 equally supported

check_model(mind1) # looks okay and supported by both methods, take this
check_model(mind6)
summary(mind1)
summary(mind6)

# plot individual growth as boxplot
library(readxl)
ind.dat.2 <- read_excel("Individual_growth.xlsx", 
                         col_types = c("text", "text", "text", "numeric", "numeric", "numeric", "numeric","numeric", 
                                       "numeric","numeric", "numeric", "numeric","numeric", "numeric","numeric", "numeric",
                                       "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","text")) 

library(plyr)
library(ggpubr) 
count(ind.dat.2, "notes")
growtha<-subset(ind.dat.2, notes!="ssf"| is.na(notes))
growth<-subset(growtha, sample.site!="KoB") # remove KoB, since it's in between pond and stream
count(growth, "habitat")
aggregate(growth$daily.growth.rate, by = list(growth$habitat), max)
aggregate(growth$daily.growth.rate, by = list(growth$habitat), min)
aggregate(growth$daily.growth.rate, by = list(growth$habitat), mean, na.rm = TRUE)

hist(growth$daily.growth.rate)
shapiro.test(growth$daily.growth.rate) # non-normal
var.test(growth$daily.growth.rate ~ growth$habitat) #okay

setwd("D:/Plots/Mark_recapture")
png("growthplot.png", height=150, width=200, units="mm", res=300);print(growthplot)
growthplot<-
  ggplot(data=growth, aes(habitat,daily.growth.rate), fill=habitat)+
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  stat_summary(fun=mean, shape=4, size=1, show.legend=FALSE) +
  annotate("text", x = 1.5, y = 0.065, label = "*", size=7)+
  scale_x_discrete(labels=c("Pond (N=63)","Stream (N=65)"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Daily growth rate (cm)")+
  scale_y_continuous(breaks=seq(-0.025,0.05,0.025), limits = c(-0.025, 0.065))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
dev.off()




####################################################################################################################################################################################################

# 6. PERCENTAGE OF INJURED LARVAE
# import rawdata
library(readxl)
overall. <- read_excel("2021_Meandata-Mastertable.xlsx", na="NA",
                       col_types = c("text", "text", "numeric", "date","date", "text", "text", "numeric", 
                                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric"))

## set categories
library(dplyr)
overall.dat <- mutate(overall., across(c(sample.site:year, session:observer), as.factor))
str(overall.dat)

# prepare and inspect data
inj.sub<-subset(overall.dat, sample.site!="KoB")
hist(inj.sub$perc.injured)

## find best transformation for data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_inj <- arcsinh_x(inj.sub$perc.injured))
(boxcox_inj <- boxcox(inj.sub$perc.injured))
(centerscale_inj <- center_scale(inj.sub$perc.injured))
(orderNorm_inj <- orderNorm(inj.sub$perc.injured))
(yeojohnson_inj <- yeojohnson(inj.sub$perc.injured))
(sqrt_inj <- sqrt(inj.sub$perc.injured))
(log_inj <- log(inj.sub$perc.injured))

# then have a look at the histograms of the different transformations for first impression
par(mfrow = c(2,4)) # display all histograms in one window
hist(inj.sub$perc.injured)
MASS::truehist(arcsinh_inj$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(boxcox_inj$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(centerscale_inj$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(orderNorm_inj$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(yeojohnson_inj$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(sqrt_inj, main = "squareroot transformation", nbins = 12)
MASS::truehist(log_inj, main = "log transformation", nbins = 12)

# let R recommend the most suitable transformation method
bn.inj<-bestNormalize(inj.sub$perc.injured, out_of_sample = FALSE) 
bn.inj 

## create an object for the best transformation
cs.inj <- centerscale_inj$x.t # centerscale transformation

## add the new object as column to your data frame
inj.transformed <- cbind(inj.sub, cs.inj)
str(inj.transformed)
hist(inj.transformed$on.inj)

## test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(inj.transformed$cs.inj) 
var.test(inj.transformed$cs.inj ~ inj.transformed$habitat) 

# transformation did not normalise data
# use other distribution?
library(performance)
mod1<-glm(perc.injured~habitat, data=inj.sub, family=poisson(link="log"))
mod2<-lm(perc.injured~habitat, data=inj.sub)
check_model(mod1)
check_model(mod2)

# Check Model Residuals
library(DHARMa)
mod1.res<- simulateResiduals(mod1)
plot(mod1.res) 
testDispersion(mod1) 
mod2.res<- simulateResiduals(mod2)
plot(mod2.res)
testDispersion(mod2) 

# still doesn't look good, use simple non-parametric test (Wilcoxon)
# implemented in the following plot


# plot percentage of injured larvae across all years and sample sites

library(plyr)
library(ggplot2)
library(ggpubr) 
count(inj.sub, "habitat")
aggregate(inj.sub$perc.injured, by = list(inj.sub$habitat), max, na.rm = TRUE)
aggregate(inj.sub$perc.injured, by = list(inj.sub$habitat), min, na.rm = TRUE)
aggregate(inj.sub$perc.injured, by = list(inj.sub$habitat), mean, na.rm = TRUE)

setwd("D:/Plots/Mark_recapture")
png("injuries.png", height=150, width=200, units="mm", res=300);print(injuries)
injuries<-
  ggplot(data=inj.sub, aes(habitat,perc.injured), fill=habitat)+
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  stat_summary(fun=mean, shape=4, size=1, show.legend=FALSE) +
  annotate("text", x = 1.5, y = 1, label = "**", size=7)+
  scale_x_discrete(labels=c("Pond (N=88)","Stream (N=66)"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Percentage of injured larvae")+
  scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0, 1))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
dev.off()



####################################################################################################################################################################################################

# 7. RECAPTURE RATES

# import rawdata
library(readxl)
recap.surv <- read_excel("recapture<-survival.xlsx", na="NA",
                         col_types = c("text", "text", "numeric", "date", "numeric", "numeric","numeric", "numeric","numeric",
                                       "numeric", "numeric"))

## set categories
library(dplyr)
recap. <- mutate(recap.surv, across(c(sample.site:habitat), as.factor))
str(recap.)

# prepare and inspect data
recap.sub<-subset(recap., sample.site!="KoB")
hist(recap.sub$r)

## find best transformation for data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_recap <- arcsinh_x(recap.sub$r))
(boxcox_recap <- boxcox(recap.sub$r))
(centerscale_recap <- center_scale(recap.sub$r))
(orderNorm_recap <- orderNorm(recap.sub$r))
(yeojohnson_recap <- yeojohnson(recap.sub$r))
(sqrt_recap <- sqrt(recap.sub$r))
(log_recap <- log(recap.sub$r))

# then have a look at the histograms of the different transformations for first impression
par(mfrow = c(2,4)) # display all histograms in one window
hist(recap.sub$r)
MASS::truehist(arcsinh_recap$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(boxcox_recap$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(centerscale_recap$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(orderNorm_recap$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(yeojohnson_recap$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(sqrt_recap, main = "squareroot transformation", nbins = 12)
MASS::truehist(log_recap, main = "log transformation", nbins = 12)

# let R recommend the most suitable transformation method
bn.recap<-bestNormalize(recap.sub$r, out_of_sample = FALSE) 
bn.recap

## create an object for the best transformation
arc.recap <- arcsinh_recap$x.t # arcsinh transformation

## add the new object as column to your data frame
recap.transformed <- cbind(recap.sub, arc.recap)
str(recap.transformed)
hist(recap.transformed$arc.recap)

## test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(recap.transformed$arc.recap) 
var.test(recap.transformed$arc.recap ~ recap.transformed$habitat) 

# transformation did not normalise data
# use other distribution?
library(performance)
library(lme4)
library(lmerTest)

mod3<-glm(r~habitat, data=recap.sub, family=poisson(link="log"))
mod4<-lm(r~habitat, data=recap.sub)
heck_model(mod3)
check_model(mod4)

# Check Model Residuals
library(DHARMa)
mod3.res<- simulateResiduals(mod3)
plot(mod3.res) 
testDispersion(mod3) # not good
mod4.res<- simulateResiduals(mod4)
plot(mod4.res)
testDispersion(mod4) # okay

# mod 4 looks okay, take linear model

# set up models
mrecap0<-lm(r~habitat,data=recap.sub)
mrecap1<-lmer(r~habitat + (1|sample.site),data=recap.sub)
mrecap2<-lmer(r~habitat+(1|occasion),data=recap.sub)
mrecap3<-lmer(r~habitat+(1|sample.site)+(1|occasion),data=recap.sub)
AIC(mrecap0, mrecap1, mrecap2, mrecap3) # mrecap0 most supported
compare_performance(mrecap0, mrecap1, mrecap2, mrecap3, rank = T) # mrecap3 most supported
summary(mrecap0)
summary(mrecap3)
check_model(mrecap0)
check_model(mrecap3) # this model looks quite okay and way better than mrecap0, take this


# plot recapture rates per habitat
library(plyr)
library(ggplot2)
library(ggpubr) 
count(recap.sub, "habitat")
aggregate(recap.sub$r, by = list(recap.sub$habitat), max, na.rm = TRUE)
aggregate(recap.sub$r, by = list(recap.sub$habitat), min, na.rm = TRUE)


setwd("D:/Plots/Mark_recapture")
png("recaptures.png", height=150, width=200, units="mm", res=300);print(recaptures)
recaptures<-
  ggplot(data=recap.sub, aes(habitat,r), fill=habitat)+
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  stat_summary(fun=mean, shape=4,size=1, show.legend=FALSE) +
  annotate("text", x = 1.5, y = 1, label = "n.s.", size=4)+
  scale_x_discrete(labels=c("Pond (N=32)","Stream (N=24)"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Recapture rate")+
  scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0, 1))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
dev.off()



####################################################################################################################################################################################################

# 8. SURVIVAL RATES

# import rawdata
library(readxl)
recap.surv <- read_excel("recapture<-survival.xlsx", na="NA",
                         col_types = c("text", "text", "numeric", "date", "numeric", "numeric","numeric", "numeric","numeric",
                                       "numeric", "numeric"))

## set categories
library(dplyr)
surv. <- mutate(recap.surv, across(c(sample.site:habitat), as.factor))
str(surv.)

# prepare and inspect data
surv.sub<-subset(surv., sample.site!="KoB")
hist(surv.sub$phi)

## find best transformation for data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_surv <- arcsinh_x(surv.sub$phi))
(boxcox_surv <- boxcox(surv.sub$phi))
(centerscale_surv <- center_scale(surv.sub$phi))
(orderNorm_surv <- orderNorm(surv.sub$phi))
(yeojohnson_surv <- yeojohnson(surv.sub$phi))
(sqrt_surv <- sqrt(surv.sub$phi))
(log_surv <- log(surv.sub$phi))
(exp_surv<-exp_x(surv.sub$phi, standardize = TRUE))

# then have a look at the histograms of the different transformations for first impression
par(mfrow = c(2,4)) # display all histograms in one window
hist(surv.sub$phi)
MASS::truehist(arcsinh_surv$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(boxcox_surv$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(centerscale_surv$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(orderNorm_surv$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(yeojohnson_surv$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(sqrt_surv, main = "squareroot transformation", nbins = 12)
MASS::truehist(log_surv, main = "log transformation", nbins = 12)

# let R recommend the most suitable transformation method
bn.surv<-bestNormalize(surv.sub$phi, out_of_sample = FALSE) 
bn.surv

## create an object for the best transformation
exp.surv <- exp_surv$x.t # exponential transformation

## add the new object as column to your data frame
surv.transformed <- cbind(surv.sub, exp.surv)
str(surv.transformed)
hist(surv.transformed$exp.surv)

## test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(surv.transformed$exp.surv)
var.test(surv.transformed$phi~ surv.transformed$habitat) 

# transformation did not normalise data
# use other distribution?
library(performance)
library(lme4)
library(lmerTest)

mod5<-glm(phi~habitat, data=surv.sub, family=poisson(link="log"))
mod6<-lm(phi~habitat, data=surv.sub)
check_model(mod5)
check_model(mod6) # this model looks quite okay and way better than mod5, take this

# Check Model Residuals
library(DHARMa)
mod5.res<- simulateResiduals(mod5)
plot(mod5.res) 
testDispersion(mod5) # not good
mod6.res<- simulateResiduals(mod6)
plot(mod6.res)
testDispersion(mod6) #  QQ-Plot okay, but homogeneity of variances not good

# better go for non-parametric comparison of means as implemented in the following plot

# plot survival per habitat
library(plyr)
library(ggplot2)
library(ggpubr) 
count(surv.sub, "habitat")
aggregate(surv.sub$phi, by = list(surv.sub$habitat), max, na.rm = TRUE)
aggregate(surv.sub$phi, by = list(surv.sub$habitat), min, na.rm = TRUE)
aggregate(surv.sub$phi, by = list(surv.sub$habitat), mean, na.rm = TRUE)

setwd("D:/Plots/Mark_recapture")
png("survival.png", height=150, width=200, units="mm", res=300);print(survival)
survival<-
  ggplot(data=surv.sub, aes(habitat,phi), fill=habitat)+
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  stat_compare_means(method="wilcox.test", label="p.signif",  label.x = 1.5, label.y = 1, size=7)+ # pvalue=0.034
  stat_summary(fun=mean, shape=4, size=1,show.legend=FALSE) +
  scale_x_discrete(labels=c("Pond (N=32)","Stream (N=24)"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Survival rate")+
  scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0, 1))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
dev.off()



####################################################################################################################################################################################################

# 9. ESTIMATED POPULATION SIZE

# import rawdata
library(readxl)
recap.surv <- read_excel("recapture<-survival.xlsx", na="NA",
                         col_types = c("text", "text", "numeric", "date", "numeric", "numeric","numeric", "numeric","numeric",
                                       "numeric", "numeric"))

## set categories
library(dplyr)
nest. <- mutate(recap.surv, across(c(sample.site:habitat), as.factor))
str(nest.)

# prepare and inspect data
nest.sub<-subset(nest., sample.site!="KoB")
hist(nest.sub$Nest)


# plot population estimates per habitat
library(plyr)
library(ggplot2)
library(ggpubr) 
count(nest.sub, "habitat")
aggregate(nest.sub$Nest, by = list(surv.sub$habitat), max, na.rm = TRUE)
aggregate(nest.sub$Nest, by = list(surv.sub$habitat), min, na.rm = TRUE)
aggregate(nest.sub$Nest, by = list(surv.sub$habitat), mean, na.rm = TRUE)

setwd("D:/Plots/Mark_recapture")
png("estimatedpopsize.png", height=150, width=200, units="mm", res=300);print(estimates)
estimates<-
  ggplot(data=nest.sub, aes(habitat,Nest), fill=habitat)+
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  annotate("text", x = 1.5, y = 1500, label = "n.s.")+
  stat_summary(fun=mean, shape=8, show.legend=FALSE) +
  scale_x_discrete(labels=c("Pond (N=32)","Stream (N=24)"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Estimated number of larvae")+
  scale_y_continuous(breaks=seq(0,1500,300), limits = c(0, 1500))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
dev.off()


# plot single plots for each sample site
library(readr)
library(ggplot2)
pop.est <- read_delim("pop-est-larvae_allinone.csv", delim = ";")

pop.est.1<-subset(pop.est, KB.week.Nest!="NA")
pop.est.2<-subset(pop.est, KB.avr.Nest!="NA")
e1<-ggplot(pop.est, aes(x=date), y=KB.week.Nest,) + ggtitle("Stream KB")+
  geom_ribbon(data=pop.est.1,aes(x=date, ymax=(KB.week.Nest+KB.week.Nest.se), ymin=(KB.week.Nest-KB.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.2,aes(x=date, ymax=(KB.avr.Nest+KB.avr.Nest.se),  ymin=(KB.avr.Nest-KB.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.1,aes(y=KB.week.Nest),color = "black")+
  geom_path(data=pop.est.1,aes(y=KB.week.Nest),color = "black")+ 
  geom_path(data=pop.est.1,aes(y=(KB.week.Nest+KB.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.1,aes(y=(KB.week.Nest-KB.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.2,aes(y=KB.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.2,aes(y=KB.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.2,aes(y=(KB.avr.Nest+KB.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.2,aes(y=(KB.avr.Nest-KB.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,800,100), limits = c(0, 800))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-01"), as.Date("2021-12-15")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)
e1


pop.est.3<-subset(pop.est, VB.week.Nest!="NA")
pop.est.4<-subset(pop.est, VB.avr.Nest!="NA")
e2<-ggplot(pop.est, aes(x=date), y=VB.week.Nest,) +
  ggtitle("Stream VB")+
  geom_ribbon(data=pop.est.3,aes(x=date, ymax=(VB.week.Nest+VB.week.Nest.se), 
                                 ymin=(VB.week.Nest-VB.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.4,aes(x=date, ymax=(VB.avr.Nest+VB.avr.Nest.se), 
                                 ymin=(VB.avr.Nest-VB.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.3,aes(y=VB.week.Nest),color = "black")+
  geom_path(data=pop.est.3,aes(y=VB.week.Nest),color = "black")+ 
  geom_path(data=pop.est.3,aes(y=(VB.week.Nest+VB.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.3,aes(y=(VB.week.Nest-VB.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.4,aes(y=VB.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.4,aes(y=VB.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.4,aes(y=(VB.avr.Nest+VB.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.4,aes(y=(VB.avr.Nest-VB.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,800,100), limits = c(0, 800))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-01"), as.Date("2021-12-15")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)
e2



pop.est.5<-subset(pop.est, MB.week.Nest!="NA")
pop.est.6<-subset(pop.est, MB.avr.Nest!="NA")
e3<-ggplot(pop.est, aes(x=date), y=MB.week.Nest,) +
  ggtitle("Stream MB")+
  geom_ribbon(data=pop.est.5,aes(x=date, ymax=(MB.week.Nest+MB.week.Nest.se), 
                                 ymin=(MB.week.Nest-MB.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.6,aes(x=date, ymax=(MB.avr.Nest+MB.avr.Nest.se), 
                                 ymin=(MB.avr.Nest-MB.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.5,aes(y=MB.week.Nest),color = "black")+
  geom_path(data=pop.est.5,aes(y=MB.week.Nest),color = "black")+ 
  geom_path(data=pop.est.5,aes(y=(MB.week.Nest+MB.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.5,aes(y=(MB.week.Nest-MB.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.6,aes(y=MB.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.6,aes(y=MB.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.6,aes(y=(MB.avr.Nest+MB.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.6,aes(y=(MB.avr.Nest-MB.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,800,100), limits = c(0, 800))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-01"), as.Date("2021-12-15")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)
e3

pop.est.7<-subset(pop.est, KoB.week.Nest!="NA")
pop.est.7<-subset(pop.est, KoB.avr.Nest!="NA")
e4<-ggplot(pop.est, aes(x=date), y=KoB.week.Nest,) +
  ggtitle("(Stream) KoB")+
  geom_ribbon(data=pop.est.7,aes(x=date, ymax=(KoB.week.Nest+KoB.week.Nest.se), 
                                 ymin=(KoB.week.Nest-KoB.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.7,aes(x=date, ymax=(KoB.avr.Nest+KoB.avr.Nest.se), 
                                 ymin=(KoB.avr.Nest-KoB.avr.Nest.se), alpha=.3), fill="lightgrey")+ 
  geom_point(data=pop.est.7,aes(y=KoB.week.Nest),color = "black")+
  geom_path(data=pop.est.7,aes(y=KoB.week.Nest),color = "black")+ 
  geom_path(data=pop.est.7,aes(y=(KoB.week.Nest+KoB.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.7,aes(y=(KoB.week.Nest-KoB.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.7,aes(y=KoB.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.7,aes(y=KoB.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.7,aes(y=(KoB.avr.Nest+KoB.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.7,aes(y=(KoB.avr.Nest-KoB.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,800,100), limits = c(0, 800))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-01"), as.Date("2021-12-15")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)
e4

pop.est.9<-subset(pop.est, KoVK.week.Nest!="NA")
pop.est.10<-subset(pop.est, KoVK.avr.Nest!="NA")
e5<-ggplot(pop.est, aes(x=date), y=KoVK.week.Nest,) +
  ggtitle("Pond KoVK")+
  geom_ribbon(data=pop.est.9,aes(x=date, ymax=(KoVK.week.Nest+KoVK.week.Nest.se), 
                                 ymin=(KoVK.week.Nest-KoVK.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.10,aes(x=date, ymax=(KoVK.avr.Nest+KoVK.avr.Nest.se), 
                                  ymin=(KoVK.avr.Nest-KoVK.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.9,aes(y=KoVK.week.Nest),color = "black")+
  geom_path(data=pop.est.9,aes(y=KoVK.week.Nest),color = "black")+ 
  geom_path(data=pop.est.9,aes(y=(KoVK.week.Nest+KoVK.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.9,aes(y=(KoVK.week.Nest-KoVK.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.10,aes(y=KoVK.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.10,aes(y=KoVK.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.10,aes(y=(KoVK.avr.Nest+KoVK.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.10,aes(y=(KoVK.avr.Nest-KoVK.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,2100,500), limits = c(0, 2100))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-01"), as.Date("2021-12-15")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)
e5



pop.est.11<-subset(pop.est, TG.week.Nest!="NA")
pop.est.12<-subset(pop.est, TG.avr.Nest!="NA")
e6<-ggplot(pop.est, aes(x=date), y=TG.week.Nest,) +
  ggtitle("Pond TG")+
  geom_ribbon(data=pop.est.11,aes(x=date, ymax=(TG.week.Nest+TG.week.Nest.se), 
                                  ymin=(TG.week.Nest-TG.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.12,aes(x=date, ymax=(TG.avr.Nest+TG.avr.Nest.se), 
                                  ymin=(TG.avr.Nest-TG.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.11,aes(y=TG.week.Nest),color = "black")+
  geom_path(data=pop.est.11,aes(y=TG.week.Nest),color = "black")+ 
  geom_path(data=pop.est.11,aes(y=(TG.week.Nest+TG.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.11,aes(y=(TG.week.Nest-TG.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.12,aes(y=TG.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.12,aes(y=TG.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.12,aes(y=(TG.avr.Nest+TG.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.12,aes(y=(TG.avr.Nest-TG.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,2100,500), limits = c(0, 2100))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-01"), as.Date("2021-12-15")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)
e6



pop.est.13<-subset(pop.est, SG.week.Nest!="NA")
pop.est.14<-subset(pop.est, SG.avr.Nest!="NA")
e7<-ggplot(pop.est, aes(x=date), y=SG.week.Nest,) +
  ggtitle("Pond SG")+
  geom_ribbon(data=pop.est.13,aes(x=date, ymax=(SG.week.Nest+SG.week.Nest.se), 
                                  ymin=(SG.week.Nest-SG.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.14,aes(x=date, ymax=(SG.avr.Nest+SG.avr.Nest.se), 
                                  ymin=(SG.avr.Nest-SG.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.13,aes(y=SG.week.Nest),color = "black")+
  geom_path(data=pop.est.13,aes(y=SG.week.Nest),color = "black")+ 
  geom_path(data=pop.est.13,aes(y=(SG.week.Nest+SG.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.13,aes(y=(SG.week.Nest-SG.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.14,aes(y=SG.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.14,aes(y=SG.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.14,aes(y=(SG.avr.Nest+SG.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.14,aes(y=(SG.avr.Nest-SG.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,2100,500), limits = c(0, 2100))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-01"), as.Date("2021-12-15")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)
e7



pop.est.15<-subset(pop.est, TT.week.Nest!="NA")
pop.est.16<-subset(pop.est, TT.avr.Nest!="NA")
e8<-ggplot(pop.est, aes(x=date), y=TT.week.Nest,) +
  ggtitle("Pond TT")+
  geom_ribbon(data=pop.est.15,aes(x=date, ymax=(TT.week.Nest+TT.week.Nest.se), 
                                  ymin=(TT.week.Nest-TT.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.16,aes(x=date, ymax=(TT.avr.Nest+TT.avr.Nest.se), 
                                  ymin=(TT.avr.Nest-TT.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.15,aes(y=TT.week.Nest),color = "black")+
  geom_path(data=pop.est.15,aes(y=TT.week.Nest),color = "black")+ 
  geom_path(data=pop.est.15,aes(y=(TT.week.Nest+TT.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.15,aes(y=(TT.week.Nest-TT.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.16,aes(y=TT.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.16,aes(y=TT.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.16,aes(y=(TT.avr.Nest+TT.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.16,aes(y=(TT.avr.Nest-TT.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,2100,500), limits = c(0, 2100))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-01"), as.Date("2021-12-15")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)
e8

# get legend
# make plot only for legend
legend5<-ggplot(pop.est, aes(x=date), y=KoVK.week.n, group=as.factor(period), colour=as.factor(period),shape=as.factor(period))+
  ggtitle("only to grab legend")+ 
  geom_line(aes(y=KoVK.week.n,color=as.factor(period)), size=0.5)+
  geom_point(aes(y=KoVK.week.n,color=as.factor(period), shape=as.factor(period)))+ 
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("darkgrey", "black"))+
  theme_classic(base_size=12, base_family="Arial")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="bottom",
        legend.title=element_blank(),
        axis.title.x=element_blank(),)


get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

legend5a<- get_only_legend(legend5)

# 2.1.2 ARRANGE MARGINS OF PLOTS 
me1<-e1+
  theme(plot.margin = unit(c(0.55,0,0.7,0.9), "cm"),
        plot.title=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

me2<-e2+
  theme(plot.margin = unit(c(0.55,0.25,0.7,0.45), "cm"),
        plot.title=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())

me3<-e3+
  theme(plot.margin = unit(c(0.55,0.3,0.7,0.25), "cm"),
        plot.title=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())

me4<-e4+
  theme(plot.margin = unit(c(0.55,0.75,0.7,0.2), "cm"),
        plot.title=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())

me5<-e5+
  theme(plot.margin = unit(c(0.2,0,1.2,0.7), "cm"),
        axis.title.y=element_blank(),
        plot.title=element_blank(),
        axis.text.x=element_blank())

me6<-e6+
  theme(plot.margin = unit(c(0.2,0.25,1.2,0.45), "cm"),
        plot.title=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())

me7<-e7+
  theme(plot.margin = unit(c(0.2,0.3,1.2,0.25), "cm"),
        plot.title=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())

me8<-e8+
  theme(plot.margin = unit(c(0.2,0.75,1.2,0.2), "cm"),
        plot.title=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        legend.position = "none")


############### 2.1.3 CREATE MULTIPLOT
library(gridExtra)
library(grid)
setwd("D:/Plots/Mark_recapture")
Multiplot5<-grid.arrange(me1, me2,me3,me4,me5,me6,me7,me8, ncol=4)
png("multiplot-popest.png", height=150, width=200, units="mm", res=300);print(Multiplot5)
Multiplot5a<-grid.arrange(Multiplot5, legend5a, ncol=1, heights = c(10, 1))
grid.text("Stream KB", x = unit(0.16, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Stream VB", x = unit(0.375, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Stream MB", x = unit(0.63, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("(Stream) KoB", x = unit(0.86, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Pond KoVK", x = unit(0.16, "npc"), y = unit(0.53, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Pond TG", x = unit(0.375, "npc"), y = unit(0.53, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Pond SG", x = unit(0.63, "npc"), y = unit(0.53, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Pond TT", x = unit(0.86, "npc"), y = unit(0.53, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.099, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.114, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.129, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.143, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.157, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.173, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("S", x = unit(0.19, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("O", x = unit(0.205, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("N", x = unit(0.22, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("D", x = unit(0.235, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.287, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.307, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.327, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.347, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.367, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.387, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("S", x = unit(0.407, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("O", x = unit(0.427, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("N", x = unit(0.448, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("D", x = unit(0.468, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.526, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.548, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.57, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.588, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.607, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.632, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("S", x = unit(0.654, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("O", x = unit(0.675, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("N", x = unit(0.695, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("D", x = unit(0.717, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.775, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.794, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.813, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.83, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.85, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.87, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("S", x = unit(0.889, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("O", x = unit(0.908, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("N", x = unit(0.927, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("D", x = unit(0.946, "npc"), y = unit(0.575, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.099, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.114, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.129, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.143, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.157, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.173, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("S", x = unit(0.19, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("O", x = unit(0.205, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("N", x = unit(0.22, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("D", x = unit(0.235, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.287, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.307, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.327, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.347, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.367, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.387, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("S", x = unit(0.407, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("O", x = unit(0.427, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("N", x = unit(0.448, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("D", x = unit(0.468, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.526, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.548, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.57, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.588, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.607, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.632, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("S", x = unit(0.654, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("O", x = unit(0.675, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("N", x = unit(0.695, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("D", x = unit(0.717, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.775, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.794, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("M", x = unit(0.813, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.83, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("J", x = unit(0.85, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("A", x = unit(0.87, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("S", x = unit(0.889, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("O", x = unit(0.908, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("N", x = unit(0.927, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("D", x = unit(0.946, "npc"), y = unit(0.15, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Month", x = unit(0.51, "npc"), y = unit(0.11, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Estimated number of larvae", x = unit(0.013, "npc"), y = unit(0.35, "npc"), rot=90, gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Estimated number of larvae", x = unit(0.013, "npc"), y = unit(0.79, "npc"), rot=90, gp=gpar(fontfamily="Arial", fontsize=12))
dev.off()
