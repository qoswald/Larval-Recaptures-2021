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

## ponds
# KoVK 
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
modwat<-lmer(on.water ~ habitat + (1|sample.site/year) + (1|session/year), data=water.transformed)
modwat1<-lmer(on.water ~ sample.site + (1|session/year), data=water.transformed)
summary(modwat)
summary(modwat1)

# check model
library(performance)
check_model(modwat)

# plot as boxplot
library(plyr)
library(ggpubr) 
count(water.sub, "habitat")

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
#save plot on local harddrive
setwd("D:/Plots/Mark_recapture")
png("watertemp.png", height=150, width=200, units="mm", res=300);print(watertemp)
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
Multiplot1<-grid.arrange(figi, figj, figk, figl, figm, fign, figo, figp, ncol=4)
Multiplot1a<-grid.arrange(Multiplot1, legend1, ncol=1, heights = c(10, 1))
grid.text("Stream KoB", x = unit(0.16, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Stream KB", x = unit(0.375, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Stream VB", x = unit(0.63, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Stream MB", x = unit(0.86, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
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
setwd("D:/Plots/Recapture")
png("multiplot-count.png", height=150, width=200, units="mm", res=300);print(Multiplot1)
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
count(overall.dat, "habitat")
hist(overall.dat$mean.size)
shapiro.test(overall.dat$mean.size) # non-normal
var.test(overall.dat$mean.size ~ overall.dat$habitat) #okay

sizeplot<-
  ggplot(data=overall.dat, aes(habitat,mean.size), fill=habitat)+
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  stat_summary(fun=mean, shape=8, show.legend=FALSE) +
  scale_x_discrete(labels=c("Pond (N=57)","Stream (N=61)"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Mean larval size (cm)")+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")

setwd("D:/Plots/Mark_recapture")
png("sizeplot.png", height=150, width=200, units="mm", res=300);print(sizeplot)
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
Multiplot2<-grid.arrange(fps1, fps2, fps3, fps4, fps5, fps6, fps7, fps8, ncol=4)
Multiplot2a<-grid.arrange(Multiplot2, legend2, ncol=1, heights = c(10, 1))
grid.text("Stream KoB", x = unit(0.155, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Stream KB", x = unit(0.375, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Stream VB", x = unit(0.63, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("Stream MB", x = unit(0.86, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
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

# save on local harddrive
setwd("D:/Plots/Recapture")
png("multiplot-meansize.png", height=150, width=200, units="mm", res=300);print(Multiplot2)
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

check_model(mind1)
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
hist(growth$daily.growth.rate)
shapiro.test(growth$daily.growth.rate) # non-normal
var.test(growth$daily.growth.rate ~ growth$habitat) #okay

growthplot<-
  ggplot(data=growth, aes(habitat,daily.growth.rate), fill=habitat)+
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  stat_summary(fun=mean, shape=8, show.legend=FALSE) +
  scale_x_discrete(labels=c("Pond (N=63)","Stream (N=65)"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Daily growth rate (cm)")+
  scale_y_continuous(breaks=seq(-0.025,0.05,0.025), limits = c(-0.025, 0.065))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")

setwd("D:/Plots/Mark_recapture")
png("growthplot.png", height=150, width=200, units="mm", res=300);print(growthplot)
dev.off()




####################################################################################################################################################################################################

# 6. DATA ON INJURED LARVAE
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
library(ResourceSelection)

mod1<-glm(perc.injured~habitat, data=inj.sub, family=poisson(link="log"))
mod2<-lm(perc.injured~habitat, data=inj.sub)
check_distribution(mod1)
hoslem.test(inj.dat.omit$perc.injured, fitted(mod1)) # goodness of fit test
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
# implemented in the following


# plot percentage of injured larvae across all years and sample sites

library(plyr)
library(ggplot2)
library(ggpubr) 
count(inj.sub, "habitat")

injuries<-
  ggplot(data=inj.sub, aes(habitat,perc.injured), fill=habitat)+
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  stat_compare_means(method="wilcox.test", label="p.signif",  label.x = 1.5, label.y = 1)+ # pvalue=0.0056
  stat_summary(fun=mean, shape=8, show.legend=FALSE) +
  scale_x_discrete(labels=c("Pond (N=88)","Stream (N=66)"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Rate of injured larvae")+
  scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0, 1))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")

setwd("D:/Plots/Mark_recapture")
png("injuries.png", height=150, width=200, units="mm", res=300);print(injuries)
dev.off()




####################################################################################################################################################################################################

