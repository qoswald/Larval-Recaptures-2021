###################################################################################################################################

#Rscript to the paper: "Population monitoring of fire salamanders (Salamandra salamandra) with a new photo-recognition software "
#authors: P. Oswald, B. Tunnat & B. A. Caspers
#created
#last modified

###################################################################################################################################


# 1. TEMPERATURE REGIMES IN PONDS AND STREAMS 
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
modwat<-lmer(on.water ~ habitat + year + (1|sample.site) + (1|date.actual), data=water.transformed)
summary(modwat)

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
  geom_text(x=1.5, y=25, size=(0.36*12), family="Arial", fontface="plain", label="Linear mixed effect model, habitat: p = 0.226")+ # factor 0.36 to get same font size as theme
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


###################################################################################################################################

# 2. NUMBER OF LARVAE

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

## test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(count.transformed$on.count) 
var.test(count.transformed$on.count ~ count.transformed$habitat) 

# set up models with different combinations of fixed and random effects 
library(lme4)
library(lmerTest)
library(performance)

plot()
mcount0<-lm(on.count~habitat, data=count.transformed) # without random effects
mcount1<-lm(on.count~habitat+sample.site , data=count.transformed)
mcount2<-lmer(on.count~habitat+(1|sample.site/year), data=count.transformed)
mcount3<-lmer(on.count~habitat+(1|sample.site)+(1|session/year), data=count.transformed)
mcount4<-lmer(on.count~habitat+(1|sample.site)+(1|session/year)+(1|water.temp), data=count.transformed)
mcount5<-lmer(on.count~habitat+(1|sample.site)+(1|session/year)+(1|water.temp)+(1|observer), data=count.transformed)
AIC(mcount0,mcount1,mcount2,mcount3,mcount4) # mcount 2 has lowest AIC
# make diagnostic plots to check the model, normality of residuals is the most important part
check_model(mcount2)
summary(mcount2)


# plot number of larvae as boxplot
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
  geom_text(x=1.5, y=100, size=(0.36*12), family="Arial", fontface="plain", label="Linear mixed effect model, habitat: p = ???")+ # factor 0.36 to get same font size as theme
  stat_summary(fun=mean, shape=8, show.legend=FALSE) +
  scale_x_discrete(labels=c("Pond (N=57)","Stream (N=61)"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Number of larvae")+
  scale_y_continuous(breaks=seq(0,100,20), limits = c(0, 100))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
dev.off()


# 2. MONITORING DATA OF LARVAE
# 2.1 Plot Graphs FOR EACH SAMPLE SITE
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


############### 2.1.1 EXTRACT LEGEND FOR MULTIPLOT 
get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

legend1<- get_only_legend(p8)


############### 2.1.2 ARRANGE MARGINS OF PLOTS 
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


############### 2.1.3 CREATE MULTIPLOT
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



##### 3. PLOT MEAN LARVAL SIZES
########## 3.1 PLOT GRAPHS FOR EACH SAMPLE SITE
ps1<-ggplot(size.dat, aes(x=date2, y=KoB, group=year, colour=year, shape=year)) +
  ggtitle("Stream KoB")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4", "goldenrod"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Larval size")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

ps2<-ggplot(size.dat, aes(x=date2, y=KB, group=year, colour=year,shape=year)) +
  ggtitle("Stream KB")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Number of larvae (count)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

ps3<-ggplot(size.dat, aes(x=date2, y=VB, group=year, colour=year, shape=year)) +
  ggtitle("Stream VB")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Number of larvae (count)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

ps4<-ggplot(size.dat, aes(x=date2, y=MB, group=year, colour=year, shape=year)) +
  ggtitle("Stream MB")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Number of larvae (count)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

ps5<-ggplot(size.dat, aes(x=date2, y=KoVK, group=year, colour=year, shape=year)) +
  ggtitle("Pond KoVK")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Number of larvae (count)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

ps6<-ggplot(size.dat, aes(x=date2, y=TG, group=year, colour=year, shape=year)) +
  ggtitle("Pond TG")+geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Number of larvae (count)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

ps7<-ggplot(size.dat, aes(x=date2, y=SG, group=year, colour=year, shape=year)) +
  ggtitle("Pond SG")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Number of larvae (count)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

ps8<-ggplot(size.dat, aes(x=date2, y=TT, group=year, colour=year, shape=year)) +
  ggtitle("Pond TT")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Number of larvae (count)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="bottom",
        legend.title =element_text(family="Arial", size=12, color="black"),
        legend.text = element_text(family="Arial", size=12, color="black"),
        axis.title.x=element_blank(),)

############### 3.1.1 EXTRACT LEGEND FOR MULTIPLOT 
legend2 <- get_only_legend(ps8)


############### 3.1.2 ARRANGE MARGINS OF PLOTS 
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

############### 3.1.3 CREATE MULTIPLOT
Multiplot2<-grid.arrange(fps1, fps2, fps3, fps4, fps5, fps6, fps7, fps8, ncol=4)

setwd("D:/Plots/Recapture")
png("multiplot-meansize.png", height=150, width=200, units="mm", res=300);print(Multiplot2)
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
dev.off()


##### 4. LARVAL RECAPTURE ANALYSES
library(R2ucare) # to test for goodness of fit (underlying assumptions)
library(dplyr) # for tidy data
library(magrittr) # for pipes
library(RMark)
MarkPath="D:/UniBieProgramme/MARK"

########## 4.1. RECAPTURES IN PONDS
##### 4.1.1. KoVK 
### 4.1.1.1 based on monthly averages
KoVK1=read.delim("KoVK1.txt",colClass=c("character","character"))
# first we have to turn our data into a matrix that is needed to work with R2ucare
KoVK1.matrix <- KoVK1$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(KoVK1))

# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(KoVK1.matrix, rep(1,nrow(KoVK1)))
# since there is no evidence for lack of fit (p>0.05), everything is fine and we can move on without performing other two tests

# First, process data
KoVK1.proc <- process.data(KoVK1, model = "POPAN")
# Second, make design data (from processed data)
KoVK1.dd <- make.design.data(KoVK1.proc)
fit.KoVK1.model <- function(){
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
  results <- mark.wrapper(cml, data = KoVK1.proc, ddl = KoVK1.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)
}
# Run function
KoVK1.models <- fit.KoVK1.model()
KoVK1.models
summary(KoVK1.models[[4]], se=TRUE)
KoVK1.models[[4]]$results$derived