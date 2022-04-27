#################### 1. DATA UPLOAD 
library(readxl)
# count data 
count.dat <- read_excel("larvalcount.xlsx") 
View(count.dat)

# mean size data 
size.dat<-read_excel("Size.xlsx") 
View(size.dat)

# data for individual larval growth rates
ind.size <- read_excel("Recapture-Data/Larval_recap_sizes.xlsx") 
View(ind.size)


#################### 2. VISUALIZE MONITORING DATA OF LARVAE - COMPARISON OF 2019/20/21

########## 2.1 PLOT GRAPHS FOR EACH SAMPLE SITE

library(ggplot2)

p1<-ggplot(count.dat, aes(x=date2, y=KoB, group=year, colour=year, shape=year)) +
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

p2<-ggplot(count.dat, aes(x=date2, y=KB, group=year, colour=year, shape=year)) +
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

p3<-ggplot(count.dat, aes(x=date2, y=VB, group=year, colour=year, shape=year)) +
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

p4<-ggplot(count.dat, aes(x=date2, y=MB, group=year, colour=year, shape=year)) +
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

p5<-ggplot(count.dat, aes(x=date2, y=KoVK, group=year, colour=year, shape=year)) +
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

p6<-ggplot(count.dat, aes(x=date2, y=TG, group=year, colour=year, shape=year)) +
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

p7<-ggplot(count.dat, aes(x=date2, y=SG, group=year, colour=year, shape=year)) +
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

p8<-ggplot(count.dat, aes(x=date2, y=TT, group=year, colour=year, shape=year)) +
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