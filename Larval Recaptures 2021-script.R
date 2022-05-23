##### 1. DATA UPLOAD 
library(readxl)
# count data 
count.dat <- read_excel("larvalcount.xlsx")

# mean size data 
size.dat<-read_excel("Size.xlsx") 

# data for population analyses
# pond data 
KoVK <- read_excel("KoVK2021.txt")
SG <- read_excel("SG2021.txt")
TG <- read_excel("TG2021.txt")
TT <- read_excel("TT2021.txt")
# stream data 
KB <- read_excel("KB2021.txt")
KoB <- read_excel("KoB2021.txt")
MB <- read_excel("MB2021.txt")
VB <- read_excel("VB2021.txt")

# data for individual larval growth rates
ind.size <- read_excel("Ind-sizes.xlsx") 
View(ind.size)



##### 2. VISUALIZE MONITORING DATA OF LARVAE - COMPARISON OF 2019/20/21
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

setwd("D:/Plots/Recapture")
png("multiplot-count.png", height=150, width=200, units="mm", res=300);print(Multiplot1)
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
