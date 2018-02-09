###########
#Author: Jacob Walsh
#Title: Video Game Sales - Types and Trends

library(MASS)
library(tidyverse)
library(stringr)
library(magrittr)
library(data.table)
library(lubridate)
library(RPostgreSQL)
library(plotly)
library(jsonlite)
library(htmltools)
library(glmnet)
library(epitools)
library(broom)
library(lme4)
library(sjPlot)
library(parallel)
library(pscl)
library(countreg)


vg.sales<-read.csv("C:/Penn State MAS/Stat 581/Video_Games_Sales_as_at_22_Dec_2016.csv", na.strings=c("", "N/A", "Misc", "<NA>"))


##############################New Categories and Variables#################################
vg.sales$type1<-ifelse(vg.sales$Platform=="PC", "PC", "Console") 
vg.sales$type2<-ifelse(vg.sales$Platform=="PC", "PC", 
                       ifelse(vg.sales$Platform %in% c("PSP", "PSV", "DS", "3DS", "GB"), "Handheld", "Console")) 

vg.sales$type3<-ifelse(vg.sales$Platform=="PC", "PC", 
                       ifelse(vg.sales$Platform %in% c("Wii", "NES", "GB", "SNES", "WiiU", "3DS", "N64", "DS", "GBA", "GC"), "Nintendo", 
                              ifelse(vg.sales$Platform %in% c("PS", "PS2", "PS3", "PS4", "PSP", "PSV"), "Playstation", 
                                     ifelse(vg.sales$Platform %in% c("XB", "X360", "XOne"), "Xbox", "NA"))))
vg.sales$logsales<-log(vg.sales$Global_Sales)

vg.sales$scoresq<-vg.sales$Critic_Score^2

vg.sales<-vg.sales[!vg.sales$type3=="NA",]
vg.sales<-vg.sales[!is.na(vg.sales$Genre),]
vgc<-vg.sales[complete.cases(vg.sales),]

GxT<-table(vg.sales$Genre, vg.sales$type3)
totals<-aggregate(Global_Sales~Name+Publisher, data=vg.sales, FUN=sum)
sales=vg.sales[,c("NA_Sales","EU_Sales","JP_Sales","Other_Sales","Global_Sales")]
cor(sales)

vg.sales$Platform<-factor(vg.sales$Platform)
plat.table<-aggregate(vg.sales$Global_Sales, by=list(vg.sales$Platform), FUN=sum)


#######################Analysis of total sales
##############Global Sales by Region
global.trend<-aggregate(vgc$Global_Sales, by=list(Year=vgc$Year_of_Release), FUN=sum)
na.trend<-aggregate(vgc$NA_Sales, by=list(Year=vgc$Year_of_Release), FUN=sum)
eu.trend<-aggregate(vgc$EU_Sales, by=list(Year=vgc$Year_of_Release), FUN=sum)
jp.trend<-aggregate(vgc$JP_Sales, by=list(Year=vgc$Year_of_Release), FUN=sum)
other.trend<-aggregate(vgc$Other_Sales, by=list(Year=vgc$Year_of_Release), FUN=sum)

eu.sales<-eu.trend$x
na.sales<-na.trend$x
jp.sales<-jp.trend$x
other.sales<-other.trend$x
year<-na.trend$Year
trend.df<-data.frame(year, na.sales, eu.sales, jp.sales, other.sales)

vg.sales$Platform<-factor(vg.sales$Platform)
plat.table<-aggregate(vg.sales$Global_Sales, by=list(vg.sales$Platform), FUN=sum)

#Console vs PC titles released

t1<-table(vg.sales$Genre, vg.sales$type1)
t1
t1df<-as.data.frame(t1)
prop.table(t1, 2)
chisq.test(t1)
tab<-epitab(t1)
tabdf<-as.data.frame(tab$tab)
tabdf[,c(1,3,5,6,7)]
epitab(t1)

t1df<-as.data.frame(prop.table(t1,2))
colnames(t1df)<-c("Genre", "Platform", "Proportion")
#Console vs PC vs Handheld

t2<-table(vg.sales$Genre, vg.sales$type2) 
t2
prop.table(t2, 2)
chisq.test(t2)
t2df<-as.data.frame(prop.table(t2,2))
colnames(t2df)<-c("Genre", "Platform", "Proportion")
epitab(t2[,c(1,2)])
epitab(t2[,c(1,3)])

t3<-table(vg.sales$Genre, vg.sales$type3) 
t3
prop.table(t3, 2)
chisq.test(t3)
t3df<-as.data.frame(prop.table(t3,2))
colnames(t3df)<-c("Genre", "Platform", "Proportion")
p4<-ggplot(t3df, aes(x=Genre, y=Proportion, fill=Platform))+geom_col(position="dodge", color="black")

tab1<-epitab(t3[,c(1,2)], conf.level = 1-0.05/11)$tab
tab2<-epitab(t3[,c(1,3)], conf.level = 1-0.05/11)$tab
tab3<-epitab(t3[,c(1,4)], conf.level = 1-0.05/11)$tab
odds<-c(tab1[,5:7], tab2[,5:7], tab3[,5:7])
genres<-unique(factor(vgc$Genre))
odds.df<-setDT(as.data.frame(matrix(odds, ncol=9, dimnames=list(rownames(tab1), c("PC", "PCmin", "PCmax", "Playstation", "PSmin", "PSmax", "Xbox", "Xmin", "Xmax")))), keep.rownames = TRUE)           


ggodds<-melt(odds.df, id.vars="rn", measure.vars=c("PC", "Playstation","Xbox"))            
ggmin<-melt(odds.df, id.vars="rn", measure.vars=c("PCmin", "PSmin","Xmin"))
ggmax<-melt(odds.df, id.vars="rn", measure.vars=c("PCmax", "PSmax","Xmax"))
ggodds$min<-ggmin$value
ggodds$max<-ggmax$value
colnames(ggodds)<-c("Genre", "Platform", "Odds", "min", "max")
ggodds


pxw<-vgc[vgc$Platform %in% c("PS3", "X360", "Wii"),]
pxw$Platform<-factor(pxw$Platform)
time.pxw<-aggregate(pxw$Global_Sales, by=list(pxw$Year_of_Release, pxw$Platform), FUN=sum)
f <- list(
  family = "Arial",
  size = 20,
  color = "black"
)
a<-list(
  family = "Arial",
  size = 20,
  color = "black"
)

pxw$Platform<-factor(pxw$Platform)
t6<-table(pxw$Genre, pxw$Platform)

chisq.test(t6)
tab1<-epitab(t6[-5,c(2,1)], conf.level = 1-0.05/3)$tab
tab2<-epitab(t6[-5,c(2,3)], conf.level = 1-0.05/3)$tab
tab3<-epitab(t6[-5,c(1,3)], conf.level = 1-0.05/3)$tab

odds<-c(tab1[,5:7], tab2[,5:7], tab3[,5:7])
odds.df<-setDT(as.data.frame(matrix(odds, ncol=9, dimnames=list(rownames(t6[-5,]), c("PS3 vs Wii", "psmin", "psmax", "Xbox360 vs Wii", "xmin", "xmax", "PS3 vs Xbox360", "min", "max")))), keep.rownames = TRUE)           


ggodds2<-melt(odds.df, id.vars="rn", measure.vars=c("PS3 vs Wii", "Xbox360 vs Wii", "PS3 vs Xbox360"))            
ggmin<-melt(odds.df, id.vars="rn", measure.vars=c("psmin", "xmin","min"))
ggmax<-melt(odds.df, id.vars="rn", measure.vars=c("psmax", "xmax","max"))
ggodds2$min<-ggmin$value
ggodds2$max<-ggmax$value
colnames(ggodds)<-c("Genre", "Platform", "Odds", "min", "max")
ggodds2

###############################################Critic Score vs Sales###########################################################

anova=aov(vgc$Global_Sales~vgc$type3)
TukeyHSD((anova))

lm1<-lm(logsales~Critic_Score, data=vgc[vgc$type3=="PC",])
summary(lm1)
lm2<-lm(logsales~Critic_Score, data=vgc[vgc$type3=="Playstation",])
summary(lm2)
lm3<-lm(logsales~Critic_Score, data=vgc[vgc$type3=="Nintendo",])
summary(lm3)
lm4<-lm(logsales~Critic_Score, data=vgc[vgc$type3=="Xbox",])
summary(lm4)
mod3<-glm(log(Global_Sales)~Genre+Platform+Critic_Score, data=vgc)
summary(mod3)
hist(mod3$residuals)

tdf<-as.data.frame(table(pxw$Genre, pxw$Platform, pxw$Year_of_Release))
colnames(tdf)<-c("Genre", "Console", "Year", "Freq")
tdf$indicator<-ifelse(tdf$Freq==0, 1, 0)


hom<-glm(Freq~Genre+Console+Year+Genre*Console+Genre*Year+Console*Year+indicator, family=quasipoisson, data=tdf)
cndlx<-glm(Freq~Genre+Console+Year+Genre*Console+Genre*Year+indicator, family=quasipoisson, data=tdf)
cndly<-glm(Freq~Genre+Console+Year+Genre*Console+Console*Year+indicator, family=quasipoisson, data=tdf)
cndlz<-glm(Freq~Genre+Console+Year+Console*Year+Genre*Year+indicator, family=quasipoisson, data=tdf)
jntx<-glm(Freq~Genre+Console+Year+Console*Year+indicator, family=quasipoisson, data=tdf)
jnty<-glm(Freq~Genre+Console+Year+Genre*Year+indicator, family=quasipoisson, data=tdf)
jntz<-glm(Freq~Genre+Console+Year+Genre*Console+indicator, family=quasipoisson, data=tdf)
ind<-glm(Freq~Genre+Console+Year+indicator, family=quasipoisson, data=tdf)

models<-matrix(round(c(hom$deviance,cndlx$deviance,cndly$deviance,cndlz$deviance,jntx$deviance,jnty$deviance,jntz$deviance,ind$deviance,  
                       hom$df.residual,cndlx$df.residual,cndly$df.residual,cndlz$df.residual,jntx$df.residual,jnty$df.residual,jntz$df.residual,ind$df.residual), 3), ncol=2)

rownames(models)<-c("(XY, XY, XZ)", "(XY, XZ)", "(XY, YZ)", "(XZ, YZ)","(X, YZ)","(Y,XZ)","(Z,XY)","(X,Y,Z)")
colnames(models)<-c("G^2", "DF")
models
#################################
      #Figures and Graphs#
#################################

c<-scales::seq_gradient_pal("blue")(seq(0,1,length.out=20))
p1<-ggplot(plat.table, aes(x=reorder(Group.1, -x), y=x), fill=factor(Group.1))+
    geom_col(fill=c)+
  theme_bw()+
  theme(title=(element_text(size=12)),axis.title=element_text(size=14), axis.text.x=element_text(size=12, angle=90),axis.text.y=element_text(size=12))+
  labs(title="Figure 1: Global Sales by Platform", x="Platform", y="Global Sales (millions of units)")
p1

p2<-plot_ly(trend.df, x=~year, y=~na.sales, name="North America", type='scatter', mode='lines+markers') %>%
  add_trace(y=~eu.sales, name="Europe", mode='lines+markers') %>%
  add_trace(y=~jp.sales, name='Japan', mode='lines+markers')%>%
  add_trace(y=~other.sales, name='Other', mode='lines+markers')%>%
  layout(title="Figure 2: Total Video Game Sales by Year of Release", font=list(size=12,
    color = 'black', face="bold"),xaxis=list(title="Year"), yaxis=list(title="Sales(in millions)"))
p2


p3<-ggplot(t1df, aes(x=Genre, y=Proportion, fill=Platform))+geom_col(position="dodge", col="black")+
  labs(title="Figure 3: Genres in Consoles vs. PC", x="Genre",y="Proportion")+
  scale_fill_manual(values=c("#3406F4","thistle4"))+
  theme_bw()+
  theme(text=(element_text(size=18)), axis.text.x=element_text(size=14,angle=45,hjust=1, vjust=1),plot.caption=element_text(size=12))
p3

p4<-ggplot(ggodds, aes(x=Platform, y=Odds, fill=Genre))+
  geom_bar(position=position_dodge(),stat="identity", col="black")+
  labs(title="Figure 4: Odds vs. Nintendo", x="Platform", y="Odds Ratio")+
  geom_hline(yintercept=1)+
  geom_errorbar(aes(ymin=min, ymax=max), width=0.4, position=position_dodge(.9))+
  theme_bw()+
  theme(text=(element_text(size=16)), axis.text.x=element_text(size=16),plot.caption=element_text(size=12))+
  scale_y_continuous(breaks=seq(0,6,1))
p4


p5<-plot_ly(time.pxw, x=~time.pxw$Group.1, y=~time.pxw$x, color=~time.pxw$Group.2)%>%
  add_lines() %>%
  
  layout(title="Figure 5: Global Sales of PS3, Wii, and Xbox 360",xaxis=list(title="Year of Release", titlefont=a), yaxis=list(title="Number of Games Sold (in millions)", titlefont=a))
p5

p6<-ggplot(ggodds2, aes(x=Platform, y=Odds, fill=Genre, label=Platform))+
  geom_bar(position=position_dodge(),stat="identity", col="black")+
  labs(title="Figure 6: Odds Comparison of Genre by Platform",x="Platform", y="Odds Ratio")+
  geom_hline(yintercept=1)+
  geom_errorbar(aes(ymin=min, ymax=max), width=0.4, position=position_dodge(.9))+
  theme_bw()+
  theme(axis.text.x=element_text(size=14), aspect.ratio=0.3)+
  scale_y_continuous(breaks=seq(0,6,1))
p6



c<-scales::seq_gradient_pal("green")(seq(0,1,length.out=4))
p7<- ggplot(vgc, aes(x=reorder(type3, -Critic_Score, FUN=median), y=Critic_Score)) +
  geom_boxplot(fill=c, notch=TRUE) +
  labs(title="Figure 7: Distribution of Critic Ratings by Platform", y="Metacritic Rating", x="Platform") +
  geom_hline(yintercept=72)+
  theme_bw()+
  theme(text=(element_text(size=20)), axis.text.x=element_text(size=20),plot.caption=element_text(size=10))
p7



c<-scales::seq_gradient_pal("blue")(seq(0,1,length.out=4))
p8<-ggplot(vgc, aes(x=reorder(type3, -logsales, FUN=median), y=logsales)) +
  geom_boxplot(fill=c, notch=TRUE) +
  labs(title="Figure 8: Distribution of Sales by Platform",y="log(Global Sales)", x="Platform") +
  geom_hline(yintercept=-1.237874)+
  theme_bw()+
  theme(text=(element_text(size=20)), axis.text.x=element_text(size=20),plot.caption=element_text(size=10))
p8

c<-scales::seq_gradient_pal("green")(seq(0,1,length.out=11))

p9<-ggplot(vgc, aes(x=reorder(Genre, -Critic_Score, FUN=median), y=Critic_Score)) +
  geom_boxplot(fill=c, notch=TRUE) +
  labs(title="Figure 9: Distribution of Critic Ratings by Genre",y="Metacritic Rating", x="Genre") +
  geom_hline(yintercept=72)+
  theme_bw()+
  theme(text=(element_text(size=16)), axis.text.x=element_text(size=18,angle=45,hjust=1, vjust=1),plot.caption=element_text(size=14))
p9

p10<-ggplot(vgc, aes(x=Critic_Score, y=logsales, color=type3))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE, size=1.3)+
  theme_bw()+
  theme(text=(element_text(size=16)), axis.text.x=element_text(size=18,angle=45,hjust=1, vjust=1),plot.caption=element_text(size=14))+
  labs(title="Figure 10: Critic Rating vs. Global Sales", x="Critic Rating", y="Log(Global Sales)")
p10