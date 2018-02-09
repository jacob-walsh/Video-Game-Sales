# Video-Game-Sales
Introduction

Over the past few decades consoles for playing video games have become commonplace in houses across the country.  Software and hardware advancements of the personal computer (PC) have led to many different machines being developed.  Every year, the video game industry churns out hundreds of titles and sells hundreds of millions of units around the globe.  Many consoles have come and gone, but a few modern machines have carved their niche in society and continue to evolve and thrive.  Xbox (developed by Microsoft), Playstation (developed by Sony), Nintendo, and the personal computer are examples of the platforms that have the biggest hit games from a variety of publishers.  
In this report, the following questions of interest are addressed:

•	Is there any evidence of differences between sales in different regions of the world?

•	Are there any associations when comparing the genre of a game and the platform on which it is released?

•	Are there any associations in sales or titles when we consider more current popular platforms?

•	Do critic ratings correlate with sales?  Do critics tend to favor some publishers/genres/platforms over others?

## Data Import and Cleaning
 Video Game Charts (VGChartz.com), a business intelligence and research firm, publishes sales data for video games with a game database that includes over 40,000 titles. A set of 16,900 titles has been compiled by the Kaggle user Gregory Smith from the vgchartz.com website, and is published in public use data set section on Kaggle.  The information gathered includes the publisher of the game, the platform that the game was released on, the year of release, and the genre of the game.  Sales information is included from North America, Europe, Japan as well as total worldwide sales through the end of 2016.  Only video game titles that had sales greater than 100,000 copies worldwide are included. Their methodology can be referenced on their website: http://www.vgchartz.com/methodology.php.  

In addition to this information, the site metacritic.com gathers multiple ratings of video game titles and combines the ratings into a single “Metascore” for the game.  This single score summarizes the many entertainment reviews that are available for the game.  These critic scores are available for approximately 6900 of the video game titles in the vgchartz data and were added by Kaggle user Rush Kirubi.  

```
##read in data
vg.sales<-read.csv("C:/Penn State MAS/Stat 581/Video_Games_Sales_as_at_22_Dec_2016.csv", na.strings=c("", "N/A", "Misc", "<NA>"))


##New Categories and Variables
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
```
## EDA
```
c<-scales::seq_gradient_pal("blue")(seq(0,1,length.out=20))
p1<-ggplot(plat.table, aes(x=reorder(Group.1, -x), y=x), fill=factor(Group.1))+
    geom_col(fill=c)+
  theme_bw()+
  theme(title=(element_text(size=12)),axis.title=element_text(size=14), axis.text.x=element_text(size=12, angle=90),axis.text.y=element_text(size=12))+
  labs(title="Figure 1: Global Sales by Platform", x="Platform", y="Global Sales (millions of units)")
```
![Alt Text](https://github.com/jacob-walsh/Video-Game-Sales/blob/master/EDA%20Visuals/plot1.png)
```
p2<-plot_ly(trend.df, x=~year, y=~na.sales, name="North America", type='scatter', mode='lines+markers') %>%
  add_trace(y=~eu.sales, name="Europe", mode='lines+markers') %>%
  add_trace(y=~jp.sales, name='Japan', mode='lines+markers')%>%
  add_trace(y=~other.sales, name='Other', mode='lines+markers')%>%
  layout(title="Figure 2: Total Video Game Sales by Year of Release", font=list(size=12,
    color = 'black', face="bold"),xaxis=list(title="Year"), yaxis=list(title="Sales(in millions)"))

```
![Alt Text](https://github.com/jacob-walsh/Video-Game-Sales/blob/master/EDA%20Visuals/plot2.png)

```
p3<-ggplot(t1df, aes(x=Genre, y=Proportion, fill=Platform))+geom_col(position="dodge", col="black")+
  labs(title="Figure 3: Genres in Consoles vs. PC", x="Genre",y="Proportion")+
  scale_fill_manual(values=c("#3406F4","thistle4"))+
  theme_bw()+
  theme(text=(element_text(size=18)), axis.text.x=element_text(size=14,angle=45,hjust=1, vjust=1),plot.caption=element_text(size=12))
```
![Alt Text](https://github.com/jacob-walsh/Video-Game-Sales/blob/master/EDA%20Visuals/plot3.png)
```
p4<-ggplot(ggodds, aes(x=Platform, y=Odds, fill=Genre))+
  geom_bar(position=position_dodge(),stat="identity", col="black")+
  labs(title="Figure 4: Odds vs. Nintendo", x="Platform", y="Odds Ratio")+
  geom_hline(yintercept=1)+
  geom_errorbar(aes(ymin=min, ymax=max), width=0.4, position=position_dodge(.9))+
  theme_bw()+
  theme(text=(element_text(size=16)), axis.text.x=element_text(size=16),plot.caption=element_text(size=12))+
  scale_y_continuous(breaks=seq(0,6,1))
```
![Alt Text](https://github.com/jacob-walsh/Video-Game-Sales/blob/master/EDA%20Visuals/plot4.png)
```
p5<-plot_ly(time.pxw, x=~time.pxw$Group.1, y=~time.pxw$x, color=~time.pxw$Group.2)%>%
  add_lines() %>%
  
  layout(title="Figure 5: Global Sales of PS3, Wii, and Xbox 360",xaxis=list(title="Year of Release", titlefont=a), yaxis=list(title="Number of Games Sold (in millions)", titlefont=a))
```
![Alt Text](https://github.com/jacob-walsh/Video-Game-Sales/blob/master/EDA%20Visuals/plot5.png)
```
p6<-ggplot(ggodds2, aes(x=Platform, y=Odds, fill=Genre, label=Platform))+
  geom_bar(position=position_dodge(),stat="identity", col="black")+
  labs(title="Figure 6: Odds Comparison of Genre by Platform",x="Platform", y="Odds Ratio")+
  geom_hline(yintercept=1)+
  geom_errorbar(aes(ymin=min, ymax=max), width=0.4, position=position_dodge(.9))+
  theme_bw()+
  theme(axis.text.x=element_text(size=14), aspect.ratio=0.3)+
  scale_y_continuous(breaks=seq(0,6,1))
```
![Alt Text](https://github.com/jacob-walsh/Video-Game-Sales/blob/master/EDA%20Visuals/plot6.png)
```
p7<- ggplot(vgc, aes(x=reorder(type3, -Critic_Score, FUN=median), y=Critic_Score)) +
  geom_boxplot(fill=c, notch=TRUE) +
  labs(title="Figure 7: Distribution of Critic Ratings by Platform", y="Metacritic Rating", x="Platform") +
  geom_hline(yintercept=72)+
  theme_bw()+
  theme(text=(element_text(size=20)), axis.text.x=element_text(size=20),plot.caption=element_text(size=10))
```
![Alt Text](https://github.com/jacob-walsh/Video-Game-Sales/blob/master/EDA%20Visuals/plot7.png)
```
c<-scales::seq_gradient_pal("blue")(seq(0,1,length.out=4))
p8<-ggplot(vgc, aes(x=reorder(type3, -logsales, FUN=median), y=logsales)) +
  geom_boxplot(fill=c, notch=TRUE) +
  labs(title="Figure 8: Distribution of Sales by Platform",y="log(Global Sales)", x="Platform") +
  geom_hline(yintercept=-1.237874)+
  theme_bw()+
  theme(text=(element_text(size=20)), axis.text.x=element_text(size=20),plot.caption=element_text(size=10))
```

![Alt Text](https://github.com/jacob-walsh/Video-Game-Sales/blob/master/EDA%20Visuals/plot8.png)
```
c<-scales::seq_gradient_pal("green")(seq(0,1,length.out=11))

p9<-ggplot(vgc, aes(x=reorder(Genre, -Critic_Score, FUN=median), y=Critic_Score)) +
  geom_boxplot(fill=c, notch=TRUE) +
  labs(title="Figure 9: Distribution of Critic Ratings by Genre",y="Metacritic Rating", x="Genre") +
  geom_hline(yintercept=72)+
  theme_bw()+
  theme(text=(element_text(size=16)), axis.text.x=element_text(size=18,angle=45,hjust=1, vjust=1),plot.caption=element_text(size=14))
```
![Alt Text](https://github.com/jacob-walsh/Video-Game-Sales/blob/master/EDA%20Visuals/plot9.png)
```
p10<-ggplot(vgc, aes(x=Critic_Score, y=logsales, color=type3))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE, size=1.3)+
  theme_bw()+
  theme(text=(element_text(size=16)), axis.text.x=element_text(size=18,angle=45,hjust=1, vjust=1),plot.caption=element_text(size=14))+
  labs(title="Figure 10: Critic Rating vs. Global Sales", x="Critic Rating", y="Log(Global Sales)")
```
![Alt Text](https://github.com/jacob-walsh/Video-Game-Sales/blob/master/EDA%20Visuals/plot10.png)


