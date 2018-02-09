
c<-scales::seq_gradient_pal("blue")(seq(0,1,length.out=20))
p1<-ggplot(plat.table, aes(x=reorder(Group.1, -x), y=x), fill=factor(Group.1))+
  theme_bw()+
  geom_col(fill=c)+
  theme(title=(element_text(size=12)), axis.text.x=element_text(size=12, angle=90), aspect.ratio=0.4)+
  labs(title="Figure 1: Global Sales by Platform", x="Platform", y="Global Sales (millions of units)")
p1

p2<-plot_ly(trend.df, x=~year, y=~na.sales, name="North America", type='scatter', mode='lines+markers') %>%
  add_trace(y=~eu.sales, name="Europe", mode='lines+markers') %>%
  add_trace(y=~jp.sales, name='Japan', mode='lines+markers')%>%
  add_trace(y=~other.sales, name='Other', mode='lines+markers')%>%
  layout(title="Figure 2: Total Video Game Sales by Year of Release", font=list(
    family = "arial",
    size = 12,
    color = 'black'),xaxis=list(title="Year"), yaxis=list(title="Sales(in millions)"))
p2

p3<-ggplot(sales, aes(x=variable, y=value, fill=Console))+geom_col(position="dodge", color="black")+
  labs(title="Figure 2.3: Platforms with Most Sales by Region", x="Region", y="Units Solds (in millions)")+
  theme_bw()+
  theme(text=element_text(size=18))
p3

p4<-ggplot(gg1, aes(x=genre, y=value, fill=variable))+
  geom_col(position="dodge")+
  labs(title="Figure 2.4: Genres and Global Sales", x="Genre",y="Proportion")+
  scale_fill_manual(values=c("#3406F4","thistle4"),name="Proportion of",breaks=c("proptitles","propsales"), labels=c("Released Titles", "Total Sales"))+
  theme_bw()+
  theme(text=(element_text(size=18)), axis.text.x=element_text(size=14,angle=45,hjust=1, vjust=1),plot.caption=element_text(size=12))
p4


p5<-ggplot(t1df, aes(x=Genre, y=Proportion, fill=Platform))+geom_col(position="dodge", col="black")+
  labs(title="Figure 3: Genres of the Consoles vs. PC", x="Genre",y="Proportion")+
  scale_fill_manual(values=c("#3406F4","thistle4"))+
  theme_bw()+
  theme(text=(element_text(size=18)), axis.text.x=element_text(size=14,angle=45,hjust=1, vjust=1),plot.caption=element_text(size=12))
p5



p6<-ggplot(ggodds, aes(x=Platform, y=Odds, fill=Genre))+
  geom_bar(position=position_dodge(),stat="identity", col="black")+
  labs(title="Figure 4: Odds vs. Nintendo", x="Platform", y="Odds Ratio")+
  geom_hline(yintercept=1)+
  geom_errorbar(aes(ymin=min, ymax=max), width=0.4, position=position_dodge(.9))+
  theme_bw()+
  theme(text=(element_text(size=18)), axis.text.x=element_text(size=16),plot.caption=element_text(size=12))+
  scale_y_continuous(breaks=seq(0,6,1))
p6



p7<-plot_ly(time.pxw, x=~time.pxw$Group.1, y=~time.pxw$x, color=~time.pxw$Group.2)%>%
  add_lines() %>%
  
  layout(title="Figure 5: Global Sales of PS3, Wii, and Xbox 360",xaxis=list(title="Year of Release", titlefont=a), yaxis=list(title="Number of Games Sold (in millions)", titlefont=a))
p7


pxw<-vgc[vgc$Platform %in% c("PS3", "X360", "Wii"),]
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
colnames(ggodds2)<-c("Genre", "Platform", "Odds", "min", "max")
ggodds2

p8<-ggplot(ggodds2, aes(x=Platform, y=Odds, fill=Genre, label=Platform))+
  geom_bar(position=position_dodge(),stat="identity", col="black")+
  labs(title="Figure 6: Odds Comparison of Genre by Platform",x="Platform", y="Odds Ratio")+
  geom_hline(yintercept=1)+
  geom_errorbar(aes(ymin=min, ymax=max), width=0.4, position=position_dodge(.9))+
  theme_bw()+
  theme(axis.text.x=element_text(size=14), aspect.ratio=0.3)+
  scale_y_continuous(breaks=seq(0,6,1))
p8



c<-scales::seq_gradient_pal("green")(seq(0,1,length.out=4))
p9<- ggplot(vgc, aes(x=reorder(type3, -Critic_Score, FUN=median), y=Critic_Score)) +
  geom_boxplot(fill=c, notch=TRUE) +
  labs(title="Figure 7: Distribution of Critic Ratings by Platform", y="Metacritic Rating", x="Platform") +
  geom_hline(yintercept=72)+
  theme_bw()+
  theme(text=(element_text(size=16)), axis.text.x=element_text(size=20),plot.caption=element_text(size=10))
p9



c<-scales::seq_gradient_pal("blue")(seq(0,1,length.out=4))
p10<-ggplot(vgc, aes(x=reorder(type3, -logsales, FUN=median), y=logsales)) +
  geom_boxplot(fill=c, notch=TRUE) +
  labs(title="Figure 8: Distribution of Sales by Platform",y="log(Global Sales)", x="Platform") +
  geom_hline(yintercept=-1.237874)+
  theme_bw()+
  theme(text=(element_text(size=20)), axis.text.x=element_text(size=20),plot.caption=element_text(size=10))
p10

c<-scales::seq_gradient_pal("green")(seq(0,1,length.out=11))
p11<-ggplot(vgc, aes(x=reorder(Genre, -Critic_Score, FUN=median), y=Critic_Score)) +
  geom_boxplot(fill=c, notch=TRUE) +
  labs(title="Figure 9: Distribution of Critic Ratings by Genre",y="Metacritic Rating", x="Genre") +
  geom_hline(yintercept=72)+
  theme_bw()+
  theme(text=(element_text(size=16)), axis.text.x=element_text(size=18,angle=45,hjust=1, vjust=1),plot.caption=element_text(size=14))
p11

vgc$Platform<-vgc$type3
p12<-ggplot(vgc, aes(x=Critic_Score, y=logsales, color=type3))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE, size=1.3)+
  theme_bw()+
  theme(text=(element_text(size=16)), axis.text.x=element_text(size=18,angle=45,hjust=1, vjust=1),plot.caption=element_text(size=14))+

  labs(title="Figure 10: Critic Rating vs. Global Sales", x="Critic Rating", y="Log(Global Sales)")+
  labs(color="Platform")
p12