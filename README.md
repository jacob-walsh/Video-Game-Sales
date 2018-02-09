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
![alt text](https://raw.githubusercontent.com/jacob-walsh/Video-Game-Sales/EDA Visuals/path/to/plot1.png)
