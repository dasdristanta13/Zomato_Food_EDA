library("ggplot2")
library('dplyr')
library("ggcorrplot")
library("tidyverse")

theme_set(theme_dark())
zom<-read.csv("zomato_restaurants_in_India.csv")

#Data Cleaning
zom2<-select(zom,-c('url','zipcode','country_id','delivery','takeaway','opentable_support'))
zom2<-subset(zom2,establishment!="[]")

zom2$establishment <- gsub("\\'|\\'","",gsub("\\[|\\]","",zom2$establishment))

#Q1.establishment vs counts

ggplot(data = zom2,aes(y=reorder(establishment,establishment, function(x) tapply(x,x,length)),fill=establishment)) + geom_bar(col="white")+ylab("ESTABLISHMENT")+scale_fill_discrete(name = "Establishment")+labs(title = "Establishment types available in Zomato")+theme(axis.text.y=element_text(face=c("bold.italic") ,size = 11),legend.position = 'None')

#2.)#aggregate rating vs avg cost (0 avg. cost to be removed)
zom2$rating_text <- factor(cut(zom2$aggregate_rating,seq(0,5,length.out = 6),include.lowest=T),labels= c("Very Bad","Bad","Good","Very Good","Excellent")) 

p1<-ggplot(zom2,aes(x=aggregate_rating,y=average_cost_for_two))+geom_point(aes(col=rating_text))+geom_smooth()+xlab("Aggregate rating")+ylab("Average cost for two")+labs(title = "Scatterplot of average cost for two people with aggregate rating")
p1$labels$colour="Rating Text"
p1

#3.)To analyse cities which have higher or lower normalised cost
zom2$relative_cost_for_two<-round((zom2$average_cost_for_two-mean(zom2$average_cost_for_two))/sd(zom2$average_cost_for_two),2)
zom2$cost_type<-ifelse(zom2$relative_cost_for_two<0,"below","above")
zom3 <- zom2[order(zom2$relative_cost_for_two), ]
ggplot(zom2, aes(y=city, x=relative_cost_for_two, label=relative_cost_for_two)) + 
  geom_bar(stat='identity', aes(fill=cost_type), width=.8)  +
  scale_fill_manual( name = " Cost Type",
                     labels = c("Above Average", "Below Average"), 
                     values = c("above"="#FF0000", "below"="#00FFFF")) + 
  labs(subtitle="Normalised Cost For Two", 
       title= "Diverging Bars") + ylab("City")+ xlab("Relative Cost for Two")+
  theme(axis.text.y=element_text(face=c("italic","bold") ,size = 6))



#above subplot
above <- zom2[(zom2$cost_type=="above"),] %>% arrange(desc(relative_cost_for_two))
above
ggplot(above, aes(y=reorder(city,relative_cost_for_two),fill=city, x=relative_cost_for_two, label=relative_cost_for_two)) + 
  geom_bar(stat='identity', width=.8)+theme(axis.text.y=element_text(face="bold" ,size = 6))+labs(title = "Subplot showing Higher Cost Type")+xlab("Relative Cost For two")+ylab("City")+ theme(legend.position = "None")


#below subplot
below <- zom2[(zom2$cost_type=="below"),] %>% arrange(desc(relative_cost_for_two))

ggplot(below, aes(y=reorder(city,relative_cost_for_two),fill=city, x=relative_cost_for_two, label=relative_cost_for_two)) + 
  geom_bar(stat='identity', width=.8)+theme(axis.text.y=element_text(face="bold" ,size = 6))+labs(title = "Subplot showing Lower Cost Type")+xlab("Relative Cost For two")+ylab("City")+theme(legend.position = "None")




#4)
a<-aggregate(list(zom2$locality),list(zom2$city),length)
colnames(a)<-c("City","Count")
a<-a%>%arrange(desc(Count))

ggplot(head(a,10),aes(fill=City,x=Count,y=reorder(City,Count)))+geom_bar(stat = "identity")+labs(title = "Top 10 cities with most number of establishment")+ylab("City")+theme(axis.text.y=element_text(face=c("italic") ,size = 10),legend.position = "None")




#5.)#Top popular foods
v<-list(trimws(unlist(str_split(zom2$cuisines,",")),'l'))
v1<-aggregate(v,v,length)
colnames(v1)<-c("food_type","count")
v1<-arrange(v1,desc(v1$count))
v1
v2 <- v1[-46,]

v2$food_type
ggplot(head(v2, n=20),aes(x=count,y=reorder(food_type,count),fill=food_type))+geom_bar(stat = "identity",col="white")+ylab("Food Type")+scale_fill_discrete(name="FOOD TYPE")+labs(title = "Famous Food types Available in Zomato")+theme(axis.text.y=element_text(face=c("bold.italic") ,size = 11),legend.position = "None")+geom_text(aes(label=count),stat = 'identity',vjust=0.4,hjust=1,col="white")

ggplot(tail(v2, n=20),aes(x=count,y=reorder(food_type,-count),fill=food_type))+geom_bar(stat = "identity",col="white")+ylab("Food Type")+scale_fill_discrete(name="FOOD TYPE")+labs(title = "Unpopular Food types Available in Zomato")+theme(axis.text.y=element_text(face=c("bold.italic") ,size = 11),legend.position = "None")+geom_text(aes(label=count),stat = 'identity',vjust=0.4,hjust=1,col="white")


#6)#top 50 returants with highest votes
name1<-list(zom2$name)
name2<-aggregate(zom2$votes,name1,sum)
length(name2$Group.1)
colnames(name2)<-c("name","votes")
name2<-arrange(name2,desc(votes))
ggplot(head(name2,n=20),aes(x=votes,y=reorder(name,votes),fill=votes))+geom_bar(stat = "identity",col="white")+xlab("VOTES")+ylab("RESTAURENT NAME")+labs(title = "TOP 50 RESTAURENT WITH RESPECT TO VOTE",fill="votes")+theme(axis.text.y=element_text(face=c("bold.italic"),size = 11))+geom_text(aes(label=votes),stat = 'identity',vjust=0.4,hjust=1,col="white")+theme(legend.position = "None")


#7)#top average_cost_for_two resturant/hotel count to see at which price the competition is highest
factor(zom2$average_cost_for_two)
avg_cost1<-list(zom2$average_cost_for_two)
avg_cost<-aggregate(avg_cost1,avg_cost1,length)
colnames(avg_cost)<-c("average_cost","count")
avg_cost<-arrange(avg_cost,desc(count))
ggplot(head(avg_cost,n=25),aes(x=count,y=reorder(average_cost,count),fill=average_cost))+geom_bar(stat = "identity",col="white") + ylab("Average Cost")+labs(title = "MOST POPULAR FOOD-COST FOR TWO",fill="AVERAGE COST")+geom_text(aes(label=count),stat = 'identity',vjust=0.4,hjust=1,col="white")+theme(axis.text.y=element_text(face=c("bold.italic") ,size = 11),legend.position = "None")


#8)piechart CASH/CREDIT/DEBIT/Digital
###########            7 Pie Chart            ##################
zom2$highlights <- gsub("\\[|\\]","",zom2$highlights)
zom2$highlights <- gsub("\\'|\\'","",zom2$highlights)
v3 <- list(trimws(unlist(str_split(zom2$highlights,",")),'l'))
v4<-  aggregate(v3, v3, length)
v4<-v4[-1,]
colnames(v4) <- c("Highlights","Count") 
v4 <- arrange(v4,desc(v4$Count))
v4
subv4<- subset(v4,v4$Highlights=="Cash" | v4$Highlights=="Credit Card" | v4$Highlights=="Debit Card" | v4$Highlights=="Digital Payments Accepted")

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
subv4<-subv4%>%group_by(Count)%>%ungroup()%>%mutate(per=Count/sum(Count))
subv4
ggplot(subv4, aes(x="", y=per, fill=Highlights)) +
  geom_bar(width = 1, stat = "identity", position = "stack",col="white") +
  coord_polar("y", start=0) + xlab("")+ylab("")+ blank_theme +
  theme(axis.text.x=element_blank())+geom_text(aes(label=paste0((round(per*100,digits = 2)),"%")),position = position_stack(vjust = 0.5))+
  labs(title="TRANSACTION MODES")

#9)correlation b/w votes vs Photo count
df1 <- data.frame(Votes=zom2$votes,Photo_Count=zom2$photo_count,Average_cost=zom2$average_cost_for_two) 
rownames(df1)
cor(df1)
df2 <- cor(df1)
df2

ggcorrplot(df2,hc.order = T,outline.color = "white",lab=TRUE)+labs(title="CORRELATION OF PHOTO COUNT VS VOTES VS AVG. COST ")
#10)#########rating text with 5 level #################
levels(factor(zom2$rating_text))
dzom1<-zom2%>%count(rating_text)
dzom1<-dzom1%>%group_by(n)%>%ungroup()%>%mutate(per=n/sum(n))

#pie chart
ggplot(dzom1, aes(x="", y=per, fill=rating_text)) +
  geom_bar(width = 1, stat = "identity", position = "stack",col="white") +
  coord_polar("y", start=0) + xlab("")+ylab("") + blank_theme +
  theme(axis.text.x=element_blank())+geom_text(aes(label=paste0((round(per*100,digits = 2)),"%")),position = position_stack(vjust = 0.5))+labs(title="OVERALL RATING",fill="RATING")


##################bar plot with count of establishment vs rating text
ggplot(zom2,aes(x=establishment,y=average_cost_for_two))+geom_bar(position='dodge',stat = "identity",aes(fill=rating_text))+
  theme(axis.text.x=element_text(face=c("bold.italic"),vjust = 0.3 ,size = 11),axis.text.y=element_text(face=c("bold.italic") ,size = 11),legend.position="top")+theme(axis.text.x=element_text(colour = "black", angle = 90))+xlab("Establishment")+ylab("Count ")+labs(fill="Rating Text",title = "Rating variation in different establishments")








#11)relative cost for two vs establishments
#zom2$relative_cost_for_two<-round((zom2$average_cost_for_two-mean(zom2$average_cost_for_two))/sd(zom2$average_cost_for_two),2)
zom2$cost_type<-ifelse(zom2$relative_cost_for_two<0,"below","above")
zom3 <- zom2[order(zom2$relative_cost_for_two), ]
ggplot(zom2, aes(y=establishment, x=relative_cost_for_two, label=relative_cost_for_two)) + 
  geom_bar(stat='identity', aes(fill=cost_type), width=.8)  +
  scale_fill_manual( name = " Cost Type",
                     labels = c("Above Average", "Below Average"), 
                     values = c("above"="#FF0000", "below"="#00FFFF")) + 
  labs(subtitle="Normalised Cost For Two", 
       title= "Diverging Bars") + ylab("Establishment")+ xlab("Relative Cost for Two")+theme(axis.text.x=element_text(face=c("bold.italic") ,size = 11),axis.text.y=element_text(face=c("bold.italic") ,size = 11))

#12) Rating Consistency of Establishments violin plot
ggplot(zom2, aes(x=establishment,y=aggregate_rating,fill=establishment)) + geom_violin(scale = "area") +theme(axis.text.x=element_text(colour = "black", angle = 90,vjust = 0.3))+xlab("Establishment")+ylab("Aggregate rating")+labs(title="Rating Consistency of Establishments")+theme(axis.text.y=element_text(face=c("bold.italic") ,size = 11),axis.text.x=element_text(face=c("bold.italic") ,size = 11),legend.position = "None")




######13)association of votes vs photo count with respect to rating text
ggplot(zom2,aes(x=photo_count,y=votes))+facet_wrap(~rating_text,scales = 'free')+geom_point(aes(col=factor(rating_text)))+theme(axis.text.y=element_text(face=c("bold.italic") ,size = 11),axis.text.x=element_text(face=c("bold.italic") ,size = 11),legend.position = 'None')+xlab("Photo Count")+labs(title = "Association of votes and photo counts with different ratings")+ylab("Votes")


###########################################################################

###########  Some top franchise with popular food or shop names  ##########

###########################################################################

#14)Famous Pizza joints
x<-zom2$name[grep("Pizza",zom2$name)]
df11<-data.frame(x)
colnames(df11)<-c("name")
head(df11)
df11%>%group_by(name)%>%summarize(count=n())
df111<-df11%>%group_by(name)%>%summarize(count=n())
df112<-arrange(df111,desc(df111$count))
ggplot(head(df112, n=5),aes(x=count,y=reorder(name,count),fill=name))+geom_bar(stat = "identity")+ylab("Names")+scale_fill_discrete(name="FOOD TYPE")+labs(title = "Famous Pizza joints available in zomato")+theme(axis.text.y=element_text(face=c("bold.italic") ,size = 11),legend.position = "None")


#15)Famous bars in Zomato
x<-zom2$name[grep("Bar$",zom2$name)]
df11<-data.frame(x)
colnames(df11)<-c("name")
head(df11)
df11%>%group_by(name)%>%summarize(count=n())
df111<-df11%>%group_by(name)%>%summarize(count=n())
df112<-arrange(df111,desc(df111$count))
ggplot(head(df112, n=5),aes(x=count,y=reorder(name,count),fill=name))+geom_bar(stat = "identity")+ylab("Names")+scale_fill_discrete(name="FOOD TYPE")+labs(title = "Famous Bars available in zomato")+theme(axis.text.y=element_text(face=c("bold.italic") ,size = 11),legend.position = "None")


#16)famous sweet shop
x<-zom2$name[grep("Sweets",zom2$name)]
df11<-data.frame(x)
colnames(df11)<-c("name")
head(df11)
df11%>%group_by(name)%>%summarize(count=n())
df111<-df11%>%group_by(name)%>%summarize(count=n())
df112<-arrange(df111,desc(df111$count))
ggplot(head(df112, n=5),aes(x=count,y=reorder(name,count),fill=name))+geom_bar(stat = "identity")+ylab("Names")+scale_fill_discrete(name="FOOD TYPE")+labs(title = "Famous Sweet Shop available in Zomato")+theme(axis.text.y=element_text(face=c("bold.italic") ,size = 11),legend.position = "None")


#17)famous cake shop

x<-zom2$name[grep("Cakes",zom2$name)]
df11<-data.frame(x)
colnames(df11)<-c("name")
head(df11)
df11%>%group_by(name)%>%summarize(count=n())
df111<-df11%>%group_by(name)%>%summarize(count=n())
df112<-arrange(df111,desc(df111$count))
ggplot(head(df112, n=5),aes(x=count,y=reorder(name,count),fill=name))+geom_bar(stat = "identity")+ylab("Names")+scale_fill_discrete(name="FOOD TYPE")+labs(title = "Famous Cake shop available in Zomato")+theme(axis.text.y=element_text(face=c("bold.italic") ,size = 11),legend.position = "None")

#18)famous cofee house
x<-zom2$name[grep("Coffee",zom2$name)]
df11<-data.frame(x)
colnames(df11)<-c("name")
head(df11)
df11%>%group_by(name)%>%summarize(count=n())
df111<-df11%>%group_by(name)%>%summarize(count=n())
df112<-arrange(df111,desc(df111$count))
ggplot(head(df112, n=5),aes(x=count,y=reorder(name,count),fill=name))+geom_bar(stat = "identity")+ylab("Names")+scale_fill_discrete(name="FOOD TYPE")+labs(title = "Famous Coffee Shop available in Zomato")+theme(axis.text.y=element_text(face=c("bold.italic") ,size = 11),legend.position = "None")

#19)famous barbecue
x<-zom2$name[grep("Barbe",zom2$name)]
df11<-data.frame(x)
colnames(df11)<-c("name")
head(df11)
df11%>%group_by(name)%>%summarize(count=n())
df111<-df11%>%group_by(name)%>%summarize(count=n())
df112<-arrange(df111,desc(df111$count))
ggplot(head(df112, n=5),aes(x=count,y=reorder(name,count),fill=name))+geom_bar(stat = "identity")+ylab("Names")+scale_fill_discrete(name="FOOD TYPE")+labs(title = "Famous Barbecue Available in Zomato")+theme(axis.text.y=element_text(face=c("bold.italic") ,size = 11),legend.position = "None")


#20)famous breweries
x<-zom2$name[grep("Brew",zom2$name)]
df11<-data.frame(x)
colnames(df11)<-c("name")
head(df11)
df11%>%group_by(name)%>%summarize(count=n())
df111<-df11%>%group_by(name)%>%summarize(count=n())
df112<-arrange(df111,desc(df111$count))
ggplot(head(df112, n=5),aes(x=count,y=reorder(name,count),fill=name))+geom_bar(stat = "identity")+ylab("Names")+scale_fill_discrete(name="FOOD TYPE")+labs(title = "Famous Breweries Available in Zomato")+theme(axis.text.y=element_text(face=c("bold.italic") ,size = 11),legend.position = "None")


#21)famous food joints
x<-zom2$name[grep("Food",zom2$name)]
df11<-data.frame(x)
colnames(df11)<-c("name")
head(df11)
df11%>%group_by(name)%>%summarize(count=n())
df111<-df11%>%group_by(name)%>%summarize(count=n())
df112<-arrange(df111,desc(df111$count))
ggplot(head(df112, n=5),aes(x=count,y=reorder(name,count),fill=name))+geom_bar(stat = "identity")+ylab("Names")+scale_fill_discrete(name="FOOD TYPE")+labs(title = "Famous Food joints Available in Zomato")+theme(axis.text.y=element_text(face=c("bold.italic") ,size = 11),legend.position = "None")


#22)famous biriyani
x<-zom2$name[grep("Biriyani",zom2$name)]
df11<-data.frame(x)
colnames(df11)<-c("name")
head(df11)
df11%>%group_by(name)%>%summarize(count=n())
df111<-df11%>%group_by(name)%>%summarize(count=n())
df112<-arrange(df111,desc(df111$count))
ggplot(head(df112, n=5),aes(x=count,y=reorder(name,count),fill=name))+geom_bar(stat = "identity")+ylab("Names")+scale_fill_discrete(name="FOOD TYPE")+labs(title = "Famous Biriyani Shops Available in Zomato")+theme(axis.text.y=element_text(face=c("bold.italic") ,size = 11),legend.position = "None")


#23)famous cafe


x<-zom2$name[grep("Cafe",zom2$name)]
df11<-data.frame(x)
colnames(df11)<-c("name")
head(df11)
df11%>%group_by(name)%>%summarize(count=n())
df111<-df11%>%group_by(name)%>%summarize(count=n())
df112<-arrange(df111,desc(df111$count))
ggplot(head(df112, n=5),aes(x=count,y=reorder(name,count),fill=name))+geom_bar(stat = "identity")+ylab("Names")+scale_fill_discrete(name="FOOD TYPE")+labs(title = "Famous Cafe Available in Zomato")+theme(axis.text.y=element_text(face=c("bold.italic") ,size = 11),legend.position = "None")

#24)famous dhabas in Zomato
x<-zom2$name[grep("Dhaba",zom2$name)]
df11<-data.frame(x)
colnames(df11)<-c("name")
head(df11)
df11%>%group_by(name)%>%summarize(count=n())
df111<-df11%>%group_by(name)%>%summarize(count=n())
df112<-arrange(df111,desc(df111$count))
ggplot(head(df112, n=5),aes(x=count,y=reorder(name,count),fill=name))+geom_bar(stat = "identity")+ylab("Names")+scale_fill_discrete(name="FOOD TYPE")+labs(title = "Famous Dhaba Available in Zomato")+theme(axis.text.y=element_text(face=c("bold.italic") ,size = 11),legend.position = "None")

