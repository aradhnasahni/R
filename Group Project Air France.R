#######################################
## Created by Team 4 - Air France Group Project
#######################################
## 2.03.19 @ Hult
#######################################
#######################################


#INSTALLING PACKAGES
install.packages("qdap")
install.packages("tm")
install.packages("ggthemes")
install.packages("dendextend")
install.packages("tidyverse")
install.packages("SnowballC")

#LOADING LIBRARIES
library(dendextend)
library(gapminder)
library(ggplot2)
library(ggthemes)
library(plotly)
library(qdap)
library(readxl)
library(rpart)
library(SnowballC)
library(sqldf)
library(tidyverse)
library(tm)
library(wordcloud)

air_france <- read_excel("Desktop/R class/air france.xlsx")#reading dataset into environment

anyNA(air_france) #checking for null values

air_france_copy <- air_france #making a new dataset with air_france


############################DATA EXPLORATION############################
summary(air_france_copy)
map(air_france, ~sum(is.na(.))) #counting null values for each column
na_subset <- air_france[rowSums(is.na(air_france)) == 1,] #making new subset of rows with null values

#getting unique values of all columns
for(i in 1:ncol(air_france_copy)){
  print(unique(air_france_copy[,i]))
} 

#replacing n/a character values for comprehensibility
air_france_copy$`Match Type` <- replace(air_france_copy$`Match Type`,
  which(air_france_copy$`Match Type`=="N/A"),"Unmatched")

#Creating new column for bidding strategy 
unique(air_france_copy$`Bid Strategy`)
air_france_copy$Bid_S <- air_france_copy$`Bid Strategy`
air_france_copy$Bid_S<-gsub("Postiion 1-4 Bid Strategy","Position 1-4 Bid Strategy",air_france_copy$Bid_S) #######changing the values to 0 and 1
air_france_copy$Bid_S<-gsub("Position 1 -2 Target","Position 1-2 Target",air_france_copy$Bid_S)
unique(air_france_copy$Bid_S)

#creating new column for ROA and Profit
air_france_copy$ROA <- air_france_copy$Amount/ air_france_copy$`Total Cost`
air_france_copy$Profit <- air_france_copy$Amount - air_france_copy$`Total Cost`\

#counting publisher observations
sqldf("select count(`Publisher Name`),`Publisher Name`  from air_france group by `Publisher Name`
      ") 

#Impressions by Campaign
sqldf("SELECT Campaign, avg(Impressions), max(Impressions), min(Impressions),count(Campaign)
      FROM air_france_copy
      GROUP BY campaign
      order by avg(Impressions) DESC
      ")

#Amount by campaign
sqldf("SELECT Campaign, avg(Amount), max(Amount), Min(Amount),count(Campaign)
      FROM air_france_copy
      GROUP BY campaign
      order by avg(Amount) DESC
      ")

#Total Cost by Campaign
sqldf("SELECT Campaign, avg(`Total Cost`), max(`Total Cost`), Min(`Total Cost`), count(Campaign)
      FROM air_france_copy
      GROUP BY campaign
      order by avg(`Total Cost`) DESC
      ")

#Bookings by Campaign
sqldf("SELECT Campaign, avg(`Total Volume of Bookings`), max(`Total Volume of Bookings`), Min(`Total Volume of Bookings`), count(Campaign)
      FROM air_france_copy
      GROUP BY campaign
      order by avg(`Total Volume of Bookings`) DESC
      ")

#Profit by campaign
sqldf("SELECT campaign, avg(Profit), max(Profit), Min(Profit), count(Campaign)
      FROM air_france_copy
      GROUP BY campaign
      order by avg(Profit) DESC
      ")

#Clicks by bidding strategy
sqldf("SELECT Bid_S, avg(Clicks), max(Clicks), min(clicks), count(Bid_S)
      FROM air_france_copy
      GROUP BY Bid_S
      order by avg(Clicks) DESC
      ")
 
#Impressions by bidding strategy
sqldf("SELECT Bid_S, avg(Impressions), max(Impressions), min(Impressions), count(Bid_S)
      FROM air_france_copy
      GROUP BY Bid_S
      order by avg(Impressions) DESC
      ")

#counting bid strategy observations; most common is Position 5-10 Bid Strategy
sqldf("select a.`Bid Strategy`,count(a.`Publisher Name`)
      from air_france a
      group by a.`Bid Strategy`
      ")

#Amount by bidding strategy
sqldf("SELECT Bid_s, avg(Amount), max(Amount), Min(Amount),count(Bid_S)
      FROM air_france_copy
      GROUP BY Bid_s
      order by avg(Amount) DESC
      ")

#Total Cost by bidding strategy
sqldf("SELECT Bid_s, avg(`Total Cost`), max(`Total Cost`), Min(`Total Cost`), count(Bid_S)
      FROM air_france_copy
      GROUP BY Bid_s
      order by avg(`Total Cost`) DESC
      ")

#creating binary column for booking volumes
air_france_copy$booking_vol_binary <- NA
for(i in 1:nrow(air_france_copy)){
  if(air_france_copy$`Total Volume of Bookings`[i] == 0){
    air_france_copy$booking_vol_binary[i]<- 0
  }else{air_france_copy$booking_vol_binary[i]<- 1}
}  

#Profit by bidding strategy
sqldf("SELECT Bid_s, avg(Profit), max(Profit), Min(Profit),count(Bid_s), count(Bid_S)
      FROM air_france_copy
      GROUP BY Bid_s
      order by avg(Profit) DESC
      ")

#Bookings by bidding strategy
sqldf("SELECT Bid_s, avg(`Total Volume of Bookings`), max(`Total Volume of Bookings`), Min(`Total Volume of Bookings`), count(Bid_S)
      FROM air_france_copy
      GROUP BY Bid_s
      order by avg(`Total Volume of Bookings`) DESC
      ")

comparison<-sqldf("select `Publisher Name`, SUM(Clicks) AS Clicks
                  from air_france_copy
                  group by `Publisher Name`")
plott <- plot_ly(data=comparison, x=~`Publisher Name`, y = ~Clicks, alpha=0.8, color =~ `Publisher Name`)%>%
  layout(title= "Total Clicks by Publishers", yaxis = list(title = "Total Clicks"),xaxis=list(title="Publishers"))
print(plott)

#comparing total cost spent on no purchase and purchase
total_cost <- sqldf("SELECT a.booking_vol_binary, SUM(a.`Total Cost`)
                    FROM air_france_copy a
                    GROUP BY a.booking_vol_binary") 

#checking click counts by publishers
comparison2<-sqldf("select   a.`Publisher Name`,sum(a.Clicks) as Click_Count
                   from air_france_copy a
                   group by a.`Publisher Name`")


#Average clicks for each booking by publishers
sqldf("SELECT `Publisher Name`, sum(Clicks), sum(`Total Volume of Bookings`), sum(Clicks)/sum(`Total Volume of Bookings`) as `AVG Clicks for each booking`
      FROM air_france_copy
      GROUP BY `Publisher Name`
      order by `AVG Clicks for each booking` DESC
      ")

sqldf("SELECT COUNT(`Total Volume of Bookings`), `Publisher Name`
FROM air_france_copy
      WHERE Campaign LIKE 'Geo%'")

##where is bidding strategy missing?
  #checking na_subset to get count of publishers
sqldf("select count(a.`Publisher Name`), a.`Publisher Name`
      from na_subset a
      group by a.`Publisher Name`
      ")

#creating new bid strategy variable to clean it
unique(air_france_copy$`Bid Strategy`)
air_france_copy$Bid_S <- air_france_copy$`Bid Strategy`
air_france_copy$Bid_S<-gsub("Postiion 1-4 Bid Strategy","Position 1-4 Bid Strategy",air_france_copy$Bid_S) #######changing the values to 0 and 1
air_france_copy$Bid_S<-gsub("Position 1 -2 Target","Position 1-2 Target",air_france_copy$Bid_S)
unique(air_france_copy$Bid_S)

sqldf(" select  a.Bid_S, a.Clicks
      from air_france_copy a
      group by a.Bid_S")

##Match type count
sqldf("SELECT count(`Match Type`),`Match Type`
                 FROM air_france_copy
                 group by `Match Type`
                 ")





#converting char columns to numeric
air_france_copy$Campaign_bin <- as.numeric(as.factor(air_france_copy$Campaign))
air_france_copy$`Match_Type_bin` <- as.numeric(as.factor(air_france_copy$`Match Type`))
air_france_copy$Status_bin <- as.numeric(as.factor(air_france_copy$Status))
air_france_copy$Publisher_Name_bin <- as.numeric(as.factor(air_france_copy$`Publisher Name`))

#########################DATA ANALYSIS#############################
#Regression analysis
modellm1 <- lm(Clicks ~ `Impressions` + `Click Charges`,data=air_france_copy)
summary(modellm1)
modellm2 <- lm(`Clicks` ~ `Click Charges` + Amount + `Impressions` ,data=air_france_copy)
summary(modellm2)
modellm5 <- glm(booking_vol_binary ~ `Click Charges` ,data=air_france_copy,family="binomial")
summary(modellm5)

#########################DATA OPTIMIZING PROCESS#############################
  #creating subsets of all publishers
Ovrtr_US_sub <- air_france_copy[which(air_france_copy$`Publisher Name`=="Overture - US"),]
Ovrtr_gl_sub <- air_france_copy[which(air_france_copy$`Publisher Name`=="Overture - Global"),]
Google_US_sub <- air_france_copy[which(air_france_copy$`Publisher Name`=="Google - US"),]
Google_gl_sub <- air_france_copy[which(air_france_copy$`Publisher Name`=="Google - Global"),]
MSN_US_sub <- air_france_copy[which(air_france_copy$`Publisher Name`=="MSN - US"),]
MSN_gl_sub <- air_france_copy[which(air_france_copy$`Publisher Name`=="MSN - Global"),]
Yahoo_US_sub <- air_france_copy[which(air_france_copy$`Publisher Name`=="Yahoo - US"),]
  
#process 1: using ROA Means
a <- mean(Ovrtr_US_sub$ROA) #calculating means
b <- mean(Ovrtr_gl_sub$ROA)
Google_US_sub$ROA[is.infinite(Google_US_sub$ROA)]<-NA
c <- mean(Yahoo_US_sub$ROA)
d <- mean(Google_US_sub$ROA, na.rm=TRUE)
e <- mean(Google_gl_sub$ROA)
f <- mean(MSN_gl_sub$ROA)
g <- mean(MSN_US_sub$ROA)

Means <- c(a,b,c,d,e,f,g) #creating vector using means
Publishers <- c(unique(air_france_copy$`Publisher Name`)) #creating vector for publisher names
ROA_Means <- data.frame(Publishers,Means) #creating dataframe for 
plot1 <- plot_ly(data=ROA_Means,x=~Publishers, y=~Means, type="bar", color = Publishers)%>%
  layout(title="ROA Means by Publishers", yaxis = list(title = "ROA Means"))
print(plot1) #plotting


air_france_copy$ROA[is.infinite(air_france_copy$ROA)]<-NA #changing inf value to null
mean(air_france_copy$ROA, na.rm=TRUE)
#creating subset for purchase observations
purchase_sub <- air_france_copy[which(air_france_copy$booking_vol_binary==1),]
mean(purchase_sub$ROA, na.rm=TRUE)

######################################################
#other plots

plot1 <- plot_ly(data=ROA_Means,x=~Publishers, y=~Means, type="bar", color = Publishers)%>%
  layout(title="ROA Means by Publishers", yaxis = list(title = "ROA Means"))
print(plot1)

plot2 <- plot_ly(data= air_france_copy, y= ~`Clicks`, x = ~ROA, color=~`Publisher Name`)%>%
  layout(xaxis=list(title="Return on Advertising"), yaxis=list(title="Click Count"))
print(plot2) #extreme outliers yahoo and overture

plot3 <- plot_ly(data=air_france_copy, x=~`Click Charges`, y = ~ROA, color = ~`Bid_S`)
print(plot3)#2 extreme outliers for one bidding strategy


plot4 <- ggplot(data=air_france_copy,aes(air_france_copy$`Avg. Cost per Click`,ROA,color=`Publisher Name`))+
  geom_point()
print(plot4)

plot5 <- plot_ly(data=air_france_copy, x=~`Match Type`, y = ~ROA, color = ~Bid_S)
print(plot5)


Clicks_sum <- sum(air_france_copy$Clicks)

plot6 <- plot_ly(data=air_france_copy, x=~`Match Type`, y = ~Clicks_sum, color = ~`Bid Strategy`)
print(plot6)


#Total number of clicks for each match type
comparison1<-sqldf("select  `Match Type`,sum(Clicks) as `Total number of Clicks`
                  from air_france_copy
                  group by `Match Type`
                  order by `Match Type`")

plot7 <- plot_ly(data=comparison1, x=~`Match Type`, y =~ `Total number of Clicks`, alpha=0.8, color = ~`Match Type`)%>%
  layout(title= "Total Clicks by Match Type", yaxis = list(title = "Total number of clicks"),xaxis=list(title="Match Type"))
print(plot7)

#Total ROA for each match type
comparison2<-sqldf("select `Match Type` ,sum(ROA) as Total_ROA
                  from air_france_copy
                  group by `Match Type`
                  order by `Match Type`")

plot8 <- plot_ly(data=comparison2, x=~`Match Type`, y =~ Total_ROA, alpha=0.8, color =~ `Match Type`)%>%
  layout(title= "ROA by Match Type", yaxis = list(title = "Total ROA"),xaxis=list(title="Match Type"))
print(plot8)



plot9 <- plot_ly(data=air_france_copy, x = ~Bid_S, y = ~ROA, type = 'scatter', mode = 'markers',
            marker = list( opacity = 15, color = 'rgb(255, 65, 54)'))
print(plot9) #outliers


plot10 <- plot_ly(data=air_france_copy, x = ~Bid_S, y = ~Clicks, type = 'scatter', mode = 'markers',
        marker = list( size= 15,opacity = 15, color = 'rgb(255, 65, 54)')) 
print(plot10) #outliers


plot11<- plot_ly(data=air_france_copy,x=~ROA,y=~`Total Volume of Bookings` ,color=~Bid_S,marker = list(size = 20,width = 2))
print(plot1)

plot_ly(y= air_france_copy$Amount,x=air_france_copy$`Publisher Name`, type="scatter", color=air_france_copy$`Publisher Name`)%>%
  add_trace

############# Plots: Pie Charts
#Profit sum per Search Engine
yahoo_pro <- sum(Yahoo_US_sub$Profit)#### yahoo 635obs Profit 836k 1.32
gglobal_pro <- sum(Google_gl_sub$Profit)##### google_global 393obs Profit 836k 2.13
gusa_pro <- sum(Google_US_sub$Profit)##### google_us 2071obs Profit 1391k 0.67
msnglobal_pro <- sum(MSN_gl_sub$Profit)#### msn_global 99obs Profit 133k 1.34
msnusa_pro <- sum(MSN_US_sub$Profit)#### msn_us 98obs Profit 165k 1.68
overglobal_pro <- sum(Ovrtr_gl_sub $Profit)#### over_global 553obs Profit 365k 0.66
overusa_pro <- sum(Ovrtr_US_sub$Profit)#### over_us 661obs Profit 205k 0.31
af_pro <- sum(air_france_copy$Profit)

#Percentage of Profit per Search Engine
ya_p <- yahoo_pro /af_pro
gg_p <- gglobal_pro/af_pro
gu_p <- gusa_pro/af_pro
mg_p <- msnglobal_pro/af_pro
mu_p <- msnusa_pro/af_pro
og_p <- overglobal_pro/af_pro
ou_p <- overusa_pro/af_pro

#Creating new data frames containing all percentages
Profit_pop <- c(ya_p,gg_p,gu_p,og_p,ou_p,mg_p,mu_p)
pop <- as.data.frame(Profit_pop)
pop$name <- c("yahoo-us","google-global","google-us","overture-global","overture-us","msn-global","msn-us")
invest_pop <- c(635/4510,393/4510,2071/4510,553/4510,661/4510,99/4510,98/4510)
pip <- as.data.frame(invest_pop)
pip$name <- c("yahoo-us","google-global","google-us","overture-global","overture-us","msn-global","msn-us")

#calculating ROA
ya_ROA <- sum(Yahoo_US_sub$ROA)
gg_ROA <- sum(Google_gl_sub$ROA)
gu_ROA <- sum(Google_US_sub$ROA, na.rm = TRUE)
mg_ROA <- sum(MSN_gl_sub$ROA)
mu_ROA <- sum(MSN_US_sub$ROA)
og_ROA <- sum(Ovrtr_gl_sub$ROA)
ou_ROA <- sum(Ovrtr_US_sub$ROA)
ap_ROA <- sum(air_france_copy$ROA, na.rm = TRUE)

#ROA percentage
ya_ROA_p <- ya_ROA/ap_ROA
gg_ROA_p <- gg_ROA/ap_ROA
gu_ROA_p <- gu_ROA/ap_ROA
mg_ROA_p <- mg_ROA/ap_ROA
mu_ROA_p <- mu_ROA/ap_ROA
og_ROA_p <- og_ROA/ap_ROA
ou_ROA_p <- ou_ROA/ap_ROA
pop$invest_pop <- c(635/4510,393/4510,2071/4510,553/4510,661/4510,99/4510,98/4510)
pop$name <- c("yahoo-us","google-global","google-us","overture-global","overture-us","msn-global","msn-us")
pop$ROA_pop <- c(ya_ROA_p,gg_ROA_p,gu_ROA_p,og_ROA_p,ou_ROA_p,mg_ROA_p,mu_ROA_p)

#Publishers percentage
plot12 <- plot_ly(data = pop, labels = ~name, values = ~invest_pop, type = 'pie',
                  textposition = 'inside',
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',
                  text = ~paste( invest_pop, '%'),
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 1)),
                  showlegend = FALSE) %>%
  layout(title = 'Percentage of Publishers',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
print(plot12)

#ROA pie chart
plot13 <- plot_ly(data = pop, labels = ~name, values = ~ROA_pop, type = 'pie',
                  textposition = 'inside',
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',
                  text = ~paste( ROA_pop, '%'),
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 1)),
                  showlegend = FALSE) %>%
  layout(title = 'ROA per Search Engine',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
print(plot13)

#Profit pie chart
plot14 <- plot_ly(data = pop, labels = ~name, values = ~Profit_pop, type = 'pie',
                  textposition = 'inside',
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',
                  text = ~paste( Profit_pop, '%'),
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 1)),
                  showlegend = FALSE) %>%
  layout(title = 'Profit per Search Engine',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
print(plot14)

##############################TEXT ANALYTICS#################################
####frequent words
text <- air_france_copy$Keyword #saving the keyword column in a vector

docs1 <- Corpus(VectorSource(text)) #Loading the text as a corpus
docs1 <- tm_map(docs1, content_transformer(tolower)) #Converting text to lower case 
docs1 <- tm_map(docs1, removePunctuation)#Removing punctuations
docs1 <- tm_map(docs1, stripWhitespace)#Eliminate extra white spaces

tdm1 <- TermDocumentMatrix(docs1) #creating term-document matrix
matrx1 <- as.matrix(tdm1) 
v1 <- sort(rowSums(matrx1),decreasing=TRUE) #sorting in decreasing order
d1 <- data.frame(word = names(v1),freq=v1)

pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

wordcloud(d1$word,d1$freq, 
          random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=pal)

####frequent words where bookings were made
text_sub <- purchase_sub$Keyword #saving the keyword column in a vector

docs2 <- Corpus(VectorSource(text_sub)) #Loading the text as a corpus
  
#Applying transformations
docs2 <- tm_map(docs2, content_transformer(tolower)) #Converting text to lower case 
docs2 <- tm_map(docs2, removePunctuation)#Removing punctuations
docs2 <- tm_map(docs2, stripWhitespace)#Eliminating extra white spaces

tdm2 <- TermDocumentMatrix(docs2) #creating term-document matrix
matrx2 <- as.matrix(tdm2) 
v2 <- sort(rowSums(matrx2),decreasing=TRUE) #sorting in decreasing order
d2 <- data.frame(word = names(v2),freq=v2)

wordcloud(d2$word,d2$freq, 
          random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=pal)

##################################################################################
#End