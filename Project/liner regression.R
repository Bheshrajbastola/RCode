options(scipen=999)

HousePrices = prices %>%
  filter(Year=="2020") %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(Price=mean(Price))

  BroardbandSpeeds = speeds %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  summarise(AverageDownload=mean(AverageDownload))
  lm_res = HousePrices %>% left_join(BroardbandSpeeds,by="Town")
  model = lm(data= lm_res, Price~AverageDownload)
  summary(model)
 
  color= c("LANCASHIRE" = "purple", "LEICESTERSHIRE" = "green")
 
 ggplot(lm_res,aes(x=AverageDownload,y=Price)) +
   geom_point(data = filter(lm_res,County.x=="LANCASHIRE"),aes(color="LANCASHIRE"))+
   geom_point(data = filter(lm_res,County.x=="LEICESTERSHIRE"), aes(color="LEICESTERSHIRE")) +
   geom_smooth(method=lm,se=FALSE,color="orange")+
   labs(x="Download Speed (Mega bit per second)",y="Price",title="House Prices vs
        Broadband Speed",color="County")
 
 
 
 
 
 
 
 
 
 HousePrices = prices %>%
   filter(Year=="2020") %>%
   left_join(Towns,by="shortPostcode") %>%  
   group_by(Town,County) %>%
   summarise(Price=mean(Price))
 Drugs = crime %>%
   left_join(Towns,by="shortPostcode") %>%
   group_by(Town,County) %>%
   filter(CrimeType=="Drugs") %>% 
   na.omit()
 
 lm_res1 = HousePrices %>% left_join(Drugs ,by="Town") %>% 
   na.omit()
 model1 = lm(data= lm_res1, Price~n)
 summary(model1)
 color= c("LANCASHIRE" = "purple", "LEICESTERSHIRE" = "green")
 
 ggplot(lm_res1,aes(x=n,y=Price)) +
   geom_point(data = filter(lm_res1,County.x=="LANCASHIRE"),aes(color="LANCASHIRE"))+
   geom_point(data = filter(lm_res1,County.x=="LEICESTERSHIRE"), aes(color="LEICESTERSHIRE")) +
   geom_smooth(method=lm,se=FALSE,color="orange")+
   labs(x="count",y="Price (?)",title="House Prices vs Drug offence Rate",color="County")
 
 
 
 
 
 
 
 school_lm= schools %>%
   left_join(Towns,by=c("shortPostCode"="shortPostcode") )%>%  
   group_by(Town,County) %>%
   
   summarise(score=mean(Attainment8Score)) 
 Drugs = crime %>%
   left_join(Towns,by="shortPostcode") %>%
   group_by(Town,County) %>%
   filter(CrimeType=="Drugs") %>% 
   na.omit()
 
 lm_res2 = school_lm %>% left_join(Drugs ,by="Town") %>% 
   na.omit()
 model2 = lm(data= lm_res2, n~score)
 summary(model2)
 color= c("LANCASHIRE" = "purple", "LEICESTERSHIRE" = "green")
 
 ggplot(lm_res2,aes(x=n,y=score)) +
   geom_point(data = filter(lm_res2,County.x=="LANCASHIRE"),aes(color="LANCASHIRE"))+
   geom_point(data = filter(lm_res2,County.x=="LEICESTERSHIRE"), aes(color="LEICESTERSHIRE")) +
   geom_smooth(method=lm,se=FALSE,color="yellow")+
   labs(x="count",y="score",title="Attainment Score vs Drug Offence Rate",color="County")
 
 
 
 
 
 
 
 
 BroardbandSpeeds = speeds %>%
   left_join(Towns,by="shortPostcode") %>%
   group_by(Town,County) %>%
   summarise(AverageDownload=mean(AverageDownload))
 Drugs = crime %>%
   left_join(Towns,by="shortPostcode") %>%
   group_by(Town,County) %>%
   filter(CrimeType=="Drugs") %>% 
   na.omit()
 
 lm_res3 = BroardbandSpeeds %>% left_join(Drugs ,by="Town") %>% 
   na.omit()
 model3 = lm(data= lm_res3, AverageDownload~n)
 summary(model3)
 color= c("LANCASHIRE" = "purple", "LEICESTERSHIRE" = "green")
 
 ggplot(lm_res3,aes(x=n,y=AverageDownload)) +
   geom_point(data = filter(lm_res3,County.x=="LANCASHIRE"),aes(color="LANCASHIRE"))+
   geom_point(data = filter(lm_res3,County.x=="LEICESTERSHIRE"), aes(color="LEICESTERSHIRE")) +
   geom_smooth(method=lm,se=FALSE,color="green")+
   labs(x="count",y="Average Download",title="Average Download vs Drug Count",color="County")
 
 
 BroardbandSpeeds = speeds %>%
   left_join(Towns,by="shortPostcode") %>%
   group_by(Town,County) %>%
   summarise(AverageDownload=mean(AverageDownload))
 school_lm= schools %>%
   left_join(Towns,by=c("shortPostCode"="shortPostcode") )%>%  
   group_by(Town,County) %>%
   
   summarise(score=mean(Attainment8Score)) 
 HousePrices = prices %>%
   filter(Year=="2020") %>%
   left_join(Towns,by="shortPostcode") %>%  
   group_by(Town,County) %>%
   summarise(Price=mean(Price))
 lm_res4 = BroardbandSpeeds %>% left_join(school_lm,by="County") %>% 
   na.omit()
 model4 = lm(data= lm_res4, AverageDownload~score)
 summary(model4)
 color= c("LANCASHIRE" = "purple", "LEICESTERSHIRE" = "green")
 
 ggplot(lm_res4,aes(x=score,y=AverageDownload)) +
   geom_point(data = filter(lm_res4,County==" LANCASHIRE"),aes(color=" LANCASHIRE"))+
   geom_point(data = filter(lm_res4,County=="LEICESTERSHIRE"), aes(color="LEICESTERSHIRE")) +
   geom_smooth(method=lm,se=FALSE,color="pink")+
   labs(x="Score",y="Average Download",title="Average Download vs School",color="County")
 
 
 
 
 school_lm= schools %>%
   left_join(Towns,by=c("shortPostCode"="shortPostcode") )%>%  
   group_by(Town,County) %>%
   
   summarise(score=mean(Attainment8Score)) 
 
 HousePrices = prices %>%
   filter(Year=="2020") %>%
   left_join(Towns,by="shortPostcode") %>%  
   group_by(Town,County) %>%
   summarise(Price=mean(Price))
 lm_res4 = HousePrices %>% left_join(school_lm,by="County") %>% 
   na.omit()
 model4 = lm(data= lm_res4, Price~score)
 summary(model4)
 color= c("LANCASHIRE" = "purple", "LEICESTERSHIRE" = "green")
 

 ggplot(lm_res4,aes(x=score,y=Price)) +
   geom_point(data = filter(lm_res4,County==" LANCASHIRE"),aes(color="LANCASHIRE"))+
   geom_point(data = filter(lm_res4,County=="LEICESTERSHIRE"), aes(color="LEICESTERSHIRE")) +
   geom_smooth(method=lm,se=FALSE,color="gold")+
   labs(x="Score",y="Average House Price",title="Average House Price vs Attainment at School",color="County")
 
 
 
 
 