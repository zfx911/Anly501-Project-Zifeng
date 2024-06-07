library(tidyverse)

# 1 and 2
df_9=read.csv('data/sta9.csv')
df_25=read.csv('data/sta25.csv')

# 1
head(df_9)
df_9=df_9%>% filter(!row_number()%in% c(1,2))
names(df_9)[1]<-'year'
names(df_9)[2]<-'Sales(in billion in USD)'

df_9[6,1]=gsub('[[:punct:]]','',df_9[6,1])
df_9$year=sapply(df_9$year,function(x){as.double(x)})
df_9$`Sales(in billion in USD)`=sapply(df_9$`Sales(in billion in USD)`,function(x){as.double(x)})

write.csv(df_9,'./data_cleaned/Holiday Ecommerce sales.csv',row.names=FALSE)

# 2
head(df_25)
df_25=df_25%>% filter(!row_number()%in% c(1,2))
df_25=subset(df_25, select = -c(3))
names(df_25)[1]<-'year'
names(df_25)[2]<-'Ecommerce as Percent of Total Sales(%)'

df_25[6,1]=gsub('[[:punct:]]','',df_25[6,1])
df_25$year=sapply(df_25$year,function(x){as.double(x)})
df_25$`Ecommerce as Percent of Total Sales(%)`=sapply(df_25$`Ecommerce as Percent of Total Sales(%)`,function(x){as.double(x)})

write.csv(df_25,'./data_cleaned/Holiday Ecommerce as percentage.csv',row.names=FALSE)

#########################
# 3.
df_9_25=df_9%>%full_join(df_25,by='year')
df_9_25

write.csv(df_9_25,'./data_cleaned/Holiday Combined.csv',row.names=FALSE)




