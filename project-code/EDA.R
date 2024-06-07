library(ggplot2)
library(lubridate)
library(tidyverse)
library(quantmod)
library(tidyquant)
library(plotly)

df_holi=read.csv('./data_cleaned/Holiday Combined.csv')

df_holi$year=sapply(df_holi$year,function(x){as.factor(x)})

ggplot(df_holi) +
  geom_col(aes(x=year,y=Sales.in.billion.in.USD.),fill='blue2')+
  geom_line(aes(year,Ecommerce.as.Percent.of.Total.Sales...*10),group=1,colour='green3',size=2)+
  geom_point(aes(year,Ecommerce.as.Percent.of.Total.Sales...*10),colour='orange2',size=2)+
  scale_y_continuous('Sales(USD in billion)',sec.axis = sec_axis(~.*0.1,name='Ecommerce as percentage',
                                                                 labels = function(b) { paste0(b,"%")}))+
  theme(
    axis.title.y = element_text(color = "blue2"),
    axis.title.y.right = element_text(color = "orange3")
  )+
  labs(title = 'Holiday sales in United State',
       y='Sales(USD in billion)',
       x='year')


png(file = "./images/fig_created/R1.png")
g=ggplot(df_holi) +
  geom_col(aes(year,Sales.in.billion.in.USD.),fill='blue')+
  geom_line(aes(year,Ecommerce.as.Percent.of.Total.Sales...),colour='red',size=2)+
  geom_point(aes(year,Ecommerce.as.Percent.of.Total.Sales...),colour='black',size=2)+
  labs(title = 'Holiday sales in United State',
       y='Sales(USD in billion)',
       x='year')

ggplotly(g)

dev.off()














