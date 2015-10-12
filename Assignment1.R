##############
#Assignment 1#
##############

#Read in data
library("readr")
df = read_csv("https://raw.githubusercontent.com/MuseumofModernArt/collection/master/Artworks.csv")
#write.csv(df, file ="C:/Users/Tr??ndur/Dropbox/Tr??ndur/Polit/Kandidat/E15/Social data science/Assignments/Assignment 1/data.csv")
#df = read.csv("C:/Users/Tr??ndur/Dropbox/Tr??ndur/Polit/Kandidat/E15/Social data science/Assignments/Assignment 1/data.csv",header=TRUE)

class(df$DateAcquired)
range(df$DateAcquired,na.rm=TRUE)

#Question 1
#Create a new dataframe of the stock of paintings at MOMA for each month in 
#the year.
library("dplyr")
library("lubridate")
df.stock <- df %>%
  filter(Classification=="Painting") %>%
  arrange(DateAcquired) %>%
  group_by(DateAcquired) %>%
  summarise(count=n()) %>%
  mutate(stock1=cumsum(count))

#Question 2
#Use ggplot2 and your new data frame to plot the the stock of paintings on 
# the y-axis and the date on the x-axis.
library("ggplot2")
p = ggplot(df.stock, aes(x=as.Date(DateAcquired), y=stock1, na.rm=TRUE))
p = p + geom_point(color="red",na.rm=TRUE) +
  geom_line(color="blue")
p = p + ggtitle("Stock of paintings at MoMA") + ylab("Stock of paintings") + xlab("Date")
p
#-what kind of geom do you think is appropriate? why
#-color the geom you have chosen red
#-add a title and custom axis labels


#Question 3
#Create the same plot but this time the color should reflect the stock 
#of paintings for curator approved and non-curator approved paintings, 
#respectively
df.stock1 <- df %>%
  arrange(DateAcquired) %>%
  filter(CuratorApproved=="N") %>%
  filter(Classification=="Painting") %>%
  group_by(DateAcquired) %>%
  summarise(count1=n())

df.stock2=left_join(df.stock,df.stock1) %>%
  mutate(count1 = ifelse(!is.na(count1),count1,0)) %>%
  mutate(stock2=cumsum(count1)) %>%
  mutate(stock=cumsum(count-count1))

df.stock2 <- df.stock2[-c(447),]

p = ggplot(df.stock2, aes(x=DateAcquired, y=stock1))
p = p + geom_area(fill="red",na.rm=TRUE)
p = p + geom_area(aes(x=DateAcquired,y=stock2),fill="blue",colour="blue")
p = p + ggtitle("Stock of paintings at\nMuseum of Modern Art") + ylab("Stock of paintings") + xlab("Date")
p

#Question 4
#Create a new dataframe of the stock of paintings grouped by what department 
#the painting belongs to.
df.department <- df %>%
  filter(Classification=="Painting") %>%
  filter(!is.na(DateAcquired)) %>%
  group_by(Department) %>%
  arrange(DateAcquired) %>%
  group_by(Department, DateAcquired) %>%
  arrange(DateAcquired) %>%
  summarise(count=n()) %>%
  mutate(stock1=cumsum(count))  

#Question 5
#Plot this data frame using ggplot2. Which department has had the highest 
#increase in their stock of paintings?
p = ggplot(df.department, aes(x=as.Date(DateAcquired), y=stock1, na.rm=TRUE))
p = p + geom_point(fill="red",na.rm=TRUE) + facet_wrap(~ Department, scales="free")
p = p + geom_line(color="blue")
p + ggtitle("Stock of paintings at MoMA") + ylab("Stock of paintings") + xlab("Date")


#Question 6
#Write a piece of code that counts the number of paintings by each artist in 
#the dataset. List the 10 painters with the highest number of paintings in 
#MOMA's collection.
df.artist <- df %>%
  filter(Classification=="Painting") %>%
  group_by(Artist) %>%
  arrange(Artist) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

head(df.artist, 10)

#Question 7
#The variable ArtistBio lists the birth place of each painter. Use this 
#information to create a world map where each country is colored according 
#to the stock of paintings in MOMA's collection.
library(maps)
library(stringr)
library(data.table)

df.artistbio <- df %>%
  filter(Classification=="Painting") %>%
  mutate(artborn1 = str_extract(ArtistBio,"[:alpha:]{1,15}")) %>%
  mutate(artborn2 = gsub("^.*?,","",ArtistBio)) %>%
  mutate(artborn2 = gsub("????????","",artborn2)) %>%
  mutate(artborn2 = gsub("born","",artborn2)) %>%
  mutate(artborn2 = gsub("The","",artborn2)) %>%
  mutate(artborn2 = str_extract(artborn2,"[:alpha:]{1,15}")) %>%
  mutate(artborn = artborn1) %>%
  mutate(region = ifelse(!is.na(artborn2), artborn2,artborn))%>%
  left_join(df.artist) %>%
  select(Artist,region,count)

df.nationality = read.csv("C:/Users/Tr??ndur/Dropbox/Tr??ndur/Polit/Kandidat/E15/Social data science/Assignments/Assignment 1/nationality.csv", stringsAsFactors = FALSE, header=TRUE) %>%
  select(V1, region=V2) %>%
  mutate(region = str_trim(region))

df.plot_art <- df.artistbio %>%
  left_join(df.nationality) %>%
  mutate(region = ifelse(is.na(V1),region,V1)) %>%
  unique() %>%
  arrange(region) %>%
  mutate(region=str_trim(region)) %>%
  group_by(region) %>%
  summarise(stock=sum(count)) %>%
  mutate(region = gsub("The United States","USA",region)) %>%
  mutate(region = gsub("Russia","USSR",region))
  
  
  
  
country <- map_data("world")
df.plot_art <- left_join(df.plot_art,country)

p = ggplot(df.plot_art, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = stock)) 
  expand_limits() 
  theme_minimal()
p

#######Do not to use#############
#df.artist <- df.artistbio %>%
#  group_by(Artist,DateAcquired) %>%
#  arrange(Artist,DateAcquired) %>%
#  summarise(count=n()) %>% 
#  mutate(stock = cumsum(count))
#
#df.artistbio <- left_join(df.artist,df.artistbio)
#
#df.artistbio <- df.artistbio %>%
#  mutate(forplot = str_extract(artborn,"[:alpha:]{1,3}")) %>%
#  arrange(Artist) %>%
#  group_by(Artist, DateAcquired)
#################################


#Question 8
#The Dimensions variable lists the dimensions of each painting. Use your data 
#manipulation skills to calculate the area of each painting (in cm's). Create a 
#data frame of the five largest and five smallest paintings in MOMA's collection.

df.dimension <- df %>%
  filter(Classification=="Painting") %>%
  mutate(size = str_extract(Dimensions, "([0.0-9.9]+ x [0.0-9.9]+ cm)")) %>%
  mutate(size1 = as.numeric(gsub("x [0.0-9.9]+ cm","", size))) %>%
  mutate(size2 = as.numeric(gsub("cm","",gsub("[0.0-9.9]+ x","", size)))) %>%
  mutate(area = size1*size2) %>%
  arrange(desc(area)) %>%
  select(Artist, area) %>%
  filter(!is.na(area))
head(df.dimension,5)
tail(df.dimension,5)
