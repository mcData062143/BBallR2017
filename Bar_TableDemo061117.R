#What countries did players come from, besides USA??  Look at 1900 to 1940, then
#1950 to 1999

#Carve out the relevant statistics from the Master file for 1900 to 1940
mast<-Master %>% select(playerID:birthCity)
mast00_40<-mast %>% filter(birthYear>=1900 & birthYear<1941 & birthCountry!="USA")

ggplot(data=mast00_40, aes(x=birthCountry))+
  geom_bar()+
  coord_flip()+
  labs(title="Player origens 1900 - 1940",  x="Country")

#One way to make a table
grp00_40<-mast00_40 %>% group_by(birthCountry) %>% summarize(count=n())
View(grp00_40)

#Another way to make a table
table(mast00_40$birthCountry)

print(grp00_40,n=nrow(grp00_40))

#label the 00-40 period as oldtime 
Oldd<-mutate(mast00_40, Period="Oldtime")

#Create a file for the period 1950 to 1999, then label it
mast50_99<-mast %>% filter(birthYear>=1950 & birthYear<1999 & birthCountry!="USA")
Modd<-mutate(mast50_99, Period="Modern")

# this plot is messy
ggplot(data=mast50_99, aes(x=birthCountry))+
  geom_bar()+
  coord_flip()+
  labs(title="Player origens 1950 - 1999",  x="Country")

table(mast50_99$birthCountry)

#Combine the 2 files
Combo<-bind_rows(Oldd, Modd)

#This plot still messy
ggplot(data=Combo, aes(x=birthCountry, fill=Period))+
  geom_bar()+
  coord_flip()+
  labs(title="Player origens 1900 - 1999",  x="Country")

#however, can group the countries and make a table.
grp00_99<-Combo %>% group_by(birthCountry, Period) %>% summarize(count=n())
View(grp00_99)

#This gives the best table
table(Combo$birthCountry, Combo$Period)

print(grp00_99,n=nrow(grp00_99))

