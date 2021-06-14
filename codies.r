

Sys.setenv(LANG = "en")
#Read csv file
my_pile_of_stuff<-read.csv("Birdies_exp.csv") #("Zebra_finch_exp.csv")
my_pile_of_stuff<-as.data.frame(my_pile_of_stuff)
#Territory is a variable that has 2 components, a letter and a number representing a grid (row and column)
#We split the letter part, and convert it to a number coordinate, it helps us calculate distances.
tmp<-str_split_fixed(my_pile_of_stuff$territory, "",n=2)
my_pile_of_stuff$x_pos<-tmp[,1]
#The way that data was captured, some fields are empty and we convert them to NA
my_pile_of_stuff$x_pos[my_pile_of_stuff$x_pos==""]<-NA

#converts the number part into a factor
my_pile_of_stuff$x_pos<-as.factor(my_pile_of_stuff$x_pos)
my_pile_of_stuff$x_pos<-droplevels(my_pile_of_stuff$x_pos)
my_pile_of_stuff$x_pos<-as.numeric(my_pile_of_stuff$x_pos)

#makes the leters into a factor, and converts them to a numeric variable
my_pile_of_stuff$y_pos<-as.factor(tmp[,2])
my_pile_of_stuff$y_pos[my_pile_of_stuff$y_pos==""]<-NA
my_pile_of_stuff$y_pos<-as.factor(my_pile_of_stuff$y_pos)
my_pile_of_stuff$y_pos<-droplevels(my_pile_of_stuff$y_pos)
my_pile_of_stuff$y_pos<-as.numeric(my_pile_of_stuff$y_pos)

#Our setup started numbering the grid from top to bottom, we transform the numbers so visualizations match the experimental set up
my_pile_of_stuff$y_pos<-my_pile_of_stuff$y_pos *-1+6




#heatmap_weather<-range %>% count(identity, x_pos,y_pos,weather, sort = TRUE)
#heatmap_exp<-range %>% count(identity, x_pos,y_pos,day, sort = TRUE)

#ggplot(heatmap_exp,aes(x=x_pos,y=y_pos,fill=n))+geom_tile()+facet_grid(day ~ identity)+scale_fill_viridis(discrete=FALSE) + theme_ipsum()
#ggplot(heatmap_weather,aes(x=x_pos,y=y_pos,fill=n))+geom_tile()+facet_grid(weather ~ identity)+scale_fill_viridis(discrete=FALSE) + theme_ipsum()

#We create a function that takes as an input a variable, and creates a heatmap of the counts of the position of each bird across the grid normalized
#by the total amount of recorded events.
plot_heat<-function(df,cond){
  print(cond)
  hm<-df %>% count(identity, x_pos,y_pos,df[cond], sort = TRUE)
  #print(hm)
  nd<-df %>% count(identity,df[cond], sort = TRUE)
  for(x in 1:dim(nd)[1]){
    hm[hm$identity==nd[x,"identity"] & hm[cond]==nd[x,cond],"n"] <- hm[hm$identity==nd[x,"identity"] & hm[cond]==nd[x,cond],"n"]/nd[x,"n"]
  }
  p<-ggplot(hm,aes(x=x_pos,y=y_pos,fill=n))+geom_tile()+facet_grid(as.formula(paste("identity","~", cond)))+scale_fill_viridis(discrete=FALSE) + theme_bw()
  print(p)
  return(hm)
}
#pdf("Birdies/awesome_plot1.pdf",height=10.5)
tmp3<-plot_heat(range,"day") #enter whatever variable
#dev.off()
#for(x in 1:dim(tmp2)[1]){
#  print(x)
#}
#for interactions
#modifying table to give instead of treatment closeness to food for ANOVA
birdies<-levels(factor(my_pile_of_stuff$identity))
nests<-c("nest_b","nest_b","lamp","nest_c","nest_c","lamp")
my_pile_of_stuff$nest<-factor(mapvalues(my_pile_of_stuff$identity,birdies,nests))

my_pile_of_stuff['closeness']<-"far"
my_pile_of_stuff[my_pile_of_stuff$treatment == my_pile_of_stuff$nest,"closeness"]<-"close"
my_pile_of_stuff[my_pile_of_stuff$treatment=="lamp" & my_pile_of_stuff$nest=="nest_b","closeness"]<-"close"
my_pile_of_stuff[my_pile_of_stuff$treatment =="nest_b" & my_pile_of_stuff$nest=="lamp","closeness"]<-"close"
distinct(my_pile_of_stuff[,c(5,6,14,15)])

my_pile_of_stuff$exp_id<-paste0(my_pile_of_stuff$day,"-",my_pile_of_stuff$session)
#Our data consists of 2 sets of observations, movement and interactions all in a single table, first we generate the movement table
tmp<-my_pile_of_stuff[,c(-3,-10,-11)] #-12
range<-tmp[complete.cases(tmp),]



condition<- !is.na(my_pile_of_stuff$recipient)
interaction<-my_pile_of_stuff[condition,]
interaction_count<-interaction %>% dplyr::count(identity,closeness,exp_id, sort = TRUE)
group_by(interaction_count, closeness) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(n, na.rm = TRUE),
    sd = sd(n, na.rm = TRUE)
  )

res.aov <- aov(n ~ closeness, data = interaction_count)
# Summary of the analysis
summary(res.aov)


interaction_count_tr<-interaction %>% dplyr::count(identity,treatment, sort = TRUE)
group_by(interaction_count_tr, treatment) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(n, na.rm = TRUE),
    sd = sd(n, na.rm = TRUE)
  )

res.aov <- aov(n ~ treatment, data = interaction_count_tr)
# Summary of the analysis
summary(res.aov)

