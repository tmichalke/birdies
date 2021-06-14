Introduction
============

Our data is a collection of agonistic interactions and locations of
individual finches in a grid of 5x5. Measurements were collected in
multiple sessions. Each pair of finches nested in the following
locations. ![](birdies_files/figure-markdown_strict/nest-1.png)

Our analysis consists first on creating and cleaning the data, followed
by some visualizations and statistical tests.

Reading Data
------------

Our data is in a csv format, with the following columns

    my_pile_of_stuff<-read.csv("Birdies_exp.csv") #("Zebra_finch_exp.csv")
    my_pile_of_stuff<-as.data.frame(my_pile_of_stuff)
    #my_pile_of_stuff

This data needs to be formatted in the following ways:

-   Data needs to be separated between interactions and movement
    measurements
-   The variable territory denotes the location in a grid with a letter
    and a number. This needs to be split up and put in an X-Y numeric
    coordinate system.
-   The location of each birds nest is known, but wasnt added to the
    data. This is important because in the agonistic interactions, the
    *treatment* is where the food was places, and this corresponds to a
    nest location
-   During the agonistic interactions, we consider a treatment where the
    food was place, which overlapped with a pair of birds nest location,
    due to where the birds decided to nest, 2 nests were in close
    proximity, meaning that depending on the condition we relabelled
    treatment by proximity to the nest.
-   We also want to calculate the distance between an interaction and
    the proximity to the food

<!-- -->

    #We obtain the names of the birds, and then map these values to their corresponding nest
    birdies<-levels(factor(my_pile_of_stuff$identity))
    nests<-c("nest_b","nest_b","lamp","nest_c","nest_c","lamp")
    my_pile_of_stuff$nest<-factor(plyr::mapvalues(my_pile_of_stuff$identity,birdies,nests))
    my_pile_of_stuff$exp_id<-paste0(my_pile_of_stuff$day,"-",my_pile_of_stuff$session)

    #Depending on the treatment, the food location was either close or far from each nest, so we create a new variable to show this
    my_pile_of_stuff['closeness']<-"far"
    my_pile_of_stuff[my_pile_of_stuff$treatment == my_pile_of_stuff$nest,"closeness"]<-"close"
    my_pile_of_stuff[my_pile_of_stuff$treatment=="lamp" & my_pile_of_stuff$nest=="nest_b","closeness"]<-"close"
    my_pile_of_stuff[my_pile_of_stuff$treatment =="nest_b" & my_pile_of_stuff$nest=="lamp","closeness"]<-"close"

This part takes care of the movement table, as well as making plots with
the variable of interest

    #We select only rows where territory is measured (this is the measurements of where the finch was in a given time)
    movement<-my_pile_of_stuff[!is.na(my_pile_of_stuff$territory),]
    movement<-movement[,c(-10,-11)]
    tmp<-str_split_fixed(movement$territory, "",n=2)
    movement$x_pos<-tmp[,1]
    #The way that data was captured, some fields are empty and we convert them to NA
    movement$x_pos[movement$x_pos==""]<-NA
    #converts the number part into a factor
    movement$x_pos<-as.factor(movement$x_pos)
    movement$x_pos<-droplevels(movement$x_pos)
    movement$x_pos<-as.numeric(movement$x_pos)
    #makes the leters into a factor, and converts them to a numeric variable
    movement$y_pos<-as.factor(tmp[,2])
    movement$y_pos[movement$y_pos==""]<-NA
    movement$y_pos<-as.factor(movement$y_pos)
    movement$y_pos<-droplevels(movement$y_pos)
    movement$y_pos<-as.numeric(movement$y_pos)
    #Our setup started numbering the grid from top to bottom, we transform the numbers so visualizations match the experimental set up
    #Only taking into account the rows with a territory
    movement$y_pos<-movement$y_pos *-1+6
    range<-movement[complete.cases(movement$territory),]

    #We create a function that takes as an input a variable, and creates a heatmap of the counts of the position of each bird across the grid #normalized by the total amount of recorded events
    plot_heat<-function(df,cond){
      #print(cond)
      hm<-df %>% dplyr::count(identity, x_pos,y_pos,df[cond], sort = TRUE)
      #print(hm)
      nd<-df %>% dplyr::count(identity,df[cond], sort = TRUE)
      for(x in 1:dim(nd)[1]){
        hm[hm$identity==nd[x,"identity"] & hm[cond]==nd[x,cond],"n"] <- hm[hm$identity==nd[x,"identity"] & hm[cond]==nd[x,cond],"n"]/nd[x,"n"]
      }
      p<-ggplot(hm,aes(x=x_pos,y=y_pos,fill=n))+geom_tile()+facet_grid(as.formula(paste(cond,"~", "identity")))+scale_fill_viridis(discrete=FALSE) + theme_bw()+theme(plot.title.position = 'plot')+theme(legend.position = 'top')+guides(fill = guide_colorbar(title="Position counts",title.position = 'top', title.hjust = .5,
     barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines'))) +
        labs(title = paste0("Movement per ",cond), xlab= "x position",ylab= "y position")
      print(p)
      return(hm)
    }

We first visualize the positions of the finches depending on the day

    tmp3<-plot_heat(range,"day") #enter whatever variable

![](birdies_files/figure-markdown_strict/movement_plot-1.png)

We also look at the positions dependent on other variables

-   cycle
-   weather

<!-- -->

    tmp4<-plot_heat(range,"cycle") 

![](birdies_files/figure-markdown_strict/movement_plot2-1.png)

    tmp5<-plot_heat(range,"weather") 

![](birdies_files/figure-markdown_strict/movement_plot2-2.png)

    #dev.off()
    #for(x in 1:dim(tmp2)[1]){
    #  print(x)
    #}

This part takes care of the interaction table, as well as calculating
the distances

    interaction<-my_pile_of_stuff[,-9]
    interaction$treatment<-as.factor(interaction$treatment)

    interaction<-interaction[complete.cases(interaction$agonistic),]

    tmp<-str_split_fixed(interaction$agonistic, "",n=2)
    interaction$x_pos<-tmp[,1]
    #The way that data was captured, some fields are empty and we convert them to NA
    interaction$x_pos[interaction$x_pos==""]<-NA

    #converts the number part into a factor
    interaction$x_pos<-as.factor(interaction$x_pos)
    interaction$x_pos<-droplevels(interaction$x_pos)
    interaction$x_pos<-as.numeric(interaction$x_pos)

    #makes the leters into a factor, and converts them to a numeric variable
    interaction$y_pos<-as.factor(tmp[,2])
    interaction$y_pos[interaction$y_pos==""]<-NA
    interaction$y_pos<-as.factor(interaction$y_pos)
    interaction$y_pos<-droplevels(interaction$y_pos)
    interaction$y_pos<-as.numeric(interaction$y_pos)

    #Our setup started numbering the grid from top to bottom, we transform the numbers so visualizations match the experimental set up
    interaction$y_pos<-interaction$y_pos *-1+6

    #we create a data frame with the coordinates of the food (and therefore also the nest)
    nest_coord<-data.frame(matrix(nrow = 2,ncol=4))
    colnames(nest_coord)<-levels(interaction$treatment)
    nest_coord[1,1]<-3
    nest_coord[2,1]<-3
    nest_coord[1,2]<-2
    nest_coord[2,2]<-5
    nest_coord[1,3]<-1
    nest_coord[2,3]<-4
    nest_coord[1,4]<-1
    nest_coord[2,4]<-2

    interaction$distance_nest<-NA
    interaction$distance_food<-NA
    interaction$nest_recipient<-factor(plyr::mapvalues(interaction$recipient,birdies,nests))

    for(x in 1:nrow(interaction)){
      tmp<-c(interaction[x,c(13,14)])
      treat<-interaction[x,"treatment"]
      nest_coord[,treat]
      interaction[x,"distance_food"]<-dist(rbind(tmp,nest_coord[,treat]))
      
      treat<-interaction[x,"nest"]
      treat2<-interaction[x,"nest_recipient"]
      nest_coord[,treat]
      interaction[x,"distance_nest"]<-min(
        dist(rbind(tmp,nest_coord[,treat])),
        dist(rbind(tmp,nest_coord[,treat2])))
        
      
    }

Interaction plots
-----------------

    interaction_count_tr<-interaction %>% dplyr::count(identity,treatment,exp_id, sort = TRUE)
    interaction_count_cl<-interaction %>% dplyr::count(identity,closeness,exp_id, sort = TRUE)
    #boxplot for interaction per treatment per indiviual
    ggplot(interaction_count_tr, aes(x=treatment, y=n,colour=treatment))  +  #or distance_food
       geom_quasirandom()+theme_bw()+scale_color_viridis(discrete = T)+facet_wrap(. ~ identity)+theme(legend.position = 'top')+
      guides(color = guide_legend(title.position = 'top', title.hjust = .5, barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+ylab("Number of interactions") + labs(title="Interactions per treatment")

![](birdies_files/figure-markdown_strict/interaction_counr-1.png)

    #plot of interaction counts normalized over exp_id per individual per treatment
    interaction_count_cl$identity<-as.factor(interaction_count_cl$identity)
    levels(interaction_count_cl$exp_id)<-sort(c(levels(interaction_count_cl$exp_id),"5-1"))

    for (x in levels(interaction_count_cl$exp_id)){
      for(y in levels(interaction_count_cl$identity)){
        if(sum(interaction$identity==y & interaction$exp_id==x)==0){
          print(paste(x,y))
          tmp<-interaction_count_cl[interaction_count_cl$identity==y,][1,]
          tmp[,3]<-x
          tmp[,4]<-0
          interaction_count_cl<-rbind(interaction_count_cl,tmp)
        }
      }
    }

    ## [1] "5-1 Captain_Hook"

    ## Warning in `[<-.factor`(`*tmp*`, ri, value = structure(c("10-1", "1-1", :
    ## invalid factor level, NA generated

    ## [1] "5-1 Droopy"
    ## [1] "5-1 Einband"
    ## [1] "5-1 Gelbbauch"
    ## [1] "5-1 Gelbwange"
    ## [1] "5-1 Scruffy"

    #boxplot for interactions related to proximity of nest
    ggplot(interaction_count_cl, aes(x=closeness, y=n,fill=closeness)) + 
      ## add half-violin from {ggdist} package
      geom_boxplot(
        width = .22, 
        ## remove outliers
        outlier.color = NA ## `outlier.shape = NA` works as well
      ) +
      ## add dot plots from {ggdist} package
      ggdist::stat_dots(
        ## orientation to the left
        side = "left", 
        ## move geom to the left
        justification = 1.3, 
        ## adjust grouping (binning) of observations 
        binwidth = .25
      )+
      theme_bw()+scale_fill_viridis(discrete = TRUE)+facet_wrap(. ~ identity)+theme(legend.position = 'top')+guides(color = guide_colorbar(title.position = 'top', title.hjust = .5,
     barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
      coord_cartesian(xlim = c(1, NA), clip = "off") +
      labs(title="Interactions related to proximity of nest") +
    ylab("number of interactions") + xlab("proximity to nest")

![](birdies_files/figure-markdown_strict/interaction_counr-2.png)

    #dist(rbind(tmp,nest_coord[,1]))
    #boxplot for interactions, distances per treatment
    ggplot(interaction, aes(x=treatment, y=distance_nest,fill=treatment)) + #or distance_food
      geom_boxplot(outlier.colour="black", outlier.shape=16,
                 outlier.size=2, notch=FALSE,show.legend=F) +
      theme_bw()+scale_fill_viridis(discrete = T)+theme(legend.position = 'top')+
      guides(fill = guide_legend(title.position = 'top',override.aes=list(size=10), title.hjust = .5,
     barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
      coord_cartesian(xlim = c(1, NA), clip = "off") +
      labs(title="Interactions related to distance of the nest") +
    xlab("treatment") + ylab("distance to nest")

![](birdies_files/figure-markdown_strict/interaction_counr-3.png)

    #distance_food
    ggplot(interaction, aes(x=treatment, y=distance_food,fill=treatment)) + #or distance_food
      geom_boxplot(outlier.colour="black", outlier.shape=16,
                 outlier.size=2, notch=FALSE,show.legend=F) +
      theme_bw()+scale_fill_viridis(discrete = T)+theme(legend.position = 'top')+
      guides(fill = guide_legend(title.position = 'top',override.aes=list(size=10), title.hjust = .5,
     barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
      coord_cartesian(xlim = c(1, NA), clip = "off") +
      labs(title="Interactions related to distance to food") +
    xlab("treatment") + ylab("distance to food")

![](birdies_files/figure-markdown_strict/interaction_counr-4.png)
Creating heatmaps for interaction per treatment

![](birdies_files/figure-markdown_strict/pressure-1.png)

    #Interaction plot per treatment per individual
    interaction_count<-interaction %>% dplyr::count(x_pos,y_pos,treatment,identity, sort = TRUE)
    ggplot(interaction_count,aes(x=x_pos,y=y_pos,fill=n))+geom_tile( colour = "grey50")+scale_fill_viridis() + theme_bw()+facet_grid(identity~treatment)+
     labs(title="Interactions per treatment per individual") +
    ylab("y position") + xlab("x position")

![](birdies_files/figure-markdown_strict/interaction%20plot-1.png)

![](birdies_files/figure-markdown_strict/sex%20and%20session-1.png)![](birdies_files/figure-markdown_strict/sex%20and%20session-2.png)

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.

    ks.test(interaction$distance_food,"punif",0,5)

    ## Warning in ks.test(interaction$distance_food, "punif", 0, 5): ties should not be
    ## present for the Kolmogorov-Smirnov test

    ## 
    ##  One-sample Kolmogorov-Smirnov test
    ## 
    ## data:  interaction$distance_food
    ## D = 0.2527, p-value = 3.959e-12
    ## alternative hypothesis: two-sided
