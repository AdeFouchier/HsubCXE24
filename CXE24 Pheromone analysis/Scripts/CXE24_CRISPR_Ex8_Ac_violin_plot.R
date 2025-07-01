#### Loading libraries ####
library("readxl")
library("ggplot2")
library("dplyr")


#### import data ####
CXE24_pheromone <-   as.data.frame(read_excel("CXE24_integration_all_pheromone_data_filtered50.xlsx"))
CXE24_CRISPR_pheromone<- subset(CXE24_pheromone, Experience=="CRISPR_Ex8")

#### #### #### Plotting #### #### #### 
#### Preparing CXE24_CRISPR_pheromone levels for plotting ####
CXE24_CRISPR_pheromone$Genotype <- factor(CXE24_CRISPR_pheromone$Genotype, # Setting Genotype as level
                                         levels = c("wt", # and ordering levels
                                                    "He",
                                                    "mut"))
levels(CXE24_CRISPR_pheromone$Genotype) <- c("CXE24 +/+", # and renaming levels
                                            "CXE24 +/-",
                                            "CXE24 -/-")


#### Prearing data info for plotting ####
plot_data <- CXE24_CRISPR_pheromone # Setting plot data

plot_data$x <- plot_data$Genotype # Setting which variables are x

#### Setting color Pallette ####
x_palette <- c("#006300", # 1st x = wt
               "#5CB157", # 2nd x = he
               "#B8FFAD") # 3rd x = mut

#### Plotting Violin without points ####

Plot_CXE24_CRISPR_Ex8_Ac_level <- ggplot(data = plot_data,
                           aes(x = Genotype,
                               y = Total_Ac))+
  geom_violin(aes (color=x),
              width = 1,
              lwd = 0.75) +
  geom_jitter(aes(fill = x),
              shape = 21,
              size = 1.2,
              stroke = 0.25,
              width = 0.1)+
  stat_summary(fun=mean, # Plotting the mean of the distance
               geom = "crossbar", # as a scatter plot
               color = "black", # choosing the outline grey
               width = 0.5)+ # chosing the crossbar width 
  xlab(label = "Genotype")+
  ylab(label = "Relative % of acetate esters")+
  scale_fill_manual(values = x_palette)+ # setting the colors
  scale_color_manual(values= x_palette)+
  theme(legend.position="none", #removing legend
        axis.text.x= element_text(face = "bold", #Changing X text to bold
                                  color = "black"),  # Changing x text color
        axis.title.x=element_blank(),  #Removing x title
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14,face="bold"),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "grey95"),
        #panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(face = "bold"),
        strip.text.y = element_text(face = "bold"))

Plot_CXE24_CRISPR_Ex8_Ac_level
#Total amoun of Ac (ng)#
Plot_CXE24_CRISPR_Ex8_Ac_am <- ggplot(data = plot_data,
                                      aes(x = Genotype,
                                          y = Total_Ac_am))+
  geom_violin(aes (color=x),
              width = 1,
              lwd = 0.75) +
  geom_jitter(aes(fill = x),
              shape = 21,
              size = 1.2,
              stroke = 0.25,
              width = 0.1)+
  stat_summary(fun=mean, # Plotting the mean of the distance
               geom = "crossbar", # as a scatter plot
               color = "black", # choosing the outline grey
               width = 0.5)+ # chosing the crossbar width 
  xlab(label = "Genotype")+
  ylab(label = "Amount of acetate esters (ng)")+
  scale_fill_manual(values = x_palette)+ # setting the colors
  scale_color_manual(values= x_palette)+
  theme(legend.position="none", #removing legend
        axis.text.x= element_text(face = "bold", #Changing X text to bold
                                  color = "black"),  # Changing x text color
        axis.title.x=element_blank(),  #Removing x title
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14,face="bold"),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "grey95"),
        #panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(face = "bold"),
        strip.text.y = element_text(face = "bold"))

Plot_CXE24_CRISPR_Ex8_Ac_am

#Total amoun of Ac (ng)#
Plot_CXE24_CRISPR_Ex8_am <- ggplot(data = plot_data,
                                      aes(x = Genotype,
                                          y = Total_amount))+
  geom_violin(aes (color=x),
              width = 1,
              lwd = 0.75) +
  geom_jitter(aes(fill = x),
              shape = 21,
              size = 1.2,
              stroke = 0.25,
              width = 0.1)+
  stat_summary(fun=mean, # Plotting the mean of the distance
               geom = "crossbar", # as a scatter plot
               color = "black", # choosing the outline grey
               width = 0.5)+ # chosing the crossbar width 
  xlab(label = "Genotype")+
  ylab(label = "Amount of pheromone (ng)")+
  scale_fill_manual(values = x_palette)+ # setting the colors
  scale_color_manual(values= x_palette)+
  theme(legend.position="none", #removing legend
        axis.text.x= element_text(face = "bold", #Changing X text to bold
                                  color = "black"),  # Changing x text color
        axis.title.x=element_blank(),  #Removing x title
        axis.text.y=element_text(size=8),
        axis.title.y=element_text(size=10,face="bold"),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "grey95"),
        #panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(face = "bold"),
        strip.text.y = element_text(face = "bold"))

Plot_CXE24_CRISPR_Ex8_am

#Total amount of Ac log contrasted by Z9-14Ald#
Plot_CXE24_CRISPR_Ex8_Ac_log_contrasted <- ggplot(data = plot_data,
                                                 aes(x = Genotype,
                                                     y = `Tot_ac/z9_14Ald_log`))+
  geom_violin(aes (color=x),
              width = 1,
              lwd = 0.75) +
  geom_jitter(aes(fill = x),
              shape = 21,
              size = 1.2,
              stroke = 0.25,
              width = 0.1)+
  stat_summary(fun=mean, # Plotting the mean of the distance
               geom = "crossbar", # as a scatter plot
               color = "black", # choosing the outline grey
               width = 0.5)+ # chosing the crossbar width 
  xlab(label = "Genotype")+
  ylab(label = "Log-contrasted to Z9-14:Ald")+
  scale_fill_manual(values = x_palette)+ # setting the colors
  scale_color_manual(values= x_palette)+
  theme(legend.position="none", #removing legend
        axis.text.x= element_text(face = "bold", #Changing X text to bold
                                  color = "black"),  # Changing x text color
        axis.title.x=element_blank(),  #Removing x title
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14,face="bold"),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "grey95"),
        #panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(face = "bold"),
        strip.text.y = element_text(face = "bold"))

Plot_CXE24_CRISPR_Ex8_Ac_log_contrasted

#Total amount of Ac log contrasted by 14Ald#
Plot_CXE24_CRISPR_Ex8_Ac_log_contrasted_14Ald <- ggplot(data = plot_data,
                                                  aes(x = Genotype,
                                                      y = `Tot_ac/14Ald_log`))+
  geom_violin(aes (color=x),
              width = 1,
              lwd = 0.75) +
  geom_jitter(aes(fill = x),
              shape = 21,
              size = 1.2,
              stroke = 0.25,
              width = 0.1)+
  stat_summary(fun=mean, # Plotting the mean of the distance
               geom = "crossbar", # as a scatter plot
               color = "black", # choosing the outline grey
               width = 0.5)+ # chosing the crossbar width 
  xlab(label = "Genotype")+
  ylab(label = "Log-contrasted to Z9-14:Ald")+
  scale_fill_manual(values = x_palette)+ # setting the colors
  scale_color_manual(values= x_palette)+
  theme(legend.position="none", #removing legend
        axis.text.x= element_text(face = "bold", #Changing X text to bold
                                  color = "black"),  # Changing x text color
        axis.title.x=element_blank(),  #Removing x title
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14,face="bold"),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "grey95"),
        #panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(face = "bold"),
        strip.text.y = element_text(face = "bold"))

Plot_CXE24_CRISPR_Ex8_Ac_log_contrasted_14Ald
#### Cleaning ####
rm(plot_data,
   #plot_data_info,
   x_palette)

