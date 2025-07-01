#### Loading libraries ####
library("readxl")
library("ggplot2")
library("dplyr")
library("tidyr")


#### import data ####
CXE24_pheromone <-   as.data.frame(read_excel("CXE24_integration_all_pheromone_data_filtered50.xlsx"))

#### #### #### Plotting #### #### #### 
#### Preparing CXE24_pheromone levels for plotting ####
CXE24_pheromone$Genotype <- factor(CXE24_pheromone$Genotype, # Setting Genotype as level
                                          levels = c("wt", # and ordering levels
                                                     "He",
                                                     "mut"))
levels(CXE24_pheromone$Genotype) <- c("CXE24 +/+", # and renaming levels
                                             "CXE24 +/-",
                                             "CXE24 -/-")

CXE24_pheromone_long <- as.data.frame(pivot_longer(data = CXE24_pheromone,
                                                         cols = colnames(CXE24_pheromone[,5:length(CXE24_pheromone)]),
                                                         names_to = "measure",
                                                         values_to = "value"))
CXE24_pheromone_long$X <- paste(CXE24_pheromone_long$Experience,CXE24_pheromone_long$Genotype,sep="_")


CXE24_pheromone_long_selec <- subset(CXE24_pheromone_long,
                                     measure %in% c("Total_amount",
                                                    "z9_16Ald/z9_14Ald_log",
                                                    "z11_16Ald/z9_14Ald_log",
                                                    "z11_16OH/z9_14Ald_log"))


CXE24_pheromone_long_selec$measure <- factor(CXE24_pheromone_long_selec$measure,  # Setting measure as factor
                                             levels = c("Total_amount",
                                                        "z9_16Ald/z9_14Ald_log",
                                                        "z11_16Ald/z9_14Ald_log",
                                                        "z11_16OH/z9_14Ald_log"))

levels(CXE24_pheromone_long_selec$measure) <- c("A) Pheromone amount", 
                                                "B) Z9-16:Ald amount",
                                                "C) Z11-16:Ald amount",
                                                "D) Z11-16:OH amount")

CXE24_pheromone_long_selec$Experience <- factor(CXE24_pheromone_long_selec$Experience,  # Setting measure as factor
                                             levels = c("HsTED_Ex8","CRISPR_Ex8","CRISPR_Ex2"))

#### Prearing data info for plotting ####
plot_data <- CXE24_pheromone_long_selec # Setting plot data
plot_data$x <- plot_data$X # Setting which variables are x
plot_data$g <- plot_data$Experience # 
plot_data$y <- plot_data$value # y
plot_data$z <- plot_data$measure # and z


plot_data_y <- plot_data %>% # Creating a data frame with the min/max number of the interest variable for the positon of plot annotations
  summarise(y_max = max(y), # computing the max y observe
            y_min = min(y)) # computing the min y observe

plot_data_y_z <- plot_data %>% # Creating a data frame with the min/max number of the interest variable for the positon of plot annotations
  group_by(z) %>% # for each z
  summarise(y_max_z = max(y), # computing the max y observe
            y_min_z = min(y)) # computing the min y observe

plot_data_y_z_x <- plot_data %>% # Creating a data frame with the max y for the position of plot annotations
  group_by(x, # by groupping by relevant variables
           z) %>%
  summarise(y_max_z_x = max(y),# computing the max y 
            y_min_z_x = min(y)) # computing the min y 

plot_data_y_z_g <- plot_data %>% # Creating a data frame with the max y for the position of plot annotations
  group_by(g, # by groupping by relevant variables
           z) %>%
  summarise(y_max_z_g = max(y),# computing the max y 
            y_min_z_g = min(y)) # computing the min y 

#### Preparing stat group info ####
x_list <- levels(plot_data$Experience)
z_list <- levels(plot_data$measure)

plot_stat_group <- data.frame(x = rep(x = x_list, # creating a data.frame to receive stat groups
                                      each = length(z_list),
                                      times = 1),
                              z = rep(x = z_list,
                                      each = 1,
                                      times = length(x_list)))

rm(x_list,
   z_list)

plot_stat_group$stat_group <- NA # Creating the stat_group variable

plot_stat_group[plot_stat_group$z  == "Pheromone amount",
                "stat_group"] <- c("a", # he
                                    "a") # mut

plot_stat_group[plot_stat_group$z  == "z9-16:Ald log-contrasted to z9-14:Ald",
                "stat_group"] <- c("a", # he
                                   "b") # mut


#### Merging all plotting info together ####

plot_data_info <- Reduce(function(x,y) merge(x = x, y = y, # Merging the data frame created for ease of manipulation
                                             all = T),
                         list(plot_data_y,
                              plot_data_y_z,
                              plot_data_y_z_x,
                              plot_data_y_z_g))

rm(plot_data_y,
   plot_data_y_z,
   plot_data_y_z_x,
   plot_data_y_z_g)

#### Setting color Pallette ####
x_palette <- c("#468499", # x = wt_HsTED
               "#87B6D1", # x = he_HsTED
               "#D8E6F3", # x = mut_HsTED
               "#006300", # 1st x = wt_CRISPR
               "#5CB157", # 2nd x = he_CRISPR
               "#B8FFAD",# x = mut_CRISPR
               "#FFA500", # x = wt_CRISPR_ex2
               "#FFD556", # x = he_CRISPR_ex2
               "#FFF7C7") # x = mut_CRISPR_ex2

#### Plotting ####
CXE24_plot_extra_pheromone <- ggplot(data = plot_data,
                         aes(x = x,
                             y = y))+
  geom_violin(aes(fill = X),
               width = 0.75)+
  stat_summary(fun=mean, # Plotting the mean of the distance
               geom = "crossbar", # as a scatter plot
               color = "black", # choosing the outline grey
               width = 0.5)+ # chosing the crossbar width
  geom_text(data = plot_data_info, # Adding a geom_text
            aes(x = x,
                y = (y_min_z - # y
                       (0.05 * y_max_z)), # adjust for heigth
                label= ""),
            size = 3.175,
            hjust = 0.5,
            vjust = 0.5)+
scale_x_discrete(limits=c("HsTED_Ex8_CXE24 +/+",
                          "HsTED_Ex8_CXE24 +/-",
                          "HsTED_Ex8_CXE24 -/-",
                          "",
                          "CRISPR_Ex8_CXE24 +/+",
                          "CRISPR_Ex8_CXE24 +/-",
                          "CRISPR_Ex8_CXE24 -/-",
                          "",
                          "CRISPR_Ex2_CXE24 +/+",
                          "CRISPR_Ex2_CXE24 +/-",
                          "CRISPR_Ex2_CXE24 -/-")) +
  facet_wrap(. ~ z,
             nrow = 2,
             scales = "free_y")+
  xlab(label = "")+
  ylab(label = "Pheromone quantity (ng)")+
  scale_fill_manual(values = x_palette) +
  theme(axis.text.x= element_blank(), #Removing x legend
        axis.title.x=element_blank(),  #Removing x title
        legend.title = element_blank(),
        legend.position='none',
        axis.text.y=element_text(size=8),
        axis.title.y=element_text(size=10,face="bold"),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "grey95"),
        #panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(face = "bold"),
        strip.text.y = element_text(face = "bold")) 

CXE24_plot_extra_pheromone

#### Cleaning and saving ####
rm(plot_data,
   plot_data_info,
   x_palette)

ggsave("CXE24_plot_Figure_2_extra_pheromone.pdf", # as pdf
       plot = CXE24_plot_extra_pheromone,
       width = 200, # choosing size
       height = 200, # choosing size
       units = "mm",
       scale = 1,
       device = "pdf",
       dpi = 400)

ggsave("CXE24_plot_Figure_2_extra_pheromone.png", # as png
       plot = CXE24_plot_extra_pheromone,
       width = 200, # choosing size
       height = 200, # choosing size
       units = "mm",
       scale = 1,
       device = "png",
       dpi = 400)

