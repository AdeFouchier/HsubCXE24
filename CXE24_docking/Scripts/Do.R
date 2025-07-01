###################################################################
#### Building Vina Script #### ####
# Saving the vina_do_script bat file
write.table(x = Vina[["parameters"]] %>%
              dplyr::select(vina_path,
                            config_arg,
                            receptor_arg,
                            center_x_arg,
                            center_y_arg,
                            center_z_arg,
                            size_x_arg,
                            size_y_arg,
                            size_z_arg,
                            ligand_arg,
                            out_arg),
            sep = "  ",
            quote = F,
            row.names	 = F,
            col.names = F,
            file = "./Vina/Enzyme_vs_pherobase/Enzyme_vs_pherobase-vina_do_script.bat")

# Replacing ' by "
readLines(con = "./Vina/Enzyme_vs_pherobase/Enzyme_vs_pherobase-vina_do_script.bat") %>%
  str_replace_all(pattern = "'",
                  replacement = '"') %>%
  writeLines(con = "./Vina/Enzyme_vs_pherobase/Enzyme_vs_pherobase-vina_do_script.bat")


#### /!\ Running Vina #### ####
# Launching Vina
system(command = paste0('"',
                        getwd(),
                        '/Vina/Enzyme_vs_pherobase/Enzyme_vs_pherobase-vina_do_script.bat"'),
       intern = T)


###################################################################
#### Resetting the Vina results data #### ####
Vina[["results"]] <- data.frame(matrix(nrow = 0, # Creating the data frame
                                       ncol = length(Parameters[["Vina_data_cols"]])))

colnames(Vina[["results"]]) <- Parameters[["Vina_data_cols"]] # Renaming the columns


#### Extracting Vina results #### ####
# for Hvir and Hsub
for(i in unique(Vina[["parameters"]]$enzyme)){ # for each enzyme
  for(j in unique(Vina[["parameters"]]$ligand)){ # for each ligand
    if(length(list.files(paste0("./Vina/",
                                unique((Vina[["parameters"]] %>%
                                        subset(enzyme == i))$folder),
                                "/output"))[grepl(list.files(paste0("./Vina/",
                                                                    unique((Vina[["parameters"]] %>%
                                                                            subset(enzyme == i))$folder),
                                                                    "/output")),
                                                  pattern = i) &
                                            grepl(list.files(paste0("./Vina/",
                                                                    unique((Vina[["parameters"]] %>%
                                                                            subset(enzyme == i))$folder),
                                                                    "/output")),
                                                  pattern = paste0("_",
                                                                   j))]) > 0){ # If there is a file for i and j
      temp <- readLines(con = paste0(paste0("./Vina/",
                                            unique((Vina[["parameters"]] %>%
                                                      subset(enzyme == i))$folder),
                                            "/output/"),
                                     list.files(paste0("./Vina/",
                                                       unique((Vina[["parameters"]] %>%
                                                                 subset(enzyme == i))$folder),
                                                       "/output"))[grepl(list.files(paste0("./Vina/",
                                                                                           unique((Vina[["parameters"]] %>%
                                                                                                     subset(enzyme == i))$folder),
                                                                                           "/output")),
                                                                         pattern = i) &
                                                                     grepl(list.files(paste0("./Vina/",
                                                                                             unique((Vina[["parameters"]] %>%
                                                                                                       subset(enzyme == i))$folder),
                                                                                             "/output")),
                                                                           pattern = paste0("_",
                                                                                            j))])) # first line is not a header
      
      Vina[["results"]] <- rbind(Vina[["results"]], # rbind of previous Vina[["results"]] with 
                                 data.frame(enzyme = i, # a data frame containing the recpetor
                                            ligand = j, # the ZINC
                                            model =  temp[grep(temp, # and subsequent extractions from the file
                                                               pattern = "MODEL")],
                                            result = temp[grep(temp,
                                                               pattern = "VINA RESULT")],
                                            inter =  temp[grep(temp,
                                                               pattern = "INTER:")],
                                            intra =  temp[grep(temp,
                                                               pattern = "REMARK INTRA:")],
                                            inter_intra =  temp[grep(temp,
                                                                     pattern = "INTER \\+ INTRA")],
                                            unbound =  temp[grep(temp,
                                                                 pattern = "UNBOUND")]))
      rm(temp)
    }
  }
  rm(j)
}
rm(i)

## for Slit
#for(i in unique(Vina[["parameters"]]$enzyme)){ # for each enzyme
#  for(j in unique(Vina[["parameters"]]$ligand)){ # for each ligand
#    if(length(list.files("./Vina/SlitCXE_vs_pherobase/output")[grepl(list.files("./Vina/SlitCXE_vs_pherobase/output"),
#                                                                     pattern = i) &
#                                                               grepl(list.files("./Vina/SlitCXE_vs_pherobase/output"),
#                                                                     pattern = paste0("_",
#                                                                                      j))]) > 0){ # If there is a file for i and j
#      temp <- read.delim(file = paste0("./Vina/SlitCXE_vs_pherobase/output/",
#                                       list.files("./Vina/SlitCXE_vs_pherobase/output")[grepl(list.files("./Vina/SlitCXE_vs_pherobase/output"),
#                                                                                              pattern = i) &
#                                                                                          grepl(list.files("./Vina/SlitCXE_vs_pherobase/output"),
#                                                                                                pattern = paste0("_",
#                                                                                                                 j))]),
#                         header = F) # first line is not a header
#      
#      Vina[["results"]] <- rbind(Vina[["results"]], # rbind of previous Vina[["results"]] with 
#                                 data.frame(enzyme = i, # a data frame containing the recpetor
#                                            ligand = j, # the ZINC
#                                            model =  temp[grep(temp[,1], # and subsequent extractions from the file
#                                                               pattern = "MODEL"),1],
#                                            result = temp[grep(temp[,1],
#                                                               pattern = "VINA RESULT"),1],
#                                            inter =  temp[grep(temp[,1],
#                                                               pattern = "INTER:"),1],
#                                            intra =  temp[grep(temp[,1],
#                                                               pattern = "REMARK INTRA:"),1],
#                                            inter_intra =  temp[grep(temp[,1],
#                                                                     pattern = "INTER \\+ INTRA"),1],
#                                            unbound =  temp[grep(temp[,1],
#                                                                 pattern = "UNBOUND"),1]))
#    }
#  }
#  rm(j)
#}
#rm(i)
#### Curating extraction #### ####
# Removing unwanted character patterns
Vina[["results"]]$model <- as.factor(as.numeric(stringr::str_remove(Vina[["results"]]$model,
                                                                    pattern = "MODEL ")))

Vina[["results"]]$result <- stringr::str_remove(Vina[["results"]]$result,
                                                pattern = "REMARK VINA RESULT:")

# Removing leading spaces in Vina[["results"]]
while(length(Vina[["results"]]$result[stringr::str_detect(Vina[["results"]]$result,  # while there is data with a space a the begining of the string
                                                          pattern = "^[:space:]")]) > 0){
  Vina[["results"]]$result <- stringr::str_remove(Vina[["results"]]$result,
                                                  pattern = "^[:space:]")
}

# Reducing the number of spaces between data in Vina[["results"]] to one
while(length(Vina[["results"]]$result[stringr::str_detect(Vina[["results"]]$result,  # while there is data with a double spaces
                                                          pattern = "[:space:][:space:]")]) > 0){
  Vina[["results"]]$result <- stringr::str_replace(Vina[["results"]]$result, # replacing
                                                   pattern = "[:space:][:space:]", # double spaces
                                                   replacement = " ") # by a single space
}

# Removing "_ranked[0-9]_orient" in the enzyme names
Vina[["results"]]$enzyme <- stringr::str_remove(Vina[["results"]]$enzyme,
                                                pattern = "_ranked[0-9]_orient")

# Removing unwanted pattern in inter
Vina[["results"]]$inter <- as.numeric(stringr::str_remove(Vina[["results"]]$inter,
                                                          pattern = "REMARK INTER:"))

# Removing unwanted pattern in inter_intra
Vina[["results"]]$inter_intra <- as.numeric(stringr::str_remove(Vina[["results"]]$inter_intra,
                                                                pattern = "REMARK INTER \\+ INTRA:"))

# Removing unwanted pattern in intra
Vina[["results"]]$intra <- as.numeric(stringr::str_remove(Vina[["results"]]$intra,
                                                          pattern = "REMARK INTRA:"))

# Removing unwanted pattern in intra
Vina[["results"]]$unbound <- as.numeric(stringr::str_remove(Vina[["results"]]$unbound,
                                                            pattern = "REMARK UNBOUND:"))

# Splitting the result data frame into the 3 differents variables it contains
Vina[["results"]] <- tidyr::separate(data = Vina[["results"]],
                                     col = result,
                                     into = c("affinity",
                                              "dist.rmsd.lb",
                                              "dist.rmsd.ub"),
                                     sep = " ")

# merging Vina[["results"]] with ligand infos
Vina[["results"]] <- merge(Vina[["results"]],
                           Parameters[["Ligands_infos"]])

## Removing (E)
#Vina[["results"]] <- subset(Vina[["results"]],
#                  unsaturation_orientation != "E")
#
# Managing variables
Vina[["results"]]$ZINC <- factor(Vina[["results"]]$ZINC,
                                 levels = unique(Vina[["results"]]$ZINC))

Vina[["results"]]$term_function <- factor(gsub(gsub(Vina[["results"]]$term_function,
                                                    pattern = "Ac",
                                                    replacement = ":OAc",
                                                    fixed = T),
                                               pattern = "OH",
                                               replacement = ":OH",
                                               fixed = T),
                                          levels = c(":OAc",
                                                     ":OH",
                                                     "Ald",
                                                     "Acid"))

Vina[["results"]]$unsaturation_orientation <- factor(Vina[["results"]]$unsaturation_orientation,
                                                     levels = c("None",
                                                                "E",
                                                                "Z"))

Vina[["results"]]$ligand <- factor(Vina[["results"]]$ligand,
                                   levels = unique(as.character((Vina[["results"]] %>%
                                                                   dplyr::arrange(carbon_chain_lenght,
                                                                                  unsaturation_orientation,
                                                                                  unsaturation_position,
                                                                                  term_function))$ligand)))
Vina[["results"]]$enzyme <- factor(Vina[["results"]]$enzyme,
                                   levels = names(Plots[["Labels"]][["enzymes"]]))

# Creating a chain_name variable (ligand without the function)
Vina[["results"]]$chain_name <- stringr::str_remove(as.character(Vina[["results"]]$ligand), # removing function in the ligand
                                                    pattern = "OH|Ald|Acid|Ac")

# Ordering
Vina[["results"]]$chain_name <- factor(Vina[["results"]]$chain_name,
                                       levels = unique(as.character((Vina[["results"]] %>%
                                                                       dplyr::arrange(carbon_chain_lenght,
                                                                                      unsaturation_orientation,
                                                                                      unsaturation_position,
                                                                                      unsaturation_orientation))$chain_name)))



Vina[["results"]]$affinity <- as.numeric(Vina[["results"]]$affinity)
Vina[["results"]]$dist.rmsd.lb <- as.numeric(Vina[["results"]]$dist.rmsd.lb)
Vina[["results"]]$dist.rmsd.ub <- as.numeric(Vina[["results"]]$dist.rmsd.ub)

Vina[["results"]]$species <- 
  factor(substr(Vina[["results"]]$enzyme,
                1,4),
         levels = names(Plots[["Labels"]][["species"]]))

Vina[["results"]]$gene <- 
  factor(substr(Vina[["results"]]$enzyme,
                5,
                (str_locate(Vina[["results"]]$enzyme, "_")[,1]-1)),
         levels = unique(substr(Plots[["Labels"]][["enzymes"]],
                                start = 5,
                                stop = nchar(Plots[["Labels"]][["enzymes"]]))))

# Setting the allele factor
Vina[["results"]] <- 
  Vina[["results"]] %>%
  mutate(allele = str_remove(string = enzyme, #from enzyme
                             pattern = paste0("^", # removing the pattern at the begning of the string
                                              species, # of species
                                              gene, # followed by gene
                                              "_")) %>% # followed by an underscore
           str_remove(pattern = "noSP$") %>% # removing any terminal "noSP"
           str_remove(pattern = "_$") %>% # and underscore
           ifelse(test = . == "" | # if allele is empty 
                    . == "allele1", # or allele1
                  yes = "wt", # setting it as wt
                  no = .)) %>% # else keeping original allele
  mutate(allele = factor(allele, # setting allele as a factor
                         levels = c("wt", # which levels starts with wt
                                    unique(allele) %>% # followed by the allele values
                                      str_subset(negate = T, # selecting values that do not match
                                                 pattern = "wt")))) # the pattern "wt"


# Saving the Vina[["results"]]
write.table(x = Vina[["results"]],
            file = "./Vina/Vina_results.tsv",
            quote = F,
            na = "",
            dec = ".",
            sep = "\t")

##################################################################
#### Creating the 1_hbonds_script.cxc #### ####
# Resetting the script
hbond_script <- c()

# Preparing the script
for(i in Parameters[["enzyme"]]){ # Loop for all enzymes
  hbond_script <- c( # setting a the hbond_script a vector containing
    hbond_script, # the previously generated script
    c( # and another vector containing the following lines
      paste0('open "C:/Pro/Projets/Heliothis Acetate production/CXE24_docking/ChimeraX/Oriented models/', # opening the pdbqt oriented model
             i,
             '_ranked0_orient.pdb"'),
      # below = opening the different ligands matched to the enzyme
      paste0('open "C:/Pro/Projets/Heliothis Acetate production/CXE24_docking/Vina/Enzyme_vs_pherobase/output/',
             i,
             '_ranked0_orient_pocket_1_vs_Z7-16Ac.pdbqt" "C:/Pro/Projets/Heliothis Acetate production/CXE24_docking/Vina/Enzyme_vs_pherobase/output/',
             i,
             '_ranked0_orient_pocket_1_vs_Z9-16Ac.pdbqt" "C:/Pro/Projets/Heliothis Acetate production/CXE24_docking/Vina/Enzyme_vs_pherobase/output/',
             i,
             '_ranked0_orient_pocket_1_vs_Z11-16Ac.pdbqt" showTool false'), # while hiding the "ViewDockX" tool
      paste0('cd "C:/Pro/Projets/Heliothis Acetate production/CXE24_docking/ChimeraX/',
             i,
             '/"'),
      paste0("hbonds #2 #3 #4 restrict #!1 saveFile ",
             i,
             "_hbonds.txt"),
      paste0("clashes #2 #3 #4 restrict #!1 saveFile ",
             i,
             "_clashes.txt"),
      paste0('save "',
             i,
             '_orient_pdbqt_w_hbonds.cxs"'),
      "close session"
    )
  )
}
rm(i)

# Adding the command to quit ChimeraX when all is done
hbond_script <- c( # setting the hbond_script as a vector
  hbond_script, # with previous hbond_script
  "quit" # followed by the command to quit the ChimeraX program
)

# Saving the script.
write(x = hbond_script,
      file = "./ChimeraX/1_hbonds_script.cxc")

#### Making sure necessary folders are there ####
for(i in Parameters[["enzyme"]]){ # for all enzymes
  if (!dir.exists( # Checking the existence 
    paste0("./ChimeraX/",  # in the folder in ChimeraX
           i # of the folder relative to the enzymz
           ,"/"))) {
    dir.create( # If the folder doesn't exist, create it
      paste0("./ChimeraX/",  # in the folder in ChimeraX
             i # of the folder relative to the enzymz
             ,"/"),
      recursive = TRUE)
    print(paste("Folder for", # Stating that the folder was created
                i,
                "created successfully."))
  } else {
    print(paste("Folder",
                i,
                "already exists.")) # Else, stating that the folder already exists
  }
  
}
rm(i)
#### /!\ Run the 1_hbonds_script.cxc in Chimera X #### ##########
shell.exec(paste0('"',
                  getwd(),
                  '/ChimeraX/"'))
##################################################################
#### Resetting the Hbonds_data all #### ####
Hbonds_data[["all"]] <- data.frame(matrix(nrow = 0, # creating data frame to receive data
                                          ncol = length(Parameters[["Hbonds_cata_cols"]])))

colnames(Hbonds_data[["all"]]) <- Parameters[["Hbonds_cata_cols"]] # naming its columns

#### Reading data and generating a table with the hbond data #### ####
for(i in Parameters[["enzyme"]]){
  print(i)
  Hbonds_data[[i]] <- read.csv(file =  paste0("./ChimeraX/", # reading data for enzyme i
                                              i,
                                              "/",
                                              i,
                                              "_hbonds.txt"),
                               skip =  grep(readLines(con = paste0("./ChimeraX/", # skipping line
                                                                   i,
                                                                   "/",
                                                                   i,
                                                                   "_hbonds.txt")),
                                            pattern = "donor"), # until the begining of the hbonds data
                               header = F, # no header
                               sep = "@") # fake sep
  
  while(length(grep(Hbonds_data[[i]][1], # loop to remove doubles spaces in the hbonds data
                    pattern = "  ")) > 0){
    Hbonds_data[[i]]$V1 <-  gsub(Hbonds_data[[i]]$V1, # using gsub of double spaces into single spaces
                                 pattern = "  ",
                                 replacement = " ")
  }
  
  temp_df1 <- separate(data = Hbonds_data[[i]] %>%  # separating data based
                         subset(! grepl(Hbonds_data[[i]]$V1,
                                        pattern = "no hydrogen")), # without no hydrogen
                       col = V1, # of the V1 (and only) variable
                       into = paste0("V", # into enough variables
                                     1:length(strsplit((Hbonds_data[[i]] %>%
                                                          subset(! grepl(Hbonds_data[[i]]$V1,
                                                                         pattern = "no hydrogen")))[1,1], # to accommodate all the variable present in the future data table
                                                       split = " ")[[1]])), # based on the space separator
                       sep = " ")# [,c(1, # with space separator and keeping only variables of interest
  #     3,
  #     4,
  #     6,
  #     7,
  #     16,
  #     17)]
  
  # Re-organising the data so that we can get Hbonds where the donor is the prot and hbonds where the donnor is the ligands
  temp_df1 <- rbind(temp_df1 %>%
                      subset(grepl(temp_df1$V2,
                                   pattern = "#1")) %>%
                      dplyr::select(V1 = V1,
                                    V2 = V3,
                                    V3 = V4,
                                    V4 = V6,
                                    V5 = V7,
                                    V6 = V16,
                                    V7 = V17),
                    temp_df1 %>%
                      subset(grepl(temp_df1$V7,
                                   pattern = "#1")) %>%
                      dplyr::select(V1 = V6,
                                    V2 = V8,
                                    V3 = V9,
                                    V4 = V1,
                                    V5 = V2,
                                    V6 = V16,
                                    V7 = V17))
  
  
  
  colnames(temp_df1) <- Parameters[["Hbonds_cata_cols"]][c(1:5,7:8)] # renaming variables of the data
  
  if(length(grep(Hbonds_data[[i]][,1],
                 pattern = "no hydrogen")) > 0){ # fi there is data with no hydrogen
    temp_df2 <- separate(data = Hbonds_data[[i]] %>% # separating data based
                           subset(grepl(Hbonds_data[[i]][,1],
                                        pattern = "no hydrogen")), # with no hydrogen
                         col = V1, # of the V1 (and only) variable
                         into = paste0("V", # into enough variables
                                       1:length(strsplit((Hbonds_data[[i]] %>%
                                                            subset(grepl(Hbonds_data[[i]]$V1,
                                                                         pattern = "no hydrogen")))[1,1], # to accommodate all the variable present in the future data table
                                                         split = " ")[[1]])), # based on the space separator
                         sep = " ")[,c(1, # with space separator and keeping only variables of interest
                                       3,
                                       4,
                                       6,
                                       7,
                                       13,
                                       14)]
    
    
    colnames(temp_df2)  <- Parameters[["Hbonds_cata_cols"]][c(1:5,7:8)] # renaming variables
    
    temp_df2$dist_DH..A <- NA # setting dist_DH..A as NA
    
    Hbonds_data[[i]] <- rbind(temp_df1, # seting Hbonds_data by binding hydrogen
                              temp_df2) # and no hydrogen data
    
    rm(temp_df2) # cleaning temp_df2
    
  } else { # or if there is only hydrogen data
    Hbonds_data[[i]] <- temp_df1 # setting Hbonds_data as hydrogen data
  }
  
  rm(temp_df1) # cleaning temp_df1
  
  # Series of gsub to remove unwanted characters in the variables strings
  Hbonds_data[[i]]$enzyme <- str_remove_all(Hbonds_data[[i]]$enzyme, 
                                            pattern = "_ranked[0-9]_orient.pdb")
  
  Hbonds_data[[i]] <- subset(Hbonds_data[[i]],
                             enzyme == i)
  
  
  Hbonds_data[[i]]$ligand <- gsub(Hbonds_data[[i]]$ligand,
                                  pattern = ".pdb",
                                  replacement = "")
  
  Hbonds_data[[i]]$ligand <- gsub(Hbonds_data[[i]]$ligand,
                                  pattern = paste0(i,
                                                   "_ranked[0-9]_orient_pocket_[0-9]_vs_"),
                                  replacement = "")
  
  Hbonds_data[[i]]$ligand_id <- gsub(Hbonds_data[[i]]$ligand_id,
                                     pattern = "#",
                                     replacement = "")
  
  Hbonds_data[[i]]$ligand_id <- gsub(Hbonds_data[[i]]$ligand_id,
                                     pattern = "/\\?",
                                     replacement = "")
  
  Hbonds_data[[i]] <- separate(data = Hbonds_data[[i]],
                               col = ligand_id,
                               into = c("ligand_id",
                                        "model"),
                               sep ="\\.")
  
  Hbonds_data[["all"]] <- rbind(Hbonds_data[["all"]], # adding to previous data
                                Hbonds_data[[i]]) # data for i
  
}
rm(i)

#### Resetting the Clashes_data all #### ####
Clashes_data[["all"]] <- data.frame(matrix(nrow = 0, # creating data frame to receive data
                                           ncol = length(Parameters[["Clashes_cata_cols"]])))

colnames(Clashes_data[["all"]]) <- Parameters[["Clashes_cata_cols"]] # naming its columns

#### Reading data and generating a table with the clashes data #### ####
for(i in Parameters[["enzyme"]]){
  print(i)
  Clashes_data[[i]] <- read.csv(file =  paste0("./ChimeraX/", # reading data for enzyme i
                                               i,
                                               "/",
                                               i,
                                               "_clashes.txt"),
                                skip =  grep(readLines(con = paste0("./ChimeraX/", # skipping line
                                                                    i,
                                                                    "/",
                                                                    i,
                                                                    "_clashes.txt")),
                                             pattern = "atom1"), # until the begining of the clashes data
                                header = F, # no header
                                sep = "@") # fake sep
  
  while(length(grep(Clashes_data[[i]][1], # loop to remove doubles spaces in the clashes data
                    pattern = "  ")) > 0){
    Clashes_data[[i]]$V1 <-  gsub(Clashes_data[[i]]$V1, # using gsub of double spaces into single spaces
                                  pattern = "  ",
                                  replacement = " ")
  }
  Clashes_data[[i]] <- separate(data = Clashes_data[[i]], # separating data based
                                col = V1, # of the V1 (and only) variable
                                into = paste0("V", # into enough variables
                                              1:length(strsplit(Clashes_data[[i]][1,1], # to accommodate all the variable present in the future data table
                                                                split = " ")[[1]])), # based on the space separator
                                sep = " ")[,c(6, # with space separator and keeping only variables of interest
                                              1,
                                              2,
                                              11,
                                              12)]
  
  
  
  colnames(Clashes_data[[i]]) <- Parameters[["Clashes_cata_cols"]][c(1:3,
                                                                     5:6)] # renaming variables of the data
  
  
  
  # Series of gsub to remove unwanted characters in the variables strings
  Clashes_data[[i]]$enzyme <- gsub(Clashes_data[[i]]$enzyme, 
                                   pattern = "_ranked[0-9]_orient.pdb",
                                   replacement = "")
  
  #Clashes_data[[i]] <- subset(Clashes_data[[i]],
  #                           enzyme == i)
  
  
  Clashes_data[[i]]$ligand <- gsub(Clashes_data[[i]]$ligand,
                                   pattern = ".pdb",
                                   replacement = "")
  
  Clashes_data[[i]]$ligand <- gsub(Clashes_data[[i]]$ligand,
                                   pattern = paste0(i,
                                                    "_ranked[0-9]_orient_pocket_[0-9]_vs_"),
                                   replacement = "")
  
  Clashes_data[[i]]$ligand_id <- gsub(Clashes_data[[i]]$ligand_id,
                                      pattern = "#",
                                      replacement = "")
  
  Clashes_data[[i]]$ligand_id <- gsub(Clashes_data[[i]]$ligand_id,
                                      pattern = "/\\?",
                                      replacement = "")
  
  Clashes_data[[i]] <- separate(data = Clashes_data[[i]],
                                col = ligand_id,
                                into = c("ligand_id",
                                         "model"),
                                sep ="\\.")
  
  Clashes_data[["all"]] <- rbind(Clashes_data[["all"]], # adding to previous data
                                 Clashes_data[[i]]) # data for i
  
}
rm(i)

#### Generating the script to test for distances in chimera #### ####
# Adding informationa bout relevant residues
Hbonds_data[["all"]] <- merge(Hbonds_data[["all"]],
                              Parameters[["relevant_residues"]],
                              all = T)

# Replacing NA relevant_residue
Hbonds_data[["all"]] <-
  Hbonds_data[["all"]] %>%
  mutate(relevant_residue = ifelse(test = is.na(relevant_residue),
                                   yes = 0,
                                   no = relevant_residue))

# Creating a data frame to store parameters of the distance ChimeraX script
Parameters[["Distances"]] <- # Setting this Distances Parameters as :
  Parameters[["relevant_residues"]] %>% # by about Parameters[["relevant_residues"]]
  merge(Parameters[["Ligands_models_infos"]]) %>%  # merged with Ligands_models_infos
  filter(prot_AA == "SER") %>% # for SER residues only
  merge( # merged with
    # This part induce mesure of distance for all ligands docking models
    distinct(Vina[["results"]] %>%
               select(enzyme,
                      ligand,
                      model)) %>% # and the models
      merge(Hbonds_data[["all"]] %>% # merged with all Hbonds_data
              select(enzyme, # keeping only varibles about enzymes
                     ligand, # their tested ligands
                     ligand_id) %>% # and the ligand_id
              filter(!is.na(ligand)) %>% # keeping only rows where there is a ligand 
              distinct), # keeping only unique values
    # alternatively use this part to mesure distance for models with Hbonds only 
    #unique(subset(Hbonds_data[["all"]][,c("enzyme", # merging as well with infos about enzymes
    #                                      "ligand", # their tested ligands
    #                                      "ligand_id",# the ligand_id
    #                                      "model")], # and the models with Hbonds
    #              !is.na(ligand))), # for when there is a ligand
    all = T) %>%
  merge(Clashes_data[["all"]], # merging with Clashes_data
        all = T) %>%
  filter(is.na(overlap)) %>% # selecting without overlap i.e. data without steric clashes
  arrange(enzyme,
          ligand,
          as.numeric(model))


distance_script <- c()

# Preparing the script
for(i in Parameters[["enzyme"]]){
  distance_script <- c(distance_script,
                       c(paste0('open "C:/Pro/Projets/Heliothis Acetate production/CXE24_docking/ChimeraX/',
                                i,
                                "/",
                                i,
                                '_orient_pdbqt_w_hbonds.cxs"'),
                         paste0('cd "C:/Pro/Projets/Heliothis Acetate production/CXE24_docking/ChimeraX/',
                                i,
                                '/"'),
                         "distance delete",
                         paste0("distance #1:",# for Ac
                                unique(subset(Parameters[["Distances"]],
                                              enzyme == i &
                                                relevant_residue == 1 &
                                                prot_AA == "SER" &
                                                ligand %in% unique(grep(x = Parameters[["Distances"]]$ligand,
                                                                        pattern = "Ac",
                                                                        value = T)))$prot_AA_pos),
                                "@OG #",
                                subset(Parameters[["Distances"]],
                                       enzyme == i &
                                         ligand %in% unique(grep(x = Parameters[["Distances"]]$ligand,
                                                                 pattern = "Ac",
                                                                 value = T)))$ligand_id,
                                ".",
                                subset(Parameters[["Distances"]],
                                       enzyme == i &
                                         ligand %in% unique(grep(x = Parameters[["Distances"]]$ligand,
                                                                 pattern = "Ac",
                                                                 value = T)))$model,
                                "/?:1@@serial_number=",
                                subset(Parameters[["Distances"]],
                                       enzyme == i &
                                         ligand %in% unique(grep(x = Parameters[["Distances"]]$ligand,
                                                                 pattern = "Ac",
                                                                 value = T)))$C_pos),
                         paste0('distance save "',
                                i,
                                '_distances.txt"'),
                         paste0('save "',
                                i,
                                '_orient_pdbqt_w_distances.cxs"'),
                         "close session"))
}
rm(i)

distance_script <- 
  c(distance_script,
    "quit")

write(x = distance_script,
      file = "./ChimeraX/2_distance_script.cxc")

#### /!\ Run the 2_distance_script.cxc in Chimera X #### #########
shell.exec(paste0('"',
                  getwd(),
                  '/ChimeraX/"'))

##################################################################
#### Resetting the Distance_data all #### ####
Distance_data[["all"]] <- data.frame(matrix(nrow = 0, # creating data frame to receive data
                                            ncol = length(Parameters[["Distance_data_cols"]])))

colnames(Distance_data[["all"]]) <- Parameters[["Distance_data_cols"]] # naming its columns


#### Reading data and generating a table with the distance data #### ####
for(i in Parameters[["enzyme"]]){
  print(i)
  Distance_data[[i]] <- read.csv(file =  paste0("./ChimeraX/", # reading data for enzyme i
                                                i,
                                                "/",
                                                i,
                                                "_distances.txt"),
                                 skip =  grep(readLines(con = paste0("./ChimeraX/", # skipping line
                                                                     i,
                                                                     "/",
                                                                     i,
                                                                     "_distances.txt")),
                                              pattern = "Distance information:"), # until the begining of the Distance data
                                 header = F, # no header
                                 sep = "@") # fake sep
  
  while(length(grep(Distance_data[[i]][1], # loop to remove doubles spaces in the Distance data
                    pattern = "  ")) > 0){
    Distance_data[[i]]$V1 <-  gsub(Distance_data[[i]]$V1, # using gsub of double spaces into single spaces
                                   pattern = "  ",
                                   replacement = " ")
  }
  
  Distance_data[[i]] <- separate(data = Distance_data[[i]],  # separating data based
                                 col = V1, # of the V1 (and only) variable
                                 into = paste0("V", # into enough variables
                                               1:length(strsplit((Distance_data[[i]])[1,1], # to accommodate all the variable present in the future data table
                                                                 split = " ")[[1]])), # based on the space separator
                                 sep = " ")[,c(1, # with space separator and keeping only variables of interest
                                               3,
                                               4,
                                               7,
                                               8,
                                               12)]
  colnames(Distance_data[[i]]) <- Parameters[["Distance_data_cols"]][c(1:5,7)]
  
  # Series of gsub to remove unwanted characters in the variables strings
  Distance_data[[i]]$enzyme <- gsub(Distance_data[[i]]$enzyme, 
                                    pattern = "_ranked[0-9]_orient.pdb",
                                    replacement = "")
  
  Distance_data[[i]] <- subset(Distance_data[[i]], # Making sure the data is about the right prot
                               enzyme == i)
  
  
  Distance_data[[i]]$ligand <- gsub(Distance_data[[i]]$ligand,
                                    pattern = ".pdb",
                                    replacement = "")
  
  Distance_data[[i]]$ligand <- gsub(Distance_data[[i]]$ligand,
                                    pattern = paste0(i,
                                                     "_ranked[0-9]_orient_pocket_[0-9]_vs_"),
                                    replacement = "")
  
  Distance_data[[i]]$ligand_id <- gsub(Distance_data[[i]]$ligand_id,
                                       pattern = "#",
                                       replacement = "")
  
  Distance_data[[i]]$ligand_id <- gsub(Distance_data[[i]]$ligand_id,
                                       pattern = "/\\?",
                                       replacement = "")
  
  Distance_data[[i]] <- separate(data = Distance_data[[i]],
                                 col = ligand_id,
                                 into = c("ligand_id",
                                          "model"),
                                 sep ="\\.")
  
  Distance_data[["all"]] <- rbind(Distance_data[["all"]], # adding to previous data
                                  Distance_data[[i]]) # data for i
  
}
rm(i)

# Checking numeric
Distance_data[["all"]]$dist_SER..C <- as.numeric(Distance_data[["all"]]$dist_SER..C) 

# Checking factors
Distance_data[["all"]]$enzyme <- factor(Distance_data[["all"]]$enzyme,
                                        levels = names(Plots[["Labels"]][["enzymes"]]))

Distance_data[["all"]]$model <-  as.factor(as.numeric(Distance_data[["all"]]$model))

###################################################################
#### Affinity x Distance plot #### ####
Plots[["affinity_x_distance"]][["data"]] <- merge(Distance_data[["all"]],
                                                  Vina[["results"]],
                                                  all = F)

Plots[["affinity_x_distance"]][["data"]]$ligand <- factor(Plots[["affinity_x_distance"]][["data"]]$ligand,
                                                          levels = unique(Parameters$Ligands_infos$ligand))

Plots[["affinity_x_distance"]][["data"]] <- merge(Plots[["affinity_x_distance"]][["data"]],
                                                  Plots[["affinity_x_distance"]][["data"]] %>%
                                                    group_by(gene,
                                                             allele,
                                                             ligand,
                                                             species) %>%
                                                    summarise(max_affinity = max(affinity),
                                                              min_affinity = min(affinity),
                                                              max_dist_SER..C = max(dist_SER..C),
                                                              min_dist_SER..C = min(dist_SER..C)))

Plots[["affinity_x_distance"]][["data"]]$scaled_affinity <- ((Plots[["affinity_x_distance"]][["data"]]$affinity -
                                                                Plots[["affinity_x_distance"]][["data"]]$min_affinity) /
                                                               (Plots[["affinity_x_distance"]][["data"]]$max_affinity -
                                                                  Plots[["affinity_x_distance"]][["data"]]$min_affinity))



Plots[["affinity_x_distance"]][["data"]] <- merge(Plots[["affinity_x_distance"]][["data"]],
                                                  Plots[["affinity_x_distance"]][["data"]] %>%
                                                    group_by(gene,
                                                             allele,
                                                             ligand,
                                                             species) %>%
                                                    summarise(max_dist_SER..C = max(dist_SER..C),
                                                              min_dist_SER..C = min(dist_SER..C),
                                                              max_dist_SER..C = max(dist_SER..C),
                                                              min_dist_SER..C = min(dist_SER..C)))

Plots[["affinity_x_distance"]][["data"]]$scaled_dist_SER..C <- ((Plots[["affinity_x_distance"]][["data"]]$dist_SER..C -
                                                                   Plots[["affinity_x_distance"]][["data"]]$min_dist_SER..C) /
                                                                  (Plots[["affinity_x_distance"]][["data"]]$max_dist_SER..C -
                                                                     Plots[["affinity_x_distance"]][["data"]]$min_dist_SER..C))


Plots[["affinity_x_distance"]][["data"]]$affinity_dist_score <-  ((Plots[["affinity_x_distance"]][["data"]]$scaled_affinity +
                                                                     Plots[["affinity_x_distance"]][["data"]]$scaled_dist_SER..C) / 
                                                                    2)



Plots[["affinity_x_distance"]][["data"]] <- merge(Plots[["affinity_x_distance"]][["data"]],
                                                  Plots[["affinity_x_distance"]][["data"]] %>%
                                                    group_by(gene,
                                                             allele,
                                                             ligand,
                                                             species) %>%
                                                    summarise(min_affinity_dist_score = min(affinity_dist_score)))

# Plotting
Plots[["affinity_x_distance"]][["plot"]] <- ggplot(data = Plots[["affinity_x_distance"]][["data"]],# %>%
                                                     #subset(dist_SER..C <= 4),
                                                   aes(x = dist_SER..C,
                                                       y = affinity))+
  geom_point(aes(color = chain_name,
                 shape = term_function),
             alpha = 0.75)+
  #facet_grid(cols = vars(gene),
  #           rows = vars(species))+
  facet_wrap(facets = . ~ ligand + enzyme,
             ncol = length(unique(Plots[["affinity_x_distance"]][["data"]]$enzyme)))+
  #facet_grid(cols = vars(ligand),
  #           rows = vars(enzyme))+
  # scale_color_brewer(palette = "Dark2")+
  scale_shape_manual(values = c(17,
                                16))

Plots[["affinity_x_distance"]][["plot"]]


# Minimym affinity with 4.1 of the SER plot
# Preparing data
Plots[["lowest_aff_x_<=4A"]][["data"]] <- merge(Plots[["affinity_x_distance"]][["data"]],
                                                  Plots[["affinity_x_distance"]][["data"]] %>%
                                                    subset(dist_SER..C <= 4) %>%
                                                    group_by(gene,
                                                             allele,
                                                             ligand,
                                                             species) %>%
                                                    summarise(min_4A_aff = min(affinity))) %>%
  subset(affinity == min_4A_aff)

# Plotting
Plots[["lowest_aff_x_<=4A"]][["plot"]] <- ggplot(data = Plots[["lowest_aff_x_<=4A"]][["data"]],
                                                   aes(x = chain_name,
                                                       y = affinity))+
  geom_point(aes(color = term_function,
                 shape = term_function),
             alpha = 0.75)+
  facet_wrap(. ~ gene + allele + species,
             ncol = length(unique(Plots[["affinity_x_distance"]][["data"]]$enzyme)),
             scales = "free_x")+
  scale_color_brewer(palette = "Dark2")+
  scale_shape_manual(values = c(17,
                                16))

Plots[["lowest_aff_x_<=4A"]][["plot"]]

#### Affinities Boxplots for all models (without clases) and CXE24 #### ####
ggplot(data = Plots[["affinity_x_distance"]][["data"]] %>%
         filter(#dist_SER..C <= 4 &
                  gene == "CXE24" &
                  species == "Hsub"),
       aes(x = allele,
           y = affinity))+
  geom_boxplot(aes(fill = allele),
               position = position_dodge(0.9))+
  stat_summary(geom = "crossbar",
               aes(group = allele),
               fun = mean,
               fun.min = mean,
               fun.max = mean,
               width = 0.5,
               position = position_dodge(0.9))+
  facet_grid(rows = vars(gene),
             cols = vars(ligand),
             scales = "free",
             space = "free")+
  scale_fill_manual(values = c("#1b9e77",
                               "#e7298a",
                               "#d95f02",
                               "#7570b3"))+
  scale_shape_manual(values = c(10,
                                9,
                                7,
                                12))+
  scale_y_continuous(breaks = seq(from = -10,
                                  to = 0,
                                  by = 0.5))+
  scale_x_discrete(labels = 
                     Plots[["Labels"]][["allele"]]
                   #  c(wt = "wild-type", del28 = "deletion 28", del31 = "deletion 31", HsubTED_trunc = "deletion TE")
  )+
  labs(fill = "Allele",
       y = expression(paste("affinity (",
                            kcal.mol^{-1},
                            ")",
                            sep = "")))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1),
        strip.background = element_blank(),
        strip.text.x = element_text(face = "italic"), 
        axis.title.x = element_blank(),
        #legend.position = "none",
        legend.position = "bottom",
        panel.grid.minor = element_blank())

###################################################################
#### Generating the script to generate the figure with selected models #### ####
# Setting the paramters for the script 
Parameters[["ChimeraX Figures"]] <- 
  merge(Distance_data[["all"]] %>%
          subset(grepl(Distance_data[["all"]]$ligand,
                       pattern = "Ac")),  # by keeping Distance_data
        Plots[["lowest_aff_x_<=4A"]][["data"]] %>% # that merges with model from the lowest_aff_x_<=4A
          select(enzyme,
                 ligand,
                 model))

Parameters[["ChimeraX Figures"]] <- 
  merge(Parameters[["ChimeraX Figures"]],
        unique(Parameters[["Distances"]] %>%
                 select(ligand,
                        C_pos)),
        all = F) %>%
  select(enzyme,
         prot_AA,
         prot_AA_pos,
         ligand,
         ligand_id,
         model,
         C_pos,
         dist_SER..C)

Parameters[["ChimeraX Figures"]] <- 
  Parameters[["ChimeraX Figures"]] %>%
  arrange(enzyme,
          ligand,
          ligand_id,
          model)

figure_script <- c() # creating an empty vector to receive figure script

# Building the script
for(i in Parameters[["enzyme"]]){
  figure_script <- c(figure_script,
                     c(paste0('open "C:/Pro/Projets/Heliothis Acetate production/CXE24_docking/ChimeraX/', # opening previous Chimera X session
                              i,
                              "/",
                              i,
                              '_orient_pdbqt_w_distances.cxs"'),
                       paste0('cd "C:/Pro/Projets/Heliothis Acetate production/CXE24_docking/ChimeraX/', # setting the working directory
                              i,
                              '/"'),
                       "hide #2-4 models", # hiding models of for ligands
                       "hide #5-7 models", # hiding clashes and distances
                       if(length((Parameters[["ChimeraX Figures"]] %>%
                                  subset(enzyme == i))$enzyme) > 0){
                         paste0("show #", # showing models with the min dist_SER..C
                                (Parameters[["ChimeraX Figures"]] %>%
                                   subset(enzyme == i))$ligand_id,
                                ".",
                                (Parameters[["ChimeraX Figures"]] %>%
                                   subset(enzyme == i))$model,
                                " models")
                       },
                       "set bgColor white", # setting backgound color
                       #"color sequential residues palette #7570B3:#1B9E77", # coloring prot
                       "color #1 silver ribbons", # setting the color of the protein ribbons
                       paste0("name important_residues #1:", # naming the selection of important residues
                              paste((Parameters[["relevant_residues"]] %>%
                                       subset(enzyme == i))$prot_AA_pos,
                                    collapse = ",")),
                       "show important_residues atoms", # showing atom for those residues
                       "color important_residues #E7298A", # coloring important_residues
                       "color #2 #7570B3", # coloring ligands
                       "color #3 #1B9E77", # coloring ligands
                       "color #4 #D95F02", # coloring ligands
                       "color byhetero", # coloring atoms shown
                       "show #1 surface", # displaying the surface
                       "color #1 silver surface transparency 66", # setting the surfance color and transparency
                       "graphics silhouettes true", # displaying silhouettes
                       if(i == "HvirCXE24_noSP"){# hiding cartoons of parts of the prot badly modelised
                         c("hide #1:1-11 cartoons",
                         "hide #1:1-11 surface")
                       } else if(i == "HsubCXE5_noSP"){
                         c("hide #1:1-17,547 cartoons",
                         "hide #1:1-17,547 surface")
                       } else if(i == "HsubCXE16_noSP"){
                         c("hide #1:1-14,548-551 cartoons",
                         "hide #1:1-14,548-551 surface")
                       } else if(i == "HsubCXE24_allele1_noSP"){
                         c("hide #1:1-8 cartoons",
                         "hide #1:1-8 surface")
                       } else if(i == "HsubCXE24_del28_noSP"){# hiding cartoos of part of the prot badly modelised
                         c("hide #1:1-8,392-395 cartoons", # "hide #1:1-8,354-395 cartoons"
                         "hide #1:1-8,392-395 surface") # "hide #1:1-8,354-395 surface"
                       } else if(i == "HsubCXE24_del31_noSP"){
                         c("hide #1:1-8,391-394 cartoons", # "hide #1:1-8,354-394 cartoons"
                         "hide #1:1-8,391-394 surface") # "hide #1:1-8,354-394 surface"
                       } else if(i == "HsubCXE24_HsubTED_trunc_noSP"){
                         c("hide #1:1-8 cartoons", # "hide #7:1-8,357-368 cartoons"
                         "hide #1:1-8 surface") # "hide #7:1-8,357-368 surface"
                       } ,
                       "view orient", # orienting view
                       "turn y -137 coordinateSystem #1",
                       "zoom 1.3",
                       paste0('save "', # saving a screenshot
                              i,
                              '_orient_front.png" width 1440 height 1080 supersample 10'),
                       "movie record supersample 3 limit 540 size 1440,1080", # starting the recording of a movie
                       "wait 30", # waiting to have time to read the labels
                       "label delete", # removing labels
                       "wait 15", # waiting before turning
                       "turn y 1 360 center #1 coordinateSystem #1", # turning the prot 360°
                       "wait 390", # waiting to be sure that the turn is complete
                       "wait 90",
                       paste0('movie encode output "', # saving the movie
                              i,
                              '_orient_front_movie.mp4" format h264 framerate 30 wait true'),
                       "view orient", # orienting view
                       "turn x 90 coordinateSystem #1",
                       "turn y -90 coordinateSystem #1",
                       "zoom 1.2",
                       paste0('save "', # saving a screenshot
                              i,
                              '_orient_top.png" width 1440 height 1080 supersample 10'),
                       paste0("sel zone #1:", # selecting on 
                              Parameters[["relevant_residues"]] %>%  # SER
                                filter(enzyme == i &
                                         prot_AA == "SER") %>%
                                pull(var = "prot_AA_pos"), # relevant residues
                              if(length((Parameters[["ChimeraX Figures"]] %>%
                                         subset(enzyme == i))$enzyme) > 0){
                                paste0(" ",
                                         paste0("#", # and O of the Ac
                                                    subset(Parameters[["ChimeraX Figures"]],
                                                           enzyme == i &
                                                             ligand %in% unique(grep(x = Parameters[["ChimeraX Figures"]]$ligand,
                                                                                     pattern = "Ac",
                                                                                     value = T)))$ligand_id,
                                                    ".",
                                                    subset(Parameters[["ChimeraX Figures"]],
                                                           enzyme == i &
                                                             ligand %in% unique(grep(x = Parameters[["ChimeraX Figures"]]$ligand,
                                                                                     pattern = "Ac",
                                                                                     value = T)))$model,
                                                    "/?:1@@serial_number=",
                                                    subset(Parameters[["ChimeraX Figures"]],
                                                           enzyme == i &
                                                             ligand %in% unique(grep(x = Parameters[["ChimeraX Figures"]]$ligand,
                                                                                     pattern = "Ac",
                                                                                     value = T)))$C_pos,
                                                    collapse = " ")
                                       )
                              },
                              " 8",
                              collapse = " "),
                       "view sel orient",
                       "sel clear",
                       "turn x 46 coordinateSystem #1",
                       "turn y -137 coordinateSystem #1",
                       paste0('save "',
                              i,
                              '_orient_zoom.png" width 1440 height 1080 supersample 10'),
                       "movie record supersample 3 limit 450 size 1440,1080",
                       "wait 30", # waiting to have time to read the labels
                       #"label delete", # removing labels
                       "wait 15", # waiting before turning
                       paste0('turn y 1 360 center #1:', # turning prot with center of rotation based on
                              paste0(Parameters[["relevant_residues"]] %>%  # relevant residues
                                       filter(enzyme == i &
                                                prot_AA == "SER") %>%
                                       pull(var = "prot_AA_pos"),
                                     "@OG"),
                              if(length((Parameters[["ChimeraX Figures"]] %>%
                                         subset(enzyme == i))$enzyme) > 0){
                                paste0(" ",
                                       paste(paste0("#", # and O of the Ac
                                                    subset(Parameters[["ChimeraX Figures"]],
                                                           enzyme == i &
                                                             ligand %in% unique(grep(x = Parameters[["ChimeraX Figures"]]$ligand,
                                                                                     pattern = "Ac",
                                                                                     value = T)))$ligand_id,
                                                    ".",
                                                    subset(Parameters[["ChimeraX Figures"]],
                                                           enzyme == i &
                                                             ligand %in% unique(grep(x = Parameters[["ChimeraX Figures"]]$ligand,
                                                                                     pattern = "Ac",
                                                                                     value = T)))$model,
                                                    "/?:1@@serial_number=",
                                                    subset(Parameters[["ChimeraX Figures"]],
                                                           enzyme == i &
                                                             ligand %in% unique(grep(x = Parameters[["ChimeraX Figures"]]$ligand,
                                                                                     pattern = "Ac",
                                                                                     value = T)))$C_pos,
                                                    collapse = " "),
                                             if(length(subset(Parameters[["ChimeraX Figures"]], # if there is OH ligands
                                                              enzyme == i &
                                                              grepl(x = Parameters[["ChimeraX Figures"]]$ligand,
                                                                    pattern = "OH"))$ligand) >0 ){
                                               paste0("#", # and the OH ligands
                                                      subset(Parameters[["ChimeraX Figures"]],
                                                             enzyme == i &
                                                               ligand %in% unique(grep(x = Parameters[["ChimeraX Figures"]]$ligand,
                                                                                       pattern = "OH",
                                                                                       value = T)))$ligand_id,
                                                      ".",
                                                      subset(Parameters[["ChimeraX Figures"]],
                                                             enzyme == i &
                                                               ligand %in% unique(grep(x = Parameters[["ChimeraX Figures"]]$ligand,
                                                                                       pattern = "OH",
                                                                                       value = T)))$model,
                                                      "/?:1@O",
                                                      collapse = " ")
                                             }
                                             ,
                                             collapse = " "))
                              },
                              "coordinateSystem #1"),
                       "wait 390", # waiting to be sure that the turn is complete
                      "wait 90",
                       paste0('movie encode output "', # saving the movie
                              i,
                              '_orient_zoom_movie.mp4" format h264 framerate 30 wait true'),
                      "view orient",
                      paste0('save "', # saving the session
                              i,
                              '_orient_pdbqt_for_figure.cxs"'),
                       "close session"))
}
rm(i)

figure_script <-
  c(figure_script,
    "quit")

write(x = figure_script,
      file = "./ChimeraX/3_figure_script.cxc")


#### /!\ Run the 3_figure_script.cxc in Chimera X #### #########
shell.exec(paste0('"',
                  getwd(),
                  '/ChimeraX/"'))
