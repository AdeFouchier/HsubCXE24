#### Libraries ####
library(tidyverse)
library(readxl)
library(gridExtra)
library(cowplot)


#### Preparing objects ####
# Various parameters
Parameters <- list() # creating list to receive data

Parameters[["enzyme"]] <- c(#"HvirCXE24_noSP", # list of enzymes to deal with
                            #"HsubCXE5_noSP",
                            #"HsubCXE16_noSP",
                            "HsubCXE24_allele1_noSP",
                            "HsubCXE24_del28_noSP",
                            "HsubCXE24_del31_noSP",
                            "HsubCXE24_HsubTED_trunc_noSP")

Parameters[["Hbonds_cata_cols"]] <- c("enzyme", # Columns of the future data
                                      "prot_AA",
                                      "prot_AA_pos",
                                      "ligand",
                                      "ligand_id",
                                      "model",
                                      "dist_D..A",
                                      "dist_DH..A")

Parameters[["Clashes_cata_cols"]] <- c("enzyme", # Columns of the future data
                                       "ligand",
                                       "ligand_id",
                                       "model",
                                       "overlap",
                                       "distance")

Parameters[["Vina_data_cols"]] <- c("enzyme",
                                    "ligand",
                                    "model",
                                    "result",
                                    "inter",
                                    "intra",
                                    "inter_intra",
                                    "unbound")

# Vina analysis related
Vina <- list() # creating list to receive data

# Hbonds analysis related
Hbonds_data <- list() # creating list to receive data

# Clashes analysis related
Clashes_data <- list() # creating list to receive data

# Distances analysis related
Parameters[["Distance_data_cols"]] <- c("enzyme", # Columns of the future data
                                        "prot_AA",
                                        "prot_AA_pos",
                                        "ligand",
                                        "ligand_id",
                                        "model",
                                        "dist_SER..C")

Distance_data <- list() # creating list to receive data

# Plotting related
Plots <- list()

# Preparing label for plotting
Plots[["Labels"]][["enzymes"]] <- c("HvirCXE24",
                                    #"HsubCXE5",
                                    #"HsubCXE16",
                                    "HsubCXE24_allele1",
                                    "HsubCXE24_del28",
                                    "HsubCXE24_del31",
                                    "HsubCXE24_HsubTED_trunc")

names(Plots[["Labels"]][["enzymes"]]) <- c("HvirCXE24_noSP",
                                           #"HsubCXE5_noSP",
                                           #"HsubCXE16_noSP",
                                           "HsubCXE24_allele1_noSP",
                                           "HsubCXE24_del28_noSP",
                                           "HsubCXE24_del31_noSP",
                                           "HsubCXE24_HsubTED_trunc_noSP")

Plots[["Labels"]][["species"]] <- c("H. subflexa",
                                    "H. virescens")

names(Plots[["Labels"]][["species"]]) <- c("Hsub",
                                           "Hvir")

Plots[["Labels"]][["allele"]] <- c("wild-type",
                                   "deletion 28",
                                   "deletion 31",
                                   "deletion TE")

names(Plots[["Labels"]][["allele"]]) <- c("wt",
                                          "del28",
                                          "del31",
                                          "HsubTED_trunc")


#### Importing data ####
# Various paramters
Parameters[["relevant_residues"]] <- data.frame(readxl::read_excel(path = "./ChimeraX/Relevant_residues.xlsx",
                                                                   sheet = "data"))

Parameters[["Ligands_models_infos"]] <- data.frame(readxl::read_excel(path = "./ChimeraX/Ligands_infos.xlsx",
                                                                      sheet = "data"))

Parameters[["Ligands_infos"]] <- data.frame(readxl::read_excel(path = "./Ligands/Ligands_list.xlsx",
                                                               sheet = "infos"))

# Vina Related
Vina[["parameters"]] <- data.frame(readxl::read_excel(path = "./Vina/Enzyme_vs_pherobase/Enzyme_vs_pherobase-param_generator.xlsx",
                                                      sheet = "variables"))
