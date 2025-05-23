
setwd("D:/SZ")
library(Tax4Fun2)

runRefBlast(path_to_otus = 'modified_sequences_bac.fasta', path_to_reference_data = './Tax4Fun2_ReferenceData_v2', path_to_temp_folder = 'bac_Ref99NR', database_mode = 'Ref99NR', use_force = TRUE, num_threads = 14)

makeFunctionalPrediction(path_to_otu_table = 'ASV-bac.txt', path_to_reference_data = './Tax4Fun2_ReferenceData_v2', path_to_temp_folder = 'bac_Ref99NR', database_mode = 'Ref99NR', normalize_by_copy_number = TRUE, min_identity_to_reference = 0.97, normalize_pathways = FALSE)

calculateFunctionalRedundancy(path_to_otu_table ='ASV-bac.txt', path_to_reference_data = './Tax4Fun2_ReferenceData_v2', 
                              path_to_temp_folder = 'bac_Ref99NR', database_mode = 'Ref99NR', min_identity_to_reference = 0.97)
