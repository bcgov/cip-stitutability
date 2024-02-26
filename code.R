library(tidyverse)
library(here)
library(vroom)
library(janitor)


cip_noc <- vroom::vroom(here("data", "cip_noc.csv"), skip= 13, n_max = 436) #garbage before and after data
colnames(cip_noc)[1] <- "Field of Study" #missing column name
cip_noc <- cip_noc[-1,] #garbage in first row

fix_data <- function(vctr){
  vctr|>
    str_remove_all(",")|>
    as.numeric()
}

mat <- cip_noc|>
  janitor::remove_constant()|>
  mutate(across(-"Field of Study", fix_data))|>
  column_to_rownames("Field of Study")|>
  t()

tbbl <- cor(mat, method = "spearman")|>
  as.data.frame()|>
  rownames_to_column("Field of Study")|>
  as_tibble()|>
  pivot_longer(cols=-"Field of Study", names_to = "Field of Study 2", values_to = "Correlation")|>
  filter(`Field of Study`!=`Field of Study 2`)|>
  na.omit()|>
  group_by(`Field of Study`)|>
  nest()|>
  mutate(data=map(data, slice_max, Correlation, n=10))|>
  unnest(data)|>
  mutate(fos_code=str_sub(`Field of Study`, 1,2),
         fos2_code=str_sub(`Field of Study 2`, 1, 2),
         same_broad=if_else(fos_code==fos2_code, "same", "different"))

#save the objects----------------------

mat|>
  as.data.frame()|>
  rownames_to_column("NOC")|>
  as_tibble()|>
  write_csv(here("data","cip_noc_processed.csv"))

tbbl|>
  write_csv(here("data","correlations.csv"))



