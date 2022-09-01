if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, reticulate, rio , glue)

pacotes<-read.table("capitulos/motivacao/requirements_carol.txt") %>% 
  select(V1) %>% 
  pull()



pacotes<-pacotes[-33]

virtualenv_create("venv")
use_virtualenv("venv")
virtualenv_install("venv",pacotes)

