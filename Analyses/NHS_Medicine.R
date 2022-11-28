#get total issued hospital medicices in hospitals
#for molnupiravir and sotrovimab

library(stringr)
library(dplyr)

nov2021 <- read.csv("~/Desktop/Imperial/covid/new_project_antivirals/medicines_NHS_hospital/scmd_202111.csv")
dez2021 <- read.csv("~/Desktop/Imperial/covid/new_project_antivirals/medicines_NHS_hospital/scmd_202112.csv")
jan2022 <- read.csv("~/Desktop/Imperial/covid/new_project_antivirals/medicines_NHS_hospital/scmd_202201.csv")
fev2022 <- read.csv("~/Desktop/Imperial/covid/new_project_antivirals/medicines_NHS_hospital/scmd_202202.csv")
mar2022 <- read.csv("~/Desktop/Imperial/covid/new_project_antivirals/medicines_NHS_hospital/scmd_202203.csv")
apr2022 <- read.csv("~/Desktop/Imperial/covid/new_project_antivirals/medicines_NHS_hospital/scmd_202204.csv")
may2022 <- read.csv("~/Desktop/Imperial/covid/new_project_antivirals/medicines_NHS_hospital/scmd_202205.csv")
jun2022 <- read.csv("~/Desktop/Imperial/covid/new_project_antivirals/medicines_NHS_hospital/scmdv3_202206.csv")
jul2022 <- read.csv("~/Desktop/Imperial/covid/new_project_antivirals/medicines_NHS_hospital/scmd_202207.csv")

all_data <- rbind(nov2021, dez2021, jan2022, fev2022, mar2022,
                  apr2022, may2022, jun2022, jul2022)

#molnupiravir by month
molnupiravir <- all_data %>%
  filter(str_detect(VMP_PRODUCT_NAME , "Molnupiravir"))

#sotrovimab by month
sotrovimab <- all_data %>%
  filter(str_detect(VMP_PRODUCT_NAME , "Sotrovimab"))



#filter quantity by YEAR_MONTH

molnupiravir_quantity <- molnupiravir %>%
  group_by(YEAR_MONTH) %>%
  summarize(total = sum(TOTAL_QUANITY_IN_VMP_UNIT))

sotrovimab_quantity <- sotrovimab %>%
  group_by(YEAR_MONTH) %>%
  summarize(total = sum(TOTAL_QUANITY_IN_VMP_UNIT))


