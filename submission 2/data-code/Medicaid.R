
# Meta --------------------------------------------------------------------

## Title:  Medicaid Expansion
## Author: Ian McCarthy
## Date Created: 12/9/2019
## Date Edited:  12/9/2019


# Preliminaries -----------------------------------------------------------
kff.dat <- read_csv('/Users/yaseenbhat/Downloads/econ470hw5/data/input/expansion-status-interactive-map_12.1.23.csv')

# Clean KFF data -------------------------------------------------------

kff.final <- kff.dat %>%
  mutate(expanded = (`Expansion Status` == 'Adopted and Implemented'),
         Description = str_replace_all(Description,c("\n"='','"'='')))

kff.final$splitvar <- kff.final %>% select(Description) %>% as.data.frame() %>%
  separate(Description, sep=" ", into=c(NA, NA, NA, "date"))

kff.final <- kff.final %>%
  mutate(date_adopted = mdy(splitvar$date)) %>%
  select(State, expanded, date_adopted)

write_tsv(kff.final,'data/output/medicaid_expansion.txt')
