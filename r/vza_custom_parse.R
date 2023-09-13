library(tidyverse)

hr <- read_csv("data/vza_hamptonroads.csv") |> 
  select(id, jurisdiction, abbrvname, overlay, customfielddata)

hr_custom <- hr |> 
  separate(customfielddata, into = c("min1", "min2", "min3", "min4", "sfd", "thonly"), sep = ',') |> 
  mutate(across(.fns = ~str_remove_all(.x, '"')),
         across(.fns = ~str_remove_all(.x, "\\{")),
         across(.fns = ~str_remove_all(.x, "\\}")),
         across(.fns = ~str_remove_all(.x, "41_116:")),
         across(.fns = ~str_remove_all(.x, '41_117:')),
         across(.fns = ~str_remove_all(.x, '41_118:')),
         across(.fns = ~str_remove_all(.x, '41_119:')),
         across(.fns = ~str_remove_all(.x, '45_121:')),
         across(.fns = ~str_remove_all(.x, '45_122:'))) |> 
  mutate(across(.fns = ~str_trim(.x)))
  

write_csv(hr_custom, "data/hr_custom_fields.csv")
