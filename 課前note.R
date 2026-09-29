library(tidyverse)
library(MASS)

data(mtcars)
mtcars

mtcars$mpg=1

pull(mtcars,mpg)==1

#永遠不要用&去提data，用pull，然後記得賦值用==而不是=

mtcars |> 
  dplyr::select(mpg:disp)