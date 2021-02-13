
data <- read.csv('annual_generation_state.csv', sep = ',', header = TRUE)
library(stringr)
names(data) <- str_replace_all(names(data), c(' ' = ''))
data$GENERATION..Megawatthours. <- as.numeric(data$GENERATION..Megawatthours.)
print(names(data))
head(data$GENERATION..Megawatthours.)
