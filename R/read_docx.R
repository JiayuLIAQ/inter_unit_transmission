source("R/functions.R")
install.packages("officer")
library(officer)

path_docx <- data.table(path = list.files(no.. = FALSE, full.names = TRUE, recursive = TRUE,
                                         pattern = "\\.docx$"))
test <- read_docx(path_docx$path[1])