



install.packages("XML")
library("XML")

# u = "http://en.wikipedia.org/wiki/World_population"
u = "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"

tables = readHTMLTable(u)
names(tables)

tables[[2]]
# Print the table. Note that the values are all characters
# not numbers. Also the column names have a preceding X since
# R doesn't allow the variable names to start with digits.
tmp = tables[[2]]


doc <- "http://www.nber.org/cycles/cyclesmain.html"
# The  main table is the second one because it's embedded in the page table.
tables <- getNodeSet(htmlParse(doc), "//table")
xt <- readHTMLTable(tables[[2]],
                    header = c("peak","trough","contraction",
                               "expansion","trough2trough","peak2peak"),
                    colClasses = c("character","character","character",
                                   "character","character","character"),
                    trim = TRUE, stringsAsFactors = FALSE
)