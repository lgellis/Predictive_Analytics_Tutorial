
# If you are using local R install, you can use the following line of code to import
# hr = read.table("/filepath/HR_comma_sep.csv", head=TRUE, sep=",")



#rename data frame
hr <- df.data.1 

#view columns
names(hr)

#quick view
head(hr)

#dim shows number of rows and columns
dim(hr)

#str shows the structure of each column including the type of data and sample values
str(hr)

#show summary statistics for each column.  For numeric: min, max, median, mean, 2st and 3rd quartile.  For factors show the counts of each.
summary(hr)


