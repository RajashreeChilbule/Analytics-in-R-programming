#----List----
#We  use list() function to create a list.

#We will create three different objects: vector, matrix and data frame.
#And will create a list with all these objects combined together.

#Creating a Vector with numeric from 1 up to 5
vect  <- 1:5

#Creating a 2x 5 matrix
mat  <- matrix(1:10, ncol = 5)
dim(mat)

# Creating a data frame by selecting the 10th row of the built-in R data set EuStockMarkets
df <- EuStockMarkets[1:10,]

# Construct list with these vec, mat, and df:
my_list <- list(vect, mat, df)
my_list

#Select Elements from List
#We need to use the [[index]] to select an element in a list. 
#The value inside the double square bracket represents the position of the item in a list we want to extract.
# Print second element of the list
my_list[[2]]
