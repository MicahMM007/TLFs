library(purrr)

numbers <- c(1,2,3,4,5)

#use purrr:map with lambda function to square each number
squared_numbers <- purrr::map(
  numbers,
  ~{
    .x ^ 2
  }
)

print(squared_numbers)
# --- end

square <- function(x){
  x^2
}

squared_numbers2 <- map(
  numbers,
  ~ square(.x)
)

print(squared_numbers2)
