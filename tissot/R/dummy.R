
# Create a function to create an object of class
student <- function(n, a, r){
  value <- list(name = n, age = a, rno = r)
  attr(value, "class") <- student
  value
}

# Method for generic function print()
print.student <- function(obj){
  cat(obj$name, "\n")
  cat(obj$age, "\n")
  cat(obj$rno, "\n")
}
