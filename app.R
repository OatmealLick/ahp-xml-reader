library(XML)

# set working dir to ~/Projects/R/ahp-first-steps
setwd("/home/lick/Projects/R/ahp-first-steps")

xml_data <- xmlTreeParse("data.xml")

xml_data <- xmlRoot(xml_data)
#print (xml_data$attr[[1]])

process_node <- function(node) {
  
  # extract ratios as list
  ratios = xmlElementsByTagName(node, 'comparisons')
  list_of_ratios <- as.numeric(xmlToList(ratios$comparisons))
  
  # get number of alternatives in curret node
  n <- as.numeric(ratios$comparisons$attr[[1]])
  
  # build comparison matrix
  ma <- matrix(data = 1, nrow = n, ncol = n)
  i <- 1
  for (row in seq(1,n-1)) {
    for (element in seq(row+1,n)) {
      ma[row,element] <- list_of_ratios[i]
      ma[element,row] <- 1 / list_of_ratios[i]
      i <- i + 1
    }
  }
  # print(ma)
  
  # get eigen vector for highest eigen value
  eigen_vec <- Re(eigen(ma)$vectors[,1])
  
  # DIY normalize vector
  eigen_vec <- eigen_vec / sum(eigen_vec)
  
  list_of_nodes = xmlElementsByTagName(node, 'criterium')
  
  # if there is more -> we aren't at the bottom yet
  if (length(list_of_nodes) != 0) {
    
    vec <- vector(mode = "list", length = length(list_of_nodes))
    
    # call recursively for every child
    i <- 1
    for(child_node in list_of_nodes) {
      vec[[i]] <- process_level(child_node)
      i <- i + 1
    }
    
    # allocate memory for vector (like in C)
    result_vec <- rep(0, length = length(vec[[1]]))
    
    # create result_vec which is previous priority vectors scaled by weight
    i <- 1
    for(alt in vec) {
      result_vec <- result_vec + alt * eigen_vec[i]
      i <- i + 1
    }
    
    # return with DIY normalization
    return(result_vec / sum(result_vec))
  
  } else { # it's rock bottom 

    # just return your priority vectors
    return (eigen_vec)
  }
}

process_node(xml_data)
