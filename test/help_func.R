### basic function ###

myMax<-function(p1,p2) {
  if (p1>p2)
    return (p1)
  else
    return (p2)
}

myMax(10,5)


### vectorized function ###

lapply(c(5,6), function(x){return (x+2)})

cumsum(1:100)
