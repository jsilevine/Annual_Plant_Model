
## code for the class: simulation

## define main simulate function

simulate.stand <- function(max_t, timestep, dnot, plot.area, full.allom = TRUE, avgs = TRUE) {

  if (!is.numeric(max_t)) {
    print("Error: max_t must be numeric")
    return()
  }

  if(!is.numeric(timestep)) {
    print("Error: timestep must be numeric")
    return()
  }

  if(!is.numeric(dnot)) {
    print("Error: dnot must be numeric")
    return()
  }

  if(!is.numeric(plot.area)) {
    print("Error: plot.area must be numeric")
    return()
  }

  if(timestep > max_t) {
    print("Error: timestep cannot be larger than max_t")
    return()
  }

  if(!is.logical(full.allom)) {
    print("Error: full.allom must be TRUE/FALSE")
    return()
  }

  ## initialize data list
  data <- list(diameter = matrix(1, nrow = cohort_density * plot.area, ncol = max_t/timestep+1),
               crown.class = matrix(1, nrow = cohort_density * plot.area, ncol = max_t/timestep+1),
               diameter.growth = matrix(1, nrow = cohort_density * plot.area, ncol = max_t/timestep))

  data[["diameter"]][,1] <- dnot
  A_c <- calc.A(crown.class = 1)
  A_u <- calc.A(crown.class = 2)

  ## loop over days in simulation
  for (i in seq(1, max_t, by = timestep)) {

    ## first, kill plants
    mu <- vector(length=nrow(data[["diameter"]]))
    mu[data[["crown.class"]][,i]==1] <- rbinom(sum(data[["crown.class"]][,i]==1),1,mu_c)
    mu[data[["crown.class"]][,i]==2] <- rbinom(sum(data[["crown.class"]][,i]==2),1,mu_u)
    data[["diameter"]] <- data[["diameter"]][mu==0,]
    data[["crown.class"]] <- data[["crown.class"]][mu==0,]
    data[["diameter.growth"]] <- data[["diameter.growth"]][mu==0,]

    ## grow plants
    ## calculate diameter growth for overstory and understory
    data[["diameter.growth"]][data[["crown.class"]][,i]==1,i] <- sapply(data[["diameter"]][data[["crown.class"]][,i]==1,i],
                                                                        calc.dd, l=l_c, r=r_c, A=A_c, simplify = "vector")
    if (any(data[["crown.class"]][,i]==2)) {
      data[["diameter.growth"]][data[["crown.class"]][,i]==2,i] <- sapply(data[["diameter"]][data[["crown.class"]][,i]==2,i],
                                                                        calc.dd, l=l_u, r=r_u, A=A_u, simplify = "vector")
    }

    ## calculate diameter for next timestep
    data[["diameter"]][,i+1] <- data[["diameter"]][,i] + data[["diameter.growth"]][,i]

    ## canopy class reassignment
    CA <- sum(alpha_w*((data[["diameter"]][,i+1])^gamma))
    if(CA<=plot.area) {
      data[["crown.class"]][,i+1] <- 1
    }
    else {
      ## order data from largest to smallest
      data[["diameter"]] <- data[["diameter"]][order(data[["diameter"]][,i+1]),]
      data[["crown.class"]] <- data[["crown.class"]][order(data[["diameter"]][,i+1]),]
      data[["diameter.growth"]] <- data[["diameter.growth"]][order(data[["diameter"]][,i+1]),]
      ## create vector of cumulative CA sums, used to determine which trees to add, remove from canopy
      ca.sum <- vector(length=nrow(data[["diameter"]]), mode = "numeric")
      for (j in 1:nrow(data[["diameter"]])) {
        ca.sum[j] <- sum((alpha_w*data[["diameter"]][,i+1]^gamma)[1:j])
      }
      ## if gap in canopy trees, add understory trees to canopy and vis versa
      data[["crown.class"]][ca.sum <= plot.area, i+1] <- 1
      data[["crown.class"]][ca.sum > plot.area, i+1] <- 2
    }
  }
  data[["max_t"]] <- max_t
  data[["timestep"]] <- timestep
  data[["plot.area"]] <- plot.area

  class(data) <- "simulation"
  if (full.allom) {
    data <- calculate.allometry(data)
  }

  if(avgs) {
    data[["avgs"]] <- simulation_averages(data)
  }

  return(data)
}

## define is function
is.simulation <- function(x) inherits(x, "simulation")

## define format and summary methods for simulate.stand
## want it to print: mean and sd for diameter, CA, root SA, height, structural biomass
##                   proportion of trees in overstory vs understory
##

format.simulation <- function(x) {

}


summary.simulation <- function(simulation) {

}


##plot.simulation <- function(simulation) {
##  timestep <- seq(1, simulation[["max_t"]]+1, by = timestep)

##  diameter.plot <- ggplot(aes(x=timestep,y=), color = ))


##}
