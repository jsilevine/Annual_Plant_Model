## function to calculate full allometry for class simulation

calculate.allometry <- function(simulation) {

  if(!is.simulation(simulation)) {
    print("Error: input must be of class simulation")
    return()
  }

  simulation[["height"]] <- H*(simulation[["diameter"]]^(gamma-1))
  simulation[["crown_area"]] <- alpha_w*(simulation[["diameter"]]^gamma)
  simulation[["struct_biomass"]] <- alpha_s*(simulation[["diameter"]]^(gamma+1))
  simulation[["root_sa"]] <- matrix(1, dim=dim(simulation[["diameter"]]))
  simulation[["root_sa"]][simulation[["crown.area"]]==1,] <- simulation[["crown_area"]]*r_c
  simulation[["root_sa"]][simulation[["crown.area"]]==2,] <- simulation[["crown_area"]]*r_u

  return(simulation)
}
