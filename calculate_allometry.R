## This file contains functions related to allometric scaling of the plants

## function to create lists of allometric constants

create_allometry <- function(n.spp, custom) {
  ## allometry will be a dataframe of allometric variables, with rows for vars and columns for spp
  ## this function also assigns values for non-allometric, but relevant variables. Not sure if should be in separate file

  ## for now, almost all numbers will be same for all species, some will be drawn from random dist.
  out <- data.frame(nrow = 11, ncol = n.spp+1)
  colnames(out) <- c("var", as.character(seq(1, n.spp, by = 1)))
  out[,1] <- c("l_c", "l_u", "r_c", "r_u", "mu_c", "mu_u", "r", "H", "alpha_s", "alpha_w", "gamma", "c_lb",
               "c_rb", "c_bg", "tau_l", "tau_r", "p_r", "p_l", "p_sw", "c_l", "c_r", "V", "k", "L_0", "a_f")

  ## function accepts a list of named vectors for each species with custom allometric constants
  if(exists(custom)) {
    if(class(custom) != "list") {
      print("Error: custom variables must be a list of named numeric vectors")
      return()
    }
    list_classes <- lapply(custom, class)
    else if(any(list_classes != "numeric")) {
      print("Error: custom variables must be a list of named numeric vectors")
      return()
    }
    list_names <- lapply(custom, names)
    else if(any(is.null(list_names))) {
      print("Error: custom variables must be a list of named numeric vectors")
      return()
    }
  }

  for (spp in 1:n.spp) {
    #### allometric variables
    out[var = "l_c", spp] <- 4               ## leaf area index of tree canopy tree
    out[var = "l_u", spp] <- 1               ## leaf are index of understory tree
    out[var = "r_c", spp] <- 6.5
    out[var = "r_u", spp] <- 2
    out[var = "mu_c", spp] <- 0.005
    out[var = "mu_u", spp] <- 0.015
    out[var = "r", spp] <- 6.5             ## fine-root surface area per unit crown area
    out[var = "H", spp] <- 3.6             ## allometric constant for height
    out[var = "alpha_s", spp] <- 0.0815        ## allometric constant for sapwood
    out[var = "alpha_w", spp] <- 0.20          ## allometric constant for crown area
    out[var = "gamma", spp] <- 1.5         ## allometric exponent

    #### carbon accumulation and allocation variables
    out[var = "c_lb", spp] <- 0.07656      ## cost of building leaf biomass per LAI
    out[var = "c_rb", spp] <- 0.02933      ## cost of builing root biomass per LAI
    out[var = "c_bg", spp] <- 0.2          ## building cost of structural biomass
    out[var = "tau_l", spp] <- 1           ## average lifetime of a unit carbon in leaf
    out[var = "tau_r", spp] <- 2           ## average lifetime of a unit carbon in roots
    out[var = "p_r", spp] <- 0.02933       ## respiration rate of roots per unit surface area
    out[var = "p_l", spp] <- 0.0638        ## respiration rate of leaves per unit surface area
    out[var = "p_sw", spp] <- 0.0466       ## respiration rate of sapwood
    out[var = "c_l", spp] <- (c_lb/tau_l) + p_l + p_sw ## cost of building and maintaining leaf biomass per unit LAI
    out[var = "c_r ", spp]<- c_rb/tau_r + p_r       ## cost of building and maintaining root biomass per unit RAI
    out[var = "V", spp] <- 0.6             ## maximum rate of carbon fixation - kgC/m^2/day
    out[var = "k", spp] <- 0.33            ## light extinction coefficient
    out[var = "L_0", spp] <- 1200          ## light level above highest canopy
    out[var = "a_f", spp] <- 0.001         ## conversion rate from photons to carbohydrates
  }

  ## reassign custom variables. Perhaps there is a way to do this that doesnt require overwriting?
  for (i in 1:length(custom)) {
    for (j in 1:length(custom[[i]])) {
      out[var = names(custom[[i]])[j], i] <- custom[[i]][j]
    }
  }
  class(out) <- "allometry"
  return(out)
}

## function to calculate full allometry for class simulation

calculate_allometry <- function(simulation) {

  if(!is.simulation(simulation)) {
    print("Error: input must be of class simulation")
    return()
  }

  simulation[["height"]] <- H*(simulation[["diameter"]]^(gamma-1))
  simulation[["crown_area"]] <- alpha_w*(simulation[["diameter"]]^gamma)
  simulation[["struct_biomass"]] <- alpha_s*(simulation[["diameter"]]^(gamma+1))

  return(simulation)
}
