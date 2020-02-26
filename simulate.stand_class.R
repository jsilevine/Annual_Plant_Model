
## code for the class: simulation

## define main simulate function


extract_column <- function(colname, data) {

  data_matrix <- data[[colname]]

  if(!is.matrix(data_matrix)) {
    print("Error: column matrix must be of class matrix")
    return()
  }

  column <- as.vector(data_matrix)
  return(column)

}


simulate_stand <- function(allometry, max_t, timestep, dnot = rnorm(n = nstart, mean = 0.0004, sd = 0.00005), plot.area, n_start, full.allom = TRUE, avgs = TRUE, full.data = TRUE) {

  if(!is.allometry(allometry)) {
    print("Error: allometry must be an allometry object created using the create_allometry function")
    return()
  }

  if(!is.numeric(max_t)) {
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

  if (!is.logical(full.allom)) {
    print("Error: full.allom must be TRUE/FALSE")
    return()
  }

  ## initialize data list
  data <- list(diameter = matrix(1, nrow = n_start, ncol = max_t/timestep+1),
               crown.class = matrix(1, nrow = n_start, ncol = max_t/timestep+1),
               diameter.growth = matrix(1, nrow = n_start, ncol = max_t/timestep+1))

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

    ## this is just to improve interpretability for summary functions, likely there is a cleaner way to do this?
    if (i == max_t) {
      data[["diameter.growth"]][,i+1] <- NA
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
      data[["diameter"]] <- data[["diameter"]][order(data[["diameter"]][,i+1], decreasing = TRUE),]
      data[["crown.class"]] <- data[["crown.class"]][order(data[["diameter"]][,i+1], decreasing = TRUE),]
      data[["diameter.growth"]] <- data[["diameter.growth"]][order(data[["diameter"]][,i+1], decreasing = TRUE),]
      ## create vector of cumulative CA sums, used to determine which trees to add/remove from canopy
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

  if (full.data) {
    colnames <- names(data)[!(names(data) %in% c("avgs", "max_t", "timestep", "plot.area"))]
    columns <- lapply(colnames, extract_columns, data = data)
    full.data <- matrix(unlist(columns), ncol = length(colnames))
    time_column <- rep(seq(1, (max_t+timestep), by = timestep), each = nrow(data[["diameter"]]))
    ind_column <- rep(seq(1, nrow(data[["diameter"]]), by = 1), times = (max_t/timestep)+1)
    full.data <- cbind(time_column, ind_column, full.data)
    colnames(full.data) <- c("timestep", "individual", colnames)
    data[["full.data"]] <- full.data
  }

  return(data)
}


## define is. method for simulation class
is.simulation <- function(x) inherits(x, "simulation")

## define format and summary methods for simulate.stand
## want it to print: mean and sd for diameter, CA, root SA, height, structural biomass
##                   proportion of trees in overstory vs understory
##

format.simulation <- function(x) {

}


summary.simulation <- function(simulation) {

  if(!full.data) {
    print("Error: simulation must have full.data element for summary method")
    return()
  }

  ## generate table of means and sds
  varnames <- c("diameter", "diameter.growth", "height", "crown_area", "struct_biomass")[c("diameter", "diameter.growth", "height", "crown_area", "struct_biomass") %in% names(simulation)]
  table_text <- matrix(0, nrow = length(varnames), ncol = 2)
  colnames(table_text <- c("mean", "std. deviation")
  rownames(table_text) <- varnames
  for(i in 1:length(varnames)) {
    table_text[i,2] <- mean(simulation[[varname]][simulation[["full.data"]][,"timestep"]==max_t+1,varname])
    table_text[i,3] <- sd(simulation[[varname]][simulation[["full.data"]][,"timestep"]==max_t+1,varname])
  }

  cat("Simulation:", "\n"
      "Max time: ", as.character(simulation[["max_t"]]), "\n",
      "Time Step: ", as.character(simulation[["timestep"]]), "\n",
      "Iterations: ", as.character(simulation[["max_t"]]/simulation[["timestep"]]), "\n",
      "Plot Area:", as.character(simulation[["plot.area"]]), "\n",
      "\n",
      "Means and standard deviations for variables at last time step", "\n")
  return(x)
}

plot_simulation_result <- function(data, y.val) {
  ggplot(aes_string(x="timestep", y.val, color = "crown.class"), data = data) +
    geom_point() +
    geom_smooth(method = gam, formula = y~s(x, bs="cs", sp=3, k = 20), se = FALSE) +
    xlab("Time Step") +
    scale_color_discrete(name = "Crown Class") +
    theme(title = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1))
}

plot.simulation <- function(simulation, avgs = FALSE, y.values) {
  if (avgs) ind <- "avgs"
  else ind <- "full.data"

  if (avgs & is.null(simulation[["avgs"]])) {
    print("Error: avgs must be defined in simulation")
    return()
  }

  if (ind == "full.data" & is.null(simulation[["full.data"]])) {
    print("Error: fulldata must be defined in simulation")
    return()
  }

  data <- as.data.frame(simulation[[ind]])
  data$crown.class <- factor(data$crown.class)

  if (missing(y.values)) {
    y.values <- as.list(colnames(data)[!(colnames(data) %in% c("timestep", "individual","crown.class"))])
  }

  plot.list <- lapply(y.values, plot_simulation_result, data = data)
  eval.list <- character()

  for (i in 1:length(y.values)) {
    if (i == length(y.values)) eval.list <- paste0(eval.list, "plot.list[[", i, "]]")
    else eval.list <- paste0(eval.list, "plot.list[[", i, "]], ")
  }
  eval(parse(text = paste0("grid.arrange(", eval.list, ", nrow = 2, top = textGrob('Stand Simulation Output'))")))

}
