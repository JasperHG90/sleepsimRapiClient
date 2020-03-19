## Communicate with the simulator setting API

#' Fetch information about the number of allocated containers, number of completed etc.
#'
#' @return list containing three elements
#' \describe{
#'     \item{allocated}{number of containers that are currently allocated but not yet finished}
#'     \item{finished}{number of containers that are completely finished}
#'     \item{finished_past_day}{number of containers that have finished in the past 24 hours}
#' }
#'
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr authenticate
#' @importFrom httr stop_for_status
#' @importFrom httr timeout
#'
#' @export
simulations_completed_info <- function() {
  # HOST/PWD/USR
  host <- Sys.getenv("SLEEPSIMR_MASTER_HOST")
  usr <- Sys.getenv("SLEEPSIMR_API_USERNAME")
  pwd <- Sys.getenv("SLEEPSIMR_API_PASSWORD")
  # Make endpoint
  ep <- file.path(host, "info")
  # GET
  resp <- GET(ep,
              authenticate(user=usr, password=pwd),
              timeout(60),
              encode = "json")
  stop_for_status(resp)
  # Return
  return(content(resp))
}

#' Fetch simulation settings used for this simulation scenario
#'
#' @param uid unique id of this container
#'
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr authenticate
#' @importFrom httr add_headers
#' @importFrom httr stop_for_status
#' @importFrom httr timeout
#'
#' @return list containing 12 elements. Simulation settings
#'
#' @export
query_simulation_settings <- function(uid = getOption("sleepsimR_uid")) {
  # HOST/PWD/USR
  host <- Sys.getenv("SLEEPSIMR_MASTER_HOST")
  usr <- Sys.getenv("SLEEPSIMR_API_USERNAME")
  pwd <- Sys.getenv("SLEEPSIMR_API_PASSWORD")
  # Make endpoint
  ep <- file.path(host, "parameters")
  # GET
  resp <- GET(ep,
              add_headers("uid" = uid),
              authenticate(user=usr, password=pwd),
              timeout(60),
              encode = "json")
  stop_for_status(resp)
  # Return
  return(content(resp))
}

#' Check if connection to server is possible
#'
#' @importFrom httr http_error
#'
#' @return TRUE if connection active, else FALSE
#'
#' @export
check_connection <- function() {
  serv <- Sys.getenv("SLEEPSIMR_MASTER_HOST")
  # Get retinas from the api
  httr::http_error(serv)
}

#' Register the address of the host server
#'
#' @param url url of the master host
#'
#' @return Exits silently
#'
#' @export
set_host <- function(url) {
  Sys.setenv("SLEEPSIMR_MASTER_HOST" = url)
}

#' Set the username/password to access the API
#'
#' @param password password used to authenticate with the API
#' @param username username used to authenticate with the API
#'
#' @return Exits silently
#'
#' @export
set_usr_pwd <- function(password, username) {
  Sys.setenv("SLEEPSIMR_API_USERNAME" = username)
  Sys.setenv("SLEEPSIMR_API_PASSWORD" = password)
}

#' Send the results of the simulation to the master node
#'
#' @param scenario_uid string. unique id of the current simulation scenario
#' @param iteration_uid string. unique id of the iteration of the current scenario
#' @param PD_subj list. The length of this list is equal to the number of subjects. It contains the subject-specific parameter estimates of the state-dependent means for each emission distribution. For more information, see return objects from \link[mHMMbayes]{mHMM_cont}.
#' @param emiss_mu_bar list. The length of this list is equal to the number of dependent variables. Each element of the list is a numeric vector that is equal to the number of hidden states, the value of which is the Maximum A Posteriori (MAP) estimate of that parameter.
#' @param gamma_int_bar numeric vector. m x m values where m is the number of hidden states.
#' @param emiss_var_bar list. The length of this list is equal to the number of dependent variables. Each element of the list is a numeric vector that is equal to the number of hidden states, the value of which is the Maximum A Posteriori (MAP) estimate of that parameter.
#' @param emiss_varmu_bar list. The length of this list is equal to the number of dependent variables. Each element of the list is a numeric vector that is equal to the number of hidden states, the value of which is the Maximum A Posteriori (MAP) estimate of that parameter.
#' @param credible_interval list. The length of this list is equal to the number of dependent variables plus one. The elements of the list are (in this order): (1) a list containing m x m elements with the lower and upper 95\% CI of the between-subject TPM intercepts (gamma_int_bar), (2) n_dep lists containing m elements with the lower and upper 95\% CI of the between-subject emission distributions.
#' @param label_switch Numeric vector of length m x n_dep.
#' @param state_order List with n_dep numeric vectors containing integers indicating the ordering of the state means (as per the start values) after handing it off to \link[sleepsimR]{run_mHMM}. To detect label switching, the order of hyperprior means and start values are ranked from low to high. However, this is annoying when comparing estimated means to the ground-truth values (because they have not been ordered).
#' @param uid unique id of this container
#'
#' @importFrom httr POST
#' @importFrom httr content
#' @importFrom httr authenticate
#' @importFrom httr stop_for_status
#' @importFrom httr timeout
#'
#' @return if successful, a message telling the container to shut down.
#'
#' @export
register_simulation_outcomes <- function(scenario_uid,
                                         iteration_uid,
                                         PD_subj,
                                         emiss_mu_bar,
                                         gamma_int_bar,
                                         emiss_var_bar,
                                         emiss_varmu_bar,
                                         credible_interval,
                                         label_switch,
                                         state_order,
                                         uid = getOption("sleepsimR_uid")) {
  # Collect options
  host <- Sys.getenv("SLEEPSIMR_MASTER_HOST")
  usr <- Sys.getenv("SLEEPSIMR_API_USERNAME")
  pwd <- Sys.getenv("SLEEPSIMR_API_PASSWORD")
  # Make endpoint
  ep <- file.path(host, "results")
  # Make body
  nb <- list(
    "uid" = uid,
    "scenario_uid" = scenario_uid,
    "iteration_uid" = iteration_uid,
    "PD_subj" = PD_subj,
    "emiss_mu_bar" = emiss_mu_bar,
    "gamma_int_bar" = gamma_int_bar,
    "emiss_var_bar" = emiss_var_bar,
    "emiss_varmu_bar" = emiss_varmu_bar,
    "credible_intervals" = credible_interval,
    "label_switch" = label_switch,
    "state_order" = state_order
  )
  # Make post request
  resp <- POST(ep,
               body=nb,
               authenticate(user=usr, password=pwd),
               timeout(60),
               encode="json")
  # Get content
  stop_for_status(resp)
  return(content(resp))
}

#' Get the container ids of all current workers
#'
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr authenticate
#' @importFrom httr stop_for_status
#'
#' @return character vector containing the unique ids of the container processes.
#'
#' @export
get_active_workers <- function() {
  # HOST/PWD/USR
  host <- Sys.getenv("SLEEPSIMR_MASTER_HOST")
  usr <- Sys.getenv("SLEEPSIMR_API_USERNAME")
  pwd <- Sys.getenv("SLEEPSIMR_API_PASSWORD")
  # Make endpoint
  ep <- file.path(host, "active_workers")
  # GET
  resp <- GET(ep,
              authenticate(user=usr, password=pwd),
              timeout(60),
              encode = "json")
  stop_for_status(resp)
  # Return
  return(content(resp))
}
