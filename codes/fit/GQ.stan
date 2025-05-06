//
// This Stan program defines the generated quantities for a hierarchical model, with a
// vector of values isotopic 'y' modeled as normally distributed
// with hierarchical slopes between C/N and y
// being dependant upon soil C content
//

data {
  // Integers
  int<lower=0,upper=1> prior_only;
  int<lower=0> N; //nb of data points
  int<lower=0> N_pred; //nb of C/N values for predictions
  int<lower=0> U; //nb of units
  int<lower=0> U_pred; //nb of SOC values for predictions
  int<lower=0> K; //nb of basis
  
  // Response
  vector[N] y;
  
  // Covariates
  matrix[U,K] SOC;
  vector[U_pred] SOC_pred;
  vector[N] CN;
  vector[N_pred] CN_pred;
  int unit[N];
  int unit_pred[N_pred];
  
  // Priors
  real lam_sigma_beta_1; real lam_sigma_beta_2;
  real lam_sigma;
  real sd_gamma_0;
  real sd_gamma;
  real mean_beta_0; real sd_beta_0;
}
parameters {
  // Hyperparameters
  real mu_SOC_mean; //mean for mean unit log SOC
  real<lower=0> sigma_SOC_mean; //sd for mean unit log SOC
  cholesky_factor_corr[2] rho; //correlation among hierarchical effects
  vector<lower=0>[2] sigma_beta; //sd of hierarchical effects
  real<multiplier=sd_gamma_0> gamma_0; //int of the slope-SOC relationship
  real<multiplier=sd_gamma> gamma; //slope of the slope-SOC relationship
  // Parameters
  vector<offset=mu_SOC_mean,multiplier=sigma_SOC_mean>[U] log_SOC_mean;
  real<lower=0> SOC_logsd;
  real<offset=mean_beta_0,multiplier=sd_beta_0> beta_0; //int of the isotope-CN relationship
  matrix[U,2] beta_raw; //slope of the isotope-CN relationship
  real<lower=0> sigma; //residual sd
}
transformed parameters{
  vector[U] SOC_mean = exp(log_SOC_mean);
  // Hyperparameters
  matrix[U,2] mu_beta = append_col(rep_vector(0,U), gamma_0 + SOC_mean * gamma); //linear predictor of slopes
  matrix[2,2] Sigma_beta = diag_post_multiply(rho, sigma_beta); //cov matrix of hierarchical effects
  // Parameters
  matrix[U,2] beta = mu_beta + beta_raw * Sigma_beta; //final hierarchical parameters
  // Linear predictor
  vector[N] mu = beta_0 + beta[unit,1] + CN .* beta[unit,2];
}
generated quantities{
  vector[U_pred] mu_beta_pred = gamma_0 + SOC_pred * gamma;
  vector[N_pred] mu_pred = beta_0 + beta[unit_pred,1] + CN_pred .* beta[unit_pred,2]; 
}

