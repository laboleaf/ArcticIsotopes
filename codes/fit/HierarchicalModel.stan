//
// This Stan program defines a hierarchical model, with a
// vector of values isotopic 'y' modeled as normally distributed
// with hierarchical slopes between C/N and y
// being dependant upon soil C content
//

data {
  // Integers
  int<lower=0,upper=1> prior_only;
  int<lower=0> N; //nb of data points
  int<lower=0> U; //nb of units
  
  // Response
  vector[N] y;
  
  // Covariates
  vector[N] SOC;
  vector[N] CN;
  array[N] int unit;
  
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
model {
  // Hyperpriors
  rho ~ lkj_corr_cholesky(2);
  sigma_beta[1] ~ exponential(lam_sigma_beta_1);
  sigma_beta[2] ~ exponential(lam_sigma_beta_2);
  gamma_0 ~ normal(0, sd_gamma_0);
  gamma ~ normal(0, sd_gamma);
  mu_SOC_mean ~ normal(3, 2);
  sigma_SOC_mean ~ exponential(1);
  log_SOC_mean ~ normal(mu_SOC_mean, sigma_SOC_mean);
  SOC_logsd ~ exponential(1);
  // Priors
  beta_0 ~ normal(mean_beta_0, sd_beta_0);
  to_vector(beta_raw) ~ std_normal();
  sigma ~ exponential(lam_sigma);
  // Likelihood
  if(prior_only == 0){
    SOC ~ lognormal(log_SOC_mean[unit], SOC_logsd);
    y ~ normal(mu, sigma);
  }
}
generated quantities{
  real y_rep[N] = normal_rng(mu, sigma);
}

