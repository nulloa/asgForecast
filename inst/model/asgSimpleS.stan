data {
  int<lower=1> n; //total number of obs over all locations and seasons
  int<lower=1> nG; //number of regions
  int<lower=1> nS; //number of seasons
  int x[n]; //week of observation indicator
  int<lower = 1, upper = nG> group[n]; //indicator for each group
  real y[n]; // observed value of flu
  int<lower = 1, upper = nS> seas[n]; // season indicator 
  int<lower=1> k; // number of mean parameters
  real<lower=0> ysd; // variance of data model
  vector[k] mu0;
  cov_matrix[k] C0;
}

parameters {
  matrix[nS, k] ctheta; // Parameters for Each Year.  This is a nS x K matrix
  vector<lower=0> [k] tau; // Standard Deviation for Parameters for S seasons
  //cholesky_factor_corr[k] Lcorr;
  vector[k] mu_s;
}

transformed parameters {
  real mu[n]; // Logit of psi
  
  for(i in 1:n){
        if (x[i] < exp(ctheta[seas[i], 4])){
          mu[i] = inv_logit(ctheta[seas[i], 1] + exp(ctheta[seas[i], 3])*exp(-((x[i] - exp(ctheta[seas[i], 4]))^2)/(2*(exp(ctheta[seas[i], 5]))^2)));
          }
          else{
            mu[i] = inv_logit(ctheta[seas[i], 1] + exp(ctheta[seas[i], 3])*exp(-((x[i] - exp(ctheta[seas[i], 4]))^2)/(2*(exp(ctheta[seas[i], 6]))^2)));
          }
  }

}

model {
  //Prior for Error Terms by Year
  tau ~ student_t(4, 0, 1); //Prior on group SD
  //Lcorr ~ lkj_corr_cholesky(1); //prior for correlations
  
  mu_s ~ multi_normal(mu0, C0); //prior on season mean
  
  for(s in 1:nS){
    //ctheta[s] ~ multi_normal_cholesky(mu_s, diag_pre_multiply(tau, Lcorr)); // c(beta1[g], beta2[g], eta[g], lmu[g], lsigma1[g], lsigma2[g])
    ctheta[s] ~ multi_normal(mu_s, diag_matrix(tau));
  }
  
  y ~ normal(mu, ysd);
}

generated quantities {
  vector[n] log_lik;
  //matrix[k,k] Omega;
  //Omega = multiply_lower_tri_self_transpose(Lcorr);
  for(i in 1:n){
    log_lik[i] = normal_lpdf(y[i] | mu[i], ysd);
  }
}

