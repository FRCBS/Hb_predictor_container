// Model 1 but with QR-reparametrization for optimized runtime
// Added a prediction option. Model creates predictions for test set simultaneously
// From 20.6. on the QR-decomposition is performed in R due to memory issues since the
// dataset is quite large
// This script fixes the initial conditions problem by using the Wooldridge solution.

// Author: Yrjö Koski, 2020

data {
    // Train data
    int<lower=1> N; // Amount of donation events
    int<lower=1> K; // Amount of variables measured at each donation
    int<lower=1> Ndon; // Amount of donors in our data
    int<lower=1> L;

    matrix[N, K] Q_star; // Q_matrix
    matrix[K, K] R_star_inv;
    vector[N] Hb; // Hemoglobin values (that we are trying to predict)
    int<lower=1, upper=Ndon> donor[N]; // Donor identifier

    matrix[Ndon, L] Z;

    int first_events[Ndon];
}
parameters {
    vector[K] beta_tilde; // Coefficients on Q_star
    real<lower=0> sigma_b; // Variance of the individual variation don_a
    real<lower=0> sigma_eps; // Variance of the random noise
    real<lower=0> sigma_eeta;
    real theta; // Common slope term for don_b
    vector[Ndon] don_b; // Donor specific random effect
    vector[L] ups; // Coefficients for exogenous variables
} 
transformed parameters {
    vector[K] beta = R_star_inv * beta_tilde;
}
model {
    sigma_b ~ inv_gamma(0.01, 0.01);
    sigma_eps ~ inv_gamma(0.01, 0.01);
    sigma_eeta ~ inv_gamma(0.01, 0.01);
    theta ~ std_normal();
    ups ~ std_normal();
    beta ~ std_normal();
    don_b ~ normal(0, sigma_b);

    for (i in 1:N) {
        if (i == first_events[donor[i]]) {
            Hb[i] ~ normal(Z[donor[i]] * ups + theta * don_b[donor[i]], sigma_eeta);
        } 
        else {
            Hb[i] ~ normal(Q_star[i,] * beta_tilde + don_b[donor[i]], sigma_eps);
        }
    } 
}
generated quantities {
    vector[N] log_lik;

    for (i in 1:N) {
        if (i == first_events[donor[i]]) {
            log_lik[i] = normal_lpdf(Hb[i] | Z[donor[i]] * ups + theta + don_b[donor[i]], sigma_eeta);
        } 
        else {
            log_lik[i] = normal_lpdf(Hb[i] | Q_star[i,] * beta_tilde + don_b[donor[i]], sigma_eps);
        }
    }
}

