// Author: Yrjö Koski, 2020
// Modified by: Jarkko Toivonen, 2020

data {
    // Train data
    int<lower=1> N;           // Number of donation events
    int<lower=1> K;           // Number of variables measured at each donation
    int<lower=1> N_donor;     // Number of donors in our data
    matrix[N, K] X;           // Data matrix X
    vector[N] Hb;             // Hemoglobin values (that we are trying to predict)
    int<lower=1, upper=N_donor> donor[N];             // Donor identifier

    // Test data
    int<lower=1> N_test;      // Number of test observations
    matrix[N_test, K] X_test; // Test data
    int<lower = 1, upper=N_donor> test_donor[N_test]; // Test donor identifier
}
parameters {
    vector[K] beta;           // Coefficients on X
    real<lower=0> sigma_b;    // Variance of the individual intercept b
    real<lower=0> sigma_eps;  // Variance of the random noise
    vector[N_donor] donor_b;  // Donor specific random effect
} 
model {
    // A priori distributions
    beta      ~ std_normal();
    sigma_b   ~ inv_gamma(0.01, 0.01);
    sigma_eps ~ inv_gamma(0.01, 0.01);
    donor_b   ~ normal(0,sigma_b);

    // Distribution for hemoglobin
    Hb        ~ normal(X * beta + donor_b[donor], sigma_eps);   
}
generated quantities {
    vector[N_test] y_pred;

    // Predict using parameters and test data
    for (i in 1:Ntest) {
        y_pred[i] = normal_rng(X_test[i]*beta + donor_b[test_donor[i]], sigma_eps);
    }
}

