// Model 1 but with QR-reparametrization for optimized runtime
// Added a prediction option. Model creates predictions for test set simultaneously
// From 20.6. on the QR-decomposition is performed in R due to memory issues since the
// dataset is quite large

// Author: Yrjö Koski, 2020

data {
    // Train data
    int<lower=1> N; // Amount of donation events
    int<lower=1> K; // Amount of variables measured at each donation
    int<lower=1> Ndon; // Amount of donors in our data
    matrix[N, K] Q_star; // Q_matrix
    matrix[K, K] R_star_inv;
    vector[N] Hb; // Hemoglobin values (that we are trying to predict)
    int<lower=1, upper=Ndon> donor[N]; // Donor identifier

    // Test data
    int<lower=1> Ntest; // Amount of test observations
    matrix[Ntest, K] x_test; // Test data
    int<lower = 1, upper=Ndon> test_donor[Ntest]; // Test donor identifier
}
parameters {
    vector[K] beta_tilde; // Coefficients on Q_star
    real<lower=0> sigmab; // Variance of the individual variation parameter b
    real<lower=0> sigmaeps; // Variance of the random noise
    vector[Ndon] donb; // Donor specific random effect
} 
transformed parameters {
    vector[K] beta = R_star_inv * beta_tilde;
}
model {
    beta ~ std_normal();
    sigmab ~ inv_gamma(0.01, 0.01);
    sigmaeps ~ inv_gamma(0.01, 0.01);
    donb ~ normal(0,sigmab);
    
    Hb ~ normal(Q_star * beta_tilde + donb[donor], sigmaeps);   
}
generated quantities {
    vector[N] log_lik;
    vector[Ntest] y_pred;

    for (n in 1:N) log_lik[n] = normal_lpdf(Hb[n] | Q_star[n, ] * beta_tilde + donb[donor[n]], sigmaeps);

    // Predict using parameters and test data
    for (i in 1:Ntest) {
        y_pred[i] = normal_rng(x_test[i]*beta + donb[test_donor[i]], sigmaeps);
    }
}

