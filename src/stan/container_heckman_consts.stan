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
    int<lower=1> L; // Amount of exogenous variables at first donation
    int<lower=1> M; // Amount of donor-specific constants

    matrix[N, K] Q_star; // Q_matrix
    matrix[K, K] R_star_inv;
    vector[N] Hb; // Hemoglobin values (that we are trying to predict)
    int<lower=1, upper=Ndon> donor[N]; // Donor identifier

    matrix[Ndon, L] Z; // Donor-specific first-event matrix
    matrix[Ndon, M] C; // Donor-specific constant matrix

    int first_events[Ndon];
    // Test data
    int<lower=1> Ntest; // Amount of test observations
    matrix[Ntest, K] x_test; // Test data
    int<lower = 1, upper=Ndon> test_donor[Ntest]; // Test donor identifier
}
parameters {
    vector[K] beta_tilde;   // Coefficients on Q_star
    real<lower=0> sigmab;   // Variance of the individual variation parameter b
    real<lower=0> sigmaeps; // Variance of the random noise
    real<lower=0> sigmaeeta;
    real theta;        // Common slope term for don_b
    vector[Ndon] donb; // Donor specific random effect
    vector[L] ups;     // Coefficients for exogenous variables
    vector[M] phi;     // Coefficients of donor specific variables
} 
transformed parameters {
    vector[K] beta = R_star_inv * beta_tilde;
}
model {
    beta ~ std_normal();
    sigmab ~ inv_gamma(0.01, 0.01);
    sigmaeps ~ inv_gamma(0.01, 0.01);
    sigmaeeta ~ inv_gamma(0.01, 0.01);
    theta ~ std_normal();
    ups ~ std_normal();
    phi ~ std_normal();
    donb ~ normal(0,sigmab);

    for (i in 1:N) {
        if (i == first_events[donor[i]]) {
            Hb[i] ~ normal(Z[donor[i]] * ups + theta * donb[donor[i]] + C[donor[i],] * phi, sigmaeeta);
        } 
        else {
            Hb[i] ~ normal(Q_star[i,] * beta_tilde + donb[donor[i]] + C[donor[i],] * phi, sigmaeps);
        }
    }
}
generated quantities {
    //vector[N] log_lik;
    vector[Ntest] y_pred;

    // for (i in 1:N) {
    //     if (i == first_events[donor[i]]) {
    //         log_lik[i] = normal_lpdf(Hb[i] | Z[donor[i]] * ups + theta * donb[donor[i]] + C[donor[i],] * phi, sigmaeeta);
    //     } 
    //     else {
    //         log_lik[i] = normal_lpdf(Hb[i] | Q_star[i,] * beta_tilde + donb[donor[i]] + C[donor[i],] * phi, sigmaeps);
    //     }
    // }
    // Predict using parameters and test data
    for (i in 1:Ntest) {
        y_pred[i] = normal_rng(x_test[i]*beta + donb[test_donor[i]] + C[donor[i],] * phi, sigmaeps);
    }
}

