// Model 1 but with QR-reparametrization for optimized runtime
// Added a prediction option. Model creates predictions for test set simultaneously
// From 20.6. on the QR-decomposition is performed in R due to memory issues since the
// dataset is quite large

// Author: Yrjö Koski, 2019

data {
    // Train data
    int<lower=1> N; // Amount of donation events
    int<lower=1> K; // Amount of variables measured at each donation
    int<lower=1> Ndon; // Amount of donors in our data
    int<lower=1> M; // Amount of donor-specific consts
    matrix[N, K] Q_star; // Q_matrix
    matrix[K, K] R_star; // R_matrix
    matrix[K, K] R_star_inv;
    matrix[Ndon, M] C;
    vector[N] Hb; // Hemoglobin values (that we are trying to predict)
    int<lower=1, upper=Ndon> donor[N]; // Donor identifier
}
parameters {
    vector[K] beta_tilde; // Coefficients on Q_star
    vector[M] phi;
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
    phi ~ std_normal();
    donb ~ normal(0,sigmab);
    
    Hb ~ normal(Q_star * beta_tilde + donb[donor] + C[donor,] * phi, sigmaeps);   
}
generated quantities {
    vector[N] log_lik;
    for (n in 1:N) log_lik[n] = normal_lpdf(Hb[n] | Q_star[n, ] * beta_tilde + donb[donor[n]] + C[donor[n],] * phi, sigmaeps);
}

