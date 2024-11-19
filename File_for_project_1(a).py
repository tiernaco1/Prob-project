import numpy as np
import matplotlib.pyplot as plt

def simulate_temp_positive(Delta_t, num_steps, num_simulations):
    P_values = [] # stores proportion of positive temps

    for _ in range(num_simulations): # completes as many sims as needed
        X = np.zeros(num_steps) # set initial temp of all values to 0
        for i in range (1, num_steps):
            X[i] = X[i - 1] + np.random.normal(0, np.sqrt(Delta_t)) # temp X(t+Δt)−X(t)∼N(0,Δt)

        P = np.sum( X > 0) / num_steps
        P_values.append(P)

    return P_values        

def plot_hist(P_values, Delta_t):
    plt.figure(figsize=(8,6))
    plt.hist(P_values, bins=30, color='orange', alpha = 0.7) # values, how many bins, color, transperancy of bars
    plt.title(f"Histogram of proportion of P (Δt = {Delta_t})")
    plt.xlabel('proportion of time X(T) > 0')
    plt.ylabel('frequency')
    plt.show()

def run_proportion_simulations(Delta_t, num_steps = 1000, num_simulations = 10000):
    for dt in Delta_t:
        print("simulation where Δt = {Delta_t}")
        P_vals = simulate_temp_positive(dt, num_steps, num_simulations)
        plot_hist(P_vals, dt)


Delta_t = [0.01, 0.001, 0.0001]
run_proportion_simulations(Delta_t)

# test