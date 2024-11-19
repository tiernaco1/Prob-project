# import numpy as np
# import matplotlib.pyplot as plt

# def simulateTemperature(T, dt, simNo):
import numpy as np
import matplotlib.pyplot as plt

def simulate_temperature(T=1, dt=0.01, num_simulations=100000):
    # n_steps = int(T / dt)
    # t_values = np.linspace(0, T, n_steps + 1)
    # Tmax_values = []

    # for _ in range(num_simulations):
    #     # Generate random increments
    #     increments = np.random.normal(loc=0, scale=np.sqrt(dt), size=n_steps)
    #     X = np.zeros(n_steps + 1)
    #     X[1:] = np.cumsum(increments)

    #     # Find Tmax
    #     Tmax = t_values[np.argmax(X)]
    #     Tmax_values.append(Tmax)

    for i in range(num_simulations):
        time = 0
        temp = 0
        tmax = 0
        while (time<=1):
            temp = temp
            


    return Tmax_values

# Parameters
dt_values = [0.01, 0.001, 0.0001]
num_simulations = 100000

for dt in dt_values:
    Tmax_values = simulate_temperature(dt, num_simulations)
    
    # Plot histogram
    plt.hist(Tmax_values, bins=50, density=True, alpha=0.7, label=f"dt={dt}")
    plt.xlabel('Tmax')
    plt.ylabel('Density')
    plt.title('Distribution of Tmax')
    plt.legend()

plt.show()
