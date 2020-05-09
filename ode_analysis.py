#script to generate and plot solutions to the deterministic ODE
import numpy as np 
import matplotlib as mpl 
import matplotlib.pyplot as plt  
from scipy import integrate 


def ode_system(t, y):
    global beta, gamma, population
    dsdt = -beta * y[1] * y[0] / population
    didt = -dsdt - gamma * y[1]
    drdt = gamma * y[1]
    return np.asarray([dsdt, didt, drdt])

infectivities = np.asarray([.4, .4, .9])
vacc_rates = np.asarray([0, .2, .95])
ode_solns = []
population = 50000
recovery_time = 6
gamma = 1 / recovery_time
daily_interactions = 4
initial_infected = 5
initial_recovered = 0

for infectivity, vaccination_rate in zip(infectivities, vacc_rates):
    beta = infectivity * daily_interactions
    initial_vulnerable = population - initial_infected - population * vaccination_rate
    initial_conditions = np.asarray([initial_vulnerable, initial_infected, initial_recovered])
    ode_solns.append(integrate.solve_ivp(ode_system, [0, 1000], initial_conditions))
plot_limits = [24, 21, 13]
fig_dims = 15
aspect_ratio = 4/3
fig_dpi = 400
fig_font = 24

font = {'family' : 'arial',
        'weight' : 'bold',
        'size'   : fig_font}

mpl.rc('font', **font)

colors = ['xkcd:weird green', 'xkcd:lightish red', 'xkcd:ultramarine blue']
labels=['Vulnerable', 'Infected', 'Recovered']

for soln_index, (infectivity, vaccination_rate) in enumerate(zip(infectivities, vacc_rates)):
  fig_name = str(infectivity) + '_' + str(vaccination_rate)
  figure, ax = plt.subplots()
  figure.set_figheight(fig_dims / aspect_ratio)
  figure.set_figwidth(fig_dims)
  for index in range(3):
    ax.plot(ode_solns[soln_index].t[:plot_limits[soln_index]+1], 100 * ode_solns[soln_index].y[index,:plot_limits[soln_index]+1]
            / population, label=labels[index], color=colors[index])

  plt.xlabel('Time (days)')
  plt.ylabel('Percent of Population')
  plt.legend()
  plt.savefig(fig_name + '.png', bbox_inches='tight', pad_inches=0.3, dpi=fig_dpi)
  plt.close()
