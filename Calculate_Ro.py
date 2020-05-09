import numpy as np
import sys

quarantine_day = 5
record_data = np.loadtxt("record_file.csv")

def calculate_Ro(new_cases, prev_cases):

	day_ratio = []
	for i in range(len(prev_cases)):
		
		if i == 2:
			break
		if new_cases[i] == 0 or prev_cases[i] == 0:
			break
		if prev_cases[i] < prev_cases[i-1]  and i > 0:
			break
	
		day_ratio.append(float(new_cases[i])/float(prev_cases[i]))

	avg_ratio = sum(day_ratio)/float(len(day_ratio))
	Ro = avg_ratio*(quarantine_day-1)

	return Ro

new_cases = record_data[:,1]
prev_cases = record_data[:,2]

Ro = calculate_Ro(new_cases, prev_cases)
print Ro

sys.exit()
