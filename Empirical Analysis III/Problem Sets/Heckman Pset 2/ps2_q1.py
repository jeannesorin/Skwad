# Import modules
import numpy as np
import pandas as pd
import scipy.stats as st
import matplotlib.pyplot as plt

# Load data
data_2 = pd.read_excel("PS2_Q1_Data.xlsx", sheet_name="K = 2")
data_5 = pd.read_excel("PS2_Q1_Data.xlsx", sheet_name="K = 5")


# Part A- z-test
z_2 = [10*np.mean(data_2[i]) for i in range(2)] # z-statistics
pz_2 = [1-st.norm.cdf(abs(z)) for z in z_2] # p-values
print(z_2)
print(pz_2)

z_5 = [(10*np.mean(data_5[i])) for i in range(5)]
pz_5 = [1-st.norm.cdf(abs(z)) for z in z_5]
print(z_5)
print(pz_5)

# Part B
# Chi-square test
chi_2 = sum(k**2 for k in z_2)
pchi_2 = 1-st.chi2.cdf(chi_2,df=2)
print(chi_2)
print(pchi_2)

chi_5 = sum(k**2 for k in z_5)
pchi_5 = 1-st.chi2.cdf(chi_2,df=5)
print(chi_5)
print(pchi_5)

# Max test
max_2 = max(z_2)/10 # get the value of the max from z-statistics
pmax_2 = 1- st.norm.cdf(10*max_2)**2
print(max_2)
print(pmax_2)

max_5 = max(z_5)/10
pmax_5 = 1- (st.norm.cdf(10*max_5)**5)
print(max_5)
print(pmax_5)


# Step Down test
a = 0.05
ranks_2 = [int(k) for k in st.rankdata(pz_2)] # get ranks
for j in range(len(pz_2)):
    if pz_2[ranks_2[i]-1]>a/(2-j): # Remeber python indexing starts at 0!
        print("Fail to reject")
    else:
        print("Rejected")
        break

ranks_5 = [int(k) for k in st.rankdata(pz_5)] # get ranks
for j in range(len(pz_5)):
    if pz_2[ranks_2[i]-1]>a/(5-j): # Remeber python indexing starts at 0!
        print("Fail to reject")
    else:
        print("Rejected")
        break

# Part C- sampling distribution for the Max
reps = 10**5
sim_2=[1]*reps
sim_5=[1]*reps
sim_20=[1]*reps
for k in range(reps):
    sim_2[k] = max(np.random.standard_normal(2))
    sim_5[k] = max(np.random.standard_normal(5))
    sim_20[k] = max(np.random.standard_normal(20))
sim_2.sort()
sim_5.sort()
sim_20.sort()
y = np.linspace(0, 1, reps)
fig, ax = plt.subplots()
ax.plot(sim_2, y, linewidth=2,alpha=1,label = 'K=2',color='red')
ax.plot(sim_5, y, linewidth=2,alpha=1,label = 'K=5',color='green')
ax.plot(sim_20, y, linewidth=2,alpha=1,label = 'K=20',color='blue')
ax.set_title('Monte Carlo Simulations')
ax.legend(loc="best")
plt.savefig("CDF.png")

# Part E- meta-analysis- pooling the data
# Using the mean
pool_2 = np.concatenate((data_2[0],data_2[1]))
z_pool_2 = np.sqrt(200)*np.mean(pool_2)
zp_pool_2 = 1-st.norm.cdf(abs(z_pool_2))
print(z_pool_2)
print(zp_pool_2)

pool_5 = np.concatenate(pd.Series([data_5[i] for i in range(5)]))
z_pool_5 = np.sqrt(200)*np.mean(pool_5)
zp_pool_5 = 1-st.norm.cdf(abs(z_pool_5))
print(z_pool_5)
print(zp_pool_5)
