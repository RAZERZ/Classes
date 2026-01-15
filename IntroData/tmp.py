import numpy as np

a = np.array([[0.3,0.4,0.3],[0.2,0.5,0.3],[0.4,0.3,0.3]])
t = np.array([[0.3,0.3], [0.4,0.3]])

print(np.linalg.inv(np.eye(2)-t)@np.array([1,1]))


# print(np.linalg.eig(a.T))
# grej = np.array([-4.93651112e-01, -8.16496581e-01,  7.07106781e-01])
# grej1 = np.array([-4.93651112e-01, -7.02503505e-01, -5.12637693e-01])

# grej2 = grej1/np.sum(grej1)
# print(grej2)
