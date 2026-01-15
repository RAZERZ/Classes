import pandas as pd
import numpy as np
import sklearn as sk
import matplotlib.pyplot as plt
import Utils
import warnings
warnings.filterwarnings('ignore')


#Sex  Length  Diameter  ...  Viscera weight  Shell weight  Rings
problem2_df = pd.read_csv('data/abalone.csv')
problem2_df.replace("M", 0, inplace=True)
problem2_df.replace("F", 1, inplace=True)
problem2_df.replace("I", -1, inplace=True)

Rings = np.array(problem2_df["Rings"])
sansRings = np.array(problem2_df.drop(columns=["Rings"]))

# print(np.mean(Rings**2)-(np.mean(Rings))**2)
# print(np.sqrt(np.var(Rings)))

X_train, X_test, Y_train, Y_test = sk.model_selection.train_test_split(sansRings, Rings, test_size=0.4, random_state=42)

LRModel = sk.linear_model.LinearRegression().fit(X_train, Y_train)
predY_test = LRModel.predict(X_test)

mae = np.mean(np.absolute(predY_test-Y_test))

print(mae)

doneEDF = Utils.makeEDF(predY_test-Y_test)
Utils.plotEDF(doneEDF, confidence_band=True)

plt.scatter(predY_test, Y_test)
plt.plot()
plt.show()
