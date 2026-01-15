import pandas as pd
import numpy as np
import sklearn as sk
import matplotlib.pyplot as plt
import Utils
import scipy
import warnings
warnings.filterwarnings('ignore')


problem3_df = pd.read_csv('data/visits_clean.csv', sep=" ")
problem3_X = np.array(problem3_df.drop(columns=["ofp", "ofnp", "opp", "opnp"]))
problem3_Y = np.array(problem3_df["ofp"])

X_train, X_test, Y_train, Y_test = sk.model_selection.train_test_split(problem3_X, problem3_Y, test_size=0.2, random_state=42)


class PoissionRegression(object):
    def __init__(self):
        self.coeffs = None
        self.result = None

    def fit(self, X,Y):
        import numpy as np
        from scipy import optimize

        def loss(coeffs):
            lam = np.exp(np.dot(X,coeffs[:-1])+coeffs[-1])

            ll = -Y*np.log(lam)+lam

            return np.mean(ll)

        initial_arguments = np.zeros(shape=X.shape[1]+1)
        self.result = optimize.minimize(loss,initial_arguments,method='cg')
        self.coeffs = self.result.x

    def predict(self,X):
        if(self.coeffs is not None):
            return np.exp(np.dot(X,self.coeffs[:-1])+self.coeffs[-1])

PR = PoissionRegression()
PR.fit(X_train,Y_train)
predY_test = PR.predict(X_test)


#-5.091323549620598
# print(np.mean(predY_test-Y_test))
#50.21171437082841
# print(np.var(predY_test-Y_test))

#5.988662131519274
# print(np.mean(Y_test))
#50.75043834616235
# print(np.var(Y_test))

#0.8973385818986758
# print(np.mean(predY_test))
#0.05048275939863351
# print(np.var(predY_test))

print(np.mean((predY_test-Y_test)**2)/np.var(Y_test))
