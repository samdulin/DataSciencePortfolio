from keras.layers import Dense
from keras.optimizers import Adam
from keras.models import Sequential
from keras.callbacks import ModelCheckpoint
import numpy as np

class NNregressor(object):
    model_name = 'NNregressor'
    VERBOSE = 0
    
    def __init__(self):
        self.model = None
        self.time_window_size = None
        self.metric = None
    
    @staticmethod
    def create_model(train, metric):
        model = Sequential()
        model.add(Dense(128, kernel_initializer='normal',input_dim = train.shape[1], activation='relu'))

        model.add(Dense(256, kernel_initializer='normal',activation='relu'))
        model.add(Dense(256, kernel_initializer='normal',activation='relu'))
        model.add(Dense(256, kernel_initializer='normal',activation='relu'))

        model.add(Dense(2, kernel_initializer='normal',activation='linear'))

        model.compile(loss='mean_squared_error', optimizer=Adam(lr = .0007), metrics=[metric])
        model.summary()
        return model
        
    def fit(self, X_train, y_train, batch_size=32, epochs=1400, validation_split=0.1, metric='mean_absolute_error'):

        self.metric = metric
        self.model = self.create_model(X_train, metric=self.metric)
        input_X_train = np.array(X_train)
        input_y_train = np.array(y_train)
        history = self.model.fit(x=X_train, y=y_train,
                                 batch_size=batch_size, epochs=epochs,
                                 verbose=0, validation_split=validation_split).history
    
    def predict(self, X_test):
        X_test = np.array(X_test)
        target = self.model.predict(x=X_test)
        return target