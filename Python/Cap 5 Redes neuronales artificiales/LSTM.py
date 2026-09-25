# Importamos librerias matematicas, de analisis de datos, graficacion y redes neuronales
import math
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from keras.models import Sequential
from keras.layers import Dense, LSTM, Dropout
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import mean_squared_error
from sklearn.metrics import mean_absolute_percentage_error

# Definimos la ruta del archivo con los datos historicos de precios de acciones de Google
ruta = r"C:\Users\alfre\documentos_ssd\UNAM\Artículos\libro\data\precio_google.csv"

# Leemos el archivo CSV y revisamos los primeros 10 registros de la tabla
data_train = pd.read_csv(ruta)
data_train.head(10)

# Extraemos la serie de precios de apertura (columna 'Open') como arreglo bidimensional
data = data_train.loc[:, ["Open"]].values

# Graficamos la serie temporal del precio de apertura a lo largo del tiempo
plt.plot(data)
plt.xlabel("Tiempo")
plt.ylabel("Precio de apertura en USD")
plt.title("Precio de apertura de Google (GOOGL)")
plt.show()

## Preprocesando data
# Ajuste de dimensiones: convertimos a matriz columna y tipo flotante de 32 bits
data = data.reshape(-1, 1)
data = data.astype("float32")
data.shape

# Rescalamiento: normalizamos los precios al intervalo entre 0 y 1 para facilitar el aprendizaje
scaler = MinMaxScaler(feature_range=(0, 1))
data = scaler.fit_transform(data)

# Conjunto de entrenamiento (80%) y conjunto de prueba (20%) respetando el orden temporal
train_size = int(len(data) * 0.8)
test_size = len(data) - train_size
train = data[0:train_size, :]
test = data[train_size:len(data), :]
print("train size: {}, test size: {} ".format(len(train), len(test)))

# Bloques de tiempo: usamos una ventana de 5 observaciones anteriores para predecir la siguiente
timesteps = 5
x_data = []
y_data = []

# Construimos las secuencias de entrada y salida para el conjunto de entrenamiento
for i in range(len(train) - timesteps - 1):
    a = train[i:(i + timesteps), 0]
    x_data.append(a)
    y_data.append(train[i + timesteps, 0])
x_train = np.array(x_data)
y_train = np.array(y_data)

# Construimos las secuencias de entrada y salida para el conjunto de prueba
x_data = []
y_data = []
for i in range(len(test) - timesteps - 1):
    a = test[i:(i + timesteps), 0]
    x_data.append(a)
    y_data.append(test[i + timesteps, 0])

x_test = np.array(x_data)
y_test = np.array(y_data)

# Redimensionamos los datos al formato requerido por la capa LSTM: (muestras, pasos de tiempo, caracteristicas)
x_train = np.reshape(x_train, (x_train.shape[0], 1, x_train.shape[1]))
x_test = np.reshape(x_test, (x_test.shape[0], 1, x_test.shape[1]))

## Modelo LSTM (Red Neuronal Recurrente para series temporales)
# Inicializamos el modelo secuencial de Keras
model = Sequential()

# Agregamos la capa recurrente LSTM con 100 neuronas y definimos la dimension de entrada
model.add(LSTM(100, input_shape=(1, timesteps)))

# Agregamos la capa densa de salida con una sola neurona para predecir el valor continuo
model.add(Dense(units=1))

# Compilamos el modelo indicando la funcion de perdida (error cuadratico medio) y el optimizador adam
model.compile(loss="mean_squared_error", optimizer="adam")

# Entrenamos la red neuronal durante 100 epocas con un tamanio de lote (batch_size) de 1
model.fit(x_train, y_train, epochs=100, batch_size=1)

# Generamos las predicciones del modelo para el conjunto de entrenamiento y de prueba
train_pred = model.predict(x_train)
test_pred = model.predict(x_test)

# Visualizando el modelo de entrenamiento y prediccion
# Preparamos un arreglo vacio con la misma forma para ubicar las predicciones de entrenamiento
train_pred_plot = np.empty_like(data)
train_pred_plot[:, :] = np.nan
train_pred_plot[timesteps:len(train_pred) + timesteps, :] = train_pred

# Preparamos un arreglo vacio para ubicar las predicciones del conjunto de prueba en la escala temporal
test_prep_plot = np.empty_like(data)
test_prep_plot[:, :] = np.nan
test_prep_plot[len(train_pred) + (timesteps * 2) + 1:len(data) - 1, :] = test_pred

# Calculamos las metricas de desempenio: MAPE (error porcentual absoluto medio) y MSE (error cuadratico medio)
mape = mean_absolute_percentage_error(y_test, test_pred)
mse = mean_squared_error(y_test, test_pred)

# Graficamos la serie real completa junto con las curvas de prediccion
plt.plot(data)  # plt.plot(scaler.inverse_transform(data))
plt.plot(train_pred_plot, alpha=0.8, color='green')
plt.plot(test_prep_plot, alpha=0.8, color='orange')
plt.xlabel("Tiempo")
plt.ylabel("Precio de apertura (escalado 0-1)")
plt.title("Prediccion de apertura de Google (GOOGL)")
plt.show()

# Imprimimos el valor obtenido para el error porcentual medio (MAPE)
print('MAPE', mape)

# Calculamos e imprimimos la tasa de precision aproximada (1 - MAPE)
1 - mape
