# Modelo Logit (Regresion Logistica para clasificacion binaria)
# Importamos las librerias para computo numerico, datos, graficas y modelos estadisticos
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm
from sklearn.datasets import load_breast_cancer

# Cargamos el conjunto de datos de cancer de mama desde la libreria scikit-learn
cancer = load_breast_cancer()

# Construimos un DataFrame con los datos numericos y nombres de caracteristicas
df = pd.DataFrame(cancer.data, columns=cancer.feature_names)

# Agregamos la columna objetivo original: 0 = maligno, 1 = benigno
df['target'] = cancer.target

# Mapeamos los codigos numericos a nombres legibles de cada diagnostico
df['target_name'] = df['target'].map({0: 'malignant', 1: 'benign'})

# Revisamos la lista de todas las columnas (caracteristicas de los tumores)
df.columns

# Modificamos la variable objetivo para que 1 represente maligno y 0 benigno
# De esta forma el modelo estima directamente la probabilidad de malignidad
df["target"] = 1 - df["target"]

# Visualizamos una muestra de las columnas clave
df[['mean radius', 'target', 'target_name']]

# Seleccionamos las variables para entrenar el modelo
x = df['mean radius'] # Variable predictora: radio promedio del tumor
y = df['target']      # Variable dependiente binaria: 1 = maligno, 0 = benigno

# Agregamos el intercepto (termino constante B0) a la variable predictora
X = sm.add_constant(x)

# Ajustamos el modelo de regresion logistica (Logit) mediante maxima verosimilitud
model = sm.Logit(y, X).fit()

# Obtenemos el resumen del modelo en formato de texto
summary = model.summary2().as_text()

# Imprimimos el resumen estadistico con coeficientes, errores y valores p
print(summary)

# Generamos una malla continua de valores para dibujar la curva sigmoide estimada
x_grid = np.linspace(x.min(), x.max(), 400)
X_grid = sm.add_constant(x_grid)

# Calculamos las probabilidades predichas de malignidad para cada punto de la malla
pred_prob = model.predict(X_grid)

# Definimos el tamanio del lienzo para la grafica
plt.figure(figsize=(8, 5))

# Dibujamos los datos reales observados como puntos de dispersion (0 o 1)
plt.scatter(x, y, alpha=0.5, label="Y observada si benigno/maligno (0/1)", s=30)

# Dibujamos la curva en forma de S (sigmoide) que representa la probabilidad estimada
plt.plot(x_grid, pred_prob, linewidth=2, label="Y estimada (probabilidad)", color='black')

# Asignamos etiquetas a los ejes horizontal y vertical
plt.xlabel("mean radius")
plt.ylabel("Probabilidad de y = 1")
plt.title("Modelo LOGIT para clasificacion de cancer de mama, propension de la variable mean radius")

# Agregamos la leyenda identificadora y una cuadricula suave
plt.legend()
plt.grid(alpha=0.2)

# Trazamos lineas de referencia: umbral de decision en 0.5 y punto de corte aproximado
plt.axhline(y=0.5, color='r', linestyle='-', alpha=0.3)
plt.axvline(x=14.8, color='r', linestyle='-', alpha=0.3)

# Evaluacion del clasificador: division de datos y matriz de confusion
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report, confusion_matrix, ConfusionMatrixDisplay

# Dividimos los datos en conjunto de entrenamiento (80%) y conjunto de prueba (20%)
x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.2, random_state=7)

# Preparamos las variables del conjunto de entrenamiento agregando constante
X_train = sm.add_constant(x_train)

# Calculamos las probabilidades predichas sobre el conjunto de prueba
X_test = sm.add_constant(x_test)
y_pred_prob = model.predict(X_test)

# Convertimos las probabilidades continuas a clases discretas usando el umbral 0.5
y_pred_class = (y_pred_prob >= 0.5).astype(int)

# Calculamos la matriz de confusion para contrastar aciertos y errores
cm = confusion_matrix(y_test, y_pred_class)
disp = ConfusionMatrixDisplay(confusion_matrix=cm, display_labels=[0, 1])

# Imprimimos la matriz de confusion numerica y el reporte con metricas de evaluacion
print("matriz de confusion\n", cm)
print("\nClassification Report:\n", classification_report(y_test, y_pred_class, target_names=cancer.target_names))
