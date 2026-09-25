# Paso 1: Importamos las librerias necesarias para el analisis y modelacion
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.datasets import load_breast_cancer
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.naive_bayes import GaussianNB
from sklearn.metrics import classification_report, confusion_matrix, ConfusionMatrixDisplay

# Paso 2: Cargamos el conjunto de datos de cancer de mama en un DataFrame
cancer = load_breast_cancer()
df = pd.DataFrame(cancer.data, columns=cancer.feature_names)
df['target'] = cancer.target
df['target_name'] = df['target'].map({0: 'malignant', 1: 'benign'})

# Mostramos las primeras filas de la tabla de datos
df

# Paso 3: Grafica de dispersion de dos caracteristicas para explorar separabilidad
plt.figure(figsize=(8, 6))
sns.scatterplot(
    data=df,
    x="mean radius",
    y="mean texture",
    hue="target_name",
    palette="Set1",
    s=60,
    alpha=0.7
)
plt.title("Conjunto de datos de Cancer de mama. \n Tamanio del tumor promedio vs indicador de textura promedio.")
plt.show()

# Paso 4: Separamos variables predictoras (X) de la variable objetivo (y), y dividimos en train y test
X = df.drop(columns=['target', 'target_name']) # Todas las caracteristicas numericas
y = df['target']                                # Etiqueta diagnostico

# Realizamos la particion estratificada: 70% entrenamiento y 30% prueba
X_train, X_test, y_train, y_test = train_test_split(
    X, y,
    test_size=0.3,
    random_state=7,
    stratify=y
)

# Paso 5: Estandarizamos las caracteristicas para que tengan media 0 y varianza 1
scaler = StandardScaler()

# Ajustamos el escalador con los datos de entrenamiento y transformamos ambos conjuntos
X_train_scaled = scaler.fit_transform(X_train)
X_test_scaled = scaler.transform(X_test)

# Mostramos el arreglo de caracteristicas de entrenamiento estandarizadas
X_train_scaled

# Paso 6: Creamos y entrenamos el clasificador Naive Bayes Gaussiano
nb = GaussianNB()
nb.fit(X_train_scaled, y_train)

# Ejemplo complementario: Evaluacion de clasificacion con division train/test
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report, confusion_matrix, ConfusionMatrixDisplay

# Dividimos los datos para la prueba con semilla fija
x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.2, random_state=7)

# Preparamos la matriz con constante para entrenamiento
X_train = sm.add_constant(x_train)

# Calculamos las probabilidades estimadas sobre el conjunto de prueba
X_test = sm.add_constant(x_test)
y_pred_prob = model.predict(X_test)

# Convertimos la probabilidad en etiqueta segun el umbral de 0.5
y_pred_class = (y_pred_prob >= 0.5).astype(int)

# Construimos la matriz de confusion
cm = confusion_matrix(y_test, y_pred_class)
disp = ConfusionMatrixDisplay(confusion_matrix=cm, display_labels=[0, 1])

# Imprimimos la matriz de confusion y el reporte detallado con metricas
print("matriz de confusion\n", cm)
print("\nClassification Report:\n", classification_report(y_test, y_pred_class, target_names=cancer.target_names))

# Paso 7: Generamos las predicciones del modelo Naive Bayes sobre el conjunto de prueba
y_pred = nb.predict(X_test_scaled)

# Paso 8: Evaluamos el desempenio mostrando la matriz de confusion y el reporte de clasificacion
print("\nConfusion Matrix:\n")
print(confusion_matrix(y_test, y_pred))

print("\nClassification Report:\n")
print(classification_report(
    y_test,
    y_pred,
    target_names=cancer.target_names
))

# Evaluacion de exactitud por cada variable individual (Criterio de informacion)
results = []

# Iteramos sobre cada columna para medir su capacidad predictiva por si sola
for variable in X.columns:
    X_var = X_train[[variable]]

    # Entrenamos el clasificador con solo esta variable
    nb = GaussianNB()
    nb.fit(X_var, y_train)

    # Predecimos y calculamos el porcentaje de clasificaciones correctas (exactitud)
    y_pred = nb.predict(X_var)
    accuracy = np.mean(y_train == y_pred) * 100

    results.append({
        'variable': variable,
        'Accuracy': accuracy
    })

# Convertimos los resultados a un DataFrame de pandas
results_df = pd.DataFrame(results)

# Graficamos la exactitud obtenida por cada variable individual en un grafico de barras
ax = sns.barplot(results_df, x='variable', y='Accuracy', orient='v')
ax.tick_params(axis='x', labelrotation=90)

# Entrenamos un modelo Naive Bayes seleccionando unicamente tres variables clave
variable = ['worst area',
            'worst radius',
            'worst perimeter']

X_var = X_train[variable]

# Ajustamos el modelo con las variables seleccionadas
nb = GaussianNB()
nb.fit(X_var, y_train)

# Obtenemos las predicciones sobre el conjunto de entrenamiento
y_pred = nb.predict(X_var)

# Imprimimos la matriz de confusion y el reporte con metricas de desempenio
print("\nConfusion Matrix:\n")
print(confusion_matrix(y_train, y_pred))

print("\nClassification Report:\n")
print(classification_report(
    y_train,
    y_pred,
    target_names=cancer.target_names
))
