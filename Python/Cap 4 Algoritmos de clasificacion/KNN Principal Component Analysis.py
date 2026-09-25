# Importamos librerias esenciales para analisis numerico, manipulacion de tablas y graficas
import numpy as np
import pandas as pd
import seaborn as sns

# Definimos la ruta del archivo de datos de clientes de tarjetas de credito y lo cargamos
path = r"C:\Users\alfre\documentos_ssd\UNAM\Artículos\libro\data\Customer_Data.csv"
df = pd.read_csv(path)

# Obtenemos un resumen estadistico descriptivo (media, desviacion estandar, cuartiles, minimos y maximos)
df.describe()

# Consultamos los nombres de todas las columnas disponibles en la tabla de datos
df.columns

# Graficamos la dispersion entre monto de compras (PURCHASES) y adelantos en efectivo (CASH_ADVANCE)
sns.scatterplot(df, x='PURCHASES', y='CASH_ADVANCE', alpha=0.7)

# Aplicamos transformacion logaritmica a las columnas numericas para suavizar distribuciones asimetricas
numeric_cols = df.select_dtypes(include=[np.number]).columns
df[numeric_cols] = np.log(df[numeric_cols])

# Graficamos nuevamente la dispersion con los valores en escala logaritmica
sns.scatterplot(df, x='PURCHASES', y='CASH_ADVANCE', alpha=0.7)

# Recargamos los datos originales y aplicamos escalamiento MinMaxScaler al intervalo [0, 1]
import pandas as pd
import numpy as np
from sklearn.preprocessing import MinMaxScaler

path = r"C:\Users\alfre\documentos_ssd\UNAM\Artículos\libro\data\Customer_Data.csv"
df = pd.read_csv(path)

# Configuramos el escalador para que devuelva un DataFrame de pandas con nombres de columnas
scaler = MinMaxScaler().set_output(transform="pandas")

# Filtramos las variables numericas
numeric_cols = df.select_dtypes(include=[np.number]).columns
df_num = df[numeric_cols]

# Ajustamos y transformamos los datos a la escala normalizada entre 0 y 1
df_scaled = scaler.fit_transform(df_num)

# Verificamos las estadisticas descriptivas de las variables escaladas
df_scaled[['PURCHASES', 'CASH_ADVANCE']].describe()

# Reduccion de dimensiones mediante Analisis de Componentes Principales (PCA)
from sklearn.decomposition import PCA

# Eliminamos filas que contengan valores nulos antes de aplicar PCA
df_scaled = df_scaled.dropna()

# Reducimos todas las variables a 2 componentes principales para poder graficarlas en dos dimensiones
pca = PCA(n_components=2).set_output(transform="pandas")
df_pca = pca.fit_transform(df_scaled)

# Inspeccionamos la tabla resultante con las dos componentes principales generadas
df_pca

# Graficamos la dispersion de las dos primeras componentes principales (pca0 vs pca1)
sns.scatterplot(data=df_pca, x='pca0', y='pca1', alpha=0.6)

# Agrupamiento no supervisado con el algoritmo K-Means
from sklearn.cluster import KMeans
from sklearn.metrics import pairwise_distances_argmin_min

# Ajustamos el modelo K-Means para segmentar los datos en 7 grupos (clusters)
kmeans = KMeans(n_clusters=7).fit(df_pca)

# Asignamos a cada observacion la etiqueta del grupo al que pertenece
df_pca['label'] = kmeans.predict(df_pca)

# Graficamos los clientes proyectados en las dos componentes, coloreando cada punto segun su grupo
sns.scatterplot(df_pca, x='pca0', y='pca1', hue='label', palette='Set1')

# Diagrama de caja para analizar la distribucion de la primera componente (pca0) en cada cluster
sns.boxplot(data=df_pca, x='label', y='pca0')

# Diagrama de caja para analizar la distribucion de la segunda componente (pca1) en cada cluster
sns.boxplot(data=df_pca, x='label', y='pca1')
