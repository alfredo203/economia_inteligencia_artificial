# Importamos pandas para manipular tablas de datos y statsmodels para modelos estadisticos
import pandas as pd
import statsmodels.api as sm

# Cargamos el conjunto de datos de Galton desde un enlace publico en internet
df = pd.read_csv('https://raw.githubusercontent.com/data-8/materials-fa17/refs/heads/master/lec/galton.csv')

# Definimos la variable dependiente (y) que deseamos predecir: altura del hijo
y = df['childHeight']

# Definimos las variables independientes (x) explicativas: altura de la madre y del padre
x = df[['mother', 'father']]

# Ajustamos el modelo de Minimos Cuadrados Ordinarios (OLS) sin constante
modelo = sm.OLS(y, x).fit()

# Mostramos el resumen estadistico completo del modelo estimado
print(modelo.summary())

# Importamos seaborn y matplotlib para visualizar los resultados mediante graficas
import seaborn as sns
import matplotlib.pyplot as plt

# Creamos un grafico de dispersion con la linea recta de regresion ajustada
sns.regplot(data=df, x='father', y='childHeight', fit_reg=True)

# Activamos la cuadricula de fondo para facilitar la lectura visual
plt.grid(True)

# Asignamos un titulo claro a la grafica
plt.title('Regresion lineal de Galton')

# Mostramos la grafica resultante en pantalla
plt.show()

# Ejemplo 2: Regresion lineal
# Caso de estudio: relacion entre anios de experiencia y salario en una empresa

import pandas as pd # Libreria para manejo y analisis de tablas de datos
import statsmodels.api as sm # Libreria para estimar modelos econometricos

# Leemos el archivo CSV local que contiene los registros de salarios
df = pd.read_csv(r'C:\Users\alfre\documentos_ssd\UNAM\Artículos\libro\data\salarios.csv')

# Mostramos el contenido de la tabla de datos en pantalla
df

# Definimos las variables para el modelo de regresion simple
# y: variable dependiente o respuesta (salario que queremos explicar)
# X: variable independiente o predictora (anios de experiencia acumulada)

X = df['YearsExperience'] # anios de experiencia laboral
y = df['Salary'] # salario anual en dolares

# Ajustamos un modelo de Minimos Cuadrados Ordinarios (OLS) sin termino constante
# Nota: al omitir la constante forzamos la recta a pasar por el origen (0,0)
modelo = sm.OLS(y, X).fit()

# Imprimimos la tabla de resultados estadisticos del modelo
print(modelo.summary())

# Inspeccionamos los valores de la variable predictora X
X

# Agregamos una columna constante de unos a la matriz X usando sm.add_constant
# Esto permite estimar el intercepto B0 (salario base cuando la experiencia es cero)
X_const = sm.add_constant(X)

# Visualizamos la nueva matriz que incluye la columna constante 'const'
X_const

# Ahora ajustamos el modelo de regresion lineal incluyendo el termino constante
modelo = sm.OLS(y, X_const).fit()

# Imprimimos el resumen estadistico del modelo con intercepto
print(modelo.summary())

# Graficamos los datos observados junto con la recta de regresion estimada con constante
sns.regplot(data=df, x='YearsExperience', y='Salary', fit_reg=True)

# Habilitamos la cuadricula para mejorar la interpretacion
plt.grid(True)

# Colocamos un titulo explicativo al grafico
plt.title('Regresion lineal de Salarios en una empresa de EE.UU')

# Desplegamos la grafica en pantalla
plt.show()
