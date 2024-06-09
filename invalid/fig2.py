import tensorflow as tf
from tensorflow.keras import layers, models

inp = tf.keras.Input(shape=(28, 28, 1))
x = layers.Flatten()(inp)
y = layers.Dense(10, activation="relu")(x)
z = layers.Dense(9, activation="sigmoid")(x)
out = layers.Add()([y, z])

model = tf.keras.Model(inp, out, name="mnist")
model.summary()

# $ python3 ./invalid/fig2.py
# ValueError: Inputs have incompatible shapes. Received shapes (10,) and (9,)