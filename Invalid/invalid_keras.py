import tensorflow as tf
from tensorflow.keras.layers import Input, Flatten, Add

input_layer = Input(shape=(3, 2)) # D2 3 2
flatten_layer = Flatten()(input_layer) # D1 6
try:
    add_layer = Add()([
        input_layer,
        flatten_layer
    ]) # error!
    print("Add layer created successfully without shape mismatch.")
except Exception as e:
    print("Error during model creation:")
    print(e)
