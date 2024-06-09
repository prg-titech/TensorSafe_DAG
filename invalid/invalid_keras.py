import tensorflow as tf
from tensorflow.keras.layers import Input, Add, Flatten
from tensorflow.keras.models import Model

input = Input(shape=(3, 2))
flattened = Flatten()(input)
added = Add()([flattened, input]) # error!

model = Model(inputs=[flattened, input], outputs=added)
model.summary()

# $ python3 ./invalid/invalid_keras.py
# ValueError: Inputs have incompatible shapes. Received shapes (3, 2) and (6,)