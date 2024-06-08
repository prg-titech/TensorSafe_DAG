from tensorflow.keras.layers import Input, Add, Flatten
from tensorflow.keras.models import Model

# 入力層を定義（異なる形状）
input = Input(shape=(3, 2))
flattened = Flatten()(input)

# Addレイヤーで入力を加算しようとする
added = Add()([flattened, input])

# モデルを定義
model = Model(inputs=[flattened, input], outputs=added)

# モデルのサマリーを表示
model.summary()
