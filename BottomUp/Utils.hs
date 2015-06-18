module Utils where
import CommonData


with :: (Value -> Value) -> Value -> Value
with f (Thunk e a) = Thunk (\x -> with f (e x)) a
with f a = f a