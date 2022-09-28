
module Controller.TestController(change) where

import Model.TestModel

change :: String -> TestModel -> TestModel
change string model = model { name = string }