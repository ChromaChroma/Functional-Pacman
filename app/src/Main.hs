module Main where
  
import Model.TestModel
import Controller.TestController
import View.TestView

main :: IO ()
main = do
  showText model
    where
      model = MkTestModel "name"
