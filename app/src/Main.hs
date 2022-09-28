module Main where
  
import Model.TestModel
import Controller.TestController
import View.TestView

main :: IO ()
main = do
  -- putStrLn "Hello, Haskell!"
  
  showText model
    where
      model = MkTestModel "name"
