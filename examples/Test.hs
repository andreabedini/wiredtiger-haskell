import Test.Tasty
import Test.Tasty.Hspec
import Access 

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs [ access ]
  defaultMain (testGroup "All Tests" [testGroup "Specs" specs])
