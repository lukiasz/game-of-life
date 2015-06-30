module GameOfLifeSpec where
import SpecHelper

spec :: Spec
spec = do
    describe "GameOfLife" $ do
        context "strUniverse" $ do
            it "returns correct string for one-cell board" $ 
                let universe = Universe {
                    cells = [(1,1)],
                    width = 2,
                    height = 2
                } in 
                strUniverse universe `shouldBe` "  \n O"
            it "returns correct string for full board" $
                let universe = Universe {
                    cells = [(0,0), (0,1), (1,0), (1,1)],
                    width = 2,
                    height = 2
                } in
                strUniverse universe `shouldBe` "OO\nOO"

        context "newUniverse" $ do
            it "creates new universe with correct width" $
                let w = 2
                    h = 3
                    positions = [False, False, False, False, False, False] in
                width (newUniverse w h positions) `shouldBe` 2

        context "newUniverse" $ do
            it "creates new universe with correct height" $
                let w = 2
                    h = 3
                    positions = [False, False, False, False, False, False] in
                height (newUniverse w h positions) `shouldBe` 3

        context "newUniverse" $ do
            it "creates new universe with correct cells positions" $
                let w = 2
                    h = 3
                    positions = [True, False, False, False, False, True] in
                cells (newUniverse w h positions) `shouldBe` [(0,0), (2,1)]

        context "isAlive" $ do
            it "returns true if given position is occupied by a living cell" $
                let board = [(1,1)] in 
                isAlive board (1,1) `shouldBe` True

        context "wrap" $ do
            it "wraps coordinates if they are outside of board" $
                let w = 5
                    h = 7 in
                wrap w h (10,10) `shouldBe` (3, 0)

        context "neighbs" $ do
            it "returns nearest coordinates for given coordinates" $
                let universe = Universe {
                    width = 5,
                    height = 5,
                    cells = []
                } in
                neighbs universe (1,1) `shouldBe` [(0,0),(1,0),(2,0),(0,1),(2,1),(0,2),(1,2),(2,2)]

        context "liveneighbs" $ do
            it "returns number of living cells" $
                let universe = Universe {
                    width = 5,
                    height = 5,
                    cells = [(1,0), (2,0)]
                } in
                liveneighbs universe (1,1) `shouldBe` 2

        context "survivors" $ do
            it "returns world with cells that survived" $
                let universe = Universe {
                    width = 5,
                    height = 5,
                    cells = [(1,0), (1,1), (2,0)]
                } in
                cells (survivors universe) `shouldBe` [(1,0), (1,1), (2,0)]

        context "births" $ do
            it "returns world with newly born cells" $
                let universe = Universe {
                    width = 5,
                    height = 5,
                    cells = [(1,0), (1,1), (2,0)]
                } in
                cells (survivors universe) `shouldBe` [(1,0), (0,0), (2,0)]
                    
main :: IO ()
main = hspec spec
