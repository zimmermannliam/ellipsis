data Expl = Ellipsis
          | Empty
          | Ele EleName SubScript
          | List Expl Expl                   -- First Expl only Ele
          | Bind Params Expl Expl Expl  -- First Expl only params
          | Function String
          | App Expl Expl
          | App2Infx Expl Expl Expl
          deriving Show

data SubScript = Expression of some sort | Constant of some sort |
-- index arithmetic!
-- index expressions

type EleName = String

type SubScript = String

type FnName = String

type Params = [String]


pp :: Expl -> String
pp Ellipsis             = "..."
pp (Ele en ss)          = en ++ "_" ++ ss
pp (List el Empty)      = pp el
pp (List el rst)        = (pp el) ++ ", " ++ (pp rst)
pp (Bind fn p l1 l2)    = fn ++ " " ++ (ppParam p) ++ ": \n" ++ (pp l1) ++ "->" ++ (pp l2)
pp (Param s)            = s
pp (App f e)          = pp f ++ " " ++ pp e
pp (App2Infx f e1 e2)  = pp e1 ++ " `" ++ pp f ++ "` " ++ pp e2
pp _                    = ""

xlist = (List (Ele "x" "1") $ List (Ellipsis) $ List (Ele "x" "n") Empty)

ppParam :: Params -> Expr
ppParam []      = ""
ppParam (x:xs)  = (x) ++ " " ++ (ppParam xs)

main :: IO ()
main = do 
    let expr = Bind "Map" (Param "f") (List (Ele "x" "1") $ List (Ellipsis) $ List (Ele "x" "n") Empty) (List (App1 (Param "f") $ Ele "x" "1") $ List (Ellipsis) $ List (App1 (Param "f") $ Ele "x" "n") Empty)
    putStrLn $ pp expr

