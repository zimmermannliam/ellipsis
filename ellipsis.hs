data Expl = Ellipsis
          | Empty
          | Ele EleName SubScript
          | List Expl Expl                   -- First Expl only Ele
          | Bind Params Expl Expl Expl  -- First Expl only params
          | Function String
          | App Expl Expl
          | App2Infx Expl Expl Expl
          deriving Show
-- data SubScript = Expression of some sort | Constant of some sort |
-- index arithmetic!
-- index expressions

type EleName = String

type SubScript = String

type FnName = String

type Params = [String]





translate :: Expl -> Expr
explain :: Expr -> Expl
extend the lambda calculus with ellipsis? would require a pattern type for pattern matching
                                          and complex type checking
                                          standard ish approach, possibly more straightforward
                                          bindings (env)
                                          prepare, then add ellipsis patterns and ellipsis expr


{-
pp :: Expl -> String
pp Ellipsis             = "..."
pp (Ele en ss)          = en ++ "_" ++ ss
pp (List el Empty)      = pp el
pp (List el rst)        = (pp el) ++ ", " ++ (pp rst)
pp (Bind fn p l1 l2)    = fn ++ " " ++ (p) ++ ": \n" ++ (pp l1) ++ "->" ++ (pp l2)
pp (App f e)          = pp f ++ " " ++ pp e
pp (App2Infx f e1 e2)  = pp e1 ++ " `" ++ pp f ++ "` " ++ pp e2
pp _                    = ""

xlist = (List (Ele "x" "1") $ List (Ellipsis) $ List (Ele "x" "n") Empty)

main :: IO ()
main = do 
    let expr = Bind "Map" (Param "f") (List (Ele "x" "1") $ List (Ellipsis) $ List (Ele "x" "n") Empty) (List (App1 (Param "f") $ Ele "x" "1") $ List (Ellipsis) $ List (App1 (Param "f") $ Ele "x" "n") Empty)
    putStrLn $ pp expr
-}
