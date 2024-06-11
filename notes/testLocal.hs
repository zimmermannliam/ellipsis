import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.State.Lazy
import Control.Monad.Reader.Class (local)

type T a = ExceptT String (State Int) a

data Tree = LeafC Char | Leaf Int | Branch [Tree] | MyBranch Int Tree
    deriving (Show, Eq)

t1 = Branch [LeafC 'A', Branch [LeafC 'B', LeafC 'C']]
t2 = Branch [LeafC 'A', MyBranch 1000 (Branch [LeafC 'B', LeafC 'C']), LeafC 'D']

fun :: Int -> Tree -> Either String Tree
fun i t = evalState (runExceptT $ go t) i

go :: Tree -> T Tree
go (LeafC _) = do
    i <- lift get
    lift $ modify (+1)
    return (Leaf i)
go (Branch ts) = do
    ts' <- mapM go ts
    return $ Branch ts'
go (MyBranch i t) = mapExceptT (local (+ i)) (go t)
go _ = throwE "bad"

fun' :: Int -> Tree -> Tree
fun' i t = evalState (go' t) i

go' :: Tree -> State Int Tree
go' (LeafC _) = do
    i <- get
    modify (+1)
    return (Leaf i)
go' (Branch ts) = do
    ts' <- mapM go' ts
    return $ Branch ts'
go' (MyBranch i t) = localState (+ i) (go' t)
go' _ = undefined


hoistMaybe :: Monad m => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

localState :: (s -> s) -> State s a -> State s a
localState f c = do
    temp <- get  -- pop
    modify f
    res <- c
    put temp  -- push
    return res