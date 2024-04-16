

Initial segments of list
```
inits (x_1 ... x_n) = (x_1 ... x_1) ... (x_1 ... x_n)
```

Binary search
```
binSearch xs t = case xs of
	[]             -> false
	x_1 ... x_n    -> let k = n/2+1 in
	   if x_k == t     then true
	   else if x_k > t then binSearch (x_1 ... x_(k-1)) t
	   else                 binSearch (x_(k+1) ... x_n)
```

Zip
```
zip (x_1 ... x_n) (y_1 ... y_n) = 
    (x_1, y_1) ... (x_n, y_n)
```

Pair adjacent elements
```
pairAdj (x_1 ... x_n) = (x_1, x_2) ... (x_(n-1), x_n)
```

Rotate left
```
rotL k (x_1 ... x_n) = let k' = k % n
    in (x_(k'+1) ... x_n) ++ (x_1 ... x_k')
```