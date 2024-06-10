-- how do we go about this:
--   induction on side of the input:
-- what are we proving: 
--   that abpmodel xs = Pure ((), xs)
-- The abpmodel contains a specific handler for State (and a specific handle for lock, but that one is pretty standard) to make the output easy to work with, but we could show the equivalence for other state handlers.
base case: size -> 0 (because didn't manage to get a working implementation that starts at 0)

abpmodel xs = handle_ hStateS 
                  (handle_ hStateS 
                    (handle hLock 
                      (goesFirstLock (sendingProgram True []) (receivingProgram True))) 
                  True) 
                (True, Nothing)
==
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (goesFirstLock (do unlock;lock;cond <- get'; if cond == b then ...) (receivingProgram True))) 
          True) 
        (True, Nothing))
== -- apply handlers
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (lockFirst (cond <- get'; if cond == b then ...) (receivingProgram True))) 
          True) 
        (True, None)))))
== apply all handlers, with the inner state bool applying on the get
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (lockFirst (put' (not cond, Nothing);return ()) (receivingProgram True))) 
          True) 
        (True, Nothing)))
==
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (put' (not cond, Nothing); fmap ((),) (receivingProgram True)) 
          True) 
        (True, Nothing))))
== apply handlers:
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (fmap ((),) (receivingProgram True)) 
          True) 
        (False, Nothing)))))
== 
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (fmap ((),) (unlock;lock;(cond,el) <- get';case el of ...)) 
          True) 
        (False, Nothing)))))
== -- apply handlers, then the get is handled by state (bool, Maybe a)
(handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (fmap ((),) (return [])) 
          True) 
        (False, Nothing))))
== -- finish apply handlers (to a Pure so as a gen, all gens being just Pure)
Pure ((),[])
-- which proves our base case

-- recursive case:
-- length = n+1

abpmodel (x:xs) = handle_ hStateS 
                  (handle_ hStateS 
                    (handle hLock 
                      (goesFirstLock (sendingProgram True xs) (receivingProgram True))) 
                  True) 
                (True, Nothing)
==
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (goesFirstLock (do unlock;lock;cond <- get'; if cond == b then ...) (receivingProgram True))) 
          True) 
        (True, Nothing))
== -- apply handlers
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (lockFirst (cond <- get'; if cond == b then ...) (receivingProgram True))) 
          True) 
        (True, None)))))
== -- apply all handlers, with the inner state bool applying on the get
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (lockFirst (put' (not cond, Just x);sendingProgram (not b) xs) (receivingProgram True))) 
          True) 
        (True, Nothing)))
  == -- apply handlers, put 
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (lockFirst (sendingProgram (not b) xs) (receivingProgram True))) 
          True) 
        (False, Just x)))
==
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (lockFirst (unlock;lock;get';if ...) (receivingProgram True))) 
          True) 
        (False, Just x)))
==
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (lockPar (lock;get';if ...) (receivingProgram True))) 
          True) 
        (False, Just x)))
--the lockPar will divide into two different cases, let us look at each separately:
-- first, the right side of the plus operator (so receivingProgram going first)
==
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (fmap swap (goesFirstLock (receivingProgram True) (lock;get';if ...)))
          True) 
        (False, Just x)))
==
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (fmap swap (goesFirstLock (unlock;lock;(cond,el) <- get'; case el of ...) (lock;get';if ...)))
          True) 
        (False, Just x)))
  == -- push handler
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (fmap swap (lockFirst ((cond,el) <- get'; case el of ...) (lock;get';if ...)))
          True) 
        (False, Just x))
  == -- apply handler for state (Bool, Maybe a)
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (fmap swap (lockFirst (put' False;fmap (x:) receivingProgram False) (lock;get';if ...)))
          True) 
        (False, Just x))
  == -- now we will push the handlers again and apply the handler for State bool on the put until the next unlock. 
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (fmap swap (lockFirst (fmap (x:) (do unlock;lock;(cond,el)<-get';case el ...)) (lock;get';if ...)))
          False) 
        (False, Just x))
  == 
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (fmap swap (lockPar (fmap (x:) (do lock;(cond,el)<-get';case el ...)) (do lock;get';if ...)))
          False) 
        (False, Just x))
-- since goesFirst on an unlock turns into another goesFirst, we can add an unlock on eihter side of the lockPar and not change the program. Because of that, lockFirst x y = lockFirst (unlock;x) (unlock;y)
  == 
handle_ hStateS 
          (handle_ hStateS 
            (handle hLock 
              (fmap swap (lockPar (fmap (x:) (receivingProgram False)) (sendingProgram False)))
          False) 
        (False, Just x))

-- to keep going with the proof, a few steps required:
--  - first, find a way to say there a recursive step even though receiving is being fmapped into 
--  - i believe we have to do another division into cases, since both receivingProgram and sendingProgram are with a false, so after a full rep itll be swapped back, therell be another sub case to prove, problem is that subcase could devolve into the base case, which could have a few problems, such as whre the swap goes then etc. maybe make a few more cases, such as that exact case which we prove both sides of the plus operator for, but how do we handle the fmap still
--
