structure StreamIO (a: Type) where
    (run: IO.FS.Stream -> IO.FS.Stream -> IO a)

instance : Monad StreamIO where
    pure x := { run := fun _ _ => pure x }
    bind mx f := { run := fun i o => do
        let y <- mx.run i o
        (f y).run i o
    }

def liftIO (mx: IO a): StreamIO a := { run := fun _ _ => mx }

def runDefault (mx: StreamIO a): IO a := do
    let stdin <- IO.getStdin
    let stdout <- IO.getStdout

    mx.run stdin stdout

def puts (msg: String): StreamIO Unit := { run := fun _ o => o.putStrLn msg }

def takes: StreamIO String := { run := fun i _ => i.getLine }
