import Bloom

def ask (message: String) := do
    puts message
    takes

def greet (name: String) := do
    puts s!"Hello {name}!"

def main : IO Unit := runDefault (ask "What is your name?" >>= greet)
